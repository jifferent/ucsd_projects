# Author : Pierre-Louis GOTTFROIS
# 02/17/2012

# Parse X and Y data files.
def parse_data
  @xs = IO.readlines('X.dat').map {|l| l.split(' ').map!(&:to_i) }
  @ys = IO.readlines('Y.dat').map(&:to_i)
end

# Compute Log-likelihood.
def compute_likelihood
  res = 0
  @ys.each_index {|t| res += Math.log(compute_noisy_or(t)) }

  return res / @ys.count
end

# Compute Noisy-OR at 't' time.
def compute_noisy_or(t)
  res = 1
  20.times {|i| res *= ((1 - @pis[i]) ** @xs[t][i]) }

  return (@ys[t] == 1 ? 1 - res : res)
end

# Update all values of Pi.
def update_pis
  new_pis = []
  20.times {|i|
    res = 0
    @ys.each_with_index {|yt, t|
      res += (yt * @xs[t][i] * @pis[i]) / compute_noisy_or(t)
    }
    new_pis << res / @tis[i].to_f
  }

  return new_pis
end

# Compute Ti.
def number_of_example_where_x_is_one
  res = Array.new(20, 0)
  res.each_index {|i|
    @ys.each_index {|t|
      res[i] += @xs[t][i]
    }
  }

  return res
end

parse_data
@pis = Array.new(20, 0.25)
@tis = number_of_example_where_x_is_one

puts "# |    0    |    1    |    2    |    4    |    8   |    16   |    32   |    64"
res = [compute_likelihood.round(4)]
64.times do |x|
  @pis = update_pis
  res << compute_likelihood.round(4)
end

puts "L | #{res[0].to_s.center(2)} | #{res[1].to_s.center(2)} | #{res[2].to_s.center(2)} | #{res[4].to_s.center(2)} | #{res[8].to_s.center(2)} | #{res[16].to_s.center(2)} | #{res[32].to_s.center(2)} | #{res.last.to_s.center(2)}"
puts "Last value of Pi :"
p @pis.map { |x| x.round(4) }