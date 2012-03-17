# Author: Pierre-Louis Gottfrois
# 03/13/2012

require 'narray'

@y = 0.99
@iteration = 5000

def parse_files
  @r = IO.readlines('rewards.txt').map(&:to_i)
  @va = [NArray.float(81,81), NArray.float(81,81), NArray.float(81,81), NArray.float(81,81)]
  IO.readlines('prob_a1.txt').map {|r| cols = r.split(' '); @va[0][(cols[0].to_i - 1),(cols[1].to_i - 1)] = cols[2].to_f}
  IO.readlines('prob_a2.txt').map {|r| cols = r.split(' '); @va[1][(cols[0].to_i - 1),(cols[1].to_i - 1)] = cols[2].to_f}
  IO.readlines('prob_a3.txt').map {|r| cols = r.split(' '); @va[2][(cols[0].to_i - 1),(cols[1].to_i - 1)] = cols[2].to_f}
  IO.readlines('prob_a4.txt').map {|r| cols = r.split(' '); @va[3][(cols[0].to_i - 1),(cols[1].to_i - 1)] = cols[2].to_f}
end

def compute_vs
  vs = Array.new(81) { [0.0] }
  @policy = Array.new(81)
  @iteration.times {|k|
    81.times {|s|
      @policy[s] ||= []
      vss = @va.each_index.map {|j| vs.each_index.inject {|sum, i| sum + @va[j][s,i] * vs[i][k]}}
      vs[s] << @r[s] + @y * vss.max
      @policy[s] << vss.index(vss.max)
    }
  }
  return vs.transpose.last
end

def display_a(vs)
  vs.each_with_index {|x, i| puts "#{i}: #{x}" unless x == 0}
end

def display_b(policy)
  arrows = ['<', '^', '>', 'v']
  9.times {|j| 9.times {|i| print "#{arrows[policy[i * 9 + j]]}".center(3)}; puts ""}
end

parse_files
display_a(compute_vs)
display_b(@policy.transpose.last)