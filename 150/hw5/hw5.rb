def parse_files
  @initial      = IO.readlines('initialStateDistribution.txt').map(&:to_f)
  @transition   = IO.readlines('transitionMatrix.txt').map {|x| x.split(' ').map(&:to_f)}
  @emission     = IO.readlines('emissionMatrix.txt').map {|x| x.split(' ').map(&:to_f)}
  @observations = IO.readlines('observations.txt').first.split(' ').map(&:to_i)
  @logs         = @transition.map {|i| i.map {|j| Math.log(j)}}
end

def transition(data, t, j)
  @transition.each_index.map {|i| data[i][t] + @logs[i][j]}
end

def compute_log_likelyhood
  res = @initial.each_index.map {|i| [Math.log(@initial[i]) + Math.log(@emission[i][0])]}
  56998.times {|t|
     @emission.each_index {|j|
       res[j][t+1] = transition(res, t, j).max + Math.log(@emission[j][@observations[t+1]])
    }
  }
  return res
end

def compute_most_likely_sequence(lit)
  res = Array.new(@observations.count-1)
  res.push(lit.transpose.last.each_with_index.max[1])
  56999.times {|t| res[56998-t] = transition(lit, 56998-t, res[56999-t]).each_with_index.max[1]}
  return res
end

parse_files and puts compute_most_likely_sequence(compute_log_likelyhood).map {|x| %(abcdefghijklmnopqrstuvwxyz)[x]}.join.squeeze

# $> ruby -v
# ruby 1.9.3dev (2011-07-31 revision 32789) [x86_64-darwin10.8.0]
# $> ruby -w hw5.rb
# everydayimshufling
# $>