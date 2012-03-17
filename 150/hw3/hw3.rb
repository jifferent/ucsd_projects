# Author : Pierre-Louis GOTTFROIS
# 02/07/2012

# Parse vocabulary file and store it into a hash.
#
# @return Hash
def parse_vocab
  unigrams = parse_unigram
  vocabs = Hash.new
  IO.readlines('vocab.txt').each_with_index {|l,i|
    vocabs[l.chomp.downcase.to_sym] = unigrams[i].chomp
  }

  return vocabs
end

# Parse unigram file and store it into an array.
#
# @return Array
def parse_unigram
  IO.readlines('unigram.txt')
end

# Parse bigram file and store it into a hash.
#
# @return Hash
def parse_bigrams
  bigrams = Hash.new
  IO.readlines('bigram.txt').each {|l|
    values = l.chomp.split(' ')
    bigrams[ [values[0].to_i, values[1].to_i] ] = values.last.to_i
  }

  return bigrams
end

# Calculate the total numbers of word frequecies.
#
# @return Int
def total_count(data)
  res = 0
  data.values.each {|value|
    res += value.to_i
  }

  return res
end

# Fetch only words starting with given letter.
def fetch_words_beginning_with(letter)
  regex = Regexp.new("^#{letter}", true)
  return @vocab.select { |key, value| key =~ regex }
end

# Compute the unigram formula for a given word.
def compute_unigram_for(word)
  @vocab[word].to_f / @total
end

# Compute the bigram formula for a given word followed by another.
def compute_bigram_for(a_word, followed)
  a_word_index = find_index_for(a_word)
  followed_index = find_index_for(followed)
  (@bigrams[[followed_index, a_word_index]]).to_f / @vocab[followed].to_f
end

# Compute unigram method over a given string.
def unigram_with_string(str)
  res = 0
  str.chomp.split(' ').each {|word|
    if res == 0
      res = compute_unigram_for(word.downcase.to_sym)
    else
      res *= compute_unigram_for(word.downcase.to_sym)
    end
  }

  return Math.log(res)
end

# Compute bigram method over a given string.
def bigram_with_string(str)
  res = 0
  words = find_indexes_for(str.chomp.split(' '))

  last_word = ''
  words.each_with_index {|(word, index), i|
    if i == 0
      res = compute_unigram_for(word)
    else
      res *= compute_bigram_for(word, last_word)
    end
    last_word = word
  }

  return Math.log(res)
end

def display_a(data)
  puts "A)"
  puts "#{'Word'.center(6)}\t\t|\tFrequency\t|\tProbability"
  puts "------------------------------------------------------------"
  data.each {|key, value|
    t = value.to_f / @total
    puts "#{key.to_s.center(10)}\t|\t#{value.center(8)}\t|\t#{t.round(6)} (#{(t.round(6) * 100).round(2)}%)"
  }
  puts "#{data.count} words."
end

def display_b
  puts "B)"
  puts "#{'Word'.center(6)}\t\t|\tFrequency\t|\tProbability"
  puts "------------------------------------------------------------"

  res = Hash.new
  @vocab.keys.each {|word|
    res[word] = compute_bigram_for(word, :the)
  }
  res = res.sort_by {|word, count| count}
  res[(res.count - 10)..-1].each {|key, value|
    puts "#{key.to_s.center(10)}\t|\t#{@vocab[key].center(8)}\t|\t#{value.round(6)} (#{(value.round(6) * 100).round(2)}%)"
  }
end

def display_c
  puts "C)"
  str = "The stock market fell by one hundred points last week"
  puts str
  puts "Unigram Likelihood : #{unigram_with_string(str).round(6)}"
  puts "Bigram Likelihood  : #{bigram_with_string(str).round(6)}"
end

def display_d
  puts "D)"
  str = "The sixteen officials sold fire insurance"
  puts str
  puts "Unigram Likelihood : #{unigram_with_string(str).round(6)}"
  puts "Bigram Likelihood  : #{bigram_with_string(str).round(6)}"
end

def find_indexes_for(words)
  res = Hash.new
  words.each {|word|
    word = word.downcase.to_sym
    res[word] = find_index_for(word)
  }

  return res
end

def find_index_for(word)
  @vocab.keys.each_with_index {|w, i|
    return (i + 1) if w.downcase.to_sym == word
  }

  return nil
end

@vocab = parse_vocab
@total = total_count(@vocab)
@bigrams = parse_bigrams

display_a(fetch_words_beginning_with('T'))
display_b
display_c
display_d