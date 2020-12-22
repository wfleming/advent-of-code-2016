#!/usr/bin/env ruby

SEED = File.read(ARGV[0]).split(",").map(&method(:Integer))

class Game
  attr_reader :ages, :last_turn, :last_said, :say_next

  def initialize(seed)
    @ages = [nil] * seed.max
    # number spoken => last turn it was spoken
    seed.each_with_index do |n, idx|
      ages[n] = idx + 1
    end
    @last_said = seed[-1]
    @say_next = 0 # no repeats in seed
    @last_turn = seed.count
  end

  def play_turn!
    # puts "DEBUG current_turn=#{last_turn + 1}, last_said=#{last_said} say_this_turn=#{say_next} ages=#{ages}"
    @last_said = say_next
    @last_turn += 1
    @say_next = last_turn - (ages[say_next] || last_turn)
    ages[last_said] = last_turn
  end

  def play_turns!(turn_count)
    @ages = ages + ([nil] * turn_count)# pre-allocate array
    until last_turn == turn_count
      play_turn!
    end
  end
end

# p1
g = Game.new(SEED)
g.play_turns!(2020)
puts "p1: after 2020 turns, last number spoken was #{g.last_said}"

# p2
g.play_turns!(30_000_000)
puts "p2: after 30M turns, last number spoken was #{g.last_said}"
