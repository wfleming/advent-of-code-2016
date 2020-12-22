#!/usr/bin/env julia

mutable struct GameState
  ages::Array{Int}
  last_turn::Int
  last_said::Int
  say_next::Int
end

function load_seed()
  read(open(ARGS[1]), String) |>
    Base.Fix2(split, r","m) |>
    Base.Fix1(map, strip) |>
    Base.Fix1(map, Base.Fix1(parse, Int))
end

function seed_game(seed):: GameState
  ages = Dict()
  for i in eachindex(seed)
    ages[seed[i]] = i
  end

  GameState(ages, length(seed), last(seed), 0)
end

function take_turn(game::GameState)::GameState
  # println("DEBUG: last_turn=$(game.last_turn) last_said=$(game.last_said) say_now=$(game.say_next) ages=$(game.ages)")
  game.last_said = game.say_next
  game.last_turn += 1
  game.say_next = game.last_turn - get(game.ages, game.say_next, game.last_turn)
  game.ages[game.last_said] = game.last_turn

  game
end

function go_to_turn(game::GameState, turn_count::Int)::GameState
  while game.last_turn < turn_count
    take_turn(game)
  end

  game
end

# p1
g = seed_game(load_seed())
g = go_to_turn(g, 2020)
println("p1: after $(g.last_turn) turns, last said was $(g.last_said)")

# p2
g = go_to_turn(g, 30_000_000)
println("p2: after $(g.last_turn) turns, last said was $(g.last_said)")
