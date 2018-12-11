# require "tempfile"
# require "chunky_png"
# require "mini_magick"
# require "rtesseract"

Point = Struct.new(:x, :y) do
  def neighbor?(other)
    (other.x - x).abs + (other.y - y).abs == 1
  end
end

Light = Struct.new(:position, :velocity) do
  def neighbor?(other)
    other.position.neighbor?(position)
  end
end

LINE = /position=< ?(-?\d+),  ?(-?\d+)> velocity=< ?(-?\d+),  ?(-?\d+)>/

def parse(input)
  input.lines.map do |line|
    m = LINE.match(line)

    if m.nil?
      raise ArgumentError, "couldn't parse line '#{line}'"
    end

    Light.new(
      Point.new(m[1].to_i, m[2].to_i),
      Point.new(m[3].to_i, m[4].to_i),
    )
  end
end

def move(lights, t)
  lights.map do |light|
    v = light.velocity
    Light.new(
      Point.new(
        light.position.x + (v.x * t),
        light.position.y + (v.y * t),
      ),
      v,
    )
  end
end

# number of lights immediately next to another light
def neighbor_count(lights)
  lights.count do |light|
    lights.any? { |l| light.neighbor?(l) }
  end
end

def print_lights(lights)
  # map the list of lights to a matrix, print it
  min_x = lights.map { |l| l.position.x }.min
  max_x = lights.map { |l| l.position.x }.max
  min_y = lights.map { |l| l.position.y }.min
  max_y = lights.map { |l| l.position.y }.max

  width = (max_x - min_x) + 1
  height = (max_y - min_y) + 1

  line = " " * width
  lines = height.times.map { line.dup }

  lights.map do |light|
    lines[light.position.y][light.position.x] = "#"
  end

  puts lines.join("\n")
end

# fitness function: returns which state is "better"
def better(state, state2, t)
  print "\033[1A\033[" if @progress_printed
  @progress_printed = true

  nc1 = neighbor_count(state)
  nc2 = neighbor_count(state2)

  puts "_t=#{t}, nc(state)=#{nc1} nc(state2)=#{nc2}"

  if nc1 < nc2
    state
  else
    state2
  end
end

def p1(lights)
  time = 0
  state = lights
  state2 = move(lights, 1)

  until state == better(state, state2, time)
    state3 = move(state2, 1)
    state = state2
    state2 = state3
    time += 1
  end

  [state, time]
end

# create image, treat each light as a 10x10 px block
# def generate_image(lights)
#   min_x = lights.map { |l| l.position.x }.min
#   max_x = lights.map { |l| l.position.x }.max
#   min_y = lights.map { |l| l.position.y }.min
#   max_y = lights.map { |l| l.position.y }.max

#   width = (max_x - min_x) + 20
#   height = (max_y - min_y) + 20

#   puts "generate_image: max_x = #{max_x}, min_x = #{min_x}"
#   puts "generate_image: width = #{width}, height = #{height}"

#   png = ChunkyPNG::Image.new(width, height, ChunkyPNG::Color::WHITE)

#   lights.each do |light|
#     x_start = 10 * light.position.x
#     y_start = 10 * light.position.y

#     (x_start..(x_start + 10)).each do |x|
#       (y_start..(y_start + 10)).each do |y|
#         png[x, y] = ChunkyPNG::Color::BLACK
#       end
#     end
#   end
# end

if $0 == __FILE__
  input = File.read(ARGV[0])
  lights = parse(input)

  p1_state, t = p1(lights)

  puts "p1: ran until t = #{t}"
  print_lights p1_state
end
