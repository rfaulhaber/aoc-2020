{:ok, input} = File.read("input.txt")
input = input
    |> String.split
    |> Enum.map(&String.to_integer/1)

diff = Enum.map(input, fn int -> 2020 - int end)
match = Enum.find_index(input, fn x -> Enum.member?(input, 2020 - x) end)

res = Enum.at(input, match) * Enum.at(diff, match)
 
IO.puts "Part 1: #{res}"

pairs = Enum.zip(input, diff)

# IO.puts "#{inspect(pairs)}"

{left, right} = Enum.find(pairs, fn {x, y} -> 2020 - x - y == 0 end)
IO.puts "Left/right #{left} #{right}"
third = abs(2020 - left - right)

IO.puts "Part 2: #{left * right * third}"
