{:ok, input} = File.read("input.txt")
input = input
    |> String.split
    |> Enum.map(&String.to_integer/1)

diff = Enum.map(input, fn int -> 2020 - int end)
match = Enum.find_index(input, fn x -> Enum.member?(input, 2020 - x) end)

res = Enum.at(input, match) * Enum.at(diff, match)
 
IO.puts "#{res}"
