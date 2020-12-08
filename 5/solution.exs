defmodule Solution do
  def parse_input() do
    {:ok, input} = File.read("./input.txt")

    input
    |> String.split("\n", trim: true)
    |> Enum.map(&row_col_for_id/1)
    |> Enum.map(&seat_id/1)
  end

  # TODO can this be generic?
  def get_row(row, min, max) do
    if String.length(row) == 1 do
      case row do
        "F" -> min
        "B" -> max
      end
    else
      {head, tail} = String.split_at(row, 1)

      next = (max - min) / 2 + min

      case head do
        "F" -> Solution.get_row(tail, min, floor(next))
        "B" -> Solution.get_row(tail, ceil(next), max)
      end
    end
  end

  def get_column(col, min, max) do
    if String.length(col) == 1 do
      case col do
        "L" -> min
        "R" -> max
      end
    else
      {head, tail} = String.split_at(col, 1)

      next = (max - min) / 2 + min

      case head do
        "L" -> Solution.get_column(tail, min, floor(next))
        "R" -> Solution.get_column(tail, ceil(next), max)
      end
    end
  end

  def row_col_for_id(id) do
    {row, col} = String.split_at(id, 7)
    {Solution.get_row(row, 0, 127), Solution.get_column(col, 0, 7)}
  end

  def seat_id(rowcol) do
    {row, col} = rowcol

    row * 8 + col
  end

  def solve(input) do
    Enum.max(input)
  end

  def solve_part_2(input) do
    index =
      input
      |> Enum.sort()
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [x, y] -> abs(x - y) end)
      |> Enum.find_index(fn x -> x > 1 end)
  end
end

input = Solution.parse_input()
part1 = Solution.solve(input)
# part2 = Solution.solve_part_2(input)

IO.puts("Part 1: #{part1}")
# IO.puts("Part 1: #{inspect(part2)}")
