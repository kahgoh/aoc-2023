defmodule Solve do
  def solve(map) do
    Enum.map(map, fn entry -> try_horizontal(entry) end)
    |> Enum.sum()
  end

  def try_horizontal(map) do
    first = Enum.at(map, 0)

    # How long is it?
    width = Enum.count(first)

    # The size of the left hand to try
    case try_split_map(map, width - 1) do
      {:ok, w} ->
        w

      _ ->
        case try_split_map(transpose(map), Enum.count(map) - 1) do
          {:ok, w} ->
            w * 100

          _other ->
            debug_print(map)
        end
    end
  end

  def debug_print(map) do
    Enum.each(map, fn e -> IO.inspect(e) end)
    :error
  end

  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end

  def try_split_map(_map, width) when width < 1, do: {:none}

  def try_split_map(map, width) do
    IO.inspect(width, label: "width")

    case test_split_map(map, width) do
      true -> {:ok, width}
      false -> try_split_map(map, width - 1)
    end
  end

  def test_split_map(map, width) do
    Enum.all?(map, fn line -> test_split(line, width) end)
  end

  def test_split(line, width) do
    [left, right | _other] = Enum.chunk_every(line, width)
    min = Enum.min([Enum.count(left), Enum.count(right)])
    last_left = get_last(left, min)
    last_left == right
  end

  def get_last(next, count), do: do_get_last(Enum.reverse(next), [], count)

  def do_get_last([], acc, _count), do: Enum.reverse(acc)
  def do_get_last(_remaining, acc, 0), do: Enum.reverse(acc)

  def do_get_last([next | remaining], acc, count),
    do: do_get_last(remaining, [next | acc], count - 1)
end
