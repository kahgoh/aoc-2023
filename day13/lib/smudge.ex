defmodule Smudge do
  def solve(map) do
    Enum.map(map, fn entry -> try_horizontal(entry) end)
    |> Enum.sum()
  end

  @doc """
  Tries look for a mirror in map from right to left. If it can't, it will transpose the map and tries one more time.
  """
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

  @doc """
  Some extra debugging functionality.
  """
  def debug_print(map) do
    Enum.each(map, fn e -> IO.inspect(e) end)
    :error
  end

  @doc """
  Transpose the list.
  """
  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end

  @doc """

  """
  def try_split_map(_map, width) when width < 1, do: {:none}

  def try_split_map(map, width) do
    IO.inspect(width, label: "width")

    case test_split_map(map, width) do
      true -> {:ok, width}
      false -> try_split_map(map, width - 1)
    end
  end

  def test_split_map(map, width) do
    splits = Enum.map(map, fn line -> test_split(line, width) end)

    # Join up all the columns into one single one to find the difference
    left = Enum.map(splits, fn {a, _b} -> a end) |> Enum.flat_map(& &1)
    right = Enum.map(splits, fn {_a, b} -> b end) |> Enum.flat_map(& &1)

    # We are interested only when there is exactly one character difference
    diff_count(left, right) == 1
  end

  def test_split(line, width) do
    [left, right | _other] = Enum.chunk_every(line, width)
    min = Enum.min([Enum.count(left), Enum.count(right)])
    last_left = get_last(left, min)
    {last_left, right}
  end

  def diff_count(left, right, acc \\ 0)
  def diff_count([], [], acc), do: acc

  def diff_count([next1 | rem1], [next2 | rem2], acc) when next1 == next2 do
    diff_count(rem1, rem2, acc)
  end

  def diff_count([next1 | rem1], [next2 | rem2], acc) when next1 != next2 do
    diff_count(rem1, rem2, acc + 1)
  end

  def get_last(next, count), do: do_get_last(Enum.reverse(next), [], count)

  def do_get_last([], acc, _count), do: Enum.reverse(acc)
  def do_get_last(_remaining, acc, 0), do: Enum.reverse(acc)

  def do_get_last([next | remaining], acc, count),
    do: do_get_last(remaining, [next | acc], count - 1)
end
