defmodule Parse do
  def read(file_name) do
    File.stream!(file_name, [:utf8], :line)
    |> Enum.chunk_while([], &chunk_input/2, fn acc -> {:cont, Enum.reverse(acc), []} end)
  end

  defp chunk_input(line, acc) do
    case String.trim(line) do
      "" -> {:cont, Enum.reverse(acc), []}
      trimmed -> {:cont, [String.to_charlist(trimmed) | acc]}
    end
  end
end
