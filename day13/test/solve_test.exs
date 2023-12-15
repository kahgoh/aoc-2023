defmodule SolveTest do
  use ExUnit.Case
  doctest Solve

  test "greets the world" do
    assert Solve.hello() == :world
  end
end
