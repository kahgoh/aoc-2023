data = Parse.read("sample.data")

part1 = Solve.solve(data)
IO.inspect(part1, label: "Part 1")

part2 = Smudge.solve(data)
IO.inspect(part2, label: "Part 2")
