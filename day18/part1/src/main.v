module main

import datatypes
import os

struct Point {
mut:
	x int
	y int
}

struct Dimension {
	width  int
	height int
}

fn main() {
	if os.args.len < 2 {
		println('Missing file name')
		return
	}
	println('File name is ' + os.args[1])

	mut top_left := Point{
		x: 0
		y: 0
	}

	mut bottom_right := Point{
		x: 0
		y: 0
	}

	mut position := Point{
		x: 0
		y: 0
	}

	lines := os.read_lines(os.args[1])!

	// First part, find the dimension of the map
	for _, line in lines {
		println('Read ${line}')
		fields := line.split(' ')
		jump := fields[1].int()
		position = match fields[0] {
			'U' {
				Point{
					x: position.x
					y: position.y - jump
				}
			}
			'R' {
				Point{
					x: position.x + jump
					y: position.y
				}
			}
			'D' {
				Point{
					x: position.x
					y: position.y + jump
				}
			}
			'L' {
				Point{
					x: position.x - jump
					y: position.y
				}
			}
			else {
				panic('Unexpected value ${fields}[0]')
			}
		}

		if position.x < top_left.x {
			top_left.x = position.x
		}
		if position.y < top_left.y {
			top_left.y = position.y
		}
		if position.x > bottom_right.x {
			bottom_right.x = position.x
		}
		if position.y > bottom_right.y {
			bottom_right.y = position.y
		}
	}

	println('top left: ${top_left.x},${top_left.y}')
	println('bottom right: ${bottom_right.x},${bottom_right.y}')

	dim := Dimension{
		width: bottom_right.x - top_left.x + 1
		height: bottom_right.y - top_left.y + 1
	}

	println('dimensions: ${dim.width} x ${dim.height}')

	// Now we can build the map. We add a border around to enable flood filling later.
	mut mapping := [][]rune{len: dim.height + 2, init: []rune{len: dim.width + 2, init: `.`}}

	// Position the starting point
	position.x = (-top_left.x) + 1
	position.y = (-top_left.y) + 1

	println('start drawing from: ${position.x},${position.y}')
	for _, line in lines {
		fields := line.split(' ')
		jump := fields[1].int()
		match fields[0] {
			'U' {
				for j in 0 .. jump + 1 {
					mapping[position.y - j][position.x] = `#`
				}
				position.y = position.y - jump
			}
			'R' {
				for i in 0 .. jump + 1 {
					mapping[position.y][position.x + i] = `#`
				}
				position.x = position.x + jump
			}
			'D' {
				for j in 0 .. jump + 1 {
					mapping[position.y + j][position.x] = `#`
				}
				position.y = position.y + jump
			}
			'L' {
				for i in 0 .. jump + 1 {
					mapping[position.y][position.x - i] = `#`
				}
				position.x = position.x - jump
			}
			else {
				panic('Unexpected value ${fields}[0]')
			}
		}
	}

	// Print out map for diagnostics
	println('Map')
	print_map(mapping)

	// Flood fill
	mut queue := datatypes.Queue[Point]{}
	queue.push(Point{
		x: 0
		y: 0
	})

	mut outside := 0
	for !queue.is_empty() {
		next := queue.pop()!
		if 0 <= next.x && next.x < dim.width + 2 && 0 <= next.y && next.y < dim.height + 2 {
			if mapping[next.y][next.x] == `.` {
				outside++
				mapping[next.y][next.x] = `_`
				queue.push(Point{ x: next.x - 1, y: next.y })
				queue.push(Point{ x: next.x + 1, y: next.y })
				queue.push(Point{ x: next.x, y: next.y - 1 })
				queue.push(Point{ x: next.x, y: next.y + 1 })
			}
		}
	}

	println('After flooding')
	print_map(mapping)
	println('Outside count: ${outside}')

	volume := (dim.width + 2) * (dim.height + 2) - outside
	println('Volume: ${volume}')
}

fn print_map(mapping [][]rune) {
	for _, row in mapping {
		for _, item in row {
			print('${item}')
		}
		println('')
	}
}
