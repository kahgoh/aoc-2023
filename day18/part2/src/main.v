module main

import arrays
import datatypes
import os
import strconv

struct Bounds {
mut:
	x1 int
	y1 int
	x2 int
	y2 int
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

	mut bounds := Bounds{}

	mut position := Point{
		x: 0
		y: 0
	}

	lines := os.read_lines(os.args[1])!
	mut parsed := []Move{len: lines.len}

	// Declare a variable to hold a direction to move
	mut direction := Direction.up

	// First part, find the dimension of the map
	for i, line in lines {
		fields := line.split(' ')
		code := fields[2]
		jump := int(strconv.parse_int(code.substr(2, 7), 16, 32)!)
		match code.substr(7, 8) {
			'0' {
				direction = Direction.right
				position = position.inc_x(jump)
			}
			'1' {
				direction = Direction.down
				position = position.inc_y(jump)
			}
			'2' {
				direction = Direction.left
				position = position.dec_x(jump)
			}
			'3' {
				direction = Direction.up
				position = position.dec_y(jump)
			}
			else {
				panic('Unknown direction')
			}
		}

		println('${code} ${direction} ${jump}')

		// Store the instruction to perform later
		parsed[i] = Move{
			amount: jump
			direction: direction
		}

		// Update the corners that we are tracking
		if position.x < bounds.x1 {
			bounds.x1 = position.x
		}
		if position.y < bounds.y1 {
			bounds.y1 = position.y
		}
		if position.x > bounds.x2 {
			bounds.x2 = position.x
		}
		if position.y > bounds.y2 {
			bounds.y2 = position.y
		}
	}

	println('bounds: ${bounds.x1},${bounds.y1} - ${bounds.x2},${bounds.y2}')

	dim := Dimension{
		width: bounds.x2 - bounds.x1
		height: bounds.y2 - bounds.y1
	}

	println('dimensions: ${dim.width} x ${dim.height}')

	// Position the starting point to collect the corners
	position = Point{
		x: -bounds.x1
		y: -bounds.y1
	}

	println('start drawing from: ${position.x},${position.y}')
	mut corners := datatypes.LinkedList[Point]{}
	corners.push(position)

	for _, move in parsed {
		position = move.move(position)
		corners.push(position)
	}

	// Now, find the "jumps" between the x and y points
	mut x_set := datatypes.Set[int]{}
	mut y_set := datatypes.Set[int]{}

	for c in corners {
		println('${c.x},${c.y}')
		x_set.add(c.x)
		y_set.add(c.y)
	}

	// TODO: Remove these points
	// Now extract and sort x and y points
	mut x_points := []int{}
	mut y_points := []int{}

	x_points.sort()
	y_points.sort()

	mut x_scales := calculate_scales(x_set)!
	println('x scale: ${x_scales}')
	mut y_scales := calculate_scales(y_set)!
	println('y scale: ${y_scales}')

	// Now prepare an array to map it
	mut mapping := [][]rune{len: y_scales.len, init: []rune{len: x_scales.len, init: `.`}}

	// Figure out where to start and translate it to the mapping somehow.
	mut cursor := Point{
		x: find_start_index(x_scales, -bounds.x1)
		y: find_start_index(y_scales, -bounds.y1)
	}
	println('start position: ${cursor.x},${cursor.y}')

	// Now we can start plotting
	for move in parsed {
		mut remaining := move.amount
		for 0 < remaining {
			match move.direction {
				.left {
					cursor = cursor.dec_x(1)
					remaining = remaining - x_scales[cursor.x]
				}
				.down {
					cursor = cursor.inc_y(1)
					remaining = remaining - y_scales[cursor.y]
				}
				.right {
					cursor = cursor.inc_x(1)
					remaining = remaining - x_scales[cursor.x]
				}
				.up {
					cursor = cursor.dec_y(1)
					remaining = remaining - y_scales[cursor.y]
				}
			}
			mapping[cursor.y][cursor.x] = `#`
		}
		if remaining != 0 {
			println('WARNING! Remaining (${remaining}) is not 0')
		}
	}

	// Add empty border around the map to support flood filling
	println('Initial map')
	print_map(mapping)

	// Finally flood fill and subtract the areas!
	mut queue := datatypes.Queue[Point]{}
	queue.push(Point{
		x: 0
		y: 0
	})

	for !queue.is_empty() {
		next := queue.pop()!
		if 0 <= next.x && next.x < x_scales.len && 0 <= next.y && next.y < y_scales.len {
			if mapping[next.y][next.x] == `.` {
				mapping[next.y][next.x] = `_`
				queue.push(next.inc_x(1))
				queue.push(next.inc_y(1))
				queue.push(next.dec_x(1))
				queue.push(next.dec_y(1))
			}
		}
	}

	println('After flood fill')
	print_map(mapping)

	mut volume := u64(0)
	for j, row in mapping {
		for i, col in row {
			if mapping[j][i] != `_` {
				volume = volume + (u64(x_scales[i]) * u64(y_scales[j]))
			}
		}
	}

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

fn calculate_scales(points datatypes.Set[int]) ![]int {
	mut copy := points.copy()
	mut scales := [1, 0]
	mut points_arr := []int{}

	for !copy.is_empty() {
		points_arr.prepend(copy.pop()!)
	}

	points_arr.sort()
	println('Points: ${points_arr}')

	for i in 1 .. points_arr.len {
		scales.prepend(points_arr[i] - points_arr[i - 1] - 1)
		scales.prepend(1)
	}
	scales.prepend(1)
	scales.reverse_in_place()
	return scales
}

fn find_start_index(scales []int, target int) int {
	mut index := 0
	mut to_go := target
	for to_go > 0 {
		index++
		to_go = to_go - scales[index]
	}
	println('Remaining: ${to_go}')
	return index + 1
}
