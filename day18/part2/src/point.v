module main

struct Point {
	x int
	y int
}

fn (p Point) inc_x(amount int) Point {
	return Point{
		x: p.x + amount
		y: p.y
	}
}

fn (p Point) dec_x(amount int) Point {
	return Point{
		x: p.x - amount
		y: p.y
	}
}

fn (p Point) inc_y(amount int) Point {
	return Point{
		x: p.x
		y: p.y + amount
	}
}

fn (p Point) dec_y(amount int) Point {
	return Point{
		x: p.x
		y: p.y - amount
	}
}
