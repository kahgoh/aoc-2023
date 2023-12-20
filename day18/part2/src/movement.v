module main

enum Direction {
	up
	right
	down
	left
}

struct Move {
	amount    int
	direction Direction
}

fn (move Move) move(from Point) Point {
	match move.direction {
		.left {
			return from.dec_x(move.amount)
		}
		.down {
			return from.inc_y(move.amount)
		}
		.right {
			return from.inc_x(move.amount)
		}
		.up {
			return from.dec_y(move.amount)
		}
	}
}
