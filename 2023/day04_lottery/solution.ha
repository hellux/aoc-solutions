use fmt;
use io;
use os;
use bufio;
use types;
use bytes;
use strings;

export fn main() void = {
	let in = bufio::newscanner(os::stdin, types::SIZE_MAX);
	let copies: [256]int = [1...];
	let i = 0;
	let part1 = 0;
	let part2 = 0;
	for (true) {
		match (bufio::scan_line(&in)) {
		case let line: const str =>
			let line = strings::toutf8(line);
			if (len(line) == 0) continue;

			let tok = bytes::tokenize(line, [':']);
			bytes::next_token(&tok);
			const numbers = bytes::next_token(&tok) as []u8;

			let tok = bytes::tokenize(numbers, [' ', '|']);
			const nums_win = bytes::next_token(&tok) as []u8;
			const nums_player = bytes::next_token(&tok) as []u8;

			let matches = 0;
			for (let j: size = 0; j < len(nums_player); j += 3) {
				if (bytes::contains(nums_win, nums_player[j..j+3])) {
					matches += 1;
				};
			};
			const score = if (matches <= 1) {
				yield matches;
			} else {
				yield 1 << (matches-1);
			};
			part1 += score;
			for (let j = 0; j < matches; j += 1) {
				copies[i+1+j] += copies[i];
			};
			part2 += copies[i];
		case io::EOF =>
			break;
		};
		i += 1;
	};

	fmt::println(part1)!;
	fmt::println(part2)!;
};
