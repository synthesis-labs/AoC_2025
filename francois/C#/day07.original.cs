using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 07, CodeType.Original)]
public class Day_07_Original : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Lines);

		var part2 = ProcessInput2(input.Lines);

		return (part1, part2);
	}

	private string ProcessInput1(string[] input)
	{
		long sum = 0;
		char[][] map = new char[input.Length][];
		for(var i = 0; i < input.Length; i++)
		{
			map[i] = new char[input[i].Length];
			for(var x = 0; x < input[i].Length; x++)
			{
				map[i][x] = input[i][x];
			}
		}
		
		for(var i = 0; i < map.Length; i++)
		{
			for(var x = 0; x < map[i].Length; x++)
			{
				if (map[i][x] == 'S')
				{
					map[i + 1][x] = '|';
				}

				if (i > 0)
				{
					if (map[i][x] == '^' && map[i - 1][x] == '|')
					{
						sum++;
						if (x < map[i].Length - 1)
						{
							map[i][x + 1] = '|';
						}

						if (x > 0)
						{
							map[i][x - 1] = '|';
						}
					}
					else if (map[i-1][x] == '|')
					{
						map[i][x] = '|';
					}
				}
			}
		}
		
		return $"{sum}";
	}

	private string ProcessInput2(string[] input)
	{
		long sum = 0;
		char[][] map = new char[input.Length][];
		for(var i = 0; i < input.Length; i++)
		{
			map[i] = new char[input[i].Length];
			for(var x = 0; x < input[i].Length; x++)
			{
				map[i][x] = input[i][x];
			}
		}
		long[] memo = [.. Enumerable.Repeat(1, map[0].Length + 1)];

		for (var y = map.Length - 1; y >= 0; --y)
		{
			for (int x = 1; x <= map[0].Length; ++x)
			{
				if (map[y][x - 1] == '^')
				{
					memo[x] = memo[x - 1] + memo[x + 1];
				}
			}
		}
		sum = memo[map[0].IndexOf('S') + 1];
		return $"{sum}";
	}
}
