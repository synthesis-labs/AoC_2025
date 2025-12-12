using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 04, CodeType.Original)]
public class Day_04_Original : IPuzzle
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
				int numAdj = 0;
				if (map[i][x] == '@')
				{
					if (i > 0)
					{
						if (map[i - 1][x] == '@')
						{
							numAdj++;
						}

						if (x > 0 && map[i - 1][x - 1] == '@')
						{
							numAdj++;
						}

						if (x < map[i].Length - 1 && map[i - 1][x + 1] == '@')
						{
							numAdj++;
						}
					}

					if (x > 0 && map[i][x - 1] == '@')
					{
						numAdj++;
					}

					if (i < map.Length - 1)
					{
						if (map[i + 1][x] == '@')
						{
							numAdj++;
						}

						if (x > 0 && map[i + 1][x - 1] == '@')
						{
							numAdj++;
						}
					}

					if (x < map[i].Length - 1 && map[i][x + 1] == '@')
					{
						numAdj++;
					}


					if (x < map[i].Length - 1 && i < map[i].Length - 1 && map[i + 1][x + 1] == '@')
					{
						numAdj++;
					}

					if (numAdj < 4)
					{
						sum++;
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
		for (var i = 0; i < input.Length; i++)
		{
			map[i] = new char[input[i].Length];
			for (var x = 0; x < input[i].Length; x++)
			{
				map[i][x] = input[i][x];
			}
		}

		for (var i = 0; i < map.Length; i++)
		{
			for (var x = 0; x < map[i].Length; x++)
			{
				int numAdj = 0;
				if (map[i][x] == '@')
				{
					if (i > 0)
					{
						if (map[i - 1][x] == '@')
						{
							numAdj++;
						}

						if (x > 0 && map[i - 1][x - 1] == '@')
						{
							numAdj++;
						}

						if (x < map[i].Length - 1 && map[i - 1][x + 1] == '@')
						{
							numAdj++;
						}
					}

					if (x > 0 && map[i][x - 1] == '@')
					{
						numAdj++;
					}

					if (i < map.Length - 1)
					{
						if (map[i + 1][x] == '@')
						{
							numAdj++;
						}

						if (x > 0 && map[i + 1][x - 1] == '@')
						{
							numAdj++;
						}
					}

					if (x < map[i].Length - 1 && map[i][x + 1] == '@')
					{
						numAdj++;
					}


					if (x < map[i].Length - 1 && i < map[i].Length - 1 && map[i + 1][x + 1] == '@')
					{
						numAdj++;
					}

					if (numAdj < 4)
					{
						sum++;
						map[i][x] = '.';
						i = 0;
						x = 0;
					}
				}
			}
		}

		return $"{sum}";
	}
}
