using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 02, CodeType.Fastest)]
public class Day_02_Fastest : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Text);

		var part2 = ProcessInput2(input.Text);

		return (part1, part2);
	}

	private string ProcessInput1(string input)
	{
		long sum = 0;
		var parts = input.Split(",");

		foreach (var part in parts)
		{
			var split = part.Split("-");
			_ = long.TryParse(split[0], out var start);
			_ = long.TryParse(split[1], out var end);
			for (long i = start; i <= end; i++)
			{
				long val = (long)(Math.Log10(i) + 1);
				if (val % 2 != 0)
				{
					continue;
				}

				long div = (long)Math.Pow(10, (long)(val / 2));

				if (i / div == i % div)
				{
					sum += i;
				}
			}
		}

		return $"{sum}";
	}

	private string ProcessInput2(string input)
	{
		long sum = 0;
		var parts = input.Split(",");

		foreach (var part in parts)
		{
			var split = part.Split("-");
			_ = long.TryParse(split[0], out var start);
			_ = long.TryParse(split[1], out var end);

			for (long i = start; i <= end; i++)
			{
				long val = (long)(Math.Log10(i) + 1);

				for (long cur = 1; cur <= (long)(val / 2); cur++)
				{
					long patt = (long)(i / Math.Pow(10, val - cur));
					long div = (long)(Math.Pow(10, cur) - 1);
					long mul = (long)(Math.Pow(10, val) - 1);

					if(i * div == patt * mul)
					{
						sum += i;
						break;
					}
				}
			}
		}

		return $"{sum}";
	}
}
