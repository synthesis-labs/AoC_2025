using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 05, CodeType.Original)]
public class Day_05_Original : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Lines);

		var part2 = ProcessInput2(input.Lines);

		return (part1, part2);
	}

	private string ProcessInput1(string[] input)
	{
		int sum = 0;
		List<(long start, long end)> freshRanges = new List<(long start, long end)>();
		foreach (var line in input)
		{
			var split = line.Split('-');
			if (split.Length == 2)
			{
				freshRanges.Add((long.Parse(split[0]), long.Parse(split[1])));
			}
			else
			{
				long.TryParse(split[0], out long checkval);

				if (checkval > 0)
				{
					if (freshRanges.Any(q => checkval >= q.start && checkval <= q.end))
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
		List<(long start, long end)> freshRanges = new List<(long start, long end)>();
		List<(long start, long end)> fullRanges = new List<(long start, long end)>();
		foreach (var line in input)
		{
			var split = line.Split('-');
			if (split.Length == 2)
			{
				freshRanges.Add((long.Parse(split[0]), long.Parse(split[1])));
			}
		}
		freshRanges = freshRanges.OrderBy(q => q.start).ToList();
		foreach (var range in freshRanges)
		{
			if (fullRanges.Count == 0)
			{
				fullRanges.Add(range);
			}
			else
			{
				var prev = fullRanges.Last();
				if (range.start <= prev.end)
				{
					fullRanges[fullRanges.Count - 1] = (prev.start, Math.Max(prev.end, range.end));
				}
				else
				{
					fullRanges.Add(range);
				}
			}
		}

		sum = fullRanges.Sum(r => r.end - r.start + 1);


		return $"{sum}";
	}
}
