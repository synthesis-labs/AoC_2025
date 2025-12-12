using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 09, CodeType.Original)]
public class Day_09_Original : IPuzzle
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
		List<(long x, long y)> red = new List<(long x, long y)>();
		foreach(var lines in input)
		{
			var splits = lines.Split(",");
			red.Add((long.Parse(splits[0]), long.Parse(splits[1])));
		}

		for (int x = 0; x < red.Count; x++)
		{
			for (int y = 0; y < red.Count; y++)
			{
				// 2147407055
				long val = (Math.Abs(red[x].x - red[y].x) + 1) * (Math.Abs(red[x].y - red[y].y) + 1);

				if (val > sum)
				{
					sum = val;
				}
			}
		}

		return $"{sum}";
	}
	
	private string ProcessInput2(string[] input)
	{
		long sum = 0;
		List<(long x, long y)> red = new List<(long x, long y)>();
		HashSet<(long x, long y)> seenValid =  new HashSet<(long, long)>();
		foreach(var lines in input)
		{
			var splits = lines.Split(",");
			red.Add((long.Parse(splits[0]), long.Parse(splits[1])));
		}

		for (int indexone = 0; indexone < red.Count; indexone++)
		{
			var (xOne, yOne) = red[indexone];
			for (int indextwo = 0; indextwo < red.Count; indextwo++)
			{
				var (xTwo, yTwo) = red[indextwo];
				if (xOne > xTwo || (xOne == xTwo && yOne > yTwo))
				{
					continue;
				}

				long top = Math.Min(xOne, xTwo);
				long bot = Math.Max(xOne, xTwo);
				long left = Math.Min(yOne, yTwo);
				long right = Math.Max(yOne, yTwo);

				bool canFill = true;

				for (int i = 0; i < red.Count; i++)
				{
					var (rowOne, colOne) = red[i];
					var (rowTwo, colTwo) = red[(i+1) % red.Count];
					if (!(Math.Max(rowOne, rowTwo) <= top || bot <= Math.Min(rowOne, rowTwo) ||
						Math.Max(colOne, colTwo) <= left || right <= Math.Min(colOne, colTwo)))
					{
						canFill = false;
						break;
					}
				}

				if (canFill)
				{
					sum = Math.Max(sum, (bot - top + 1) * (right - left + 1));
				}
			}
		}
		return $"{sum}";
	}
}
