using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 02, CodeType.Original)]
public class Day_02_Original : IPuzzle
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
			for (var i = start; i <= end; i++)
			{
				var val = i.ToString();
				if (val.Length % 2 != 0)
				{
					continue;
				}

				string chunk = val.Substring(0, val.Length / 2);
				string chunktwo = val.Substring(val.Length / 2);

				if(string.Equals(chunk, chunktwo))
				{
					sum += long.Parse(val);
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

			for (var i = start; i <= end; i++)
			{
				var val = i.ToString();

				for(var group = 1; group < val.Length; group++)
				{
					if (val.Length % group != 0) continue;

					var testval = i.ToString("N0", new NumberFormatInfo()
					{
						NumberGroupSizes = new[] { group },
						NumberGroupSeparator = ","
					});

					var testgroups = testval.Split(",");

					if(testgroups.All(q => string.Equals(q, testgroups[0])))
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
