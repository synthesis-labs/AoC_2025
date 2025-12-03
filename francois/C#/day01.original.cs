namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 01, CodeType.Original)]
public class Day_01_Original : IPuzzle
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
		var cur = 50;
		foreach (var line in input)
		{
			var segment = line;
			var rot = line[0];
			int.TryParse(line.Split(rot)[1], out var val);
			
			switch (rot)
			{
				case 'L':
					cur -= val;
					break;
				case 'R':
					cur += val;
					break;
				default:
					break;
			}

			if(cur % 100 == 0)
			{
				sum++;
			}
		}

		return $"{sum}";
	}

	private string ProcessInput2(string[] input)
	{
		int sum = 0;
		var cur = 50;
		foreach (var line in input)
		{
			var prev = cur;
			var segment = line;
			var rot = line[0];
			int.TryParse(line.Split(rot)[1], out var val);

			switch (rot)
			{
				case 'L':
					for(var i = 0; i < val; i++)
					{
						cur--;
						if (cur == 0)
						{
							sum++;
						}
						if (cur < 0)
						{
							cur = 99;
						}
					}
					break;
				case 'R':
					for (var i = 0; i < val; i++)
					{
						cur++;
						if (cur == 0)
						{
							sum++;
						}
						if (cur > 99)
						{
							cur = 0;
							sum++;
						}
					}
					break;
				default:
					break;
			}
		}

		return $"{sum}";
	}
}
