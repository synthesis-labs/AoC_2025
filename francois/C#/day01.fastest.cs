namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 01, CodeType.Fastest)]
public class Day_01_Fastest : IPuzzle
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
			var rot = line[0];
			_ = int.TryParse(line.Split(rot)[1], out var val);
			
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
		var newPos = 0;
		foreach (var line in input)
		{
			var rot = line[0];
			_ = int.TryParse(line.Split(rot)[1], out var val);

			var shift = val % 100;
			sum += (val / 100);
			
			if (rot == 'L')
			{
				newPos = (cur - shift + 100) % 100;
				if ((newPos > cur || newPos == 0) && cur != 0)
				{
					sum++;
				}
			}
			else
			{
				newPos = (cur + shift) % 100;
				if (newPos < cur)
				{
					sum += 1;
				}
			}
			cur = newPos;
		}

		return $"{sum}";
	}
}
