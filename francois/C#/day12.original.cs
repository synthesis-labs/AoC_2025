using System.Collections.Concurrent;
using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 12, CodeType.Original)]
public class Day_12_Original : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Lines);

		var part2 = ProcessInput2(input.Text);

		return (part1, part2);
	}

	private string ProcessInput1(string[] input)
	{
		long sum = 0;
		var shapes = new List<List<string>>();
		var regions = new List<(int width, int height, List<int> counts)>();
		int index = 0;

		while (index < input.Length)
		{
			var line = input[index].Trim();
			if (string.IsNullOrEmpty(line))
			{
				index++;
				continue;
			}

			if (line.Contains("x"))
			{
				break;
			}

			index++;
			var rows = new List<string>();
			while (index < input.Length)
			{
				var l = input[index];
				if (string.IsNullOrWhiteSpace(l))
				{
					index++;
					break;
				}
				if (l.All(c => c == '.' || c == '#'))
				{
					rows.Add(l);
					index++;
				}
				else
				{
					break;
				}
			}
			shapes.Add(rows);
		}

		var widths = new HashSet<int>(shapes.Select(shape => shape.Max(row => row.Length)));
		var heights = new HashSet<int>(shapes.Select(q => q.Count));
		int boxW = widths.First();
		int boxH = heights.First();
		if ((widths.Count == 1 && heights.Count == 1))
		{

			while (index < input.Length)
			{
				var line = input[index].Trim();
				index++;

				if (string.IsNullOrWhiteSpace(line))
				{
					continue;
				}

				var parts = line.Split(":");
				var l = parts[0];
				var r = parts[1];

				var wh = l.Split('x');
				int w = int.Parse(wh[0]);
				int h = int.Parse(wh[1]);

				var counts = r.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).ToList();

				regions.Add((w, h, counts));
			}
		}

		foreach (var region in regions)
		{
			int total = region.counts.Sum();
			int capacity = (region.width / boxW) * (region.height / boxH);

			if(total <= capacity)
			{
				sum++;
			}
		}

		return $"{sum}";
	}

	private string ProcessInput2(string input)
	{
		long sum = 0;
		foreach (var lines in input)
		{
		}

		return $"{sum}";
	}
}
