using System.Collections.Concurrent;
using System.Globalization;
using Microsoft.Z3;
namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 10, CodeType.Original)]
public class Day_10_Original : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Lines);

		var part2 = ProcessInput2(input.Lines);

		return (part1, part2);
	}

	private string ProcessInput1(string[] input)
	{
		ConcurrentBag<long> sum = [];
		input.AsParallel().ForEach(lines =>
		{
			long minpresses = long.MaxValue;
			var priorityQueue = new PriorityQueue<(int[] buttons, bool[] lights), int>();
			var splits = lines.Split(" ");
			var finalTarget = splits.First().Trim('[').Trim(']');
			var buttonIndexes = splits.Skip(1).SkipLast(1).ToList();
			bool[] target = new bool[finalTarget.Length];
			int[][] buttons = new int[buttonIndexes.Count][];
			for (int x = 0; x < finalTarget.Length; x++)
			{
				if (finalTarget[x] == '#')
				{
					target[x] = true;
				}
			}

			for (int x = 0; x < buttonIndexes.Count; x++)
			{
				var buttonoptions = buttonIndexes[x].Split(',');
				buttons[x] = new int[buttonoptions.Length];
				for (int y = 0; y < buttonoptions.Length; y++)
				{
					buttonoptions[y] = buttonoptions[y].Trim('(').Trim(')');
					if (int.TryParse(buttonoptions[y], out int validButton))
					{
						buttons[x][y] = validButton;
					}
				}
				priorityQueue.Enqueue((buttons[x], new bool[finalTarget.Length]), 0);
			}


			while (priorityQueue.TryDequeue(out var element, out int presses))
			{
				if (Enumerable.SequenceEqual(element.lights, target))
				{
					if (minpresses >= presses)
					{
						minpresses = presses;
						break;
					}
				}

				var newLights = new bool[element.lights.Length];
				for (int x = 0; x < element.lights.Length; x++)
				{
					newLights[x] = element.lights[x];
				}
				for (int x = 0; x < element.buttons.Length; x++)
				{
					newLights[element.buttons[x]] = !newLights[element.buttons[x]];
				}

				int pressCount = presses + 1;
				for (int x = 0; x < buttons.Length; x++)
				{
					priorityQueue.Enqueue((buttons[x], newLights), pressCount);
				}
			}

			sum.Add(minpresses);
		});
		return $"{sum.Sum()}";
	}

	private string ProcessInput2(string[] input)
	{
		ConcurrentBag<long> sum = [];
		input.AsParallel().ForEach(lines =>
		{
			using var context = new Context();
			using var optimizer = context.MkOptimize();
			var splits = lines.Split(" ");
			var joltages = splits.Last()[1..^1].Split(',').Select(int.Parse).ToArray();
			var buttonIndexes = splits.Skip(1).SkipLast(1).ToList();
			List<HashSet<int>> buttons = [];

			int index = 0;
			while (index < buttonIndexes.Count)
			{
				buttons.Add(buttonIndexes[index][1..^1].Split(',').Select(int.Parse).ToHashSet());
				index++;
			}

			var presses = Enumerable.Range(0, buttons.Count)
				.Select(i => context.MkIntConst($"p{i}"))
				.ToArray();

			foreach (var press in presses)
			{
				optimizer.Add(context.MkGe(press, context.MkInt(0)));
			}


			for (int x = 0; x < joltages.Length; x++)
			{
				var affect = presses.Where((_, b) => buttons[b].Contains(x)).ToArray();

				if (affect.Length > 0)
				{
					var total = affect.Length == 1 ? affect[0] : context.MkAdd(affect);
					optimizer.Add(context.MkEq(total, context.MkInt(joltages[x])));
				}
			}

			optimizer.MkMinimize(presses.Length == 1 ? presses[0] : context.MkAdd(presses));
			optimizer.Check();

			var result = optimizer.Model;

			sum.Add(presses.Sum(x => ((IntNum)result.Evaluate(x, true)).Int64));
		});
		return $"{sum.Sum()}";
	}
}
