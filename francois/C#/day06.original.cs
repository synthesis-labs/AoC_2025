using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 06, CodeType.Original)]
public class Day_06_Original : IPuzzle
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
		Dictionary<long, List<long>> valuemap = new Dictionary<long, List<long>>();
		Dictionary<long, string> operandmap = new Dictionary<long, string>();

		foreach (var line in input)
		{
			var splits = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
			for (long x = 0; x < splits.Length; x++)
			{
				if (long.TryParse(splits[x], out var value))
				{
					if (valuemap.ContainsKey(x))
					{
						valuemap[x].Add(value);
					}
					else
					{
						valuemap.Add(x, new List<long>() { value });
					}
				}
				else
				{
					if (operandmap.ContainsKey(x))
					{
						operandmap[x] = splits[x];
					}
					else
					{
						operandmap.Add(x, splits[x] );
					}
				}
			}
		}

		for (var x = 0; x < valuemap.Keys.Count; x++)
		{
			switch (operandmap[x])
			{
				case "+":
					sum += valuemap[x].Sum();
					break;
				case "*":
					sum += valuemap[x].Aggregate((a, b) => a * b);
					break;
				default:
					break;
			}
		}

		return $"{sum}";
	}

	private string ProcessInput2(string[] input)
	{
		long sum = 0;
		Dictionary<long, List<string>> valuemap = new Dictionary<long, List<string>>();
		List<int> operationIndexes = new List<int>();
		string operations = input.Last();
		for (int x = 0; x < operations.Length; x++)
		{
			if (operations[x] != ' ')
			{
				operationIndexes.Add(x);
			}
		}

		for (int x = 0; x < operationIndexes.Count; x++)
		{
			int operationIndex = operationIndexes[x];
			int nextInd = (x + 1 < operationIndexes.Count) ? operationIndexes[x + 1] : operations.Length + 1;
			List<string> currentProblem = new List<string>();
			foreach (var line in input)
			{
				currentProblem.Add(line.Substring(operationIndex, nextInd - 1 - operationIndex));
			}
			valuemap.Add(x, currentProblem);
		}
		
		
		for (var x = 0; x < valuemap.Keys.Count; x++)
		{
			string curOperation = valuemap[x].Last().Trim();
			List<long> variables = new List<long>();
			for (var y = 0; y < valuemap[x].First().Length; y++)
			{
				string curVar = string.Empty;
				for (var z = 0; z < valuemap[x].Count - 1; z++)
				{
					curVar += valuemap[x][z][y];
				}

				if (long.TryParse(curVar, out var value))
				{
					variables.Add(value);
				}
			}
			switch (curOperation)
			{
				case "+":
					sum += variables.Sum();
					break;
				case "*":
					sum += variables.Aggregate((a, b) => a * b);
					break;
				default:
					break;
			}
		}

		return $"{sum}";
	}
}
