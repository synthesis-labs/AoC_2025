using System.Collections.Concurrent;
using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 11, CodeType.Original)]
public class Day_11_Original : IPuzzle
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
		Dictionary<string, List<string>> nodes = [];
		Queue<string> queue = new();
		foreach (var lines in input)
		{
			var splits = lines.Split(':');
			var start = splits[0];
			var end = splits[1].Split(' ', StringSplitOptions.RemoveEmptyEntries).ToList();
			_ = nodes.TryAdd(start, end);
		}

		queue.Enqueue("you");

		while (queue.TryDequeue(out var current))
		{
			if (nodes[current].Contains("out"))
			{
				sum++;
				continue;
			}

			foreach (var node in nodes[current])
			{
				queue.Enqueue(node);
			}
		}
		return $"{sum}";
	}

	private string ProcessInput2(string[] input)
	{
		long sum = 0;
		Dictionary<string, HashSet<string>> nodes = [];
		Dictionary<(string cur, bool fft, bool dac), long> seenPath = [];
		foreach (var lines in input)
		{
			var splits = lines.Split(':');
			var start = splits[0];
			var end = splits[1].Split(' ', StringSplitOptions.RemoveEmptyEntries).ToHashSet();
			_ = nodes.TryAdd(start, end);
		}

		long DFS(string cur, bool fft, bool dac)
		{
			long res = 0;
			if (cur == "dac")
			{
				dac = true;
			}

			if (cur == "fft")
			{
				fft = true;
			}

			foreach(var node in nodes[cur])
			{
				if (node == "out")
				{
					if (dac && fft)
					{
						res++;
					}
					else
					{
						continue;
					}
				}
				else
				{
					if (seenPath.ContainsKey((node, fft, dac)))
					{
						res += seenPath[(node, fft, dac)];
					}
					else
					{
						res += DFS(node, fft, dac);
					}
				}
			}

			if (!seenPath.TryAdd((cur, fft, dac), res))
			{
				seenPath[(cur, fft, dac)] = res;
			}
			return res;
		}

		sum = DFS("svr", false, false);

		return $"{sum}";
	}
}
