using System.Globalization;

namespace AdventOfCode.Puzzles._2025;

[Puzzle(2025, 08, CodeType.Original)]
public class Day_08_Original : IPuzzle
{
	public (string, string) Solve(PuzzleInput input)
	{
		var part1 = ProcessInput1(input.Lines, 1000);

		var part2 = ProcessInput2(input.Lines);

		return (part1, part2);
	}

	private string ProcessInput1(string[] input, int iterations)
	{
		long sum = 0;
		List<(long x, long y, long z)> nodes = new List<(long x, long y, long z)>();
		List<HashSet<(long x, long y, long z)>> connections = [];
		foreach(var lines in input)
		{
			var split =  lines.Split(',');
			nodes.Add((long.Parse(split[0]), long.Parse(split[1]), long.Parse(split[2])));
		}

		int merges = 0;
		PriorityQueue<((long x, long y, long z) a, (long x, long y, long z) b), double> queue = new();

		List<((long x, long y, long z) a, (long x, long y, long z) b)> pairs = [];
		List<int[]> iterationList = new List<int[]>();
		int[] result = new int[2];
		Stack<int> stack = new(2);
		stack.Push(0);
		while (stack.Count > 0)
		{
			int index = stack.Count - 1;
			int value = stack.Pop();
			while (value < nodes.Count)
			{
				result[index++] = value++;
				stack.Push(value);
				if (index != 2) continue;
				iterationList.Add((int[])result.Clone());
				break;
			}
		}

		foreach (int[] j in iterationList)
		{
			pairs.Add((nodes.ElementAt(j[0]), (nodes.ElementAt(j[1]))));
		}
		
		foreach (var box in pairs)
		{
			var dist = Math.Sqrt(Math.Pow(box.a.x - box.b.x, 2) + Math.Pow(box.a.y - box.b.y, 2) + Math.Pow(box.a.z - box.b.z, 2));
			queue.Enqueue(box, dist);
		}

		while (queue.TryDequeue(out var cur, out double _))
		{
			if (merges == iterations)
			{
				sum = connections.OrderByDescending(x => x.Count()).Take(3).Aggregate(1, (a, b) => a *= b.Count);
			}

			(var a, var b) = cur;
			var existing = connections.Where(x => x.Contains(a) || x.Contains(b));

			if (existing.Count() == 0)
			{
				HashSet<(long x, long y, long z)> newConn = new();
				newConn.Add(a);
				newConn.Add(b);
				connections.Add(newConn);
			}
			else if (existing.Count() == 1)
			{
				var conn = existing.First();
				if (conn.Contains(a) && !conn.Contains(b))
				{
					conn.Add(b);
				}
				else
				{
					conn.Add(a);
				}
			}
			else if (existing.Count() == 2)
			{
				existing.First().UnionWith(existing.Last());
				connections.Remove(existing.Last());
			}

			merges++;
		}
		
		return $"{sum}";
	}

	private string ProcessInput2(string[] input)
	{
		long sum = 0;
		List<(long x, long y, long z)> nodes = new List<(long x, long y, long z)>();
		List<HashSet<(long x, long y, long z)>> connections = [];
		foreach(var lines in input)
		{
			var split =  lines.Split(',');
			nodes.Add((long.Parse(split[0]), long.Parse(split[1]), long.Parse(split[2])));
		}

		PriorityQueue<((long x, long y, long z) a, (long x, long y, long z) b), double> queue = new();

		List<((long x, long y, long z) a, (long x, long y, long z) b)> pairs = [];
		List<int[]> iterationList = new List<int[]>();
		int[] result = new int[2];
		Stack<int> stack = new(2);
		stack.Push(0);
		while (stack.Count > 0)
		{
			int index = stack.Count - 1;
			int value = stack.Pop();
			while (value < nodes.Count)
			{
				result[index++] = value++;
				stack.Push(value);
				if (index != 2) continue;
				iterationList.Add((int[])result.Clone());
				break;
			}
		}

		foreach (int[] j in iterationList)
		{
			pairs.Add((nodes.ElementAt(j[0]), (nodes.ElementAt(j[1]))));
		}
		
		foreach (var box in pairs)
		{
			var dist = Math.Sqrt(Math.Pow(box.b.x - box.a.x, 2) + Math.Pow(box.b.y - box.a.y, 2) + Math.Pow(box.b.z - box.a.z, 2));
			queue.Enqueue(box, dist);
		}

		while (queue.TryDequeue(out var cur, out double _))
		{
			(var a, var b) = cur;
			var existing = connections.Where(x => x.Contains(a) || x.Contains(b)).ToList();

			if (existing.Count == 0)
			{
				HashSet<(long x, long y, long z)> newConn = new();
				newConn.Add(a);
				newConn.Add(b);
				connections.Add(newConn);
			}
			else if (existing.Count == 1)
			{
				var conn = existing.First();
				if (conn.Contains(a) && !conn.Contains(b))
				{
					conn.Add(b);
				}
				else
				{
					conn.Add(a);
				}
			}
			else if (existing.Count == 2)
			{
				existing.First().UnionWith(existing.Last());
				connections.Remove(existing.Last());
			}

			if (connections.Count == 1 && connections.First().Count == nodes.Count)
			{
				sum = a.x * b.x;
				break;
			}
		}
		
		return $"{sum}";
	}
}
