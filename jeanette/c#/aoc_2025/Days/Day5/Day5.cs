namespace aoc_2025.Days.Day2;

public class Day5
{
    public static void Part1()
    {
        var count = 0;
        // Split into fresh and available
        var split = HelperFunctions.SplitStringOnRegex(Day5_Input.Input, @"\n\n");

        var freshRanges = HelperFunctions.ConvertNewlinesToRow(split[0]);
        var available = HelperFunctions.ConvertNewlinesToRow(split[1]);

        foreach (var product in available)
        {
            var productInt = long.Parse(product);
            foreach (var range in freshRanges)
            {
                var rangeValues = range.Split("-").Select(long.Parse).ToArray();
                if (productInt >= rangeValues[0] && productInt <= rangeValues[1])
                {
                    count++;
                    break;
                }
            }
        }
        
        Console.WriteLine($"Result = [{count}]");
        Console.WriteLine();

    }

    public record Range(long start, long end);
    
    public static void Part2()
    {
        long count = 0;
        // Split into fresh and available
        var splitInput = HelperFunctions.SplitStringOnRegex(Day5_Input.Input, @"\n\n");

        var freshRanges = HelperFunctions.ConvertNewlinesToRow(splitInput[0]);
        
        var ranges = freshRanges.Select(x =>
        {
            var split = x.Split("-");
            return new Range(long.Parse(split[0]), long.Parse(split[1]));
        }).OrderBy(x => x.start).ToList();

        for(var i =1; i < ranges.Count; i++)
        {
            if (ranges[i].start <= ranges[i-1].end)
            {
                Console.WriteLine($"Matches with [{ranges[i-1].start}], [{ranges[i-1].end}]");
                Console.WriteLine($"Removing range [{ranges[i].start}, {ranges[i].end}]");
                
                if(ranges[i].end >= ranges[i-1].end)
                    ranges[i - 1] = new Range(ranges[i-1].start, ranges[i].end);
                    
                ranges.Remove(ranges[i]);
                i--;
            }
        }

        foreach (var i in ranges)
        {
            Console.WriteLine($"{(i.end - i.start) + 1}");
            count += (i.end - i.start) + 1;
        }
        
        Console.WriteLine($"Result = [{count}]");
        Console.WriteLine();
    }
}