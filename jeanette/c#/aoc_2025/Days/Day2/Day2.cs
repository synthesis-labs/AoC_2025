using System.Diagnostics;

namespace aoc_2025.Days.Day2;

public class Day2
{
    public static void Part1()
    {
        var input = HelperFunctions.CommaSeperatedToArray(Day2_Input.Input);
        long count = 0;

        foreach (var range in input)
        {
            var rangePoints = range.Split("-");
            var start = long.Parse(rangePoints[0]);
            var end = long.Parse(rangePoints[1]);
            
            Console.WriteLine($"O Start [{start}] - O End [{end}]");

            if (start.ToString().Length % 2 != 0)
            {
                start = (long)Math.Truncate(Math.Pow(10, start.ToString().Length));
            }

            if (end.ToString().Length % 2 != 0)
            {
                var test = end.ToString().Length + 1;
                var test2 = Math.Pow(10, end.ToString().Length);
                end = (long)Math.Truncate(Math.Pow(10, end.ToString().Length-1));
                end = end -1;
            }

            foreach (var number in HelperFunctions.LongRange(start, end))
            {
                // Console.Write($"Start [{start}] - End [{end}] - number [{number}] ");
                var value = number.ToString();
                // Console.Write($" - len: [{value.Length}] - mod [{value.Length % 2}]");
                if (value.Length % 2 == 0)
                {
                    var middle = (value.Length) / 2;
                    var first = value[..middle];
                    var second = value[middle..];
                    
                    if (first == second)
                    {
                        // Console.Write($" - first [{first}] = second [{second}]");
                        count+= number;
                    }
                }
                Console.WriteLine();
                
            }
        }
        
        Console.WriteLine($"The result {count}");
    }
    
    public static void Part2()
    {
        var input = HelperFunctions.CommaSeperatedToArray(Day2_Input.Input);
        long count = 0;

        foreach (var range in input)
        {
            var rangePoints = range.Split("-");
            var start = long.Parse(rangePoints[0]);
            var end = long.Parse(rangePoints[1]);
            
            Console.WriteLine();
            Console.Write($"Start [{start}] - End [{end}] -> ");

            foreach (var number in HelperFunctions.LongRange(start, end))
            {
                var numberStr = number.ToString();
                var len = numberStr.Length;

                if (len == 1)
                {
                    continue;
                }
                
                var test2 = (decimal)len / 2;
                var possibleRounds = Math.Ceiling(test2);
                
                for (var i = 1; i <= possibleRounds; i++)
                {
                    //Check if number can be evenly split
                    if(len % i != 0)
                        continue;

                    var section = numberStr[..i];
                    var toCompare = numberStr.Substring(i, i);

                    if (section != toCompare) continue;

                    var newI = i;
                    var match = false;
                    while (section == toCompare)
                    {
                        if (newI + i >= len)
                        {
                            Console.Write($"{number}, ");
                            match = true;
                            break;
                        }
                        else
                        {
                            newI += i;
                            toCompare = numberStr.Substring(newI, i);
                        }
                    }

                    if (match)
                    {
                        count += number;
                        break;
                    }
                }
            }
            Console.Write($" current count {count}");
        }

        Console.WriteLine();
        Console.WriteLine($"The result {count}");
    }
    
}