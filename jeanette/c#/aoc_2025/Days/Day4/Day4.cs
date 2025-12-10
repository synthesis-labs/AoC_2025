using System.IO.Compression;

namespace aoc_2025.Days.Day2;

public class Day4
{
    private static int _multiplier = 0;
    public static void Part1()
    {
        var input = RollCordinates(Day4_Input.Input);
        var count = 0;
        var matches = "";
        
        foreach (var position in input)
        {
            var result = Test(input, position);
            count += result;
            //if (result == 1) matches = string.Concat(matches, ($", [{position}]"));
            
            //Console.WriteLine();
        }
        
        //Console.WriteLine($"Matches = {matches}");
        Console.WriteLine($"Result = [{count}]");
    }
    
    public static void Part2()
    {
        var input = RollCordinates(Day4_Input.Input);
        var count = 0;
        var count2 = 0;

        do
        {
            count2 = 0;
            foreach (var position in input.ToList())
            {
                var result = Test(input, position);
                count += result;
                if (result == 1) input.Remove(position);
                count2 += result;
            }
            
        } while (count2 > 0);
        
        Console.WriteLine($"Result = [{count}]");
    }

    public static int Test(List<int> input, int intPosition)
    {
        //Console.WriteLine($"Number [{intPosition}]");
        var matches = 0;

        if (input.Contains(intPosition - (_multiplier)))
        {
            //Console.Write($"-- [{intPosition - (_multiplier)}]");
            matches++;
        }
        
        if (input.Contains(intPosition + (_multiplier)))
        {
            //Console.Write($"-- [{intPosition + (_multiplier)}]");
            matches++;
        }
        
        if(input.Contains(intPosition - (_multiplier-1)))
        {
            //Console.Write($"-- [{intPosition - (_multiplier-1)}]");
            matches++;
        }
        
        if(input.Contains(intPosition + (_multiplier-1)))
        {
            //Console.Write($"-- [{intPosition + (_multiplier-1)}]");
            matches++;
        }
        
        if(input.Contains(intPosition - (_multiplier+1)))
        {
            //Console.Write($"-- [{intPosition - (_multiplier+1)}]");
            matches++;
        }
        
        if(input.Contains(intPosition + (_multiplier+1)))
        {
            //Console.Write($"-- [{intPosition + (_multiplier+1)}]");
            matches++;
        }
        
        if(input.Contains(intPosition - 1))
        {
            //Console.Write($"-- [{intPosition -1}]");
            matches++;
        }
        if(input.Contains(intPosition +1))
        {
            //Console.Write($"-- [{intPosition +1}]");
            matches++;
        }
        
        // matches += (input.ContainsKey(intPosition - 11) ? 1 : 0) +
        //            (input.ContainsKey(intPosition - 10) ? 1 : 0) +
        //            (input.ContainsKey(intPosition - 9) ? 1 : 0) +
        //            (input.ContainsKey(intPosition - 1) ? 1 : 0) +
        //            (input.ContainsKey(intPosition + 1) ? 1 : 0) +
        //            (input.ContainsKey(intPosition + 9) ? 1 : 0) +
        //            (input.ContainsKey(intPosition + 10) ? 1 : 0) +
        //            (input.ContainsKey(intPosition + 11) ? 1 : 0);
        
        return matches < 4 ? 1 : 0;
    }
    
    public static List<int> RollCordinates(string input)
    {
        var asRows = HelperFunctions.ConvertNewlinesToRow(input);
        //[x,y]
        var result = new List<int>();
        _multiplier = asRows.Length + 5;

        for(var y =0; y < asRows.Length; y++)
        {
            var columns = asRows[y].ToCharArray();

            for (var x = 0; x < columns.Length; x++)
            {
                if (columns[x] == '@')
                {
                    result.Add((x*_multiplier)+y);
                }
            }
        }

        return result;
    }
}