using System.Text.RegularExpressions;

namespace aoc_2025;

public static class HelperFunctions
{
    public static List<Tuple<int, int, int, int>> ConvertRowsToOrderedString(string input)
    {
        var listOfInput = input.Split( Environment.NewLine);
        var list1 = new List<int>();
        var list2 = new List<int>();
        foreach (var row in listOfInput)
        {
            var test = row.Split("   ");
            list1.Add(int.Parse(test.First()));
            list2.Add(int.Parse(test.Last()));
        }

        list1 = list1.Order().ToList();
        list2 = list2.Order().ToList();

        var result = new List<Tuple<int, int, int, int>>();
        for(var i=0; i < list1.Count; i++ )
        {
            result.Add(new Tuple<int, int, int, int>(
                list1[i], 
                list2[i], 
                Math.Abs(list1[i] - list2[i]), 
                i==0 ? Math.Abs(list1[i] - list2[i]) : Math.Abs(list1[i] - list2[i]) + result[i-1].Item4));
        }

        return result;
    }

    public static Tuple<List<int>, List<int>> ConvertToOrderedStrings(string inputAsStringOf2Lists)
    {
        var listOfInput = ConvertNewlinesToRow(inputAsStringOf2Lists);
        var list1 = new List<int>();
        var list2 = new List<int>();
        foreach (var row in listOfInput)
        {
            var test = row.Split("   ");
            list1.Add(int.Parse(test.First()));
            list2.Add(int.Parse(test.Last()));
        }

        list1 = list1.Order().ToList();
        list2 = list2.Order().ToList();

        return new Tuple<List<int>, List<int>>(list1, list2);
    }

    public static string[] ConvertNewlinesToRow(string input)
    {
        return input.Split( Environment.NewLine);
    }

    public static List<string> CleanStringWithRegex(string input, string regex)
    {
        return Regex.Matches(input, regex).Select(x => x.Value).ToList();
    }
    
    public static List<string> CleanStringWithRegex2(string input, string regex)
    {
        return Regex.Matches(input, regex).Select(x => x.Groups[1].Value).ToList();
    }
    
    public static List<string> SplitStringOnRegex(string input, string regex)
    {
        return Regex.Split(input, regex).ToList();
    }

    public static string[] CommaSeperatedToArray(string input)
    {
        return input.Split(",");
    }
    
    public static IEnumerable<long> LongRange(long start, long limit)
    {
        for (long i = start; i <= limit; i++)
        {
            yield return i;
        }
    }
    
}