// See https://aka.ms/new-console-template for more information

using System.Diagnostics;
using aoc_2025.Days.Day1;
using aoc_2025.Days.Day2;

var dayToRun = "5.2";

var stopwatch = new Stopwatch();
stopwatch.Start();

switch (dayToRun)
{
    case "1.1": Day1.Part1(); break;
    case "1.2": Day1.Part2(); break;
    case "2.1": Day2.Part1(); break;
    case "2.2": Day2.Part2(); break;
    case "3.1": Day3.Part1(); break;
    case "3.2": Day3.Part2(); break;
    case "4.1": Day4.Part1(); break;
    case "4.2": Day4.Part2(); break;
    case "5.1": Day5.Part1(); break;
    case "5.2": Day5.Part2(); break;
    case "6.1": Day6.Part1(); break;
    case "6.2": Day6.Part2(); break;
    case "7.1": Day7.Part1(); break;
    case "7.2": Day7.Part2(); break;
    case "8.1": Day8.Part1(); break;
    case "8.2": Day8.Part2(); break;
    case "9.1": Day9.Part1(); break;
    case "9.2": Day9.Part2(); break;
    // case "10.1": Day10.Part1(); break;
    // case "10.2": Day10.Part2(); break;
    // case "11.1": Day11.Part1(); break;
    // case "11.2": Day11.Part2(); break;
    // case "12.1": Day12.Part1(); break;
    // case "12.2": Day12.Part2(); break;
}

Console.WriteLine($"Time Taken: [{stopwatch.Elapsed}]");
Console.WriteLine("===========================================================");