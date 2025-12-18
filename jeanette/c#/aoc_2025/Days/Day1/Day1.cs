namespace aoc_2025.Days.Day1;

public class Day1
{
        public static void Part1()
        {
            var input = HelperFunctions.ConvertNewlinesToRow(Day1_Input.Input1);
            var count = 0;
            var result = 50.00;
            
            Console.WriteLine("Starting at 50");
            
            foreach (var turn in input)
            {
                var direction = turn[0];
                var movement = turn.Length > 2 ? double.Parse(turn[^2..]) : double.Parse(turn[^1..]);
                Console.Write($"  -> [{movement}] ----> ");
                
                var oldResult = result;
                if (direction == 'L')
                {
                    result = result - movement;
                    Console.Write($"Move {direction} : {oldResult} - {movement} = {result}");
                }
                else if(direction == 'R')
                {
                    result = result + movement;
                    Console.Write($"Move {direction} : {oldResult} + {movement} = {result}");
                }
                else
                {
                    throw new Exception("SHIT");
                }

                switch (result)
                {
                    case 100: 
                        result = 0;
                        Console.Write($" -> {result}");
                        count = count + 1;
                        Console.WriteLine($" -> ADD 1 = {count}");
                        Console.WriteLine("============================================================================");
                        continue;
                        break;
                    case 0:
                        count = count + 1;
                        Console.WriteLine($" -> ADD 1 = {count}");
                        Console.WriteLine("============================================================================");
                        continue;
                    case < 0:
                        result = result + 100;
                        Console.Write($" -> {result}");
                        break;
                    case > 100:
                        result = result - 100;
                        Console.Write($" -> {result}");
                        break;
                    default:
                        break;
                }
                
                Console.WriteLine();
            }
            
            Console.WriteLine($"The result {count}");
            
        }
        public static void Part2()
        { 
            var input = HelperFunctions.ConvertNewlinesToRow(Day1_Input.Input1);
            var count = 0;
            var result = 50.00;
            
            Console.WriteLine("Starting at 50");
            
            foreach (var turn in input)
            {
                var direction = turn[0];
                Console.Write($"  -> [{turn}] -> ");
                Console.Write($" [{turn[1..]}] ----> ");

                var extraRotations = int.Parse(turn[1..]) / 100;
                count += extraRotations;
                Console.Write($" er -> [{extraRotations}] ----> ");
                var movement = turn.Length > 2 ? double.Parse(turn[^2..]) : double.Parse(turn[^1..]);
                
                var oldResult = result;
                if (direction == 'L')
                {
                    result = result - movement;
                    Console.Write($"Move {direction} : {oldResult} - {movement} = {result}");
                    count += oldResult < movement && oldResult != 0 ? 1 : 0;
                }
                else
                {
                    result += movement;
                    Console.Write($"Move {direction} : {oldResult} + {movement} = {result}");
                    count += result > 100 ? 1 : 0;
                }

                switch (result)
                {
                    case 100 or 0: 
                        result = 0;
                        count += 1;
                        break;
                    case < 0:
                        result = result + 100;
                        break;
                    case > 100:
                        result = result - 100;
                        break;
                    default:
                        break;
                }
                Console.Write($" -> {result} -> count [{count}]");
                Console.WriteLine();
            }
            
            Console.WriteLine($"The result {count}");
        }
}