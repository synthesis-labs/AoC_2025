namespace aoc_2025.Days.Day2;

public class Day3
{
    public static void Part1()
    {
        var input = HelperFunctions.ConvertNewlinesToRow(Day3_Input.Input);
        long count = 0;

        foreach (var battery in input)
        {
            Console.WriteLine($" [{battery}] ");
            var inputArray = battery.ToCharArray();

            var slot1 = inputArray[0];
            var slot2 = inputArray[1];
            var slot2Index = 1;

            for (var i = 0; i < inputArray.Length - 2; i++)
            {
                // Next one is bigger
                if (inputArray[i + 1] > slot1)
                {
                    slot1 = inputArray[i + 1];
                    slot2 = inputArray[i + 2];
                    slot2Index = i + 2;
                }
            }

            for (var i = slot2Index; i < inputArray.Length - 1; i++)
            {
                if (inputArray[i + 1] > slot2)
                {
                    slot2 = inputArray[i + 1];
                }
            }

            var result = int.Parse(string.Concat(slot1,slot2));
            count += result;
            Console.WriteLine($" -> [{slot1}{slot2}]");
        }
        
        Console.WriteLine($"Result = [{count}]");
    }
    
    public static void Part2()
    {
        var input = HelperFunctions.ConvertNewlinesToRow(Day3_Input.Input);
        long count = 0;

        foreach (var battery in input)
        {
            var inputArray = battery.ToCharArray();
            var slots = new char[12];
            var selectedIndex = 0;
            
            //Console.WriteLine($"Start info : length [{inputArray.Length}]");
            
            // loop through each slot
            foreach (var slotNumber in Enumerable.Range(0, 12))
            {
                var end = inputArray.Length - (11-slotNumber);

                //Console.Write($"[{slotNumber}] Slot Range = [{selectedIndex}] - [{end-1}]");
                var compareRange = inputArray[selectedIndex..end];
//                Console.Write($" --> len [{compareRange.Length}]");
                //Console.Write($" --> selectedIndex [{selectedIndex}]");
                slots[slotNumber] = compareRange[0];
                var temp = selectedIndex;
                for(var i=1; i < compareRange.Length; i++)
                {
                    if (slots[slotNumber] < compareRange[i])
                    {
                        slots[slotNumber] = compareRange[i];
                        temp = selectedIndex + i;
                    }
                }

                selectedIndex = temp+1;
                //Console.WriteLine($" --> [{slots[slotNumber]}]");
            }
            Console.WriteLine($"Battery output = [{new string(slots)}]");
            var result = long.Parse(new string(slots));
            count += result;

        }
        
        Console.WriteLine($"The result {count}");
    }
}