def buildLargestForDigits(bank, digits):
  originalBank = bank[:]
  largest = max(bank)
  location = bank.index(largest)
  while location > len(originalBank) - digits:
    bank = bank.replace(largest, '', 1)
    largest = max(bank)
    location = originalBank.index(largest)
  originalBank = originalBank[location+1:]
  return (largest, originalBank)

def solution(digits: int):
  with open('input.txt', 'r') as file:
    lines = file.readlines()
    score = 0
    for line in lines:
      maxnum = ''
      bank = line.strip()
      while len(maxnum) < digits:
        (num, bank) = buildLargestForDigits(bank, digits - len(maxnum))
        maxnum += num
      score += int(maxnum)
    print(score)

solution(2) # part 1
solution(12) # part 2