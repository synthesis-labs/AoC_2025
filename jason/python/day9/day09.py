def part1():
  with open('input.txt', 'r') as file:
    content = file.read().strip().split('\n')
    first = []
    second = []
    for a in content:
      b = a.split(',')
      first.append(int(b[0]))
      second.append(int(b[1]))
    maxArea = 0
    for i in range(0, len(first)):
      for j in range(i+1, len(first)):
        currArea = abs(first[i] - first[j] + 1) * abs(second[i] - second[j] + 1)
        if currArea > maxArea:
          maxArea = currArea
    print(maxArea)

from coordinates import Coordinate
from shapely import box
from shapely.geometry.polygon import Polygon
Coordinate.default_order = 'xy'  

def rect_from_diagonal(p1, p2):
  x1, y1 = p1
  x2, y2 = p2
  minx, maxx = sorted((x1, x2))
  miny, maxy = sorted((y1, y2))
  return box(minx, miny, maxx, maxy)

def rectangle_in_or_on_polygon(poly, rect_p1, rect_p2):
  rect = rect_from_diagonal(rect_p1, rect_p2)
  return poly.covers(rect)

def part2():
  with open('input.txt', 'r') as file:
    content = file.read().strip().split('\n')
    coordList = []
    for a in content:
      b = a.split(',')
      coordList.append(Coordinate(x = int(b[1]), y = int(b[0])))

    rightCoords = coordList.copy()
    coordList.sort(key=lambda c: (c.x, c.y))
    redCoords = coordList.copy()
    coordlength = len(coordList)
    for i in range(0, coordlength):
      for j in range(i+1, coordlength):
        if coordList[i].x == coordList[j].x:
          for z in range(coordList[i].y + 1, coordList[j].y):
            coordList.append(Coordinate(x = coordList[i].x, y = z))
        elif coordList[i].y == coordList[j].y:
          for z in range(coordList[i].x + 1, coordList[j].x):
            coordList.append(Coordinate(x = z, y = coordList[i].y))
    polyTup = []
    for coord in rightCoords:
      polyTup.append((coord.x, coord.y))
    poly = Polygon(polyTup)
    maxArea = 0
    for i in range(0, len(redCoords)):
      for j in range(i+1, len(redCoords)):
        if rectangle_in_or_on_polygon(poly, (redCoords[i].x, redCoords[i].y), (redCoords[j].x, redCoords[j].y)):
          currArea = (abs(redCoords[i].x - redCoords[j].x) + 1) * (abs(redCoords[i].y - redCoords[j].y) + 1)
          if currArea > maxArea:
            maxArea = currArea
    print(maxArea)

part1()
part2()