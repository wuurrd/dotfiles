# Draw a voronoi diagram for a specific set of points

from PIL import Image
from collections import namedtuple
import math

class Position(namedtuple('Position', ['x', 'y', 'color'])):
    def __new__(cls, x, y, color=None):
        return super(Position, cls).__new__(cls, x, y, color)

class Metric(object):
    def distance(self, a, b):
        return 1

class EuclideanDistance(Metric):
    def distance(self, a, b):
        return math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2)

class ManhattanDistance(Metric):
    def distance(self, a, b):
        return abs(a.x - b.x) + abs(a.y - b.y)

class VoronoiDiagram(object):
    '''
    http://en.wikipedia.org/wiki/Voronoi_diagram
    '''
    def __init__(self, metric):
        self._points = []
        self._colors = [(255, 0, 0),
                        (0, 255, 0),
                        (0, 0, 255),
                        (255, 255, 0),
                        (0, 255, 255),
                        (255, 0, 255),
                        (100, 0, 100),
                        (50, 100, 50),
                        (75, 30, 200)
                       ]
        self.metric = metric
        self._width = 1280
        self._height = 720

    def add_point(self, x, y):
        assert x < self._width and x > 0
        assert y < self._height and y > 0
        unique_color = self._get_unique_color()
        self._points.append(Position(x, y, unique_color))

    def _get_unique_color(self):
        return self._colors.pop()

    def get_distance(self, position_1, position_2):
        return self.metric.distance(position_1, position_2)

    def closest_point(self, position):
        closest_point = None
        closest_distance = None
        for point in self._points:
            distance = self.get_distance(position, point)
            if closest_distance is None or distance < closest_distance:
                closest_point = point
                closest_distance = distance
        return closest_point

    def create_image(self):
        image = Image.new('RGB', (self._width, self._height))
        for x in range(self._width):
            for y in range(self._height):
                position = Position(x, y)
                closest = self.closest_point(position)
                image.putpixel((x, y), closest.color)
        for point in self._points:
            image.putpixel((point.x, point.y), (0, 0, 0))
        image.save('voronoi.jpg')

if __name__ == '__main__':
    diagram = VoronoiDiagram(ManhattanDistance())
    for x_point, y_point in [(30, 713),
                             (15, 428),
                             (607, 427),
                             (1200, 638),
                             (1000, 638),
                            ]:
        diagram.add_point(x_point, y_point)
    diagram.create_image()
