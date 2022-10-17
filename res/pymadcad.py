#!/usr/bin/python3

from madcad import *
from madcad.rendering import Displayable
from madcad.displays import GridDisplay

# create a wire from custom points
s = 100
mycurve = Wire([  vec3(sin(t/s), cos(t/s), 0.1*t/s)
                  for t in range(int(s*6*pi)) ])

# create a sphere
mysphere = uvsphere(vec3(0), 0.5)

# display in a separated window
show([mycurve,   # displaying the curve
      mysphere,  # displaying the sphere
      Displayable(GridDisplay, vec3(0)),   # this is to have a 2D grid centered on origin
      ])

