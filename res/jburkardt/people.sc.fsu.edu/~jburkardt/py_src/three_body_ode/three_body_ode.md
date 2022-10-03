# three_body_ode {#three_body_ode align="center"}

------------------------------------------------------------------------

**three_body_ode**, a Python code which defines a set of ordinary
differential equations (ODE) which simulates the behavior of three
planets, constrained to lie in a plane, and moving under the influence
of gravity, by Walter Gander and Jiri Hrebicek.

Three bodies, regarded as point masses, are constrained to lie in a
plane. The masses of each body are given, as are the positions and
velocities at a starting time T = 0. The bodies move in accordance with
the gravitational force between them.

The force exerted on the 0-th body by the 1st body can be written:

            F = - m0 m1 ( p0 - p1 ) / |p0 - p1|^3
          

assuming that units have been normalized to that the gravitational
coefficient is 1. Newton\'s laws of motion can be written:

      
            m0 p0'' = - m0 m1 ( p0 - p1 ) / |p0 - p1|^3 
                      - m0 m2 ( p0 - p2 ) / |p0 - p2|^3
      
            m1 p1'' = - m1 m0 ( p1 - p0 ) / |p1 - p0|^3 
                      - m1 m2 ( p1 - p2 ) / |p1 - p2|^3
      
            m2 p2'' = - m2 m0 ( p2 - p0 ) / |p2 - p0|^3 
                      - m2 m1 ( p2 - p1 ) / |p2 - p1|^3
          

Letting

            y1 = p0(x)
            y2 = p0(y)
            y3 = p0'(x)
            y4 = p0'(y)
          

and using similar definitions for p1 and p2, the 3 second order vector
equations can be rewritten as 12 first order equations. In particular,
the first four are:

            y1' = y3
            y2' = y4
            y3' = - m1 ( y1 - y5  ) / |(y1,y2) - (y5,y6) |^3 
                  - m2 ( y1 - y9  ) / |(y1,y2) - (y9,y10)|^3
            y4' = - m1 ( y2 - y6  ) / |(y1,y2) - (y5,y6) |^3 
                  - m2 ( y2 - y10 ) / |(y1,y2) - (y9,y10)|^3
          

and so on. This first order system can be integrated by a standard ODE
solver.

Note that when any two bodies come close together, the solution changes
very rapidly, and very small steps must be taken by the ODE solver. For
this system, the first near collision occurs around T=15.8299, and the
results will not be very accurate after that point.

### Licensing: {#licensing align="center"}

The computer code and data files described and made available on this
web page are distributed under [the GNU LGPL
license.](https://www.gnu.org/licenses/lgpl-3.0.en.html)

### Languages: {#languages align="center"}

**three_body_ode** is available in [a C
version](../../c_src/three_body_ode/three_body_ode.html) and [a C++
version](../../cpp_src/three_body_ode/three_body_ode.html) and [a
FORTRAN90 version](../../f_src/three_body_ode/three_body_ode.html) and
[a MATLAB version](../../py_src/three_body_ode/three_body_ode.html) and
[an Octave version](../../octave_src/three_body_ode/three_body_ode.html)
and [a Python version](../../py_src/three_body_ode/three_body_ode.html).

### Related Data and codes: {#related-data-and-codes align="center"}

[python_ode](../../py_src/python_ode/python_ode.html), Python codes
which sets up various systems of ordinary differential equations (ODE).

### Author: {#author align="center"}

Original MATLAB version by Dominik Gruntz, Joerg Waldvogel.

### Reference: {#reference align="center"}

1.  Dominik Gruntz, Joerg Waldvogel,\
    Orbits in the Planar Three-Body Problem,\
    Walter Gander, Jiri Hrebicek, editors,\
    Solving Problems in Scientific Computing using Maple and Matlab,\
    Springer, 1997,\
    ISBN: 3-540-61793-0,\
    LC: Q183.9.G36.

### Source Code: {#source-code align="center"}

-   [three_body_ode.m](three_body_ode.m), the source code.
-   [three_body_ode.sh](three_body_ode.sh), runs all the tests.
-   [three_body_ode.txt](three_body_ode.txt), the output file.

```{=html}
<!-- -->
```
-   [three_body_1.png](three_body_1.png), an image of the first third of
    the trajectory.
-   [three_body_2.png](three_body_2.png), an image of the second third
    of the trajectory.
-   [three_body_3.png](three_body_3.png), an image of the last third of
    the trajectory.
-   [three_body_4.png](three_body_4.png), an image of the entire
    trajectory.

------------------------------------------------------------------------

*Last modified on 11 November 2020.*
