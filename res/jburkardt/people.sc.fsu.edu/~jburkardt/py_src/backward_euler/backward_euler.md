# backward_euler {#backward_euler align="center"}

------------------------------------------------------------------------

**backward_euler**, a Python code which solves one or more ordinary
differential equations (ODE) using the (implicit) backward Euler method,
using fsolve() for the implicit equation.

Unless the right hand side of the ODE is linear in the dependent
variable, each backward Euler step requires the solution of an implicit
nonlinear equation. Such equations can be approximately solved using
methods such as fixed point iteration, or an implicit equation solver
like fsolve().

### Licensing: {#licensing align="center"}

The computer code and data files described and made available on this
web page are distributed under [the GNU LGPL
license.](https://www.gnu.org/licenses/lgpl-3.0.en.html)

### Languages: {#languages align="center"}

**backward_euler** is available in [a FreeFem++
version](../../freefem_src/backward_euler/backward_euler.html) and [a
MATLAB version](../../m_src/backward_euler/backward_euler.html) and [an
Octave version](../../octave_src/backward_euler/backward_euler.html) and
[a Python version](../../py_src/backward_euler/backward_euler.html) and
[an R version](../../r_src/backward_euler/backward_euler.html).

### Related Data and Programs: {#related-data-and-programs align="center"}

[euler](../../py_src/euler/euler.html), a Python code which solves one
or more ordinary differential equations (ODE) using the forward Euler
method.

[leapfrog](../../py_src/leapfrog/leapfrog.html), a Python code which
uses the leapfrog method to solve a second order ordinary differential
equation (ODE) of the form y\'\'=f(t,y).

[midpoint](../../py_src/midpoint/midpoint.html), a Python code which
solves one or more ordinary differential equations (ODE) using the
(implicit) midpoint method.

[midpoint_explicit](../../py_src/midpoint_explicit/midpoint_explicit.html),
a Python code which solves one or more ordinary differential equations
(ODE) using the (explicit) midpoint method, also called the modified
Euler method.

[midpoint_fixed](../../py_src/midpoint_fixed/midpoint_fixed.html), a
Python code which solves one or more ordinary differential equations
(ODE) using the (implicit) midpoint method, using a simple fixed-point
iteration to solve the nonlinear equation.

[rk4](../../py_src/rk4/rk4.html), a Python code which applies the fourth
order Runge-Kutta (RK) algorithm to estimate the solution of an ordinary
differential equation (ODE) at the next time step.

[rkf45](../../py_src/rkf45/rkf45.html), a Python code which implements
the Runge-Kutta-Fehlberg (rkf) solver for the solution of a system of
ordinary differential equations (ODE).

[trapezoidal](../../py_src/trapezoidal/trapezoidal.html), a Python code
which solves one or more ordinary differential equations (ODE) using the
(implicit) trapezoidal method.

[trapezoidal_fixed](../../py_src/trapezoidal_fixed/trapezoidal_fixed.html),
a Python code which solves one or more ordinary differential equations
(ODE) using the (implicit) trapezoidal method, using a fixed point
method to handle the implicit system.

### Source Code: {#source-code align="center"}

-   [backward_euler.py](backward_euler.py), the source code.
-   [backward_euler.sh](backward_euler.sh), runs all the tests.
-   [backward_euler.txt](backward_euler.txt), the output file.

```{=html}
<!-- -->
```
-   [humps_backward_euler.png](humps_backward_euler.png),
-   [predator_prey_backward_euler.png](predator_prey_backward_euler.png),

------------------------------------------------------------------------

*Last revised on 01 May 2021.*
