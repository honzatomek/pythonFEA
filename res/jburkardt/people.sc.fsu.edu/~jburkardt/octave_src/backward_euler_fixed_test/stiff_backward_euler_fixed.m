function stiff_backward_euler_fixed ( tspan, y0, n )

%*****************************************************************************80
%
%% stiff_backward_euler_fixed() uses the backward Euler method on the stiff ODE.
%
%  Discussion:
%
%    A fixed point iteration is used to solve the implicit equation
%    (which turns out to be trivial to solve in this case.)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TSPAN(2): the first and last times.
%
%    real Y0: the initial condition.
%
%    integer N: the number of steps to take.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_backward_euler_fixed()\n' );
  fprintf ( 1, '  Solve stiff ODE using the backward Euler method.\n' );
  fprintf ( 1, '  Use a fixed point iteration to solve the implicit equation.\n' );

  [ t1, y1 ] = backward_euler_fixed ( @stiff_deriv, tspan, y0, n );

  t2 = linspace ( tspan(1), tspan(2), 101 );
  y2 = stiff_exact ( t2 );

  plot ( t1, y1, 'ro-', t2, y2, 'b-', 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y(t) -->' );
  title ( 'stiff backward euler fixed: time plot' );
  legend ( 'Computed', 'Exact' );
  filename = 'stiff_backward_euler_fixed.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_backward_euler_fixed()\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

