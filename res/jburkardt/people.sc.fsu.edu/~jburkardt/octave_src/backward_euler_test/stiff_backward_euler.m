function stiff_backward_euler ( tspan, y0, n )

%*****************************************************************************80
%
%% stiff_backward_euler() uses the backward Euler method on the stiff ODE.
%
%  Discussion:
%
%    fsolve() is used to solve the backward Euler equation.
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
  fprintf ( 1, 'stiff_backward_euler()\n' );
  fprintf ( 1, '  Use backward_euler() to solve the stiff ODE.\n' );

  [ t1, y1 ] = backward_euler ( @stiff_deriv, tspan, y0, n );

  t2 = linspace ( tspan(1), tspan(2), 101 );
  y2 = stiff_exact ( t2 );

  plot ( t1, y1, 'ro-', t2, y2, 'b-', 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y(t) -->' );
  title ( 'stiff backward euler: time plot' );
  legend ( 'Computed', 'Exact' );
  filename = 'stiff_backward_euler.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_backward_euler()\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

