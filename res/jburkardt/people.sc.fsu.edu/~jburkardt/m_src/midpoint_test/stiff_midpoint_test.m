function stiff_midpoint_test ( tspan, y0, n )

%*****************************************************************************80
%
%% stiff_midpoint_test(): solve stiff ODE using midpoint().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 October 2020
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
  fprintf ( 1, 'stiff_midpoint_test\n' );
  fprintf ( 1, '  Solve stiff ODE using midpoint().\n' );

  f = @ stiff_deriv;
  [ t1, y1 ] = midpoint ( f, tspan, y0, n );

  t2 = linspace ( tspan(1), tspan(2), 101 );
  y2 = stiff_exact ( t2 );

  plot ( t1, y1, 'ro-', t2, y2, 'b-', 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y(t) -->' );
  title ( 'midpoint: Stiff ODE' );
  legend ( 'Computed', 'Exact' );
  filename = 'stiff_midpoint.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_midpoint_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

