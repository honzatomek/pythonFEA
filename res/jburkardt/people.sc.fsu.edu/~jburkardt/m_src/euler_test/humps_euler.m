function humps_euler ( n )

%*****************************************************************************80
%
%% humps_euler() solves the humps ODE using euler().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N: the number of steps to take.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'humps_euler:\n' );
  fprintf ( 1, '  Solve the humps ODE using euler().\n' );

  f = @ humps_deriv;
  tspan = [ 0.0, 2.0 ];
  y0 = humps_exact ( tspan(1) );

  [ t, y ] = euler ( f, tspan, y0, n );

  y2 = humps_exact ( t );

  clf ( );
  hold ( 'on' );
  plot ( t, y, 'r-', 'linewidth', 3 );
  plot ( t, y2, 'b-', 'linewidth', 3 );
  hold ( 'off' );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y(t) -->' );
  title ( 'humps euler(): plot' );
  legend ( 'Euler', 'Exact' );
  filename = 'humps_euler.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end
