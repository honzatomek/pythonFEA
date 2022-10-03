function rk4_predator_test ( )

%*****************************************************************************80
%
%% rk4_predator_test calls rk4() with specific values of n, a, b, and y0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 March 2020
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'rk4_predator_test:\n' );
  fprintf ( 1, '  Solve predator-prey equations using rk4().\n' );

  f = @ predator_deriv;
  tspan = [ 0.0, 5.0 ];
  y0 = [ 5000, 100 ];
  n = 1000;
  
  [ t, y ] = rk4 ( f, tspan, y0, n );

  dt = ( tspan(2) - tspan(1) ) / n;

  figure ( 1 )
  plot ( t, y, 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- T -->' );
  ylabel ( '<-- Y(T) -->' );
  label = sprintf ( 'Predator rk4 solution with dt = %g\n', dt );
  title ( label );
  filename = 'rk4_predator_plot.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  figure ( 2 )
  plot ( y(:,1), y(:,2), 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- Prey(T) -->' );
  ylabel ( '<-- Predators(T) -->' );
  label = sprintf ( 'Predator rk4 solution with dt = %g\n', dt );
  title ( label );
  filename = 'rk4_predator_phase.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end
  
