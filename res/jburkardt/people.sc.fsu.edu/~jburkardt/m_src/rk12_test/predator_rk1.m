function predator_rk1 ( )

%*****************************************************************************80
%
%% predator_rk1 calls rk1() with specific values of n, a, b, and y0.
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
  f = @ predator_deriv;
  tspan = [ 0.0, 5.0 ];
  y0 = [ 5000, 100 ];
  n = 1000;
  
  [ t, y ] = rk1 ( f, tspan, y0, n );

  dt = ( tspan(2) - tspan(1) ) / n;

  figure ( 1 )
  plot ( t, y(:,1), 'g', ...
         t, y(:,2), 'r', 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- T -->' );
  ylabel ( '<-- Y(T) -->' );
  label = sprintf ( 'Predator RK1 solution with dt = %g\n', dt );
  title ( label );
  filename = 'predator_rk1_plot.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  figure ( 2 )
  plot ( y(:,1), y(:,2), 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- Prey(T) -->' );
  ylabel ( '<-- Predators(T) -->' );
  label = sprintf ( 'Predator RK1 solution with dt = %g\n', dt );
  title ( label );
  filename = 'predator_rk1_phase.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end

  
