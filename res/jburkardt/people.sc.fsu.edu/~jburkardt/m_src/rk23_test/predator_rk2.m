function predator_rk2 ( )

%*****************************************************************************80
%
%% predator_rk2 calls rk2() with specific values of n, a, b, and y0.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 October 2019
%
%  Author:
%
%    John Burkardt
%
  f = @ predator_deriv;
  tspan = [ 0.0, 5.0 ];
  y0 = [ 5000, 1000 ];
  n = 1000;
  
  [ t, y ] = rk2 ( f, tspan, y0, n );

  dt = ( tspan(2) - tspan(1) ) / n;

  figure ( 1 )
  plot ( t, y, 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- T -->' );
  ylabel ( '<-- Y(T) -->' );
  label = sprintf ( 'Predator rk2 solution with dt = %g\n', dt );
  title ( label );
  filename = 'predator_rk2_plot.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  figure ( 2 )
  plot ( y(:,1), y(:,2), 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- Prey(T) -->' );
  ylabel ( '<-- Predators(T) -->' );
  label = sprintf ( 'Predator rk2 solution with dt = %g\n', dt );
  title ( label );
  filename = 'predator_rk2_phase.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end

  
