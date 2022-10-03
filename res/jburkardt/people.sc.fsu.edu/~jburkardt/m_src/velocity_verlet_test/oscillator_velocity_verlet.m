function oscillator_velocity_verlet ( n )

%*****************************************************************************80
%
%% oscillator_verlocity_verlet() uses velocity-verlet on the oscillator ODE.
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
  fprintf ( 1, 'oscillator_verlocity_verlet:\n' );
  fprintf ( 1, '  Use velocity_verlet() to solve the oscillator ODE.\n' );

  t0 = 0.0;
  y0 = [ 1.0, 0.0 ];
  tstop = 20.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Parameter values:\n' );
  fprintf ( 1, '    t0      = %g\n', t0 );
  fprintf ( 1, '    y0      = (%g,%g)\n', y0 );
  fprintf ( 1, '    tstop   = %g\n', tstop );

  tspan = [ t0, tstop ];
  f = @ oscillator_deriv;

  [ t, y ] = velocity_verlet ( f, tspan, y0, n );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Number of equal steps = %d\n', n );

  ye = oscillator_exact ( t );
%
%  Plot Y1(T).
%
  clf ( );
  hold ( 'on' );
    plot ( t, y(:,1),  'r-', 'linewidth', 3 );
    plot ( t, ye(:,1), 'b-', 'linewidth', 3 );
  hold ( 'off' );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y1(t) -->' );
  title ( 'oscillator velocity verlet: y1(t)' );
  legend ( 'Velocity Verlet', 'Exact' );
  filename = 'oscillator_velocity_verlet_y1.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Plot Y2(T).
%
  clf ( );
  hold ( 'on' );
    plot ( t, y(:,2),  'r-', 'linewidth', 3 );
    plot ( t, ye(:,2), 'b-', 'linewidth', 3 );
  hold ( 'off' );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y2(t) -->' );
  title ( 'oscillator velocity verlet: y2(t)' );
  legend ( 'Velocity Verlet', 'Exact' );
  filename = 'oscillator_velocity_verlet_y2.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Plot Phase.
%
  clf ( );
  hold ( 'on' );
    plot ( y(:,1),  y(:,2),  'r-', 'linewidth', 3 );
    plot ( ye(:,1), ye(:,2), 'b-', 'linewidth', 3 );
  hold ( 'off' );
  grid ( 'on' );
  xlabel ( '<-- y1(t) -->' );
  ylabel ( '<-- y2(t) -->' );
  title ( 'oscillator velocity verlet: phase(t)' );
  legend ( 'Velocity Verlet', 'Exact' );
  axis ( 'equal' );
  filename = 'oscillator_velocity_verlet_phase.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Conservation.
%
  h = 0.5 * ( y(:,1).^2 + y(:,2).^2 );

  clf ( );
  hold ( 'on' );
    plot ( t, h,  'r-', 'linewidth', 3 );
    plot ( [t0,tstop], [h(1),h(1)], 'b--', 'linewidth', 3 );
    plot ( [t0,tstop], [0.0,0.0], 'k--', 'linewidth', 3 );
  hold ( 'off' );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- Energy -->' );
  title ( 'oscillator velocity verlet: conservation' );
  legend ( 'Velocity Verlet', 'Exact' );
  filename = 'oscillator_velocity_verlet_conservation.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  return
end
