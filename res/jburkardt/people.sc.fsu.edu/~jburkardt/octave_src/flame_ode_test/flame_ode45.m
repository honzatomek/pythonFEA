function [ t, y ] = flame_ode45 ( )

%*****************************************************************************80
%
%% flame_ode45 solves the flame ODE using ODE45.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Shirley Abelman, Kailash Patidar,
%    Comparison of some recent numerical methods for initial-value problems
%    for stiff ordinary differential equations,
%    Computers and Mathematics with Applications,
%    Volume 55, Number 4, 2008, pages 733-744.
%    
%    Cleve Moler,
%    Cleve's Corner: Stiff Differential Equations,
%    MATLAB News and Notes,
%    May 2003, pages 12-13.
%
%  Output:
%
%    real T(:), Y(:): the sequence of time and solution estimates.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'base_ode45\n' );
  fprintf ( 1, '  Use ode45() to solve the flame ODE\n' );
  fprintf ( 1, '\n' );
%
%  Get the starting point.
%
  [ t0, y0 ] = flame_parameters ( );
%
%  Get the stopping point.
%
  tstop = 2.0 / y0;
%
%  Call ODE45 to estimate the solution.
%
  tspan = [ t0, tstop ];

  [ t, y ] = ode45 ( @flame_deriv, tspan, y0 );
%
%  Plot the solution curve.
%
  plot ( t, y(:,1), 'r-', 'Linewidth', 3 );

  title ( sprintf ( 'ode45: Flame ODE, DELTA = %g', y0 ) );
  grid ( 'on' );
  xlabel ( '<--- T --->' );
  ylabel ( '<--- X(T) --->' );
%
%  Save a copy in a file.
%
  filename = 'flame_ode45.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'flame_ode45:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

