function [ t, y ] = flame_euler ( n )

%*****************************************************************************80
%
%% flame_euler solves the flame ODE using euler.
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
%  Input:
%
%    integer N: the number of steps to take.
%
%  Output:
%
%    real T(:), Y(:): the sequence of time and solution estimates.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'flame_euler\n' );
  fprintf ( 1, '  Use euler() to solve the flame ODE\n' );
%
%  Get the starting point.
%
  [ t0, y0 ] = flame_parameters ( )
%
%  Get the stopping point.
%
  tstop = 2.0 / y0;
%
%  Estimate the solution.
%
  tspan = [ t0, tstop ];

  [ t, y ] = euler ( @flame_deriv, tspan, y0, n );
%
%  Plot the solution curve.
%
  plot ( t, y(:,1), 'r-', 'linewidth', 3 );

  title ( sprintf ( 'euler: Flame ODE, DELTA = %g', y0 ) );
  grid ( 'on' );
  xlabel ( '<--- T --->' );
  ylabel ( '<--- X(T) --->' );
%
%  Save a copy in a file.
%
  filename = 'flame_euler.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'flame_euler:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

