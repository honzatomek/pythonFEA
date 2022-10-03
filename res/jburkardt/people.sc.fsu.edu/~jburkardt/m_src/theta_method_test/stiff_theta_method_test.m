function stiff_theta_method_test ( tspan, y0, n, theta )

%*****************************************************************************80
%
%% stiff_theta_method_test(): theta method + fsolve() on the stiff ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 March 2021
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
%    real THETA: the value of theta
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_theta_method_test():\n' );
  fprintf ( 1, '  Solve stiff ODE using the theta method + fsolve().\n' );
  fprintf ( 1, '  Using theta = %g\n', theta );

  f = @ stiff_deriv;
  [ t1, y1 ] = theta_method ( f, tspan, y0, n, theta );

  t2 = linspace ( tspan(1), tspan(2), 101 );
  y2 = stiff_exact ( t2 );

  plot ( t1, y1, 'ro-', t2, y2, 'b-', 'linewidth', 3 );
  grid ( 'on' );
  xlabel ( '<-- t -->' );
  ylabel ( '<-- y(t) -->' );
  label = sprintf ( 'Stiff ODE: theta method using theta = %g', theta );
  title ( label );
  legend ( 'Computed', 'Exact' );

  filename = sprintf ( 'stiff_theta_method_%g.png', theta );
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'stiff_theta_method_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

