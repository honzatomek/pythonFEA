function y = biochemical_linear_exact ( t )

%*****************************************************************************80
%
%% biochemical_linear_exact returns the exact solution of a linear biochemical ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Angela Martiradonna, Gianpiero Colonna, Fasma Diele,
%    GeCo: Geometric conservative nonstandard schemesfor biochemical systems,
%    Applied Numerical Mathematics,
%    2019.
%
%  Input:
%
%    real T: the current time.
%
%  Output:
%
%    real Y(2): the exact solution.
%
  [ a, b, t0, y0 ] = biochemical_linear_parameters ( );

  t = t(:);

  y10 = y0(1);
  y20 = y0(2);

  y1 = y10 + ( 1.0 - exp ( - ( a + b ) * ( t - t0 ) ) ) ...
    * ( - a * y10 + b * y20 ) / ( a + b );

  y2 = y20 + ( 1.0 - exp ( - ( a + b ) * ( t - t0 ) ) ) ...
    * (   a * y10 - b * y20 ) / ( a + b );

  y = [ y1, y2 ];

  return
end
