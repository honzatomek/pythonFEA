function dydt = biochemical_linear_deriv ( t, y )

%*****************************************************************************80
%
%% biochemical_linear_deriv evaluates the derivative of a linear biochemical ODE.
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
%    real T, Y(2): the arguments of the derivative.
%
%  Output:
%
%    real DYDT(2): the value of the derivative.
%
  [ a, b, t0, y0 ] = biochemical_linear_parameters ( );

  y1 = y(1);
  y2 = y(2);

  S = [ -1.0,  1.0; ...
         1.0, -1.0 ];

  r = [ a * y1; b * y2 ];
  
  dydt = S * r;

  return
end
