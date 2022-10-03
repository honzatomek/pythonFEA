function dydt = kepler_deriv ( t, y )

%*****************************************************************************80
%
%% kepler_deriv() evaluates the derivative of a Kepler ODE.
%
%  Discussion:
%
%    We consider a Kepler problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Ernst Hairer, Christian Lubich, Gerhard Wanner,
%    Geometric Numerical Integration:
%    Structure-Preserving Algorithms for Ordinary Differential Equations,
%    Springer, 2006,
%    ISSN: 0179-3632
%
%  Input:
%
%    real T, Y(4): the arguments of the derivative.
%
%  Output:
%
%    real DYDT(4): the value of the derivative.
%
  q1 = y(1);
  q2 = y(2);
  p1 = y(3);
  p2 = y(4);

  dq1dt = p1;
  dq2dt = p2;
  dp1dt =         - q1 ./ sqrt ( ( q1.^2 + q2.^2 ).^3 );
  dp2dt =         - q2 ./ sqrt ( ( q1.^2 + q2.^2 ).^3 );

  dydt = [ dq1dt; dq2dt; dp1dt; dp2dt ];

  return
end
