function dpqdt = kepler_perturbed_deriv ( t, pq )

%*****************************************************************************80
%
%% kepler_perturbed_deriv evaluates the derivative of a perturbed Kepler ODE.
%
%  Discussion:
%
%    We consider a perturbed Kepler two-body gravitational problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2020
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
%    real T, PQ(4): the arguments of the derivative.
%
%  Output:
%
%    real DPQDT: the value of the derivative.
%
  delta = 0.015;

  q1 = pq(1);
  q2 = pq(2);
  p1 = pq(3);
  p2 = pq(4);

  dq1dt = p1;
  dq2dt = p2;
  dp1dt =         - q1 ./ sqrt ( ( q1.^2 + q2.^2 ).^3 ) ...
          - delta * q1 ./ sqrt ( ( q1.^2 + q2.^2 ).^5 );
  dp2dt =         - q2 ./ sqrt ( ( q1.^2 + q2.^2 ).^3 ) ...
          - delta * q2 ./ sqrt ( ( q1.^2 + q2.^2 ).^5 );

  dpqdt = [ dq1dt; dq2dt; dp1dt; dp2dt ];

  return
end
