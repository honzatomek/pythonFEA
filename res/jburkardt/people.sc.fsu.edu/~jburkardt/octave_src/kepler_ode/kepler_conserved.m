function [ h1, h2 ] = kepler_conserved ( y )

%*****************************************************************************80
%
%% kepler_conserved() evaluates conservation for a Kepler ODE.
%
%  Discussion:
%
%    We consider a Kepler two-body gravitational problem.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 May 2021
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
%    real Y(N,4): the current solution.
%
%  Output:
%
%    real H1(N): the value of energy.
%
%    real H2(N): the value of angular momentum.
%
  q1 = y(:,1);
  q2 = y(:,2);
  p1 = y(:,3);
  p2 = y(:,4);

  h1 = 0.5 * ( p1.^2 + p2.^2 ) - 1.0 ./ sqrt ( q1.^2 + q2.^2 );

  h2 = q1 .* p2 - q2 .* p1;

  return
end
