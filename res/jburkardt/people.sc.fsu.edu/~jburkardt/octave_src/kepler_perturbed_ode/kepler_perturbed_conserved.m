function h = kepler_perturbed_conserved ( pq )

%*****************************************************************************80
%
%% kepler_perturbed_conserved evaluates the Hamiltonian of a perturbed Kepler ODE.
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
%    real PQ(4): the arguments of the Hamiltonian.
%
%  Output:
%
%    real H: the value of the Hamiltonian.
%
  q1 = pq(1);
  q2 = pq(2);
  p1 = pq(3);
  p2 = pq(4);

  h = 0.5 * ( p1.^2 + p2.^2 ) - 1.0 ./ sqrt ( q1.^2 + q2.^2 ) ...
    - 0.005 ./ ( sqrt ( q1.^2 + q2.^2 ) ).^3 / 2.0;

  return
end
