function h = biochemical_nonlinear_conserved ( y )

%*****************************************************************************80
%
%% biochemical_nonlinear_conserved evaluates two conserved quantities.
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
%    GeCo: Geometric conservative nonstandard schemes for biochemical systems,
%    Applied Numerical Mathematics,
%    2019.
%
%  Input:
%
%    real Y(4): the current solution.
%
%  Output:
%
%    real H(2): the conserved quantities.
%
  [ a, b, kc, kn, rmax, e, t0, y0 ] = biochemical_nonlinear_parameters ( );

  E = [ 1.0, 0.0, a, a; ...
        0.0, 1.0, b, b ];

  h = E * y;

  return
end
