function h = biochemical_linear_conserved ( t, y )

%*****************************************************************************80
%
%% biochemical_linear_conserved evaluates a quantity that should be conserved.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 August 2020
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
%    real Y(2): the current solution.
%
%  Output:
%
%    real H: the conserved quantity.
%
  h = y1 + y2;

  return
end
