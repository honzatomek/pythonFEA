function h = oscillator_conserved ( y )

%*****************************************************************************80
%
%% oscillator_conserved defines a conserved quantity of the oscillator ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Uri Ascher, Sebastian Reich,
%    The midpoint scheme and variants for hamiltonian systems:
%    advantages and disadvantages,
%    SIAM Journal on Scientific Computing,
%    Volume 21, Number 3, pages 1045-1065, 1999.
%
%  Input:
%
%    real Y(2): the current solution variables.
%
%  Output:
%
%    real H: the hamiltonian.
%
  [ alpha, beta, epsilon, t0, y0 ] = oscillator_parameters ( );

  p = y(:,1);
  q = y(:,2);

  h = ( p / epsilon ).^2 + ( q.^2 );

  return
end
