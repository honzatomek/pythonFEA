function dydt = oscillator_deriv ( t, y )

%*****************************************************************************80
%
%% oscillator_deriv defines the oscillator ODE.
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
%    real T: the current time.
%
%    real Y(2): the current solution variables.
%
%  Output:
%
%    real DYDT(2): the derivative.
%
  [ alpha, beta, epsilon, t0, y0 ] = oscillator_parameters ( );

  p = y(1);
  q = y(2);

  dpdt = q;
  dqdt = - p / epsilon^2;

  dydt = [ dpdt; dqdt ];

  return
end
