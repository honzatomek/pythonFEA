function y = oscillator_exact ( t )

%*****************************************************************************80
%
%% oscillator_exact defines the exact solution of the oscillator ODE.
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
%  Output:
%
%    real PQ(2): the exact solution at time T.
%
  [ alpha, beta, epsilon, t0, y0 ] = oscillator_parameters ( );

  p = alpha * epsilon * sin ( ( t - t0 ) / epsilon ) ... 
    + beta  * epsilon * cos ( ( t - t0 ) / epsilon );

  q = alpha *           cos ( ( t - t0 ) / epsilon ) ...
    - beta            * sin ( ( t - t0 ) / epsilon );

  y = [ p; q ];

  return
end
