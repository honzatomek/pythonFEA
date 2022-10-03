function value = quadex_exact ( t )

%*****************************************************************************80
%
%% quadex_exact evaluates the exact solution of the "quadex" ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 October 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T: the evaluation time.
%
%  Output:
%
%    real VALUE, the solution at the given time.
%
  c = 0.0;
  value = c * exp ( 5.0 * t ) + t.^2 + 2.0 * t / 5.0 + 2.0 / 25.0;

  return
end

