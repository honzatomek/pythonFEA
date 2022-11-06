function exact = p36_exact ( )

%*****************************************************************************80
%
%% p36_exact() returns the exact integral for problem 36.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real EXACT, the value of the integral.
%
  alpha = p36_param_get ( );

  exact = 1.0 / ( alpha + 1.0 )^2;

  return
end
