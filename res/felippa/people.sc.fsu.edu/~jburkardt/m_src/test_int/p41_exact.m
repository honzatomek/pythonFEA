function exact = p41_exact ( )

%*****************************************************************************80
%
%% p41_exact() returns the exact integral for problem 41.
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
  alpha = p41_param_get ( );

  exact = pi / sqrt ( ( 1.0 + 2.0^(-alpha) )^2 - 1.0 );

  return
end
