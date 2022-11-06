function exact = p43_exact ( )

%*****************************************************************************80
%
%% p43_exact() returns the exact integral for problem 43.
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
  alpha = p43_param_get ();

  exact = gamma ( alpha );

  return
end
