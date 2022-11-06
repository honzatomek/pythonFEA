function exact = p44_exact ( )

%*****************************************************************************80
%
%% p44_exact() returns the exact integral for problem 44.
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
  alpha = p44_param_get ( );

  exact = ( 20.0 * sin ( 2.0^alpha ) ...
    - 2.0^alpha * cos ( 2.0^alpha ) ...
    + 2.0^alpha * exp ( -20.0 ) ) / ( 400.0 + 4.0^alpha );

  return
end
