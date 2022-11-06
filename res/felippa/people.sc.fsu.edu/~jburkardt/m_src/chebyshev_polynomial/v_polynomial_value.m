function value = v_polynomial_value ( n, x )

%*****************************************************************************80
%
%% v_polynomial_value(): returns the single value V(n,x).
%
%  Discussion:
%
%    In cases where calling V_POLYNOMIAL is inconvenient, because it returns
%    a vector of values for multiple arguments X, this simpler interface
%    may be appropriate.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the order of the polynomial.
%
%    real X, the argument of the polynomial.
%
%  Output:
%
%    real VALUE, the value of V(n,x).
%
  if ( n < 0 )
    value = 0.0;
  else
    m = 1;
    v_vec = v_polynomial ( m, n, x );
    value = v_vec(n+1);
  end

  return
end
