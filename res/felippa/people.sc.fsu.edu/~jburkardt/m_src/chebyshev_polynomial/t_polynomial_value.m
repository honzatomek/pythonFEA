function value = t_polynomial_value ( n, x )

%*****************************************************************************80
%
%% t_polynomial_value(): returns the single value T(n,x).
%
%  Discussion:
%
%    In cases where calling T_POLYNOMIAL is inconvenient, because it returns
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
%    real VALUE, the value of T(n,x).
%
  if ( n < 0 )
    value = 0.0;
  else
    m = 1;
    v_vec = t_polynomial ( m, n, x );
    value = v_vec(n+1);
  end

  return
end
