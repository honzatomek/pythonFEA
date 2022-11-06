function value = uu_product ( i, j, x )

%*****************************************************************************80
%
%% uu_product(): evaluate U(i,x)*U(j,x)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, J, the indices.
%
%    real X, the argument.
%
%  Output:
%
%    real VALUE, the value.
%
  value = 0.0;
  for k = abs ( i - j ) : 2 : i + j
    value = value + u_polynomial_value ( k, x );
  end

  return
end
