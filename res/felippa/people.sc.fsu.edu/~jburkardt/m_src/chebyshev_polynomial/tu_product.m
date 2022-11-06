function value = tu_product ( i, j, x )

%*****************************************************************************80
%
%% tu_product(): evaluate T(i,x)*U(j,x)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 July 2015
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
  if ( i < 0 )
    value = 0.0;
  elseif ( j < 0 )
    value = 0.0;
  elseif ( i == 1 )
    value = u_polynomial_value ( j, x );
  else
    value = 0.5 * ( uu_product ( i, j, x ) - uu_product ( i - 2, j, x ) );
  end

  return
end
