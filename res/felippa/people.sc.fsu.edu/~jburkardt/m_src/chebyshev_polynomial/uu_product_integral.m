function value = uu_product_integral ( i, j )

%*****************************************************************************80
%
%% uu_product_integral(): integral (-1<=x<=1) U(i,x)*U(j,x)*sqrt(1-x^2) dx
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 April 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, J, the polynomial indices.
%    0 <= I, J.
%
%  Output:
%
%    real VALUE, the value of the integral.
%
  if ( i < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'UU_PRODUCT_INTEGRAL - Fatal error!\n' );
    fprintf ( 1, '  0 <= I is required.\n' );
    error ( 'UU_PRODUCT_INTEGRAL - Fatal error!' );
  end

  if ( j < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'UU_PRODUCT_INTEGRAL - Fatal error!\n' );
    fprintf ( 1, '  0 <= J is required.\n' );
    error ( 'UU_PRODUCT_INTEGRAL - Fatal error!' );
  end

  if ( i ~= j )
    value = 0.0;
  else
    value = pi / 2.0;
  end

  return
end
