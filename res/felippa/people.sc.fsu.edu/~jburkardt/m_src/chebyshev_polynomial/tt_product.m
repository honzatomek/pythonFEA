function value = tt_product ( i, j, x )

%*****************************************************************************80
%
%% tt_product(): evaluate T(i,x)*T(j,x)
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
  if ( i < 0 || j < 0 )
    value = 0.0;
  else
    ipj = i + j;
    tipj = t_polynomial_value ( ipj, x );
    imj = abs ( i - j );
    timj = t_polynomial_value ( imj, x );
    value = 0.5 * ( tipj + timj );
  end

  return
end
