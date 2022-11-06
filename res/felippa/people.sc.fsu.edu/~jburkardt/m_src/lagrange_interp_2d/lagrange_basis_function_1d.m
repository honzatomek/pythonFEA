function yi = lagrange_basis_function_1d ( mx, xd, i, xi ) 

%*****************************************************************************80
%
%% lagrange_basis_function_1d() evaluates a 1D Lagrange basis function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 August 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer MX, the degree of the basis function.
%
%    real XD(MX+1), the interpolation nodes.
%
%    integer I, the index of the basis function.
%    1 <= I <= MX+1.
%
%    real XI, the evaluation point.
%
%  Output:
%
%    real YI, the value of the I-th Lagrange 1D basis function
%    for the nodes XD, evaluated at XI.
%
  if ( xi == xd(i) )
    yi = 1.0;
  else
    j = find ( 1 : mx + 1 ~= i );
    yi = prod ( xi - xd(j) ) / prod ( xd(i) - xd(j) );
  end

  return
end
