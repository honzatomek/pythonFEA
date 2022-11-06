function q = triangle01_monomial_integral ( i, j )

%*****************************************************************************80
%
%% triangle01_monomial_integral(): monomial integrals in the unit triangle.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 April 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, J, the exponents.  
%    Each exponent must be nonnegative.
%
%  Output:
%
%    real Q, the integral.
%
  k = 0;
  q = 1.0;

  for l = 1 : i
    k = k + 1;
    q = q * l / k;
  end

  for l = 1 : j
    k = k + 1;
    q = q * l / k;
  end

  for l = 1 : 2
    k = k + 1;
    q = q / k;
  end

  return
end
