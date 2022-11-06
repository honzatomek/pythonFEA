function integral = tetrahedron01_monomial_integral ( e )

%*****************************************************************************80
%
%% tetrahedron01_monomial_integral(): integrals in the unit tetrahedron in 3D.
%
%  Discussion:
%
%    The monomial is F(X,Y,Z) = X^E(1) * Y^E(2) * Z^E(3).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    14 January 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer E(3), the exponents.  
%    Each exponent must be nonnegative.
%
%  Output:
%
%    real INTEGRAL, the integral.
%
  m = 3;

  if ( any ( e(1:m) < 0 ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON01_MONOMIAL_INTEGRAL - Fatal error!\n' );
    fprintf ( 1, '  All exponents must be nonnegative.\n' );
    error ( 'TETRAHEDRON01_MONOMIAL_INTEGRAL - Fatal error!\n' );
  end

  k = 0;
  integral = 1.0;

  for i = 1 : m

    for j = 1 : e(i)
      k = k + 1;
      integral = integral * j / k;
    end

  end

  for i = 1 : m
    k = k + 1;
    integral = integral / k;
  end

  return
end
