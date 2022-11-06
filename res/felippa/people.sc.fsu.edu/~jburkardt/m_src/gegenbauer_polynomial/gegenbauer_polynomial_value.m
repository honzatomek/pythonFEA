function c = gegenbauer_polynomial_value ( m, n, alpha, x )

%*****************************************************************************80
%
%% gegenbauer_polynomial_value() computes the Gegenbauer polynomials C(I,ALPHA)(X).
%
%  Differential equation:
%
%    (1-X*X) Y'' - (2 ALPHA + 1) X Y' + M (M + 2 ALPHA) Y = 0
%
%  Recursion:
%
%    C(0,ALPHA,X) = 1,
%    C(1,ALPHA,X) = 2*ALPHA*X
%    C(M,ALPHA,X) = (  ( 2*M-2+2*ALPHA) * X * C(M-1,ALPHA,X) 
%                    + (  -M+2-2*ALPHA)   *   C(M-2,ALPHA,X) ) / M
%
%  Restrictions:
%
%    ALPHA must be greater than -0.5.
%
%  Special values:
%
%    If ALPHA = 1, the Gegenbauer polynomials reduce to the Chebyshev
%    polynomials of the second kind.
%
%  Norm:
%
%    Integral ( -1 <= X <= 1 ) ( 1 - X^2 )^( ALPHA - 0.5 ) * C(M,ALPHA,X)^2 dX
%
%    = PI * 2^( 1 - 2 * ALPHA ) * Gamma ( M + 2 * ALPHA ) 
%      / ( M! * ( M + ALPHA ) * ( Gamma ( ALPHA ) )^2 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 November 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%  Input:
%
%    integer M, the highest order polynomial to compute.
%    Note that polynomials 0 through N will be computed.
%
%    integer N, the number of evaluation points.
%
%    real ALPHA, a parameter which is part of the definition of
%    the Gegenbauer polynomials.  It must be greater than -0.5.
%
%    real X(N), the evaluation points.
%
%  Output:
%
%    real C(1:M+1,N), the values of Gegenbauer polynomials 0 through M
%    at the N points X.  
%
  check = gegenbauer_alpha_check ( alpha );

  if ( ~check )
    error ( 'GEGENBAUER_POLYNOMIAL_VALUE - Fatal error!' );
  end

  c = zeros ( m + 1, n );

  if ( m < 0 )
    return
  end

  if ( n == 0 )
    return
  end

  c(1,1:n) = 1.0;

  if ( m < 1 )
    return
  end

  c(2,1:n) = 2.0 * alpha * x(1:n)';

  for i = 2 : m
    c(i+1,1:n) = (  (     2 * i - 2  + 2.0 * alpha ) * x(1:n) .* c(i,1:n)     ...
                 +  (       - i + 2  - 2.0 * alpha ) *           c(i-1,1:n) ) ...
                 /            i ;
  end

  return
end
