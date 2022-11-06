function vpp = p_polynomial_prime2 ( m, n, x )

%*****************************************************************************80
%
%% p_polynomial_prime2(): second derivative of Legendre polynomials P(n,x).
%
%  Discussion:
%
%    P(0,X) = 1
%    P(1,X) = X
%    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
%
%    P'(0,X) = 0
%    P'(1,X) = 1
%    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
%
%    P"(0,X) = 0
%    P"(1,X) = 0
%    P"(N,X) = ( (2*N-1)*(2*P'(N-1,X)+X*P"(N-1,X)-(N-1)*P'(N-2,X) ) / N
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 May 2013
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    ISBN: 0-486-61272-4,
%    LC: QA47.A34.
%
%    Daniel Zwillinger, editor,
%    CRC Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Input:
%
%    integer M, the number of evaluation points.
%
%    integer N, the highest order polynomial to evaluate.
%    Note that polynomials 0 through N will be evaluated.
%
%    real X(M,1), the evaluation points.
%
%  Output:
%
%    real VPP(M,N+1), the second derivatives of the
%    Legendre polynomials of order 0 through N at the point X.
%

%
%  Destroy all row vectors!
%
  x = x(:);

  if ( n < 0 )
    vpp = [];
    return
  end

  v = zeros ( m, n + 1 );
  vp = zeros ( m, n + 1 );
  vpp = zeros ( m, n + 1 );

  v(1:m,1) = 1.0;
  vp(1:m,1) = 0.0;
  vpp(1:m,1) = 0.0;

  if ( n < 1 )
    return
  end

  v(1:m,2) = x(1:m,1);
  vp(1:m,2) = 1.0;
  vpp(1:m,2) = 0.0;
 
  for i = 2 : n
 
    v(1:m,i+1) = ( ( 2 * i - 1 ) * x(1:m,1) .* v(1:m,i)     ...
                 - (     i - 1 ) *             v(1:m,i-1) ) ...
                 / (     i     );
 
    vp(1:m,i+1) = ( ( 2 * i - 1 ) * ( v(1:m,i) + x(1:m,1) .* vp(1:m,i) )   ...
                  - (     i - 1 ) *                          vp(1:m,i-1) ) ...
                  / (     i     );

    vpp(1:m,i+1) = ( ( 2 * i - 1 ) * ( 2.0 * vp(1:m,i) + x(1:m,1) .* vpp(1:m,i) )   ...
                  - (     i - 1 ) *                                  vpp(1:m,i-1) ) ...
                  / (     i     );
 
  end
 
  return
end
