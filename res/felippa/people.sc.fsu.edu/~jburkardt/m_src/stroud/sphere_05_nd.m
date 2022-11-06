function result = sphere_05_nd ( func, n, xc, r )

%*****************************************************************************80
%
%% sphere_05_nd() approximates surface integrals on a sphere in ND.
%
%  Integration region:
%
%    R1^2 <= sum ( X(1:N) - XC(1:N) )^2 <= R2^2.
%
%  Discussion:
%
%    A 2*N+2**N points 5-th degree formula is used, Stroud number UN:5-2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    29 November 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Arthur Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Input:
%
%    external FUNC, the name of the user supplied
%    function which evaluates F(X), at the N dimensional point
%    X, of the form
%      function value = func ( n, x )
%
%    integer N, the dimension of the space.
%
%    real XC(N), the center of the sphere.
%
%    real R, the radius of the sphere.
%
%  Output:
%
%    real RESULT, the approximate integral of the function.
%
  x1 = 1.0;
  x2 = 1.0 / sqrt ( n );

  w1 = 1.0 / ( n * ( n + 2 ) );
  w2 = n / ( ( n + 2 ) * 2^n );

  x(1:n) = xc(1:n);

  quad = 0.0;

  for i = 1 : n
    x(i) = xc(i) + r * x1;
    quad = quad + w1 * feval ( func, n, x );
    x(i) = xc(i) - r * x1;
    quad = quad + w1 * feval ( func, n, x );
    x(i) = xc(i);
  end
%
%  For this computation, we need to keep R8VEC_MIRROR_NEXT happy
%  by storing in X the "perturbation" from XC, for which we
%  interested in computing all possible sign variations.
%
  x(1:n) = abs ( r ) * x2;

  while ( true )

    quad = quad + w2 * feval ( func, n, xc(1:n) + x(1:n) );

    [ x, done ] = r8vec_mirror_next ( n, x );

    if ( done )
      break
    end

  end

  volume = sphere_area_nd ( n, r );
  result = quad * volume;

  return
end
