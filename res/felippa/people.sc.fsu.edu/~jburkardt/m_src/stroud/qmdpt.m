function result = qmdpt ( func, n, nsub )

%*****************************************************************************80
%
%% qmdpt() carries out product midpoint quadrature for the unit cube in ND.
%
%  Integration region:
%
%    Points X(1:N) such that:
%
%      -1 <= X(1:N) <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    01 April 2008
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Arthur H Stroud,
%    Approximate Calculation of Multiple Integrals,
%    Prentice Hall, 1971.
%
%  Input:
%
%    external FUNC, the name of the user supplied
%    function which evaluates the function, of the form
%      function value = func ( n, x )
%
%    integer N, the dimension of the cube.
%
%    integer NSUB, the number of subdivisions (in each dimension).
%
%  Output:
%
%    real RESULT, the approximate integral of the function.
%
  w = 1.0E+00 / ( nsub ^ n );
  quad = 0.0E+00;

  more = 0;
  ix(1:n) = 0;

  while ( true )

    [ ix, more ] = vec_lex_next ( n, nsub, ix, more );

    if ( ~ more ) 
      break
    end 

    x(1:n) = ( 2 * ix(1:n) + 1 - nsub ) / nsub;

    quad = quad + w * feval ( func, n, x );

  end

  volume = 2.0E+00^n;
  result = quad * volume;

  return
end
