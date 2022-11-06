function [ x, w ] = nco_set ( n )

%*****************************************************************************80
%
%% nco_set() sets abscissas and weights for open Newton-Cotes quadrature.
%
%  Discussion:
%
%    The open Newton-Cotes rules use equally spaced abscissas, and
%    hence may be used with equally spaced data.
%
%    The rules are called "open" because they do not include the interval
%    endpoints.
%
%    Most of the rules involve negative weights.  These can produce loss
%    of accuracy due to the subtraction of large, nearly equal quantities.
%
%    The integral:
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Abramowitz and Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%    Daniel Zwillinger, editor,
%    Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Input:
%
%    integer N, the order.
%    N must be between 1 and 10.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  x = zeros ( n, 1 );
  w = zeros ( n, 1 );

  if ( n == 1 )

    w(1) = 2.0;

  elseif ( n == 2 )

    w(1) = 1.0;
    w(2) = 1.0;

  elseif ( n == 3 )

    d = 3.0;

    w(1) =   4.0 / d;
    w(2) = - 2.0 / d;
    w(3) =   4.0 / d;

  elseif ( n == 4 )

    d = 12.0;

    w(1) = 11.0 / d;
    w(2) =  1.0 / d;
    w(3) =  1.0 / d;
    w(4) = 11.0 / d;

  elseif ( n == 5 )

    d = 10.0;

    w(1) =   11.0 / d;
    w(2) = - 14.0 / d;
    w(3) =   26.0 / d;
    w(4) = - 14.0 / d;
    w(5) =   11.0 / d;

  elseif ( n == 6 )

    d = 1440.0;

    w(1) =  1222.0 / d;
    w(2) = - 906.0 / d;
    w(3) =  1124.0 / d;
    w(4) =  1124.0 / d;
    w(5) = - 906.0 / d;
    w(6) =  1222.0 / d;

  elseif ( n == 7 )

    d = 945.0;

    w(1) =    920.0 / d;
    w(2) = - 1908.0 / d;
    w(3) =   4392.0 / d;
    w(4) = - 4918.0 / d;
    w(5) =   4392.0 / d;
    w(6) = - 1908.0 / d;
    w(7) =    920.0 / d;

  elseif ( n == 8 )

    d = 40320.0;

    w(1) =   32166.0 / d;
    w(2) = - 50454.0 / d;
    w(3) =   89406.0 / d;
    w(4) = - 30798.0 / d;
    w(5) = - 30798.0 / d;
    w(6) =   89406.0 / d;
    w(7) = - 50454.0 / d;
    w(8) =   32166.0 / d;

  elseif ( n == 9 )

    d = 4536.0;

    w(1) =    4045.0 / d;
    w(2) = - 11690.0 / d;
    w(3) =   33340.0 / d;
    w(4) = - 55070.0 / d;
    w(5) =   67822.0 / d;
    w(6) = - 55070.0 / d;
    w(7) =   33340.0 / d;
    w(8) = - 11690.0 / d;
    w(9) =    4045.0 / d;

  elseif ( n == 10 )

    w(1) =    0.758508873456792;
    w(2) =   -1.819664627425049;
    w(3) =    4.319301146384676;
    w(4) =   -4.708337742504753;
    w(5) =    2.450192350088813;
    w(6) =    2.450192350087711;
    w(7) =   -4.708337742504625;
    w(8) =    4.319301146384526;
    w(9) =   -1.819664627425028;
    w(10) =   0.758508873456790;

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'NCO_SET - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of N = %d\n', n );
    fprintf ( 1, '  Legal values are 1 and 10.\n' );
    error ( 'NCO_SET - Fatal error!' );

  end
%
%  Set the abscissas.
%
  for i = 1 : n
    x(i) = ( 2 * i - n - 1 ) / ( n + 1 );
  end

  return
end
