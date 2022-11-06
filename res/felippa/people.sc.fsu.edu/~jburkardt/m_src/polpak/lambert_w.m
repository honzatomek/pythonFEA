function w = lambert_w ( x )

%*****************************************************************************80
%
%% lambert_w() computes the Lambert W function.
%
%  Discussion:
%
%    The function W(X) is defined implicitly by:
%
%      W(X) * e^W(X) = X
%
%    The function is also known as the "Omega" function.
%
%    In Mathematica, the function can be evaluated by:
%
%      W = ProductLog [ X ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Robert Corless, Gaston Gonnet, David Hare, David Jeffrey, Donald Knuth,
%    On the Lambert W Function,
%    Advances in Computational Mathematics,
%    Volume 5, 1996, pages 329-359.
%
%    Brian Hayes,
%    "Why W?",
%    The American Scientist,
%    Volume 93, March-April 2005, pages 104-108.
%
%    Eric Weisstein,
%    CRC Concise Encyclopedia of Mathematics,
%    CRC Press, 2002,
%    Second edition,
%    ISBN: 1584883472,
%    LC: QA5.W45
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Cambridge University Press, 1999,
%    ISBN: 0-521-64314-7,
%    LC: QA76.95.W65.
%
%  Input:
%
%    real X, the argument of the function.
%
%  Output:
%
%    real w: the value of the Lambert W function.
%
  it_max = 100;
  tol = 1.0E-10;

  w = lambert_w_estimate ( x );

  it = 0;

  while ( it <= it_max )

    if ( abs ( ( x - w * exp ( w ) ) ) < tol * abs ( ( w + 1.0 ) * exp ( w ) ) )
      break
    end

    w = w - ( w * exp ( w ) - x ) ...
      / ( ( w + 1.0 ) * exp ( w ) ...
      - ( w + 2.0 ) * ( w * exp ( w ) - x ) ...
      / ( 2.0 * w + 2.0 ) );

    it = it + 1;

  end

  return
end
