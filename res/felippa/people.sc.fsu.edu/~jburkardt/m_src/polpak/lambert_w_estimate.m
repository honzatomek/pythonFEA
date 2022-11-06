function value = lambert_w_estimate ( x )

%*****************************************************************************80
%
%% lambert_w_estimate() is estimates the Lambert W function.
%
%  Discussion:
%
%    This crude approximation can be used as a good starting point
%    for an iterative process.
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
%    12 June 2022
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
%    real X: the argument of the function.
%
%  Output:
%
%    real value: an estimate for the Lambert W function.
%
  if ( x <= 500.0 )

    value = 0.04 + 0.665 * ( 1.0 + 0.0195 * log ( x + 1.0 ) ) * log ( x + 1.0 );

  else

    value = log ( x - 4.0 ) - ( 1.0 - 1.0 / log ( x ) ) * log ( log ( x ) );

  end

  return
end
