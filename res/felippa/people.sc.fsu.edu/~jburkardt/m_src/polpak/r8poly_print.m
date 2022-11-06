function r8poly_print ( c, label )

%*****************************************************************************80
%
%% r8poly_print() prints out a polynomial.
%
%  Discussion:
%
%    The power sum form of a polynomial is:
%
%      p(x) = c(0) + c(1) * x + ... + c(n-1) * x^(n-1) + c(n) * x^(n)
%
%    Because MATLAB doesn't allow 0 indexing, we use an adjusted formula:
%
%      p(x) = c(1) + c(2) * x + ... + c(n) * x^(n-1) + c(n+1) * x^(n)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 October 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real C(1:D+1): the polynomial.
%
%    character LABEL: an optional title.
%
  d = length ( c ) - 1;

  fprintf ( 1, '\n' );
  if ( 0 < length ( label ) )
    fprintf ( 1, '%s = \n', label );
  end

  if ( d < 0 )
    fprintf ( 1, '  Zero polynomial\n' );
    return
  end

  if ( all ( c == 0 ) )
    fprintf ( 1, '  0\n' );
    return
  end

  for i = d : -1 : 0

    if ( c(i+1) ~= 0 )

      if ( 2 <= i )
        fprintf ( 1, '%+14f * x^%d\n', c(i+1), i );
      elseif ( i == 1 )
        fprintf ( 1, '%+14f * x\n', c(i+1) );
      elseif ( i == 0 )
        fprintf ( 1, '%+14f\n', c(i+1) );
      end

    end

  end

  return
end
