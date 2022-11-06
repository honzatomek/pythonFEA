function [ r, s ] = reference_sample ( code )

%*****************************************************************************80
%
%% reference_sample() samples a reference element.
%
%  Discussion:
%
%    The routine either samples the unit triangle or the unit square.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string CODE, identifies the element desired.
%    Legal values include 'Q4', 'Q8', 'Q9', 'Q12', 'Q16', 'QL', 'T3', 
%    'T4', 'T6' and 'T10'.
%
%  Output:
%
%    real R, S, a random point in the reference element.
%
  r = rand ( 1, 1 );
  s = rand ( 1, 1 );

  if ( code(1) == 'T' || code(1) == 't' )

    if ( 1.0 < r + s )
      r = 1.0 - r;
      s = 1.0 - s;
    end

  end

  return
end
