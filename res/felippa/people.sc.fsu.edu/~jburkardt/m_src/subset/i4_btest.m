function value = i4_btest ( i4, pos )

%*****************************************************************************80
%
%% i4_btest() returns true if the POS-th bit of an I4 is 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 June 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Military Standard 1753,
%    FORTRAN, DoD Supplement To American National Standard X3.9-1978,
%    9 November 1978.
%
%  Input:
%
%    integer I4, the integer to be tested.
%
%    integer POS, the bit position, between 0 and 31.
%
%  Output:
%
%    logical VALUE, is true if the POS-th bit of I4 is 1.
%
  i4_huge = 2147483647;

  if ( pos < 0 )

    fprintf ( 1, '\n' );
    fprintf ( 1, 'I4_BTEST - Fatal error!\n' );
    fprintf ( 1, '  POS < 0.\n' );
    error ( 'I4_BTEST - Fatal error!' );

  elseif ( pos < 31 )

    if ( 0 <= i4 )
      j = floor ( i4 );
    else
      j = floor ( ( i4_huge + i4 ) + 1 );
    end

    for k = 1 : pos
      j = floor ( j / 2 );
    end

    if ( mod ( j, 2 ) == 0 )
      value = false;
    else
      value = true;
    end

  elseif ( pos == 31 )

    if ( i4 < 0 )
      value = true;
    else
      value = false;
    end

  elseif ( 31 < pos )

    fprintf ( 1, '\n' );
    fprintf ( 1, 'I4_BTEST - Fatal error!\n' );
    fprintf ( 1, '  31 < POS.\n' );
    error ( 'I4_BTEST - Fatal error!' );

  end

  return
end
