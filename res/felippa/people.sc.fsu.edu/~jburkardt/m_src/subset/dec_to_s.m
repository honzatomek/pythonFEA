function s = dec_to_s ( mantissa, exponent )

%*****************************************************************************80
%
%% dec_to_s() returns a string representation of a decimal.
%
%  Discussion:
%
%    A decimal value is represented by MANTISSA * 10^EXPONENT.
%
%  Example:
%
%    MANTISSA EXPONENT   S
%    ----     ----       ------
%       0        0       0
%      21        3       21000
%      -3        0       -3
%     147       -2       14.7
%      16       -5       0.00016
%     123      -21       0.0000000000000000012
%      34      -30       0.0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer MANTISSA, EXPONENT, integers which represent the decimal.
%
%  Output:
%
%    string S, the representation of the value.
%
  p = 0;
  s = '';

  if ( mantissa == 0 )
    p = p + 1;
    s = '0';
    return
  end

  if ( mantissa < 0 )
    p = p + 1;
    s = '-';
    mantissa = abs ( mantissa );
  end
%
%  Mantissas should be normalized so that they do not end in 0!
%
  while ( 10 * floor ( mantissa / 10 ) == mantissa )
    mantissa = mantissa / 10;
    exponent = exponent + 1;
  end
%
%  How many digits are there in the mantissa?
%
  mantissa_digits = 0;
  mantissa_ten = 1;

  while ( mantissa_ten <= mantissa )
    mantissa_ten = mantissa_ten * 10;
    mantissa_digits = mantissa_digits + 1;
  end
%
%  For a positive exponent, we just print the mantissa,
%  possibly followed by some zeros.
%
  if ( 0 <= exponent )

    for i = 1 : mantissa_digits
      mantissa_ten = mantissa_ten / 10;
      d = floor ( mantissa / mantissa_ten );
      mantissa = mantissa - d * mantissa_ten;
      p = p + 1;
      c = digit_to_ch ( d );
      s = [ s, c ];
    end

    for i = 1 : exponent
      p = p + 1;
      s = [ s, '0' ];
    end
%
%  A negative mantissa means, 
%  * possibly some digits, or else 0,
%  * a decimal point,
%  * possibly some zeros
%  * the remaining digits.
%
  elseif ( exponent < 0 )

    if ( 0 < mantissa_digits + exponent )

      for i = 1 : mantissa_digits + exponent
        mantissa_ten = mantissa_ten / 10;
        d = floor ( mantissa / mantissa_ten );
        mantissa = mantissa - d * mantissa_ten;
        p = p + 1;
        c = digit_to_ch ( d );
        s = [ s, c ];
      end

      p = p + 1;
      s = [ s, '.' ];

      ihi = - exponent;

      for i = 1 : ihi
        mantissa_ten = mantissa_ten / 10;
        d = floor ( mantissa / mantissa_ten );
        mantissa = mantissa - d * mantissa_ten;
        p = p + 1;
        c = digit_to_ch ( d );
        s = [ s, c ];
      end

    elseif ( 0 == mantissa_digits + exponent )

      p = p + 2;
      s = [ s, '0', '.' ];

      for i = 1 : mantissa_digits
        mantissa_ten = mantissa_ten / 10;
        d = floor ( mantissa / mantissa_ten );
        mantissa = mantissa - d * mantissa_ten;
        p = p + 1;
        c = digit_to_ch ( d );
        s = [ s, c ];
      end

    elseif ( mantissa_digits + exponent < 0 )

      p = p + 2;
      s = [ s, '0', '.' ];

      ihi = - ( mantissa_digits + exponent );

      for i = 1 : ihi
        p = p + 1;
        s = [ s, '0' ];
      end

      for i = 1 : mantissa_digits
        mantissa_ten = mantissa_ten / 10;
        d = floor ( mantissa / mantissa_ten );
        mantissa = mantissa - d * mantissa_ten;
        p = p + 1;
        c = digit_to_ch ( d );
        s = [ s, c ];
      end

    end

  end

  return
end
