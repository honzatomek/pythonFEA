function [ top, bot ] = dec_to_rat ( mantissa, exponent )

%*****************************************************************************80
%
%% dec_to_rat() converts a decimal to a rational representation.
%
%  Discussion:
%
%    A decimal value is represented by MANTISSA * 10^EXPONENT.
%
%    A rational value is represented by TOP / BOT.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer MANTISSA, EXPONENT, the decimal number.
%
%  Output:
%
%    integer TOP, BOT, the rational value.
%
  mantissa = round ( mantissa );
  exponent = round ( exponent );

  if ( exponent == 0 )
    top = mantissa;
    bot = 1;
  elseif ( 0 < exponent )
    top = mantissa * 10 ^ exponent;
    bot = 1;
  else
    top = mantissa;
    bot = 10 ^ ( - exponent );
    div = gcd ( top, bot );
    top = floor ( top / div );
    bot = floor ( bot / div );
  end

  return
end
