function c = digit_to_ch ( digit )

%*****************************************************************************80
%
%% digit_to_ch() returns the character representation of a decimal digit.
%
%  Example:
%
%    DIGIT   C
%    -----  ---
%      0    '0'
%      1    '1'
%    ...    ...
%      9    '9'
%     17    '*'
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 April 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIGIT, the digit value between 0 and 9.
%
%  Output:
%
%    character C, the corresponding character, or '*' if DIGIT
%    was illegal.
%
  if ( 0 <= digit && digit <= 9 )
    c = char ( '0' + digit );
  else
    c = '*';
  end

  return
end
