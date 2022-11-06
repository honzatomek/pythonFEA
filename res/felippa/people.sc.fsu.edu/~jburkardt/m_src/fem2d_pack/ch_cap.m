function c = ch_cap ( c )

%*****************************************************************************80
%
%% ch_cap() capitalizes a single character.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 May 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    character C, the character to capitalize.
%
%  Output:
%
%    character C, the capitalized character.
%
  if ( 'a' <= c && c <= 'z' )
    c = c + 'A' - 'a';
  end

  c = char ( c );

  return
end
