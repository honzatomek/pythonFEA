function s = boolean_to_string ( b )

%*****************************************************************************80
%
%% boolean_to_string() returns 'True' or 'False' depending on a boolean value.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    boolean B: the boolean value.
%
%  Output:
%
%    string S: the corresponding string.
%
  if ( b )
    s = 'True';
  else
    s = 'False';
  end

  return
end

