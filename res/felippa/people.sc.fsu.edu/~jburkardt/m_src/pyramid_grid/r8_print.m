function r8_print ( label, r )

%*****************************************************************************80
%
%% r8_print() prints an R8.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string LABEL, a title.
%
%    real R, the value.
%
  if ( 0 < length ( label ) )
    fprintf ( 1, '%s  ', label );
  end
  fprintf ( 1, '%g\n', r );

  return
end

