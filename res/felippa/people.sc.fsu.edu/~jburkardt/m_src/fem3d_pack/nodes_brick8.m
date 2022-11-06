function p = nodes_brick8 ( )

%*****************************************************************************80
%
%% nodes_brick8() returns the natural coordinates of the BRICK8 element.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 2010
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real P(3,8), the coordinates.
%
  p = [ ...
    -1.0, -1.0, -1.0; ...
    +1.0, -1.0, -1.0; ...
    +1.0, +1.0, -1.0; ...
    -1.0, +1.0, -1.0; ...
    -1.0, -1.0, +1.0; ...
    +1.0, -1.0, +1.0; ...
    +1.0, +1.0, +1.0; ...
    -1.0, +1.0, +1.0 ]';

  return
end
