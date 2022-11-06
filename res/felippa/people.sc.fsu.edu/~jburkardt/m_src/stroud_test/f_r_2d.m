function value = f_r_2d ( x, y )

%*****************************************************************************80
%
%% F_R_2D evaluates the function R in 2D.
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
%    real X, Y, the arguments.
%
%  Output:
%
%    real VALUE, the function value.
%
  value = sqrt ( x * x + y * y );

  return
end
