function line_exp_print_2d ( p1, p2, label )

%*****************************************************************************80
%
%% line_exp_print_2d(): prints an explicit line in 2D.
%
%  Discussion:
%
%    The explicit form of a line in 3D is:
%
%      the line through the distinct points P1 and P2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 September 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(2), P2(2), two points on the line.
%
%    string label: a label for the line.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '%s\n', label );
  fprintf ( 1, '  ( %f, %f )\n', p1(1:2) );
  fprintf ( 1, '  ( %f, %f )\n', p2(1:2) );

  return
end
