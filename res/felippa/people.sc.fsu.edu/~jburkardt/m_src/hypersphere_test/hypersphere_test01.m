function hypersphere_test01 ( )

%*****************************************************************************80
%
%% hypersphere_test01() tests the coordinate conversion routines.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 December 2013
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypersphere_test01():\n' );
  fprintf ( 1, '  Test the coordinate conversion routines:\n' );
  fprintf ( 1, '  cartesian_to_hypersphere(): X       -> R,Theta\n' );
  fprintf ( 1, '  hypersphere_to_cartesian(): R,Theta -> X.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Pick a random X, and compute X2 by converting X\n' );
  fprintf ( 1, '  to hypersphere and back.  Consider norm of difference.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  M    | X - X2 |\n' );

  n = 1;
  for m = 1 : 5 
    fprintf ( 1, '\n' );
    for test = 1 : 5
      x = rand ( m, n );
      c = rand ( m, 1 );
      [ r, theta ] = cartesian_to_hypersphere ( m, n, c, x );
      x2 = hypersphere_to_cartesian ( m, n, c, r, theta );
      err = norm ( x - x2 );
      fprintf ( 1, '  %d  %g\n', m, err );
    end
  end

  return
end
