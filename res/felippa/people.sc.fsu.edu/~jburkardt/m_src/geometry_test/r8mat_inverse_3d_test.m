function r8mat_inverse_3d_test ( )

%*****************************************************************************80
%
%% r8mat_inverse_3d_test() tests r8mat_inverse_3d().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 April 2009
%
%  Author:
%
%    John Burkardt
%
  n = 3;
%
%  Each ROW of this definion is a COLUMN of the matrix.
%
  a = [ ...
    1.0, 4.0, 7.0; ...
    2.0, 5.0, 8.0; ...
    3.0, 6.0, 0.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'r8mat_inverse_3d_test():\n' );
  fprintf ( 1, '  r8mat_inverse_3d() inverts a 3 by 3 matrix.\n' );

  r8mat_print ( n, n, a, '  Matrix A to be inverted:' )
%
%  Compute the inverse matrix.
%
  [ b, det ] = r8mat_inverse_3d ( a );
 
  if ( det == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, '  The input matrix was singular, no inverse\n' );
    fprintf ( 1, '  could be computed.\n' );
    return
  end

  r8mat_print ( n, n, b, '  Inverse matrix B:' );

  c =  a * b;

  r8mat_print ( n, n, c, '  Product C = A * B:' );
 
  return
end
