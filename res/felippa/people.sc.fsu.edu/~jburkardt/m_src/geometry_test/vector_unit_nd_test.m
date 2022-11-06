function vector_unit_nd_test ( )

%*****************************************************************************80
%
%% vector_unit_nd_test() tests vector_unit_nd();
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'vector_unit_nd_test():\n' );
  fprintf ( 1, '  vector_unit_nd() normalizes a vector in ND.\n' );
 
  m = 5;
  x = -10.0 + 20.0 * rand ( m, 1 );
  r8vec_print ( m, x, '  Original vector:' );
  x_norm = norm ( x );
  fprintf ( 1, '  Norm of original vector = %g\n', x_norm );

  x = vector_unit_nd ( m, x );
  r8vec_print ( m, x, '  Normalized vector:' );
  x_norm = norm ( x );
  fprintf ( 1, '  Norm of normalized vector = %g\n', x_norm );

  return
end
