function hypersphere_test06 ( )

%*****************************************************************************80
%
%% hypersphere_test06() tests the stereographic mapping.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 May 2013
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hypersphere_test06():\n' );
  fprintf ( 1, '  Test the stereographic mapping:\n' );
  fprintf ( 1, '  hypersphere_stereograph() maps hypersphere points to the plane.\n' );
  fprintf ( 1, '  hypersphere_stereograph_inverse() inverts the mapping.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Pick a random X1 on the hypersphere.\n' );
  fprintf ( 1, '  Map it to a point X2 on the plane.\n' );
  fprintf ( 1, '  Map it back to a point X3 on the hypersphere.\n' );
  fprintf ( 1, '  Consider norm of difference.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  M    || X1 - X3 ||\n' );

  n = 1;
  for m = 2 : 5 
    fprintf ( 1, '\n' );
    for test = 1 : 5
      x1 = hypersphere_01_surface_uniform ( m, n );
      x2 = hypersphere_stereograph ( m, n, x1 );
      x3 = hypersphere_stereograph_inverse ( m, n, x2 );
      err = norm ( x1 - x3 );
      fprintf ( 1, '  %d  %g\n', m, err );
    end
  end

  return
end
