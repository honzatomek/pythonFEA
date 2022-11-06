function hypersphere_test02 ( )

%*****************************************************************************80
%
%% hypersphere_test02() tests hypersphere_01_surface_uniform().
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
  fprintf ( 1, 'hypersphere_test02\n' );
  fprintf ( 1, '  hypersphere_01_surface_uniform() samples uniformly from the\n' );
  fprintf ( 1, '  surface of the unit hypersphere\n' );

  n = 1;
  for m = 1 : 5
    for test = 1 : 3
      x = hypersphere_01_surface_uniform ( m, n );
      r8vec_transpose_print ( m, x, '  Random hypersphere point:' );
    end
  end

  return
end
