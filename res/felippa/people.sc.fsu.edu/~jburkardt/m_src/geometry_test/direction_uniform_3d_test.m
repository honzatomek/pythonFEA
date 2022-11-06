function direction_uniform_3d_test ( )

%*****************************************************************************80
%
%% direction_uniform_3d_test() tests direction_uniform_3d().
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
  fprintf ( 1, '\n' );
  fprintf ( 1, 'direction_uniform_3d_test():\n' );
  fprintf ( 1, '  direction_uniform_3d() picks a random direction vector.\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10
    vran = direction_uniform_3d ( );
    fprintf ( 1, '  %10f  %10f  %10f\n', vran(1:3) );
  end

  return
end
