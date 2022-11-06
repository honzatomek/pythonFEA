function cylinder_volume_test ( )

%*****************************************************************************80
%
%% cylinder_volume_test() tests cylinder_volume().
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
  p1 = [ 1.0, 2.0, 3.0 ];
  p2 = [ 5.0, 6.0, 5.0 ];
  r = 5.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'cylinder_volume_test():\n' );
  fprintf ( 1, '  cylinder_volume() computes the volume of a cylinder.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Radius R = %f\n', r );
  fprintf ( 1, '  Center of bottom disk = %14f  %14f  %14f\n', p1(1:3) );
  fprintf ( 1, '  Center of top disk =    %14f  %14f  %14f\n', p2(1:3) );

  volume = cylinder_volume ( p1, p2, r );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Volume (computed) = %f\n', volume );
  fprintf ( 1, '  Volume (exact)    = %f\n', pi * 150.0 );

  return
end
