function tetrahedron_sample_test ( )

%*****************************************************************************80
%
%% tetrahedron_sample_test() tests tetrahedron_sample().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 January 2007
%
%  Author:
%
%    John Burkardt
%
  t = [ ...
     1.0, 4.0, 3.0; ...
     2.0, 4.0, 3.0; ...
     1.0, 6.0, 3.0; ...
     1.0, 4.0, 4.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'tetrahedron_sample_test():\n' );
  fprintf ( 1, '  tetrahedron_sample() samples a tetrahedron.\n' );
  fprintf ( 1, '  We are computing the XSI coordinates just to verify\n' );
  fprintf ( 1, '  that the points are inside the tetrahedron.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  tetrahedron vertices:\n' );
  disp ( t );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  (X,Y,Z)   (XSI1,XSI2,XSI3,XSI4):\n' );
  fprintf ( 1, '\n' );

  for i = 1 : 10
    p = tetrahedron_sample ( t, 1 );
    xsi = tetrahedron_barycentric ( t, p );
    fprintf ( 1, '  %8f  %8f  %8f    %8f  %8f  %8f  %8f\n', p(1:3), xsi(1:4) );
  end

  return
end
