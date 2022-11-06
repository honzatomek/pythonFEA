function triangle_circumcenter_nd_test ( )

%*****************************************************************************80
%
%% triangle_circumcenter_nd_test() tests triangle_circumcenter_nd().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    28 October 2010
%
%  Author:
%
%    John Burkardt
%
  m1 = 2;
  test_num = 4;
  t_test = cat ( 3, ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      10.0,  6.0 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      10.5,  5.86602539 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      10.5, 15.0 ]', ...
    [ 10.0,  5.0; ...
      11.0,  5.0; ...
      20.0,   7.0 ]' );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_circumcenter_nd_test():\n' );
  fprintf ( 1, '  triangle_circumcenter_nd() computes the circumcenter\n' );
  fprintf ( 1, '  of a triangle in ND.\n' );
%
%  Vary the dimension.
%
  for m2 = 2 : 5

    fprintf ( 1, '\n' );
    fprintf ( 1, '  M2 = %d\n', m2 );
%
%  Randomly choose a mapping P2 = O2 + A12 * ( P1 - O1 )
%
    a12 = rand ( m2, m1 );
    o1 = rand ( m1, 1 );
    o2 = rand ( m2, 1 );
%
%  Map each M1-dimensional triangle into M2 space.
%
    for test = 1 : test_num

      t1(1:m1,1:3) = t_test(1:m1,1:3,test);

      for j = 1 : 3
        for i = 1 : m1
          t1(i,j) = t1(i,j) - o1(i);
        end
      end

      t2(1:m2,1:3) = a12(1:m2,1:m1) * t1(1:m1,1:3);

      for j = 1 : 3
        for i = 1 : m2
          t2(i,j) = t2(i,j) + o2(i);
        end
      end

      pc2 = triangle_circumcenter_nd ( m2, t2 );

      r8vec_print ( m2, pc2, '  circumcenter:' );
      fprintf ( 1, '\n' );
      fprintf ( 1, '  Distances from circumcenter to vertices:\n' );
      fprintf ( 1, '\n' );
      for j = 1 : 3
        fprintf ( 1, '  %f\n', r8vec_norm_affine ( m2, pc2,      t2(1:m2,j) ) );
      end

    end

  end

  return
end
