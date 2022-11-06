function angle_rad_test ( )

%*****************************************************************************80
%
%% angle_rad_test() tests angle_rad().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2010
%
%  Author:
%
%    John Burkardt
%
  ntest = 6;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'angle_rad_test():\n' );
  fprintf ( 1, '  angle_rad() computes the angle between two rays;\n' );
  fprintf ( 1, '\n' );

  for j = 1 : ntest

    if ( j == 1 )

      p1(1:2,1) = [ 1.0; 0.0 ];
      p2(1:2,1) = [ 0.0; 0.0 ];
      p3(1:2,1) = [ 1.0; 1.0 ];

    elseif ( j == 2 )

      p1(1:2,1) = [ 1.0; 0.0 ];
      p2(1:2,1) = [ 0.0; 0.0 ];
      p3(1:2,1) = [ 0.0; 1.0 ];

    elseif ( j == 3 )

      p1(1:2,1) = [ 1.0; -1.0 ];
      p2(1:2,1) = [ 0.0;  0.0 ];
      p3(1:2,1) = [ 0.0;  1.0 ];

    elseif ( j == 4 )

      p1(1:2,1) = [  1.0; 0.0 ];
      p2(1:2,1) = [  0.0; 0.0 ];
      p3(1:2,1) = [ -1.0; 0.0 ];

    elseif ( j == 5 )

      p1(1:2,1) = [ 1.0;  0.0 ];
      p2(1:2,1) = [ 0.0;  0.0 ];
      p3(1:2,1) = [ 0.0; -1.0 ];

    elseif ( j == 6 )

      p1(1:2,1) = [ 1.0;  0.0 ];
      p2(1:2,1) = [ 0.0;  0.0 ];
      p3(1:2,1) = [ 1.0; -0.01 ];

    end

    angle = angle_rad ( p1, p2, p3 );

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Angle = %f\n', angle );

  end

  return
end
