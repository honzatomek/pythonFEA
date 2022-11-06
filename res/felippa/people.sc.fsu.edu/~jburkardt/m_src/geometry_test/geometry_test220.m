function geometry_test220 ( )

%*****************************************************************************80
%
%% geometry_test220() tests VECTOR_DIRECTIONS_ND;
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2009
%
%  Author:
%
%    John Burkardt
%
  dim_num = 2;
  ntest = 5;

  vtest = [ ...
     1.0,        0.0; ...
     1.7320508,  1.0; ...
    -1.7320508,  1.0; ...
    -1.7320508, -1.0; ...
     1.7320508, -1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'TEST220\n' );
  fprintf ( 1, '  VECTOR_DIRECTIONS_ND computes the angles\n' );
  fprintf ( 1, '  that a vector makes with the axes.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '     X        Y       AX       AY       AX       AY\n' );
  fprintf ( 1, '                     (__Radians___)  (___Degrees___)\n' );
  fprintf ( 1, '\n' );

  for i = 1 : ntest

    v(1:dim_num) = vtest(1:dim_num,i);

    angle(1:dim_num) = vector_directions_nd ( dim_num, v );

    for j = 1 : dim_num
      angle_degrees(j) = radians_to_degrees ( angle(j) );
    end

    fprintf ( 1, '  %10f  %10f  %10f  %10f  %10f  %10f\n', ...
      v(1:dim_num), angle(1:dim_num), angle_degrees(1:dim_num) );
 
  end

  return
end
