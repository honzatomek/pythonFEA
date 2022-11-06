function polygon_perimeter_quad_test ( )

%*****************************************************************************80
%
%% polygon_perimeter_quad_test() tests polygon_perimeter_quad().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    20 October 2015
%
%  Author:
%
%    John Burkardt
%
  n1 = 4;
  n2 = 3;
 
  v1 = [ ...
    0.0, 0.0; ...
    1.0, 0.0; ...
    1.0, 1.0; ...
    0.0, 1.0 ]';
  v2 = [ ...
    1.0, 1.0; ...
    4.0, 3.0; ...
    2.0, 5.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'polygon_perimeter_quad_test():\n' );
  fprintf ( 1, '  polygon_perimeter_quad() estimates the integral of\n' );
  fprintf ( 1, '  a function over the perimeter of a polygon using\n' );
  fprintf ( 1, '  the composite midpoint rule over each side.\n' );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v1 );

  hmax = 0.5;
  value = polygon_perimeter_quad ( n1, v1, hmax, @f1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Using HMAX = %g, estimated integral of 1 over perimeter = %g\n', hmax, value );

  fprintf ( 1, '\n' );
  hmax = 2.0;
  for i = 1 : 3
    hmax = hmax / 2.0;
    value = polygon_perimeter_quad ( n1, v1, hmax, @fx2 );
    fprintf ( 1, '  Using HMAX = %g, estimated integral of x^2 over perimeter = %g\n', hmax, value );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '  polygon vertices:\n' );
  disp ( v2 );

  hmax = 0.5;
  value = polygon_perimeter_quad ( n2, v2, hmax, @f1 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Using HMAX = %g, estimated integral of 1 over perimeter = %g\n', hmax, value );

  fprintf ( 1, '\n' );
  hmax = 2.0;
  for i = 1 : 3
    hmax = hmax / 2.0;
    value = polygon_perimeter_quad ( n2, v2, hmax, @fx2 );
    fprintf ( 1, '  Using HMAX = %g, estimated integral of x^2 over perimeter = %g\n', hmax, value );
  end

  return
end
function value = f1 ( x, y )

%*****************************************************************************80
%
%% f1() evaluates f(x,y) = 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    19 October 2015
%
%  Author:
%
%    John Burkardt
%
  value = 1.0;

  return
end
function value = fx2 ( x, y )

%*****************************************************************************80
%
%% fx2() evaluates f(x,y) = x^2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    19 October 2015
%
%  Author:
%
%    John Burkardt
%
  value = x ^ 2;

  return
end
 
