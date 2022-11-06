function r = triangle_inradius ( t )

%*****************************************************************************80
%
%% triangle_inradius(): radius of the inscribed circle of a triangle.
%
%  Discussion:
%
%    The inscribed circle of a triangle is the largest circle that can
%    be drawn inside the triangle.  It is tangent to all three sides,
%    and the lines from its center to the vertices bisect the angles
%    made by each vertex.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer and John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%
%  Output:
%
%    real R, the radius of the inscribed circle.
%
  dim_num = 2;
%
%  Compute the length of each side.
%
  a = sqrt ( sum ( ( t(1:dim_num,1) - t(1:dim_num,2) ).^2 ) );
  b = sqrt ( sum ( ( t(1:dim_num,2) - t(1:dim_num,3) ).^2 ) );
  c = sqrt ( sum ( ( t(1:dim_num,3) - t(1:dim_num,1) ).^2 ) );

  perimeter = a + b + c;

  if ( perimeter == 0.0 )
    r = 0.0;
    return
  end

  r = 0.5 * sqrt ( ...
      ( - a + b + c )  ...
    * ( + a - b + c )  ...
    * ( + a + b - c ) / perimeter );

  return
end
