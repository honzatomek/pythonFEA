function area = quadrilateral_area2 ( q )

%*****************************************************************************80
%
%% quadrilateral_area2() computes the area of a quadrilateral.
%
%  Discussion:
%
%    A quadrilateral is a polygon defined by 4 vertices.
%
%    This algorithm computes the area of the related
%    Varignon parallelogram first.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real Q(2,4), the vertices, specified in
%    counter clockwise order.
%
%  Output:
%
%    real AREA, the area of the quadrilateral.
%

%
%  Define a parallelogram by averaging consecutive vertices.
%
  p(1:2,1:3) = ( q(1:2,1:3) + q(1:2,2:4) ) / 2.0;
  p(1:2,  4) = ( q(1:2,  4) + q(1:2,1  ) ) / 2.0;
%
%  Compute the area.
%
  area = parallelogram_area ( p );
%
%  The quadrilateral's area is twice that of the parallelogram.
%
  area = 2.0 * area;

  return
end
