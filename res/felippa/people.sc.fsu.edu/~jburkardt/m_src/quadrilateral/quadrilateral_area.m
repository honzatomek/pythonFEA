function area = quadrilateral_area ( quad )

%*****************************************************************************80
%
%% quadrilateral_area() computes the area of a quadrilateral.
%
%  Discussion:
%
%    This algorithm should be able to handle nonconvex quadrilaterals.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 October 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real QUAD(2,4), the X and Y coordinates
%    of the corners of the quadrilateral.  The corners should be
%    specified in clockwise or counterclockwise order.
%
%  Output:
%
%    real AREA, the area of the quadrilateral.
%
  dim_num = 2;
  
  area = 0.0;

  t(1:dim_num,1:3) = quad(1:dim_num,1:3);
  
  area = area + triangle_area ( t );

  t(1:dim_num,1:2) = quad(1:dim_num,3:4);
  t(1:dim_num,3) = quad(1:dim_num,1);
  
  area = area + triangle_area ( t );

  return
end
