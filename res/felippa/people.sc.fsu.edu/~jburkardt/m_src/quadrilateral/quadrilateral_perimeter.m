function perimeter = quadrilateral_perimeter ( xy )

%*****************************************************************************80
%
%% quadrilateral_perimeter() computes the perimater of a quadrilateral.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real XY(2,4), the X and Y coordinates
%    of the corners of the quadrilateral.  The corners should be
%    specified in clockwise or counterclockwise order.
%
%  Output:
%
%    real PERIMETER, the length of the perimeter.
%
  perimeter = 0.0;
  im1 = 4;
  for i = 1 : 4
    perimeter = perimeter + norm ( xy(1:2,i) - xy(1:2,im1) );
    im1 = i;
  end

  return
end
