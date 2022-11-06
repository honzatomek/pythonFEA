function d = drectangle0 ( p, x1, x2, y1, y2 )

%*****************************************************************************80
%
%% drectangle0() returns the signed distance of one or more points to a rectangle.
%
%  Licensing:
%
%    (C) 2004 Per-Olof Persson. 
%    See COPYRIGHT.TXT for details.
%
%  Modified:
%
%    16 December 2020
%
%  Reference:
%
%    Per-Olof Persson and Gilbert Strang,
%    A Simple Mesh Generator in MATLAB,
%    SIAM Review,
%    Volume 46, Number 2, June 2004, pages 329-345.
%
%  Input:
%
%    real P(NP,2), the coordinates of a set of nodes.
%
%    real X1, X2, Y1, Y2, the left, right, bottom and top
%    coordinates of the rectangle.
%
%  Output:
%
%    real D, the signed distance of the points to the rectangle,
%    which is negative, 0 or positive depending on whether each point
%    is inside, or, or outside the rectangle.
%
  d1 =  y1 - p(:,2);
  d2 = -y2 + p(:,2);
  d3 =  x1 - p(:,1);
  d4 = -x2 + p(:,1);

  d5 = sqrt ( d1.^2 + d3.^2 );
  d6 = sqrt ( d1.^2 + d4.^2 );
  d7 = sqrt ( d2.^2 + d3.^2 );
  d8 = sqrt ( d2.^2 + d4.^2 );

  d = - min ( min ( min ( -d1, -d2 ), -d3 ), -d4 );

  ix = d1>0 && d3>0;
  d(ix) = d5(ix);
  ix = d1>0 && d4>0;
  d(ix) = d6(ix);
  ix = d2>0 && d3>0;
  d(ix) = d7(ix);
  ix = d2>0 && d4>0;
  d(ix) = d8(ix);

  return
end
