function alpha_pq = moment_normalized ( n, x, y, p, q )

%*****************************************************************************80
%
%% moment_normalized() computes a normalized moment of a polygon.
%
%  Discussion:
%
%    Alpha(P,Q) = Integral ( x, y in polygon ) x^p y^q dx dy / Area ( polygon )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 April 2019
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Carsten Steger,
%    On the calculation of arbitrary moments of polygons,
%    Technical Report FGBV-96-05,
%    Forschungsgruppe Bildverstehen, Informatik IX,
%    Technische Universitaet Muenchen, October 1996.
%
%  Input:
%
%    integer N, the number of vertices of the polygon.
%
%    real X(N), Y(N), the vertex coordinates.
%
%    integer P, Q, the indices of the moment.
%
%  Output:
%
%    real ALPHA_PQ, the normalized moment Alpha(P,Q).
%
  nu_pq = moment_polygon ( n, x, y, p, q );
  nu_00 = moment_polygon ( n, x, y, 0, 0 );

  alpha_pq = nu_pq / nu_00;

  return
end
