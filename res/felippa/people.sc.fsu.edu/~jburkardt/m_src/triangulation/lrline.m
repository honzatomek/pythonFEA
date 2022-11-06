function value = lrline ( xu, yu, xv1, yv1, xv2, yv2, dv )

%*****************************************************************************80
%
%% lrline() determines if a point is left of, right or, or on a directed line.
%
%  Discussion:
%
%    The directed line is parallel to, and at a signed distance DV from
%    a directed base line from (XV1,YV1) to (XV2,YV2).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2005
%
%  Author:
%
%    Original FORTRAN77 version by Barry Joe,
%    MATLAB version by John Burkardt
%
%  Reference:
%
%    Barry Joe,
%    GEOMPACK - a software package for the generation of meshes
%    using geometric algorithms,
%    Advances in Engineering Software,
%    Volume 13, pages 325-331, 1991.
%
%  Input:
%
%    real XU, YU, the coordinates of the point whose
%    position relative to the directed line is to be determined.
%
%    real XV1, YV1, XV2, YV2, the coordinates of two points
%    that determine the directed base line.
%
%    real DV, the signed distance of the directed line
%    from the directed base line through the points (XV1,YV1) and (XV2,YV2).
%    DV is positive for a line to the left of the base line.
%
%  Output:
%
%    integer VALUE, the result:
%    +1, the point is to the right of the directed line;
%     0, the point is on the directed line;
%    -1, the point is to the left of the directed line.
%
  tol = 100.0 * eps;

  dx = xv2 - xv1;
  dy = yv2 - yv1;
  dxu = xu - xv1;
  dyu = yu - yv1;

  tolabs = tol * max ( abs ( dx ), ...
                 max ( abs ( dy ), ...
                 max ( abs ( dxu ), ...
                 max ( abs ( dyu ), abs ( dv ) ) ) ) );

  t = dy * dxu - dx * dyu + dv * sqrt ( dx * dx + dy * dy );

  if ( tolabs < t )
    value = 1;
  elseif ( -tolabs <= t )
    value = 0;
  else
    value = -1;
  end

  return
end
