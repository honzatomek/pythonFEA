function inside = cylinder_point_inside ( p1, p2, r, p )

%*****************************************************************************80
%
%% cylinder_point_inside() determines if a cylinder contains a point.
%
%  Discussion:
%
%    The surface and interior of a (right) (finite) cylinder in 3D is defined
%    by an axis, which is the line segment from point P1 to P2, and a
%    radius R.  The points contained in the volume include:
%    * points at a distance less than or equal to R from the line through P1
%      and P2, whose nearest point on the line through P1 and P2 is, in fact,
%      P1, P2, or any point between them.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 August 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real P1(3), P2(3), the first and last points
%    on the axis line of the cylinder.
%
%    real R, the radius of the cylinder.
%
%    real P(3), the point.
%
%  Output:
%
%    logical INSIDE, is TRUE if the point is inside the cylinder.
%
  dim_num = 3;
  
  axis(1:dim_num) = p2(1:dim_num) - p1(1:dim_num);
       
  axis_length = norm ( axis );
  
  axis(1:dim_num) = axis(1:dim_num) / axis_length;

  p_dot_axis = ( p(1:dim_num) - p1(1:dim_num) ) * axis(1:dim_num)';
%
%  If the point lies below or above the "caps" of the cylinder, we're done.
%
  if ( p_dot_axis < 0.0 | axis_length < p_dot_axis )

    inside = false;
%
%  Otherwise, determine the length of the off-axial component,
%  which is the distance from P to the axis.
%
  else

    p_length = norm ( p(1:dim_num) - p1(1:dim_num) );
    
    off_axis_component = sqrt ( p_length.^2 - p_dot_axis.^2 );

    if ( off_axis_component <= r )
      inside = true;
    else
      inside = false;
    end

  end

  return
end
