function value = angle_rad_nd ( dim_num, v1, v2 )

%*****************************************************************************80
%
%% angle_rad_nd() returns the angle in radians between two rays in ND.
%
%  Discussion:
%
%    This routine always computes the SMALLER of the two angles between
%    two rays.  Thus, if the rays make an (exterior) angle of 1.5 PI,
%    then the (interior) angle of 0.5 PI is reported.
%
%    X dot Y = Norm(X) * Norm(Y) * Cos( Angle(X,Y) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    real V1(DIM_NUM), V2(DIM_NUM), the two rays.
%
%  Output:
%
%    real VALUE, the angle between the rays,
%    in radians.  This value will always be between 0 and PI.
%
  v1 = v1(:);
  v2 = v2(:);

  dot = v1(1:dim_num)' * v2(1:dim_num);

  v1norm = sqrt ( sum ( v1(1:dim_num).^2 ) );

  if ( v1norm == 0.0 )
    value = 0.0;
    return
  end

  v2norm = sqrt ( sum ( v2(1:dim_num).^2 ) );

  if ( v2norm == 0.0 )
    value = 0.0;
    return
  end

  value = acos ( dot / ( v1norm * v2norm ) );

  return
end
