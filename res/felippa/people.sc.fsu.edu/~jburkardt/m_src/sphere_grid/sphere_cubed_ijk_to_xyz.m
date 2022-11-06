function xyz = sphere_cubed_ijk_to_xyz ( n, i, j, k )

%*****************************************************************************80
%
%% sphere_cubed_ijk_to_xyz(): cubed sphere IJK to XYZ coordinates.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 October 2012
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of sections into which each face of
%    the cube is to be divided.
%
%    integer I, J, K, indices between 0 and N.  Normally,
%    at least one of the indices should have the value 0 or N.
%
%  Output:
%
%    real XYZ(3,1), coordinates of the point.
%
  if ( i == 0 )
    xc = -1.0;
  elseif ( i == n )
    xc = +1.0;
  else
    xc = tan ( ( 2 * i - n ) * 0.25 * pi / n );
  end

  if ( j == 0 )
    yc = -1.0;
  elseif ( j == n )
    yc = +1.0;
  else
    yc = tan ( ( 2 * j - n ) * 0.25 * pi / n );
  end

  if ( k == 0 )
    zc = -1.0;
  elseif ( k == n )
    zc = +1.0;
  else
    zc = tan ( ( 2 * k - n ) * 0.25 * pi / n );
  end

  xyzn = sqrt ( xc^2 + yc^2 + zc^2 );

  xyz = zeros ( 3, 1 );

  xyz(1) = xc / xyzn;
  xyz(2) = yc / xyzn;
  xyz(3) = zc / xyzn;

  return
end
