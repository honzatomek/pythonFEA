function [ ns, xyz ] = sphere_cubed_points_face ( n, i1, j1, k1, i2, ...
  j2, k2, ns, xyz )

%*****************************************************************************80
%
%% sphere_cubed_points_face(): points on one face of a cubed sphere grid.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 September 2012
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
%    integer I1, J1, K1, I2, J2, K2, the logical indices, between 0 
%    and N, of two corners of the face grid.  It is guaranteed that I1 <= I2,
%    J1 <= J2, and K1 <= K2.  
%
%    integer NS, the number of points.
%
%    real XYZ(3,NS), distinct points on the unit sphere
%    generated by a cubed sphere grid.
%
%  Output:
%
%    integer NS, the number of points.
%
%    real XYZ(3,NS), distinct points on the unit sphere
%    generated by a cubed sphere grid.
%
  for i = i1 : i2

    if ( i1 < i2 )
      xc = tan ( ( 2 * i - n ) * 0.25 * pi / n );
    elseif ( i1 == 0 )
      xc = -1.0;
    elseif ( i1 == n )
      xc = +1.0;
    else
      xc = 0.0;
    end

    for j = j1 : j2

      if ( j1 < j2 )
        yc = tan ( ( 2 * j - n ) * 0.25 * pi / n );
      elseif ( j1 == 0 )
        yc = -1.0;
      elseif ( j1 == n )
        yc = +1.0;
      else
        yc = 0.0;
      end

      for k = k1 : k2

        if ( k1 < k2 )
          zc = tan ( ( 2 * k - n ) * 0.25 * pi / n );
        elseif ( k1 == 0 )
          zc = -1.0;
        elseif ( k1 == n )
          zc = +1.0;
        else
          zc = 0.0;
        end

        xyzn = sqrt ( xc^2 + yc^2 + zc^2 );

        ns = ns + 1;
        xyz(1,ns) = xc / xyzn;
        xyz(2,ns) = yc / xyzn;
        xyz(3,ns) = zc / xyzn;

      end
    end
  end

  return
end
