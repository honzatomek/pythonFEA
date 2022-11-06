function v = voxels_line_3d ( v1, v2, n )

%*****************************************************************************80
%
%% voxels_line_3d() computes voxels along a line in 3D.
%
%  Discussion:
%
%    The line itself is defined by two voxels.  The line will begin
%    at the first voxel, and move towards the second.  If the value of
%    N is equal to the L1 distance between the two voxels, then the
%    line will "almost" reach the second voxel.  Depending on the
%    direction, 1, 2 or 3 more steps may be needed.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Daniel Cohen,
%    Voxel Traversal along a 3D Line,
%    Graphics Gems IV,
%    edited by Paul Heckbert,
%    AP Professional, 1994.
%
%  Input:
%
%    integer V1(3), the voxel that begins the line.
%
%    integer V2(3), the voxel that ends the line.
%
%    integer N, the number of voxels to compute.
%
%  Output:
%
%    integer V(3,N), a sequence of voxels, whose
%    first value is V1 and which proceeds towards V2.
%
  if ( n <= 0 )
    return
  end
%
%  Determine the number of voxels on the line.
%
  for i = 1 : 3
    s(i) = r8_sign ( v2(i) - v1(i) );
  end
  a(1:3) = abs ( v2(1:3) - v1(1:3) );

  exy = a(2) - a(1);
  exz = a(3) - a(1);
  ezy = a(2) - a(3);
%
%  We start at the starting point.
%
  v(1:3,1) = v1(1:3)';

  for i = 2 : n

    v(1:3,i) = v(1:3,i-1);

    if ( exy < 0 )

      if ( exz < 0 )
        v(1,i) = v(1,i) + s(1);
        exy = exy + 2 * a(2);
        exz = exz + 2 * a(3);
      else
        v(3,i) = v(3,i) + s(3);
        exz = exz - 2 * a(1);
        ezy = ezy + 2 * a(2);
      end

    elseif ( ezy < 0 )
      v(3,i) = v(3,i) + s(3);
      exz = exz - 2 * a(1);
      ezy = ezy + 2 * a(2);
    else
      v(2,i) = v(2,i) + s(2);
      exy = exy - 2 * a(1);
      ezy = ezy - 2 * a(3);
    end

  end

  return
end
