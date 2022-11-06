function zi = pwl_interp_2d ( nxd, nyd, xd, yd, zd, ni, xi, yi )

%*****************************************************************************80
%
%% pwl_interp_2d(): piecewise linear interpolant to data defined on a 2D grid.
%
%  Discussion:
%
%    Thanks to Adam Hirst for pointing out an error in the formula that
%    chooses the interpolation triangle, 04 February 2018.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 February 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NXD, NYD, the number of X and Y data values.
%
%    real XD(NXD), YD(NYD), the sorted X and Y data.
%
%    real ZD(NXD,NYD), the Z data.
%
%    integer NI, the number of interpolation points.
%
%    real XI(NI), YI(NI), the coordinates of the
%    interpolation points.
%
%  Output:
%
%    real ZI(NI), the value of the interpolant.
%
  zi = zeros ( ni, 1 );

  for k = 1 : ni
%
%  For interpolation point (xi(k),yi(k)), find data intervals I and J so that:
%
%    xd(i) <= xi(k) <= xd(i+1),
%    yd(j) <= yi(k) <= yd(j+1).
%
%  But if the interpolation point is not within a data interval, 
%  assign the dummy interpolant value zi(k) = infinity.
%
    i = r8vec_bracket5 ( nxd, xd, xi(k) );
    if ( i == -1 )
      zi(k) = r8_huge ( );
      continue
    end

    j = r8vec_bracket5 ( nyd, yd, yi(k) );
    if ( j == -1 )
      zi(k) = r8_huge ( );
      continue
    end
%
%  The rectangular cell is arbitrarily split into two triangles.
%  The linear interpolation formula depends on which triangle 
%  contains the data point.
%
%    (I,J+1)--(I+1,J+1)
%      |\       |
%      | \      |
%      |  \     |
%      |   \    |
%      |    \   |
%      |     \  |
%    (I,J)---(I+1,J)
%
    if ( yi(k) < yd(j+1) ...
      + ( yd(j) - yd(j+1) ) * ( xi(k) - xd(i) ) / ( xd(i+1) - xd(i) ) )

      dxa = xd(i+1) - xd(i);
      dya = yd(j)   - yd(j);

      dxb = xd(i)   - xd(i);
      dyb = yd(j+1) - yd(j);

      dxi = xi(k)   - xd(i);
      dyi = yi(k)   - yd(j);

      det = dxa * dyb - dya * dxb;

      alpha = ( dxi * dyb - dyi * dxb ) / det;
      beta =  ( dxa * dyi - dya * dxi ) / det;
      gamma = 1.0 - alpha - beta;

      zi(k) = alpha * zd(i+1,j) + beta * zd(i,j+1) + gamma * zd(i,j);

    else

      dxa = xd(i)   - xd(i+1);
      dya = yd(j+1) - yd(j+1);

      dxb = xd(i+1) - xd(i+1);
      dyb = yd(j)   - yd(j+1);

      dxi = xi(k)   - xd(i+1);
      dyi = yi(k)   - yd(j+1);

      det = dxa * dyb - dya * dxb;

      alpha = ( dxi * dyb - dyi * dxb ) / det;
      beta =  ( dxa * dyi - dya * dxi ) / det;
      gamma = 1.0 - alpha - beta;

      zi(k) = alpha * zd(i,j+1) + beta * zd(i+1,j) + gamma * zd(i+1,j+1);

    end

  end

  return
end
 
