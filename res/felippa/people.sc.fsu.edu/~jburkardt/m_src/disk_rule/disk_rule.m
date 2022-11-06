function [ w, x, y ] = disk_rule ( nr, nt, xc, yc, rc )

%*****************************************************************************80
%
%% disk_rule() computes a quadrature rule for a general disk.
%
%  Discussion:
%
%    The general disk is the region:
%
%      ( x - xc ) ^ 2 + ( y - yc ) ^ 2 <= rc ^ 2.
%
%    The integral I(f) is then approximated by
%
%      S(f) = sum ( 1 <= i <= NT * NR ) W(i) * F ( X(i), Y(i) ).
%
%      Area = pi * RC ^ 2
%
%      Q(f) = Area * S(f)
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 April 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NR, the number of points in the radial rule.
%
%    integer NT, the number of angles to use.
%
%    real XC, YC, the coordinates of the disk center.
%
%    real RC, the radius of the disk.
%
%  Output:
%
%    real W(NR*NT), the weights for the rule.
%
%    real X(NR*NT), Y(NR*NT), the points for the rule.
%
  [ w01, r01, t01 ] = disk01_rule ( nr, nt );
%
%  Recompute the rule for the general circle in terms of X, Y.
%
  w = repmat ( w01, 1, nt );
  x = xc + rc * r01(1:nr) * cos ( t01(1:nt) )';
  y = yc + rc * r01(1:nr) * sin ( t01(1:nt) )';
%
%  Return column vectors.
%
  w = reshape ( w, nr * nt, 1 );
  x = reshape ( x, nr * nt, 1 );
  y = reshape ( y, nr * nt, 1 );

  return
end
