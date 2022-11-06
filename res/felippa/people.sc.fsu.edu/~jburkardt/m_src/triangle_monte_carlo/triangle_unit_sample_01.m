function p = triangle_unit_sample_01 ( p_num )

%*****************************************************************************80
%
%% triangle_unit_sample_01() selects points from the unit triangle.
%
%  Discussion:
%
%    The unit triangle has vertices (1,0), (0,1), (0,0).
%
%    Any point in the unit triangle CAN be chosen by this algorithm.
%
%    However, the points that are chosen tend to be clustered near
%    the centroid.
%
%    This routine is supplied as an example of "bad" sampling.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer P_NUM, the number of points.
%
%  Output:
%
%    real P(2,P_NUM), the points.
%
  for j = 1 : p_num

    e = rand ( 3, 1 );

    e_sum = sum ( e(1:3) );

    e(1:3) = e(1:3) / e_sum;
%
%  We may take the values E(1:3) as being the barycentric
%  coordinates of the point.
%
    p(1:2,j) = e(1:2);

  end

  return
end
