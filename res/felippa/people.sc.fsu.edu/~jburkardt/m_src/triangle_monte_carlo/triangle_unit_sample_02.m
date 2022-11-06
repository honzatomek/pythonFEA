function p = triangle_unit_sample_02 ( p_num )

%*****************************************************************************80
%
%% triangle_unit_sample_02() selects points from the unit triangle.
%
%  Discussion:
%
%    The unit triangle has vertices (1,0), (0,1), (0,0).
%
%    The sampling is uniform.
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

%
%  Generate the points using barycentric coordinates.
%
  for j = 1 : p_num

    r = rand ( 2, 1 );

    if ( 1.0 < sum ( r(1:2) ) )
      r(1:2) = 1.0 - r(1:2);
    end

    p(1:2,j) = r(1:2);

  end

  return
end
