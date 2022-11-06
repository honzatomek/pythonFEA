function result = triangle_monte_carlo ( t, p_num, f_num, ...
  triangle_unit_sample, triangle_integrand )

%*****************************************************************************80
%
%% triangle_monte_carlo() applies the Monte Carlo rule to integrate a function.
%
%  Discussion:
%
%    The function f(x,y) is to be integrated over a triangle T.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 February 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the triangle vertices.
%
%    integer P_NUM, the number of sample points.
%
%    integer F_NUM, the number of functions to integrate.
%
%    TRIANGLE_UNIT_SAMPLE, the sampling routine.
%
%    TRIANGLE_INTEGRAND, the integrand routine.
%
%  Output:
%
%    real RESULT(F_NUM), the approximate integrals.
%
  area = triangle_area ( t );

  p = triangle_unit_sample ( p_num );

  p2 = reference_to_physical_t3 ( t, p_num, p );

  fp = triangle_integrand ( p_num, p2, f_num );

  result = zeros ( f_num, 1 );
  
  for i = 1 : f_num
    result(i) = area * sum ( fp(i,1:p_num) ) / p_num;
  end

  return
end
