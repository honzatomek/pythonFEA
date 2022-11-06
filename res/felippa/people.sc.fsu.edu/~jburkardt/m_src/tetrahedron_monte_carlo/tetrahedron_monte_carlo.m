function result = tetrahedron_monte_carlo ( t, p_num, f_num, ...
  tetrahedron_integrand )

%*****************************************************************************80
%
%% tetrahedron_monte_carlo() applies the Monte Carlo rule to integrate a function.
%
%  Discussion:
%
%    The function f(x,y,z) is to be integrated over a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(3,4), the vertices.
%
%    integer P_NUM, the number of sample points.
%
%    integer F_NUM, the number of functions to integrate.
%
%    external TETRAHEDRON_INTEGRAND, the integrand routine.
%
%  Output:
%
%    real RESULT(F_NUM), the approximate integrals.
%
  volume = tetrahedron_volume ( t );

  p = tetrahedron01_sample ( p_num );

  p2 = reference_to_physical_tet4 ( t, p_num, p );

  fp = tetrahedron_integrand ( p_num, p2, f_num );

  result = zeros ( f_num, 1 );
  
  for i = 1 : f_num
    result(i) = volume * sum ( fp(i,1:p_num) ) / p_num;
  end

  return
end
