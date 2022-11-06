function fp = triangle_integrand_03 ( p_num, p, f_num )

%*****************************************************************************80
%
%% triangle_integrand_03() evaluates 3 integrand functions.
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
%    real P(2,P_NUM), the evaluation points.
%
%    integer F_NUM, the number of integrands.
%
%  Output:
%
%    real FP(F_NUM,P_NUM), the integrand values.
%
  fp = zeros ( 3, p_num );

  fp(1,1:p_num) = p(1,1:p_num) .* p(1,1:p_num);
  fp(2,1:p_num) = p(1,1:p_num) .* p(2,1:p_num);
  fp(3,1:p_num) = p(2,1:p_num) .* p(2,1:p_num);

  return
end
