function fp = triangle_integrand_05 ( p_num, p, f_num )

%*****************************************************************************80
%
%% triangle_integrand_05() evaluates 5 integrand functions.
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
  fp = zeros ( 5, p_num );

  fp(1,1:p_num) = p(1,1:p_num).^4;
  fp(2,1:p_num) = p(1,1:p_num).^3 .* p(2,1:p_num);
  fp(3,1:p_num) = p(1,1:p_num).^2 .* p(2,1:p_num).^2;
  fp(4,1:p_num) = p(1,1:p_num)    .* p(2,1:p_num).^3;
  fp(5,1:p_num) =                    p(2,1:p_num).^4;

  return
end
