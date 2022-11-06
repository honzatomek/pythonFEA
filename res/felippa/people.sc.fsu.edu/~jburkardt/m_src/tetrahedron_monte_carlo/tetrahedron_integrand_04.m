function fp = tetrahedron_integrand_04 ( p_num, p, f_num )

%*****************************************************************************80
%
%% tetrahedron_integrand_04() evaluates 10 integrand functions.
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
%    integer P_NUM, the number of points.
%
%    real P(3,P_NUM), the evaluation points.
%
%    integer F_NUM, the number of integrands.
%
%  Output:
%
%    real FP(F_NUM,P_NUM), the integrand values.
%
  fp = zeros ( f_num, p_num );

  fp( 1,1:p_num) = p(1,1:p_num).^3;
  fp( 2,1:p_num) = p(1,1:p_num).^2 .* p(2,1:p_num);
  fp( 3,1:p_num) = p(1,1:p_num).^2                    .* p(3,1:p_num);
  fp( 4,1:p_num) = p(1,1:p_num)    .* p(2,1:p_num).^2;
  fp( 5,1:p_num) = p(1,1:p_num)    .* p(2,1:p_num)    .* p(3,1:p_num);
  fp( 6,1:p_num) = p(1,1:p_num)                       .* p(3,1:p_num).^2;
  fp( 7,1:p_num) =                    p(2,1:p_num).^3;
  fp( 8,1:p_num) =                    p(2,1:p_num).^2 .* p(3,1:p_num);
  fp( 9,1:p_num) =                    p(2,1:p_num)    .* p(3,1:p_num).^2;
  fp(10,1:p_num) =                                       p(3,1:p_num).^3;

  return
end
