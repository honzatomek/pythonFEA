function fp = tetrahedron_integrand_01 ( p_num, p, f_num )

%*****************************************************************************80
%
%% tetrahedron_integrand_01() evaluates 1 integrand function.
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
  fp(1,1:p_num) = 1.0;

  return
end
