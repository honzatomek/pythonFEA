function [ w, xy ] = triangle_unit_o12 ( )

%*****************************************************************************80
%
%% triangle_unit_o12() returns a 12 point quadrature rule for the unit triangle.
%
%  Discussion:
%
%    This rule is precise for monomials through degree 6.
%
%    The integration region is:
%
%      0 <= X
%      0 <= Y
%      X + Y <= 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Carlos Felippa,
%    A compendium of FEM integration formulas for symbolic work,
%    Engineering Computation,
%    Volume 21, Number 8, 2004, pages 867-890.
%
%  Output:
%
%    real W(12), the weights.
%
%    real XY(2,12), the abscissas.
%
  w(1:12,1) = [ ...
     0.050844906370206816921, ...
     0.050844906370206816921, ...
     0.050844906370206816921, ...
     0.11678627572637936603, ...
     0.11678627572637936603, ...
     0.11678627572637936603, ...
     0.082851075618373575194, ...
     0.082851075618373575194, ...
     0.082851075618373575194, ...
     0.082851075618373575194, ...
     0.082851075618373575194, ...
     0.082851075618373575194 ];

  xy(1:2,1:12) = [ ...
    0.87382197101699554332,  0.063089014491502228340; ...
    0.063089014491502228340,  0.87382197101699554332; ...
    0.063089014491502228340,  0.063089014491502228340; ...
    0.50142650965817915742,  0.24928674517091042129; ...
    0.24928674517091042129,  0.50142650965817915742; ...
    0.24928674517091042129,  0.24928674517091042129; ...
    0.053145049844816947353,  0.31035245103378440542; ...
    0.31035245103378440542,  0.053145049844816947353; ...
    0.053145049844816947353,  0.63650249912139864723; ...
    0.31035245103378440542,  0.63650249912139864723; ...
    0.63650249912139864723,  0.053145049844816947353; ...
    0.63650249912139864723,  0.31035245103378440542 ]';

  return
end
