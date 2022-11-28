function [ w, xyz ] = tetrahedron_unit_o14 ( )

%*****************************************************************************80
%
%% tetrahedron_unit_o14() returns a 14 point quadrature rule for the unit tetrahedron.
%
%  Discussion:
%
%    The integration region is:
%
%      0 <= X
%      0 <= Y
%      0 <= Z
%      X + Y + Z <= 1.
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
%    real W(14), the weights.
%
%    real XYZ(3,14), the abscissas.
%
  w(1:14,1) = [ ...
    0.073493043116361949544, ...
    0.073493043116361949544, ...
    0.073493043116361949544, ...
    0.073493043116361949544, ...
    0.11268792571801585080, ...
    0.11268792571801585080, ...
    0.11268792571801585080, ...
    0.11268792571801585080, ...
    0.042546020777081466438, ...
    0.042546020777081466438, ...
    0.042546020777081466438, ...
    0.042546020777081466438, ...
    0.042546020777081466438, ...
    0.042546020777081466438 ];

  xyz(1:3,1:14) = [ ...
    0.72179424906732632079,  0.092735250310891226402,  0.092735250310891226402; ...
    0.092735250310891226402,  0.72179424906732632079,  0.092735250310891226402; ...
    0.092735250310891226402,  0.092735250310891226402,  0.72179424906732632079; ...
    0.092735250310891226402,  0.092735250310891226402,  0.092735250310891226402; ...
    0.067342242210098170608,  0.31088591926330060980,  0.31088591926330060980; ...
    0.31088591926330060980,  0.067342242210098170608,  0.31088591926330060980; ...
    0.31088591926330060980,  0.31088591926330060980,  0.067342242210098170608; ...
    0.31088591926330060980,  0.31088591926330060980,  0.31088591926330060980; ...
    0.045503704125649649492,  0.045503704125649649492,  0.45449629587435035051; ...
    0.045503704125649649492,  0.45449629587435035051,  0.045503704125649649492; ...
    0.045503704125649649492,  0.45449629587435035051,  0.45449629587435035051; ...
    0.45449629587435035051,  0.045503704125649649492,  0.045503704125649649492; ...
    0.45449629587435035051,  0.045503704125649649492,  0.45449629587435035051; ...
    0.45449629587435035051,  0.45449629587435035051,  0.045503704125649649492 ]';

  return
end