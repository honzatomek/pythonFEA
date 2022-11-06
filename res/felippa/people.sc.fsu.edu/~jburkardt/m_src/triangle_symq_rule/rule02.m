function [ x, y, w ] = rule02 ( )

%*****************************************************************************80
%
%% rule02() returns the rule of degree 2.
%
%  Discussion:
%
%    Order 2 (3 pts)
%    1/6 data for 2-th order quadrature with 1 nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 June 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Output:
%
%    real X(*), Y(*), the coordinates of the nodes.
%
%    real W(*), the weights.
%
  x = [ ...
       0.00000000000000000000000000000000 ];
  y = [ ... ...
       0.57735026918962576450914878050196 ];
  w = [ ... ...
       0.21934566882541541013653648363283 ];

  return
end
