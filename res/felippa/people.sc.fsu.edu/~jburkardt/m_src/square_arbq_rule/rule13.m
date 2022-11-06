function [ x, w ] = rule13 ( n )

%*****************************************************************************80
%
%% rule13() returns the rule of degree 13.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 July 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
%  Input:
%
%    integer N, the number of nodes.
%
%  Output:
%
%    real X(2,N), the coordinates of the nodes.
%
%    real W(N), the weights.
%
  xs = [ ...
    -.9572976997863074,0.8595560056416388, ...
    0.9572976997863074,-.8595560056416386, ...
    -.7788097115544195,0.9834866824398722, ...
    0.7788097115544197,-.9834866824398722, ...
    -.4758086252182752,0.8500766736997490, ...
    0.4758086252182753,-.8500766736997490, ...
    0.3907362161294613,0.9413272258729251, ...
    -.3907362161294612,-.9413272258729251, ...
    -.1381834598624646,0.9589251702875351, ...
    0.1381834598624647,-.9589251702875351, ...
    0.6478216371870111,0.7558053565720809, ...
    -.6478216371870111,-.7558053565720809, ...
    0.7074150899644462E-01,0.6962500784917495, ...
    -.7074150899644453E-01,-.6962500784917495, ...
    -.3427165560404070,0.4093045616940387, ...
    0.3427165560404070,-.4093045616940387, ...
    -.7375869198366919D-30 ];
  ys = [ ...
    -.8595560056416389,-.9572976997863074, ...
    0.8595560056416387,0.9572976997863074, ...
    -.9834866824398722,-.7788097115544196, ...
    0.9834866824398722,0.7788097115544198, ...
    -.8500766736997490,-.4758086252182752, ...
    0.8500766736997490,0.4758086252182753, ...
    -.9413272258729251,0.3907362161294612, ...
    0.9413272258729251,-.3907362161294611, ...
    -.9589251702875351,-.1381834598624647, ...
    0.9589251702875351,0.1381834598624648, ...
    -.7558053565720809,0.6478216371870111, ...
    0.7558053565720809,-.6478216371870111, ...
    -.6962500784917495,0.7074150899644457E-01, ...
    0.6962500784917495,-.7074150899644449E-01, ...
    -.4093045616940387,-.3427165560404070, ...
    0.4093045616940387,0.3427165560404070, ...
    -.6522588594679827D-30 ];
  ws = [ ...
    0.2699339218118215E-01,0.2699339218118215E-01, ...
    0.2699339218118215E-01,0.2699339218118215E-01, ...
    0.2120743264134157E-01,0.2120743264134157E-01, ...
    0.2120743264134157E-01,0.2120743264134157E-01, ...
    0.8403587015611026E-01,0.8403587015611026E-01, ...
    0.8403587015611026E-01,0.8403587015611026E-01, ...
    0.5479564090947502E-01,0.5479564090947502E-01, ...
    0.5479564090947502E-01,0.5479564090947502E-01, ...
    0.4272687338421139E-01,0.4272687338421139E-01, ...
    0.4272687338421139E-01,0.4272687338421139E-01, ...
    0.9175668641747110E-01,0.9175668641747110E-01, ...
    0.9175668641747110E-01,0.9175668641747110E-01, ...
    0.1508552789574409,0.1508552789574409, ...
    0.1508552789574409,0.1508552789574409, ...
    0.1816350488471704,0.1816350488471704, ...
    0.1816350488471704,0.1816350488471704, ...
    0.2124022307685795 ];

  x(1,1:n) = xs(1:n);
  x(2,1:n) = ys(1:n);
  w(1:n) = ws(1:n);

  return
end
