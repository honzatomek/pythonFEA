function h = robertson_conserved ( t, y )

%*****************************************************************************80
%
%% robertson_conserved evaluates a quantity that should be conserved.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 August 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Ernst Hairer, Gerhard Wanner,
%    Solving Ordinary Differential Equations II: 
%    Stiff and Differential-algebraic Problems,
%    Springer-Verlag, second revised edition, 1996.
%
%  Input:
%
%    real Y(:,3): the current solution.
%
%  Output:
%
%    real H(:): the conserved quantity.
%
  h = sum ( y' );

  return
end
