function ball_and_stick_display ( )

%*****************************************************************************80
%
%% ball_and_stick_display() shows how to make a 3D ball and stick plot.
%
%  Discussion:
%
%    I wanted to illustrate the Lax-Wendroff stencil for approximating
%    solutions of a hyperbolic partial differential equation.
%
%    A "simple" 3D graph is all I needed, with three layers of data.
%
%    Layer 1 (time = 0) shows a central point, its square domain, and its
%    four neighbors.
%
%    Layer 2 (time = 1/2) shows the domain, with a node on the midpoint of
%    each side, where the fluxes are approximated.
%
%    Layer 3 (time = 1) shows the central point, where the new approximation
%    is made, and its region.
%
%    It's not hard to do this in MATLAB, all with the PLOT3 command, if you
%    know the right property names and values to apply.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2003
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ball_and_stick_display():\n' );
  fprintf ( 1, '  MATLAB version\n' );
  fprintf ( 1, '  How to make a 3D ball and stick plot.\n' );

  pause_seconds = 2.0;
%
%  Clear the plotting frame.
%
  figure ( 1 );
  clf
%
%  Make all the subsequent plotting "cumulative".
%
  hold on
%
%  Nodes on the blue level.
%
  plot3 (  0.0,  0.0, 0.0, 'o', 'MarkerFaceColor', 'b', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  pause ( pause_seconds );

  plot3 ( -1.0,  0.0, 0.0, 'o', 'MarkerFaceColor', 'b', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  1.0,  0.0, 0.0, 'o', 'MarkerFaceColor', 'b', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  0.0, -1.0, 0.0, 'o', 'MarkerFaceColor', 'b', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  0.0,  1.0, 0.0, 'o', 'MarkerFaceColor', 'b', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  pause ( pause_seconds );
%
%  Nodes on the green level.
%
  plot3 (  0.0,  0.0, 0.5, 'o', 'MarkerFaceColor', 'g', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )

  plot3 ( -0.5,  0.0, 0.5, 'o', 'MarkerFaceColor', 'g', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  0.5,  0.0, 0.5, 'o', 'MarkerFaceColor', 'g', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  0.0, -0.5, 0.5, 'o', 'MarkerFaceColor', 'g', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  plot3 (  0.0,  0.5, 0.5, 'o', 'MarkerFaceColor', 'g', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  pause ( pause_seconds );
%
%  Nodes on the red level.
%
  plot3 (  0.0,  0.0, 1.0, 'o', 'MarkerFaceColor', 'r', ...
    'MarkerEdgeColor', 'k', 'MarkerSize', 12 )
  pause ( pause_seconds );
%
%  Line from blue to red level.
%
  plot3 (  [  0.0,  0.0 ],  [  0.0,  0.0 ],  [ 0.0, 1.0 ], 'k', 'LineWidth', 3 )
  pause ( pause_seconds );
%
%  Lines on the blue level.
%
  plot3 (  [  1.0,  0.0 ],  [  0.0,  0.0 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [ -1.0,  0.0 ],  [  0.0,  0.0 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [  0.0,  0.0 ],  [  1.0,  0.0 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [  0.0,  0.0 ],  [ -1.0,  0.0 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )

  plot3 (  [ -0.5,  0.5 ],  [ -0.5, -0.5 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [  0.5,  0.5 ],  [ -0.5,  0.5 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [  0.5, -0.5 ],  [  0.5,  0.5 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  plot3 (  [ -0.5, -0.5 ],  [  0.5, -0.5 ],  [ 0.0, 0.0 ], 'b', 'LineWidth', 3 )
  pause ( pause_seconds );
%
%  Lines on the green level.
%
  plot3 (  [  0.5,  0.0 ],  [  0.0,  0.0 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [ -0.5,  0.0 ],  [  0.0,  0.0 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [  0.0,  0.0 ],  [  0.5,  0.0 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [  0.0,  0.0 ],  [ -0.5,  0.0 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )

  plot3 (  [ -0.5,  0.5 ],  [ -0.5, -0.5 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [  0.5,  0.5 ],  [ -0.5,  0.5 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [  0.5, -0.5 ],  [  0.5,  0.5 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  plot3 (  [ -0.5, -0.5 ],  [  0.5, -0.5 ],  [ 0.5, 0.5 ], 'g', 'LineWidth', 3 )
  pause ( pause_seconds );
%
%  Lines on the red level.
%
  plot3 (  [ -0.5,  0.5 ],  [ -0.5, -0.5 ],  [ 1.0, 1.0 ], 'r', 'LineWidth', 3 )
  plot3 (  [  0.5,  0.5 ],  [ -0.5,  0.5 ],  [ 1.0, 1.0 ], 'r', 'LineWidth', 3 )
  plot3 (  [  0.5, -0.5 ],  [  0.5,  0.5 ],  [ 1.0, 1.0 ], 'r', 'LineWidth', 3 )
  plot3 (  [ -0.5, -0.5 ],  [  0.5, -0.5 ],  [ 1.0, 1.0 ], 'r', 'LineWidth', 3 )
%
%  Labels.
%
  title ( 'Lax-Wendroff stencil' )
  xlabel ( '- X -' )
  ylabel ( '- Y -' )
  zlabel ( '- Time -' )
  pause ( pause_seconds );
%
%  "Release" the plotting screen.
%
  hold ( 'off' );

  pause ( pause_seconds );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ball_and_stick_display():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
