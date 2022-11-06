function ellipse_area3_test ( )

%*****************************************************************************80
%
%% ellipse_area3_test() tests ellipse_area3().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2016
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_area3_test():\n' );
  fprintf ( 1, '  ellipse_area3() computes the area of an ellipse.\n' );

  r1 = 10.0;
  r2 = 10.0 / 3.0;

  area = ellipse_area3 ( r1, r2 );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Ellipse: (x/%g)^2 + (y/%g)^2 = 1\n', r1, r2 );
  fprintf ( 1, '  Area = %g\n', area );

  return
end
