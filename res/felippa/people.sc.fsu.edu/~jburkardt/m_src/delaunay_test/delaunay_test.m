function delaunay_test ( )

%*****************************************************************************80
%
%% delaunay_test() tests delaunay()
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 December 2020
%
%  Author:
%
%    John Burkardt
%
  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'matlab_delaunay_delaunay_test():\n' );
  fprintf ( 1, '  Test the MATLAB built-in function delaunay(),\n' );
  fprintf ( 1, '  which computes the Delaunay triangulation of a set of points.\n' );

  n = 50;

  figure ( 1 );
  subplot ( 1, 2, 1 );
  x1 = rand ( n, 1 );
  y1 = 1.0 + rand ( n, 1 );
  t1 = delaunay ( x1, y1 );
  triplot ( t1, x1, y1, 'b', 'LineWidth', 3.0 );

  subplot ( 1, 2, 2 )
  x2 = 1.0 + rand ( n, 1 );
  y2 = 1.0 + rand ( n, 1 );
  t2 = delaunay ( x2, y2 );
  triplot ( t2, x2, y2, 'k', 'LineWidth', 3.0 );
  filename = 'delaunay_split.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  figure ( 2 )

  x = [ x1; x2 ];
  y = [ y1; y2 ];
  t = delaunay ( x, y );
  triplot ( t, x, y, 'LineWidth', 3.0 );
  filename = 'delaunay_full.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  figure ( 3 )

  t_num = size ( t, 1 );

  kr = [];
  kb = [];
  kg = [];

  for jt = 1 : t_num
    
    i1 = min ( t(jt,1:3) );
    i2 = max ( t(jt,1:3) );
    if ( 1 <= i1 & i1 <= 50 & 1 <= i2 & i2 <= 50 )
      kb = [ kb, jt ];
    end
    if ( 51 <= i1 & i1 <= 100 & 51 <= i2 & i2 <= 100 )
      kg = [ kg, jt ];
    end
    if ( 1 <= i1 & i1 <= 50 & 51 <= i2 & i2 <= 100 )
      kr = [ kr, jt ];
    end
  end 

  hold ( 'on' );
  triplot ( t, x, y, 'k' )
  triplot ( t(kb,1:3), x, y, 'b', 'LineWidth', 3.0 )
  triplot ( t(kr,1:3), x, y, 'r', 'LineWidth', 3.0 )
  triplot ( t(kg,1:3), x, y, 'k', 'LineWidth', 3.0 )

  filename = 'delaunay_highlight.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'delaunay_delaunay_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

