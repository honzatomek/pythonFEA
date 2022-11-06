function x = uniform_in_tetrahedron ( n, v )

%*****************************************************************************80
%
%% uniform_in_tetrahedron() returns uniform points in a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Claudio Rocchini, Paolo Cignoni,
%    Generating Random Points in a Tetrahedron,
%    Journal of Graphics Tools,
%    Volume 5, Number 5, 2000, pages 9-12.
%
%  Input:
%
%    integer N, the number of points.
%
%    real V(4,3), the vertices of the tetrahedron.
%
%  Output:
%
%    real X(N,3), the points.
%
  x = zeros ( n, 3 );

  for i = 1 : n

    c = rand ( 1, 3 );

    if ( 1.0 < c(1,1) + c(1,2) )
      c(1,1) = 1.0 - c(1,1);
      c(1,2) = 1.0 - c(1,2);
    end

    if ( 1.0 < c(1,2) + c(1,3) )
      t = c(1,3);
      c(1,3) = 1.0 - c(1,1) - c(1,2);
      c(1,2) = 1.0 - t;
    elseif ( 1.0 < c(1,1) + c(1,2) + c(1,3) )
       t = c(1,3);
       c(1,3) = c(1,1) + c(1,2) + c(1,3) - 1.0;
       c(1,1) = 1.0 - c(1,2) - t;
    end

    c(1,4) = 1.0 - c(1,1) - c(1,2) - c(1,3);

    x(i,1:3) = c(1,1:4) * v(1:4,1:3);

  end

  return
end
