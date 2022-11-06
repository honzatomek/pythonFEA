function x = reference_tet4_uniform2 ( n )

%*****************************************************************************80
%
%% reference_tet4_uniform2(): uniform sample points in the reference tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 August 2009
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
%  Output:
%
%    real X(3,N), the points.
%
  for j = 1 : n

    c = rand ( 3, 1 );

    if ( 1.0 < c(1) + c(2) )
      c(1) = 1.0 - c(1);
      c(2) = 1.0 - c(2);
    end

    if ( 1.0 < c(2) + c(3) )
      t = c(3);
      c(3) = 1.0 - c(1) - c(2);
      c(2) = 1.0 - t;
    elseif ( 1.0 < c(1) + c(2) + c(3) )
       t = c(3);
       c(3) = c(1) + c(2) + c(3) - 1.0;
       c(1) = 1.0 - c(2) - t;
    end

    x(1:3,j) = c(1:3);

  end

  return
end
