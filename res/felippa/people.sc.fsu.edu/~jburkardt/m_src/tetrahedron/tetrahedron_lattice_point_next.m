function [ v, more ] = tetrahedron_lattice_point_next ( c, v, more )

%*****************************************************************************80
%
%% tetrahedron_lattice_point_next() returns the next tetrahedron lattice point.
%
%  Discussion:
%
%    The lattice tetrahedron is defined by the vertices:
%
%      (0,0,0), (C(4)/C(1),0,0), (0,C(4)/C(2),0) and (0,0,C(4)/C(3))
%
%    The lattice tetrahedron is bounded by the lines
%
%      0 <= X,
%      0 <= Y
%      0 <= Z,
%      X / C(1) + Y / C(2) + Z / C(3) <= C(4)
%
%    Lattice points are listed one at a time, starting at the origin,
%    with X increasing first.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer C(4), coefficients defining the
%    lattice tetrahedron.  These should be positive.
%
%    integer V(3).  On first call, the input
%    value is not important.  On a repeated call, the input value should
%    be the output value from the previous call.  
%
%    logical MORE.  On set MORE to FALSE to indicate
%    that this is the first call for a given tetrahedron.  Thereafter, the input
%    value should be the output value from the previous call.  
%
%  Output:
%
%    integer V(3),  V contains
%    the next lattice point.
%
%    logical MORE, MORE is TRUE if not only is the returned value V a lattice point,
%    but the routine can be called again for another lattice point.
%    If the output value is FALSE, then no more lattice points were found,
%    and V was reset to 0, and the routine should not be called further
%    for this tetrahedron.
%
  n = 3;

  if ( ~ more )

    v(1:n) = 0;
    more = 1;

  else

    c1n = i4vec_lcm ( n, c );

    rhs = c1n * c(n+1);

    lhs =        c(2) * c(3) * v(1) ...
        + c(1) *        c(3) * v(2) ...
        + c(1) * c(2)        * v(3);

    if ( lhs + c1n / c(1) <= rhs )

      v(1) = v(1) + 1;

    else

      lhs = lhs - c1n * v(1) / c(1);
      v(1) = 0;

      if ( lhs + c1n / c(2) <= rhs )

        v(2) = v(2) + 1;

      else

        lhs = lhs - c1n * v(2) / c(2);
        v(2) = 0;

        if ( lhs + c1n / c(3) <= rhs )

          v(3) = v(3) + 1;

        else

          v(3) = 0;
          more = 0;

        end

      end
    end
  end

  return
end
