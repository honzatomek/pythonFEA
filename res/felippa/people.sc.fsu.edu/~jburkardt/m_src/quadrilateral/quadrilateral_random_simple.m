function q = quadrilateral_random_simple ( )

%*****************************************************************************80
%
%% quadrilateral_random_simple() returns a random simple quadrilateral.
%
%  Description:
%
%    The quadrilateral is constrained in that the vertices must all lie
%    with the unit square.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 August 2018
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real Q(2,4), the coordinates of the nodes of the quadrilateral.
%
  while ( true )
%
%  Generate 4 random points.
%
    q = quadrilateral_random ( );
%
%  Break if the quadrilateral is simple.
%
    if ( quadrilateral_is_simple ( q ) )
      break
    end

  end

  return
end
