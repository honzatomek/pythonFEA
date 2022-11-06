function w = direction_uniform_nd ( dim_num )

%*****************************************************************************80
%
%% direction_uniform_nd() generates a random direction vector in ND.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 February 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the dimension of the space.
%
%  Output:
%
%    real W(DIM_NUM), a random direction vector, with unit norm.
%

%
%  Get N values from a standard normal distribution.
%
  w = randn ( dim_num, 1 );
%
%  Compute the length of the vector.
%
  w_norm = norm ( w );
%
%  Normalize the vector.
%
  w(1:dim_num) = w(1:dim_num) / w_norm;

  return
end
