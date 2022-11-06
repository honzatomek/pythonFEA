function w = direction_uniform_nd ( d )

%*****************************************************************************80
%
%% direction_uniform_nd() generates a random direction vector in D dimensions.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer D, the dimension of the space.
%
%  Output:
%
%    real W(D), a random direction vector, with unit norm.
%

%
%  Get N values from a standard normal distribution.
%
  w = randn ( 1, d );
%
%  Compute the length of the vector.
%
  w_norm = norm ( w );
%
%  Normalize the vector.
%
  w = w / w_norm;

  return
end
