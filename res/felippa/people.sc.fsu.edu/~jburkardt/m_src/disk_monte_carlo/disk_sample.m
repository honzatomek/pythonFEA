function x = disk_sample ( center, r, n )

%*****************************************************************************80
%
%% disk_sample() uniformly samples a disk.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    02 February 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the center of the disk.
%
%    real R, the radius of the disk.
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(2,N), the points.
%
  x = randn ( 2, n );
  x_norm = ones ( 1, 2 ) * ( x.^2 );
  x_norm = sqrt ( x_norm );
  for i = 1 : 2
    x(i,1:n) = x(i,1:n) ./ x_norm(1:n);
  end

  center = center(:);
  center_vec = repmat ( center, 1, n );

  r2 = rand ( 1, n );
  r2_vec = repmat ( r2, 2, 1 );

  x = center_vec + r * sqrt ( r2_vec ) .* x;

  return
end
