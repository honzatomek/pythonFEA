function a = vec_random ( n, base )

%*****************************************************************************80
%
%% vec_random() selects a random N-vector of integers modulo a given base.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 July 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the size of the vector to be generated.
%
%    integer BASE, the base to be used.
%
%  Output:
%
%    integer A(N), a list of N random values between 0 and BASE-1.
%
  a = randi ( [ 0, base-1 ], n );

  return
end
