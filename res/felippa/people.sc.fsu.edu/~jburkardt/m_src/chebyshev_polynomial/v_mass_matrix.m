function a = v_mass_matrix ( n )

%*****************************************************************************80
%
%% v_mass_matrix() computes the mass matrix for the Chebyshev V polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N:
%
%  Output:
%
%    real A(N+1,N+1), the mass matrix.
%
  [ x, w ] = v_quadrature_rule ( n + 1 );

  phi = v_polynomial ( n + 1, n, x );

  for i = 1 : n + 1
    phiw(:,i) = w(i) * phi(i,:)';
  end

  a = phiw * phi;

  return
end
