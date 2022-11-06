function a = u_mass_matrix ( n )

%*****************************************************************************80
%
%% u_mass_matrix() computes the mass matrix for the Chebyshev U polynomial.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 July 2015
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
  [ x, w ] = u_quadrature_rule ( n + 1 );

  phi = u_polynomial ( n + 1, n, x );

  for i = 1 : n + 1
    phiw(:,i) = w(i) * phi(i,:)';
  end

  a = phiw * phi;

  return
end
