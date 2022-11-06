function fi = rbf_interp ( m, nd, xd, r0, phi, w, ni, xi )

%*****************************************************************************80
%
%% rbf_interp() evaluates a radial basis function interpolant.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 July 2012
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    William Press, Brian Flannery, Saul Teukolsky, William Vetterling,
%    Numerical Recipes in FORTRAN: The Art of Scientific Computing,
%    Third Edition,
%    Cambridge University Press, 2007,
%    ISBN13: 978-0-521-88068-8,
%    LC: QA297.N866.
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer ND, the number of data points.
%
%    real XD(M,ND), the data points.
%
%    real R0, a scale factor.  R0 should be larger than the typical
%    separation between points, but smaller than the maximum separation.
%    The value of R0 has a significant effect on the resulting interpolant.
%
%    function V = PHI ( R, R0 ), a function handle to evaluate the radial
%    basis functions.
%
%    real W(ND), the weights, as computed by RBF_WEIGHTS.
%
%    integer NI, the number of interpolation points.
%
%    real XI(M,NI), the interpolation points.
%
%  Output:
%
%    real FI(NI), the interpolated values.
% 
  fi = zeros ( ni, 1 );

  if ( true )

    for i = 1 : ni
      if ( m == 1 )
        d = xd - xi(i);
        r = abs ( d );
        v = phi ( r(1:nd), r0 );
        fi(i) = v' * w;
      else
        d = xd - repmat ( xi(:,i), 1, nd );
        r = sqrt ( sum ( d.^2 ) );
        v = phi ( r(1:nd), r0 );
        fi(i) = v * w;
      end
    end

  else
%
%  Alternative, unvectorized code.
%
    for i = 1 : ni
      for j = 1 : nd
        r = norm ( xi(:,i) - xd(:,j) );
        fi(i) = fi(i) + w(j) * phi ( r, r0 );
      end
    end

  end

  return
end
