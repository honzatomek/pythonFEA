function qfsum = cegqfs ( nt, kind, alpha, beta, f )

%*****************************************************************************80
%
%% cegqfs() estimates an integral using a standard quadrature formula.
%
%  Discussion:
%
%    The user chooses one of the standard quadrature rules
%    with the default values of A and B.  This routine determines
%    the corresponding weights and evaluates the quadrature formula
%    on a given function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 January 2010
%
%  Author:
%
%    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Sylvan Elhay, Jaroslav Kautsky,
%    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of
%    Interpolatory Quadrature,
%    ACM Transactions on Mathematical Software,
%    Volume 13, Number 4, December 1987, pages 399-415.
%
%  Input:
%
%    integer NT, the number of knots.
%
%    integer KIND, the rule.
%    1, Legendre,             (a,b)       1.0
%    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
%    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
%    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
%    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
%    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
%    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
%    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
%    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
%
%    real ALPHA, the value of Alpha, if needed.
%
%    real BETA, the value of Beta, if needed.
%
%    function F, the name of a routine which
%    evaluates the function and some of its derivatives.  The routine
%    must have the form
%      function value = f ( x, i )
%    and return in VALUE the value of the I-th derivative of the function
%    at X.  The value  I will always be 0.  The value X will always be a knot.
%
%  Output:
%
%    real QFSUM, the value of the quadrature formula
%    applied to F.
%
  lu = 0;

  [ t, wts ] = cgqfs ( nt, kind, alpha, beta, lu );
%
%  Evaluate the quadrature sum.
%
  qfsum = eiqfs ( nt, t, wts, f );

  return
end
