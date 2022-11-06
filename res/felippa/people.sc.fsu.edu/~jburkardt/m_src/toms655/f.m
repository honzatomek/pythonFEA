function value = f ( x, i )

%*****************************************************************************80
%
%% f() returns values of the integrand or its derivatives.
%
%  Discussion:
%
%    This function is an example of an integrand function.
%
%    The package can generate quadrature formulas that use derivative 
%    information as well as function values.  Therefore, this routine is
%    set up to provide derivatives of any order as well as the function
%    value.  In an actual application, the highest derivative needed
%    is of order one less than the highest knot multiplicity.
%
%    In other words, in the usual case where knots are not repeated,
%    this routine only needs to return function values, not any derivatives.
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
%  Input:
%
%    real X, the evaluation point.
%
%    integer I, the order of the derivative of F to
%    be evaluated.
%
%  Output:
%
%    real VALUE, the value of the I-th derivative of F at X.
%
  l = mod ( i, 4 );

  if ( l == 0 )
    value = sin ( x );
  elseif ( l == 1 )
    value = cos ( x );
  elseif ( l == 2 )
    value = - sin ( x );
  elseif ( l == 3 )
    value = - cos ( x );
  end

  return
end

