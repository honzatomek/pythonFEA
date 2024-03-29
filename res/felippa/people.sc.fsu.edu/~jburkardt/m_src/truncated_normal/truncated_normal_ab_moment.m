function moment = truncated_normal_ab_moment ( order, mu, sigma, a, b )

%*****************************************************************************80
%
%% truncated_normal_ab_moment(): moments of the truncated Normal distribution.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Phoebus Dhrymes,
%    Moments of Truncated Normal Distributions,
%    May 2005.
%
%  Input:
%
%    integer ORDER, the order of the moment.
%    0 <= ORDER.
%
%    real MU, SIGMA, the mean and standard deviation of the
%    parent Normal distribution.
%    0 < S.
%
%    real A, B, the lower and upper truncation limits.
%    A < B.
%
%  Output:
%
%    real MOMENT, the moment of the PDF.
%
  if ( order < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
    fprintf ( 1, '  ORDER < 0.\n' );
    error ( 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
  end

  if ( sigma <= 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
    fprintf ( 1, '  SIGMA <= 0.0.\n' );
    error ( 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
  end

  if ( b <= a )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
    fprintf ( 1, '  B <= A.\n' );
    error ( 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
  end

  a_h = ( a - mu ) / sigma;
  a_pdf = normal_01_pdf ( a_h );
  a_cdf = normal_01_cdf ( a_h );

  if ( a_cdf == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
    fprintf ( 1, '  PDF/CDF ratio fails, because A_CDF is too small.\n' );
    fprintf ( 1, '  A_PDF = %g\n', a_pdf );
    fprintf ( 1, '  A_CDF = %g\n', a_cdf );
    error ( 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
  end

  b_h = ( b - mu ) / sigma;
  b_pdf = normal_01_pdf ( b_h );
  b_cdf = normal_01_cdf ( b_h );

  if ( b_cdf == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
    fprintf ( 1, '  PDF/CDF ratio fails, because B_CDF too small.\n' );
    fprintf ( 1, '  B_PDF = %g\n', b_pdf );
    fprintf ( 1, '  B_CDF = %g\n', b_cdf );
    error ( 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!\n' );
  end

  moment = 0.0;
  irm2 = 0.0;
  irm1 = 0.0;

  for r = 0 : order

    if ( r == 0 )
      ir = 1.0;
    elseif ( r == 1 )
      ir = - ( b_pdf - a_pdf ) / ( b_cdf - a_cdf );
    else
      ir = ( r - 1 ) * irm2 ...
        - ( b_h ^ ( r - 1 ) * b_pdf - a_h ^ ( r - 1 ) * a_pdf ) ...
        / ( b_cdf - a_cdf );
    end

    moment = moment + nchoosek ( order, r ) ...
      * mu ^ ( order - r ) ...
      * sigma ^ r * ir;

    irm2 = irm1;
    irm1 = ir;

  end

  return
end
