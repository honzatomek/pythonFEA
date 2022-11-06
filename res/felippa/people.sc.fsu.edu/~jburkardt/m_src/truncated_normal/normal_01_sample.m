function x = normal_01_sample ( )

%*****************************************************************************80
%
%% normal_01_sample() samples the standard normal probability distribution.
%
%  Discussion:
%
%    The standard normal probability distribution function (PDF) has
%    mean 0 and standard deviation 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 October 2004
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real X, a sample of the standard normal PDF.
%
  x = randn ( 1, 1 );

  return
end
