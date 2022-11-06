function clenshaw_curtis_rule ( n, a, b, filename )

%*****************************************************************************80
%
%% clenshaw_curtis_rule() generates a Clenshaw Curtis rule.
%
%  Discussion:
%
%    This code computes a standard Clenshaw Curtis quadrature rule
%    and writes it to a file.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 February 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points in the rule;
%
%    real A, B, the endpoints;
%
%    character FILENAME, the root name of the output files.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'clenshaw_curtis_rule():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Compute a Clenshaw Curtis rule for approximating\n' );
  fprintf ( 1, '    Integral ( A <= x <= B ) f(x) dx\n' );
  fprintf ( 1, '  of order N.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The user specifies N, A, B, and FILENAME.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  N is the number of points:\n' );
  fprintf ( 1, '  A is the left endpoint;\n' );
  fprintf ( 1, '  B is the right endpoint;\n' );
  fprintf ( 1, '  FILENAME is used to generate 3 files:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '    filename_w.txt - the weight file\n' );
  fprintf ( 1, '    filename_x.txt - the abscissa file.\n' );
  fprintf ( 1, '    filename_r.txt - the region file.\n' );
%
%  Get N.
%
  if ( nargin < 1 )
    n = input ( '  Enter the rule order N:  ' );
  elseif ( ischar ( n ) )
    n = str2num ( n );
  end
%
%  Get A.
%
  if ( nargin < 2 )
    a = input ( '  Enter the left endpoint A:  ' );
  elseif ( ischar ( a ) )
    a = str2num ( a );
  end
%
%  Get B.
%
  if ( nargin < 3 )
    b = input ( '  Enter the right endpoint B:  ' );
  elseif ( ischar ( b ) )
    b = str2num ( b );
  end
%
%  Get FILENAME.
%
  if ( nargin < 4 )
    fprintf ( 1,  '\n' );
    fprintf ( 1,  '  FILENAME is the ''root name'' of the quadrature files).\n' );
    filename = input ( '  Enter FILENAME as a quoted string:  ' );
  end
%
%  Input summary.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  N = %d\n', n );
  fprintf ( 1, '  A = %f\n', a );
  fprintf ( 1, '  B = %f\n', b );
  fprintf ( 1, '  FILENAME = "%s".\n', filename );
%
%  Construct the rule and output it.
%
  [ x, w ] = clenshaw_curtis_ab ( n, a, b );
  r = [ a; b ];
%
%  Write the rule.
%
  rule_write ( n, filename, x, w, r );
%
%  Terminate.
%
  fprintf ( 1,  '\n' );
  fprintf ( 1,  'clenshaw_curtis_rule():\n' );
  fprintf ( 1,  '  Normal end of execution.\n' );

  return
end
function [ x, w ] = clenshaw_curtis_ab ( n, a, b )

%*****************************************************************************80
%
%% clenshaw_curtis_ab() computes a Clenshaw Curtis quadrature rule for [a,b].
%
%  Discussion:
%
%    Our convention is that the abscissas are numbered from left to right.
%
%    The rule is defined on [a,b].
%
%    The integral to approximate:
%
%      Integral ( a <= X <= b ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 October 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer n: the order of the rule.  1 <= N.
%
%    real a, b: the left and right endpoints.
%
%  Output:
%
%    real x(n): the abscissas.
%
%    real w(n): the weights.
%
  [ xi, rho ] = clenshaw_curtis_m1p1 ( n );

  x = 0.5 * ( ( xi + 1.0 ) * b - ( xi - 1.0 ) * a );
  w = 0.5 * ( b - a ) * rho;

  return
end
function [ x, w ] = clenshaw_curtis_m1p1 ( n )

%*****************************************************************************80
%
%% clenshaw_curtis_m1p1() computes a Clenshaw Curtis quadrature rule for [-1,+1].
%
%  Discussion:
%
%    Our convention is that the abscissas are numbered from left to right.
%
%    The rule is defined on [-1,1].
%
%    The integral to approximate:
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 February 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N: the order of the rule.  1 <= N.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'clenshaw_curtis_m1p1(): Fatal error!\n' );
    fprintf ( 1, '  Illegal value of N = %d\n', n );
    error ( 'clenshaw_curtis_m1p1(): Fatal error!' );
  end

  w = zeros ( n, 1 );
  x = zeros ( n, 1 );

  if ( n == 1 )
    x(1) = 0.0;
    w(1) = 2.0;
    return
  end

  for i = 1 : n
    x(i) = cos ( ( n - i ) * pi / ( n - 1 ) );
  end

  x(1) = -1.0;
  if ( mod ( n, 2 ) == 1 )
    x((n+1)/2) = 0.0;
  end
  x(n) = +1.0;

  w(1:n) = 1.0;

  for i = 1 : n

    theta = ( i - 1 ) * pi / ( n - 1 );

    for j = 1 : ( n - 1 ) / 2

      if ( 2 * j == ( n - 1 ) )
        b = 1.0;
      else
        b = 2.0;
      end

      w(i) = w(i) - b * cos ( 2.0 * j * theta ) / ( 4 * j * j - 1 );

    end

  end

  w(1)     =       w(1)     / ( n - 1 );
  w(2:n-1) = 2.0 * w(2:n-1) / ( n - 1 );
  w(n)     =       w(n)     / ( n - 1 );

  return
end
function r8mat_write ( output_filename, m, n, table )

%*****************************************************************************80
%
%% r8mat_write() writes an R8MAT file.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 August 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string OUTPUT_FILENAME, the output filename.
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
%    real TABLE(M,N), the points.
%

%
%  Open the file.
%
  output_unit = fopen ( output_filename, 'wt' );

  if ( output_unit < 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'r8mat_write(): Error!\n' );
    fprintf ( 1, '  Could not open the output file.\n' );
    error ( 'r8mat_write(): Error!' );
  end
%
%  Write the data.
%
%  For smaller data files, and less precision, try:
%
%     fprintf ( output_unit, '  %14.6f', table(i,j) );
%
  for j = 1 : n
    for i = 1 : m
      fprintf ( output_unit, '  %24.16f', table(i,j) );
    end
    fprintf ( output_unit, '\n' );
  end
%
%  Close the file.
%
  fclose ( output_unit );

  return
end
function rule_write ( order, filename, x, w, r )

%*****************************************************************************80
%
%% rule_write() writes a quadrature rule to a file.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 February 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer ORDER, the order of the rule.
%
%    string FILENAME, specifies the output files.
%    write files 'filename_w.txt', 'filename_x.txt', 'filename_r.txt' defining
%    weights, abscissas, and region.
%
%    real X(ORDER), the abscissas.
%
%    real W(ORDER), the weights.
%
%    real R(2), the region.
%
  filename_x = strcat ( filename, '_x.txt' );
  filename_w = strcat ( filename, '_w.txt' );
  filename_r = strcat ( filename, '_r.txt' );

  fprintf ( 1, '\n' );
  fprintf ( 1,'  Creating quadrature files.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  "Root" file name is   "%s".\n', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Weight file will be   "%s".\n', filename_w );
  fprintf ( 1, '  Abscissa file will be "%s".\n', filename_x );
  fprintf ( 1, '  Region file will be   "%s".\n', filename_r );

  r8mat_write ( filename_w, 1, order, w' );
  r8mat_write ( filename_x, 1, order, x' );
  r8mat_write ( filename_r, 1, 2,     r' );

  return
end

