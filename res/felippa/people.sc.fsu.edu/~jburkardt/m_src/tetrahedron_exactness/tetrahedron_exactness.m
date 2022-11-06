function tetrahedron_exactness ( quad_filename, degree_max )

%*****************************************************************************80
%
%% tetrahedron_exactness() investigates the exactness of a tetrahedron quadrature.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 February 2010
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    character QUAD_FILENAME, the common filename prefix for the quadrature rule.
%
%    integer DEGREE_MAX, the maximum degree to check.
%
  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'TETRAHEDRON_EXACTNESS\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Investigate the polynomial exactness of a quadrature\n' );
  fprintf ( 1, '  rule for the tetrahedron by integrating all monomials\n' );
  fprintf ( 1, '  of a given degree.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The rule will be adjusted to the unit tetrahedron.\n' );
%
%  Get the quadrature file root name:
%
  if ( 1 <= nargin )

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS:\n' );

    quad_filename = input ( '  Enter the "root" name of the quadrature files.' );

  end
%
%  Create the names of:
%    the quadrature X file;
%    the quadrature W file;
%    the quadrature R file;
%
  quad_x_filename = strcat ( quad_filename, '_x.txt' );
  quad_w_filename = strcat ( quad_filename, '_w.txt' );
  quad_r_filename = strcat ( quad_filename, '_r.txt' );
%
%  The second command line argument is the maximum degree.
%
  if ( 2 <= nargin )

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS:\n' );

    degree_max = input ( '  Please enter the maximum total degree to check.' );

  end
%
%  Summarize the input.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'TETRAHEDRON_EXACTNESS: User input:\n' );
  fprintf ( 1, '  Quadrature rule X file = "%s".\n', quad_x_filename );
  fprintf ( 1, '  Quadrature rule W file = "%s".\n', quad_w_filename );
  fprintf ( 1, '  Quadrature rule R file = "%s".\n', quad_r_filename );
  fprintf ( 1, '  Maximum total degree to check = %d', degree_max );
%
%  Read the X file.
%
  [ dim_num, point_num ] = r8mat_header_read ( quad_x_filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Spatial dimension = %d\n', dim_num );
  fprintf ( 1, '  Number of points  = %d\n', point_num );

  if ( dim_num ~= 3 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS - Fatal error!\n' );
    fprintf ( 1, '  The quadrature abscissas must be 3 dimensional.\n' );
    error ( 'TETRAHEDRON_EXACTNESS - Fatal error!' );
  end

  x = r8mat_data_read ( quad_x_filename, dim_num, point_num );
%
%  Read the W file.
%
  [ dim_num2, point_num2 ] = r8mat_header_read ( quad_w_filename );

  if ( dim_num2 ~= 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS - Fatal error!\n' );
    fprintf ( 1, '  The quadrature weight file should have exactly\n');
    fprintf ( 1, '  one value on each line.\n' );
    error ( 'TETRAHEDRON_EXACTNESS - Fatal error!' );
  end

  if ( point_num2 ~= point_num )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS - Fatal error!\n' );
    fprintf ( 1, '  The quadrature weight file should have exactly\n' );
    fprintf ( 1, '  the same number of lines as the abscissa file.\n' );
    error ( 'NINT_EXACTNESS - Fatal error!' );
  end

  w = r8mat_data_read ( quad_w_filename, 1, point_num2 );
%
%  Read the R file.
%
  [ dim_num3, point_num3 ] = r8mat_header_read ( quad_r_filename );

  if ( dim_num3 ~= dim_num )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS - Fatal error!\n' );
    fprintf ( 1, '  The quadrature region file should have the same\n' );
    fprintf ( 1, '  number of values on each line as the abscissa file\n' );
    fprintf ( 1, '  does.\n' );
    error ( 'TETRAHEDRON_EXACTNESS - Fatal error!' );
  end

  if ( point_num3 ~= 4 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'TETRAHEDRON_EXACTNESS - Fatal error!\n' );
    fprintf ( 1, '  The quadrature region file should have 4 lines.\n' );
    error ( 'TETRAHEDRON_EXACTNESS - Fatal error!' );
  end

  r = r8mat_data_read ( quad_r_filename, dim_num3, 4 );
%
%  Rescale the weights.
%
  volume = tetrahedron_volume ( r );
  w(1:point_num) = ( 1.0 / 6.0 ) * w(1:point_num) / volume;
%
%  Translate the abscissas.
%
  x_ref = tetrahedron_order4_physical_to_reference ( r, point_num, x );
%
%  Explore the monomials.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '      Error    Degree  Exponents\n' );
  fprintf ( 1, '\n' );

  for degree = 0 : degree_max

    expon = [];
    more = 0;
    h = 0;
    t = 0;

    while ( true )

      [ expon, more, h, t ] = comp_next ( degree, dim_num, expon, more, h, t );

      quad_error = tet01_monomial_quadrature ( dim_num, expon, point_num, ...
        x_ref, w );

      fprintf ( 1, '  %24.16f   %2d  ', quad_error, degree );
      for dim = 1 : dim_num
        fprintf ( 1, '%3d', expon(dim) );
      end
      fprintf ( 1, '\n' );

      if ( ~ more )
        break
      end

    end

    fprintf ( 1, '\n' );

  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'TETRAHEDRON_EXACTNESS:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  return
end
function [ a, more, h, t ] = comp_next ( n, k, a, more, h, t )

%*****************************************************************************80
%
%% comp_next() computes the compositions of the integer N into K parts.
%
%  Discussion:
%
%    A composition of the integer N into K parts is an ordered sequence
%    of K nonnegative integers which sum to N.  The compositions (1,2,1)
%    and (1,1,2) are considered to be distinct.
%
%    The routine computes one composition on each call until there are no more.
%    For instance, one composition of 6 into 3 parts is
%    3+2+1, another would be 6+0+0.
%
%    On the first call to this routine, set MORE = FALSE.  The routine
%    will compute the first element in the sequence of compositions, and
%    return it, as well as setting MORE = TRUE.  If more compositions
%    are desired, call again, and again.  Each time, the routine will
%    return with a new composition.
%
%    However, when the LAST composition in the sequence is computed 
%    and returned, the routine will reset MORE to FALSE, signaling that
%    the end of the sequence has been reached.
%
%    This routine originally used a SAVE statement to maintain the
%    variables H and T.  I have decided that it is safer
%    to pass these variables as arguments, even though the user should
%    never alter them.  This allows this routine to safely shuffle
%    between several ongoing calculations.
%
%    There are 28 compositions of 6 into three parts.  This routine will
%    produce those compositions in the following order:
%
%     I         A
%     -     ---------
%     1     6   0   0
%     2     5   1   0
%     3     4   2   0
%     4     3   3   0
%     5     2   4   0
%     6     1   5   0
%     7     0   6   0
%     8     5   0   1
%     9     4   1   1
%    10     3   2   1
%    11     2   3   1
%    12     1   4   1
%    13     0   5   1
%    14     4   0   2
%    15     3   1   2
%    16     2   2   2
%    17     1   3   2
%    18     0   4   2
%    19     3   0   3
%    20     2   1   3
%    21     1   2   3
%    22     0   3   3
%    23     2   0   4
%    24     1   1   4
%    25     0   2   4
%    26     1   0   5
%    27     0   1   5
%    28     0   0   6
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    02 July 2008
%
%  Author:
%
%    Original FORTRAN77 version by Albert Nijenhuis and Herbert Wilf.
%    MATLAB version by John Burkardt.
%
%  Reference:
%
%    Albert Nijenhuis, Herbert Wilf,
%    Combinatorial Algorithms for Computers and Calculators,
%    Second Edition,
%    Academic Press, 1978,
%    ISBN: 0-12-519260-6,
%    LC: QA164.N54.
%
%  Input:
%
%    integer N, the integer whose compositions are desired.
%
%    integer K, the number of parts in the composition.
%
%    integer A(K), the previous composition.  On the first call,
%    with MORE = FALSE, set A = [].  Thereafter, A should be the 
%    value of A output from the previous call.
%
%    logical MORE.  The input value of MORE on the first
%    call should be FALSE, which tells the program to initialize.
%    On subsequent calls, MORE should be TRUE, or simply the
%    output value of MORE from the previous call.
%
%    integer H, T, two internal parameters needed for the
%    computation.  The user may need to initialize these before the
%    very first call, but these initial values are not important.
%    The user should not alter these parameters once the computation
%    begins.
%
%  Output:
%
%    integer A(K), the next composition.
%
%    logical MORE, will be TRUE unless the composition 
%    that is being returned is the final one in the sequence.
%
%    integer H, T, the updated values of the two internal 
%    parameters.
%
  if ( ~ more )

    t = n;
    h = 0;
    a(1) = n;
    a(2:k) = 0;

  else
      
    if ( 1 < t )
      h = 0;
    end

    h = h + 1;
    t = a(h);
    a(h) = 0;
    a(1) = t - 1;
    a(h+1) = a(h+1) + 1;

  end

  more = ( a(k) ~= n );

  return
end
function column_num = file_column_count ( input_file_name )

%*****************************************************************************80
%
%% file_column_count() counts the columns in the first line of a file.
%
%  Discussion:
%
%    The file is assumed to be a simple text file.
%
%    Most lines of the file are presumed to consist of COLUMN_NUM words,
%    separated by spaces.  There may also be some blank lines, and some 
%    comment lines, which have a "#" in column 1.
%
%    The routine tries to find the first non-comment non-blank line and
%    counts the number of words in that line.
%
%    If all lines are blanks or comments, it goes back and tries to analyze
%    a comment line.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    21 February 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string INPUT_FILE_NAME, the name of the file.
%
%  Output:
%
%    integer COLUMN_NUM, the number of columns in the file.
%
  FALSE = 0;
  TRUE = 1;
%
%  Open the file.
%
  input_unit = fopen ( input_file_name );

  if ( input_unit < 0 ) 
    fprintf ( 1, '\n' );
    fprintf ( 1, 'FILE_COLUMN_COUNT - Error!\n' );
    fprintf ( 1, '  Could not open the file "%s".\n', input_file_name );
    error ( 'FILE_COLUMN_COUNT - Error!' );
  end
%
%  Read one line, but skip blank lines and comment lines.
%  Use FGETL so we drop the newline character!
%
  got_one = FALSE;

  while ( true )

    line = fgetl ( input_unit );

    if ( line == -1 )
      break;
    end

    if ( s_len_trim ( line ) == 0 )

    elseif ( line(1) == '#' )

    else
      got_one = TRUE;
      break;
    end

  end

  fclose ( input_unit );

  if ( got_one == FALSE ) 
    fprintf ( 1, '\n' );
    fprintf ( 1, 'FILE_COLUMN_COUNT - Warning!\n' );
    fprintf ( 1, '  The file does not seem to contain any data.\n' );
    column_num = -1;
    return;
  end

  column_num = s_word_count ( line );

  return
end
function row_num = file_row_count ( input_file_name )

%*****************************************************************************80
%
%% file_row_count() counts the number of row records in a file.
%
%  Discussion:
%
%    Each input line is a "RECORD".
%
%    The records are divided into three groups:
%    
%    * BLANK LINES (nothing but blanks)
%    * COMMENT LINES (begin with a '#')
%    * DATA RECORDS (anything else)
%
%    The value returned by the function is the number of data records.
%
%    By the way, if the MATLAB routine FGETS is used, instead of
%    FGETL, then the variable LINE will include line termination 
%    characters, which means that a blank line would not actually
%    have zero characters.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    31 December 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string INPUT_FILE_NAME, the name of the input file.
%
%  Output:
%
%    integer ROW_NUM, the number of rows found. 
%
  input_unit = fopen ( input_file_name );

  if ( input_unit < 0 ) 
    fprintf ( 1, '\n' );
    fprintf ( 1, 'FILE_ROW_COUNT - Error!\n' );
    fprintf ( 1, '  Could not open the file "%s".\n', input_file_name );
    error ( 'FILE_ROW_COUNT - Error!' );
  end

  blank_num = 0;
  comment_num = 0;
  row_num = 0;
  
  record_num = 0;

  while ( true )

    line = fgetl ( input_unit );

    if ( line == -1 )
      break;
    end

    record_num = record_num + 1;
    record_length = s_len_trim ( line );
    
    if ( record_length <= 0 )
      blank_num = blank_num + 1;
    elseif ( line(1) == '#' )
      comment_num = comment_num + 1;
    else
      row_num = row_num + 1;
    end

  end

  fclose ( input_unit );

  return
end
function value = monomial_value ( dim_num, point_num, x, expon )

%*****************************************************************************80
%
%% monomial_value() evaluates a monomial.
%
%  Discussion:
%
%    This routine evaluates a monomial of the form
%
%      product ( 1 <= dim <= dim_num ) x(dim)^expon(dim)
%
%    where the exponents are nonnegative integers.  Note that
%    if the combination 0^0 is encountered, it should be treated
%    as 1.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 May 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    integer POINT_NUM, the number of points at which the
%    monomial is to be evaluated.
%
%    real X(DIM_NUM,POINT_NUM), the point coordinates.
%
%    integer EXPON(DIM_NUM), the exponents.
%
%  Output:
%
%    real VALUE(POINT_NUM), the value of the monomial.
%
  value(1:point_num) = 1.0;

  for dim = 1 : dim_num
    if ( 0 ~= expon(dim) )
      value(1:point_num) = value(1:point_num) .* x(dim,1:point_num).^expon(dim);
    end
  end

  return
end
function det = r8mat_det_4d ( a )

%*****************************************************************************80
%
%% r8mat_det_4d() computes the determinant of a 4 by 4 matrix.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 January 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A(4,4), the matrix whose determinant is desired.
%
%  Output:
%
%    real DET, the determinant of the matrix.
%
  det = ...
      a(1,1) * ( ...
        a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) ...
      - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) ...
      + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) ) ...
    - a(1,2) * ( ...
        a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) ...
      - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) ...
      + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) ) ...
    + a(1,3) * ( ...
        a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) ...
      - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) ...
      + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) ) ...
    - a(1,4) * ( ...
        a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) ...
      - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) ...
      + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) );

  return
end
function table = r8mat_data_read ( input_filename, m, n )

%*****************************************************************************80
%
%% r8mat_data_read() reads data from an R8MAT file.
%
%  Discussion:
%
%    An R8MAT is an array of R8's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 January 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string INPUT_FILENAME, the name of the input file.
%
%    integer M, N, the number of rows and columns of data.
%
%  Output:
%
%    real TABLE(M,N), the point coordinates.
%
  table = zeros ( m, n );
%
%  Build up the format string for reading M real numbers.
%
  string = ' ';

  for i = 0 : m
    string = strcat ( string, ' %f' );
  end

  input_unit = fopen ( input_filename );

  if ( input_unit < 0 ) 
    fprintf ( 1, '\n' );
    fprintf ( 1, 'R8MAT_DATA_READ - Error!\n' );
    fprintf ( 1, '  Could not open the file.\n' );
    error ( 'R8MAT_DATA_READ - Error!' );
  end

  i = 0;

  while ( i < n )

    line = fgets ( input_unit );

    if ( line == -1 )
      break;
    end

    if ( line(1) == '#' )

    elseif ( s_len_trim ( line ) == 0 )
      
    else

      [ x, count ] = sscanf ( line, string );

      if ( count == m )
        i = i + 1;
        table(1:m,i) = x(1:m);
      end

    end

  end

  fclose ( input_unit );

  return
end
function [ m, n ] = r8mat_header_read ( input_filename )

%*****************************************************************************80
%
%% r8mat_header_read() reads the header from an R8MAT file.
%
%  Discussion:
%
%    An R8MAT is an array of R8's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 October 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string INPUT_FILENAME, the name of the input file.
%
%  Output:
%
%    integer M, the spatial dimension.
%
%    integer N, the number of points.
%
  m = file_column_count ( input_filename );

  if ( m <= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'R8MAT_HEADER_READ - Fatal error!\n' );
    fprintf ( 1, '  There was some kind of I/O problem while trying\n' );
    fprintf ( 1, '  to count the number of data columns in\n' );
    fprintf ( 1, '  the file %s.\n', input_filename );
  end

  n = file_row_count ( input_filename );

  if ( n <= 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'R8MAT_HEADER_READ - Fatal error!\n' );
    fprintf ( 1, '  There was some kind of I/O problem while trying\n' );
    fprintf ( 1, '  to count the number of data rows in\n' );
    fprintf ( 1, '  the file %s\n', input_filename );
  end

  return
end
function len = s_len_trim ( s )

%*****************************************************************************80
%
%% s_len_trim() returns the length of a character string to the last nonblank.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 June 2003
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string S, the string to be measured.
%
%  Output:
%
%    integer LEN, the length of the string up to the last nonblank.
%
  len = length ( s );

  while ( 0 < len )
    if ( s(len) ~= ' ' )
      return
    end
    len = len - 1;
  end

  return
end
function word_num = s_word_count ( s )

%*****************************************************************************80
%
%% s_word_count() counts the number of "words" in a string.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 January 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string S, the string to be examined.
%
%  Output:
%
%    integer WORD_NUM, the number of "words" in the string.
%    Words are presumed to be separated by one or more blanks.
%
  FALSE = 0;
  TRUE = 1;

  word_num = 0;
  s_length = length ( s );

  if ( s_length <= 0 )
    return;
  end

  blank = TRUE;

  for i = 1 : s_length

    if ( s(i) == ' ' )
      blank = TRUE;
    elseif ( blank == TRUE )
      word_num = word_num + 1;
      blank = FALSE;
    end

  end

  return
end
function value = tet01_monomial_integral ( expon )

%*****************************************************************************80
%
%% tet01_monomial_integral() integrates a monomial over the unit tetrahedron.
%
%  Discussion:
%
%    This routine evaluates a monomial of the form
%
%      product ( 1 <= dim <= dim_num ) x(dim)^expon(dim)
%
%    where the exponents are nonnegative integers.  Note that
%    if the combination 0^0 is encountered, it should be treated
%    as 1.
%
%    Integral ( over unit tetrahedron ) x^l y^m z^n dx dy = 
%    l! * m! * n! / ( m + n + 3 )!
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 July 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    integer EXPON(DIM_NUM), the exponents.
%
%  Output:
%
%    real VALUE, the value of the integral of the monomial.
%

%
%  The first computation ends with VALUE = 1.0;
%
  value = 1.0;

  k = 0;

  for i = 1 : expon(1)
    k = k + 1;
%   value = value * i / k;
  end

  for i = 1 : expon(2)
    k = k + 1;
    value = value * i / k;
  end

  for i = 1 : expon(3)
    k = k + 1;
    value = value * i / k;
  end

  k = k + 1;
  value = value / k;

  k = k + 1;
  value = value / k;

  k = k + 1;
  value = value / k;

  return
end
function quad_error = tet01_monomial_quadrature ( dim_num, expon, point_num, ...
  x, weight )

%*****************************************************************************80
%
%% tet01_monomial_quadrature() applies quadrature to a monomial in a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 July 2007
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer DIM_NUM, the spatial dimension.
%
%    integer EXPON(DIM_NUM), the exponents.
%
%    integer POINT_NUM, the number of points in the rule.
%
%    real X(DIM_NUM,POINT_NUM), the quadrature points.
%
%    real WEIGHT(POINT_NUM), the quadrature weights.
%
%  Output:
%
%    real QUAD_ERROR, the quadrature error.
%

%
%  Get the exact value of the integral of the unscaled monomial.
%
  scale = tet01_monomial_integral ( expon );
%
%  Evaluate the monomial at the quadrature points.
%
  value = monomial_value ( dim_num, point_num, x, expon );
%
%  Compute the weighted sum and divide by the exact value.
%
  volume = 1.0 / 6.0;
  quad = volume * ( weight * transpose ( value ) ) / scale;
%
%  Error:
%
  exact = 1.0;
  quad_error = abs ( quad - exact );

  return
end
function ref = tetrahedron_order4_physical_to_reference ( t, n, phy )

%*****************************************************************************80
%
%% tetrahedron_order4_physical_to_reference() maps physical points to reference points.
%
%  Discussion:
%
%    Given the vertices of an order 4 physical tetrahedron and a point
%    (X,Y,Z) in the physical tetrahedron, the routine computes the value
%    of the corresponding image point (R,S,T) in reference space.
%
%    This routine may be appropriate for an order 10 tetrahedron,
%    if the mapping between reference and physical space is linear.
%    This implies, in particular, that the edges of the image tetrahedron
%    are straight, the faces are flat, and the "midside" nodes in the
%    physical tetrahedron are halfway along the sides of the physical
%    tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 December 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(3,4), the X, Y,Z coordinates
%    of the vertices.  The vertices are assumed to be the images of
%    (0,0,0), (1,0,0), (0,1,0) and (0,0,1) respectively.
%
%    integer N, the number of points to transform.
%
%    real PHY(3,N), the coordinates of physical points
%    to be transformed.
%
%  Output:
%
%    real REF(3,N), the coordinates of the corresponding
%    points in the reference space.
%

%
%  Set up the matrix.
%
  a(1:3,1) = t(1:3,2) - t(1:3,1);
  a(1:3,2) = t(1:3,3) - t(1:3,1);
  a(1:3,3) = t(1:3,4) - t(1:3,1);
%
%  Compute the determinant.
%
  det =  a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) ...
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) ...
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) );
%
%  If the determinant is zero, bail out.
%
  if ( det == 0.0 )
    ref(1:3,1:n) = 0.0;
    return
  end
%
%  Compute the solution.
%
  ref(1,1:n) = ...
    (   ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) * ( phy(1,1:n) - t(1,1) ) ...
      - ( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) * ( phy(2,1:n) - t(2,1) ) ...
      + ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) * ( phy(3,1:n) - t(3,1) ) ...
    ) / det;

  ref(2,1:n) = ...
    ( - ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) * ( phy(1,1:n) - t(1,1) ) ...
      + ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) * ( phy(2,1:n) - t(2,1) ) ...
      - ( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) * ( phy(3,1:n) - t(3,1) ) ...
    ) / det;

  ref(3,1:n) = ...
    (   ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) * ( phy(1,1:n) - t(1,1) ) ...
      - ( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) * ( phy(2,1:n) - t(2,1) ) ...
      + ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) * ( phy(3,1:n) - t(3,1) ) ...
    ) / det;

  return
end
function volume = tetrahedron_volume ( tetra )

%*****************************************************************************80
%
%% tetrahedron_volume() computes the volume of a tetrahedron.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 May 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real TETRA(3,4), the vertices of the tetrahedron.
%
%  Output:
%
%    real VOLUME, the volume of the tetrahedron.
%
  dim_num = 3;

  a(1:dim_num,1:4) = tetra(1:dim_num,1:4);
  a(4,1:4) = 1.0;

  volume = abs ( r8mat_det_4d ( a ) ) / 6.0;

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end
 
