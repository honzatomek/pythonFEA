# include <float.h>
# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>

int main ( int argc, char *argv[] );
char ch_cap ( char ch );
int ch_eqi ( char ch1, char ch2 );
int ch_to_digit ( char ch );
int file_column_count ( char *input_filename );
int file_row_count ( char *input_filename );
int product_rule_size ( char *list_filename, int list_num );
double *r8mat_data_read ( char *input_filename, int m, int n );
void r8mat_header_read ( char *input_filename, int *m, int *n );
void r8mat_write ( char *output_filename, int m, int n, double table[] );
void r8vec_direct_product ( int factor_index, int factor_order, 
  double factor_value[], int factor_num, int point_num, double x[] );
void r8vec_direct_product2 ( int factor_index, int factor_order, 
  double factor_value[], int factor_num, int point_num, double w[] );
int s_len_trim ( char *s );
int s_to_i4 ( char *s, int *last, int *error );
double s_to_r8 ( char *s, int *lchar, int *error );
int s_to_r8vec ( char *s, int n, double rvec[] );
int s_word_count ( char *s );
void timestamp ( );

/******************************************************************************/

int main ( int argc, char *argv[] )

/******************************************************************************/
/*
  Purpose:

    MAIN is the main program for PRODUCT_RULE.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    15 February 2014

  Author:

    John Burkardt
*/
{
  int dim;
  int dim_num;
  int dim_num_1d;
  char list_filename[80];
  int list_num;
  FILE *list_unit;
  int point;
  int point_num;
  int point_num_1d;
  int point_num_1d2;
  char quad_filename[80];
  char quad_1d_filename[80];
  char quad_r_1d_filename[80];
  char quad_r_filename[80];
  char quad_w_1d_filename[80];
  char quad_w_filename[80];
  char quad_x_1d_filename[80];
  char quad_x_filename[80];
  double *r;
  double *r_1d;
  double *w;
  double *w_1d;
  double *x;
  double *x_1d;

  timestamp ( );
  printf ( "\n" );
  printf ( "PRODUCT_RULE\n" );
  printf ( "  C version\n" );
  printf ( "\n" );
  printf ( "  Compiled on %s at %s\n", __DATE__, __TIME__  );
  printf ( "\n" );
  printf ( "  Create a multidimensional product rule\n" );
  printf ( "  as a product of distinct 1D integration rules.\n" );
/*
  Get the list filename:
*/
  if ( 1 < argc ) 
  {
    strcpy ( list_filename, argv[1] );
  }
  else
  {
    printf ( "\n" );
    printf ( "PRODUCT_RULE:\n" );
    printf ( "  Enter the name of the file listing the factors.\n" );

    scanf ( "%s", list_filename );
  }
/*
  Get the product file root name:
*/
  if ( 2 < argc ) 
  {
    strcpy ( quad_filename, argv[2] );
  }
  else
  {
    printf ( "\n" );
    printf ( "PRODUCT_RULE:\n" );
    printf ( "  Enter the filename prefix for the output.\n" );

    scanf ( "%s", quad_filename );
  }
/*
  Count the items in the list file.
*/
  list_num = file_row_count ( list_filename );
/*
  Determine the spatial dimension and number of points in the product.
*/
  dim_num = list_num;
  point_num = product_rule_size ( list_filename, list_num );
/*
  Allocate the product items.
*/
  x = ( double * ) malloc ( dim_num * point_num * sizeof ( double ) );
  w = ( double * ) malloc ( point_num * sizeof ( double ) );
  r = ( double * ) malloc ( dim_num * 2 * sizeof ( double ) );

  for ( point = 0; point < point_num; point++ )
  {
    for ( dim = 0; dim < dim_num; dim++ )
    {
      x[dim+point*dim_num] = 0.0;
    }
  }
  for ( point = 0; point < point_num; point++ )
  {
    w[point] = 1.0;
  }
  for ( point = 0; point < 2; point++ )
  {
    for ( dim = 0; dim < dim_num; dim++ )
    {
      r[dim+point*dim_num] = 0.0;
    }
  }
  list_unit = fopen ( list_filename, "rt" );

  if ( !list_unit )
  {
    printf ( "\n" );
    printf ( "PRODUCT_RULE - Fatal error!\n" );
    printf ( "  Could not open the list file.\n" );
    exit ( 1 );
  }
/*
  Read the factor information and apply it.
*/
  for ( dim = 0; dim < dim_num; dim++ )
  {
    fscanf ( list_unit, "%s", quad_1d_filename );

    sprintf ( quad_x_1d_filename, "%s_x.txt", quad_1d_filename );
    sprintf ( quad_w_1d_filename, "%s_w.txt", quad_1d_filename );
    sprintf ( quad_r_1d_filename, "%s_r.txt", quad_1d_filename );
/*
  Read the X file.
*/
    r8mat_header_read ( quad_x_1d_filename, &dim_num_1d, &point_num_1d );

    if ( dim_num_1d != 1 ) 
    {
      printf ( "\n" );
      printf ( "PRODUCT_RULE - Fatal error!\n" );
      printf ( "  The 1D quadrature abscissa file should have exactly\n" );
      printf ( "  one value on each line.\n" );
      exit ( 1 );
    }

    printf ( "\n" );
    printf ( "  Number of points in 1D rule = %d\n", point_num_1d );

    x_1d = r8mat_data_read ( quad_x_1d_filename, dim_num_1d, point_num_1d );
/*
  Read the W file.
*/
    r8mat_header_read ( quad_w_1d_filename, &dim_num_1d, &point_num_1d2 );

    if ( dim_num_1d != 1 ) 
    {
      printf ( "\n" );
      printf ( "PRODUCT_RULE - Fatal error!\n" );
      printf ( "  The 1D quadrature weight file should have exactly\n" );
      printf ( "  one value on each line.\n" );
      exit ( 1 );
    }

    if ( point_num_1d2 != point_num_1d )
    {
      printf ( "\n" );
      printf ( "PRODUCT_RULE - Fatal error!\n" );
      printf ( "  The 1D quadrature weight file should have exactly\n" );
      printf ( "  the same number of lines as the abscissa file.\n" );
      exit ( 1 );
    }

    w_1d = r8mat_data_read ( quad_w_1d_filename, dim_num_1d, point_num_1d );
/*
  Read the R file.
*/
    r8mat_header_read ( quad_r_1d_filename, &dim_num_1d, &point_num_1d2 );

    if ( dim_num_1d != 1 )
    { 
      printf ( "\n" );
      printf ( "PRODUCT_RULE - Fatal error!\n" );
      printf ( "  The 1D quadrature region file should have exactly\n" );
      printf ( "  one value on each line.\n" );
      exit ( 1 );
    }

    if ( point_num_1d2 != 2 )
    {
      printf ( "\n" );
      printf ( "PRODUCT_RULE - Fatal error!\n" );
      printf ( "  The 1D quadrature region file should have two lines.\n" );
      exit ( 1 );
    }

    r_1d = r8mat_data_read ( quad_r_1d_filename, 1, 2 );
/*
  Update the X, W, and R of the product rule.
*/
    r8vec_direct_product ( dim, point_num_1d, x_1d, dim_num, point_num, x );

    r8vec_direct_product2 ( dim, point_num_1d, w_1d, dim_num, point_num, w );

    r[dim+0*dim_num] = r_1d[0];
    r[dim+1*dim_num] = r_1d[1];

    free ( r_1d );
    free ( w_1d );
    free ( x_1d );
  }
/*
  Write product rule to files.
*/
  sprintf ( quad_x_filename, "%s_x.txt", quad_filename );
  sprintf ( quad_w_filename, "%s_w.txt", quad_filename );
  sprintf ( quad_r_filename, "%s_r.txt", quad_filename );

  printf ( "\n" );
  printf ( "  Creating product quadrature rule X file = '%s'\n",
    quad_x_filename );

  r8mat_write ( quad_x_filename, dim_num, point_num, x );

  printf ( "  Creating product quadrature rule W file = '%s'\n",
    quad_w_filename );

  r8mat_write ( quad_w_filename, 1, point_num, w );
 
  printf ( "  Creating product quadrature rule R file = '%s'\n",
    quad_r_filename );

  r8mat_write ( quad_r_filename, dim_num, 2, r );
/*
  Free memory.
*/
  free ( r );
  free ( w );
  free ( x );
/*
  Terminate.
*/
  printf ( "\n" );
  printf ( "PRODUCT_RULE:\n" );
  printf ( "  Normal end of execution.\n" );
  printf ( "\n" );
  timestamp ( );

  return 0;
}
/******************************************************************************/

char ch_cap ( char ch )

/******************************************************************************/
/*
  Purpose:

    CH_CAP capitalizes a single character.

  Discussion:

    This routine should be equivalent to the library "toupper" function.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    19 July 1998

  Author:

    John Burkardt

  Parameters:

    Input, char CH, the character to capitalize.

    Output, char CH_CAP, the capitalized character.
*/
{
  if ( 97 <= ch && ch <= 122 )
  {
    ch = ch - 32;
  }

  return ch;
}
/******************************************************************************/

int ch_eqi ( char ch1, char ch2 )

/******************************************************************************/
/*
  Purpose:

    CH_EQI is TRUE (1) if two characters are equal, disregarding case.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    13 June 2003

  Author:

    John Burkardt

  Parameters:

    Input, char CH1, CH2, the characters to compare.

    Output, int CH_EQI, is TRUE (1) if the two characters are equal,
    disregarding case and FALSE (0) otherwise.
*/
{
  int value;

  if ( 97 <= ch1 && ch1 <= 122 )
  {
    ch1 = ch1 - 32;
  }
  if ( 97 <= ch2 && ch2 <= 122 )
  {
    ch2 = ch2 - 32;
  }
  if ( ch1 == ch2 )
  {
    value = 1;
  }
  else
  {
    value = 0;
  }
  return value;
}
/******************************************************************************/

int ch_to_digit ( char ch )

/******************************************************************************/
/*
  Purpose:

    CH_TO_DIGIT returns the integer value of a base 10 digit.

  Example:

     CH  DIGIT
    ---  -----
    '0'    0
    '1'    1
    ...  ...
    '9'    9
    ' '    0
    'X'   -1

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    13 June 2003

  Author:

    John Burkardt

  Parameters:

    Input, char CH, the decimal digit, '0' through '9' or blank are legal.

    Output, int CH_TO_DIGIT, the corresponding integer value.  If the
    character was 'illegal', then DIGIT is -1.
*/
{
  int digit;

  if ( '0' <= ch && ch <= '9' )
  {
    digit = ch - '0';
  }
  else if ( ch == ' ' )
  {
    digit = 0;
  }
  else
  {
    digit = -1;
  }

  return digit;
}
/******************************************************************************/

int file_column_count ( char *input_filename )

/******************************************************************************/
/*
  Purpose:

    FILE_COLUMN_COUNT counts the number of columns in the first line of a file.

  Discussion:

    The file is assumed to be a simple text file.

    Most lines of the file is presumed to consist of COLUMN_NUM words, separated
    by spaces.  There may also be some blank lines, and some comment lines,
    which have a "#" in column 1.

    The routine tries to find the first non-comment non-blank line and
    counts the number of words in that line.

    If all lines are blanks or comments, it goes back and tries to analyze
    a comment line.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    13 June 2003

  Author:

    John Burkardt

  Parameters:

    Input, char *INPUT_FILENAME, the name of the file.

    Output, int FILE_COLUMN_COUNT, the number of columns assumed 
    to be in the file.
*/
{
# define LINE_MAX 256

  int column_num;
  char *error;
  FILE *input;
  int got_one;
  char line[LINE_MAX];
/*
  Open the file.
*/
  input = fopen ( input_filename, "r" );

  if ( !input )
  {
    column_num = -1;
    printf ( "\n" );
    printf ( "FILE_COLUMN_COUNT - Fatal error!\n" );
    printf ( "  Could not open the input file: \"%s\"\n", input_filename );
    return column_num;
  }
/*
  Read one line, but skip blank lines and comment lines.
*/
  got_one = 0;

  for ( ; ; )
  {
    error = fgets ( line, LINE_MAX, input );

    if ( !error )
    {
      break;
    }

    if ( s_len_trim ( line ) == 0 )
    {
      continue;
    }

    if ( line[0] == '#' )
    {
      continue;
    }

    got_one = 1;
    break;

  }

  if ( got_one == 0 )
  {
    fclose ( input );

    input = fopen ( input_filename, "r" );

    for ( ; ; )
    {
      error = fgets ( line, LINE_MAX, input );

      if ( !error )
      {
        break;
      }

      if ( s_len_trim ( line ) == 0 )
      {
        continue;
      }

      got_one = 1;
      break;
    }
  }

  fclose ( input );

  if ( got_one == 0 )
  {
    printf ( "\n" );
    printf ( "FILE_COLUMN_COUNT - Warning!\n" );
    printf ( "  The file does not seem to contain any data.\n" );
    return -1;
  }

  column_num = s_word_count ( line );

  return column_num;

# undef LINE_MAX
}
/******************************************************************************/

int file_row_count ( char *input_filename )

/******************************************************************************/
/*
  Purpose:

    FILE_ROW_COUNT counts the number of row records in a file.

  Discussion:

    It does not count lines that are blank, or that begin with a
    comment symbol '#'.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    13 June 2003

  Author:

    John Burkardt

  Parameters:

    Input, char *INPUT_FILENAME, the name of the input file.

    Output, int FILE_ROW_COUNT, the number of rows found.
*/
{
# define LINE_MAX 255

  int comment_num;
  char *error;
  FILE *input;
  char line[LINE_MAX];
  int record_num;
  int row_num;

  row_num = 0;
  comment_num = 0;
  record_num = 0;

  input = fopen ( input_filename, "r" );

  if ( !input )
  {
    printf ( "\n" );
    printf ( "FILE_ROW_COUNT - Fatal error!\n" );
    printf ( "  Could not open the input file: \"%s\"\n", input_filename );
    return (-1);
  }

  for ( ; ; )
  {
    error = fgets ( line, LINE_MAX, input );

    if ( !error )
    {
      break;
    }

    record_num = record_num + 1;

    if ( line[0] == '#' )
    {
      comment_num = comment_num + 1;
      continue;
    }

    if ( s_len_trim ( line ) == 0 )
    {
      comment_num = comment_num + 1;
      continue;
    }

    row_num = row_num + 1;
  }

  fclose ( input );

  return row_num;

# undef LINE_MAX
}
/******************************************************************************/

int product_rule_size ( char *list_filename, int list_num )

/******************************************************************************/
/*
  Purpose:

    PRODUCT_RULE_SIZE returns the size of a product rule of distinct factors.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    15 February 2014

  Author:

    John Burkardt

  Parameters:

    Input, char *LIST_FILENAME, a file containing a list
    of prefixes defining quadrature rules.

    Input, int LIST_NUM, the number of prefixes in the file.

    Output, int PRODUCT_RULE_SIZE, the number of points in the product rule.
*/
{
  int dim_num_1d;
  int list;
  FILE *list_unit;
  int point_num;
  int point_num_1d;
  char quad_1d_filename[80];
  char quad_x_1d_filename[80];

  point_num = 1;

  list_unit = fopen ( list_filename, "rt" );

  if ( !list_unit )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "PRODUCT_RULE_SIZE - Fatal error!\n" );
    fprintf ( stderr, "  Could not open list file.\n" );
    exit ( 1 );
  }

  for ( list = 1; list <= list_num; list++ )
  {
    fscanf ( list_unit, "%s", quad_1d_filename );

    sprintf ( quad_x_1d_filename, "%s_x.txt", quad_1d_filename );
    
    r8mat_header_read ( quad_x_1d_filename, &dim_num_1d, &point_num_1d );

    point_num = point_num * point_num_1d;
  }

  fclose ( list_unit );

  return point_num;
}
/******************************************************************************/

double *r8mat_data_read ( char *input_filename, int m, int n )

/******************************************************************************/
/*
  Purpose:

    R8MAT_DATA_READ reads the data from an R8MAT file.

  Discussion:

    An R8MAT is an array of R8's.

    The file is assumed to contain one record per line.

    Records beginning with the '#' character are comments, and are ignored.
    Blank lines are also ignored.

    Each line that is not ignored is assumed to contain exactly (or at least)
    M real numbers, representing the coordinates of a point.

    There are assumed to be exactly (or at least) N such records.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    27 January 2005

  Author:

    John Burkardt

  Parameters:

    Input, char *INPUT_FILENAME, the name of the input file.

    Input, int M, the number of spatial dimensions.

    Input, int N, the number of points.  The program
    will stop reading data once N values have been read.

    Output, double R8MAT_DATA_READ[M*N], the data.
*/
{
# define LINE_MAX 255

  int error;
  char *got_string;
  FILE *input;
  int i;
  int j;
  char line[255];
  double *table;
  double *x;

  input = fopen ( input_filename, "r" );

  if ( !input )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8MAT_DATA_READ - Fatal error!\n" );
    fprintf ( stderr, "  Could not open the input file: \"%s\"\n", input_filename );
    exit ( 1 );
  }

  table = ( double * ) malloc ( m * n * sizeof ( double ) );

  x = ( double * ) malloc ( m * sizeof ( double ) );

  j = 0;

  while ( j < n )
  {
    got_string = fgets ( line, LINE_MAX, input );

    if ( !got_string )
    {
      break;
    }

    if ( line[0] == '#' || s_len_trim ( line ) == 0 )
    {
      continue;
    }

    error = s_to_r8vec ( line, m, x );

    if ( error == 1 )
    {
      continue;
    }

    for ( i = 0; i < m; i++ )
    {
      table[i+j*m] = x[i];
    }
    j = j + 1;

  }

  fclose ( input );

  free ( x );

  return table;

# undef LINE_MAX
}
/******************************************************************************/
 
void r8mat_header_read ( char *input_filename, int *m, int *n )
 
/******************************************************************************/
/*
  Purpose:

    R8MAT_HEADER_READ reads the header from an R8MAT file.

  Discussion:

    An R8MAT is an array of R8's.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    04 June 2004

  Author:

    John Burkardt

  Parameters:

    Input, char *INPUT_FILENAME, the name of the input file.

    Output, int *M, the number of spatial dimensions.

    Output, int *N, the number of points.
*/
{
  *m = file_column_count ( input_filename );

  if ( *m <= 0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8MAT_HEADER_READ - Fatal error!\n" );
    fprintf ( stderr, "  FILE_COLUMN_COUNT failed.\n" );
    exit ( 1 );
  }

  *n = file_row_count ( input_filename );

  if ( *n <= 0 )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8MAT_HEADER_READ - Fatal error!\n" );
    fprintf ( stderr, "  FILE_ROW_COUNT failed.\n" );
    exit ( 1 );
  }

  return;
}
/******************************************************************************/

void r8mat_write ( char *output_filename, int m, int n, double table[] )

/******************************************************************************/
/*
  Purpose:

    R8MAT_WRITE writes an R8MAT file.

  Discussion:

    An R8MAT is an array of R8's.

  Licensing:

    This code is distributed under the GNU LGPL license. 

  Modified:

    01 June 2009

  Author:

    John Burkardt

  Parameters:

    Input, char *OUTPUT_FILENAME, the output filename.

    Input, int M, the spatial dimension.

    Input, int N, the number of points.

    Input, double TABLE[M*N], the data.
*/
{
  int i;
  int j;
  FILE *output;
/*
  Open the file.
*/
  output = fopen ( output_filename, "wt" );

  if ( !output )
  {
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "R8MAT_WRITE - Fatal error!\n" );
    fprintf ( stderr, "  Could not open the output file.\n" );
    exit ( 1 );
  }
/*
  Write the data.
*/
  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < m; i++ )
    {
      fprintf ( output, "  %24.16g", table[i+j*m] );
    }
    fprintf ( output, "\n" );
  }
/*
  Close the file.
*/
  fclose ( output );

  return;
}
/******************************************************************************/

void r8vec_direct_product ( int factor_index, int factor_order,
  double factor_value[], int factor_num, int point_num, double x[] )

/******************************************************************************/
/*
  Purpose:

    R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.

  Discussion:

    An R8VEC is a vector of R8's.

    To explain what is going on here, suppose we had to construct
    a multidimensional quadrature rule as the product of K rules
    for 1D quadrature.

    The product rule will be represented as a list of points and weights.

    The J-th item in the product rule will be associated with
      item J1 of 1D rule 1,
      item J2 of 1D rule 2,
      ...,
      item JK of 1D rule K.

    In particular,
      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
    and
      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)

    So we can construct the quadrature rule if we can properly
    distribute the information in the 1D quadrature rules.

    This routine carries out that task.

    Another way to do this would be to compute, one by one, the
    set of all possible indices (J1,J2,...,JK), and then index
    the appropriate information.  An advantage of the method shown
    here is that you can process the K-th set of information and
    then discard it.

  Example:

    Rule 1:
      Order = 4
      X(1:4) = ( 1, 2, 3, 4 )

    Rule 2:
      Order = 3
      X(1:3) = ( 10, 20, 30 )

    Rule 3:
      Order = 2
      X(1:2) = ( 100, 200 )

    Product Rule:
      Order = 24
      X(1:24) =
        ( 1, 10, 100 )
        ( 2, 10, 100 )
        ( 3, 10, 100 )
        ( 4, 10, 100 )
        ( 1, 20, 100 )
        ( 2, 20, 100 )
        ( 3, 20, 100 )
        ( 4, 20, 100 )
        ( 1, 30, 100 )
        ( 2, 30, 100 )
        ( 3, 30, 100 )
        ( 4, 30, 100 )
        ( 1, 10, 200 )
        ( 2, 10, 200 )
        ( 3, 10, 200 )
        ( 4, 10, 200 )
        ( 1, 20, 200 )
        ( 2, 20, 200 )
        ( 3, 20, 200 )
        ( 4, 20, 200 )
        ( 1, 30, 200 )
        ( 2, 30, 200 )
        ( 3, 30, 200 )
        ( 4, 30, 200 )

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    31 May 2009

  Author:

    John Burkardt

  Parameters:

    Input, int FACTOR_INDEX, the index of the factor being processed.
    The first factor processed must be factor 0.

    Input, int FACTOR_ORDER, the order of the factor.

    Input, double FACTOR_VALUE[FACTOR_ORDER], the factor values
    for factor FACTOR_INDEX.

    Input, int FACTOR_NUM, the number of factors.

    Input, int POINT_NUM, the number of elements in the direct product.

    Input/output, double X[FACTOR_NUM*POINT_NUM], the elements of the
    direct product, which are built up gradually.

  Local Parameters:

    Local, int START, the first location of a block of values to set.

    Local, int CONTIG, the number of consecutive values to set.

    Local, int SKIP, the distance from the current value of START
    to the next location of a block of values to set.

    Local, int REP, the number of blocks of values to set.
*/
{
  static int contig = 0;
  int i;
  int j;
  int k;
  static int rep = 0;
  static int skip = 0;
  int start;

  if ( factor_index == 0 )
  {
    contig = 1;
    skip = 1;
    rep = point_num;
    for ( j = 0; j < point_num; j++ )
    {
      for ( i = 0; i < factor_num; i++ )
      {
        x[i+j*factor_num] = 0.0;
      }
    }
  }

  rep = rep / factor_order;
  skip = skip * factor_order;

  for ( i = 0; i < factor_order; i++ )
  {
    start = 0 + i * contig;

    for ( k = 1; k <= rep; k++ )
    {
      for ( j = start; j < start + contig; j++ )
      {
        x[factor_index+j*factor_num] = factor_value[i];
      }
      start = start + skip;
    }
  }
  contig = contig * factor_order;

  return;
}
/******************************************************************************/

void r8vec_direct_product2 ( int factor_index, int factor_order,
  double factor_value[], int factor_num, int point_num, double w[] )

/******************************************************************************/
/*
  Purpose:

    R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.

  Discussion:

    An R8VEC is a vector of R8's.

    To explain what is going on here, suppose we had to construct
    a multidimensional quadrature rule as the product of K rules
    for 1D quadrature.

    The product rule will be represented as a list of points and weights.

    The J-th item in the product rule will be associated with
      item J1 of 1D rule 1,
      item J2 of 1D rule 2,
      ...,
      item JK of 1D rule K.

    In particular,
      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
    and
      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)

    So we can construct the quadrature rule if we can properly
    distribute the information in the 1D quadrature rules.

    This routine carries out that task for the weights W.

    Another way to do this would be to compute, one by one, the
    set of all possible indices (J1,J2,...,JK), and then index
    the appropriate information.  An advantage of the method shown
    here is that you can process the K-th set of information and
    then discard it.

  Example:

    Rule 1:
      Order = 4
      W(1:4) = ( 2, 3, 5, 7 )

    Rule 2:
      Order = 3
      W(1:3) = ( 11, 13, 17 )

    Rule 3:
      Order = 2
      W(1:2) = ( 19, 23 )

    Product Rule:
      Order = 24
      W(1:24) =
        ( 2 * 11 * 19 )
        ( 3 * 11 * 19 )
        ( 4 * 11 * 19 )
        ( 7 * 11 * 19 )
        ( 2 * 13 * 19 )
        ( 3 * 13 * 19 )
        ( 5 * 13 * 19 )
        ( 7 * 13 * 19 )
        ( 2 * 17 * 19 )
        ( 3 * 17 * 19 )
        ( 5 * 17 * 19 )
        ( 7 * 17 * 19 )
        ( 2 * 11 * 23 )
        ( 3 * 11 * 23 )
        ( 5 * 11 * 23 )
        ( 7 * 11 * 23 )
        ( 2 * 13 * 23 )
        ( 3 * 13 * 23 )
        ( 5 * 13 * 23 )
        ( 7 * 13 * 23 )
        ( 2 * 17 * 23 )
        ( 3 * 17 * 23 )
        ( 5 * 17 * 23 )
        ( 7 * 17 * 23 )

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    31 May 2009

  Author:

    John Burkardt

  Parameters:

    Input, int FACTOR_INDEX, the index of the factor being processed.
    The first factor processed must be factor 0.

    Input, int FACTOR_ORDER, the order of the factor.

    Input, double FACTOR_VALUE[FACTOR_ORDER], the factor values for
    factor FACTOR_INDEX.

    Input, int FACTOR_NUM, the number of factors.

    Input, int POINT_NUM, the number of elements in the direct product.

    Input/output, double W[POINT_NUM], the elements of the
    direct product, which are built up gradually.

  Local Parameters:

    Local, integer START, the first location of a block of values to set.

    Local, integer CONTIG, the number of consecutive values to set.

    Local, integer SKIP, the distance from the current value of START
    to the next location of a block of values to set.

    Local, integer REP, the number of blocks of values to set.
*/
{
  static int contig = 0;
  int i;
  int j;
  int k;
  static int rep = 0;
  static int skip = 0;
  int start;

  if ( factor_index == 0 )
  {
    contig = 1;
    skip = 1;
    rep = point_num;
    for ( i = 0; i < point_num; i++ )
    {
      w[i] = 1.0;
    }
  }

  rep = rep / factor_order;
  skip = skip * factor_order;

  for ( j = 0; j < factor_order; j++ )
  {
    start = 0 + j * contig;

    for ( k = 1; k <= rep; k++ )
    {
      for ( i = start; i < start + contig; i++ )
      {
        w[i] = w[i] * factor_value[j];
      }
      start = start + skip;
    }
  }

  contig = contig * factor_order;

  return;
}
/******************************************************************************/

int s_len_trim ( char *s )

/******************************************************************************/
/*
  Purpose:

    S_LEN_TRIM returns the length of a string to the last nonblank.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    26 April 2003

  Author:

    John Burkardt

  Parameters:

    Input, char *S, a pointer to a string.

    Output, int S_LEN_TRIM, the length of the string to the last nonblank.
    If S_LEN_TRIM is 0, then the string is entirely blank.
*/
{
  int n;
  char *t;

  n = strlen ( s );
  t = s + strlen ( s ) - 1;

  while ( 0 < n )
  {
    if ( *t != ' ' )
    {
      return n;
    }
    t--;
    n--;
  }

  return n;
}
/******************************************************************************/

int s_to_i4 ( char *s, int *last, int *error )

/******************************************************************************/
/*
  Purpose:

    S_TO_I4 reads an I4 from a string.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    13 June 2003

  Author:

    John Burkardt

  Parameters:

    Input, char *S, a string to be examined.

    Output, int *LAST, the last character of S used to make IVAL.

    Output, int *ERROR is TRUE (1) if an error occurred and FALSE (0) otherwise.

    Output, int *S_TO_I4, the integer value read from the string.
    If the string is blank, then IVAL will be returned 0.
*/
{
  char c;
  int i;
  int isgn;
  int istate;
  int ival;

  *error = 0;
  istate = 0;
  isgn = 1;
  i = 0;
  ival = 0;

  while ( *s )
  {
    c = s[i];
    i = i + 1;
/*
  Haven't read anything.
*/
    if ( istate == 0 )
    {
      if ( c == ' ' )
      {
      }
      else if ( c == '-' )
      {
        istate = 1;
        isgn = -1;
      }
      else if ( c == '+' )
      {
        istate = 1;
        isgn = + 1;
      }
      else if ( '0' <= c && c <= '9' )
      {
        istate = 2;
        ival = c - '0';
      }
      else
      {
        *error = 1;
        return ival;
      }
    }
/*
  Have read the sign, expecting digits.
*/
    else if ( istate == 1 )
    {
      if ( c == ' ' )
      {
      }
      else if ( '0' <= c && c <= '9' )
      {
        istate = 2;
        ival = c - '0';
      }
      else
      {
        *error = 1;
        return ival;
      }
    }
/*
  Have read at least one digit, expecting more.
*/
    else if ( istate == 2 )
    {
      if ( '0' <= c && c <= '9' )
      {
        ival = 10 * (ival) + c - '0';
      }
      else
      {
        ival = isgn * ival;
        *last = i - 1;
        return ival;
      }
    }
  }
/*
  If we read all the characters in the string, see if we're OK.
*/
  if ( istate == 2 )
  {
    ival = isgn * ival;
    *last = s_len_trim ( s );
  }
  else
  {
    *error = 1;
    *last = 0;
  }

  return ival;
}
/******************************************************************************/

double s_to_r8 ( char *s, int *lchar, int *error )

/******************************************************************************/
/*
  Purpose:

    S_TO_R8 reads an R8 value from a string.

  Discussion:

    We have had some trouble with input of the form 1.0E-312.
    For now, let's assume anything less than 1.0E-20 is zero.

    This routine will read as many characters as possible until it reaches
    the end of the string, or encounters a character which cannot be
    part of the real number.

    Legal input is:

       1 blanks,
       2 '+' or '-' sign,
       2.5 spaces
       3 integer part,
       4 decimal point,
       5 fraction part,
       6 'E' or 'e' or 'D' or 'd', exponent marker,
       7 exponent sign,
       8 exponent integer part,
       9 exponent decimal point,
      10 exponent fraction part,
      11 blanks,
      12 final comma or semicolon.

    with most quantities optional.

  Example:

    S                 R

    '1'               1.0
    '     1   '       1.0
    '1A'              1.0
    '12,34,56'        12.0
    '  34 7'          34.0
    '-1E2ABCD'        -100.0
    '-1X2ABCD'        -1.0
    ' 2E-1'           0.2
    '23.45'           23.45
    '-4.2E+2'         -420.0
    '17d2'            1700.0
    '-14e-2'         -0.14
    'e2'              100.0
    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    02 May 2011

  Author:

    John Burkardt

  Parameters:

    Input, char *S, the string containing the
    data to be read.  Reading will begin at position 1 and
    terminate at the end of the string, or when no more
    characters can be read to form a legal real.  Blanks,
    commas, or other nonnumeric data will, in particular,
    cause the conversion to halt.

    Output, int *LCHAR, the number of characters read from
    the string to form the number, including any terminating
    characters such as a trailing comma or blanks.

    Output, int *ERROR, is TRUE (1) if an error occurred and FALSE (0)
    otherwise.

    Output, double S_TO_R8, the value that was read from the string.
*/
{
  char c;
  int ihave;
  int isgn;
  int iterm;
  int jbot;
  int jsgn;
  int jtop;
  int nchar;
  int ndig;
  double r;
  double rbot;
  double rexp;
  double rtop;
  char TAB = 9;
  static double ten = 10.0;

  nchar = s_len_trim ( s );
  *error = 0;
  r = 0.0;
  *lchar = -1;
  isgn = 1;
  rtop = 0.0;
  rbot = 1.0;
  jsgn = 1;
  jtop = 0;
  jbot = 1;
  ihave = 1;
  iterm = 0;

  for ( ; ; )
  {
    c = s[*lchar+1];
    *lchar = *lchar + 1;
/*
  Blank or TAB character.
*/
    if ( c == ' ' || c == TAB )
    {
      if ( ihave == 2 )
      {
      }
      else if ( ihave == 6 || ihave == 7 )
      {
        iterm = 1;
      }
      else if ( 1 < ihave )
      {
        ihave = 11;
      }
    }
/*
  Comma.
*/
    else if ( c == ',' || c == ';' )
    {
      if ( ihave != 1 )
      {
        iterm = 1;
        ihave = 12;
        *lchar = *lchar + 1;
      }
    }
/*
  Minus sign.
*/
    else if ( c == '-' )
    {
      if ( ihave == 1 )
      {
        ihave = 2;
        isgn = -1;
      }
      else if ( ihave == 6 )
      {
        ihave = 7;
        jsgn = -1;
      }
      else
      {
        iterm = 1;
      }
    }
/*
  Plus sign.
*/
    else if ( c == '+' )
    {
      if ( ihave == 1 )
      {
        ihave = 2;
      }
      else if ( ihave == 6 )
      {
        ihave = 7;
      }
      else
      {
        iterm = 1;
      }
    }
/*
  Decimal point.
*/
    else if ( c == '.' )
    {
      if ( ihave < 4 )
      {
        ihave = 4;
      }
      else if ( 6 <= ihave && ihave <= 8 )
      {
        ihave = 9;
      }
      else
      {
        iterm = 1;
      }
    }
/*
  Exponent marker.
*/
    else if ( ch_eqi ( c, 'E' ) || ch_eqi ( c, 'D' ) )
    {
      if ( ihave < 6 )
      {
        ihave = 6;
      }
      else
      {
        iterm = 1;
      }
    }
/*
  Digit.
*/
    else if ( ihave < 11 && '0' <= c && c <= '9' )
    {
      if ( ihave <= 2 )
      {
        ihave = 3;
      }
      else if ( ihave == 4 )
      {
        ihave = 5;
      }
      else if ( ihave == 6 || ihave == 7 )
      {
        ihave = 8;
      }
      else if ( ihave == 9 )
      {
        ihave = 10;
      }

      ndig = ch_to_digit ( c );

      if ( ihave == 3 )
      {
        rtop = 10.0 * rtop + ( double ) ndig;
      }
      else if ( ihave == 5 )
      {
        rtop = 10.0 * rtop + ( double ) ndig;
        rbot = 10.0 * rbot;
      }
      else if ( ihave == 8 )
      {
        jtop = 10 * jtop + ndig;
      }
      else if ( ihave == 10 )
      {
        jtop = 10 * jtop + ndig;
        jbot = 10 * jbot;
      }
    }
/*
  Anything else is regarded as a terminator.
*/
    else
    {
      iterm = 1;
    }
/*
  If we haven't seen a terminator, and we haven't examined the
  entire string, go get the next character.
*/
    if ( iterm == 1 || nchar <= *lchar + 1 )
    {
      break;
    }

  }
/*
  If we haven't seen a terminator, and we have examined the
  entire string, then we're done, and LCHAR is equal to NCHAR.
*/
  if ( iterm != 1 && (*lchar) + 1 == nchar )
  {
    *lchar = nchar;
  }
/*
  Number seems to have terminated.  Have we got a legal number?
  Not if we terminated in states 1, 2, 6 or 7!
*/
  if ( ihave == 1 || ihave == 2 || ihave == 6 || ihave == 7 )
  {
    *error = 1;
    return r;
  }
/*
  Number seems OK.  Form it.

  We have had some trouble with input of the form 1.0E-312.
  For now, let's assume anything less than 1.0E-20 is zero.
*/
  if ( jtop == 0 )
  {
    rexp = 1.0;
  }
  else
  {
    if ( jbot == 1 )
    {
      if ( jsgn * jtop < -20 )
      {
        rexp = 0.0;
      }
      else
      {
        rexp = pow ( ten, jsgn * jtop );
      }
    }
    else
    {
      if ( jsgn * jtop < -20 * jbot )
      {
        rexp = 0.0;
      }
      else
      {
        rexp = jsgn * jtop;
        rexp = rexp / jbot;
        rexp = pow ( ten, rexp );
      }
    }
  }

  r = isgn * rexp * rtop / rbot;

  return r;
}
/******************************************************************************/

int s_to_r8vec ( char *s, int n, double rvec[] )

/******************************************************************************/
/*
  Purpose:

    S_TO_R8VEC reads an R8VEC from a string.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    19 February 2001

  Author:

    John Burkardt

  Parameters:

    Input, char *S, the string to be read.

    Input, int N, the number of values expected.

    Output, double RVEC[N], the values read from the string.

    Output, int S_TO_R8VEC, is TRUE (1) if an error occurred and FALSE (0)
    otherwise.
*/
{
  int error;
  int i;
  int lchar;

  error = 0;

  for ( i = 0; i < n; i++ )
  {
    rvec[i] = s_to_r8 ( s, &lchar, &error );

    if ( error )
    {
      return error;
    }

    s = s + lchar;
  }

  return error;
}
/******************************************************************************/

int s_word_count ( char *s )

/******************************************************************************/
/*
  Purpose:

    S_WORD_COUNT counts the number of "words" in a string.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    16 September 2015

  Author:

    John Burkardt

  Parameters:

    Input, char *S, the string to be examined.

    Output, int S_WORD_COUNT, the number of "words" in the string.
    Words are presumed to be separated by one or more blanks.
*/
{
  int blank;
  int word_num;
  char *t;

  word_num = 0;
  blank = 1;
  t = s;

  while ( *t )
  {
    if ( *t == ' ' || *t == '\n' )
    {
      blank = 1;
    }
    else if ( blank )
    {
      word_num = word_num + 1;
      blank = 0;
    }
    t++;
  }

  return word_num;
}
/******************************************************************************/

void timestamp ( )

/******************************************************************************/
/*
  Purpose:

    TIMESTAMP prints the current YMDHMS date as a time stamp.

  Example:

    31 May 2001 09:45:54 AM

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    24 September 2003

  Author:

    John Burkardt

  Parameters:

    None
*/
{
# define TIME_SIZE 40

  static char time_buffer[TIME_SIZE];
  const struct tm *tm;
  time_t now;

  now = time ( NULL );
  tm = localtime ( &now );

  strftime ( time_buffer, TIME_SIZE, "%d %B %Y %I:%M:%S %p", tm );

  printf ( "%s\n", time_buffer );

  return;
# undef TIME_SIZE
}