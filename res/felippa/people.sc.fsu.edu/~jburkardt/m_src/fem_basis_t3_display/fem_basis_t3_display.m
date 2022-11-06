function fem_basis_t3_display ( prefix, node_index )

%*****************************************************************************80
%
%% fem_basis_t3_display() displays a finite element T3 basis.
%
%  Discussion:
%
%    This program reads a data file defining a set of nodes, and a
%    data file defining the triangulation of those nodes using 3 node triangles
%    (or 6 node triangles, as long as the vertices are listed first).
%
%    The program then asks the user interactively to select one of the
%    nodes.  It computes the basis function associated with that node
%    and displays it over the entire mesh.  Of course, the basis function
%    will only be nonzero over a small number of the elements, but it
%    is instructive to see the entire mesh.
%
%    The display is initially "flat", but by using the manipulator
%    on the graphics menu, the user can easily get some dramatic images
%    of the basis function.
%
%  Usage:
%
%    fem_basis_t3_display ( 'prefix' )
%
%    where 'prefix' is the common prefix for the FEM files:
%
%    * 'prefix'_nodes.txt,    the node coordinates.
%    * 'prefix'_elements.txt, the nodes that make up each element;
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 March 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string PREFIX, the common filename prefix.
%
%    integer NODE_INDEX, the index of the node whose basis function
%    is to be displayed.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'fem_basis_t3_display():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Display basis functions associated with \n' );
  fprintf ( 1, '  a finite element grid of linear triangles ("T3").\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  This program reads two files:\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  * node_file,     the node file,\n' );
  fprintf ( 1, '  * element_file,  the element file,\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The user specifies a basis function by node index, and\n' );
  fprintf ( 1, '  the program displays a surface plot of that basis function.\n' );
  fprintf ( 1, '  (Use the 3D ROTATE option to see the full picture!\n' );
%
%  Get the prefix if missing.
%
  if ( nargin < 1 )
    prefix = input ( '  Enter the common file prefix:  ' );
  end
%
%  Construct the file names.
%
  node_filename = strcat ( prefix, '_nodes.txt' );
  element_filename = strcat ( prefix, '_elements.txt' );
%
%  Read the nodes.
%
  node_xy = readmatrix ( node_filename );
  node_xy = node_xy';
  [ dim_num, node_num ] = size ( node_xy );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Spatial dimension = %d\n', dim_num );
  fprintf ( 1, '  Number of nodes = %d\n', node_num );
%
%  Read the elements.
%
  element_node = readmatrix ( element_filename );
  element_node = element_node';
  [ element_order, element_num ] = size ( element_node );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Order of elements  = %d\n', element_order );
  fprintf ( 1, '  Number of elements = %d\n', element_num );
%
%  Set up the graph.
%
  x_min = min ( node_xy(1,:) );
  x_max = max ( node_xy(1,:) );
  y_min = min ( node_xy(2,:) );
  y_max = max ( node_xy(2,:) );
  z_min = 0.0;
  z_max = 1.0;

  node_min = min ( min ( element_node ) );
  node_max = max ( max ( element_node ) );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Every basis function is associated with a node.\n' );
  fprintf ( 1, '  To chooose a basis function, you specify a node.\n' );
  fprintf ( 1, '  Nodes range in value from %d to %d.\n', node_min, node_max );
  
  clf ( );
    
  fprintf ( 1, '\n' );
  
  for element = 1 : element_num
    
    local = 0;
    for j = 1 : element_order
      if ( element_node(j,element) == node_index )
        fprintf ( 1, '  Node %d occurs as local node %d in element %d.\n', ...
          node_index, j, element );
        local = j;
      end 
    end

    z(1:2,1:2) = 0.0;

    if ( local == 0 )

      x(1,1) = node_xy(1,element_node(1,element)); 
      y(1,1) = node_xy(2,element_node(1,element));
      x(2,1) = node_xy(1,element_node(1,element)); 
      y(2,1) = node_xy(2,element_node(1,element));
      x(1,2) = node_xy(1,element_node(2,element)); 
      y(1,2) = node_xy(2,element_node(2,element));
      x(2,2) = node_xy(1,element_node(3,element)); 
      y(2,2) = node_xy(2,element_node(3,element));

    else

      x(1,1) = node_xy(1,element_node(local,element)); 
      y(1,1) = node_xy(2,element_node(local,element));
      z(1,1) = 1.0;

      x(2,1) = node_xy(1,element_node(local,element)); 
      y(2,1) = node_xy(2,element_node(local,element));
      z(2,1) = 1.0;

      j = i4_wrap ( local + 1, 1, element_order );
      x(1,2) = node_xy(1,element_node(j,element)); 
      y(1,2) = node_xy(2,element_node(j,element));

      j = i4_wrap ( j + 1, 1, element_order );
      x(2,2) = node_xy(1,element_node(j,element)); 
      y(2,2) = node_xy(2,element_node(j,element));

    end

    caxis ( [ -0.4, 1.2 ] );
    surface ( x, y, z, 'FaceColor', 'interp' );

  end

  axis ( [ x_min, x_max, y_min, y_max, z_min, z_max ] );
  axis ( 'equal' );

  xlabel ( '--X axis--', 'fontsize', 16 );
  ylabel ( '--Y axis--', 'fontsize', 16 );
  zlabel ( '--Z axis--', 'fontsize', 16 );

  title_string = sprintf ( 'T3 basis function for node %d', node_index );
  title ( title_string, 'fontsize', 16 );
  filename = sprintf ( '%s_node%d.png', prefix, node_index );
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  pause ( 5 );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'fem_basis_t3_display():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
function [ qi, dqidx, dqidy ] = basis_11_t3 ( t, i, p )

%*****************************************************************************80
%
%% basis_11_t3(): one basis at one point for the T3 element.
%
%  Discussion:
%
%    The routine is given the coordinates of the nodes of a triangle. 
%        
%           3
%          / \
%         /   \
%        /     \
%       1-------2
%
%    It evaluates the linear basis function Q(I)(X,Y) associated with
%    node I, which has the property that it is a linear function
%    which is 1 at node I and zero at the other two nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 February 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(2,3), the coordinates of the nodes.
%
%    integer I, the index of the desired basis function.
%    I should be between 1 and 3.
%
%    real P(2), the coordinates of a point at which the basis
%    function is to be evaluated.
%
%  Output:
%
%    real QI, DQIDX, DQIDY, the values of the basis function
%    and its X and Y derivatives.
%
  area = abs ( t(1,1) * ( t(2,2) - t(2,3) ) ...
             + t(1,2) * ( t(2,3) - t(2,1) ) ...
             + t(1,3) * ( t(2,1) - t(2,2) ) );

  if ( area == 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'BASIS_11_T3 - Fatal error!\n' );
    fprintf ( 1, '  Element has zero area.\n' );
    error ( 'BASIS_11_T3 - Fatal error!' );
  end

  if ( i < 1 | 3 < i )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'BASIS_11_T3 - Fatal error!\n' );
    fprintf ( 1, '  Basis index I is not between 1 and 3.\n' );
    fprintf ( 1, '  I = %d\n', i );
    error ( 'BASIS_11_T3 - Fatal error!' );
  end

  ip1 = i4_wrap ( i + 1, 1, 3 );
  ip2 = i4_wrap ( i + 2, 1, 3 );

  qi = ( ( t(1,ip2) - t(1,ip1) ) * ( p(2) - t(2,ip1) ) ...
       - ( t(2,ip2) - t(2,ip1) ) * ( p(1) - t(1,ip1) ) ) / area;

  dqidx = - ( t(2,ip2) - t(2,ip1) ) / area;
  dqidy =   ( t(1,ip2) - t(1,ip1) ) / area;

  return
end
function value = i4_modp ( i, j )

%*****************************************************************************80
%
%% i4_modp() returns the nonnegative remainder of I4 division.
%
%  Discussion:
%
%    If
%      NREM = I4_MODP ( I, J )
%      NMULT = ( I - NREM ) / J
%    then
%      I = J * NMULT + NREM
%    where NREM is always nonnegative.
%
%    The MOD function computes a result with the same sign as the
%    quantity being divided.  Thus, suppose you had an angle A,
%    and you wanted to ensure that it was between 0 and 360.
%    Then mod(A,360) would do, if A was positive, but if A
%    was negative, your result would be between -360 and 0.
%
%    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
%
%  Example:
%
%        I     J     MOD  I4_MODP    Factorization
%
%      107    50       7       7    107 =  2 *  50 + 7
%      107   -50       7       7    107 = -2 * -50 + 7
%     -107    50      -7      43   -107 = -3 *  50 + 43
%     -107   -50      -7      43   -107 =  3 * -50 + 43
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 1999
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, the number to be divided.
%
%    integer J, the number that divides I.
%
%  Output:
%
%    integer VALUE, the nonnegative remainder when I is
%    divided by J.
%
  if ( j == 0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'I4_MODP - Fatal error!\n' );
    fprintf ( 1, '  Illegal divisor J = %d\n', j );
    error ( 'I4_MODP - Fatal error!' );
  end

  value = mod ( i, j );

  if ( value < 0 )
    value = value + abs ( j );
  end

  return
end
function value = i4_wrap ( ival, ilo, ihi )

%*****************************************************************************80
%
%% i4_wrap() forces an integer to lie between given limits by wrapping.
%
%  Example:
%
%    ILO = 4, IHI = 8
%
%    I   Value
%
%    -2     8
%    -1     4
%     0     5
%     1     6
%     2     7
%     3     8
%     4     4
%     5     5
%     6     6
%     7     7
%     8     8
%     9     4
%    10     5
%    11     6
%    12     7
%    13     8
%    14     4
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 October 2006
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer IVAL, an integer value.
%
%    integer ILO, IHI, the desired bounds for the integer value.
%
%  Output:
%
%    integer VALUE, a "wrapped" version of IVAL.
%
  jlo = min ( ilo, ihi );
  jhi = max ( ilo, ihi );

  wide = jhi - jlo + 1;

  if ( wide == 1 )
    value = jlo;
  else
    value = jlo + i4_modp ( ival - jlo, wide );
  end

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

