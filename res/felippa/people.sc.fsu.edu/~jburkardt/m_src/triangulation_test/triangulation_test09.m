function triangulation_test09 ( )

%*****************************************************************************80
%
%% triangulation_test09() tests triangle_order3_physical_to_reference(), triangle_order3_reference_to_physical().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    07 December 2006
%
%  Author:
%
%    John Burkardt
%
  n = 10;

  t = [ ...
    1.0, 1.0; ...
    3.0, 1.0; ...
    2.0, 5.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangulation_test09\n' );
  fprintf ( 1, '  For an order 3 triangle,\n' );
  fprintf ( 1, '  TRIANGLE_ORDER3_PHYSICAL_TO_REFERENCE\n' );
  fprintf ( 1, '    maps a physical point to a reference point.\n' );
  fprintf ( 1, '  TRIANGLE_ORDER3_REFERENCE_TO_PHYSICAL\n' );
  fprintf ( 1, '    maps a reference point to a physical point.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '   (  XSI    ETA ) ==> ( X      Y  ) ==> ( XSI2    ETA2 )\n' );
  fprintf ( 1, '\n' );

  ref(1:2,1:n) = triangle_reference_sample ( n );

  phy = triangle_order3_reference_to_physical ( t, n, ref );

  ref2 = triangle_order3_physical_to_reference ( t, n, phy );

  for j = 1 : n

    fprintf ( 1, '  %8f  %8f  %8f  %8f  %8f  %8f\n', ...
      ref(1:2,j), phy(1:2,j), ref2(1:2,j) );

  end

  return
end
