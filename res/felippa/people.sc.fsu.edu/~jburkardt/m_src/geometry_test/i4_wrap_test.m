function i4_wrap_test ( )

%*****************************************************************************80
%
%% i4_wrap_test() tests i4_wrap().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 April 2009
%
%  Author:
%
%    John Burkardt
%
  ilo = 4;
  ihi = 8;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'i4_wrap_test():\n' );
  fprintf ( 1, '  i4_wrap() forces an integer to lie within given limits.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  ILO = %d\n', ilo );
  fprintf ( 1, '  IHI = %d\n', ihi );
  fprintf ( 1, '\n' );
  fprintf ( 1, '     I  I4_WRAP(I)\n' );
  fprintf ( 1, '\n' );

  for i = -10 : 20
    fprintf ( 1, '  %6d  %6d\n', i, i4_wrap ( i, ilo, ihi ) );
  end

  return
end
