function perm0_print ( n, p, title )

%*****************************************************************************80
%
%% perm0_print() prints a permutation of (0,...,N-1).
%
%  Example:
%
%    Input:
%
%      P = 6 1 3 0 4 2 5
%
%    Printed output:
%
%      "This is the permutation:"
%
%      0 1 2 3 4 5 6
%      6 1 3 0 4 2 5
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 June 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of objects permuted.
%
%    integer P(N), the permutation, in standard index form.
%
%    string TITLE, an optional title.
%    If no title is supplied, then only the permutation is printed.
%
  inc = 20;

  if ( s_len_trim ( title ) ~= 0 )

    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', title );

    for ilo = 1 : inc : n
      ihi = min ( n, ilo + inc - 1 );

      fprintf ( 1, '\n' );
      fprintf ( 1, '  ' );
      
      for i = ilo : ihi
        fprintf ( 1, '%4d', i - 1 )
      end
      fprintf ( 1, '\n' );

      fprintf ( 1, '  ' );
      for i = ilo : ihi
        fprintf ( 1, '%4d', p(i) )
      end
      fprintf ( 1, '\n' );

    end

  else

    for ilo = 1 : inc : n

      ihi = min ( n, ilo + inc - 1 );
      fprintf ( 1, '  ' );
      for i = ilo : ihi
        fprintf ( 1, '%4d', p(i) )
      end
      fprintf ( 1, '\n' );

    end

  end

  return
end
