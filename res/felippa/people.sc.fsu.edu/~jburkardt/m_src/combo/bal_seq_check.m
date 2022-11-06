function check = bal_seq_check ( n, t )

%*****************************************************************************80
%
%% bal_seq_check() checks a balanced sequence.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    25 November 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Donald Kreher, Douglas Simpson,
%    Combinatorial Algorithms,
%    CRC Press, 1998,
%    ISBN: 0-8493-3988-X,
%    LC: QA164.K73.
%
%  Input:
%
%    integer N, the number of 0's (and 1's) in the sequence.
%    N must be positive.
%
%    integer T(2*N), a balanced sequence.
%
%  Output:
%
%    logical CHECK, error flag.
%    true, T is a legal balanced sequence.
%    false, T is not a legal balanced sequence.
%
  check = true;

  if ( n < 1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'bal_seq_check - Fatal error!\n' );
    fprintf ( 1, '  n < 1\n' );
    check = false;
    return
  end

  one_count = 0;
  zero_count = 0;

  for i = 1 : 2 * n

    if ( t(i) == 0 )
      zero_count = zero_count + 1;
    elseif ( t(i) == 1 )
      one_count = one_count + 1;
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'bal_seq_check - Fatal error!\n' );
      fprintf ( 1, '  t(i) is not 0 or 1\n' );
      check = false;
      return
    end

    if ( zero_count < one_count )
      fprintf ( 1, '\n' );
      fprintf ( 1, 'bal_seq_check - Fatal error!\n' );
      fprintf ( 1, '  zero_count < one_count\n' );
      check = false;
      return
    end

  end

  if ( one_count ~= zero_count )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'bal_seq_check - Fatal error!\n' );
    fprintf ( 1, '  zero_count ~= one_count\n' );
    check = false;
  end

  return
end

