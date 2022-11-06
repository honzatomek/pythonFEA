function [ nc, kc, ac ] = ksub_to_comp ( ns, ks, as )

%*****************************************************************************80
%
%% ksub_to_comp() converts a K-subset to a composition.
%
%  Discussion:
%
%    There is a bijection between K subsets and compositions.
%
%    Because we allow a composition to have entries that are 0, we need
%    to implicitly add 1 to each entry before establishing the bijection.
%
%    Let AS be a KS subset of a set of the integers 1 through NS.
%
%    Then let 
%      NC = NS - KS, 
%      KC = KS + 1, 
%    and define
%      AC(1) = AS(1) - 1;
%      AC(2:KC-1) = AS(2:KC-1) - AS(1:KC-2) - 1;
%      AC(KC) = NS - AS(KS).
%
%    Then AC is a composition of NC into KC parts.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 December 2013
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer NS, the size of the set.
%
%    integer KS, the size of the subset.
%
%    integer AS(KS), the entries of the K-subset, in increasing order.
%
%  Output:
%
%    integer NC, the composition sum.
%
%    integer KC, the number of parts of the composition.
%
%    integer AC(KC), the parts of the composition.
%
  nc = ns - ks;
  kc = ks + 1;
  ac = zeros ( kc, 1 );

  ac(1) = as(1) - 1;
  for i = 2 : kc - 1;
    ac(i) = as(i) - as(i-1) - 1;
  end
  ac(kc) = ns - as(ks);

  return
end


