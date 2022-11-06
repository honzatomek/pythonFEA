function [ x, w ] = rule19 ( n )

%*****************************************************************************80
%
%% rule19() returns the rule of degree 19.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    01 July 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
%  Input:
%
%    integer N, the number of nodes.
%
%  Output:
%
%    real X(2,N), the coordinates of the nodes.
%
%    real W(N), the weights.
%
  xs = [ ...
   -0.9734386316165470E+00, ...
    0.9744990929036832E+00, ...
    0.9734386316165472E+00, ...
   -0.9744990929036830E+00, ...
   -0.3841574585766744E+00, ...
    0.9670641778942685E+00, ...
    0.3841574585766745E+00, ...
   -0.9670641778942685E+00, ...
    0.2986734938364671E+00, ...
    0.9905525689050123E+00, ...
   -0.2986734938364670E+00, ...
   -0.9905525689050123E+00, ...
   -0.7396581737067777E+00, ...
    0.9869464369033261E+00, ...
    0.7396581737067779E+00, ...
   -0.9869464369033261E+00, ...
   -0.1425244970455050E+00, ...
    0.9733021904515969E+00, ...
    0.1425244970455051E+00, ...
   -0.9733021904515969E+00, ...
    0.7650240374639232E+00, ...
    0.9804863471920530E+00, ...
   -0.7650240374639230E+00, ...
   -0.9804863471920530E+00, ...
   -0.7599006633708002E+00, ...
    0.7279453517455540E+00, ...
    0.7599006633708002E+00, ...
   -0.7279453517455540E+00, ...
   -0.1192987760526789E+00, ...
   -0.2637912058730560E-02, ...
    0.1192987760526789E+00, ...
    0.2637912058730575E-02, ...
   -0.8850504442537889E+00, ...
    0.9022234232868145E+00, ...
    0.8850504442537889E+00, ...
   -0.9022234232868145E+00, ...
    0.5304174652462883E+00, ...
    0.9125489607085608E+00, ...
   -0.5304174652462881E+00, ...
   -0.9125489607085608E+00, ...
   -0.2858528945041368E+00, ...
    0.2941600854694212E+00, ...
    0.2858528945041368E+00, ...
   -0.2941600854694212E+00, ...
   -0.5671850101113227E+00, ...
    0.8836081660895880E+00, ...
    0.5671850101113227E+00, ...
   -0.8836081660895880E+00, ...
    0.3174295148500719E+00, ...
    0.7293427112089215E+00, ...
   -0.3174295148500718E+00, ...
   -0.7293427112089215E+00, ...
   -0.2492430513869149E+00, ...
    0.7672563284436533E+00, ...
    0.2492430513869150E+00, ...
   -0.7672563284436533E+00, ...
   -0.5087793568494521E+00, ...
    0.5623738439118215E+00, ...
    0.5087793568494521E+00, ...
   -0.5623738439118215E+00, ...
    0.7335719396414396E-01, ...
    0.8930925855397183E+00, ...
   -0.7335719396414385E-01, ...
   -0.8930925855397183E+00, ...
    0.8350775723842838E-02, ...
    0.5392457387102469E+00, ...
   -0.8350775723842772E-02, ...
   -0.5392457387102469E+00 ];
  ys = [ ...
   -0.9744990929036833E+00, ...
   -0.9734386316165471E+00, ...
    0.9744990929036831E+00, ...
    0.9734386316165473E+00, ...
   -0.9670641778942685E+00, ...
   -0.3841574585766744E+00, ...
    0.9670641778942685E+00, ...
    0.3841574585766745E+00, ...
   -0.9905525689050123E+00, ...
    0.2986734938364670E+00, ...
    0.9905525689050123E+00, ...
   -0.2986734938364669E+00, ...
   -0.9869464369033261E+00, ...
   -0.7396581737067778E+00, ...
    0.9869464369033261E+00, ...
    0.7396581737067780E+00, ...
   -0.9733021904515969E+00, ...
   -0.1425244970455050E+00, ...
    0.9733021904515969E+00, ...
    0.1425244970455051E+00, ...
   -0.9804863471920530E+00, ...
    0.7650240374639231E+00, ...
    0.9804863471920530E+00, ...
   -0.7650240374639229E+00, ...
   -0.7279453517455540E+00, ...
   -0.7599006633708002E+00, ...
    0.7279453517455540E+00, ...
    0.7599006633708002E+00, ...
    0.2637912058730553E-02, ...
   -0.1192987760526789E+00, ...
   -0.2637912058730568E-02, ...
    0.1192987760526789E+00, ...
   -0.9022234232868145E+00, ...
   -0.8850504442537889E+00, ...
    0.9022234232868145E+00, ...
    0.8850504442537889E+00, ...
   -0.9125489607085608E+00, ...
    0.5304174652462882E+00, ...
    0.9125489607085608E+00, ...
   -0.5304174652462880E+00, ...
   -0.2941600854694212E+00, ...
   -0.2858528945041368E+00, ...
    0.2941600854694212E+00, ...
    0.2858528945041368E+00, ...
   -0.8836081660895880E+00, ...
   -0.5671850101113227E+00, ...
    0.8836081660895880E+00, ...
    0.5671850101113227E+00, ...
   -0.7293427112089215E+00, ...
    0.3174295148500719E+00, ...
    0.7293427112089215E+00, ...
   -0.3174295148500718E+00, ...
   -0.7672563284436533E+00, ...
   -0.2492430513869149E+00, ...
    0.7672563284436533E+00, ...
    0.2492430513869150E+00, ...
   -0.5623738439118215E+00, ...
   -0.5087793568494521E+00, ...
    0.5623738439118215E+00, ...
    0.5087793568494521E+00, ...
   -0.8930925855397183E+00, ...
    0.7335719396414390E-01, ...
    0.8930925855397183E+00, ...
   -0.7335719396414379E-01, ...
   -0.5392457387102469E+00, ...
    0.8350775723842805E-02, ...
    0.5392457387102469E+00, ...
   -0.8350775723842739E-02 ];
  ws = [ ...
    0.4076118519980060E-02, ...
    0.4076118519980060E-02, ...
    0.4076118519980060E-02, ...
    0.4076118519980060E-02, ...
    0.1627326938099484E-01, ...
    0.1627326938099484E-01, ...
    0.1627326938099484E-01, ...
    0.1627326938099484E-01, ...
    0.1254157952509427E-01, ...
    0.1254157952509427E-01, ...
    0.1254157952509427E-01, ...
    0.1254157952509427E-01, ...
    0.1028929333936017E-01, ...
    0.1028929333936017E-01, ...
    0.1028929333936017E-01, ...
    0.1028929333936017E-01, ...
    0.1475928282295525E-01, ...
    0.1475928282295525E-01, ...
    0.1475928282295525E-01, ...
    0.1475928282295525E-01, ...
    0.1207323692393111E-01, ...
    0.1207323692393111E-01, ...
    0.1207323692393111E-01, ...
    0.1207323692393111E-01, ...
    0.4619184040692218E-01, ...
    0.4619184040692218E-01, ...
    0.4619184040692218E-01, ...
    0.4619184040692218E-01, ...
    0.3696173437828049E-01, ...
    0.3696173437828049E-01, ...
    0.3696173437828049E-01, ...
    0.3696173437828049E-01, ...
    0.2018069481193246E-01, ...
    0.2018069481193246E-01, ...
    0.2018069481193246E-01, ...
    0.2018069481193246E-01, ...
    0.3738944032940469E-01, ...
    0.3738944032940469E-01, ...
    0.3738944032940469E-01, ...
    0.3738944032940469E-01, ...
    0.9821701539315209E-01, ...
    0.9821701539315209E-01, ...
    0.9821701539315209E-01, ...
    0.9821701539315209E-01, ...
    0.3844110871724747E-01, ...
    0.3844110871724747E-01, ...
    0.3844110871724747E-01, ...
    0.3844110871724747E-01, ...
    0.7127049386881731E-01, ...
    0.7127049386881731E-01, ...
    0.7127049386881731E-01, ...
    0.7127049386881731E-01, ...
    0.6966749913838975E-01, ...
    0.6966749913838975E-01, ...
    0.6966749913838975E-01, ...
    0.6966749913838975E-01, ...
    0.7715964130310782E-01, ...
    0.7715964130310782E-01, ...
    0.7715964130310782E-01, ...
    0.7715964130310782E-01, ...
    0.4598470092336809E-01, ...
    0.4598470092336809E-01, ...
    0.4598470092336809E-01, ...
    0.4598470092336809E-01, ...
    0.9562983140360957E-01, ...
    0.9562983140360957E-01, ...
    0.9562983140360957E-01, ...
    0.9562983140360957E-01 ];

  x(1,1:n) = xs(1:n);
  x(2,1:n) = ys(1:n);
  w(1:n) = ws(1:n);

  return
end
