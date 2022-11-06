function [ x, w ] = laguerre_set ( n )

%*****************************************************************************80
%
%% laguerre_set() sets abscissas and weights for Laguerre quadrature.
%
%  Discussion:
%
%    The abscissas are the zeroes of the Laguerre polynomial L(N)(X).
%
%    The integral:
%
%      Integral ( 0 <= X < +oo ) exp ( -X ) * F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * f ( X(I) )
%
%    The integral:
%
%      Integral ( 0 <= X < +oo ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * exp ( X(I) ) * f ( X(I) )
%
%    Mathematica can numerically estimate the abscissas for the
%    n-th order polynomial to p digits of precision by the command:
%
%      NSolve [ LaguerreL[n,x] == 0, x, p ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 April 2010
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz, Irene Stegun,
%    Handbook of Mathematical Functions,
%    National Bureau of Standards, 1964,
%    ISBN: 0-486-61272-4,
%    LC: QA47.A34.
%
%    Vladimir Krylov,
%    Approximate Calculation of Integrals,
%    Dover, 2006,
%    ISBN: 0486445798,
%    LC: QA311.K713.
%
%    Arthur Stroud, Don Secrest,
%    Gaussian Quadrature Formulas,
%    Prentice Hall, 1966,
%    LC: QA299.4G3S7.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Cambridge University Press, 1999,
%    ISBN: 0-521-64314-7,
%    LC: QA76.95.W65.
%
%    Daniel Zwillinger, editor,
%    CRC Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996,
%    ISBN: 0-8493-2479-3,
%    LC: QA47.M315.
%
%  Input:
%
%    integer N, the order.
%    N must be between 1 and 10.
%
%  Output:
%
%    real X(N), the abscissas.
%
%    real W(N), the weights.
%
  x = zeros ( n, 1 );
  w = zeros ( n, 1 );

  if ( n == 1 )

    x(1) =  1.00000000000000000000000000000E+00;

    w(1) =  1.00000000000000000000000000000E+00;

  elseif ( n == 2 )

    x(1) = 0.585786437626904951198311275790E+00;
    x(2) = 3.41421356237309504880168872421E+00;

    w(1) = 0.85355339059327376220042218105E+00;
    w(2) = 0.146446609406726237799577818948E+00;

  elseif ( n == 3 )

    x(1) = 0.415774556783479083311533873128E+00;
    x(2) = 2.29428036027904171982205036136E+00;
    x(3) = 6.28994508293747919686641576551E+00;

    w(1) = 0.71109300992917301544959019114E+00;
    w(2) = 0.27851773356924084880144488846E+00;
    w(3) = 0.010389256501586135748964920401E+00;

  elseif ( n == 4 )

    x(1) = 0.322547689619392311800361459104E+00;
    x(2) = 1.74576110115834657568681671252E+00;
    x(3) = 4.53662029692112798327928538496E+00;
    x(4) = 9.39507091230113312923353644342E+00;

    w(1) = 0.60315410434163360163596602382E+00;
    w(2) = 0.35741869243779968664149201746E+00;
    w(3) = 0.03888790851500538427243816816E+00;
    w(4) = 0.0005392947055613274501037905676E+00;

  elseif ( n == 5 )

    x(1) = 0.263560319718140910203061943361E+00;
    x(2) = 1.41340305910651679221840798019E+00;
    x(3) = 3.59642577104072208122318658878E+00;
    x(4) = 7.08581000585883755692212418111E+00;
    x(5) = 12.6408008442757826594332193066E+00;

    w(1) = 0.52175561058280865247586092879E+00;
    w(2) = 0.3986668110831759274541333481E+00;
    w(3) = 0.0759424496817075953876533114E+00;
    w(4) = 0.00361175867992204845446126257E+00;
    w(5) = 0.00002336997238577622789114908455E+00;

  elseif ( n == 6 )

    x(1) = 0.222846604179260689464354826787E+00;
    x(2) = 1.18893210167262303074315092194E+00;
    x(3) = 2.99273632605931407769132528451E+00;
    x(4) = 5.77514356910451050183983036943E+00;
    x(5) = 9.83746741838258991771554702994E+00;
    x(6) = 15.9828739806017017825457915674E+00;

    w(1) = 0.45896467394996359356828487771E+00;
    w(2) = 0.4170008307721209941133775662E+00;
    w(3) = 0.1133733820740449757387061851E+00;
    w(4) = 0.01039919745314907489891330285E+00;
    w(5) = 0.000261017202814932059479242860E+00;
    w(6) = 8.98547906429621238825292053E-07;

  elseif ( n == 7 )

    x(1) = 0.193043676560362413838247885004E+00;
    x(2) = 1.02666489533919195034519944317E+00;
    x(3) = 2.56787674495074620690778622666E+00;
    x(4) = 4.90035308452648456810171437810E+00;
    x(5) = 8.18215344456286079108182755123E+00;
    x(6) = 12.7341802917978137580126424582E+00;
    x(7) = 19.3957278622625403117125820576E+00;

    w(1) = 0.40931895170127390213043288002E+00;
    w(2) = 0.4218312778617197799292810054E+00;
    w(3) = 0.1471263486575052783953741846E+00;
    w(4) = 0.0206335144687169398657056150E+00;
    w(5) = 0.00107401014328074552213195963E+00;
    w(6) = 0.0000158654643485642012687326223E+00;
    w(7) = 3.17031547899558056227132215E-08;

  elseif ( n == 8 )

    x(1) = 0.170279632305100999788861856608E+00;
    x(2) = 0.903701776799379912186020223555E+00;
    x(3) = 2.25108662986613068930711836697E+00;
    x(4) = 4.26670017028765879364942182690E+00;
    x(5) = 7.04590540239346569727932548212E+00;
    x(6) = 10.7585160101809952240599567880E+00;
    x(7) = 15.7406786412780045780287611584E+00;
    x(8) = 22.8631317368892641057005342974E+00;

    w(1) = 0.36918858934163752992058283938E+00;
    w(2) = 0.4187867808143429560769785813E+00;
    w(3) = 0.175794986637171805699659867E+00;
    w(4) = 0.033343492261215651522132535E+00;
    w(5) = 0.0027945362352256725249389241E+00;
    w(6) = 0.00009076508773358213104238501E+00;
    w(7) = 8.4857467162725315448680183E-07;
    w(8) = 1.04800117487151038161508854E-09;

  elseif ( n == 9 )

    x(1) = 0.152322227731808247428107073127E+00;
    x(2) = 0.807220022742255847741419210952E+00;
    x(3) = 2.00513515561934712298303324701E+00;
    x(4) = 3.78347397333123299167540609364E+00;
    x(5) = 6.20495677787661260697353521006E+00;
    x(6) = 9.37298525168757620180971073215E+00;
    x(7) = 13.4662369110920935710978818397E+00;
    x(8) = 18.8335977889916966141498992996E+00;
    x(9) = 26.3740718909273767961410072937E+00;

    w(1) = 0.336126421797962519673467717606E+00;
    w(2) = 0.411213980423984387309146942793E+00;
    w(3) = 0.199287525370885580860575607212E+00;
    w(4) = 0.0474605627656515992621163600479E+00;
    w(5) = 0.00559962661079458317700419900556E+00;
    w(6) = 0.000305249767093210566305412824291E+00;
    w(7) = 6.59212302607535239225572284875E-06;
    w(8) = 4.1107693303495484429024104033E-08;
    w(9) = 3.29087403035070757646681380323E-11;

  elseif ( n == 10 )

    x(1) = 0.137793470540492430830772505653E+00;
    x(2) = 0.729454549503170498160373121676E+00;
    x(3) = 1.80834290174031604823292007575E+00;
    x(4) = 3.40143369785489951448253222141E+00;
    x(5) = 5.55249614006380363241755848687E+00;
    x(6) = 8.33015274676449670023876719727E+00;
    x(7) = 11.8437858379000655649185389191E+00;
    x(8) = 16.2792578313781020995326539358E+00;
    x(9) = 21.9965858119807619512770901956E+00;
    x(10) = 29.9206970122738915599087933408E+00;

    w(1) = 0.30844111576502014154747083468E+00;
    w(2) = 0.4011199291552735515157803099E+00;
    w(3) = 0.218068287611809421588648523E+00;
    w(4) = 0.062087456098677747392902129E+00;
    w(5) = 0.009501516975181100553839072E+00;
    w(6) = 0.0007530083885875387754559644E+00;
    w(7) = 0.00002825923349599565567422564E+00;
    w(8) = 4.249313984962686372586577E-07;
    w(9) = 1.839564823979630780921535E-09;
    w(10) = 9.911827219609008558377547E-13;
 
  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'LAGUERRE_SET - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of N = %d\n', n );
    fprintf ( 1, '  Legal values are 1 to 10.\n' );
    error ( 'LAGUERRE_SET - Fatal error!' );

  end

  return
end

