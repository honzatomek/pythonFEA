function [ x, w ] = clenshaw_curtis_set ( n )

%*****************************************************************************80
%
%% clenshaw_curtis_set() sets a Clenshaw-Curtis quadrature rule.
%
%  Discussion:
%
%    The integral:
%
%      Integral ( -1 <= X <= 1 ) F(X) dX
%
%    The quadrature rule:
%
%      Sum ( 1 <= I <= N ) W(I) * F ( X(I) )
%
%    The abscissas for the rule of order N can be regarded 
%    as the cosines of equally spaced angles between 180 and 0 degrees:
%
%      X(I) = cos ( ( I - 1 ) * PI / ( N - 1 ) )
%
%    except for the basic case N = 1, when
%
%      X(1) = 0.
%
%    A Clenshaw-Curtis rule that uses N points will integrate
%    exactly all polynomials of degrees 0 through N-1.  If N
%    is odd, then by symmetry the polynomial of degree N will
%    also be integrated exactly.
%
%    If the value of N is increased in a sensible way, then
%    the new set of abscissas will include the old ones.  One such
%    sequence would be N(K) = 2*K+1 for K = 0, 1, 2, ...
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 May 2007
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Charles Clenshaw, Alan Curtis,
%    A Method for Numerical Integration on an Automatic Computer,
%    Numerische Mathematik,
%    Volume 2, Number 1, December 1960, pages 197-205.
%
%  Input:
%
%    integer N, the order.
%    N must be between 1 and 17, 33, 65 or 129.
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

    x(1) =  0.00000000000000000000;
    w(1) =  2.00000000000000000000;

  elseif ( n == 2 )

    x(1) = -1.00000000000000000000;
    x(2) =  1.00000000000000000000;

    w(1) =  1.00000000000000000000;
    w(2) =  1.00000000000000000000;

  elseif ( n == 3 )

    x(1) = -1.00000000000000000000;
    x(2) =  0.00000000000000000000;
    x(3) =  1.00000000000000000000;

    w(1) =  0.33333333333333333333;
    w(2) =  1.33333333333333333333;
    w(3) =  0.33333333333333333333;

  elseif ( n == 4 )

    x(1) = -1.00000000000000000000;
    x(2) = -0.50000000000000000000;
    x(3) =  0.50000000000000000000;
    x(4) =  1.00000000000000000000;

    w(1) =  0.11111111111111111111;
    w(2) =  0.88888888888888888889;
    w(3) =  0.88888888888888888889;
    w(4) =  0.11111111111111111111;

  elseif ( n == 5 )

    x(1) = -1.00000000000000000000;
    x(2) = -0.70710678118654752440;
    x(3) =  0.00000000000000000000;
    x(4) =  0.70710678118654752440;
    x(5) =  1.00000000000000000000;

    w(1) =  0.06666666666666666667;
    w(2) =  0.53333333333333333333;
    w(3) =  0.80000000000000000000;
    w(4) =  0.53333333333333333333;
    w(5) =  0.06666666666666666667;

  elseif ( n == 6 )

    x(1) = -1.00000000000000000000;
    x(2) = -0.80901699437494742410;
    x(3) = -0.30901699437494742410;
    x(4) =  0.30901699437494742410;
    x(5) =  0.80901699437493732410;
    x(6) =  1.00000000000000000000;

    w(1) =  0.04000000000000000000;
    w(2) =  0.36074304120001121619;
    w(3) =  0.59925695879998878381;
    w(4) =  0.59925695879998878381;
    w(5) =  0.36074304120001121619;
    w(6) =  0.04000000000000000000;

  elseif ( n == 7 )

    x(1) = -1.00000000000000000000;
    x(2) = -0.86602540378443864676;
    x(3) = -0.50000000000000000000;
    x(4) =  0.00000000000000000000;
    x(5) =  0.50000000000000000000;
    x(6) =  0.86602540378443864676;
    x(7) =  1.00000000000000000000;
   
    w(1) =  0.02857142857142857143;
    w(2) =  0.25396825396825396825;
    w(3) =  0.45714285714285714286;
    w(4) =  0.52063492063492063492;
    w(5) =  0.45714285714285714286;
    w(6) =  0.25396825396825396825;
    w(7) =  0.02857142857142857143;

  elseif ( n == 8 )
   
    x(1) = -1.00000000000000000000;
    x(2) = -0.90096886790241912624;
    x(3) = -0.62348980185873353053;
    x(4) = -0.22252093395631440429;
    x(5) =  0.22252093395631440429;
    x(6) =  0.62348980185873353053;
    x(7) =  0.90096886790241910624;
    x(8) =  1.00000000000000000000;
   
    w(1) =  0.02040816326530612245;
    w(2) =  0.19014100721820835178;
    w(3) =  0.35224242371815911533;
    w(4) =  0.43720840579832641044;
    w(5) =  0.43720840579832641044;
    w(6) =  0.35224242371815911533;
    w(7) =  0.19014100721820835178;
    w(8) =  0.02040816326530612245;

  elseif ( n == 9 )
 
    x(1) = -1.00000000000000000000;
    x(2) = -0.92387953251128675613;
    x(3) = -0.70710678118654752440;
    x(4) = -0.38268343236508977173;
    x(5) =  0.00000000000000000000;
    x(6) =  0.38268343236508977173;
    x(7) =  0.70710678118654752440;
    x(8) =  0.92387953251128675613;
    x(9) =  1.00000000000000000000;

    w(1) =  0.01587301587301587302;
    w(2) =  0.14621864921601815501;
    w(3) =  0.27936507936507936508;
    w(4) =  0.36171785872048978150;
    w(5) =  0.39365079365079365079;
    w(6) =  0.36171785872048978150;
    w(7) =  0.27936507936507936508;
    w(8) =  0.14621864921601815501;
    w(9) =  0.01587301587301587302;

  elseif ( n == 10 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.93969262078590838405;
    x(3)  = -0.76604444311897903520;
    x(4)  = -0.50000000000000000000;
    x(5)  = -0.17364817766693034885;
    x(6)  =  0.17364817766693034885;
    x(7)  =  0.50000000000000000000;
    x(8)  =  0.76604444311897903520;
    x(9)  =  0.93969262078590838405;
    x(10) =  1.00000000000000000000;

    w(1)  =  0.01234567901234567901;
    w(2)  =  0.11656745657203712296;
    w(3)  =  0.22528432333810440813;
    w(4)  =  0.30194003527336860670;
    w(5)  =  0.34386250580414418320;
    w(6)  =  0.34386250580414418320;
    w(7)  =  0.30194003527336860670;
    w(8)  =  0.22528432333810440813;
    w(9)  =  0.11656745657203712296;
    w(10) =  0.01234567901234567901;

  elseif ( n == 11 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.95105651629515357212;
    x(3)  = -0.80901699437494742410;
    x(4)  = -0.58778525229247312917;
    x(5)  = -0.30901699437494742410;
    x(6)  =  0.00000000000000000000;
    x(7)  =  0.30901699437494742410;
    x(8)  =  0.58778525229247312917;
    x(9)  =  0.80901699437494742410;
    x(10) =  0.95105651629515357212;
    x(11) =  1.00000000000000000000;

    w(1)  =  0.01010101010101010101;
    w(2)  =  0.09457905488370156116;
    w(3)  =  0.18563521442424776529;
    w(4)  =  0.25358833328368660623;
    w(5)  =  0.29921327042423708320;
    w(6)  =  0.31376623376623376623;
    w(7)  =  0.29921327042423708320;
    w(8)  =  0.25358833328368660623;
    w(9)  =  0.18563521442424776529;
    w(10) =  0.09457905488370156116;
    w(11) =  0.01010101010101010101;

  elseif ( n == 12 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.95949297361449738989;
    x(3)  = -0.84125353283118116886;
    x(4)  = -0.65486073394528506406;
    x(5)  = -0.41541501300188642553;
    x(6)  = -0.14231483827328514044;
    x(7)  =  0.14231483827328514044;
    x(8)  =  0.41541501300188642553;
    x(9)  =  0.65486073394528506406;
    x(10) =  0.84125353283118116886;
    x(11) =  0.95949297361449738989;
    x(12) =  1.00000000000000000000;

    w(1)  =  0.00826446280991735537;
    w(2)  =  0.07856015374620000543;
    w(3)  =  0.15504045508256136552;
    w(4)  =  0.21556254600086858099;
    w(5)  =  0.25991734106691617602;
    w(6)  =  0.28265504129353651666;
    w(7)  =  0.28265504129353651666;
    w(8)  =  0.25991734106691617602;
    w(9)  =  0.21556254600086858099;
    w(10) =  0.15504045508256136552;
    w(11) =  0.07856015374620000543;
    w(12) =  0.00826446280991735537;

  elseif ( n == 13 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.96592582628906828675;
    x(3)  = -0.86602540378443864676;
    x(4)  = -0.70710678118654752440;
    x(5)  = -0.50000000000000000000;
    x(6)  = -0.25881904510252076235;
    x(7)  =  0.00000000000000000000;
    x(8)  =  0.25881904510252076235;
    x(9)  =  0.50000000000000000000;
    x(10) =  0.70710678118654752440;
    x(11) =  0.86602540378443864676;
    x(12) =  0.96592582628906828675;
    x(13) =  1.00000000000000000000;

    w(1)  =  0.00699300699300699301;
    w(2)  =  0.06605742495207439452;
    w(3)  =  0.13154253154253154253;
    w(4)  =  0.18476338476338476338;
    w(5)  =  0.22697302697302697303;
    w(6)  =  0.25267569378104433860;
    w(7)  =  0.26198986198986198986;
    w(8)  =  0.25267569378104433860;
    w(9)  =  0.22697302697302697303;
    w(10) =  0.18476338476338476338;
    w(11) =  0.13154253154253154253;
    w(12) =  0.06605742495207439452;
    w(13) =  0.00699300699300699301;

  elseif ( n == 14 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.97094181742605202716;
    x(3)  = -0.88545602565320989590;
    x(4)  = -0.74851074817110109863;
    x(5)  = -0.56806474673115580251;
    x(6)  = -0.35460488704253562597;
    x(7)  = -0.12053668025532305335;
    x(8)  =  0.12053668025532305335;
    x(9)  =  0.35460488704253562597;
    x(10) =  0.56806474673115580251;
    x(11) =  0.74851074817110109863;
    x(12) =  0.88545602565320989590;
    x(13) =  0.97094181742605202716;
    x(14) =  1.00000000000000000000;

    w(1)  =  0.00591715976331360947;
    w(2)  =  0.05646531376341444627;
    w(3)  =  0.11276867248985655881;
    w(4)  =  0.16003802611671868523;
    w(5)  =  0.19899241036578321848;
    w(6)  =  0.22590304977856444935;
    w(7)  =  0.23991536772234903239;
    w(8)  =  0.23991536772234903239;
    w(9)  =  0.22590304977856444935;
    w(10) =  0.19899241036578321848;
    w(11) =  0.16003802611671868523;
    w(12) =  0.11276867248985655881;
    w(13) =  0.05646531376341444627;
    w(14) =  0.00591715976331360947;

  elseif ( n == 15 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.97492791218182360702;
    x(3)  = -0.90096886790241912624;
    x(4)  = -0.78183148246802980871;
    x(5)  = -0.62348980185873353053;
    x(6)  = -0.43388373911755812048;
    x(7)  = -0.22252093395631440429;
    x(8)  =  0.00000000000000000000;
    x(9)  =  0.22252093395631440429;
    x(10) =  0.43388373911755812048;
    x(11) =  0.62348980185873353053;
    x(12) =  0.78183148246802980871;
    x(13) =  0.90096886790241912624;
    x(14) =  0.97492791218182360702;
    x(15) =  1.00000000000000000000;

    w(1)  =  0.00512820512820512821;
    w(2)  =  0.04869938729508823855;
    w(3)  =  0.09782039167605215913;
    w(4)  =  0.13966507849560431803;
    w(5)  =  0.17560578900106674677;
    w(6)  =  0.20205146748238357364;
    w(7)  =  0.21888151163057340180;
    w(8)  =  0.22429633858205286777;
    w(9)  =  0.21888151163057340180;
    w(10) =  0.20205146748238357364;
    w(11) =  0.17560578900106674677;
    w(12) =  0.13966507849560431803;
    w(13) =  0.09782039167605215913;
    w(14) =  0.04869938729508823855;
    w(15) =  0.00512820512820512821;

  elseif ( n == 16 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.97814760073380563793;
    x(3)  = -0.91354545764260089550;
    x(4)  = -0.80901699437494742410;
    x(5)  = -0.66913060635885821383;
    x(6)  = -0.50000000000000000000;
    x(7)  = -0.30901699437494742410;
    x(8)  = -0.10452846326765347140;
    x(9)  =  0.10452846326765347140;
    x(10) =  0.30901699437494742410;
    x(11) =  0.50000000000000000000;
    x(12) =  0.66913060635885821383;
    x(13) =  0.80901699437494742410;
    x(14) =  0.91354545764260089550;
    x(15) =  0.97814760073380563793;
    x(16) =  1.00000000000000000000;

    w(1)  =  0.00444444444444444444;
    w(2)  =  0.04251476624752508988;
    w(3)  =  0.08553884025933288291;
    w(4)  =  0.12294010082849361533;
    w(5)  =  0.15573317603967369176;
    w(6)  =  0.18132978132978132978;
    w(7)  =  0.19921478132638853955;
    w(8)  =  0.20828410952436040635;
    w(9)  =  0.20828410952436040635;
    w(10) =  0.19921478132638853955;
    w(11) =  0.18132978132978132978;
    w(12) =  0.15573317603967369176;
    w(13) =  0.12294010082849361533;
    w(14) =  0.08553884025933288291;
    w(15) =  0.04251476624752508988;
    w(16) =  0.00444444444444444444;

  elseif ( n == 17 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.98078528040323044913;
    x(3)  = -0.92387953251128675613;
    x(4)  = -0.83146961230254523708;
    x(5)  = -0.70710678118654752440;
    x(6)  = -0.55557023301960222474;
    x(7)  = -0.38268343236508977173;
    x(8)  = -0.19509032201612826785;
    x(9)  =  0.00000000000000000000;
    x(10) =  0.19509032201612826785;
    x(11) =  0.38268343236508977173;
    x(12) =  0.55557023301960222474;
    x(13) =  0.70710678118654752440;
    x(14) =  0.83146961230254523708;
    x(15) =  0.92387953251128675613;
    x(16) =  0.98078528040323044913;
    x(17) =  1.00000000000000000000;

    w(1)  =  0.00392156862745098039;
    w(2)  =  0.03736870283720561032;
    w(3)  =  0.07548233154315183441;
    w(4)  =  0.10890555258189093044;
    w(5)  =  0.13895646836823307412;
    w(6)  =  0.16317266428170330256;
    w(7)  =  0.18147378423649335700;
    w(8)  =  0.19251386461292564687;
    w(9)  =  0.19641012582189052777;
    w(10) =  0.19251386461292564687;
    w(11) =  0.18147378423649335700;
    w(12) =  0.16317266428170330256;
    w(13) =  0.13895646836823307412;
    w(14) =  0.10890555258189093044;
    w(15) =  0.07548233154315183441;
    w(16) =  0.03736870283720561032;
    w(17) =  0.00392156862745098039;

  elseif ( n == 33 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.99518472667219688624;
    x(3)  = -0.98078528040323044913;
    x(4)  = -0.95694033573220886494;
    x(5)  = -0.92387953251128675613;
    x(6)  = -0.88192126434835502971;
    x(7)  = -0.83146961230254523708;
    x(8)  = -0.77301045336273696081;
    x(9)  = -0.70710678118654752440;
    x(10) = -0.63439328416364549822;
    x(11) = -0.55557023301960222474;
    x(12) = -0.47139673682599764856;
    x(13) = -0.38268343236508977173;
    x(14) = -0.29028467725446236764;
    x(15) = -0.19509032201612826785;
    x(16) = -0.098017140329560601994;
    x(17) =  0.000000000000000000000;
    x(18) =  0.098017140329560601994;
    x(19) =  0.19509032201612826785;
    x(20) =  0.29028467725446236764;
    x(21) =  0.38268343236508977173;
    x(22) =  0.47139673682599764856;
    x(23) =  0.55557023301960222474;
    x(24) =  0.63439328416364549822;
    x(25) =  0.70710678118654752440;
    x(26) =  0.77301045336273696081;
    x(27) =  0.83146961230254523708;
    x(28) =  0.88192126434835502971;
    x(29) =  0.92387953251128675613;
    x(30) =  0.95694033573220886494;
    x(31) =  0.98078528040323044913;
    x(32) =  0.99518472667219688624;
    x(33) =  1.00000000000000000000;

    w(1)  =  0.00097751710654936461;
    w(2)  =  0.00939319796295501470;
    w(3)  =  0.01923424513268114918;
    w(4)  =  0.02845791667723369009;
    w(5)  =  0.03759434191404720602;
    w(6)  =  0.04626276283775174949;
    w(7)  =  0.05455501630398031044;
    w(8)  =  0.06227210954529400455;
    w(9)  =  0.06942757563043545090;
    w(10) =  0.07588380044138847048;
    w(11) =  0.08163481765493851023;
    w(12) =  0.08657753844182743544;
    w(13) =  0.09070611286772099874;
    w(14) =  0.09394324443876873573;
    w(15) =  0.09629232594548817919;
    w(16) =  0.09769818820805558182;
    w(17) =  0.09817857778176829677;
    w(18) =  0.09769818820805558182;
    w(19) =  0.09629232594548817919;
    w(20) =  0.09394324443876873573;
    w(21) =  0.09070611286772099874;
    w(22) =  0.08657753844182743544;
    w(23) =  0.08163481765493851023;
    w(24) =  0.07588380044138847048;
    w(25) =  0.06942757563043545090;
    w(26) =  0.06227210954529400455;
    w(27) =  0.05455501630398031044;
    w(28) =  0.04626276283775174949;
    w(29) =  0.03759434191404720602;
    w(30) =  0.02845791667723369009;
    w(31) =  0.01923424513268114918;
    w(32) =  0.00939319796295501470;
    w(33) =  0.00097751710654936461;

  elseif ( n == 65 )

    x(1)  = -1.00000000000000000000;
    x(2)  = -0.99879545620517239271;
    x(3)  = -0.99518472667219688624;
    x(4)  = -0.98917650996478097345;
    x(5)  = -0.98078528040323044913;
    x(6)  = -0.97003125319454399260;
    x(7)  = -0.95694033573220886494;
    x(8)  = -0.94154406518302077841;
    x(9)  = -0.92387953251128675613;
    x(10) = -0.90398929312344333159;
    x(11) = -0.88192126434835502971;
    x(12) = -0.85772861000027206990;
    x(13) = -0.83146961230254523708;
    x(14) = -0.80320753148064490981;
    x(15) = -0.77301045336273696081;
    x(16) = -0.74095112535495909118;
    x(17) = -0.70710678118654752440;
    x(18) = -0.67155895484701840063;
    x(19) = -0.63439328416364549822;
    x(20) = -0.59569930449243334347;
    x(21) = -0.55557023301960222474;
    x(22) = -0.51410274419322172659;
    x(23) = -0.47139673682599764856;
    x(24) = -0.42755509343028209432;
    x(25) = -0.38268343236508977173;
    x(26) = -0.33688985339222005069;
    x(27) = -0.29028467725446236764;
    x(28) = -0.24298017990326388995;
    x(29) = -0.19509032201612826785;
    x(30) = -0.14673047445536175166;
    x(31) = -0.098017140329560601994;
    x(32) = -0.049067674327418014255;
    x(33) =  0.000000000000000000000;
    x(34) =  0.049067674327418014255;
    x(35) =  0.098017140329560601994;
    x(36) =  0.14673047445536175166;
    x(37) =  0.19509032201612826785;
    x(38) =  0.24298017990326388995;
    x(39) =  0.29028467725446236764;
    x(40) =  0.33688985339222005069;
    x(41) =  0.38268343236508977173;
    x(42) =  0.42755509343028209432;
    x(43) =  0.47139673682599764856;
    x(44) =  0.51410274419322172659;
    x(45) =  0.55557023301960222474;
    x(46) =  0.59569930449243334347;
    x(47) =  0.63439328416364549822;
    x(48) =  0.67155895484701840063;
    x(49) =  0.70710678118654752440;
    x(50) =  0.74095112535495909118;
    x(51) =  0.77301045336273696081;
    x(52) =  0.80320753148064490981;
    x(53) =  0.83146961230254523708;
    x(54) =  0.85772861000027206990;
    x(55) =  0.88192126434835502971;
    x(56) =  0.90398929312344333159;
    x(57) =  0.92387953251128675613;
    x(58) =  0.94154406518302077841;
    x(59) =  0.95694033573220886494;
    x(60) =  0.97003125319454399260;
    x(61) =  0.98078528040323044913;
    x(62) =  0.98917650996478097345;
    x(63) =  0.99518472667219688624;
    x(64) =  0.99879545620517239271;
    x(65) =  1.00000000000000000000;

    w(1)  =  0.00024420024420024420;
    w(2)  =  0.00235149067531170332;
    w(3)  =  0.00483146544879091264;
    w(4)  =  0.00719269316173611402;
    w(5)  =  0.00958233879528379039;
    w(6)  =  0.01192339471421277160;
    w(7)  =  0.01425206043235199679;
    w(8)  =  0.01653498765728958965;
    w(9)  =  0.01878652974179578354;
    w(10) =  0.02098627442973743378;
    w(11) =  0.02314069493435819848;
    w(12) =  0.02523506498175476590;
    w(13) =  0.02727225714146838686;
    w(14) =  0.02924065319746833770;
    w(15) =  0.03114129710406762447;
    w(16) =  0.03296454656997632997;
    w(17) =  0.03471049818092511427;
    w(18) =  0.03637092028663918309;
    w(19) =  0.03794545992128481711;
    w(20) =  0.03942698871295609976;
    w(21) =  0.04081501340035783384;
    w(22) =  0.04210333111141810203;
    w(23) =  0.04329151496169082935;
    w(24) =  0.04437417923925731580;
    w(25) =  0.04535110955166067221;
    w(26) =  0.04621766751092557684;
    w(27) =  0.04697395904661414870;
    w(28) =  0.04761604458525019296;
    w(29) =  0.04814443257251220341;
    w(30) =  0.04855584485714105274;
    w(31) =  0.04885125664306609371;
    w(32) =  0.04902801843102555294;
    w(33) =  0.04908762351494245585;
    w(34) =  0.04902801843102555294;
    w(35) =  0.04885125664306609371;
    w(36) =  0.04855584485714105274;
    w(37) =  0.04814443257251220341;
    w(38) =  0.04761604458525019296;
    w(39) =  0.04697395904661414870;
    w(40) =  0.04621766751092557684;
    w(41) =  0.04535110955166067221;
    w(42) =  0.04437417923925731580;
    w(43) =  0.04329151496169082935;
    w(44) =  0.04210333111141810203;
    w(45) =  0.04081501340035783384;
    w(46) =  0.03942698871295609976;
    w(47) =  0.03794545992128481711;
    w(48) =  0.03637092028663918309;
    w(49) =  0.03471049818092511427;
    w(50) =  0.03296454656997632997;
    w(51) =  0.03114129710406762447;
    w(52) =  0.02924065319746833770;
    w(53) =  0.02727225714146838686;
    w(54) =  0.02523506498175476590;
    w(55) =  0.02314069493435819848;
    w(56) =  0.02098627442973743378;
    w(57) =  0.01878652974179578354;
    w(58) =  0.01653498765728958965;
    w(59) =  0.01425206043235199679;
    w(60) =  0.01192339471421277160;
    w(61) =  0.00958233879528379039;
    w(62) =  0.00719269316173611402;
    w(63) =  0.00483146544879091264;
    w(64) =  0.00235149067531170332;
    w(65) =  0.00024420024420024420;

  elseif ( n == 129 )

    x(1)   = -1.00000000000000000000;
    x(2)   = -0.99969881869620422012;
    x(3)   = -0.99879545620517239271;
    x(4)   = -0.99729045667869021614;
    x(5)   = -0.99518472667219688624;
    x(6)   = -0.99247953459870999816;
    x(7)   = -0.98917650996478097345;
    x(8)   = -0.98527764238894124477;
    x(9)   = -0.98078528040323044913;
    x(10)  = -0.97570213003852854446;
    x(11)  = -0.97003125319454399260;
    x(12)  = -0.96377606579543986669;
    x(13)  = -0.95694033573220886494;
    x(14)  = -0.94952818059303666720;
    x(15)  = -0.94154406518302077841;
    x(16)  = -0.93299279883473888771;
    x(17)  = -0.92387953251128675613;
    x(18)  = -0.91420975570353065464;
    x(19)  = -0.90398929312344333159;
    x(20)  = -0.89322430119551532034;
    x(21)  = -0.88192126434835502971;
    x(22)  = -0.87008699110871141865;
    x(23)  = -0.85772861000027206990;
    x(24)  = -0.84485356524970707326;
    x(25)  = -0.83146961230254523708;
    x(26)  = -0.81758481315158369650;
    x(27)  = -0.80320753148064490981;
    x(28)  = -0.78834642762660626201;
    x(29)  = -0.77301045336273696081;
    x(30)  = -0.75720884650648454758;
    x(31)  = -0.74095112535495909118;
    x(32)  = -0.72424708295146692094;
    x(33)  = -0.70710678118654752440;
    x(34)  = -0.68954054473706692462;
    x(35)  = -0.67155895484701840063;
    x(36)  = -0.65317284295377676408;
    x(37)  = -0.63439328416364549822;
    x(38)  = -0.61523159058062684548;
    x(39)  = -0.59569930449243334347;
    x(40)  = -0.57580819141784530075;
    x(41)  = -0.55557023301960222474;
    x(42)  = -0.53499761988709721066;
    x(43)  = -0.51410274419322172659;
    x(44)  = -0.49289819222978403687;
    x(45)  = -0.47139673682599764856;
    x(46)  = -0.44961132965460660005;
    x(47)  = -0.42755509343028209432;
    x(48)  = -0.40524131400498987091;
    x(49)  = -0.38268343236508977173;
    x(50)  = -0.35989503653498814878;
    x(51)  = -0.33688985339222005069;
    x(52)  = -0.31368174039889147666;
    x(53)  = -0.29028467725446236764;
    x(54)  = -0.26671275747489838633;
    x(55)  = -0.24298017990326388995;
    x(56)  = -0.21910124015686979723;
    x(57)  = -0.19509032201612826785;
    x(58)  = -0.17096188876030122636;
    x(59)  = -0.14673047445536175166;
    x(60)  = -0.12241067519921619850;
    x(61)  = -0.098017140329560601994;
    x(62)  = -0.073564563599667423529;
    x(63)  = -0.049067674327418014255;
    x(64)  = -0.024541228522912288032;
    x(65)  =  0.00000000000000000000;
    x(66)  =  0.024541228522912288032;
    x(67)  =  0.049067674327418014255;
    x(68)  =  0.073564563599667423529;
    x(69)  =  0.098017140329560601994;
    x(70)  =  0.12241067519921619850;
    x(71)  =  0.14673047445536175166;
    x(72)  =  0.17096188876030122636;
    x(73)  =  0.19509032201612826785;
    x(74)  =  0.21910124015686979723;
    x(75)  =  0.24298017990326388995;
    x(76)  =  0.26671275747489838633;
    x(77)  =  0.29028467725446236764;
    x(78)  =  0.31368174039889147666;
    x(79)  =  0.33688985339222005069;
    x(80)  =  0.35989503653498814878;
    x(81)  =  0.38268343236508977173;
    x(82)  =  0.40524131400498987091;
    x(83)  =  0.42755509343028209432;
    x(84)  =  0.44961132965460660005;
    x(85)  =  0.47139673682599764856;
    x(86)  =  0.49289819222978403687;
    x(87)  =  0.51410274419322172659;
    x(88)  =  0.53499761988709721066;
    x(89)  =  0.55557023301960222474;
    x(90)  =  0.57580819141784530075;
    x(91)  =  0.59569930449243334347;
    x(92)  =  0.61523159058062684548;
    x(93)  =  0.63439328416364549822;
    x(94)  =  0.65317284295377676408;
    x(95)  =  0.67155895484701840063;
    x(96)  =  0.68954054473706692462;
    x(97)  =  0.70710678118654752440;
    x(98)  =  0.72424708295146692094;
    x(99)  =  0.74095112535495909118;
    x(100) =  0.75720884650648454758;
    x(101) =  0.77301045336273696081;
    x(102) =  0.78834642762660626201;
    x(103) =  0.80320753148064490981;
    x(104) =  0.81758481315158369650;
    x(105) =  0.83146961230254523708;
    x(106) =  0.84485356524970707326;
    x(107) =  0.85772861000027206990;
    x(108) =  0.87008699110871141865;
    x(109) =  0.88192126434835502971;
    x(110) =  0.89322430119551532034;
    x(111) =  0.90398929312344333159;
    x(112) =  0.91420975570353065464;
    x(113) =  0.92387953251128675613;
    x(114) =  0.93299279883473888771;
    x(115) =  0.94154406518302077841;
    x(116) =  0.94952818059303666720;
    x(117) =  0.95694033573220886494;
    x(118) =  0.96377606579543986669;
    x(119) =  0.97003125319454399260;
    x(120) =  0.97570213003852854446;
    x(121) =  0.98078528040323044913;
    x(122) =  0.98527764238894124477;
    x(123) =  0.98917650996478097345;
    x(124) =  0.99247953459870999816;
    x(125) =  0.99518472667219688624;
    x(126) =  0.99729045667869021614;
    x(127) =  0.99879545620517239271;
    x(128) =  0.99969881869620422012;
    x(129) =  1.00000000000000000000;

    w(1)   =  0.00006103888176768602;
    w(2)   =  0.00058807215382869754;
    w(3)   =  0.00120930061875273991;
    w(4)   =  0.00180308126695362360;
    w(5)   =  0.00240715327877140915;
    w(6)   =  0.00300345869904497128;
    w(7)   =  0.00360197835812614147;
    w(8)   =  0.00419553798718534675;
    w(9)   =  0.00478862143341336763;
    w(10)  =  0.00537724746840184621;
    w(11)  =  0.00596388034730799521;
    w(12)  =  0.00654590843862298928;
    w(13)  =  0.00712483332325489785;
    w(14)  =  0.00769875778896082811;
    w(15)  =  0.00826865154203087108;
    w(16)  =  0.00883303867470133581;
    w(17)  =  0.00939256583934814871;
    w(18)  =  0.00994602784923457905;
    w(19)  =  0.01049386202576892125;
    w(20)  =  0.01103504877427254184;
    w(21)  =  0.01156988348290849967;
    w(22)  =  0.01209748052807164113;
    w(23)  =  0.01261803597977743271;
    w(24)  =  0.01313076516693974630;
    w(25)  =  0.01363579321293772047;
    w(26)  =  0.01413241437853094133;
    w(27)  =  0.01462070254634350205;
    w(28)  =  0.01510001572479266783;
    w(29)  =  0.01557039073899425960;
    w(30)  =  0.01603123858745057916;
    w(31)  =  0.01648256956220377909;
    w(32)  =  0.01692383985846499368;
    w(33)  =  0.01735504125411394958;
    w(34)  =  0.01777566938875279997;
    w(35)  =  0.01818570377926339481;
    w(36)  =  0.01858467519566908661;
    w(37)  =  0.01897255587067948426;
    w(38)  =  0.01934890842392451844;
    w(39)  =  0.01971370183700155725;
    w(40)  =  0.02006652805198357604;
    w(41)  =  0.02040735612003867863;
    w(42)  =  0.02073580533490147816;
    w(43)  =  0.02105184759002011131;
    w(44)  =  0.02135512797425970725;
    w(45)  =  0.02164562356712882440;
    w(46)  =  0.02192300400598756892;
    w(47)  =  0.02218725355897195088;
    w(48)  =  0.02243806539722630184;
    w(49)  =  0.02267543270456671718;
    w(50)  =  0.02289907134390605882;
    w(51)  =  0.02310898491627407168;
    w(52)  =  0.02330491126131143273;
    w(53)  =  0.02348686571193163505;
    w(54)  =  0.02365460746057766523;
    w(55)  =  0.02380816473024258975;
    w(56)  =  0.02394731750476901502;
    w(57)  =  0.02407210792327850000;
    w(58)  =  0.02418233623893147567;
    w(59)  =  0.02427805942075745923;
    w(60)  =  0.02435909748927643184;
    w(61)  =  0.02442552306156708690;
    w(62)  =  0.02447717542743444284;
    w(63)  =  0.02451414358881568292;
    w(64)  =  0.02453628559651495473;
    w(65)  =  0.02454370750551418263;
    w(66)  =  0.02453628559651495473;
    w(67)  =  0.02451414358881568292;
    w(68)  =  0.02447717542743444284;
    w(69)  =  0.02442552306156708690;
    w(70)  =  0.02435909748927643184;
    w(71)  =  0.02427805942075745923;
    w(72)  =  0.02418233623893147567;
    w(73)  =  0.02407210792327850000;
    w(74)  =  0.02394731750476901502;
    w(75)  =  0.02380816473024258975;
    w(76)  =  0.02365460746057766523;
    w(77)  =  0.02348686571193163505;
    w(78)  =  0.02330491126131143273;
    w(79)  =  0.02310898491627407168;
    w(80)  =  0.02289907134390605882;
    w(81)  =  0.02267543270456671718;
    w(82)  =  0.02243806539722630184;
    w(83)  =  0.02218725355897195088;
    w(84)  =  0.02192300400598756892;
    w(85)  =  0.02164562356712882440;
    w(86)  =  0.02135512797425970725;
    w(87)  =  0.02105184759002011131;
    w(88)  =  0.02073580533490147816;
    w(89)  =  0.02040735612003867863;
    w(90)  =  0.02006652805198357604;
    w(91)  =  0.01971370183700155725;
    w(92)  =  0.01934890842392451844;
    w(93)  =  0.01897255587067948426;
    w(94)  =  0.01858467519566908661;
    w(95)  =  0.01818570377926339481;
    w(96)  =  0.01777566938875279997;
    w(97)  =  0.01735504125411394958;
    w(98)  =  0.01692383985846499368;
    w(99)  =  0.01648256956220377909;
    w(100) =  0.01603123858745057916;
    w(101) =  0.01557039073899425960;
    w(102) =  0.01510001572479266783;
    w(103) =  0.01462070254634350205;
    w(104) =  0.01413241437853094133;
    w(105) =  0.01363579321293772047;
    w(106) =  0.01313076516693974630;
    w(107) =  0.01261803597977743271;
    w(108) =  0.01209748052807164113;
    w(109) =  0.01156988348290849967;
    w(110) =  0.01103504877427254184;
    w(111) =  0.01049386202576892125;
    w(112) =  0.00994602784923457905;
    w(113) =  0.00939256583934814871;
    w(114) =  0.00883303867470133581;
    w(115) =  0.00826865154203087108;
    w(116) =  0.00769875778896082811;
    w(117) =  0.00712483332325489785;
    w(118) =  0.00654590843862298928;
    w(119) =  0.00596388034730799521;
    w(120) =  0.00537724746840184621;
    w(121) =  0.00478862143341336763;
    w(122) =  0.00419553798718534675;
    w(123) =  0.00360197835812614147;
    w(124) =  0.00300345869904497128;
    w(125) =  0.00240715327877140915;
    w(126) =  0.00180308126695362360;
    w(127) =  0.00120930061875273991;
    w(128) =  0.00058807215382869754;
    w(129) =  0.00006103888176768602;

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'CLENSHAW_CURTIS_SET - Fatal error!\n' );
    fprintf ( 1, '  Illegal value of N = %d\n', n );
    fprintf ( 1, '  Legal values are 1 to 17, 33, 65 or 129.\n' );
    error ( 'CLENSHAW_CURTIS_SET - Fatal error!' );

  end

  return
end