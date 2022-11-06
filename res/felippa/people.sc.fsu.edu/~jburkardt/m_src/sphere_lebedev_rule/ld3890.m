function [ x, y, z, w ] = ld3890 ( )

%*****************************************************************************80;
%
%% ld3890() computes the 3890 point Lebedev angular grid.
%
%  Modified:
%
%    14 September 2010
%
%  Author:
%
%    Dmitri Laikov
%
%  Reference:
%
%    Vyacheslav Lebedev, Dmitri Laikov,
%    A quadrature formula for the sphere of the 131st
%    algebraic order of accuracy,
%    Russian Academy of Sciences Doklady Mathematics,
%    Volume 59, Number 3, 1999, pages 477-481.
%
%  Output:
%
%    real X(N), Y(N), Z(N), W(N), the coordinates
%    and weights of the points.
%
  n = 0;
  x = zeros(3890,1);
  y = zeros(3890,1);
  z = zeros(3890,1);
  w = zeros(3890,1);
  a = 0.0;
  b = 0.0;
  v = 0.1807395252196920E-04;
  [ n, x, y, z, w ] = gen_oh ( 1, n, a, b, v, x, y, z, w );
  v = 0.2848008782238827E-03;
  [ n, x, y, z, w ] = gen_oh ( 2, n, a, b, v, x, y, z, w );
  v = 0.2836065837530581E-03;
  [ n, x, y, z, w ] = gen_oh ( 3, n, a, b, v, x, y, z, w );
  a = 0.1587876419858352E-01;
  v = 0.7013149266673816E-04;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4069193593751206E-01;
  v = 0.1162798021956766E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.7025888115257997E-01;
  v = 0.1518728583972105E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.1027495450028704;
  v = 0.1798796108216934E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.1371457730893426;
  v = 0.2022593385972785E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.1727758532671953;
  v = 0.2203093105575464E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.2091492038929037;
  v = 0.2349294234299855E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.2458813281751915;
  v = 0.2467682058747003E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.2826545859450066;
  v = 0.2563092683572224E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.3191957291799622;
  v = 0.2639253896763318E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.3552621469299578;
  v = 0.2699137479265108E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.3906329503406230;
  v = 0.2745196420166739E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4251028614093031;
  v = 0.2779529197397593E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4584777520111870;
  v = 0.2803996086684265E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4905711358710193;
  v = 0.2820302356715842E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.5212011669847385;
  v = 0.2830056747491068E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.5501878488737995;
  v = 0.2834808950776839E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6025037877479342;
  v = 0.2835282339078929E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6254572689549016;
  v = 0.2833819267065800E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6460107179528248;
  v = 0.2832858336906784E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6639541138154251;
  v = 0.2833268235451244E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6790688515667495;
  v = 0.2835432677029253E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6911338580371512;
  v = 0.2839091722743049E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6999385956126490;
  v = 0.2843308178875841E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.7053037748656896;
  v = 0.2846703550533846E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4732224387180115E-01;
  v = 0.1051193406971900E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.1202100529326803;
  v = 0.1657871838796974E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.2034304820664855;
  v = 0.2064648113714232E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.2912285643573002;
  v = 0.2347942745819741E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.3802361792726768;
  v = 0.2547775326597726E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.4680598511056146;
  v = 0.2686876684847025E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.5528151052155599;
  v = 0.2778665755515867E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.6329386307803041;
  v = 0.2830996616782929E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.8056516651369069E-01;
  b = 0.2363454684003124E-01;
  v = 0.1403063340168372E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1156476077139389;
  b = 0.5191291632545936E-01;
  v = 0.1696504125939477E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1520473382760421;
  b = 0.8322715736994519E-01;
  v = 0.1935787242745390E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1892986699745931;
  b = 0.1165855667993712;
  v = 0.2130614510521968E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2270194446777792;
  b = 0.1513077167409504;
  v = 0.2289381265931048E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2648908185093273;
  b = 0.1868882025807859;
  v = 0.2418630292816186E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3026389259574136;
  b = 0.2229277629776224;
  v = 0.2523400495631193E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3400220296151384;
  b = 0.2590951840746235;
  v = 0.2607623973449605E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3768217953335510;
  b = 0.2951047291750847;
  v = 0.2674441032689209E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4128372900921884;
  b = 0.3307019714169930;
  v = 0.2726432360343356E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4478807131815630;
  b = 0.3656544101087634;
  v = 0.2765787685924545E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4817742034089257;
  b = 0.3997448951939695;
  v = 0.2794428690642224E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5143472814653344;
  b = 0.4327667110812024;
  v = 0.2814099002062895E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5454346213905650;
  b = 0.4645196123532293;
  v = 0.2826429531578994E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5748739313170252;
  b = 0.4948063555703345;
  v = 0.2832983542550884E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1599598738286342;
  b = 0.2792357590048985E-01;
  v = 0.1886695565284976E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1998097412500951;
  b = 0.5877141038139065E-01;
  v = 0.2081867882748234E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2396228952566202;
  b = 0.9164573914691377E-01;
  v = 0.2245148680600796E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2792228341097746;
  b = 0.1259049641962687;
  v = 0.2380370491511872E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3184251107546741;
  b = 0.1610594823400863;
  v = 0.2491398041852455E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3570481164426244;
  b = 0.1967151653460898;
  v = 0.2581632405881230E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3949164710492144;
  b = 0.2325404606175168;
  v = 0.2653965506227417E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4318617293970503;
  b = 0.2682461141151439;
  v = 0.2710857216747087E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4677221009931678;
  b = 0.3035720116011973;
  v = 0.2754434093903659E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5023417939270955;
  b = 0.3382781859197439;
  v = 0.2786579932519380E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5355701836636128;
  b = 0.3721383065625942;
  v = 0.2809011080679474E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5672608451328771;
  b = 0.4049346360466055;
  v = 0.2823336184560987E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5972704202540162;
  b = 0.4364538098633802;
  v = 0.2831101175806309E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2461687022333596;
  b = 0.3070423166833368E-01;
  v = 0.2221679970354546E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2881774566286831;
  b = 0.6338034669281885E-01;
  v = 0.2356185734270703E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3293963604116978;
  b = 0.9742862487067941E-01;
  v = 0.2469228344805590E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3697303822241377;
  b = 0.1323799532282290;
  v = 0.2562726348642046E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4090663023135127;
  b = 0.1678497018129336;
  v = 0.2638756726753028E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4472819355411712;
  b = 0.2035095105326114;
  v = 0.2699311157390862E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4842513377231437;
  b = 0.2390692566672091;
  v = 0.2746233268403837E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5198477629962928;
  b = 0.2742649818076149;
  v = 0.2781225674454771E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5539453011883145;
  b = 0.3088503806580094;
  v = 0.2805881254045684E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5864196762401251;
  b = 0.3425904245906614;
  v = 0.2821719877004913E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6171484466668390;
  b = 0.3752562294789468;
  v = 0.2830222502333124E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3350337830565727;
  b = 0.3261589934634747E-01;
  v = 0.2457995956744870E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3775773224758284;
  b = 0.6658438928081572E-01;
  v = 0.2551474407503706E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4188155229848973;
  b = 0.1014565797157954;
  v = 0.2629065335195311E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4586805892009344;
  b = 0.1368573320843822;
  v = 0.2691900449925075E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4970895714224235;
  b = 0.1724614851951608;
  v = 0.2741275485754276E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5339505133960747;
  b = 0.2079779381416412;
  v = 0.2778530970122595E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5691665792531440;
  b = 0.2431385788322288;
  v = 0.2805010567646741E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6026387682680377;
  b = 0.2776901883049853;
  v = 0.2822055834031040E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6342676150163307;
  b = 0.3113881356386632;
  v = 0.2831016901243473E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4237951119537067;
  b = 0.3394877848664351E-01;
  v = 0.2624474901131803E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4656918683234929;
  b = 0.6880219556291447E-01;
  v = 0.2688034163039377E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5058857069185980;
  b = 0.1041946859721635;
  v = 0.2738932751287636E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5443204666713996;
  b = 0.1398039738736393;
  v = 0.2777944791242523E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5809298813759742;
  b = 0.1753373381196155;
  v = 0.2806011661660987E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6156416039447128;
  b = 0.2105215793514010;
  v = 0.2824181456597460E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6483801351066604;
  b = 0.2450953312157051;
  v = 0.2833585216577828E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5103616577251688;
  b = 0.3485560643800719E-01;
  v = 0.2738165236962878E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5506738792580681;
  b = 0.7026308631512033E-01;
  v = 0.2778365208203180E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5889573040995292;
  b = 0.1059035061296403;
  v = 0.2807852940418966E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6251641589516930;
  b = 0.1414823925236026;
  v = 0.2827245949674705E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6592414921570178;
  b = 0.1767207908214530;
  v = 0.2837342344829828E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5930314017533384;
  b = 0.3542189339561672E-01;
  v = 0.2809233907610981E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6309812253390175;
  b = 0.7109574040369549E-01;
  v = 0.2829930809742694E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6666296011353230;
  b = 0.1067259792282730;
  v = 0.2841097874111479E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6703715271049922;
  b = 0.3569455268820809E-01;
  v = 0.2843455206008783E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  
  return
end
