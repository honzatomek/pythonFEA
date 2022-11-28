function [ x, w ] = rule15 ( n )

%*****************************************************************************80
%
%% rule15() returns the rule of degree 15.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 July 2014
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
%    real X(3,N), the coordinates of the nodes.
%
%    real W(N), the weights.
%
  xs = [ ...
    0.4846214646897683E+00, 0.6027166661210369E-01, ...
    0.5119120893947191E+00, -0.5797527815798613E+00, ...
    0.8916317278515248E+00, 0.4427375637743738E+00, ...
    -0.2409001206926237E+00, 0.4213881124359606E+00, ...
    0.2454110415378226E+00, -0.1642469924955429E+00, ...
    -0.5907939646559498E+00, -0.6671386201069288E+00, ...
    0.7552290798850946E+00, 0.1001864121384495E+00, ...
    -0.7219847932977880E+00, 0.2982096063117719E+00, ...
    0.5073944886321697E+00, -0.4863692703613542E+00, ...
    0.4639404445534597E+00, -0.8081387191848195E+00, ...
    -0.5896154562963155E+00, 0.1349771790076746E+00, ...
    -0.1459114981397958E+00, -0.7771044530879095E+00, ...
    0.3049106464545794E+00, 0.7478398889947702E+00, ...
    0.2593964431357709E+00, -0.8826393735985893E+00, ...
    0.5355784681079047E+00, -0.1464672879292506E+00, ...
    0.6066659900076390E+00, -0.8588774763360313E+00, ...
    -0.9697378209073355E+00, -0.3207448401664478E+00, ...
    0.3238825883494653E+00, -0.8911881463557618E+00, ...
    0.9588421432293531E+00, -0.9459659824876482E-01, ...
    0.4326888655727574E+00, -0.4031429838762605E+00, ...
    -0.4784772616280669E+00, 0.8992518592510214E+00, ...
    0.6586422425595307E+00, 0.8204427703547663E+00, ...
    -0.5614110159343418E+00, 0.9520903109905504E-01, ...
    0.5640256443183821E+00, -0.8042276926515493E+00, ...
    0.6821201856074234E+00, -0.7682858721251195E+00, ...
    0.7082541697630407E+00, -0.4618970778119202E+00, ...
    -0.2168388877255139E+00, 0.5371020397039006E+00, ...
    0.8885379409916848E+00, 0.6251053221154703E+00, ...
    0.8755744976159847E+00, -0.1289396220206777E+00, ...
    0.9646166796039118E+00, 0.2607898402928713E+00, ...
    0.9703940236997658E+00, 0.7108751014443095E-01, ...
    0.7712967479205184E+00, 0.8842270989720199E+00, ...
    0.7991197473160693E+00, -0.3410291594283488E+00, ...
    -0.4192756150578085E+00, 0.7230818664415763E+00, ...
    0.1436938638381775E+00, -0.9757899278543728E+00, ...
    0.8447822406339708E+00, 0.9613637788842541E+00, ...
    0.8507427476805113E+00, -0.8848833562540662E+00, ...
    0.9720070913947075E+00, -0.6259228134698737E+00, ...
    0.9441521606893316E+00, 0.2886043547146998E+00, ...
    0.9428658265051933E+00, 0.9499838153205579E+00, ...
    -0.1175912491528422E+00, -0.9810321115887223E+00, ...
    0.9305760666668829E+00, -0.9783274226607617E+00, ...
    -0.9834890551889414E+00, 0.4453699774671322E-01, ...
    -0.5306871050560035E+00, 0.9868369576014261E+00, ...
    -0.5232830891127319E+00, -0.8918143354641236E+00, ...
    0.8676179401449021E+00, 0.9964872079697851E+00, ...
    0.9918319243897599E+00, 0.7865264660879382E+00, ...
    -0.9241660476457195E-01, -0.4846214646897686E+00, ...
    -0.6027166661210399E-01, -0.5119120893947190E+00, ...
    0.5797527815798613E+00, -0.8916317278515249E+00, ...
    -0.4427375637743741E+00, 0.2409001206926235E+00, ...
    -0.4213881124359606E+00, -0.2454110415378225E+00, ...
    0.1642469924955428E+00, 0.5907939646559499E+00, ...
    0.6671386201069287E+00, -0.7552290798850947E+00, ...
    -0.1001864121384497E+00, 0.7219847932977881E+00, ...
    -0.2982096063117720E+00, -0.5073944886321696E+00, ...
    0.4863692703613542E+00, -0.4639404445534597E+00, ...
    0.8081387191848196E+00, 0.5896154562963155E+00, ...
    -0.1349771790076746E+00, 0.1459114981397958E+00, ...
    0.7771044530879095E+00, -0.3049106464545795E+00, ...
    -0.7478398889947702E+00, -0.2593964431357710E+00, ...
    0.8826393735985893E+00, -0.5355784681079047E+00, ...
    0.1464672879292507E+00, -0.6066659900076391E+00, ...
    0.8588774763360314E+00, 0.9697378209073355E+00, ...
    0.3207448401664478E+00, -0.3238825883494653E+00, ...
    0.8911881463557617E+00, -0.9588421432293531E+00, ...
    0.9459659824876483E-01, -0.4326888655727574E+00, ...
    0.4031429838762604E+00, 0.4784772616280668E+00, ...
    -0.8992518592510215E+00, -0.6586422425595307E+00, ...
    -0.8204427703547664E+00, 0.5614110159343418E+00, ...
    -0.9520903109905507E-01, -0.5640256443183821E+00, ...
    0.8042276926515494E+00, -0.6821201856074234E+00, ...
    0.7682858721251195E+00, -0.7082541697630407E+00, ...
    0.4618970778119202E+00, 0.2168388877255139E+00, ...
    -0.5371020397039005E+00, -0.8885379409916848E+00, ...
    -0.6251053221154704E+00, -0.8755744976159846E+00, ...
    0.1289396220206776E+00, -0.9646166796039118E+00, ...
    -0.2607898402928713E+00, -0.9703940236997658E+00, ...
    -0.7108751014443089E-01, -0.7712967479205184E+00, ...
    -0.8842270989720198E+00, -0.7991197473160693E+00, ...
    0.3410291594283487E+00, 0.4192756150578084E+00, ...
    -0.7230818664415765E+00, -0.1436938638381774E+00, ...
    0.9757899278543728E+00, -0.8447822406339707E+00, ...
    -0.9613637788842542E+00, -0.8507427476805113E+00, ...
    0.8848833562540663E+00, -0.9720070913947076E+00, ...
    0.6259228134698737E+00, -0.9441521606893316E+00, ...
    -0.2886043547146998E+00, -0.9428658265051933E+00, ...
    -0.9499838153205579E+00, 0.1175912491528422E+00, ...
    0.9810321115887223E+00, -0.9305760666668829E+00, ...
    0.9783274226607617E+00, 0.9834890551889414E+00, ...
    -0.4453699774671330E-01, 0.5306871050560035E+00, ...
    -0.9868369576014261E+00, 0.5232830891127319E+00, ...
    0.8918143354641237E+00, -0.8676179401449020E+00, ...
    -0.9964872079697851E+00, -0.9918319243897600E+00, ...
    -0.7865264660879382E+00, 0.9241660476457186E-01 ];
  ys = [ ...
    -0.2262155685815833E+00, -0.5802224500744104E+00, ...
    -0.8812418910175070E+00, 0.4577182827511190E-01, ...
    0.7310521233845833E+00, 0.4830906487655276E+00, ...
    -0.8061293440666314E+00, -0.1078387681721650E+00, ...
    0.8648936466868999E+00, 0.1116488353049886E+00, ...
    -0.9117169599889896E+00, 0.7671850694134570E+00, ...
    0.8098232031170816E+00, 0.9416109016304105E+00, ...
    -0.8571607992505791E+00, -0.8294951885681802E+00, ...
    -0.3287584215285467E-01, -0.9716511178926364E+00, ...
    0.4508573465614439E+00, -0.9829067455694369E+00, ...
    -0.3886494734052406E+00, 0.6210687867431628E+00, ...
    0.7058206696430964E+00, 0.8874742900432545E+00, ...
    0.2936260801274535E-01, 0.4728249466868350E+00, ...
    -0.7999771197371979E+00, 0.9406572379268412E-01, ...
    -0.4746762454784017E+00, 0.5158509951163104E+00, ...
    -0.8256099261094548E+00, -0.5791703340444312E+00, ...
    0.4466546017661202E+00, -0.1927320854548640E+00, ...
    -0.4345261425945900E+00, 0.5425166397866776E-02, ...
    0.9375941025615940E+00, -0.3420778054235573E+00, ...
    0.7075968971274725E+00, 0.6496964078624228E+00, ...
    0.4004462354823037E+00, -0.6968996840182875E+00, ...
    0.3055994655170601E+00, -0.5169213379049986E+00, ...
    0.9549786512838463E+00, -0.8791296893067777E+00, ...
    0.8303000567130658E+00, -0.6377483111957468E+00, ...
    0.8739025562234195E+00, -0.1693477428011365E+00, ...
    -0.2351350601654628E+00, -0.6299185210533546E+00, ...
    0.1604152098962463E+00, -0.5905825901125751E+00, ...
    0.1734591472408230E+00, 0.2417993452840939E+00, ...
    -0.7167382075250317E+00, -0.5500881365309197E+00, ...
    -0.2400335193040073E+00, 0.7704776594613698E+00, ...
    -0.5549501648642681E+00, 0.2871796808122397E+00, ...
    -0.2817721114790634E+00, 0.9549722793117502E+00, ...
    -0.9499810867427769E+00, 0.9910163892213160E+00, ...
    -0.9801994414073382E+00, -0.4857741672686406E+00, ...
    -0.9028324174014650E+00, -0.7689895270308303E+00, ...
    -0.8885921033548654E+00, 0.2255470425271027E+00, ...
    -0.7650697965238176E-01, 0.9735656776897391E+00, ...
    -0.2305869558790821E+00, 0.7981924334040106E+00, ...
    -0.6544414972640588E+00, -0.9790786374374271E+00, ...
    0.9251057242786117E+00, 0.6489915714062549E+00, ...
    -0.9627047612899647E+00, 0.9597706404726861E+00, ...
    0.5373877582566533E+00, 0.8266106540930876E+00, ...
    -0.8547964831867243E+00, 0.9558026916663981E+00, ...
    0.9856879028440860E+00, -0.9065724188604554E+00, ...
    -0.9845747454374670E+00, 0.9891294476252082E+00, ...
    0.9881191368012432E+00, 0.3020236952873330E+00, ...
    0.3856116113348522E+00, 0.7292572061308893E+00, ...
    -0.1882646447022738E+00, 0.2262155685815840E+00, ...
    0.5802224500744105E+00, 0.8812418910175072E+00, ...
    -0.4577182827511186E-01, -0.7310521233845831E+00, ...
    -0.4830906487655275E+00, 0.8061293440666314E+00, ...
    0.1078387681721650E+00, -0.8648936466868998E+00, ...
    -0.1116488353049886E+00, 0.9117169599889896E+00, ...
    -0.7671850694134570E+00, -0.8098232031170816E+00, ...
    -0.9416109016304105E+00, 0.8571607992505792E+00, ...
    0.8294951885681802E+00, 0.3287584215285466E-01, ...
    0.9716511178926363E+00, -0.4508573465614438E+00, ...
    0.9829067455694369E+00, 0.3886494734052407E+00, ...
    -0.6210687867431628E+00, -0.7058206696430964E+00, ...
    -0.8874742900432544E+00, -0.2936260801274522E-01, ...
    -0.4728249466868351E+00, 0.7999771197371979E+00, ...
    -0.9406572379268432E-01, 0.4746762454784017E+00, ...
    -0.5158509951163104E+00, 0.8256099261094548E+00, ...
    0.5791703340444312E+00, -0.4466546017661200E+00, ...
    0.1927320854548639E+00, 0.4345261425945901E+00, ...
    -0.5425166397866769E-02, -0.9375941025615940E+00, ...
    0.3420778054235572E+00, -0.7075968971274725E+00, ...
    -0.6496964078624228E+00, -0.4004462354823038E+00, ...
    0.6968996840182876E+00, -0.3055994655170602E+00, ...
    0.5169213379049985E+00, -0.9549786512838463E+00, ...
    0.8791296893067777E+00, -0.8303000567130657E+00, ...
    0.6377483111957469E+00, -0.8739025562234195E+00, ...
    0.1693477428011367E+00, 0.2351350601654628E+00, ...
    0.6299185210533547E+00, -0.1604152098962464E+00, ...
    0.5905825901125752E+00, -0.1734591472408231E+00, ...
    -0.2417993452840938E+00, 0.7167382075250318E+00, ...
    0.5500881365309197E+00, 0.2400335193040072E+00, ...
    -0.7704776594613698E+00, 0.5549501648642682E+00, ...
    -0.2871796808122396E+00, 0.2817721114790634E+00, ...
    -0.9549722793117502E+00, 0.9499810867427769E+00, ...
    -0.9910163892213160E+00, 0.9801994414073382E+00, ...
    0.4857741672686406E+00, 0.9028324174014649E+00, ...
    0.7689895270308303E+00, 0.8885921033548654E+00, ...
    -0.2255470425271028E+00, 0.7650697965238172E-01, ...
    -0.9735656776897391E+00, 0.2305869558790820E+00, ...
    -0.7981924334040106E+00, 0.6544414972640588E+00, ...
    0.9790786374374271E+00, -0.9251057242786117E+00, ...
    -0.6489915714062550E+00, 0.9627047612899647E+00, ...
    -0.9597706404726861E+00, -0.5373877582566532E+00, ...
    -0.8266106540930876E+00, 0.8547964831867243E+00, ...
    -0.9558026916663980E+00, -0.9856879028440859E+00, ...
    0.9065724188604554E+00, 0.9845747454374671E+00, ...
    -0.9891294476252082E+00, -0.9881191368012432E+00, ...
    -0.3020236952873331E+00, -0.3856116113348522E+00, ...
    -0.7292572061308894E+00, 0.1882646447022739E+00 ];
  zs = [ ...
    0.9718923364663833E+00, -0.6995279408119103E+00, ...
    0.4077821712986755E+00, -0.1984488505968536E+00, ...
    -0.1635978697790265E+00, -0.9519012083224356E+00, ...
    -0.7709973566567310E+00, 0.6575891838559990E+00, ...
    -0.2431469413756804E+00, -0.2154126099125328E+00, ...
    0.2917708722416409E+00, -0.7837093593401955E+00, ...
    0.5503667466636896E-01, 0.6034320247288605E+00, ...
    -0.5348830267053428E+00, 0.6967890074634113E+00, ...
    -0.6004372124809728E+00, -0.7978753021283712E+00, ...
    0.7111633203919522E+00, -0.4796592851863407E+00, ...
    -0.1066936069842310E+00, -0.4937724895804262E-02, ...
    -0.3617523885223834E+00, 0.7864082151686236E+00, ...
    0.8896593937798930E+00, -0.3003206340764916E+00, ...
    -0.8604298301675264E+00, -0.9764052940319004E+00, ...
    0.4627660680406495E+00, -0.8466728944778892E+00, ...
    -0.9693428983943551E+00, 0.7807520536361380E+00, ...
    0.2410080019231437E+00, 0.2275963934164933E+00, ...
    -0.1058345123272911E+00, -0.4676766976465098E-01, ...
    0.1025372637039018E+00, 0.6347152609726029E+00, ...
    0.3752067881462010E+00, 0.5361771132313945E+00, ...
    0.9695076036670979E+00, 0.6398892617052058E+00, ...
    0.9539776797271410E+00, 0.1946562772645561E+00, ...
    0.5161190440947852E+00, -0.2830148609612029E+00, ...
    0.9517662443841286E+00, -0.7967109780188084E+00, ...
    -0.7304370458992153E+00, -0.5338593513474772E+00, ...
    -0.3179475898860780E+00, 0.5547973598962450E+00, ...
    0.8313518081338683E+00, 0.9654245596905445E+00, ...
    -0.5628516000227574E+00, -0.8613302773849665E+00, ...
    -0.4483881659919932E+00, -0.9517697705546500E+00, ...
    -0.7234814778587770E+00, -0.8875078228929659E+00, ...
    0.9243612035652934E+00, 0.4601935017159294E+00, ...
    0.8090143745218911E+00, 0.9163662035802967E+00, ...
    0.4382382798746217E+00, 0.8459991721751295E+00, ...
    -0.1366146439527482E+00, -0.7720184760904137E+00, ...
    0.9707987948940664E+00, 0.5206831670219514E+00, ...
    0.9588210246069581E+00, 0.8185288301633218E+00, ...
    -0.9577089524402139E+00, 0.1925548617062123E+00, ...
    0.4997201390064559E+00, 0.5406623725365253E-01, ...
    -0.9375810397798050E+00, 0.1306044624286102E+00, ...
    -0.8929122835431057E+00, 0.9831724010468225E+00, ...
    0.6385058885805324E+00, -0.7720170702032852E+00, ...
    0.3549666743399563E+00, -0.1714024163539602E+00, ...
    -0.6784170912845590E+00, 0.9880880116453774E+00, ...
    -0.8468765296700308E+00, -0.6791166853273773E+00, ...
    0.9378710595411145E+00, 0.9586435791263295E+00, ...
    -0.4583535837605982E+00, -0.1539477252636465E+00, ...
    -0.9251749829870214E+00, -0.9949152686131382E+00, ...
    0.9985713487179375E+00, -0.9718923364663833E+00, ...
    0.6995279408119103E+00, -0.4077821712986756E+00, ...
    0.1984488505968536E+00, 0.1635978697790266E+00, ...
    0.9519012083224355E+00, 0.7709973566567310E+00, ...
    -0.6575891838559990E+00, 0.2431469413756804E+00, ...
    0.2154126099125329E+00, -0.2917708722416409E+00, ...
    0.7837093593401955E+00, -0.5503667466636887E-01, ...
    -0.6034320247288605E+00, 0.5348830267053428E+00, ...
    -0.6967890074634113E+00, 0.6004372124809728E+00, ...
    0.7978753021283711E+00, -0.7111633203919521E+00, ...
    0.4796592851863406E+00, 0.1066936069842310E+00, ...
    0.4937724895804275E-02, 0.3617523885223834E+00, ...
    -0.7864082151686236E+00, -0.8896593937798931E+00, ...
    0.3003206340764915E+00, 0.8604298301675265E+00, ...
    0.9764052940319005E+00, -0.4627660680406495E+00, ...
    0.8466728944778892E+00, 0.9693428983943552E+00, ...
    -0.7807520536361380E+00, -0.2410080019231437E+00, ...
    -0.2275963934164933E+00, 0.1058345123272911E+00, ...
    0.4676766976465104E-01, -0.1025372637039019E+00, ...
    -0.6347152609726029E+00, -0.3752067881462010E+00, ...
    -0.5361771132313945E+00, -0.9695076036670979E+00, ...
    -0.6398892617052058E+00, -0.9539776797271410E+00, ...
    -0.1946562772645561E+00, -0.5161190440947853E+00, ...
    0.2830148609612028E+00, -0.9517662443841286E+00, ...
    0.7967109780188085E+00, 0.7304370458992153E+00, ...
    0.5338593513474772E+00, 0.3179475898860781E+00, ...
    -0.5547973598962450E+00, -0.8313518081338683E+00, ...
    -0.9654245596905445E+00, 0.5628516000227572E+00, ...
    0.8613302773849664E+00, 0.4483881659919933E+00, ...
    0.9517697705546500E+00, 0.7234814778587769E+00, ...
    0.8875078228929659E+00, -0.9243612035652934E+00, ...
    -0.4601935017159294E+00, -0.8090143745218911E+00, ...
    -0.9163662035802967E+00, -0.4382382798746217E+00, ...
    -0.8459991721751295E+00, 0.1366146439527482E+00, ...
    0.7720184760904137E+00, -0.9707987948940665E+00, ...
    -0.5206831670219514E+00, -0.9588210246069581E+00, ...
    -0.8185288301633218E+00, 0.9577089524402139E+00, ...
    -0.1925548617062122E+00, -0.4997201390064560E+00, ...
    -0.5406623725365260E-01, 0.9375810397798049E+00, ...
    -0.1306044624286103E+00, 0.8929122835431057E+00, ...
    -0.9831724010468225E+00, -0.6385058885805324E+00, ...
    0.7720170702032852E+00, -0.3549666743399564E+00, ...
    0.1714024163539601E+00, 0.6784170912845590E+00, ...
    -0.9880880116453774E+00, 0.8468765296700308E+00, ...
    0.6791166853273775E+00, -0.9378710595411145E+00, ...
    -0.9586435791263295E+00, 0.4583535837605982E+00, ...
    0.1539477252636465E+00, 0.9251749829870214E+00, ...
    0.9949152686131382E+00, -0.9985713487179375E+00 ];
  ws = [ ...
    0.2931082855526895E-02, 0.1466168295291819E-01, ...
    0.1008190603381851E-01, 0.2521840249289902E-01, ...
    0.9545986541148931E-02, 0.7815725861454997E-02, ...
    0.1225157612225792E-01, 0.2516512639883486E-01, ...
    0.1749166437590727E-01, 0.4056629885298555E-01, ...
    0.1199350194114567E-01, 0.1133390863336471E-01, ...
    0.1485090749543295E-01, 0.1026773216326104E-01, ...
    0.1201022409690237E-01, 0.1547758419389476E-01, ...
    0.2898163426866567E-01, 0.5077671719424524E-02, ...
    0.2462022242995652E-01, 0.3796424546988349E-02, ...
    0.3310665553884919E-01, 0.3508140615002558E-01, ...
    0.2950704004490954E-01, 0.8017748658106175E-02, ...
    0.1975278275621659E-01, 0.2559496685146834E-01, ...
    0.1349736154064143E-01, 0.4377789154192862E-02, ...
    0.3252004086526810E-01, 0.2138516935436983E-01, ...
    0.5015100019990201E-02, 0.1268265369185493E-01, ...
    0.9980860281711945E-02, 0.4271671806230670E-01, ...
    0.4274198226264674E-01, 0.2231057111860769E-01, ...
    0.4634811584165995E-02, 0.3628925840326913E-01, ...
    0.2963883283828190E-01, 0.3004053280506377E-01, ...
    0.9656867842652010E-02, 0.1208027996271507E-01, ...
    0.1067097545113014E-01, 0.2483163417257641E-01, ...
    0.1052923525628832E-01, 0.2351898637367898E-01, ...
    0.7064042544802274E-02, 0.1458074471394978E-01, ...
    0.1255548203713305E-01, 0.2826856991152390E-01, ...
    0.3484550434776072E-01, 0.3029815093584674E-01, ...
    0.2852474545169975E-01, 0.9146009437075924E-02, ...
    0.1990115803594484E-01, 0.2069177392023117E-01, ...
    0.1613360621469677E-01, 0.1339344756044318E-01, ...
    0.9274781087469087E-02, 0.1530433781694090E-01, ...
    0.3917613624484169E-02, 0.4743890758461208E-01, ...
    0.2001420743439168E-01, 0.2832855798485957E-02, ...
    0.9111027190743101E-02, 0.3380728915063295E-02, ...
    0.9357526881990973E-02, 0.2181586678118927E-01, ...
    0.5497095242202157E-02, 0.6401916148758926E-02, ...
    0.3861833303734157E-02, 0.8708380742935694E-02, ...
    0.8616911070460580E-02, 0.5860715399693166E-02, ...
    0.1134352478246400E-01, 0.2833705081651916E-01, ...
    0.4870087478700985E-02, 0.1085506021481164E-01, ...
    0.3085543406158564E-02, 0.2320131607643815E-02, ...
    0.1217435492763373E-01, 0.1876868717373962E-02, ...
    0.1764889584855365E-01, 0.6738482260511419E-02, ...
    0.4000917701843583E-02, 0.2530717864603938E-02, ...
    0.4454840968844286E-02, 0.2917862788830831E-02, ...
    0.3031055423950877E-02, 0.1091305108499325E-02, ...
    0.4268181631337476E-02, 0.6826137445231240E-02, ...
    0.3247017230887978E-02, 0.3899933571265500E-02, ...
    0.7578127425388513E-02, 0.2931082855526887E-02, ...
    0.1466168295291819E-01, 0.1008190603381851E-01, ...
    0.2521840249289902E-01, 0.9545986541148931E-02, ...
    0.7815725861455011E-02, 0.1225157612225792E-01, ...
    0.2516512639883484E-01, 0.1749166437590725E-01, ...
    0.4056629885298554E-01, 0.1199350194114567E-01, ...
    0.1133390863336472E-01, 0.1485090749543296E-01, ...
    0.1026773216326105E-01, 0.1201022409690238E-01, ...
    0.1547758419389476E-01, 0.2898163426866567E-01, ...
    0.5077671719424526E-02, 0.2462022242995652E-01, ...
    0.3796424546988351E-02, 0.3310665553884920E-01, ...
    0.3508140615002557E-01, 0.2950704004490954E-01, ...
    0.8017748658106186E-02, 0.1975278275621657E-01, ...
    0.2559496685146833E-01, 0.1349736154064142E-01, ...
    0.4377789154192863E-02, 0.3252004086526811E-01, ...
    0.2138516935436982E-01, 0.5015100019990189E-02, ...
    0.1268265369185493E-01, 0.9980860281711927E-02, ...
    0.4271671806230667E-01, 0.4274198226264675E-01, ...
    0.2231057111860768E-01, 0.4634811584165991E-02, ...
    0.3628925840326913E-01, 0.2963883283828189E-01, ...
    0.3004053280506377E-01, 0.9656867842652003E-02, ...
    0.1208027996271506E-01, 0.1067097545113014E-01, ...
    0.2483163417257640E-01, 0.1052923525628832E-01, ...
    0.2351898637367896E-01, 0.7064042544802277E-02, ...
    0.1458074471394977E-01, 0.1255548203713306E-01, ...
    0.2826856991152389E-01, 0.3484550434776074E-01, ...
    0.3029815093584674E-01, 0.2852474545169975E-01, ...
    0.9146009437075919E-02, 0.1990115803594483E-01, ...
    0.2069177392023119E-01, 0.1613360621469677E-01, ...
    0.1339344756044317E-01, 0.9274781087469082E-02, ...
    0.1530433781694089E-01, 0.3917613624484167E-02, ...
    0.4743890758461208E-01, 0.2001420743439166E-01, ...
    0.2832855798485960E-02, 0.9111027190743106E-02, ...
    0.3380728915063295E-02, 0.9357526881990973E-02, ...
    0.2181586678118927E-01, 0.5497095242202148E-02, ...
    0.6401916148758928E-02, 0.3861833303734160E-02, ...
    0.8708380742935682E-02, 0.8616911070460571E-02, ...
    0.5860715399693163E-02, 0.1134352478246400E-01, ...
    0.2833705081651915E-01, 0.4870087478700985E-02, ...
    0.1085506021481165E-01, 0.3085543406158568E-02, ...
    0.2320131607643819E-02, 0.1217435492763373E-01, ...
    0.1876868717373960E-02, 0.1764889584855365E-01, ...
    0.6738482260511409E-02, 0.4000917701843592E-02, ...
    0.2530717864603941E-02, 0.4454840968844280E-02, ...
    0.2917862788830830E-02, 0.3031055423950871E-02, ...
    0.1091305108499324E-02, 0.4268181631337478E-02, ...
    0.6826137445231243E-02, 0.3247017230887967E-02, ...
    0.3899933571265500E-02, 0.7578127425388515E-02 ];

  x(1,1:n) = xs(1:n);
  x(2,1:n) = ys(1:n);
  x(3,1:n) = zs(1:n);
  w(1:n) = ws(1:n);

  return
end
