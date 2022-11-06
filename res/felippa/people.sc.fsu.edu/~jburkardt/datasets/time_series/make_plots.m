%  Make plots using MATLAB.
%
  y = load ( 'airline_passengers.txt' );
  plot ( y(:,3), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- January 1949 -- December 1960 -->' )
  ylabel ( '<-- Thousands of Passengers -->' )
  title ( 'Monthly International Airline Traffic' )
  print ( '-dpng', 'airline_passengers.png' )

  y = load ( 'beveridge_wheat.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year -->' )
  ylabel ( '<-- Wheat Price Index -->' )
  title ( 'Beveridge Wheat Price Index, 1500-1869' )
  print ( '-dpng', 'beveridge_wheat.png' )

  y = load ( 'co2.txt' );
  plot ( y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Month -->' )
  ylabel ( '<-- CO2 Concentration -->' )
  title ( 'Monthly Carbon Dioxide Readings' )
  print ( '-dpng', 'co2.png' )

  y = load ( 'ecg.txt' );
  plot ( y, 'Linewidth', 1 );
  grid on
  xlabel ( '<-- Time -->' )
  ylabel ( '<-- ECG Reading -->' )
  title ( 'Simulated Electrocardiogram Data' )
  print ( '-dpng', 'ecg.png' )

  y = load ( 'el_nino.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 1 );
  grid on
  xlabel ( '<-- Month -->' )
  ylabel ( '<-- Pressure Difference -->' )
  title ( 'Monthly pressure difference, Easter Island & Darwin, Australia' )
  print ( '-dpng', 'el_nino.png' )

  y = load ( 'gnp.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year by Quarters -->' )
  ylabel ( '<-- GNP in Billions -->' )
  title ( 'Estimated Quarterly US GNP, Not Adjusted for Inflation' )
  print ( '-dpng', 'gnp.png' )

  y = load ( 'hog_price.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year-->' )
  ylabel ( '<-- 1000 * log of price per head -->' )
  title ( 'Hog Prices' )
  print ( '-dpng', 'hog_price.png' )

  y = load ( 'hurricanes.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year-->' )
  ylabel ( '<-- Number of Atlantic hurricanes -->' )
  title ( 'Atlantic Hurricanes' )
  print ( '-dpng', 'hurricanes.png' )

  y = load ( 'ibm_stock.txt' );
  plot ( y, 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Trading Days -->' )
  ylabel ( '<-- Stock Price -->' )
  title ( 'IBM Common Stock Price, Box-Jenkins Series B' )
  print ( '-dpng', 'ibm_stock.png' )

  y = load ( 'lynx.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year -->' )
  ylabel ( '<-- Number of Lynx Captured -->' )
  title ( 'Yearly Lynx Harvest' )
  print ( '-dpng', 'lynx.png' )

  y = load ( 'measles_nyc.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Month -->' )
  ylabel ( '<-- Cases -->' )
  title ( 'New York City Measles Cases' )
  print ( '-dpng', 'measles_nyc.png' )

  y = load ( 'nile.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Years -->' )
  ylabel ( '<-- High Water Mark -->' )
  title ( 'Yearly Nile High Water Mark' )
  print ( '-dpng', 'nile.png' )

  y = load ( 'snowfall.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year -->' )
  ylabel ( '<-- Total Snow in Inches -->' )
  title ( 'Yearly Snowfall in Michigan' )
  print ( '-dpng', 'snowfall.png' )

  y = load ( 'sunspot.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year -->' )
  ylabel ( '<-- Sunspots -->' )
  title ( 'Yearly Sunspots' )
  print ( '-dpng', 'sunspot.png' )

  y = load ( 'us_population_census.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Year -->' )
  ylabel ( '<-- Population -->' )
  title ( 'US Population Counts' )
  print ( '-dpng', 'us_population_census.png' )

  y = load ( 'wing_oscillation.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- Observation -->' )
  ylabel ( '<-- Y coordinate -->' )
  title ( 'Wing Oscillations' )
  print ( '-dpng', 'wing_oscillation.png' )

  y = load ( 'wometco_sales.txt' );
  plot ( y(:,1), y(:,2), 'Linewidth', 2 );
  grid on
  xlabel ( '<-- 4 Week Periods -->' )
  ylabel ( '<-- Sales -->' )
  title ( 'WOMETCO Sales' )
  print ( '-dpng', 'wometco_sales.png' )

  close
