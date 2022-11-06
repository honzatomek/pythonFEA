function [ lon, lat, city, tag ] = florida_district_reader ( filename )

%*****************************************************************************80
%
%% florida_district_reader() reads Florida Congressional District data.
%
%  Discussion:
%
%    This is the beginning of an evolving project to try to apply a
%    CVT iteration to the Florida Congressional District problem.
%
%    The file whould be a comma separated value (CSV) file that
%    is easy to write, but awkward for MATLAB to read.  
%
%    Nonetheless, the MATLAB "table()" command is the start for a process
%    of retrieving this data, which includes, for each congressional district,
%    * the index (1 through 27);
%    * the tag (for now, just the index as a string);
%    * a city (usually the home of the representative);
%    * 'N' or 'S' (for the latitude);
%    * Dlat, Mlat, Slat, for the degrees, minutes and seconds of latitude;
%    * 'E' or 'W' (for the longitude);
%    * Dlon, Mlon, Slon, for the degrees, minutes and seconds of longitude.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 June 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string FILENAME, the name of the file to be read.
%
%  Output:
%
%    real LON[N], LAT[N], the longitude and latitude for 
%    representative cities in each congressional district.
%
%    string CITY[N], the name of the city.
%
%    string TAG[N], the tag for the district.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'florida_district_reader():\n' );
  fprintf ( 1, '  Read a file which contains the latitude and longitude of\n' );
  fprintf ( 1, '  representative cities in each Florida congressional district.\n' );
%
%  Read all the CSV data as a table, which is some ungodly abstraction whose
%  only plus is that MATLAB can actually read such a file even if it contains
%  some nonnumeric data.  The so-called "CSVREAD" function cannot!
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  About to read the file "%s":\n', filename );

  T = readtable ( filename );
%
%  Extract the tag as a cell array.  
%  Because it is a cell array, tag I must be referenced as "tag{i}".
%
  tag = table2cell ( T(:,2) );
%
%  Extract the city names as a cell array.  
%  Because it is a cell array, city I must be referenced as "city{i}".
%
  city = table2cell ( T(:,3) );
%
%  Extract the longitude and latitude data as numeric arrays.
%
  dlon = table2array ( T(:,9) );
  mlon = table2array ( T(:,10) );
  slon = table2array ( T(:,11) );

  dlat = table2array ( T(:,5) );
  mlat = table2array ( T(:,6) );
  slat = table2array ( T(:,7) );
%
%  Convert from Degrees/Minutes/Seconds to real degrees.
%
  lon = - ( ( slon / 60.0 + mlon ) / 60.0 + dlon );
  lat =   ( ( slat / 60.0 + mlat ) / 60.0 + dlat );

  return
end

