/*
Time Analysis - Example 
This is an example script to be used once the OpenStreetMap data is in PostgreSQL.

This example will be focused on Nairobi in the year 2012.
Please note, every section of this analysis was re-run for each year (2008-2018) and for the other case study (Greater Manchester),
yet these will not be produced for conciseness. 

Original OSM data being used in this include:
kenya_2012 - snapshot extracted from full history dump using osmium
Nairobi data was extracted from this file.

Note: The temporal analysis did not incorporate MLR,  Global Moran's I, Local Moran's I or GWR.
*/

--Extract Nairobi data from the Kenya file using shapefiles
--NOTE: OSM data which extends constituency boundary (is not fully contained within a single constituency) is automatically removed at this stage
--CREATE A NEW TABLE ONLY INCLUDING NAIROBI EDITS 
CREATE TABLE nairobi2012_point AS (
	SELECT * FROM PUBLIC.kenya2012_point,
	PUBLIC.nairobi_consts WHERE st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.kenya2012_point.way)
	);

CREATE TABLE nairobi2012_line AS (
	SELECT * FROM PUBLIC.kenya2012_line,
	PUBLIC.nairobi_consts WHERE st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.kenya2012_line.way)
	);

CREATE TABLE nairobi2012_polygon AS (
	SELECT * FROM PUBLIC.kenya2012_polygon,
	PUBLIC.nairobi_consts WHERE st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.kenya2012_polygon.way)
	);

--Delete anonymous imports
--NOTE: None were present
DELETE
FROM nairobi2012_point
WHERE osm_uid = '0';

DELETE
FROM nairobi2012_line
WHERE osm_uid = '0';

DELETE
FROM nairobi2012_polygon
WHERE osm_uid = '0';

--BULK IMPORTS
--Check if bulk imports are present in each table 
--Results extracted to CSV file to formulate SQL scripts which will delete the necessary data 
SELECT osm_timestamp::DATE,
	osm_uid,
	count(*)
FROM PUBLIC.nairobi2012_point
GROUP BY osm_timestamp::DATE,
	osm_uid
ORDER BY count(*) DESC;

SELECT osm_timestamp::DATE,
	osm_uid,
	count(*)
FROM PUBLIC.nairobi2012_line
GROUP BY osm_timestamp::DATE,
	osm_uid
ORDER BY count(*) DESC;

SELECT osm_timestamp::DATE,
	osm_uid,
	count(*)
FROM PUBLIC.nairobi2012_polygon
GROUP BY osm_timestamp::DATE,
	osm_uid
ORDER BY count(*) DESC;

--Deleting the data associated with bulk imports
--Only necessary to remove imports from one table
DELETE
FROM nairobi2012_point
WHERE osm_uid = '215343'
	AND osm_timestamp::DATE = '2010-01-12'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-09-15'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-09-08'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-07-29'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-11'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-09-17'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-07-28'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-16'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-07-26'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-10-13'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-09'
	OR osm_uid = '18236'
	AND osm_timestamp::DATE = '2009-04-13'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-07-30'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-09-05'
	OR osm_uid = '232642'
	AND osm_timestamp::DATE = '2011-11-22'
	OR osm_uid = '18236'
	AND osm_timestamp::DATE = '2012-07-26'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-09-11'
	OR osm_uid = '18236'
	AND osm_timestamp::DATE = '2012-07-25'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-18'
	OR osm_uid = '18236'
	AND osm_timestamp::DATE = '2012-07-24'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-28'
	OR osm_uid = '408968'
	AND osm_timestamp::DATE = '2011-08-08';

--INFERRING LOCALS
--obtain the percentage of edits which are within Nairobi per user (out of all edits in Kenya)
CREATE TABLE n_2012_local AS (
	SELECT osm_uid,
	sum(coalesce(points_within, 0)) + sum(coalesce(line_within, 0)) + sum(coalesce(polygon_within, 0)) AS total_within,
	sum(coalesce(points_total, 0)) + sum(coalesce(line_total, 0)) + sum(coalesce(polygon_total, 0)) AS total_total,
	round((
			(
				SELECT sum(coalesce(points_within, 0)) + sum(coalesce(line_within, 0)) + sum(coalesce(polygon_within, 0))
				) / (
				(
					SELECT sum(coalesce(points_total, 0.0000000000000000000000001)) + sum(coalesce(line_total, 0.0000000000000000000001)) + sum(coalesce(polygon_total, 0.000000001))
					)
				) * 100
			), 2) AS PERCENT FROM (
	SELECT *
	FROM (
		SELECT osm_uid
		FROM PUBLIC.kenya2012_point
		
		UNION
		
		SELECT osm_uid
		FROM PUBLIC.kenya2012_line
		
		UNION
		
		SELECT osm_uid
		FROM PUBLIC.kenya2012_polygon
		) AS kosm_uid
	) tbl0 INNER JOIN (
	SELECT osm_uid AS point_osm_uid,
		count(*) AS points_within
	FROM kenya2012_point,
		nairobi_consts
	WHERE ST_within(kenya2012_point.way, nairobi_consts.geom)
	GROUP BY osm_uid
	) tbl1 ON tbl0.osm_uid = tbl1.point_osm_uid INNER JOIN (
	SELECT osm_uid AS osm_uid2,
		count(*) AS points_total
	FROM kenya2012_point
	GROUP BY osm_uid
	) tbl2 ON tbl0.osm_uid = tbl2.osm_uid2 FULL JOIN (
	SELECT osm_uid AS line_osm_uid,
		count(*) AS line_within
	FROM kenya2012_line,
		nairobi_consts
	WHERE ST_within(kenya2012_line.way, nairobi_consts.geom)
	GROUP BY osm_uid
	) tbl3 ON tbl0.osm_uid = tbl3.line_osm_uid FULL JOIN (
	SELECT osm_uid AS osm_uid2,
		count(*) AS line_total
	FROM kenya2012_line
	GROUP BY osm_uid
	) tbl4 ON tbl0.osm_uid = tbl4.osm_uid2 FULL JOIN (
	SELECT osm_uid AS polygon_osm_uid,
		count(*) AS polygon_within
	FROM kenya2012_polygon,
		nairobi_consts
	WHERE ST_within(kenya2012_polygon.way, nairobi_consts.geom)
	GROUP BY osm_uid
	) tbl5 ON tbl0.osm_uid = tbl5.polygon_osm_uid FULL JOIN (
	SELECT osm_uid AS osm_uid2,
		count(*) AS polygon_total
	FROM kenya2012_polygon
	GROUP BY osm_uid
	) tbl6 ON tbl0.osm_uid = tbl6.osm_uid2 GROUP BY tbl0.osm_uid,
	tbl1.point_osm_uid,
	tbl1.points_within,
	tbl2.osm_uid2,
	tbl2.points_total,
	tbl3.line_osm_uid,
	tbl3.line_within,
	tbl4.osm_uid2,
	tbl4.line_total,
	tbl5.polygon_osm_uid,
	tbl5.polygon_within,
	tbl6.osm_uid2,
	tbl6.polygon_total ORDER BY PERCENT DESC
	);

--Obtain inferred local UID's from the resulting table (>=70% edits in Nairobi)
--Data extracted to CSV to create scripts to create final tables for analysis
SELECT osm_uid,
	PERCENT
FROM n_2012_local
WHERE osm_uid IS NOT NULL
	AND PERCENT BETWEEN 70
		AND 100;

--Create a POI table from the point table
--This removes the nodes (point data) associated with the vertices of the line & polygon features
--create POI table 
CREATE TABLE nairobi2012_poi AS (
	SELECT * FROM nairobi2012_point WHERE "access" IS NOT NULL
	OR "addr:houseconstituen" IS NOT NULL
	OR "addr:housenumber" IS NOT NULL
	OR "addr:place" IS NOT NULL
	OR "addr:city" IS NOT NULL
	OR "addr:country" IS NOT NULL
	OR "addr:interpolation" IS NOT NULL
	OR admin_level IS NOT NULL
	OR aerialway IS NOT NULL
	OR aeroway IS NOT NULL
	OR amenity IS NOT NULL
	OR area IS NOT NULL
	OR barrier IS NOT NULL
	OR bicycle IS NOT NULL
	OR brand IS NOT NULL
	OR bridge IS NOT NULL
	OR boundary IS NOT NULL
	OR building IS NOT NULL
	OR capital IS NOT NULL
	OR construction IS NOT NULL
	OR covered IS NOT NULL
	OR culvert IS NOT NULL
	OR cutting IS NOT NULL
	OR denomination IS NOT NULL
	OR disused IS NOT NULL
	OR ele IS NOT NULL
	OR embankment IS NOT NULL
	OR foot IS NOT NULL
	OR "generator:source" IS NOT NULL
	OR harbour IS NOT NULL
	OR highway IS NOT NULL
	OR historic IS NOT NULL
	OR horse IS NOT NULL
	OR intermittent IS NOT NULL
	OR junction IS NOT NULL
	OR landuse IS NOT NULL
	OR layer IS NOT NULL
	OR leisure IS NOT NULL
	OR "lock" IS NOT NULL
	OR man_made IS NOT NULL
	OR military IS NOT NULL
	OR motorcar IS NOT NULL
	OR constituen IS NOT NULL
	OR "natural" IS NOT NULL
	OR office IS NOT NULL
	OR oneway IS NOT NULL
	OR "operator" IS NOT NULL
	OR place IS NOT NULL
	OR population IS NOT NULL
	OR "power" IS NOT NULL
	OR power_source IS NOT NULL
	OR public_transport IS NOT NULL
	OR railway IS NOT NULL
	OR "ref" IS NOT NULL
	OR religion IS NOT NULL
	OR route IS NOT NULL
	OR service IS NOT NULL
	OR shop IS NOT NULL
	OR sport IS NOT NULL
	OR surface IS NOT NULL
	OR toll IS NOT NULL
	OR tourism IS NOT NULL
	OR "tower:type" IS NOT NULL
	OR tunnel IS NOT NULL
	OR water IS NOT NULL
	OR waterway IS NOT NULL
	OR wetland IS NOT NULL
	OR width IS NOT NULL
	OR wood IS NOT NULL
	);

--Creating the final tables for analysis
--This extracts only the edits created by the inferred locals (see above)
CREATE TABLE n2012_final_poi AS (
	SELECT * FROM nairobi2012_poi WHERE osm_uid = '152289'
	OR osm_uid = '16315'
	OR osm_uid = '338611'
	OR osm_uid = '109852'
	OR osm_uid = '114161'
	OR osm_uid = '116004'
	OR osm_uid = '121415'
	OR osm_uid = '123574'
	OR osm_uid = '12614'
	OR osm_uid = '129447'
	OR osm_uid = '1295'
	OR osm_uid = '131735'
	OR osm_uid = '151957'
	OR osm_uid = '170672'
	OR osm_uid = '18069'
	OR osm_uid = '191112'
	OR osm_uid = '192021'
	OR osm_uid = '19239'
	OR osm_uid = '193865'
	OR osm_uid = '19862'
	OR osm_uid = '20163'
	OR osm_uid = '202858'
	OR osm_uid = '207249'
	OR osm_uid = '215343'
	OR osm_uid = '216549'
	OR osm_uid = '22108'
	OR osm_uid = '227137'
	OR osm_uid = '230870'
	OR osm_uid = '232312'
	OR osm_uid = '243041'
	OR osm_uid = '252326'
	OR osm_uid = '25504'
	OR osm_uid = '261003'
	OR osm_uid = '261018'
	OR osm_uid = '261052'
	OR osm_uid = '265690'
	OR osm_uid = '28052'
	OR osm_uid = '283266'
	OR osm_uid = '286581'
	OR osm_uid = '297047'
	OR osm_uid = '297215'
	OR osm_uid = '301356'
	OR osm_uid = '310392'
	OR osm_uid = '31075'
	OR osm_uid = '324047'
	OR osm_uid = '33073'
	OR osm_uid = '336481'
	OR osm_uid = '344031'
	OR osm_uid = '346151'
	OR osm_uid = '352832'
	OR osm_uid = '355242'
	OR osm_uid = '355617'
	OR osm_uid = '3585'
	OR osm_uid = '383494'
	OR osm_uid = '383550'
	OR osm_uid = '384773'
	OR osm_uid = '398864'
	OR osm_uid = '400795'
	OR osm_uid = '413785'
	OR osm_uid = '415989'
	OR osm_uid = '42429'
	OR osm_uid = '425742'
	OR osm_uid = '42619'
	OR osm_uid = '429923'
	OR osm_uid = '429943'
	OR osm_uid = '429994'
	OR osm_uid = '433827'
	OR osm_uid = '435017'
	OR osm_uid = '43972'
	OR osm_uid = '440940'
	OR osm_uid = '44659'
	OR osm_uid = '451142'
	OR osm_uid = '474505'
	OR osm_uid = '474570'
	OR osm_uid = '474572'
	OR osm_uid = '474574'
	OR osm_uid = '474600'
	OR osm_uid = '477692'
	OR osm_uid = '488491'
	OR osm_uid = '488646'
	OR osm_uid = '494109'
	OR osm_uid = '508796'
	OR osm_uid = '51045'
	OR osm_uid = '513716'
	OR osm_uid = '513730'
	OR osm_uid = '514275'
	OR osm_uid = '514279'
	OR osm_uid = '514285'
	OR osm_uid = '514287'
	OR osm_uid = '514329'
	OR osm_uid = '5211'
	OR osm_uid = '521191'
	OR osm_uid = '521872'
	OR osm_uid = '530154'
	OR osm_uid = '531109'
	OR osm_uid = '535126'
	OR osm_uid = '5359'
	OR osm_uid = '547959'
	OR osm_uid = '564222'
	OR osm_uid = '564773'
	OR osm_uid = '56597'
	OR osm_uid = '5735'
	OR osm_uid = '64805'
	OR osm_uid = '67244'
	OR osm_uid = '68842'
	OR osm_uid = '69102'
	OR osm_uid = '76114'
	OR osm_uid = '76471'
	OR osm_uid = '9112'
	OR osm_uid = '95383'
	OR osm_uid = '6494'
	OR osm_uid = '562199'
	OR osm_uid = '393385'
	OR osm_uid = '4902'
	OR osm_uid = '513735'
	OR osm_uid = '261009'
	OR osm_uid = '22726'
	OR osm_uid = '5891'
	OR osm_uid = '21640'
	OR osm_uid = '408968'
	OR osm_uid = '261053'
	OR osm_uid = '18675'
	OR osm_uid = '261050'
	OR osm_uid = '527748'
	OR osm_uid = '261848'
	OR osm_uid = '345885'
	OR osm_uid = '540100'
	OR osm_uid = '9839'
	OR osm_uid = '347165'
	OR osm_uid = '429920'
	OR osm_uid = '7079'
	OR osm_uid = '22301'
	OR osm_uid = '30587'
	OR osm_uid = '51324'
	OR osm_uid = '70260'
	OR osm_uid = '559500'
	OR osm_uid = '514293'
	OR osm_uid = '44221'
	OR osm_uid = '385790'
	OR osm_uid = '540114'
	OR osm_uid = '236134'
	OR osm_uid = '64578'
	OR osm_uid = '513724'
	OR osm_uid = '165869'
	OR osm_uid = '514291'
	OR osm_uid = '236381'
	OR osm_uid = '293774'
	OR osm_uid = '110263'
	OR osm_uid = '999'
	OR osm_uid = '67862'
	OR osm_uid = '261043'
	OR osm_uid = '392442'
	OR osm_uid = '92274'
	OR osm_uid = '314635'
	OR osm_uid = '229494'
	OR osm_uid = '438201'
	);

CREATE TABLE n2012_final_line AS (
	SELECT * FROM nairobi2012_line WHERE osm_uid = '152289'
	OR osm_uid = '16315'
	OR osm_uid = '338611'
	OR osm_uid = '109852'
	OR osm_uid = '114161'
	OR osm_uid = '116004'
	OR osm_uid = '121415'
	OR osm_uid = '123574'
	OR osm_uid = '12614'
	OR osm_uid = '129447'
	OR osm_uid = '1295'
	OR osm_uid = '131735'
	OR osm_uid = '151957'
	OR osm_uid = '170672'
	OR osm_uid = '18069'
	OR osm_uid = '191112'
	OR osm_uid = '192021'
	OR osm_uid = '19239'
	OR osm_uid = '193865'
	OR osm_uid = '19862'
	OR osm_uid = '20163'
	OR osm_uid = '202858'
	OR osm_uid = '207249'
	OR osm_uid = '215343'
	OR osm_uid = '216549'
	OR osm_uid = '22108'
	OR osm_uid = '227137'
	OR osm_uid = '230870'
	OR osm_uid = '232312'
	OR osm_uid = '243041'
	OR osm_uid = '252326'
	OR osm_uid = '25504'
	OR osm_uid = '261003'
	OR osm_uid = '261018'
	OR osm_uid = '261052'
	OR osm_uid = '265690'
	OR osm_uid = '28052'
	OR osm_uid = '283266'
	OR osm_uid = '286581'
	OR osm_uid = '297047'
	OR osm_uid = '297215'
	OR osm_uid = '301356'
	OR osm_uid = '310392'
	OR osm_uid = '31075'
	OR osm_uid = '324047'
	OR osm_uid = '33073'
	OR osm_uid = '336481'
	OR osm_uid = '344031'
	OR osm_uid = '346151'
	OR osm_uid = '352832'
	OR osm_uid = '355242'
	OR osm_uid = '355617'
	OR osm_uid = '3585'
	OR osm_uid = '383494'
	OR osm_uid = '383550'
	OR osm_uid = '384773'
	OR osm_uid = '398864'
	OR osm_uid = '400795'
	OR osm_uid = '413785'
	OR osm_uid = '415989'
	OR osm_uid = '42429'
	OR osm_uid = '425742'
	OR osm_uid = '42619'
	OR osm_uid = '429923'
	OR osm_uid = '429943'
	OR osm_uid = '429994'
	OR osm_uid = '433827'
	OR osm_uid = '435017'
	OR osm_uid = '43972'
	OR osm_uid = '440940'
	OR osm_uid = '44659'
	OR osm_uid = '451142'
	OR osm_uid = '474505'
	OR osm_uid = '474570'
	OR osm_uid = '474572'
	OR osm_uid = '474574'
	OR osm_uid = '474600'
	OR osm_uid = '477692'
	OR osm_uid = '488491'
	OR osm_uid = '488646'
	OR osm_uid = '494109'
	OR osm_uid = '508796'
	OR osm_uid = '51045'
	OR osm_uid = '513716'
	OR osm_uid = '513730'
	OR osm_uid = '514275'
	OR osm_uid = '514279'
	OR osm_uid = '514285'
	OR osm_uid = '514287'
	OR osm_uid = '514329'
	OR osm_uid = '5211'
	OR osm_uid = '521191'
	OR osm_uid = '521872'
	OR osm_uid = '530154'
	OR osm_uid = '531109'
	OR osm_uid = '535126'
	OR osm_uid = '5359'
	OR osm_uid = '547959'
	OR osm_uid = '564222'
	OR osm_uid = '564773'
	OR osm_uid = '56597'
	OR osm_uid = '5735'
	OR osm_uid = '64805'
	OR osm_uid = '67244'
	OR osm_uid = '68842'
	OR osm_uid = '69102'
	OR osm_uid = '76114'
	OR osm_uid = '76471'
	OR osm_uid = '9112'
	OR osm_uid = '95383'
	OR osm_uid = '6494'
	OR osm_uid = '562199'
	OR osm_uid = '393385'
	OR osm_uid = '4902'
	OR osm_uid = '513735'
	OR osm_uid = '261009'
	OR osm_uid = '22726'
	OR osm_uid = '5891'
	OR osm_uid = '21640'
	OR osm_uid = '408968'
	OR osm_uid = '261053'
	OR osm_uid = '18675'
	OR osm_uid = '261050'
	OR osm_uid = '527748'
	OR osm_uid = '261848'
	OR osm_uid = '345885'
	OR osm_uid = '540100'
	OR osm_uid = '9839'
	OR osm_uid = '347165'
	OR osm_uid = '429920'
	OR osm_uid = '7079'
	OR osm_uid = '22301'
	OR osm_uid = '30587'
	OR osm_uid = '51324'
	OR osm_uid = '70260'
	OR osm_uid = '559500'
	OR osm_uid = '514293'
	OR osm_uid = '44221'
	OR osm_uid = '385790'
	OR osm_uid = '540114'
	OR osm_uid = '236134'
	OR osm_uid = '64578'
	OR osm_uid = '513724'
	OR osm_uid = '165869'
	OR osm_uid = '514291'
	OR osm_uid = '236381'
	OR osm_uid = '293774'
	OR osm_uid = '110263'
	OR osm_uid = '999'
	OR osm_uid = '67862'
	OR osm_uid = '261043'
	OR osm_uid = '392442'
	OR osm_uid = '92274'
	OR osm_uid = '314635'
	OR osm_uid = '229494'
	OR osm_uid = '438201'
	);

CREATE TABLE n2012_final_polygon AS (
	SELECT * FROM nairobi2012_polygon WHERE osm_uid = '152289'
	OR osm_uid = '16315'
	OR osm_uid = '338611'
	OR osm_uid = '109852'
	OR osm_uid = '114161'
	OR osm_uid = '116004'
	OR osm_uid = '121415'
	OR osm_uid = '123574'
	OR osm_uid = '12614'
	OR osm_uid = '129447'
	OR osm_uid = '1295'
	OR osm_uid = '131735'
	OR osm_uid = '151957'
	OR osm_uid = '170672'
	OR osm_uid = '18069'
	OR osm_uid = '191112'
	OR osm_uid = '192021'
	OR osm_uid = '19239'
	OR osm_uid = '193865'
	OR osm_uid = '19862'
	OR osm_uid = '20163'
	OR osm_uid = '202858'
	OR osm_uid = '207249'
	OR osm_uid = '215343'
	OR osm_uid = '216549'
	OR osm_uid = '22108'
	OR osm_uid = '227137'
	OR osm_uid = '230870'
	OR osm_uid = '232312'
	OR osm_uid = '243041'
	OR osm_uid = '252326'
	OR osm_uid = '25504'
	OR osm_uid = '261003'
	OR osm_uid = '261018'
	OR osm_uid = '261052'
	OR osm_uid = '265690'
	OR osm_uid = '28052'
	OR osm_uid = '283266'
	OR osm_uid = '286581'
	OR osm_uid = '297047'
	OR osm_uid = '297215'
	OR osm_uid = '301356'
	OR osm_uid = '310392'
	OR osm_uid = '31075'
	OR osm_uid = '324047'
	OR osm_uid = '33073'
	OR osm_uid = '336481'
	OR osm_uid = '344031'
	OR osm_uid = '346151'
	OR osm_uid = '352832'
	OR osm_uid = '355242'
	OR osm_uid = '355617'
	OR osm_uid = '3585'
	OR osm_uid = '383494'
	OR osm_uid = '383550'
	OR osm_uid = '384773'
	OR osm_uid = '398864'
	OR osm_uid = '400795'
	OR osm_uid = '413785'
	OR osm_uid = '415989'
	OR osm_uid = '42429'
	OR osm_uid = '425742'
	OR osm_uid = '42619'
	OR osm_uid = '429923'
	OR osm_uid = '429943'
	OR osm_uid = '429994'
	OR osm_uid = '433827'
	OR osm_uid = '435017'
	OR osm_uid = '43972'
	OR osm_uid = '440940'
	OR osm_uid = '44659'
	OR osm_uid = '451142'
	OR osm_uid = '474505'
	OR osm_uid = '474570'
	OR osm_uid = '474572'
	OR osm_uid = '474574'
	OR osm_uid = '474600'
	OR osm_uid = '477692'
	OR osm_uid = '488491'
	OR osm_uid = '488646'
	OR osm_uid = '494109'
	OR osm_uid = '508796'
	OR osm_uid = '51045'
	OR osm_uid = '513716'
	OR osm_uid = '513730'
	OR osm_uid = '514275'
	OR osm_uid = '514279'
	OR osm_uid = '514285'
	OR osm_uid = '514287'
	OR osm_uid = '514329'
	OR osm_uid = '5211'
	OR osm_uid = '521191'
	OR osm_uid = '521872'
	OR osm_uid = '530154'
	OR osm_uid = '531109'
	OR osm_uid = '535126'
	OR osm_uid = '5359'
	OR osm_uid = '547959'
	OR osm_uid = '564222'
	OR osm_uid = '564773'
	OR osm_uid = '56597'
	OR osm_uid = '5735'
	OR osm_uid = '64805'
	OR osm_uid = '67244'
	OR osm_uid = '68842'
	OR osm_uid = '69102'
	OR osm_uid = '76114'
	OR osm_uid = '76471'
	OR osm_uid = '9112'
	OR osm_uid = '95383'
	OR osm_uid = '6494'
	OR osm_uid = '562199'
	OR osm_uid = '393385'
	OR osm_uid = '4902'
	OR osm_uid = '513735'
	OR osm_uid = '261009'
	OR osm_uid = '22726'
	OR osm_uid = '5891'
	OR osm_uid = '21640'
	OR osm_uid = '408968'
	OR osm_uid = '261053'
	OR osm_uid = '18675'
	OR osm_uid = '261050'
	OR osm_uid = '527748'
	OR osm_uid = '261848'
	OR osm_uid = '345885'
	OR osm_uid = '540100'
	OR osm_uid = '9839'
	OR osm_uid = '347165'
	OR osm_uid = '429920'
	OR osm_uid = '7079'
	OR osm_uid = '22301'
	OR osm_uid = '30587'
	OR osm_uid = '51324'
	OR osm_uid = '70260'
	OR osm_uid = '559500'
	OR osm_uid = '514293'
	OR osm_uid = '44221'
	OR osm_uid = '385790'
	OR osm_uid = '540114'
	OR osm_uid = '236134'
	OR osm_uid = '64578'
	OR osm_uid = '513724'
	OR osm_uid = '165869'
	OR osm_uid = '514291'
	OR osm_uid = '236381'
	OR osm_uid = '293774'
	OR osm_uid = '110263'
	OR osm_uid = '999'
	OR osm_uid = '67862'
	OR osm_uid = '261043'
	OR osm_uid = '392442'
	OR osm_uid = '92274'
	OR osm_uid = '314635'
	OR osm_uid = '229494'
	OR osm_uid = '438201'
	);

--OpenStreetMap Statistics 
--The same statistics used as dependent variables in the non-spatial and spatial analysis of this dissertation
--Temporal analysis conducted at constituency level and then later aggregated to city level 
--Individual queries for easier interpret-ability
SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS school
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'school'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS college
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'college'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS pub
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'pub'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS bar
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'bar'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS pharmacy
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'pharmacy'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS hospital
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'hospital'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS dentist
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'dentist'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS clinic
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'clinic'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS police
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'police'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS bank
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'bank'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS atm
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'atm'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS restaurant
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'restaurant'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS fast_food
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'fast_food'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS toilets
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'toilets'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS drinking_water
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'drinking_water'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS place_of_worship
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.amenity = 'place_of_worship'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS bus_stop
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.highway = 'bus_stop'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS street_lamp
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
WHERE PUBLIC.n2012_final_poi.highway = 'street_lamp'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS hotel
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.tourism = 'hotel'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS industrial
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.building = 'industrial'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS apartment
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.building = 'apartments'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS house
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.building = 'house'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS church
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.building = 'church'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS mosque
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
WHERE PUBLIC.n2012_final_polygon.building = 'mosque'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_line.way) AS footway
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
WHERE PUBLIC.n2012_final_line.highway = 'footway'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_line.way) AS primary_road
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
WHERE PUBLIC.n2012_final_line.highway = 'primary'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_line.way) AS residential
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
WHERE PUBLIC.n2012_final_line.highway = 'residential'
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_line.way) AS unclassified
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
WHERE PUBLIC.n2012_final_line.highway = 'unclassified'
GROUP BY PUBLIC.nairobi_consts.constituen;

--Additional statistics
--Total poi, line and polygon counts
--Total edit count was calculated by summing the totals for poi, line and polygon per constituency 
SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_poi.way) AS poi_total
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_line.way) AS line_total
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
GROUP BY PUBLIC.nairobi_consts.constituen;

SELECT PUBLIC.nairobi_consts.constituen,
	count(PUBLIC.n2012_final_polygon.way) AS polygon_total
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
GROUP BY PUBLIC.nairobi_consts.constituen;

--Calculate the number of 'new' (version 1) edits
--version 1
SELECT tbl0.constituen,
	tbl1.poi_total,
	tbl2.line_total,
	tbl3.poly_total,
	sum(coalesce(tbl1.poi_total, 0)) + sum(coalesce(tbl2.line_total, 0)) + sum(coalesce(tbl3.poly_total, 0)) AS overall_v1_count
FROM (
	(
		SELECT constituen
		FROM nairobi_consts
		) tbl0 FULL JOIN (
		SELECT PUBLIC.nairobi_consts.constituen,
			count(PUBLIC.n2012_final_poi.way) AS poi_total
		FROM PUBLIC.nairobi_consts
		LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way)
		WHERE PUBLIC.n2012_final_poi.osm_version = '1'
		GROUP BY PUBLIC.nairobi_consts.constituen
		) tbl1 ON tbl0.constituen = tbl1.constituen
	FULL JOIN (
		SELECT PUBLIC.nairobi_consts.constituen,
			count(PUBLIC.n2012_final_line.way) AS line_total
		FROM PUBLIC.nairobi_consts
		LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way)
		WHERE PUBLIC.n2012_final_line.osm_version = '1'
		GROUP BY PUBLIC.nairobi_consts.constituen
		) tbl2 ON tbl0.constituen = tbl2.constituen
	FULL JOIN (
		SELECT PUBLIC.nairobi_consts.constituen,
			count(PUBLIC.n2012_final_polygon.way) AS poly_total
		FROM PUBLIC.nairobi_consts
		LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way)
		WHERE PUBLIC.n2012_final_polygon.osm_version = '1'
		GROUP BY PUBLIC.nairobi_consts.constituen
		) tbl3 ON tbl0.constituen = tbl3.constituen
	)
GROUP BY tbl0.constituen,
	tbl1.poi_total,
	tbl2.line_total,
	tbl3.poly_total;

--Number of distinct users per constituency 
--Create a table to store the results of the intermediate queries
CREATE TABLE n_uid_2012 (
	osm_uid VARCHAR(80),
	constituen VARCHAR(80)
	);

--Insert the osm_uid's per edit and the constituency in which that edit is contained into the previously created table
--Repeat for poi, line and polygon 
INSERT INTO n_uid_2012 (
	osm_uid,
	constituen
	)
SELECT PUBLIC.n2012_final_poi.osm_uid,
	nairobi_consts.constituen
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_poi ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_poi.way);

INSERT INTO n_uid_2012 (
	osm_uid,
	constituen
	)
SELECT PUBLIC.n2012_final_line.osm_uid,
	nairobi_consts.constituen
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_line ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_line.way);

INSERT INTO n_uid_2012 (
	osm_uid,
	constituen
	)
SELECT PUBLIC.n2012_final_polygon.osm_uid,
	nairobi_consts.constituen
FROM PUBLIC.nairobi_consts
LEFT JOIN PUBLIC.n2012_final_polygon ON st_contains(PUBLIC.nairobi_consts.geom, PUBLIC.n2012_final_polygon.way);

--Obtain distinct osm_uid's per constituency
SELECT count(DISTINCT osm_uid),
	constituen
FROM n_uid_2012
GROUP BY constituen;
