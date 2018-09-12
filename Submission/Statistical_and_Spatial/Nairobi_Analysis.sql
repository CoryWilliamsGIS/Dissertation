--Ward Analysis
--Nairobi

--Using individual queries, rather than a join


--More exploratory queries are not repeated here as were included in the other case study and similar adapted

--total points in each ward, group by ward name
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

--distinct users 
--export as csv
--repeat for the other tables
SELECT public.nairobi_wards_polys.name, string_agg(distinct public.nairobi_final_poi3.osm_uid, ' ') AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name 

--total line in each ward, group by ward name
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_line3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_line3
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_line3.way) 
GROUP BY public.nairobi_wards_polys.name;

--total polygon in each ward, group by ward name
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_polygon3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_polygon3
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

--exactly which points are within each ward
--repeat for line, poly
SELECT
t1.osm_id as "id",
t2.name as "Ward Name"
FROM public.nairobi_final_poi3 t1
JOIN public.nairobi_wards_polys t2 ON (ST_Within(t1.way, t2.geom))
WHERE t1.type = 1
AND t2.type = 3;

--select count of version 1 point edits, group by ward
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3.osm_version = '1'
GROUP BY public.nairobi_wards_polys.name;	

--alternative of above
--select count of version 1 point edits, group by ward
--this can be repeated for which ever column
SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.way) AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 WHERE public.nairobi_final_poi3.osm_version = '1' AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

--select count of version 1 line edits, group by ward
SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_line3.way) AS total 
FROM nairobi_wards_polys, public.nairobi_final_line3 WHERE public.nairobi_final_line3.osm_version = '1' AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_line3.way) 
GROUP BY public.nairobi_wards_polys.name;	


--select count of distinct osm_uids
SELECT public.nairobi_wards_polys.name, count(distinct public.nairobi_final_polygon3.osm_uid) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_polygon3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way)  
GROUP BY public.nairobi_wards_polys.name;

--building count
SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_polygon3.*) AS total 
FROM nairobi_wards_polys, public.nairobi_final_polygon3 
WHERE public.nairobi_final_polygon3.building is not null and building != 'no' AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

--building editor count
SELECT nairobi_wards_polys.name AS name, count(distinct public.nairobi_final_polygon3.osm_uid) AS total 
FROM nairobi_wards_polys, public.nairobi_final_polygon3 
WHERE public.nairobi_final_polygon3.building is not null and building != 'no' AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

--poi with corresponding polygon 
SELECT nairobi_wards_polys.name AS name, count(distinct public.nairobi_final_poi3.osm_id)   
FROM nairobi_wards_polys, public.nairobi_final_poi3, public.nairobi_final_polygon3 
WHERE st_within(nairobi_final_poi3.way, nairobi_final_polygon3.way) AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

--count of null amenities
--divide by total number of poi's and * 100 to get % null
SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.*) AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE public.nairobi_final_poi3.amenity is null AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

--building count with low level addresses
SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_polygon3.*) AS total 
FROM nairobi_wards_polys, public.nairobi_final_polygon3 
WHERE public.nairobi_final_polygon3.building is not null and building != 'no' and ("addr:housenumber" is not null or "addr:housename" is not null or "addr:place" is not null) AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;


--Amenity Frequency
create table test as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity)
AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity
);

create table test1 as (
SELECT
  * 
FROM (
  SELECT
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY total desc) AS r,
    t.*
  FROM
    test t) x
WHERE
  x.r <= 1
	);

select amenity, count(amenity) from test1
group by test1.amenity
order by count(amenity) desc;








-----------------

--somewhat near to what i wanted....
SELECT public.nairobi_wards_polys.name, public.nairobi_final_poi3."access", count(*) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3."access" is not null
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3."access"
order by count(*) desc



--also works
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.*) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3."amenity" is not null
GROUP BY public.nairobi_wards_polys.name
order by count(*) desc;

---------------
--ROUGH WORK
---------------

SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.amenity) AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE  ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity;



SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3.osm_version = '1'
GROUP BY public.nairobi_wards_polys.name;

select "access", count(*) from manc_final_polygon3 where "access" is not null
group by "access"
order by count desc

SELECT public.nairobi_wards_polys.name, public.nairobi_final_poi3."access", count(*) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3.osm_version = '1'
GROUP BY public.nairobi_wards_polys.name;


SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.*) AS total 
FROM nairobi_wards_polys, public.nairobi_final_poli3 
WHERE public.nairobi_final_poi3.amenity is null and building AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_polygon3.*) AS total 
FROM nairobi_wards_polys, public.nairobi_final_polygon3 
WHERE public.nairobi_final_polygon3.building is not null and building != 'no' and ("addr:housenumber" is not null or "addr:housename" is not null or "addr:street" is not null AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

SELECT COUNT (*) FROM nairobi_final_polygon3 where building is not null and building != 'no' and osm_id > 0 and ("addr:housenumber" is not null or "addr:housename" is not null or "addr:street" is not null);

(SELECT
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3 WHERE amenity is null)
as null_total) /		
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3)
*100)
as percent_null

(SELECT
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3 )
as null_total) /		
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3 WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way))
*100)
as percent_null


select osm_user, count(osm_user) as Buildings_Edited from nairobi_polygon_final where building is not null and building != 'no'
group by osm_user order by Buildings_Edited desc

SELECT nairobi_wards_polys.name AS name, count(distinct public.nairobi_final_poi3.osm_id)   
FROM nairobi_wards_polys, public.nairobi_final_poi3, public.nairobi_final_polygon3 
WHERE st_within(nairobi_final_poi3.way, nairobi_final_polygon3.way) AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name;

select count(distinct nairobi_point.osm_id) from nairobi_point, nairobi_polygon where st_within(nairobi_point.way, nairobi_polygon.way)


 
--exactly which points are within each ward
SELECT(
(SELECT
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3 WHERE osm_version = '1')+
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_line3 WHERE osm_version = '1')+
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_polygon3 WHERE osm_version = '1')
as new_total) /
(SELECT		
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_poi3)+
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_line3)+
(SELECT COUNT(*)::FLOAT FROM public.nairobi_final_polygon3)) 
*100)
as percent_new


--total points in each ward, group by ward name
SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

SELECT count(nairobi_final_poi3.*) 
FROM
public.nairobi_final_poi3,
public.nairobi_wards_polys
WHERE
ST_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) and nairobi_final_poi3.osm_version = '1';





-- Which meeting points are located within a carpark?

--SELECT UpdateGeometrySRID('nairobi_wards_newcs_name_airbase','geom',3857);
select 
  a.osm_id as "id", 
  b.name as "ward Name" 
from 
  public.nairobi_final_poi3 a, public.nairobi_wards_newcs_name_airbase b
where 
  st_within(a.way, b.geom) = 't';
  
  
 nairobi_wards_newcs_name_airbase
 

	
	
SELECT i.osm_id
FROM public.nairobi_final_poi3 AS i
JOIN public.nairobi_wards_newcs_name_airbase AS z ON name = 'AIRBASE' AND ST_Within(i.way, z.geom)


SELECT public.nairobi_wards_polys.geom, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.geom;

SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.way) AS total, 
 FROM nairobi_wards_polys, public.nairobi_final_poi3 WHERE nairobi_final_poi3.osm_version = '1'  AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 

UNION SELECT way AS way, amenity AS type, leisure AS type_b FROM planet_osm_polygon WHERE (amenity IN ('bar', 'nightclub', 'biergarten', 'pub', 'restaurant') OR leisure IN ('bandstand', 'dance')) AND building IS NOT NULL UNION SELECT pol.way AS way, pt.amenity AS type, pt.leisure AS type_b FROM planet_osm_polygon pol, planet_osm_point pt WHERE pol.building IS NOT NULL AND pt.amenity IN ('cafe') AND (pt.tags->'opening_hours') similar to '%-(2[1234]|0?[012345]):%' AND ST_Contains(pol.way, pt.way) UNION SELECT way AS way, amenity AS type, leisure AS type_b FROM planet_osm_polygon WHERE amenity IN ('cafe') AND building IS NOT NULL AND (tags->'opening_hours') similar to '%-(2[1234]|0?[012345]):%') AS data"
"


SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3.osm_version = '1'
GROUP BY public.nairobi_wards_polys.name;

SELECT nairobi_wards_polys.name AS name, count(public.nairobi_final_poi3.way) AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 WHERE public.nairobi_final_poi3.osm_version = '1' AND ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;



SELECT nairobi_wards_polys.name AS name from public.nairobi_wards_polys,
(select count(distinct osm_uid)
from (
    select osm_uid from public.nairobi_final_poi3
    union 
	select osm_uid from public.nairobi_final_line3
	union
	select osm_uid from public.nairobi_final_polygon3
) as userid)
where ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;







select public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity), count(distinct public.nairobi_final_poi3.amenity) from public.nairobi_final_poi3
join lateral (
    select nairobi_wards_polys.name from nairobi_wards_polys
    WHERE  ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
    order by popularity desc
    limit 1
) p on true
order by category.id;







SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity), 
row_number() over (partition by nairobi_wards_polys.name order by count(amenity)) 
AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE  ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity

SELECT *
FROM  (
   SELECT nairobi_wards_polys.name, row_number() OVER (PARTITION BY store_id ORDER BY employee_id) AS rn
   FROM   employees
   WHERE  currently_employed
   ) e
JOIN   stores s USING (store_id)
WHERE  rn <= 15
ORDER  BY store_id, e.rn;

select depname, empno, salary, row_number() over (partition by depname order by salary) as rownum from empsalary
          ) tmp
		  
select * from (	  
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity), 
AS total 
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
WHERE public.nairobi_final_poi3."access" is not null
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity
--order by count(*) desc
)
-------------------------------------------------








--ward analysis
--Manchester

--total points in each ward, group by ward name
SELECT public.manchester_ward_polys.name, count(public.manchester_final_poi3.way) AS total
FROM public.manchester_ward_polys LEFT JOIN public.manchester_final_poi3 
ON st_contains(public.manchester_ward_polys.geom, public.manchester_final_poi3.way) 
GROUP BY public.manchester_ward_polys.name;

--total line in each ward, group by ward name
SELECT public.manchester_ward_polys.name, count(public.manc_final_line3.way) AS total
FROM public.manchester_ward_polys LEFT JOIN public.manc_final_line3
ON st_contains(public.manchester_ward_polys.geom, public.manc_final_line3.way) 
GROUP BY public.manchester_ward_polys.name;

--total polygon in each ward, group by ward name
SELECT public.manchester_ward_polys.name, count(public.manc_final_polygon3.way) AS total
FROM public.manchester_ward_polys LEFT JOIN public.manc_final_polygon3
ON st_contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;

--exactly which points are within each ward
--repeat for line, poly
SELECT
t1.osm_id as "id",
t2.name as "Ward Name"
FROM public.manchester_final_poi3 t1
JOIN public.manchester_ward_polys t2 ON (ST_Within(t1.way, t2.geom))
WHERE t1.type = 1
AND t2.type = 3;

--select count of version 1 point edits, group by ward
SELECT public.manchester_ward_polys.name, count(public.manchester_final_poi3.way) AS total
FROM public.manchester_ward_polys LEFT JOIN public.manchester_final_poi3 
ON st_contains(public.manchester_ward_polys.geom, public.manchester_final_poi3.way) 
WHERE public.manchester_final_poi3.osm_version = '1'
GROUP BY public.manchester_ward_polys.name;	

--alternative of above
--select count of version 1 point edits, group by ward
--this can be repeated for which ever column
SELECT manchester_ward_polys.name AS name, count(public.manchester_final_poi3.way) AS total 
FROM manchester_ward_polys, public.manchester_final_poi3 WHERE public.manchester_final_poi3.osm_version = '1' AND ST_Contains(public.manchester_ward_polys.geom, public.manchester_final_poi3.way) 
GROUP BY public.manchester_ward_polys.name;

--select count of version 1 line edits, group by ward
SELECT manchester_ward_polys.name AS name, count(public.manc_final_line3.way) AS total 
FROM manchester_ward_polys, public.manc_final_line3 WHERE public.manc_final_line3.osm_version = '1' AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_line3.way) 
GROUP BY public.manchester_ward_polys.name;	

--select count of version 1 polygon edits, group by ward
SELECT manchester_ward_polys.name AS name, count(public.manc_final_polygon3.way) AS total 
FROM manchester_ward_polys, public.manc_final_polygon3 WHERE public.manc_final_polygon3.osm_version = '1' AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;	


--select count of distinct osm_uids
SELECT public.manchester_ward_polys.name, count(distinct public.manc_final_polygon3.osm_uid) AS total
FROM public.manchester_ward_polys LEFT JOIN public.manc_final_polygon3 
ON st_contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way)  
GROUP BY public.manchester_ward_polys.name;

--building count
SELECT manchester_ward_polys.name AS name, count(public.manc_final_polygon3.*) AS total 
FROM manchester_ward_polys, public.manc_final_polygon3 
WHERE public.manc_final_polygon3.building is not null and building != 'no' AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;

--building editor count
SELECT manchester_ward_polys.name AS name, count(distinct public.manc_final_polygon3.osm_uid) AS total 
FROM manchester_ward_polys, public.manc_final_polygon3 
WHERE public.manc_final_polygon3.building is not null and building != 'no' AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;

--poi with corresponding polygon 
SELECT manchester_ward_polys.name AS name, count(distinct public.manchester_final_poi3.osm_id)   
FROM manchester_ward_polys, public.manchester_final_poi3, public.manc_final_polygon3 
WHERE st_within(manchester_final_poi3.way, manc_final_polygon3.way) AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;

--count of null amenities
--divide by total number of poi's and * 100 to get % null
SELECT manchester_ward_polys.name AS name, count(public.manchester_final_poi3.*) AS total 
FROM manchester_ward_polys, public.manchester_final_poi3 
WHERE public.manchester_final_poi3.amenity is null AND ST_Contains(public.manchester_ward_polys.geom, public.manchester_final_poi3.way) 
GROUP BY public.manchester_ward_polys.name;

--building count with low level addresses
SELECT manchester_ward_polys.name AS name, count(public.manc_final_polygon3.*) AS total 
FROM manchester_ward_polys, public.manc_final_polygon3 
WHERE public.manc_final_polygon3.building is not null and building != 'no' and ("addr:housenumber" is not null or "addr:housename" is not null or "addr:place" is not null) AND ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name;

--feature count = Number of features/Land Area
--area already calculated 
--number of edits already calculated
--srid:3857 units = meters
--m2 to km2 = m2 / 1,000,000 
--name, shape_area
--calculated in excel
select name, st_area(geom) from manchester_ward_polys

----
--calculating the frequency of amenities which become most frequent
create table test as (
SELECT manchester_ward_polys.name AS name, public.manc_final_polygon3.amenity, count(public.manc_final_polygon3.amenity)
AS total 
FROM manchester_ward_polys, public.manc_final_polygon3 
WHERE ST_Contains(public.manchester_ward_polys.geom, public.manc_final_polygon3.way) 
GROUP BY public.manchester_ward_polys.name, public.manc_final_polygon3.amenity
);

create table test1 as (
SELECT
  * 
FROM (
  SELECT
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY total desc) AS r,
    t.*
  FROM
    test t) x
WHERE
  x.r <= 1
	);

select amenity, count(amenity) from test1
group by test1.amenity
order by count(amenity) desc;

--------------------







--feature count = Number of features/Land Area
--area already calculated 
--number of edits already calculated
--srid:3857 units = meters
--m2 to km2 = m2 / 1,000,000 
--name, shape_area
--calculated in excel
select name, st_area(geom) from nairobi_wards_polys

SELECT public.nairobi_wards_polys.name, count(public.nairobi_final_poi3.way) AS total
FROM public.nairobi_wards_polys LEFT JOIN public.nairobi_final_poi3 
ON st_contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name;

select name, st_area(geom) from nairobi_wards_polys

select * from (
select name, amenity, total, row_number() over (partition by name order by total) as rownum from test
) tmp
where rownum < 2;

--I THINK IT IS DONE!!!!
  SELECT
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY total desc) AS r,
    t.*
  FROM
    test t) x
WHERE
  x.r <= 1;
  
  
  -----
  
  --select count distinct osm_uid from nairobi_final_poi3 where 
--ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way)

SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.osm_uid, count(public.nairobi_final_poi3.osm_uid), count(distinct public.nairobi_final_poi3.osm_uid)
AS uid
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) and nairobi_wards_polys.name = 'AIRBASE'
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.osm_uid

/*--create table test3 as (
SELECT
  * 
FROM (
  SELECT
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY uid desc) AS r,
    t.*
  FROM
    test2 t) x
--WHERE
  --x.r <= 1
	--);
create table test2 as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.osm_uid, count(public.nairobi_final_poi3.osm_uid)
AS uid
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.osm_uid
)
create table test1 as (
SELECT
  * 
FROM (
  SELECT
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY total desc) AS r,
    t.*
  FROM
    test t) x
WHERE
  x.r <= 1
	);
select amenity, count(amenity) from test1
group by test1.amenity
order by count(amenity) desc;
*/

create table n1 as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.osm_uid, count(public.nairobi_final_poi3.osm_uid) as total_count, count(distinct public.nairobi_final_poi3.osm_uid)
AS uid
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.osm_uid
);

create table n2 as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_line3.osm_uid, count(public.nairobi_final_line3.osm_uid) as total_count, count(distinct public.nairobi_final_line3.osm_uid)
AS uid
FROM nairobi_wards_polys, public.nairobi_final_line3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_line3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_line3.osm_uid
);

create table n3 as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_polygon3.osm_uid, count(public.nairobi_final_polygon3.osm_uid) as total_count, count(distinct public.nairobi_final_polygon3.osm_uid)
AS uid
FROM nairobi_wards_polys, public.nairobi_final_polygon3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_polygon3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_polygon3.osm_uid
);

create table n_union1 as (
select *
from (
    select * from public.n1
    union 
	select * from public.n2
	union 
	select * from public.n3
) as userid
)
--repeat




		  
---rough
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity), 
row_number() over (partition by nairobi_wards_polys.name order by count(amenity)) 
AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE  ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity


create table test as (
SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity)
AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity
)


SELECT nairobi_wards_polys.name AS name, public.nairobi_final_poi3.amenity, count(public.nairobi_final_poi3.amenity)
AS total 
FROM nairobi_wards_polys, public.nairobi_final_poi3 
WHERE ST_Contains(public.nairobi_wards_polys.geom, public.nairobi_final_poi3.way) 
GROUP BY public.nairobi_wards_polys.name, public.nairobi_final_poi3.amenity
© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
Pricing
API
Training
Blog
About
Press h to open a hovercard with more details.