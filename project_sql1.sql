select * from project1.dbo.Data1;
select * from project1.dbo.Data2;

-- number of rows in our dataset

select count(*) from project1..Data1;
select count(*) from project1..Data2;

-- dataset for jharkhand and bihar
select * from project1..Data1 where state in ('Jharkhand','Bihar');

-- population of india
select sum(population) Total_population from project1..Data2;


-- average growth
select avg(Growth)*100 as avg_growth from project1..Data1;

select state,avg(Growth)*100 as avg_growth from project1..Data1 group by state;

-- average sex ratio growth
select state,round(avg(Sex_Ratio),0) as avg_SexRatio from project1..Data1 group by state order by avg_SexRatio desc;

-- average literacy rate
select state,round(avg(Literacy),0) as avg_LiteracyRatio from project1..Data1 group by state order by avg_LiteracyRatio desc;
select state,round(avg(Literacy),0) as avg_LiteracyRatio from project1..Data1 group by state having round(avg(Literacy),0)>90 order by avg_LiteracyRatio desc;

-- top 3 state showing highest growth ratio
select top 3 State,avg(growth)*100 as avgg from project1.dbo.Data1 group by State order by avgg desc;
select count(distinct State )from project1.dbo.Data1;

-- bottom 3 state showing lowest sex ratio
select top 3 State,round(avg(Sex_Ratio),0) as avgg_sexratio from project1.dbo.Data1 group by State order by avgg_sexratio ASC;

-- top and bottom 3 states in literacy rate

drop table if exists #topstates
create table #topstates
(state nvarchar(255),
 topstate float
 )

 insert into #topstates
 select state,round(avg(Literacy),0) as avg_LiteracyRatio from project1..Data1 
 group by state order by avg_LiteracyRatio desc;

 select top 3 * from #topstates order by #topstates.topstate desc
   -- --------------------------------------------------------------
drop table if exists #bottomstates
create table #bottomstates
(state nvarchar(255),
 bottomstate float
 )

 insert into #bottomstates
 select state,round(avg(Literacy),0) as avg_LiteracyRatio from project1..Data1 
 group by state order by avg_LiteracyRatio asc;

 select top 3 * from #bottomstates order by #bottomstates.bottomstate asc;

 -- Union operator to combine two table
 select * from (select top 3 * from #topstates order by #topstates.topstate desc) a
 union
 select * from (select top 3 * from #bottomstates order by #bottomstates.bottomstate asc) b;

 -- states starting with letter (left,like)

 select distinct state from project1.dbo.Data1 where left(lower(state),1) in ('m');
 select distinct state from project1.dbo.Data1 where lower(state) like 'a%';
 select distinct state from project1.dbo.Data1 where left(lower(state),1) in ('a','b');
 select distinct state from project1.dbo.Data1 where lower(state) like 'a%' or lower(state) like 'm%';
 select distinct state from project1.dbo.Data1 where lower(state) like 'a%'  and lower(state) like '%h';

 -- joining both tables

 select a.district,a.state,a.sex_ratio/1000,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District;

 

 -- formulas
-- females/males = sex_ratio     (1)                       |    females =  (sex_ratio) * males
-- males + females = population   (2)
-- females = population - males   (3)                      |  (population - males) =  sex_ratio * males
--                                                         |   population = sex_ratio*males + males
--                                                         |    population = males(sex_ratio+1)
--                                                         |   males = population/(sex_ratio+1)                            ---males
--                                                         |   females = population - population/(sex_ration+1)          
--														   |   females = population(1-1/(sex_ratio+1))
--														   |   females = (population* (sex_ratio)) / (sex_ratio+1)          --females


-- total males and females                                                      

select c.district,c.state,c.population/(c.sex_ratio+1) males,(c.population* c.sex_ratio) / (c.sex_ratio+1) females from
 (select a.district,a.state,a.sex_ratio/1000 sex_ratio,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District) c;		



--Literacy means percentage of population who can read and write 

--     literacy_ratio = total literate people/population
--     total literate people = literacy_ratio*population
--     population = literate + illiterate
--     illiterate = population - literate people
--     illiterate = population - literacy_ration * population
--     total  illiterate = population(1-literacy_ratio)
--	   total illiterate people = (1 - literacy_ratio)*population      

 -- total literacy rate
 select c.district,c.state,round(c.literacy_ratio*c.population,0) as literate_people,(1 - c.literacy_ratio)*c.population as illiterate_people from
 (select a.district,a.state,a.Literacy/100 literacy_ratio,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District) c;

 select d.state,sum(d.literate_people) as total_literate_people,sum(d.illiterate_people) as total_illiterate_people from
  (select c.district,c.state,round(c.literacy_ratio*c.population,0) as literate_people,round((1 - c.literacy_ratio)*c.population,0) as illiterate_people from
 (select a.district,a.state,a.Literacy/100 literacy_ratio,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District) c)d
 group by d.state;



--      population
--      previous_census+growth*previous_census = population
--		previous_census= population/(1+growth)
--

-- population in previous census
select e.state,sum(e.previous_census_population) as previous_census_population,sum(e.current_census_population) as current_census_population from
(select d.district,d.state,round(population/(1+growth),0) as previous_census_population,population as current_census_population from
 (select a.district,a.state,a.growth as growth,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District)d)e group by e.state;


 select  sum(m.previous_census_population) as previous_total , sum(m.current_census_population) as Currect_total from
 (select e.state,sum(e.previous_census_population) as previous_census_population,sum(e.current_census_population) as current_census_population from
(select d.district,d.state,round(population/(1+growth),0) as previous_census_population,population as current_census_population from
 (select a.district,a.state,a.growth as growth,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District)d)e group by e.state)m;

 
 --population vs area

 select q.*,r.* from (

 select '1' as keyy,n.*from
 (select  sum(m.previous_census_population) as previous_total , sum(m.current_census_population) as Currect_total from
 (select e.state,sum(e.previous_census_population) as previous_census_population,sum(e.current_census_population) as current_census_population from
(select d.district,d.state,round(population/(1+growth),0) as previous_census_population,population as current_census_population from
 (select a.district,a.state,a.growth as growth,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District)d)e 
 group by e.state)m)n)q inner join (

 select '1' as keyy,z. * from
 (select sum(area_km2) as total_area from project1..data2)z ) r on q.keyy=r.keyy;


 --------------------------------------------------------------

 select g.total_area/g.previous_total as previous_census_population_vs_area , g.total_area/g.Currect_total  as  current_census_population_vs_area from
 (select q.*,r.total_area from (

 select '1' as keyy,n.*from
 (select  sum(m.previous_census_population) as previous_total , sum(m.current_census_population) as Currect_total from
 (select e.state,sum(e.previous_census_population) as previous_census_population,sum(e.current_census_population) as current_census_population from
(select d.district,d.state,round(population/(1+growth),0) as previous_census_population,population as current_census_population from
 (select a.district,a.state,a.growth as growth,b.population from project1..Data1 a inner join project1..Data2 b on a.District=b.District)d)e 
 group by e.state)m)n)q inner join (

 select '1' as keyy,z. * from
 (select sum(area_km2) as total_area from project1..data2)z ) r on q.keyy=r.keyy)g;



  --window
 --top 3 distiricts from each state with highest literacy rate

 select a.* from
 (select district,state,literacy,rank() over(partition by state order by literacy desc) rnk from project1..data1) a
 where a.rnk in (1,2,3);