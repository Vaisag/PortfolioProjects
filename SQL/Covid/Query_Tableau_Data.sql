-- Queries for Tableau Dashboard

-- 1st Query
-- Shows the global death percentage

select sum(cast(new_cases as int)) as total_cases
, sum(cast(new_deaths as int)) as total_deaths
, cast(sum(cast(new_deaths as float))/sum(cast(new_cases as float)) as decimal(20,10)) * 100 as DeathPercentage
from CovidDeaths
where continent is not null

-- 2nd Query
-- Shows total deaths broken down by location

select continent, sum(cast(new_deaths as int)) as TotalDeathCount
from CovidDeaths
where continent is not NULL
and location not in ('World', 'European Union', 'International')
group by continent
order by TotalDeathCount DESC

-- 3rd Query
-- Shows max infection and percentage of infection broken down by country

select location, population
, isnull(max(total_cases),0) as HighestInfectionCount
, isnull(max(total_Cases)/cast(population as decimal(20,10))*100,0) as PercentPopulationInfected
from CovidDeaths
group by location, population
order by PercentPopulationInfected DESC

-- 4th Query
select location, population
, date
, isnull(max(total_cases),0) as HighestInfectionCount
, isnull(max(total_Cases)/cast(population as decimal(20,10))*100,0) as PercentPopulationInfected
from CovidDeaths
where continent is not null
group by location, population, date 
order by PercentPopulationInfected DESC

-- Query 5 - global vaccination numbers
select d.date 
, isnull(sum(d.new_cases),0) as new_cases
, isnull(sum(v.people_fully_vaccinated),0) as people_fully_vaccinated
, isnull(sum(cast(d.new_deaths as bigint)),0) as new_deaths
from CovidDeaths d 
join CovidVaccinations v on d.location = v.location and d.date = v.date 
where d.continent is not null
group by d.date
order by d.date 
