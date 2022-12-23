/*
Covid 19 Data Exploration 
Skills used: Joins, CTE's, Temp Tables, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types
*/

Select *
From CovidDeaths
Where continent is not null 
order by 3,4


-- Select required Data we need

Select Location, date, total_cases, new_cases, total_deaths, population
From CovidDeaths
Where continent is not null 
order by 1,2


-- Total Cases vs Total Deaths
-- Shows likelihood of dying if you contract covid in a specific country

Select Location
, date
, total_cases
, total_deaths
, cast(total_deaths as decimal(20,10))/cast(total_cases as decimal(20,10))*100 as DeathPercentage
From CovidDeaths
Where location like '%United States%'
and continent is not null 
order by 1,2


-- Total Cases vs Population
-- Shows what percentage of population infected with Covid

Select Location, date, Population, total_cases,  (total_cases/population)*100 as PercentPopulationInfected
From CovidDeaths
Where location like '%United States%'
order by 1,2


-- Countries with Highest Infection Rate compared to Population

SELECT location, population, max(total_cases) as HighestInfectionCount, (MAX(total_cases)/cast(population as decimal(20,10))) * 100 as PercentPopulationInfected
from CovidDeaths
where continent is not null
group by location, Population
order by PercentPopulationInfected desc


-- Countries with Highest Death Count per Population

Select Location, Population, MAX(cast(Total_deaths as int)) as TotalDeathCount, (max(Total_deaths)/cast(population as decimal(20,10))) * 100 as PercentPopulationDeath
From CovidDeaths
Where continent is not null 
Group by Location, Population
order by PercentPopulationDeath desc

-- BREAKING THINGS DOWN BY CONTINENT

-- Showing contintents with the highest death count per population

Select continent, MAX(cast(Total_deaths as int)) as TotalDeathCount
From CovidDeaths
Where continent is not null 
Group by continent
order by TotalDeathCount desc

-- GLOBAL NUMBERS 
-- Shows the death percentage

select sum(cast(new_cases as int)) as total_cases
, sum(cast(new_deaths as int)) as total_deaths
, cast(sum(cast(new_deaths as float))/sum(cast(new_cases as float)) as decimal(20,10)) * 100 as DeathPercentage
from CovidDeaths
where continent is not null

-- Total Population vs Vaccinations
-- Shows Percentage of Population that has recieved at least one Covid Vaccine

select d.continent, d.location, d.date, d.population, v.new_vaccinations,
sum(cast(v.new_vaccinations as float)) over (PARTITION by v.location order by v.location,v.date) as RollingVaccinationCount
from CovidDeaths d
join CovidVaccinations v 
    on d.location = v.location
    and d.date = v.date
where d.continent is not null
order by 2,3


-- Using CTE to perform Calculation on Partition By in previous query

With PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingVaccinationCount)
as
(
select d.continent, d.location, d.date, d.population, v.new_vaccinations,
sum(cast(v.new_vaccinations as float)) over (PARTITION by v.location order by v.location,v.date) as RollingVaccinationCount
from CovidDeaths d
join CovidVaccinations v 
    on d.location = v.location
    and d.date = v.date
where d.continent is not null
)

Select *, (RollingVaccinationCount/Population)*100 as RollPopulationPercentage
From PopvsVac

-- Using Temp Table to perform Calculation on Partition By in previous query

Drop TABLE if EXISTS #PercentPopulationVaccinated
Create table #PercentPopulationVaccinated
(
    continent nvarchar(255),
    location nvarchar(255),
    Date datetime,
    population numeric,
    new_vaccinations numeric,
    RollingVaccinationCount numeric
)

insert into #PercentPopulationVaccinated
select d.continent
, d.LOCATION
, d.date
, d.population
, v.new_vaccinations
, sum(new_vaccinations) over (PARTITION BY d.location order by d.location, d.date) as RollingVaccinationCount
from CovidDeaths d 
join CovidVaccinations v on d.location = v.LOCATION and d.date = v.date
where d.continent is not null
order by 2,3

select *, cast(RollingVaccinationCount/population as decimal(20,10))
from #PercentPopulationVaccinated

-- Creating View to store data for later visualizations - this might have to be separate but keeping it here

drop view if EXISTS PercentPopulationVaccinated
Create View PercentPopulationVaccinated as
Select d.continent, d.location, d.date, d.population, v.new_vaccinations
, SUM(v.new_vaccinations) OVER (Partition by d.Location Order by d.location, d.Date) as RollingVaccinationCount
From CovidDeaths d
Join CovidVaccinations v
	On d.location = v.location
	and d.date = v.date
where d.continent is not null 

select * from PercentPopulationVaccinated
order by 2,3
