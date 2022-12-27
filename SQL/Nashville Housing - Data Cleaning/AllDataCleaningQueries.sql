--Data Cleaning using SQL
--Skills used
---Change Datatypes, Add derived columns, Update columns with null values with default values
---Window functions, Type casting, Aggregation, String splits, Dealing with missing data
---Case Statements, Deleting duplicate rows, Deleting unwanted columns
--Sections
----Data Exploration
----Backup and Restore
----Datatype Updates
----Data Cleaning/Imputation

-- Data Exploration
-- View the first few rows of the table
select top 10 * from NashvilleHousing;

--BackUp And Restore Section
-- Backing up data before updating
-- Drop a table called 'NashvilleHousingbkp' in schema 'dbo'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[NashvilleHousingbkp]', 'U') IS NOT NULL
DROP TABLE [dbo].[NashvilleHousingbkp]
GO
Select * into NashvilleHousingbkp 
from NashvilleHousing;

-- Cell to restore backup if required
-- Drop a table called 'NashvilleHousing' in schema 'dbo'
-- Drop the table if it already exists
IF OBJECT_ID('[dbo].[NashvilleHousing]', 'U') IS NOT NULL
DROP TABLE [dbo].[NashvilleHousing]
GO
Select * into NashvilleHousing 
from NashvilleHousingbkp
GO

-- Datatype Updates
-- Checking SalePrice column
select top 5 SalePrice, convert(bigint, SalePrice) as SalePriceConverted
from NashvilleHousing;

-- Convert Sale price to int avoid decimal places
Alter TABLE NashvilleHousing
Alter COLUMN SalePrice bigint; 

select top 5 SalePrice from NashvilleHousing;

-- Imputing property Address
with propAddCTE as (select parcelId
, propertyAddress
, LAG(propertyAddress) over (PARTITION by parcelId order by parcelId) as PrevPropertyAddress
, LEAD(PropertyAddress) over (partition by parcelId order by parcelId) as NextPropertyAddress
, LAG(ParcelID) over (partition by parcelId order by parcelId) as PrevParcelId
, LEAD(ParcelID) over (partition by parcelId order by parcelId) as NextPracelId
from NashvilleHousing)

select top 5 * from propAddCTE
where propertyAddress is null
and (((ParcelID = PrevParcelId) and (PrevPropertyAddress is not null) ) 
        or ((ParcelID = NextPracelId) and (NextPropertyAddress is not null)));


select count(*) as NullAddressCount from NashvilleHousing where PropertyAddress is null

with propAddCTE as (select parcelId
, propertyAddress
, LAG(propertyAddress) over (PARTITION by parcelId order by parcelId) as PrevPropertyAddress
, LEAD(PropertyAddress) over (partition by parcelId order by parcelId) as NextPropertyAddress
, LAG(ParcelID) over (partition by parcelId order by parcelId) as PrevParcelId
, LEAD(ParcelID) over (partition by parcelId order by parcelId) as NextParcelId
from NashvilleHousing)
-- Update addresses
Update NashvilleHousing 
set propertyAddress = pa.PrevPropertyAddress 
from propAddCTE pa 
join NashvilleHousing nh on nh.ParcelID = pa.parcelId
where pa.parcelId = nh.parcelId and (pa.parcelId = pa.PrevParcelId)
and nh.PropertyAddress is null and pa.PrevPropertyAddress is not null ;


with propAddCTE as (select parcelId
, propertyAddress
, LAG(propertyAddress) over (PARTITION by parcelId order by parcelId) as PrevPropertyAddress
, LEAD(PropertyAddress) over (partition by parcelId order by parcelId) as NextPropertyAddress
, LAG(ParcelID) over (partition by parcelId order by parcelId) as PrevParcelId
, LEAD(ParcelID) over (partition by parcelId order by parcelId) as NextParcelId
from NashvilleHousing)
-- Update addresses
Update NashvilleHousing 
set propertyAddress = pa.NextPropertyAddress 
from propAddCTE pa 
join NashvilleHousing nh on nh.ParcelID = pa.parcelId
where pa.parcelId = nh.parcelId and (pa.parcelId = pa.NextParcelId)
and nh.PropertyAddress is null and pa.NextPropertyAddress is not null ;


-- Property Address Split

select top 10 propertyAddress,
 substring(propertyAddress,1,CHARINDEX(',',PropertyAddress)-1) as PropertyStreet,
 substring(PropertyAddress,CHARINDEX(',',PropertyAddress)+1,len(PropertyAddress)) as PropertyCity
from NashvilleHousing

-- Add a new column '[PropertyStreet]', [PropertyCity] to table '[NashvilleHousing]' in schema '[dbo]'
ALTER TABLE [dbo].[NashvilleHousing]
    ADD [PropertyStreet] varchar(100)  NULL,
        [PropertyCity] varchar(100) NULL
GO

-- Update rows in table '[NashvilleHousing]' in schema '[dbo]'
UPDATE [dbo].[NashvilleHousing]
SET
    [PropertyStreet] = substring(propertyAddress,1,CHARINDEX(',',PropertyAddress)-1),
    [PropertyCity] = substring(PropertyAddress,CHARINDEX(',',PropertyAddress)+1,len(PropertyAddress))
GO

-- Update Owner Address
select top 5 ownerAddress
, PARSENAME(REPLACE(ownerAddress,',','.'),3) as OwnerStreet
, PARSENAME(REPLACE(ownerAddress,',','.'),2) as OwnerCity
, PARSENAME(REPLACE(ownerAddress,',','.'),1) as OwnerState
from NashvilleHousing

-- Add new columns '[OwnerStreet]', '[OwnerCity]' and '[OwnerState]' to table '[NashvilleHousing]' in schema '[dbo]'
ALTER TABLE [dbo].[NashvilleHousing]
    ADD [OwnerStreet] varchar(100)  NULL,
        [OwnerCity] varchar(100) NULL,
        [OwnerState] varchar(25) NULL
GO

-- Update rows in table '[NashvilleHousing]' in schema '[dbo]'
UPDATE [dbo].[NashvilleHousing]
SET
    [OwnerStreet]   = PARSENAME(REPLACE(ownerAddress,',','.'),3),
    [OwnerCity]     = PARSENAME(REPLACE(ownerAddress,',','.'),2),
    [OwnerState]    = PARSENAME(REPLACE(ownerAddress,',','.'),1)
GO

-- SoldasVacant field update 
select DISTINCT SoldasVacant
, count(*) as count 
from NashvilleHousing
group by SoldAsVacant
order by 2

-- Update rows in table '[TableName]' in schema '[dbo]'
UPDATE [dbo].[NashvilleHousing]
SET
    [SoldAsVacant] = CASE   WHEN SoldAsVacant = 'Y' THEN 'Yes'
                            WHEN SoldAsVacant = 'N' THEN 'No'
                            ELSE SoldAsVacant
                     END
GO 

select DISTINCT SoldasVacant
, count(*) as count 
from NashvilleHousing
group by SoldAsVacant
order by 2

-- Remove Duplicates
WITH DuplicateRowsCTE as (
    select uniqueID,
            ROW_NUMBER() over (partition by ParcelID, propertyAddress, SaleDate, legalReference order by uniqueID) rownum 
            from NashvilleHousing
)

-- Delete rows from table '[NashvilleHousing]' in schema '[dbo]'
DELETE FROM [dbo].[NashvilleHousing]
WHERE UniqueID in (select UniqueID from DuplicateRowsCTE
where rownum >1)
GO 

-- Delete unwanted Columns
ALTER TABLE NashvilleHousing
drop column TaxDistrict
GO