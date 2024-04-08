# Brown_bear_predation
Data and code for Pop et al., 2023 - Predictors of brown bear predation events on livestock in the Romanian Carpathians

<hr>

### GENERAL INFORMATION

**Title:** Predictors of brown bear predation events on livestock in the Romanian Carpathians

**Author Information (data):**  
    Principal Investigator Contact Information  
		Name: Mihai I. Pop  
		Institution: Centre for Environmental Research (CCMESI), University of Bucharest, 
		Address: 1 N. Balcescu,Bucharest, Romania   
		Email: minelpop@yahoo.com 

**Author Information (code):** 		
		Data Analysis Contact Information  
		Name: Marissa A. Dyck  
		Institution: Ohio University, 
		Address: 57 Oxbow Trail 107 Irvine Hall, Athens, Ohio, USA 45701.   
		Email: marissadyck17@gmail.com 


**Date of data collection:** 2008 - 2016

**Geographic location of data collection:** Romania

**Information about funding sources that supported the collection of the data:** The data collection was partially funded by the European Commission LIFE Nature Programme 32 within the project LIFE08/NAT/RO/000500.The work of MIP and CIJ was supported by a grant 33 of the Romanian Ministry of Education and Research, CNCS-
UEFISCDI, project number PN-34III-P1-1.1-PD-2019-1207, within PNCDI III. MAD and VDP were supported by the Ohio35 University, Department of Biological Sciences


### SHARING/ACCESS INFORMATION

**Licenses/restrictions placed on the data:** None

**Link to publications that cite:**  <a href = "https://conbio.onlinelibrary.wiley.com/action/showCitFormats?doi=10.1111%2Fcsp2.12884&mobileUi=0" target = "_blank">[Link to publication]</a> 

**Recommended citation for this dataset:** Pop, Mihai I., et al. (2023), Data from: Predictors of brown bear predation events on livestock in the Romanian Carpathians Dataset

**Recommended citation for this manuscript:** Pop, M. I., Dyck, M. A., Chiriac, S., Lajos, B., Szabó, S., Iojă, C. I., & Popescu, V. D. (2023). Predictors of brown bear predation events on livestock in the Romanian Carpathians. *Conservation Science and Practice* , e12884. https://doi.org/10.1111/csp2.12884. 



### DATA & FILE OVERVIEW

**File List:**  

*Files in main folder*  

* <span style = "color: #7B0F17;">**Brown_bear_predation.Rproj**</span>; R project to run code for Pop et al., 2023
* <span style = "color: #7B0F17;">**README.html**</span>; a web browser version of this README file 
* <span style = "color: #7B0F17;">**README.md**</span>; a markdown version of this README file that can be altered in Rstudio 

*Files in data folder*  

*/raw* 

* <span style = "color: #7B0F17;">**pagube_2008_2016_spatial.csv**</span>; contains raw data for brown bear predation of all livestock types in Romania from 2008-2016 and pesudo-absences

*/processed*  

* <span style = "color: #7B0F17;">**cows.csv**</span>; contains data for brown bear predation of cows specifically in Romania from 2008-2016 and pesudo-absences - a subset of data from the *pagube_2008_2016_spatial.csv* 
* <span style = "color: #7B0F17;">**sheep.csv**</span>; contains data for brown bear predation of sheep specifically in Romania from 2008-2016 and pesudo-absences - a subset of data from the *pagube_2008_2016_spatial.csv* 
* <span style = "color: #7B0F17;">**other.csv**</span>; contains data for brown bear predation of other livestock types in Romania from 2008-2016 and pesudo-absences - a subset of data from the *pagube_2008_2016_spatial.csv* 


*Files in scripts folder*

* <span style = "color: #7B0F17;">**brown_bear_predation_data_formatting.R**</span>; R script with code for formatting the raw data for Pop et al., 2023
* <span style = "color: #7B0F17;">**brown_bear_predation_glmm**</span>; R script with code for generalized linear mixed effect models used in Pop et al., 2023raw


### METHODOLOGICAL INFORMATION

**Description of methods used for collection/generation of data:** Methodological details are provided in the paper <a href = "Link to publication" target = "_blank">[https://conbio.onlinelibrary.wiley.com/action/showCitFormats?doi=10.1111%2Fcsp2.12884&mobileUi=0]</a> 

**Methods for processing the data:** We used generalized linear mixed effects models with a binomial distribution to process the data. Full details can be found in the paper (link above).

**Instrument- or software-specific information needed to interpret the data:** We used program R version 4.4.1 with package lme4. 

* Download R <a href = "https://cran.r-project.org/bin/windows/" target = "_blank">[Windows link]</a> <a href = "https://cran.r-project.org/bin/macosx/" target = "_blank">[Mac link]</a>
* Downlad R Studio <a href = "https://www.rstudio.com/products/rstudio/" target = "_blank">[link]</a>

**People involved with sample collection, processing, analysis and/or submission:** The data was collected by three local EPAs (within the framework of the EU LIFE Nature project123LIFEURSUS LIFE08/NAT/RO/000500)

### DATA-SPECIFIC INFORMATION FOR: [ <span style = "color: #7B0F17;">pagube_2008_2016_spatial.csv</span>]

* **Number of variables:** 24
*  **Number of observations/rows:** 3024

**Variable List:**

* <span style = "color: #002747;">**damage**</span>, binary indicator for presence/absence of livestock damage caused by brown bears (0 = no damage 1 = damage)
* <span style = "color: #002747;">**year**</span>, factor describing the year the damage was recorded
* <span style = "color: #002747;">**month**</span>, numeric notation describing the month the damage was recorded (1 = January)
* <span style = "color: #002747;">**targetspp**</span>, indicator of the livestock type affected 
* <span style = "color: #002747;">**point_x**</span>, UTM Northing for livestock damage
* <span style = "color: #002747;">**point_y**</span>, UTM Easting for livestock damage
* <span style = "color: #002747;">**bear_abund**</span>, relative bear abundance estimated by hunters in 2016 and recorded at the game unit level
* <span style = "color: #002747;">**landcover_code**</span>, the landcover type in 2018 as defined by <a href = "https://land.copernicus.eu/eagle/files/eagle-related-projects/pt_clc-conversion-to-fao-lccs3_dec2010" target = "_blank">CORINE Land Cover Classification system</a>.
* <span style = "color: #002747;">**altitude**</span>, the altitude at the location recorded in meters.
* <span style = "color: #002747;">**human_population**</span>, number of people living in administrative units
* <span style = "color: #002747;">**dist_to_forest**</span>, distance to nearest forest measured in meters
* <span style = "color: #002747;">**dist_to_town**</span>, distance to nearest human settlement (defined as artificial surface class in CORINE Land Cover) measured in meters
* <span style = "color: #002747;">**livestock_killed**</span>, number of livestock killed per incident
* <span style = "color: #002747;">**shannondivindex**</span>, shannon diversity index for flora 
* <span style = "color: #002747;">**prop_arable**</span>, proportion of arable land (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_orchards**</span>, proportion of orchards/permanent crops (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_pasture**</span>, proportion of pastures (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_ag_mosaic**</span>, proportion of heterogeneous agricultural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_seminatural**</span>, proportion of aseminatural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_deciduous**</span>, proportion of deciduous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_coniferous**</span>, proportion of coniferous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_mixedforest**</span>, proportion of mixed forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_grassland**</span>, proportion of grasslands (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_for_regen**</span>, proportion of transitional woodland-shrub (defined by CORINE Land Cover classification) in 10 km2 area around location

### DATA-SPECIFIC INFORMATION FOR: [ <span style = "color: #7B0F17;">cows.csv</span>]

* **Number of variables:** 34
* **Number of cases/rows:** 1620

**Variable List:**

* <span style = "color: #002747;">**damage**</span>, binary indicator for presence/absence of livestock damage caused by brown bears (0 = no damage 1 = damage)
* <span style = "color: #002747;">**year**</span>, factor describing the year the damage was recorded
* <span style = "color: #002747;">**month**</span>, numeric notation describing the month the damage was recorded (1 = January)
* <span style = "color: #002747;">**targetspp**</span>, indicator of the livestock type affected 
* <span style = "color: #002747;">**point_x**</span>, UTM Northing for livestock damage
* <span style = "color: #002747;">**point_y**</span>, UTM Easting for livestock damage
* <span style = "color: #002747;">**bear_abund**</span>, relative bear abundance estimated by hunters in 2016 and recorded at the game unit level
* <span style = "color: #002747;">**landcover_2**</span>, grouped classification of CORINE Land Cover variables from landcover_code (see brown_bear_predation_data_formatting for specific groupings)
* <span style = "color: #002747;">**altitude**</span>, the altitude at the location recorded in meters
* <span style = "color: #002747;">**human_population**</span>, number of people living in administrative units
* <span style = "color: #002747;">**dist_to_forest**</span>, distance to nearest forest measured in meters
* <span style = "color: #002747;">**dist_to_town**</span>, distance to nearest human settlement (defined as artificial surface class in CORINE Land Cover) measured in meters
* <span style = "color: #002747;">**livestock_killed**</span>, number of livestock killed per incident
* <span style = "color: #002747;">**shannondivindex**</span>, shannon diversity index for flora
* <span style = "color: #002747;">**prop_arable**</span>, proportion of arable land (defined by CORINE Land Cover classification) in 10 km2 area around location
<span style = "color: #002747;">**prop_orchards**</span>, proportion of orchards/permanent crops (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_pasture**</span>, proportion of pastures (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_ag_mosaic**</span>, proportion of heterogeneous agricultural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_seminatural**</span>, proportion of aseminatural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_deciduous**</span>, proportion of deciduous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_coniferous**</span>, proportion of coniferous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_mixedforest**</span>, proportion of mixed forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_grassland**</span>, proportion of grasslands (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_for_regen**</span>, proportion of transitional woodland-shrub (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**openhab_10k**</span>, proportion of open habitat (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**forest_10k**</span>, proportion of forest (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**urban_10k**</span>, proportion of artificial surfaces (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**heteroag_10k**</span>, proportion of heterogenous agriculture (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**ag_10k**</span>, proportion of agricultural land (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**shdi_10k**</span>, shannon diversity index for flora calculated for a 10km2 area around each location (using a moving window approach in GIS) and based on CORINE Land Cover 2012 classes, scaled and centered
* <span style = "color: #002747;">**s.bear_abund**</span>, scaled and centered *bear_abund*
* <span style = "color: #002747;">**s.altitude**</span>, scaled and centered *altitude*
* <span style = "color: #002747;">**s.dist_to_town**</span>, scaled and centered *dist_to_town*
* <span style = "color: #002747;">**s.dist_to_forest**</span>, scaled and centered *dist_to_forest*


### DATA-SPECIFIC INFORMATION FOR: [ <span style = "color: #7B0F17;">sheep.csv</span>]

* **Number of variables:** 34
* **Number of cases/rows:** 844

**Variable List:**

* <span style = "color: #002747;">**damage**</span>, binary indicator for presence/absence of livestock damage caused by brown bears (0 = no damage 1 = damage)
* <span style = "color: #002747;">**year**</span>, factor describing the year the damage was recorded
* <span style = "color: #002747;">**month**</span>, numeric notation describing the month the damage was recorded (1 = January)
* <span style = "color: #002747;">**targetspp**</span>, indicator of the livestock type affected 
* <span style = "color: #002747;">**point_x**</span>, UTM Northing for livestock damage
* <span style = "color: #002747;">**point_y**</span>, UTM Easting for livestock damage
* <span style = "color: #002747;">**bear_abund**</span>, relative bear abundance estimated by hunters in 2016 and recorded at the game unit level
* <span style = "color: #002747;">**landcover_2**</span>, grouped classification of CORINE Land Cover variables from landcover_code (see brown_bear_predation_data_formatting for specific groupings)
* <span style = "color: #002747;">**altitude**</span>, the altitude at the location recorded in meters
* <span style = "color: #002747;">**human_population**</span>, number of people living in administrative units
* <span style = "color: #002747;">**dist_to_forest**</span>, distance to nearest forest measured in meters
* <span style = "color: #002747;">**dist_to_town**</span>, distance to nearest human settlement (defined as artificial surface class in CORINE Land Cover) measured in meters
* <span style = "color: #002747;">**livestock_killed**</span>, number of livestock killed per incident
* <span style = "color: #002747;">**shannondivindex**</span>, shannon diversity index for flora
* <span style = "color: #002747;">**prop_arable**</span>, proportion of arable land (defined by CORINE Land Cover classification) in 10 km2 area around location
<span style = "color: #002747;">**prop_orchards**</span>, proportion of orchards/permanent crops (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_pasture**</span>, proportion of pastures (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_ag_mosaic**</span>, proportion of heterogeneous agricultural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_seminatural**</span>, proportion of aseminatural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_deciduous**</span>, proportion of deciduous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_coniferous**</span>, proportion of coniferous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_mixedforest**</span>, proportion of mixed forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_grassland**</span>, proportion of grasslands (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_for_regen**</span>, proportion of transitional woodland-shrub (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**openhab_10k**</span>, proportion of open habitat (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**forest_10k**</span>, proportion of forest (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**urban_10k**</span>, proportion of artificial surfaces (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**heteroag_10k**</span>, proportion of heterogenous agriculture (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**ag_10k**</span>, proportion of agricultural land (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**shdi_10k**</span>, shannon diversity index for flora calculated for a 10km2 area around each location (using a moving window approach in GIS) and based on CORINE Land Cover 2012 classes, scaled and centered
* <span style = "color: #002747;">**s.bear_abund**</span>, scaled and centered *bear_abund*
* <span style = "color: #002747;">**s.altitude**</span>, scaled and centered *altitude*
* <span style = "color: #002747;">**s.dist_to_town**</span>, scaled and centered *dist_to_town*
* <span style = "color: #002747;">**s.dist_to_forest**</span>, scaled and centered *dist_to_forest*

### DATA-SPECIFIC INFORMATION FOR: [ <span style = "color: #7B0F17;">other.csv</span>]

* **Number of variables:** 34
* **Number of cases/rows:** 536

**Variable List:**

* <span style = "color: #002747;">**damage**</span>, binary indicator for presence/absence of livestock damage caused by brown bears (0 = no damage 1 = damage)
* <span style = "color: #002747;">**year**</span>, factor describing the year the damage was recorded
* <span style = "color: #002747;">**month**</span>, numeric notation describing the month the damage was recorded (1 = January)
* <span style = "color: #002747;">**targetspp**</span>, indicator of the livestock type affected 
* <span style = "color: #002747;">**point_x**</span>, UTM Northing for livestock damage
* <span style = "color: #002747;">**point_y**</span>, UTM Easting for livestock damage
* <span style = "color: #002747;">**bear_abund**</span>, relative bear abundance estimated by hunters in 2016 and recorded at the game unit level
* <span style = "color: #002747;">**landcover_2**</span>, grouped classification of CORINE Land Cover variables from landcover_code (see brown_bear_predation_data_formatting for specific groupings)
* <span style = "color: #002747;">**altitude**</span>, the altitude at the location recorded in meters
* <span style = "color: #002747;">**human_population**</span>, number of people living in administrative units
* <span style = "color: #002747;">**dist_to_forest**</span>, distance to nearest forest measured in meters
* <span style = "color: #002747;">**dist_to_town**</span>, distance to nearest human settlement (defined as artificial surface class in CORINE Land Cover) measured in meters
* <span style = "color: #002747;">**livestock_killed**</span>, number of livestock killed per incident
* <span style = "color: #002747;">**shannondivindex**</span>, shannon diversity index for flora
* <span style = "color: #002747;">**prop_arable**</span>, proportion of arable land (defined by CORINE Land Cover classification) in 10 km2 area around location
<span style = "color: #002747;">**prop_orchards**</span>, proportion of orchards/permanent crops (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_pasture**</span>, proportion of pastures (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_ag_mosaic**</span>, proportion of heterogeneous agricultural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_seminatural**</span>, proportion of aseminatural areas (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_deciduous**</span>, proportion of deciduous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_coniferous**</span>, proportion of coniferous forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_mixedforest**</span>, proportion of mixed forests (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_grassland**</span>, proportion of grasslands (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**prop_for_regen**</span>, proportion of transitional woodland-shrub (defined by CORINE Land Cover classification) in 10 km2 area around location
* <span style = "color: #002747;">**openhab_10k**</span>, proportion of open habitat (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**forest_10k**</span>, proportion of forest (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**urban_10k**</span>, proportion of artificial surfaces (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**heteroag_10k**</span>, proportion of heterogenous agriculture (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**ag_10k**</span>, proportion of agricultural land (landcover_2 grouping) extracted within a 10km2 area around each location (using a moving window approach in GIS), scaled and centered
* <span style = "color: #002747;">**shdi_10k**</span>, shannon diversity index for flora calculated for a 10km2 area around each location (using a moving window approach in GIS) and based on CORINE Land Cover 2012 classes, scaled and centered
* <span style = "color: #002747;">**s.bear_abund**</span>, scaled and centered *bear_abund*
* <span style = "color: #002747;">**s.altitude**</span>, scaled and centered *altitude*
* <span style = "color: #002747;">**s.dist_to_town**</span>, scaled and centered *dist_to_town*
* <span style = "color: #002747;">**s.dist_to_forest**</span>, scaled and centered *dist_to_forest*
