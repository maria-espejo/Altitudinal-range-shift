#Title
Altitudinal range shifts among high-elevation plant species from the Tropical Andes


#Description

This data package contains datasets and R scripts used to analyze altitudinal range shifts of *Espeletia* species (Asteraceae) in the Páramo de Sumapaz (Eastern Colombian Andes). The analyses test predictions and background assumptions derived from the “expand” and “lean” hypotheses proposed by Lenoir and Svenning (2015, Ecography 38: 15–28), assessing species and cohort responses to recent climate warming.

All files required to reproduce the analyses presented in the associated manuscript are included.


##Data reuse

These data are provided to enable reproducibility and reuse. Users should cite the associated publication when using these data.

##Data files

**Occupancy dataset**
File name: Occupancy_2025April15.csv
Each row represents the presence/absence of a morphotype within a sampled 30 × 30 m quadrat.


Fecha: Date of sampling

Cuadrante: Code of the 2 × 2 km square

Celda: Code of the 30 × 30 m quadrat

Latitud: Latitude

Longitud: Longitude

Altitud: Elevation (m a.s.l.)

Nombre.provisional.morfo: Provisional morphotype name assigned in the field

Size-class presence/absence variables

Presence/absence of individuals recorded within each 30 × 30 m quadrat:

de0a5.plantaula: *Espeletia argentea* individuals with 0–5 cm stem length, or seedlings of other morphotypes

de5a10.de0a25: *E. argentea* individuals with 5–10 cm stem length, or other morphotypes with 0–25 cm stem length

de10a15.de25a50: *E. argentea* 10–15 cm, or other morphotypes 25–50 cm

de15a20.de50a75: *E. argentea* 15–20 cm, or other morphotypes 50–75 cm

de20a25.de75a100: *E. argentea* 20–25 cm, or other morphotypes 75–100 cm

de25a30.de100a125: *E. argentea* 25–30 cm, or other morphotypes 100–125 cm

de30a35.de125a150: *E. argentea* 30–35 cm, or other morphotypes 125–150 cm

de35a40.de150a175: *E. argentea* 35–40 cm, or other morphotypes 150–175 cm

de40a45.de175a200: *E. argentea* 40–45 cm, or other morphotypes 175–200 cm

de45a50.de200a225: *E. argentea* 45–50 cm, or other morphotypes 200–225 cm

de50a55.de225a250: *E. argentea* 50–55 cm, or other morphotypes 225–250 cm

masde55.masde250: *E. argentea* >55 cm, or other morphotypes >250 cm

Floracion: Presence/absence of flowering individuals

Fructificacion: Presence/absence of fruiting individuals

Colector.1–6: Collector codes

Numerodecoleccion_Plantula1–3: Seedling collection numbers

Numerodecoleccion_Adulto1–3: Adult collection numbers

Repeticion: Quadrat resampled (0 = no; 1 = yes)



**Phenotype determination dataset**
File name: phenodata_species_taxa_determinations_2025March30.csv
Each row represents a single herbarium specimen. Only variables relevant to the associated manuscript are described here.


Corrected.Collector.Collection.Number: Standardized collector code + collection number

Collector.Collection.Number: Original collector code + collection number

Collector: Collector name

Collection.Number: Collection number

Occupancy.Adults: Adult specimen associated with a 30 × 30 m occupancy quadrat

Occupancy.Seedling: Seedling specimen associated with a 30 × 30 m occupancy quadrat

Observation: Notes related to specimen classification

Phenotypic.group: Phenotypic group assignment from external analyses

Cuatrecasas.Taxa: Species name following Cuatrecasas (2013, The New York Botanical Garden Press 107)

Pineda.Group: Phenotypic group following Pineda et al. (2020, BioRxiv)

Species.Taxa.Assignment.For.Occupancy.Analysis: Final group used in occupancy analyses

Comment.Species.Taxa.Assignment.For.Occupancy.Analysis: Comments on assignment decisions



**Disturbance assessment dataset**
File name: DisturbioEstratos_2019Aug14.csv
Presence/absence of crops, pastures, and other disturbances assessed within each 30 × 30 m quadrat. Each quadrat was divided into a 4 × 4 grid (16 points).


Fecha: Date of sampling

Cuadrante: 2 × 2 km square code

Celda: 30 × 30 m quadrat code

Latitud: Latitude

Longitud: Longitude

Altitud: Elevation

Estrato: Disturbance or vegetation type

Columns 1–16: Presence/absence at each grid point



**Growth-rate reference dataset**

File name: Fagua_Gonzalez_2007_points.csv

Digitized growth rate (cm·yr⁻¹) and stem length data for *Espeletia grandiflora* from the Páramo de Chingaza, extracted from Figure 3 of Fagua & González (2007, Plant Biology 9: 127–135) using PlotDigitizer.




##R scripts

All scripts are written in R and were executed using the datasets provided in this repository. Together, these scripts test predictions and background assumptions associated with climate-driven altitudinal range shifts in Espeletia species from the Páramo de Sumapaz, following the framework proposed by Lenoir and Svenning (2015, Ecography 38: 15–28).


**OccupancyYoungAdult_Undisturbed_2025May30.R**

This script tests predictions derived from the hypothesis that *Espeletia* species are responding to global warming by expanding or leaning their altitudinal ranges. Analyses focus on differences in the elevational distributions of younger and older cohorts, as defined by stem-length classes.
Only undisturbed 30 × 30 m quadrats are included in order to isolate climate-related signals from local land-use effects. 
The script generates and compares observed elevational occupancy patterns with those produced by a null model, enabling evaluation of whether observed cohort differences depart from random expectations.



**OccupancyYoungAdult_AllQuadrats_2025May30.R**

This script tests the same range-shift predictions as the undisturbed analysis but includes all sampled quadrats, regardless of disturbance status. The inclusion of disturbed quadrats allows assessment of species and cohort distributions across the full sampled area.
The script generates and compares observed elevational occupancy patterns with those produced by a null model, enabling evaluation of whether observed cohort differences depart from random expectations.



**OccupancyBackgroundAssumptions_Undisturbed_2025May30.R**

This script evaluates key background assumptions required to interpret observed elevational patterns as evidence of climate-driven range shifts, using only undisturbed quadrats. Specifically, it examines whether:

i)The trailing edge (lower elevational limit) of the species’ distribution, or at least that of the younger cohort, is included within the surveyed elevational gradient.

ii)The leading edge (upper elevational limit) of the species’ distribution, or at least that of the older cohort, occurs below the upper limit of the surveyed gradient.

iii)The leading edge of the species’ distribution coincides with the upper limit of the realized elevational gradient.

These checks ensure that inferred range shifts are not artifacts of incomplete sampling along the elevation gradient.



**OccupancyBackgroundAssumptions_AllQuadrats_2025May30.R**

This script evaluates the same set of background assumptions described above, but includes all sampled quadrats, regardless of disturbance status.



**Growth rate simulations**

This script simulates stem-length growth trajectories in *Espeletia* (Espeletiinae, Asteraceae) using published growth-rate data from Fagua and González (2007, Plant Biology 9: 127–135). The simulations provide a reference for linking stem length to approximate age and cohort structure.





