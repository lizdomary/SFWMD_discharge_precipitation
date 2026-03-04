Seasonal Water Quality Dynamics in Urban Canals Using High-Frequency Sensor Data

Project Overview

- This project analyzes high-frequency water quality data collected from buoy sensors in two urban canals in Miami, Florida: Coral Gables Canal and Little River. The analysis integrates continuous sensor measurements, discharge data from the South Florida Water Management District (SFWMD DBHYDRO), and precipitation records to evaluate how water quality conditions vary between the wet and dry seasons.
- Using Principal Component Analysis (PCA) and non-parametric statistical tests, the project explores how hydrological and biogeochemical variables interact and structure seasonal water quality patterns in coastal urban canal systems connected to Biscayne Bay.
- The analysis focuses on key indicators of water chemistry and hydrology, including fluorescent dissolved organic matter (fDOM), salinity, conductivity, dissolved oxygen, pH, turbidity, chlorophyll-a, and discharge.

The primary analysis pipeline can be found in:
- app.Rmd
- data_code.R

Key Research Question

- Do wet and dry seasons produce distinct multivariate water quality conditions in Miami’s urban canal systems?

Skills Demonstrated

This project demonstrates skills in:

- Environmental data analysis
- Time-series data processing
- Multivariate statistical analysis
- Data cleaning and preprocessing
- Exploratory data analysis (EDA)
- Statistical testing
- Scientific data visualization
- Reproducible research workflows

Methods Used

The workflow includes several analytical steps:

- Data Integration
- Combined buoy sensor data, discharge records, and precipitation data
- Processed and standardized datasets for analysis
- Data Cleaning
- Removed problematic sensor records
- Filtered outliers (e.g., unrealistic pH values)
- Converted discharge units
- Created seasonal classifications (wet vs. dry)
- Multivariate Analysis
- Applied Principal Component Analysis (PCA) to examine relationships among water quality variables
- Identified dominant environmental gradients influencing canal water chemistry

Statistical Testing

- Used Kruskal–Wallis tests to evaluate seasonal differences
- Applied Dunn post-hoc tests with Bonferroni correction
- Generated compact letter displays (CLD) to summarize seasonal group differences

Visualization

Produced figures including:

- PCA biplots
- Seasonal PCA score distributions
- Violin + boxplot comparisons of PCA axes

Statistical methods

- Principal Component Analysis (PCA)
- Kruskal–Wallis tests
- Dunn multiple comparisons

Data Sources

Data used in this analysis include:

- High-frequency buoy water quality data
- Discharge data from SFWMD DBHYDRO
- Precipitation records from the South Florida Water Management District

Monitoring locations:

- Coral Gables Canal (G93)
- Little River Canal (S27)

These canals represent urban waterways influenced by both freshwater discharge and marine tidal inputs from Biscayne Bay.
