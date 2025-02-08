# LTBI Dynamics in Canadian Immigrants Amid Declining TB Incidence

## Overview  
This repository contains code and data for estimating **latent tuberculosis infection (LTBI) prevalence** among **Canadian immigrants** from high TB burden countries (India, China, Philippines, Vietnam) under different **annual risk of infection (ARI) reduction scenarios** from 2024 to 2050.

## Files and Structure  

### **Data Files**
- `200repLARI.Rdata` – 200 replicate estimates of annual risk of TB infection (ARI).  
- `fcst.Rdata` – Forecasted ARI estimates up to 2021.  
- `ARI census data 27-05-2024.csv` – Census data for immigrants by age, year of arrival, and country of birth.  
- `NNDSS skeleton.csv` – TB case notification data by year, age, and country of birth.  

### **Code Files**
- `ARIdecreaseCode.R` – Computes force of infection (FOI) and projects future ARI trends under different reduction scenarios.  
- `Functions3 iso3.R` – Helper functions for processing census and TB data.  
- `Graphs for manuscript.R` – Generates visualizations for ARI trends, LTBI prevalence, and new infections.  
- `new manuscript code.R` – Produces summary statistics and tables for the manuscript.  

## Methodology  
1. **Data Processing**  
   - Cleans and reshapes **census data** (age, year of arrival, country of birth).  
   - Integrates **TB notification data** to assess infection risks.  

2. **ARI Reduction Scenarios (2024-2050)**  
   - **Status Quo:** No change in ARI.  
   - **1% to 5% Decrease:** Simulates gradual reductions in TB incidence.  

3. **Model Outputs**  
   - **LTBI Prevalence:** Estimated using historical ARI and census data.  
   - **New TB Infections:** Projected under different ARI scenarios.  

## Results Visualization  
- **ggplot2** is used for graphical representation of trends.  
- **Summary tables** are generated for different immigrant age groups and countries.  
