# ECON4274_Individual-Project

# Analysis of PPP Loan Effectiveness on Small Business Survival

## Project Overview
This repository contains an analysis of the effectiveness of the U.S. CARES Act Paycheck Protection Program (PPP) on small business survival. Using a difference-in-differences (DiD) approach, the analysis examines how PPP loans affected firm survival rates, with a focus on five major states: California, Florida, Illinois, New York, and Texas.

## Key Findings
- PPP loans significantly improved firm survival rates, particularly during and immediately after the pandemic
- Geographic variations were observed, with states like Illinois and New York experiencing lower survival rates
- Industry-specific differences emerged, with sectors like Transportation and Warehousing showing higher survival rates
- The model's improvement across specifications emphasizes the critical role of PPP in sustaining businesses during economic crises

## Methodology
The analysis employs:
- Difference-in-differences (DiD) analysis to estimate causal effects
- Treatment intensity normalization using z-scores
- Regression analysis with various control variables
- Statistical testing to validate model assumptions

## Data Sources
- PPP loan data from SBA (Small Business Administration)
- SUSB (Statistics of U.S. Businesses) data for baseline business statistics
- Data segmented by state, industry sectors (NAICS codes), and year

## Implementation Details
The analysis pipeline includes:
1. Data import and cleaning
2. Filtering for the five target states
3. NAICS code standardization
4. Treatment intensity calculation
5. Control and treatment group definition
6. DiD regression with fixed effects
7. Robustness checks with additional controls
8. Statistical validation

## Results Visualization
The project includes visualizations of:
- Loan distribution across states
- Loan distribution across industry sectors
- Survival rate comparisons between treatment and control groups

## Technical Requirements
The analysis requires the following R packages:
- dplyr
- tidyr
- ggplot2
- stargazer
