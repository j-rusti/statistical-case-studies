Lucy Letby Blood Analysis
=========================

This project investigates the relationship between insulin and C-peptide levels in babies F and L from the Lucy Letby case, using statistical analysis on 1,305 blood test records.

Overview
--------
We analyse whether the infants’ C-peptide measurements were unusually low given their insulin levels. Key steps include:
- Hypothesis testing to reject a linear C-peptide/insulin relationship factor of 5.0–10.0.
- Linear regression models with and without intercepts.
- Larger polynomial and transformed models selected via backward selection (AIC and F-test).
- Residual analysis and Box-Cox transformations to improve model fit.
- Prediction intervals and z-tests for individual insulin measurements of the infants.

Data
----
- CSV file: `Bloodlevels.csv`
- Columns: Year, C-peptide, Insulin
- Cleaning: Converts non-numeric "<5" and "<1" values, removes NAs.

Methods
-------
- Linear regression (with and without intercept)
- Polynomial regression and model selection
- Residual diagnostics and transformations
- Prediction intervals for specific insulin values
- Z-scores to assess unusual insulin levels

Usage
-----
1. Install R (≥4.0) and required packages:
   install.packages(c("ggplot2","dplyr","cv","MASS"))

2. Load and clean data:
   bldlvl <- read.csv("Bloodlevels.csv") %>%
             mutate(Cpep = as.numeric(CPeptide),
                    Ins = as.numeric(Insulin)) %>%
             filter(!is.na(Cpep) & !is.na(Ins))

3. Fit models and generate predictions:
   - Linear model: lm(Cpep ~ 0 + Ins, data=bldlvl)
   - Larger polynomial: lm(Cpep ~ Ins + I(Ins^2) + I(Ins^3) + ..., data=bldlvl)
   - Transformed residual model for improved fit

4. Use predict() to generate prediction intervals for babies F and L.

Findings
--------
- Infants’ C-peptide levels were unusually low relative to their insulin.
- Linear model and larger polynomial models largely agree.
- Z-scores indicate baby F’s insulin level is extreme compared to population data.

Limitations
-----------
- Unknown confounding factors
- Dataset may not be fully representative
- Cannot draw definitive causal conclusions
