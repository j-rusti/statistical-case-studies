Power Demand and Climate Analysis
=========================

This project investigates the relationship between temperature, time, and electricity demand for Great Britain, using historical winter data (1991–2013) to help model and forecast peak demand and assess the influence of weather and temporal trends.

Overview
--------
We analyse how weather (temperature, wind, solar) and temporal variables (year, month, weekend/workday) affect gross electricity demand. Key steps include:
- Data cleaning and construction of composite temperature metrics (TE, TA, MA_TA).
- Exploratory analysis by weekday, month and year.
- Attempts to reduce confounding between temporal and weather effects.
- Linear model fitting and model selection (AIC / stepwise), comparing TE, TA and MA_TA variants.
- Residual diagnostics, Box–Cox and other transformations.
- Prediction of annual peak demand using historical weather scenarios and out-of-sample forecasts (5- and 10-year ahead).

Data
----
- Main CSV: `SCS_demand_modelling.csv` (demand, year, monthindex, weekend flag, TE, wind, solar_S, etc.)
- Hourly temperature CSV: `SCS_hourly_temp.csv` (used to build TA and MA_TA)
- Processing: filter winter months (Nov–Mar), remove/adjust edge years (1991 partial, 2014/2015 partial), compute weighted temperature metrics, compute moving averages.

Methods
-------
- Feature engineering: TE (temperature estimate), TA (weighted daily temp: avg/max/min), MA_TA (moving average TA)
- Confounder checks: regress climate covariates on temporal covariates and inspect boxplots
- Model building: stepwise linear models with terms for `year`, `year^2`, `year^3`, `monthindex`, `weekend`, temperature terms (TE/TA/MA_TA + transforms), `wind`, `solar_S`
- Diagnostics: residual plots, AIC, cross-validation (cv), Box–Cox, transformation regressions (e.g. gross^2)
- Forecasting: use fitted model and historical weather scenarios to predict maximum annual demand; compute 95% prediction intervals and 5-/10-year ahead forecasts where data permits

Key Model (example)
-------
A representative final model used:
Gross Demand ~ Year + Year^2 + Year^3 + Month + Weekend + TE + I(TE^2) + wind + solar_S

(Variants replace TE with TA or MA_TA and include suitable transforms chosen by stepwise selection.)

Usage
-----
1. Install R (≥4.0) and required packages:
   install.packages(c("ggplot2","dplyr","MASS","cv","lubridate","zoo","boot"))

2. Load and preprocess data (example):
   dem <- read.csv("SCS_demand_modelling.csv", stringsAsFactors = FALSE) %>%
          rename(gross = demand_gross) %>%
          mutate(weekend = as.factor(ifelse(wdayindex %in% c(0,6),1,0)),
                 monthindex = as.factor(monthindex),
                 true_year = year - 1991,
                 year = start_year - 1991)

   hourlytemp <- read.csv("SCS_hourly_temp.csv", stringsAsFactors = FALSE) %>%
                 mutate(date = lubridate::dmy_hm(Date)) %>%
                 filter(month(date) %in% c(11,12,1,2,3)) %>%
                 ... # compute daily summaries and TA, then attach MA_TA to dem

3. Fit models:
   gross_fit_TE <- lm(gross ~ year + I(year^2) + I(year^3) + monthindex + weekend + TE + I(TE^2) + wind + solar_S, data = dem)
   gross_fit_TA <- lm(... TA ...)
   gross_fit_MA_TA <- lm(... MA_TA ...)

   # Use step() for model simplification:
   gross_fit_TE <- step(gross_fit_TE)

4. Diagnostics and transformations:
   plot(gross_fit_TE); AIC(gross_fit_TE); cv(gross_fit_TE)
   # If needed, run Box–Cox and transformed model:
   boxcox(gross_fit_TE)
   tr_gross_fit_TE <- lm(I(gross^2) ~ ..., data = dem)

5. Forecast / predict maxima:
   # Generate predictions for target years or apply historical weather scenarios
   predict(gross_fit_TE, newdata = some_new_data, interval = "prediction", level = 0.95)

Findings
--------
- Year (long-run trend) has a strong effect on predicted maximum demand and often outweighs weather differences when projecting peak annual demand.
- TE/TA/MA_TA variants give similar patterns, but smoothing (MA_TA) can improve stability.
- 5- and 10-year forecasts show wide 95% prediction intervals, especially for longer horizons; predictive accuracy improves with more historical data but remains variable.
- Residual analyses reveal remaining unexplained variability and suggest model limitations for extreme events.

Limitations
-----------
- Dataset lacks extreme temperature events and regional granularity (e.g., demand by region), limiting inference on rare peaks.
- Important confounders (technology changes, demand-side behaviour, generation mix shifts) are not included.
- Stepwise model selection can risk overfitting; alternative methods (GAMs, random forests, Bayesian models) might capture nonlinearity better.
- Forecasts assume future temporal structure similar to historical trends — innovations or structural breaks would reduce reliability.

Recommendations
---------------
- Incorporate regional demand breakdowns and additional covariates (population, economic activity, heating degree days).
- Test nonlinear models (GAMs), ensemble methods, and Bayesian approaches to better quantify uncertainty.
- Use more granular temperature inputs and experiment with alternative TA weighting schemes.
- Present forecast ranges under multiple weather and socio-technical scenarios to inform planning.

Files
-----
- `SCS_demand_modelling.csv` — main demand dataset  
- `SCS_hourly_temp.csv` — hourly temperatures used to compute TA / MA_TA  
- Rmd / R scripts: model fitting, diagnostics, plots, and forecasting code

