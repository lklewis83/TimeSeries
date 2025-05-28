# 📊 Walmart Forecast Model Review | R-Shiny Dashboard

A comparative time series forecasting dashboard built in **R-Shiny**, using three distinct modeling approaches: **TSWGE**, **Prophet**, and **TimeGen**.
[R-Shiny Forecast Review Dashboard](https://blackjuliet.shinyapps.io/Forecast_Model_Review/).


---

## 📁 Project Structure

- **app.R** — Main Shiny app script
- **www/** — Static resources (e.g., Walmart logo)
- **tswge/** — TSWGE MLP model — Forecasts, residuals, and metrics from Prophet
- **prophet/** — Forecasts, residuals, and metrics from Prophet
- **timegen/** — Forecasts, residuals, and metrics from TimeGen
- **data/** — Raw and scaled input data
- **rmd_outputs/** — Rendered R Markdown model reports

---

## 🛠️ Models Compared

### 🔹 TSWGE (MLP)
- Classical MLP with external regressors
- High accuracy, lower ease-of-use
- **ASE:** 0.10 | **WMAE:** 0.29

### 🔹 Prophet
- Additive decomposition with holiday effects
- Best for explainability and ease of implementation
- **ASE:** 0.34 | **WMAE:** 0.33

### 🔹 TimeGen (Nixtla)
- Transformer-based generative forecasting (TimeGPT)
- Minimal tuning required; lower interpretability
- **ASE:** 0.88 | **WMAE:** 0.64

---

## 🔍 Data Source

[Kaggle – Walmart Sales Forecast Dataset](https://www.kaggle.com/datasets/aslanahmedov/walmart-sales-forecast)

- Filtered for **Store A** only
- Features include weekly sales, CPI, temperature, and holidays

---

## 📁 Folder Structure
##### README.md — Project overview and instructions
##### app.R — Main Shiny dashboard script
##### www/ — Static assets
- walmart_logo.png

##### tswge/ — TSWGE-specific outputs
- tswge_ase.rds
- tswge_wmae.rds
- tswge_forecast_with_dates.csv
- tswge_residuals_with_dates.csv

##### prophet/ — Prophet model outputs
- prophet_for.rds
- prophet_model.rds
- prophet_ase.rds
- prophet_wmae.rds
- residuals_prophet.rds

##### timegen/ — TimeGen (Nixtla) model outputs
- timeG_for.rds
- timeG_ase.rds
- timeG_wmae.rds
- residuals_timeG.rds
- nixtla_forecast_w.rds

##### data/ — Input and reference datasets
- walmart_train.csv
- walmart_test.csv
- scaled_datasets.csv

##### rmd_outputs/ — R Markdown report outputs
- tswge_forecast.Rmd
- prophet_forecast.Rmd
- timeG_forecast.Rmd

