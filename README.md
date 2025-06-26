# 🧬 SARS-CoV-2 Wastewater and Case Data Analysis
This repository contains R code for processing, merging, and visualizing SARS-CoV-2 wastewater surveillance data and COVID-19 case data for the Toronto region. The goal is to analyze temporal trends and relationships between wastewater viral load, case numbers, and Ct values over time.

📊 Key Features
Merge and preprocess SARS-CoV-2 wastewater and case data specific to Toronto.

Calculate and visualize rolling averages for daily COVID-19 cases.

Analyze Ct values from lab metadata, including rolling averages.

Perform exploratory log-transformed regression data prep for viral loads (N1, N2).

Generate visual PDF outputs for trends over time.

📂 Files
Raw wastewater data
Metadata linking site IDs
COVID case data and region info from CovidTimelineCanada
Lab data with Ct values
N1/N2 gene copies

🧪 R Packages Used
dplyr, stringr, lubridate, ggplot2, ggpubr, scales, zoo, gridExtra

🖼️ Example Visualizations
Toronto Daily Case Counts: Include 3-day and 7-day rolling averages (blue & green lines).

Ct Value Trends: Ct scatter plot with smoothing and rolling average overlays.

Log-Transformed Viral Load (N1/N2): Regression-ready variables generated.

💾 Outputs
Cleaned wastewater data

Processed Ct data

Toronto region COVID case trends

Daily cases visualization

🚀 Usage
Clone the repo and run the R script in an R environment with required packages installed. Update file paths as needed depending on your directory structure.


📌 Notes
Be sure to have local versions of all required .csv files.

Set working directories appropriately to avoid path errors.

Data is region-specific and assumes sample IDs follow Toronto site conventions.

📬 Contact
For questions or collaboration, please reach out.
