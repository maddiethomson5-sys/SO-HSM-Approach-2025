# SO-HSM-Approach-2025
This repository contains R notebooks and rendered HTML outputs for my internship project with the UlB Marine Biology Lab on habitat suitability modelling (HSM) of Southern Ocean fishes.

## Project Overview
This project leverages available presence data for commercially and ecologically important Southern Ocean fish species to estimate and visualise habitat suitability.  

The code and associated Shiny application focuses on three target species:  
- *Dissostichus eleginoides* (Patagonian toothfish)  
- *Dissostichus mawsoni* (Antarctic toothfish)  
- *Champsocephalus gunnari* (Mackerel icefish)  

### Data Sources
- **Environmental data**: [Bio-ORACLE](https://www.bio-oracle.org/)  
- **Occurrence records**: [OBIS](https://obis.org/) and [GBIF](https://www.gbif.org/)  

### Important Notes
- Model outputs do **not** represent observed distributions, but rather areas of *potential environmental suitability*.  
- Presence data for Southern Ocean fish are spatially biased toward surveyed or fished regions. Predictions should therefore be interpreted with caution.  
- Future applications could include projecting habitat suitability under climate change scenarios and comparing current vs. future distributions.  

## Repository Contents
- `*.Rmd` — R notebooks containing modular HSM code and workflow.  
- `*.html` — Pre-rendered versions of the notebooks (viewable in any browser).  

## Usage
- The modular R code (spread across six scripts) enables extension to additional species, provided sufficient presence data are available.
Scripts 1–3 handle data acquisition and preparation, while Scripts 4–6 focus on model development and refinement.
1. Open `.Rmd` files in [RStudio](https://posit.co/download/rstudio/) to view or modify the code.  
2. Run notebooks to reproduce analyses (note: some scripts may take hours to run).
3. Connect with associated Shiny app to explore environmental responses: https://thomson-nerka.shinyapps.io/AntarcticHabitatMapper/

## Acknowledgements
Special thanks to the ULB Marine Biology Lab — Pablo, Charlie, Anton, and Ming — for their support and guidance.  

---

📌 Author: Maddie Thomson  
📅 Year: 2025  
