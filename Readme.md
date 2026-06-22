# Data and Code for Performing Quantitative Analyses for "Factors Associated with Financial Inclusion in Indonesia Before and During COVID-19: Evidence from Global Findex Data"

## Information on Related Publication

**Title**: Factors Associated with Financial Inclusion in Indonesia Before and During COVID-19: Evidence from Global Findex Data

**Authors**: Puguh Prasetyoputra [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--5494--7003-brightgreen?logo=orcid&logoColor=white)](https://orcid.org/0000-0001-5494-7003), Yovita Isnasari, Ari Purwanto Sarwo Prasojo [![ORCID](https://img.shields.io/badge/ORCID-0000--0002--4862--5523-brightgreen?logo=orcid&logoColor=white)](https://orcid.org/0000-0002-4862-5523), Iwan Hermawan

**Authors affiliation**: Research Center for Population, National Research and Innovation Agency (BRIN), Indonesia; Research Center for Economics of Industry, Services, and Trade, National Research and Innovation Agency (BRIN), Indonesia 

**Articles' DOI**: 10.19139/soic-2310-5070-2852(https://doi.org/10.19139/soic-2310-5070-2852)

**Abstract**: This study examines the factors associated with financial inclusion and the use of financial technology (FinTech) in Indonesia, both before and during the COVID-19 pandemic, using the Global Findex data from 2017 and 2021. Multivariable logistic regression models were fitted to analyze the factors associated with formal account ownership, savings, borrowing, mobile/Internet payments, and mobile money services usage. The results suggest that formal account ownership remained stable, whereas savings and borrowing declined during the pandemic. Education was observed as a variable with a significant correlation with financial inclusion and the use of FinTech. Higher income and mobile phone ownership significantly increased the likelihood of inclusion for all the indicators. Female individuals have a higher probability of owning a formal account and saving in one than males. Moreover, the pandemic accelerated the adoption of digital financial services. Policy recommendations include: 1) strengthening financial and digital literacy programs, especially for underserved groups; 2) expanding affordable digital infrastructure; 3) developing gender-responsive financial products; 4) balancing FinTech innovation with consumer protection; and 5) leveraging public-private partnerships to scale digital payment ecosystems. Future research should examine the long-term impacts on household resilience and explore the behavioral factors influencing inclusion beyond socioeconomic variables.

## Table of Contents (Main):
```
├── 📁 data
├── 📁 logs
├── 📁 models
├── 📁 outputs
├── 📁 syntax
│   ├── 📄 master.R
├── 📄 manage_renv.R
└── 📄 renv.lock
```

| File / Folder Path | Description |
| :--- | :--- |
| `data/` | Directory containing all project datasets. |
| `logs/` | Execution logs from scripts.
| `models/` | Stored R model objects (`.rds` or `.RData`) for reuse. |
| `outputs/` | High-quality, publication-ready outputs (e.g., figures, tables). |
| `syntax/` | R scripts for data analysis, and model estimation. |
| `syntax/master.R` | Master script to execute the entire data pipeline or workflow sequentially. |
| `manage_renv.R` | Renv environment manager. |
| `renv.lock` | Project lockfile recording the exact versions and sources of R packages to ensure reproducibility. |

## Data Availability Statement  
The data used in this project comes from a **Global Financial Inclusion (Global Findex) Database**.  

| Dataset Name | Description | Source | Access |
|-------------|-------------|--------|--------|
| micro_world_139countries.dta | 2021 Global Findex - World Microdata | https://doi.org/10.48529/jq97-aj70 | Publicly available upon request |
| micro_world | 2017 Global Findex - World Microdata | https://doi.org/10.48529/d3cf-fj47 | Publicly available upon request |

## How to Use for Replication

To replicate the results of this project, please follow these steps:

### 1. Environment Setup
* **IDE:** We recommend using **RStudio** (through open `.Rproj` file) or **Positron** (through open folder).
* **Dependencies:** Run the `manage_renv.R` script to initialize the environment manager and install all required packages.

### 2. Running the Analysis
The entire pipeline is controlled via a single master script.
* Open and execute `syntax/master.R`.
* You can toggle specific stages of the pipeline by changing the value to `1` (to run) or `0` (to skip) within the configuration section of the script.

### 3. Output Directories
All generated files are automatically organized into the following folders:
* `output/`: Contains final results (e.g., figures, tables).
* `models/`: Stores saved models as R objects for inference analysis.
* `logs/`: Contains execution logs for each pipeline stage to help monitor the process and debug errors.