# Replication code for Côté-Gendreau (2026)

Welcome to the GitHub repository that will allow you to replicate the paper "Multigenerational and gender-symmetric transmission of migration behaviors in historical Quebec" (PNAS, 2026).

This repository contains only R code files and supplementary files. The data files need to be requested as explained below.


## Data access

The original data can be obtained from the Research Programme in Historical Demography (Programme de recherche en démographie historique, PRDH) by writing directly to the project director, Lisa Dillon (ly.dillon@umontreal.ca).

This paper uses the July 2025 version of the Population Register of Historic Quebec (Registre de la population du Québec ancien, RPQA). For an exact replication, please request the July 2025 version with the following files:

- data_ACTE.2025-07.sav
- data_INDIVIDU.2025-07.sav
- data_MENTION.2025-07.sav
- data_UNION.2025-07.sav

You also need to request the parish file. The file used in this version is titled:

- RPQA.Paroisses.2025-09-01.xlsx

Regardless of the version of the file, you need the following columns:

- parish code
- latitude
- longitude
- year of foundation
- province or state

In addition, find in the repository the following file:

- censuses_dates.xlsx

This file provides the exact dates associated with the censuses that were integrated in the database so that they can be used as observations in the timeline construction process.


## Running the files

The order in which files should be run is given in the file master.R. The construction of the timelines is currently very computationally demanding and takes a couple of days to run. I hope to eventually be able to make a faster version available.

Once the timelines are constructed, the analytic part of the code is not overly computationally demanding and does not modify the timeline files.
