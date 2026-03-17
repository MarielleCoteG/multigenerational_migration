# Replication materials for paper: Multigenerational and gender-symmetric transmission of migration behaviors in historical Quebec (Côté-Gendreau 2026)

Welcome to the GitHub repository that will allow you to replicate the paper "Multigenerational and gender-symmetric transmission of migration behaviors in historical Quebec" (PNAS, 2026).

This repository contains only R code files and supplementary files. The data files need to be requested as explained below.


## Data access

The original data can be obtained from the Research Programme in Historical Demography (_Programme de recherche en démographie historique_, PRDH) by writing directly to the project director, Lisa Dillon (ly.dillon@umontreal.ca).

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

In addition, find in the repository the following files:

- data_aux/censuses_dates.xlsx
- data_aux/linkage_reliability.csv

The first file provides the exact dates associated with the censuses that were integrated in the database so that they can be used as observations in the timeline construction process. The second file provides assessments of linkage quality which are used to unlink about 100 records. I also provide code for assessing linkage quality but it takes a long time to run.


## Running the files

Use the file master.R to fun the code in the correct order. The construction of the timelines is currently very computationally demanding and takes a few days to run on a personal computer. I hope to eventually be able to make a faster version available.

Once the timelines are constructed, the analytic part of the code (from get_moves.R onwards) is not overly computationally demanding and does not modify the timeline files, so it can be run multiple times.
