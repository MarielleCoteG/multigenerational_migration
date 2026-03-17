options(scipen = 999)

print("ATTENTION: Please set the working directory to the replication directory")
setwd("~/Dropbox\ (Princeton)/Dissertation/Multigenerational/PNAS/Replication")
setwd("C:/Users/mc76/Princeton Dropbox/Marielle Cote-Gendreau/Dissertation/Multigenerational/PNAS/replication")

# Create directories
if (!dir.exists("data_raw")) {dir.create("data_raw")}
if (!dir.exists("data_modif")) {dir.create("data_modif")}
if (!dir.exists("register")) {dir.create("register")}
if (!dir.exists("figures")) {dir.create("figures")}
if (!dir.exists("tables")) {dir.create("tables")}

print("ATTENTION: Put all data files obtained from PRDH in folder data_raw")

# 0. Run the file that treats the parish file
# You might need to rename columns or adapt the beginning of the code depending on the file that you are given
source("code/distance_matrix.R")

# 1. Run the file that prepares the original data

source("code/clean_data.R")

# 2. Run the file that establishes temporal bounds to unions (creation to dissolution)
## based on dates of marriage and death but also childbirth, remarriage, etc.
## to compensate for missing dates

source("code/union_bounds.R")

# 3. Run the file that interpolates missing dates of death

source("code/estimate_DOD.R")

# 4. Run the file that estimates missing dates of birth

source("code/estimate_DOB.R")

# 5. Run the file that creates timelines at the union level

source("code/timelines_union.R")

# 6. Clean union-level timelines

source("code/clean_timelines_unions.R")

# 7. Run the file that creates timelines at the individual level

source("code/timelines_individuals.R")

# 8. Clean individual-level timelines

source("code/clean_timelines_individuals.R")

# 9. Derive migration events

source("code/get_moves.R")

# 10. Build the analytic samples

source("code/build_analytic_sample.R")

# 11. Make figures

source("code/figures_PNAS.R")

# 12. Make tables and compute a few numbers cited in the paper

source("code/tables_PNAS.R")

