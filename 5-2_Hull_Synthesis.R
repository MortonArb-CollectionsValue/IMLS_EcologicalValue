# Doign some synthesis of the hull analysis
path.dat <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Environmental Niche Value" ## path for Shannon & Christy
path.out <- file.path(path.dat, "Analysis/PrelimPCA/figures")
path.figs <- file.path(path.dat, "figures")

load(file.path(path.dat, "Extracted Data", "HullAnaly.RData"))


gen.stats
