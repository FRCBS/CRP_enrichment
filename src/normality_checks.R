## Normality checks from bootstrap objects
## to determine if normal approximated CIs are OK
## This is for the Ferritin x CRP publication

LOADPATH <- "~/CRP_enrichment/data/"

# Load RDS objects
frCRP <- readRDS(paste0(LOADPATH, "PUBL_", "finrisk_CRP_", "10000.rds"))
frGlycA <- readRDS(paste0(LOADPATH, "PUBL_", "finrisk_GlycA_", "10000.rds"))
frAPOB <- readRDS(paste0(LOADPATH, "PUBL_", "finrisk_APOB_", "10000.rds"))
frAPOA1 <- readRDS(paste0(LOADPATH, "PUBL_", "finrisk_APOA1_", "10000.rds"))

h2kCRP <- readRDS(paste0(LOADPATH, "PUBL_", "health2k_CRP_", "10000.rds"))
h2kGlycA <- readRDS(paste0(LOADPATH, "PUBL_", "health2k_GlycA_", "10000.rds"))
h2kHbA1C <- readRDS(paste0(LOADPATH, "PUBL_", "health2k_HbA1C_", "10000.rds"))
h2kAPOB <- readRDS(paste0(LOADPATH, "PUBL_", "health2k_APOB_", "10000.rds"))
h2kAPOA1 <- readRDS(paste0(LOADPATH, "PUBL_", "health2k_APOA1_", "10000.rds"))

# The loaded lists each contain 3x51 separate bootobjs.
# One for every subgroup (men, menstruating women, non-menstruating women) and for ferritin thresholds 0-50
# However, for the sake of sanity, we are interested only on the most crucial comparison points:
# no filter against filter levels of 15, 30 and 50. So we'll be drawing Q-Q plots for these.
#
# NB! Shapiro-Wilk tests and other significance tests for normality won't achieve what we want here.
# We are interested "good enough normality", and these tests will 1) lack power with small sample sizes
# 2) always reject normality with large sample sizes.

qqnorm(y = frCRP$men[[16]]$t, main = "QQ for: FinRisk 1997 | CRP | Men | filter at 15"); qqline(y = frCRP$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$men[[31]]$t, main = "QQ for: FinRisk 1997 | CRP | Men | filter at 30"); qqline(y = frCRP$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$men[[51]]$t, main = "QQ for: FinRisk 1997 | CRP | Men | filter at 50"); qqline(y = frCRP$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_mens[[16]]$t, main = "QQ for: FinRisk 1997 | CRP | Menstruating women | filter at 15"); qqline(y = frCRP$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_mens[[31]]$t, main = "QQ for: FinRisk 1997 | CRP | Menstruating women | filter at 30"); qqline(y = frCRP$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_mens[[51]]$t, main = "QQ for: FinRisk 1997 | CRP | Menstruating women | filter at 50"); qqline(y = frCRP$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_nonmens[[16]]$t, main = "QQ for: FinRisk 1997 | CRP | Non-menstruating women | filter at 15"); qqline(y = frCRP$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_nonmens[[31]]$t, main = "QQ for: FinRisk 1997 | CRP | Non-menstruating women | filter at 30"); qqline(y = frCRP$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frCRP$women_nonmens[[51]]$t, main = "QQ for: FinRisk 1997 | CRP | Non-menstruating women | filter at 50"); qqline(y = frCRP$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

qqnorm(y = h2kCRP$men[[16]]$t, main = "QQ for: Health 2000 | CRP | Men | filter at 15"); qqline(y = h2kCRP$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$men[[31]]$t, main = "QQ for: Health 2000 | CRP | Men | filter at 30"); qqline(y = h2kCRP$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$men[[51]]$t, main = "QQ for: Health 2000 | CRP | Men | filter at 50"); qqline(y = h2kCRP$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_mens[[16]]$t, main = "QQ for: Health 2000 | CRP | Menstruating women | filter at 15"); qqline(y = h2kCRP$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_mens[[31]]$t, main = "QQ for: Health 2000 | CRP | Menstruating women | filter at 30"); qqline(y = h2kCRP$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_mens[[51]]$t, main = "QQ for: Health 2000 | CRP | Menstruating women | filter at 50"); qqline(y = h2kCRP$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_nonmens[[16]]$t, main = "QQ for: Health 2000 | CRP | Non-menstruating women | filter at 15"); qqline(y = h2kCRP$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_nonmens[[31]]$t, main = "QQ for: Health 2000 | CRP | Non-menstruating women | filter at 30"); qqline(y = h2kCRP$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kCRP$women_nonmens[[51]]$t, main = "QQ for: Health 2000 | CRP | Non-menstruating women | filter at 50"); qqline(y = h2kCRP$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

# Next the markers included in the supplement. For these the normality assumption
# would be useful, but not crucial: we are not really arguing much about them.

# GlyCA

qqnorm(y = frGlycA$men[[16]]$t, main = "QQ for: FinRisk 1997 | GlycA | Men | filter at 15"); qqline(y = frGlycA$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$men[[31]]$t, main = "QQ for: FinRisk 1997 | GlycA | Men | filter at 30"); qqline(y = frGlycA$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$men[[51]]$t, main = "QQ for: FinRisk 1997 | GlycA | Men | filter at 50"); qqline(y = frGlycA$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_mens[[16]]$t, main = "QQ for: FinRisk 1997 | GlycA | Menstruating women | filter at 15"); qqline(y = frGlycA$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_mens[[31]]$t, main = "QQ for: FinRisk 1997 | GlycA | Menstruating women | filter at 30"); qqline(y = frGlycA$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_mens[[51]]$t, main = "QQ for: FinRisk 1997 | GlycA | Menstruating women | filter at 50"); qqline(y = frGlycA$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_nonmens[[16]]$t, main = "QQ for: FinRisk 1997 | GlycA | Non-menstruating women | filter at 15"); qqline(y = frGlycA$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_nonmens[[31]]$t, main = "QQ for: FinRisk 1997 | GlycA | Non-menstruating women | filter at 30"); qqline(y = frGlycA$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frGlycA$women_nonmens[[51]]$t, main = "QQ for: FinRisk 1997 | GlycA | Non-menstruating women | filter at 50"); qqline(y = frGlycA$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

qqnorm(y = h2kGlycA$men[[16]]$t, main = "QQ for: Health 2000 | GlycA | Men | filter at 15"); qqline(y = h2kGlycA$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$men[[31]]$t, main = "QQ for: Health 2000 | GlycA | Men | filter at 30"); qqline(y = h2kGlycA$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$men[[51]]$t, main = "QQ for: Health 2000 | GlycA | Men | filter at 50"); qqline(y = h2kGlycA$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_mens[[16]]$t, main = "QQ for: Health 2000 | GlycA | Menstruating women | filter at 15"); qqline(y = h2kGlycA$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_mens[[31]]$t, main = "QQ for: Health 2000 | GlycA | Menstruating women | filter at 30"); qqline(y = h2kGlycA$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_mens[[51]]$t, main = "QQ for: Health 2000 | GlycA | Menstruating women | filter at 50"); qqline(y = h2kGlycA$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_nonmens[[16]]$t, main = "QQ for: Health 2000 | GlycA | Non-menstruating women | filter at 15"); qqline(y = h2kGlycA$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_nonmens[[31]]$t, main = "QQ for: Health 2000 | GlycA | Non-menstruating women | filter at 30"); qqline(y = h2kGlycA$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kGlycA$women_nonmens[[51]]$t, main = "QQ for: Health 2000 | GlycA | Non-menstruating women | filter at 50"); qqline(y = h2kGlycA$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

# HbA1C

qqnorm(y = h2kHbA1C$men[[16]]$t, main = "QQ for: Health 2000 | HbA1CA| Men | filter at 15"); qqline(y = h2kHbA1C$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$men[[31]]$t, main = "QQ for: Health 2000 | HbA1CA | Men | filter at 30"); qqline(y = h2kHbA1C$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$men[[51]]$t, main = "QQ for: Health 2000 | HbA1CA | Men | filter at 50"); qqline(y = h2kHbA1C$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_mens[[16]]$t, main = "QQ for: Health 2000 | HbA1CA | Menstruating women | filter at 15"); qqline(y = h2kHbA1C$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_mens[[31]]$t, main = "QQ for: Health 2000 | HbA1CA | Menstruating women | filter at 30"); qqline(y = h2kHbA1C$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_mens[[51]]$t, main = "QQ for: Health 2000 | HbA1CA | Menstruating women | filter at 50"); qqline(y = h2kHbA1C$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_nonmens[[16]]$t, main = "QQ for: Health 2000 | HbA1CA | Non-menstruating women | filter at 15"); qqline(y = h2kHbA1C$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_nonmens[[31]]$t, main = "QQ for: Health 2000 | HbA1CA | Non-menstruating women | filter at 30"); qqline(y = h2kHbA1C$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kHbA1C$women_nonmens[[51]]$t, main = "QQ for: Health 2000 | HbA1CA | Non-menstruating women | filter at 50"); qqline(y = h2kHbA1C$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

# APOB

qqnorm(y = frAPOB$men[[16]]$t, main = "QQ for: FinRisk 1997 | APOB | Men | filter at 15"); qqline(y = frAPOB$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$men[[31]]$t, main = "QQ for: FinRisk 1997 | APOB | Men | filter at 30"); qqline(y = frAPOB$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$men[[51]]$t, main = "QQ for: FinRisk 1997 | APOB | Men | filter at 50"); qqline(y = frAPOB$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_mens[[16]]$t, main = "QQ for: FinRisk 1997 | APOB | Menstruating women | filter at 15"); qqline(y = frAPOB$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_mens[[31]]$t, main = "QQ for: FinRisk 1997 | APOB | Menstruating women | filter at 30"); qqline(y = frAPOB$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_mens[[51]]$t, main = "QQ for: FinRisk 1997 | APOB | Menstruating women | filter at 50"); qqline(y = frAPOB$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_nonmens[[16]]$t, main = "QQ for: FinRisk 1997 | APOB | Non-menstruating women | filter at 15"); qqline(y = frAPOB$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_nonmens[[31]]$t, main = "QQ for: FinRisk 1997 | APOB | Non-menstruating women | filter at 30"); qqline(y = frAPOB$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOB$women_nonmens[[51]]$t, main = "QQ for: FinRisk 1997 | APOB | Non-menstruating women | filter at 50"); qqline(y = frAPOB$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

qqnorm(y = h2kAPOB$men[[16]]$t, main = "QQ for: Health 2000 | APOB | Men | filter at 15"); qqline(y = h2kAPOB$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$men[[31]]$t, main = "QQ for: Health 2000 | APOB | Men | filter at 30"); qqline(y = h2kAPOB$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$men[[51]]$t, main = "QQ for: Health 2000 | APOB | Men | filter at 50"); qqline(y = h2kAPOB$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_mens[[16]]$t, main = "QQ for: Health 2000 | APOB | Menstruating women | filter at 15"); qqline(y = h2kAPOB$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_mens[[31]]$t, main = "QQ for: Health 2000 | APOB | Menstruating women | filter at 30"); qqline(y = h2kAPOB$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_mens[[51]]$t, main = "QQ for: Health 2000 | APOB | Menstruating women | filter at 50"); qqline(y = h2kAPOB$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_nonmens[[16]]$t, main = "QQ for: Health 2000 | APOB | Non-menstruating women | filter at 15"); qqline(y = h2kAPOB$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_nonmens[[31]]$t, main = "QQ for: Health 2000 | APOB | Non-menstruating women | filter at 30"); qqline(y = h2kAPOB$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOB$women_nonmens[[51]]$t, main = "QQ for: Health 2000 | APOB | Non-menstruating women | filter at 50"); qqline(y = h2kAPOB$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

# APOA1

qqnorm(y = frAPOA1$men[[16]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Men | filter at 15"); qqline(y = frAPOA1$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$men[[31]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Men | filter at 30"); qqline(y = frAPOA1$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$men[[51]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Men | filter at 50"); qqline(y = frAPOA1$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_mens[[16]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Menstruating women | filter at 15"); qqline(y = frAPOA1$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_mens[[31]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Menstruating women | filter at 30"); qqline(y = frAPOA1$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_mens[[51]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Menstruating women | filter at 50"); qqline(y = frAPOA1$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_nonmens[[16]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Non-menstruating women | filter at 15"); qqline(y = frAPOA1$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_nonmens[[31]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Non-menstruating women | filter at 30"); qqline(y = frAPOA1$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = frAPOA1$women_nonmens[[51]]$t, main = "QQ for: FinRisk 1997 | APOA1 | Non-menstruating women | filter at 50"); qqline(y = frAPOA1$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)

qqnorm(y = h2kAPOA1$men[[16]]$t, main = "QQ for: Health 2000 | APOA1 | Men | filter at 15"); qqline(y = h2kAPOA1$men[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$men[[31]]$t, main = "QQ for: Health 2000 | APOA1 | Men | filter at 30"); qqline(y = h2kAPOA1$men[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$men[[51]]$t, main = "QQ for: Health 2000 | APOA1 | Men | filter at 50"); qqline(y = h2kAPOA1$men[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_mens[[16]]$t, main = "QQ for: Health 2000 | APOA1 | Menstruating women | filter at 15"); qqline(y = h2kAPOA1$women_mens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_mens[[31]]$t, main = "QQ for: Health 2000 | APOA1 | Menstruating women | filter at 30"); qqline(y = h2kAPOA1$women_mens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_mens[[51]]$t, main = "QQ for: Health 2000 | APOA1 | Menstruating women | filter at 50"); qqline(y = h2kAPOA1$women_mens[[51]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_nonmens[[16]]$t, main = "QQ for: Health 2000 | APOA1 | Non-menstruating women | filter at 15"); qqline(y = h2kAPOA1$women_nonmens[[16]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_nonmens[[31]]$t, main = "QQ for: Health 2000 | APOA1 | Non-menstruating women | filter at 30"); qqline(y = h2kAPOA1$women_nonmens[[31]]$t, col = 2, lwd = 2, lty = 2)
qqnorm(y = h2kAPOA1$women_nonmens[[51]]$t, main = "QQ for: Health 2000 | APOA1 | Non-menstruating women | filter at 50"); qqline(y = h2kAPOA1$women_nonmens[[51]]$t, col = 2, lwd = 2, lty = 2)
