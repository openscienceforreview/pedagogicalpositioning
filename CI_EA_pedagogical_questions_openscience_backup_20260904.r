# PPedagogical Positioning in Parents’ and Children’s Questions:
# Cultural Patterns and Links to Early Math and Reading

# ===========================================================================
# 1. PACKAGES
# ===========================================================================

# Packages ----------------------------------------------------------------
library(ggplot2)
library(gridExtra)
library(tidyr)
library(stringr)
library(dplyr)
library(forcats)
library(QuantPsyc)
library(broom)
library(openxlsx)

# ===========================================================================
# 2. DATA
# ===========================================================================

# Importing data ----------------------------------------------------------
qp_full_y12 <- read.csv(file = "/Users/ranwei/Dropbox (Personal)/Brown Questions Project/QP R scripts and data/qp_full_y12_20260721.csv", header = TRUE)
qp_full_y12_ca <- read.csv(file = "/Users/ranwei/Dropbox (Personal)/Brown Questions Project/QP R scripts and data/qp_full_y12_ca_20260721.csv", header = TRUE)
qp_full_y12_ea <- read.csv(file = "/Users/ranwei/Dropbox (Personal)/Brown Questions Project/QP R scripts and data/qp_full_y12_ea_20260721.csv", header = TRUE)

# Convert education codes to years of education.

qp_full_y12$momed_years <- qp_full_y12$momed
summary(qp_full_y12$momed_years)
qp_full_y12$momed_years[qp_full_y12$momed_years == 4] <- 12 # high school graduate or GED
qp_full_y12$momed_years[qp_full_y12$momed_years == 5] <- 14 # partial college or has completed specialized training
qp_full_y12$momed_years[qp_full_y12$momed_years == 6] <- 16 # standard college or university graduate 
qp_full_y12$momed_years[qp_full_y12$momed_years == 7] <- 18 # graduate professional training (at least 18 years of education for master's degree)

qp_full_y12$daded_years <- qp_full_y12$daded
summary(qp_full_y12$daded_years)
qp_full_y12$daded_years[qp_full_y12$daded_years == 4] <- 12 # high school graduate or GED
qp_full_y12$daded_years[qp_full_y12$daded_years == 5] <- 14 # partial college or has completed specialized training
qp_full_y12$daded_years[qp_full_y12$daded_years == 6] <- 16 # standard college or university graduate 
qp_full_y12$daded_years[qp_full_y12$daded_years == 7] <- 18 # graduate professional training (at least 18 years of education for master's degree)
summary(qp_full_y12$daded_years)

qp_full_y12_ca$momed_years <- qp_full_y12_ca$momed
summary(qp_full_y12_ca$momed_years)
qp_full_y12_ca$momed_years[qp_full_y12_ca$momed_years == 4] <- 12 # high school graduate or GED
qp_full_y12_ca$momed_years[qp_full_y12_ca$momed_years == 5] <- 14 # partial college or has completed specialized training
qp_full_y12_ca$momed_years[qp_full_y12_ca$momed_years == 6] <- 16 # standard college or university graduate 
qp_full_y12_ca$momed_years[qp_full_y12_ca$momed_years == 7] <- 18 # graduate professional training (at least 18 years of education for master's degree)

qp_full_y12_ca$daded_years <- qp_full_y12_ca$daded
summary(qp_full_y12_ca$daded_years)
qp_full_y12_ca$daded_years[qp_full_y12_ca$daded_years == 4] <- 12 # high school graduate or GED
qp_full_y12_ca$daded_years[qp_full_y12_ca$daded_years == 5] <- 14 # partial college or has completed specialized training
qp_full_y12_ca$daded_years[qp_full_y12_ca$daded_years == 6] <- 16 # standard college or university graduate 
qp_full_y12_ca$daded_years[qp_full_y12_ca$daded_years == 7] <- 18 # graduate professional training (at least 18 years of education for master's degree)
summary(qp_full_y12_ca$daded_years)

# ===========================================================================
# 3. METHODS
# ===========================================================================

# --- 3.1 Participants ------------------------------------------------------

# Demographics ------------------------------------------------------------
table(qp_full_y12$chinese)
table(qp_full_y12_ca$childsex) # 1 = boy, 2 = girl 
table(qp_full_y12_ea$childsex)

summary(qp_full_y12_ca$age_months)
sd(qp_full_y12_ca$age_months)
summary(qp_full_y12_ea$age_months)
sd(qp_full_y12_ea$age_months)

summary(qp_full_y12_ca$y2age_months)
mean(qp_full_y12_ca$y2age_months, na.rm = TRUE)
sd(qp_full_y12_ca$y2age_months, na.rm = TRUE)

summary(qp_full_y12_ea$y2age_months)
mean(qp_full_y12_ea$y2age_months, na.rm = TRUE)
sd(qp_full_y12_ea$y2age_months, na.rm = TRUE)

summary(qp_full_y12_ca$hh_ses)
sd(qp_full_y12_ca$hh_ses)
summary(qp_full_y12_ea$hh_ses)
sd(qp_full_y12_ea$hh_ses)

summary(qp_full_y12_ca$momed_years)
sd(qp_full_y12_ca$momed_years)
summary(qp_full_y12_ea$momed_years)
sd(qp_full_y12_ea$momed_years)

summary(qp_full_y12_ca$daded_years)
sd(qp_full_y12_ca$daded_years)
summary(qp_full_y12_ea$daded_years)
sd(qp_full_y12_ea$daded_years)

t.test(qp_full_y12$age_months ~ qp_full_y12$chinese)
t.test(qp_full_y12$y2age_months ~ qp_full_y12$chinese)
t.test(qp_full_y12$momed_years ~ qp_full_y12$chinese)
t.test(qp_full_y12$hh_ses ~ qp_full_y12$chinese)
t.test(qp_full_y12$daded_years ~ qp_full_y12$chinese)

# Caregiver type by ethnocultural group ------------------------------------
# mother_participate: 0 = father, 1 = mother, 2 = grandmother.
# Reported in Participants as a mother vs. non-mother comparison, because the
# father and grandmother cells are too sparse to support a 3 x 2 test.
table(qp_full_y12$mother_participate, qp_full_y12$chinese)

caregiver_type <- ifelse(qp_full_y12$mother_participate == 1, "mother", "other")
caregiver_tab  <- table(caregiver_type, qp_full_y12$chinese)
caregiver_tab
fisher.test(caregiver_tab)

# Full 3 x 2 table, for reference
fisher.test(table(qp_full_y12$mother_participate, qp_full_y12$chinese))

# Preschool type by ethnocultural group ------------------------------------
# 1 = private, 0 = public, blank = undisclosed or not attending preschool
preschool_tab <- table(qp_full_y12$preschool_type, qp_full_y12$chinese)
preschool_tab
round(prop.table(preschool_tab, 2) * 100, 1)
fisher.test(preschool_tab)

# Private vs. public only, excluding the undisclosed cases
fisher.test(preschool_tab[c("private", "public"), ])

# Weekly preschool hours ----------------------------------------------------
# Reported in Table 1 and in the Participants section.

t.test(qp_full_y12$sch_hr ~ qp_full_y12$chinese)

mean(qp_full_y12_ca$sch_hr)
sd(qp_full_y12_ca$sch_hr)
range(qp_full_y12_ca$sch_hr)

mean(qp_full_y12_ea$sch_hr)
sd(qp_full_y12_ea$sch_hr)
range(qp_full_y12_ea$sch_hr)

# Robustness check: mother-child dyads only ---------------------------------
# The Participants section reports that the ethnocultural comparisons hold
# when the sample is restricted to mother-child dyads. The models below are
# the main group comparisons refit on that subset.

qp_mothers <- qp_full_y12[qp_full_y12$mother_participate == 1, ]
table(qp_mothers$chinese)   # 0 = EA, 1 = CI

# Total questions
summary(lm(prt_totalq ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(chi_totalq ~ chinese + momed_years + childsex + age_months, data = qp_mothers))

# Social-pragmatic function (proportion scores)
summary(lm(pd_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(ix_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(at_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))

# Linguistic form (proportion scores)
summary(lm(o_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(r_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(c_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))
summary(lm(m_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_mothers))

# Length of maternal U.S. residence

summary(qp_full_y12_ca$mom_usr)
sd(qp_full_y12_ca$mom_usr)

# --- 3.2 Procedure: language of assessment ---------------------------------

table(qp_full_y12_ca$prelas_level1)
(6+13)/55
(16+12)/55
(8)/55
# 5-6 are fluent, 3-4 are limited, and 1-2 are non-English speakers.

qp_full_y12_ca$assess_language <- with(qp_full_y12_ca,
                                       ifelse(prelas_level1 %in% c(1, 2), "mandarin",
                                              ifelse(prelas_level1 %in% c(3, 4), "mix",
                                                     ifelse(prelas_level1 == 5, "english", NA))))
table(qp_full_y12_ca$assess_language)
aov_math <- aov(y1mhw ~ assess_language, data = qp_full_y12_ca)
summary(aov_math)

qp_full_y12_ca %>%
  group_by(assess_language) %>%
  summarise(n = n(), mean = mean(y1mhw, na.rm = TRUE),
            sd = sd(y1mhw, na.rm = TRUE))

TukeyHSD(aov_math)

aov_reading <- aov(y1rdw ~ assess_language, data = qp_full_y12_ca)
summary(aov_reading)

qp_full_y12_ca %>%
  group_by(assess_language) %>%
  summarise(n = n(), mean = mean(y1rdw, na.rm = TRUE),
            sd = sd(y1rdw, na.rm = TRUE))

TukeyHSD(aov_reading)

# --- 3.3 Caregiver-child interaction task ----------------------------------

# Duration of interactions ------------------------------------------------
sd(qp_full_y12_ca$duration_min)
sd(qp_full_y12_ea$duration_min)
summary(qp_full_y12_ca$duration_min)
summary(qp_full_y12_ea$duration_min)
t.test(qp_full_y12$duration_min ~ qp_full_y12$chinese)

# --- 3.4 Woodcock-Johnson descriptives (Table 1) ---------------------------

summary(qp_full_y12_ca$y1rdw)
mean(qp_full_y12_ca$y1rdw)
sd(qp_full_y12_ca$y1rdw)

summary(qp_full_y12_ca$y1mhw)
mean(qp_full_y12_ca$y1mhw)
sd(qp_full_y12_ca$y1mhw)

summary(qp_full_y12_ea$y1rdw)
mean(qp_full_y12_ea$y1rdw)
sd(qp_full_y12_ea$y1rdw)

summary(qp_full_y12_ea$y1mhw)
mean(qp_full_y12_ea$y1mhw)
sd(qp_full_y12_ea$y1mhw)

# comparing the two groups 
t.test(qp_full_y12$y1rdw ~ qp_full_y12$chinese)
t.test(qp_full_y12$y1mhw ~ qp_full_y12$chinese)

# ===========================================================================
# 4. RESULTS
# ===========================================================================

# --- 4.1 Question use: descriptive statistics ------------------------------

summary(qp_full_y12_ca$prt_totalq)
mean(qp_full_y12_ca$prt_totalq, na.rm = TRUE)
sd(qp_full_y12_ca$prt_totalq, na.rm = TRUE)

summary(qp_full_y12_ea$prt_totalq)
mean(qp_full_y12_ea$prt_totalq, na.rm = TRUE)
sd(qp_full_y12_ea$prt_totalq, na.rm = TRUE)

summary(qp_full_y12_ca$o_prt)
mean(qp_full_y12_ca$o_prt, na.rm = TRUE)
sd(qp_full_y12_ca$o_prt, na.rm = TRUE)

summary(qp_full_y12_ca$c_prt)
mean(qp_full_y12_ca$c_prt, na.rm = TRUE)
sd(qp_full_y12_ca$c_prt, na.rm = TRUE)

summary(qp_full_y12_ca$t_prt)
mean(qp_full_y12_ca$t_prt, na.rm = TRUE)
sd(qp_full_y12_ca$t_prt, na.rm = TRUE)

summary(qp_full_y12_ca$e_prt)
mean(qp_full_y12_ca$e_prt, na.rm = TRUE)
sd(qp_full_y12_ca$e_prt, na.rm = TRUE)

summary(qp_full_y12_ca$r_prt)
mean(qp_full_y12_ca$r_prt, na.rm = TRUE)
sd(qp_full_y12_ca$r_prt, na.rm = TRUE)

summary(qp_full_y12_ca$f_prt)
mean(qp_full_y12_ca$f_prt, na.rm = TRUE)
sd(qp_full_y12_ca$f_prt, na.rm = TRUE)

summary(qp_full_y12_ca$m_prt)
mean(qp_full_y12_ca$m_prt, na.rm = TRUE)
sd(qp_full_y12_ca$m_prt, na.rm = TRUE)

summary(qp_full_y12_ca$i_prt)
mean(qp_full_y12_ca$i_prt, na.rm = TRUE)
sd(qp_full_y12_ca$i_prt, na.rm = TRUE)


summary(qp_full_y12_ca$o_chi)
mean(qp_full_y12_ca$o_chi, na.rm = TRUE)
sd(qp_full_y12_ca$o_chi, na.rm = TRUE)

summary(qp_full_y12_ca$c_chi)
mean(qp_full_y12_ca$c_chi, na.rm = TRUE)
sd(qp_full_y12_ca$c_chi, na.rm = TRUE)

summary(qp_full_y12_ca$t_chi)
mean(qp_full_y12_ca$t_chi, na.rm = TRUE)
sd(qp_full_y12_ca$t_chi, na.rm = TRUE)

summary(qp_full_y12_ca$e_chi)
mean(qp_full_y12_ca$e_chi, na.rm = TRUE)
sd(qp_full_y12_ca$e_chi, na.rm = TRUE)

summary(qp_full_y12_ca$r_chi)
mean(qp_full_y12_ca$r_chi, na.rm = TRUE)
sd(qp_full_y12_ca$r_chi, na.rm = TRUE)

summary(qp_full_y12_ca$f_chi)
mean(qp_full_y12_ca$f_chi, na.rm = TRUE)
sd(qp_full_y12_ca$f_chi, na.rm = TRUE)

summary(qp_full_y12_ca$m_chi)
mean(qp_full_y12_ca$m_chi, na.rm = TRUE)
sd(qp_full_y12_ca$m_chi, na.rm = TRUE)

summary(qp_full_y12_ca$i_chi)
mean(qp_full_y12_ca$i_chi, na.rm = TRUE)
sd(qp_full_y12_ca$i_chi, na.rm = TRUE)



summary(qp_full_y12_ea$o_prt)
mean(qp_full_y12_ea$o_prt, na.rm = TRUE)
sd(qp_full_y12_ea$o_prt, na.rm = TRUE)

summary(qp_full_y12_ea$c_prt)
mean(qp_full_y12_ea$c_prt, na.rm = TRUE)
sd(qp_full_y12_ea$c_prt, na.rm = TRUE)

summary(qp_full_y12_ea$t_prt)
mean(qp_full_y12_ea$t_prt, na.rm = TRUE)
sd(qp_full_y12_ea$t_prt, na.rm = TRUE)

summary(qp_full_y12_ea$e_prt)
mean(qp_full_y12_ea$e_prt, na.rm = TRUE)
sd(qp_full_y12_ea$e_prt, na.rm = TRUE)

summary(qp_full_y12_ea$r_prt)
mean(qp_full_y12_ea$r_prt, na.rm = TRUE)
sd(qp_full_y12_ea$r_prt, na.rm = TRUE)

summary(qp_full_y12_ea$f_prt)
mean(qp_full_y12_ea$f_prt, na.rm = TRUE)
sd(qp_full_y12_ea$f_prt, na.rm = TRUE)

summary(qp_full_y12_ea$m_prt)
mean(qp_full_y12_ea$m_prt, na.rm = TRUE)
sd(qp_full_y12_ea$m_prt, na.rm = TRUE)

summary(qp_full_y12_ea$i_prt)
mean(qp_full_y12_ea$i_prt, na.rm = TRUE)
sd(qp_full_y12_ea$i_prt, na.rm = TRUE)


summary(qp_full_y12_ea$o_chi)
mean(qp_full_y12_ea$o_chi, na.rm = TRUE)
sd(qp_full_y12_ea$o_chi, na.rm = TRUE)

summary(qp_full_y12_ea$c_chi)
mean(qp_full_y12_ea$c_chi, na.rm = TRUE)
sd(qp_full_y12_ea$c_chi, na.rm = TRUE)

summary(qp_full_y12_ea$t_chi)
mean(qp_full_y12_ea$t_chi, na.rm = TRUE)
sd(qp_full_y12_ea$t_chi, na.rm = TRUE)

summary(qp_full_y12_ea$e_chi)
mean(qp_full_y12_ea$e_chi, na.rm = TRUE)
sd(qp_full_y12_ea$e_chi, na.rm = TRUE)

summary(qp_full_y12_ea$r_chi)
mean(qp_full_y12_ea$r_chi, na.rm = TRUE)
sd(qp_full_y12_ea$r_chi, na.rm = TRUE)

summary(qp_full_y12_ea$f_chi)
mean(qp_full_y12_ea$f_chi, na.rm = TRUE)
sd(qp_full_y12_ea$f_chi, na.rm = TRUE)

summary(qp_full_y12_ea$m_chi)
mean(qp_full_y12_ea$m_chi, na.rm = TRUE)
sd(qp_full_y12_ea$m_chi, na.rm = TRUE)

summary(qp_full_y12_ea$i_chi)
mean(qp_full_y12_ea$i_chi, na.rm = TRUE)
sd(qp_full_y12_ea$i_chi, na.rm = TRUE)


summary(qp_full_y12_ca$ix_prt)
mean(qp_full_y12_ca$ix_prt, na.rm = TRUE)
sd(qp_full_y12_ca$ix_prt, na.rm = TRUE)

summary(qp_full_y12_ca$pd_prt)
mean(qp_full_y12_ca$pd_prt, na.rm = TRUE)
sd(qp_full_y12_ca$pd_prt, na.rm = TRUE)

summary(qp_full_y12_ca$ha_prt)
mean(qp_full_y12_ca$ha_prt, na.rm = TRUE)
sd(qp_full_y12_ca$ha_prt, na.rm = TRUE)

summary(qp_full_y12_ca$ch_prt)
mean(qp_full_y12_ca$ch_prt, na.rm = TRUE)
sd(qp_full_y12_ca$ch_prt, na.rm = TRUE)

summary(qp_full_y12_ca$ac_prt)
mean(qp_full_y12_ca$ac_prt, na.rm = TRUE)
sd(qp_full_y12_ca$ac_prt, na.rm = TRUE)

summary(qp_full_y12_ca$at_prt)
mean(qp_full_y12_ca$at_prt, na.rm = TRUE)
sd(qp_full_y12_ca$at_prt, na.rm = TRUE)

summary(qp_full_y12_ca$co_prt)
mean(qp_full_y12_ca$co_prt, na.rm = TRUE)
sd(qp_full_y12_ca$co_prt, na.rm = TRUE)

summary(qp_full_y12_ca$cl_prt)
mean(qp_full_y12_ca$cl_prt, na.rm = TRUE)
sd(qp_full_y12_ca$cl_prt, na.rm = TRUE)

summary(qp_full_y12_ca$em_prt)
mean(qp_full_y12_ca$em_prt, na.rm = TRUE)
sd(qp_full_y12_ca$em_prt, na.rm = TRUE)

summary(qp_full_y12_ca$st_prt)
mean(qp_full_y12_ca$st_prt, na.rm = TRUE)
sd(qp_full_y12_ca$st_prt, na.rm = TRUE)


summary(qp_full_y12_ea$ix_prt)
mean(qp_full_y12_ea$ix_prt, na.rm = TRUE)
sd(qp_full_y12_ea$ix_prt, na.rm = TRUE)

summary(qp_full_y12_ea$pd_prt)
mean(qp_full_y12_ea$pd_prt, na.rm = TRUE)
sd(qp_full_y12_ea$pd_prt, na.rm = TRUE)

summary(qp_full_y12_ea$ha_prt)
mean(qp_full_y12_ea$ha_prt, na.rm = TRUE)
sd(qp_full_y12_ea$ha_prt, na.rm = TRUE)

summary(qp_full_y12_ea$ch_prt)
mean(qp_full_y12_ea$ch_prt, na.rm = TRUE)
sd(qp_full_y12_ea$ch_prt, na.rm = TRUE)

summary(qp_full_y12_ea$ac_prt)
mean(qp_full_y12_ea$ac_prt, na.rm = TRUE)
sd(qp_full_y12_ea$ac_prt, na.rm = TRUE)

summary(qp_full_y12_ea$at_prt)
mean(qp_full_y12_ea$at_prt, na.rm = TRUE)
sd(qp_full_y12_ea$at_prt, na.rm = TRUE)

summary(qp_full_y12_ea$co_prt)
mean(qp_full_y12_ea$co_prt, na.rm = TRUE)
sd(qp_full_y12_ea$co_prt, na.rm = TRUE)

summary(qp_full_y12_ea$cl_prt)
mean(qp_full_y12_ea$cl_prt, na.rm = TRUE)
sd(qp_full_y12_ea$cl_prt, na.rm = TRUE)

summary(qp_full_y12_ea$em_prt)
mean(qp_full_y12_ea$em_prt, na.rm = TRUE)
sd(qp_full_y12_ea$em_prt, na.rm = TRUE)

summary(qp_full_y12_ea$st_prt)
mean(qp_full_y12_ea$st_prt, na.rm = TRUE)
sd(qp_full_y12_ea$st_prt, na.rm = TRUE)



summary(qp_full_y12_ca$ix_chi)
mean(qp_full_y12_ca$ix_chi, na.rm = TRUE)
sd(qp_full_y12_ca$ix_chi, na.rm = TRUE)

summary(qp_full_y12_ca$pd_chi)
mean(qp_full_y12_ca$pd_chi, na.rm = TRUE)
sd(qp_full_y12_ca$pd_chi, na.rm = TRUE)

summary(qp_full_y12_ca$ha_chi)
mean(qp_full_y12_ca$ha_chi, na.rm = TRUE)
sd(qp_full_y12_ca$ha_chi, na.rm = TRUE)

summary(qp_full_y12_ca$ch_chi)
mean(qp_full_y12_ca$ch_chi, na.rm = TRUE)
sd(qp_full_y12_ca$ch_chi, na.rm = TRUE)

summary(qp_full_y12_ca$ac_chi)
mean(qp_full_y12_ca$ac_chi, na.rm = TRUE)
sd(qp_full_y12_ca$ac_chi, na.rm = TRUE)

summary(qp_full_y12_ca$at_chi)
mean(qp_full_y12_ca$at_chi, na.rm = TRUE)
sd(qp_full_y12_ca$at_chi, na.rm = TRUE)

summary(qp_full_y12_ca$co_chi)
mean(qp_full_y12_ca$co_chi, na.rm = TRUE)
sd(qp_full_y12_ca$co_chi, na.rm = TRUE)

summary(qp_full_y12_ca$cl_chi)
mean(qp_full_y12_ca$cl_chi, na.rm = TRUE)
sd(qp_full_y12_ca$cl_chi, na.rm = TRUE)

summary(qp_full_y12_ca$em_chi)
mean(qp_full_y12_ca$em_chi, na.rm = TRUE)
sd(qp_full_y12_ca$em_chi, na.rm = TRUE)

summary(qp_full_y12_ca$st_chi)
mean(qp_full_y12_ca$st_chi, na.rm = TRUE)
sd(qp_full_y12_ca$st_chi, na.rm = TRUE)


summary(qp_full_y12_ea$ix_chi)
mean(qp_full_y12_ea$ix_chi, na.rm = TRUE)
sd(qp_full_y12_ea$ix_chi, na.rm = TRUE)

summary(qp_full_y12_ea$pd_chi)
mean(qp_full_y12_ea$pd_chi, na.rm = TRUE)
sd(qp_full_y12_ea$pd_chi, na.rm = TRUE)

summary(qp_full_y12_ea$ha_chi)
mean(qp_full_y12_ea$ha_chi, na.rm = TRUE)
sd(qp_full_y12_ea$ha_chi, na.rm = TRUE)

summary(qp_full_y12_ea$ch_chi)
mean(qp_full_y12_ea$ch_chi, na.rm = TRUE)
sd(qp_full_y12_ea$ch_chi, na.rm = TRUE)

summary(qp_full_y12_ea$ac_chi)
mean(qp_full_y12_ea$ac_chi, na.rm = TRUE)
sd(qp_full_y12_ea$ac_chi, na.rm = TRUE)

summary(qp_full_y12_ea$at_chi)
mean(qp_full_y12_ea$at_chi, na.rm = TRUE)
sd(qp_full_y12_ea$at_chi, na.rm = TRUE)

summary(qp_full_y12_ea$co_chi)
mean(qp_full_y12_ea$co_chi, na.rm = TRUE)
sd(qp_full_y12_ea$co_chi, na.rm = TRUE)

summary(qp_full_y12_ea$cl_chi)
mean(qp_full_y12_ea$cl_chi, na.rm = TRUE)
sd(qp_full_y12_ea$cl_chi, na.rm = TRUE)

summary(qp_full_y12_ea$em_chi)
mean(qp_full_y12_ea$em_chi, na.rm = TRUE)
sd(qp_full_y12_ea$em_chi, na.rm = TRUE)

summary(qp_full_y12_ea$st_chi)
mean(qp_full_y12_ea$st_chi, na.rm = TRUE)
sd(qp_full_y12_ea$st_chi, na.rm = TRUE)

# --- 4.2 Group comparisons: frequency scores -------------------------------

# Regression models: form comparisons -------------------------------------------------------

m1 <- lm(prt_totalq ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1) 
m1 <- lm(chi_totalq ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1) 

summary(qp_full_y12_ca$chi_totalq)
summary(qp_full_y12_ea$chi_totalq)
sd(qp_full_y12_ca$chi_totalq)
summary(qp_full_y12_ea$chi_totalq)
sd(qp_full_y12_ea$chi_totalq)


m1 <- lm(o_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(c_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(o_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(c_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(co_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_prt ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(co_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_chi ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

# --- 4.3 Group comparisons: proportion scores ------------------------------

# Regression models comparing proportion scores for each question type ---------------------------------------------------

m1 <- lm(c_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(o_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(o_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(c_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(co_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_prt_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
lm.beta(m1)
m1 <- lm(co_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_chi_pct ~ chinese + momed_years + childsex + age_months, data = qp_full_y12)
summary(m1)


# exporting the comparison models -----------------------------------------

data <- qp_full_y12
covariates <- "chinese + momed_years + childsex + age_months"

# ---- Assemble the full list of outcomes, matching your script ---------------
# Form categories
form_types <- c("o","c","t","e","r","f","m","i")
# Function categories
func_types <- c("ix","pd","ha","ch","ac","co","at","cl","em","st")

# Frequency outcomes (raw counts)
freq_outcomes <- c(
  paste0(form_types, "_prt"), paste0(form_types, "_chi"),
  paste0(func_types, "_prt"), paste0(func_types, "_chi")
)
# Proportion outcomes (percent-of-total)
pct_outcomes <- c(
  paste0(form_types, "_prt_pct"), paste0(form_types, "_chi_pct"),
  paste0(func_types, "_prt_pct"), paste0(func_types, "_chi_pct")
)

all_outcomes <- c(freq_outcomes, pct_outcomes)

# Keep only outcomes that actually exist in the data (guards against typos /
# categories that don't have a _pct version, etc.)
all_outcomes <- all_outcomes[all_outcomes %in% names(data)]

# ---- Fit each model; collect coefficients and fit statistics ----------------
coef_list <- list()
fit_list  <- list()

for (dv in all_outcomes) {
  f <- as.formula(paste(dv, "~", covariates))
  m <- lm(f, data = data)
  
  td <- broom::tidy(m)   # term, estimate, std.error, statistic, p.value
  
  # standardized betas aligned to the same term order
  sb <- tryCatch({
    b <- lm.beta::lm.beta(m)$standardized.coefficients
    b[match(td$term, names(b))]
  }, error = function(e) rep(NA_real_, nrow(td)))
  td$std_beta <- as.numeric(sb)
  
  td$outcome  <- dv
  td$measure  <- ifelse(grepl("_pct$", dv), "proportion", "frequency")
  td$speaker  <- ifelse(grepl("_chi", dv), "child", "parent")
  coef_list[[dv]] <- td[, c("outcome","measure","speaker","term",
                            "estimate","std.error","statistic","p.value","std_beta")]
  
  gl <- broom::glance(m)
  gl$outcome <- dv
  gl$measure <- ifelse(grepl("_pct$", dv), "proportion", "frequency")
  gl$speaker <- ifelse(grepl("_chi", dv), "child", "parent")
  gl$n       <- nobs(m)
  fit_list[[dv]] <- gl[, c("outcome","measure","speaker","r.squared",
                           "adj.r.squared","statistic","df","df.residual","p.value","n")]
}

coefficients <- dplyr::bind_rows(coef_list)
model_fit    <- dplyr::bind_rows(fit_list)

# round for readability
coefficients <- coefficients %>%
  mutate(across(c(estimate, std.error, statistic, p.value, std_beta), ~ round(., 5)))
model_fit <- model_fit %>%
  mutate(across(c(r.squared, adj.r.squared, statistic, p.value), ~ round(., 5)))

# ---- Write one Excel workbook with two sheets -------------------------------
writexl::write_xlsx(
  list(coefficients = coefficients, model_fit = model_fit),
  path = "all_question_models.xlsx"
)

cat("Wrote all_question_models.xlsx:",
    nrow(coefficients), "coefficient rows across",
    length(all_outcomes), "models.\n")
cat("Outcomes included:\n"); print(all_outcomes)

# --- 4.4 Linguistic forms within pedagogical questions ---------------------

# Comparing forms of pedagogical questions ---------------------
t.test(qp_full_y12$c ~ qp_full_y12$chinese)
t.test(qp_full_y12$o ~ qp_full_y12$chinese)
t.test(qp_full_y12$f ~ qp_full_y12$chinese)
t.test(qp_full_y12$i ~ qp_full_y12$chinese)
t.test(qp_full_y12$m ~ qp_full_y12$chinese)
t.test(qp_full_y12$r ~ qp_full_y12$chinese)
t.test(qp_full_y12$t ~ qp_full_y12$chinese)
t.test(qp_full_y12$e ~ qp_full_y12$chinese)

# Proportion of each linguistic form WITHIN caregivers' pedagogical questions.
# Denominator is pd_prt (each caregiver's total pedagogical questions).
# One EA dyad asked no pedagogical questions, so these are undefined for that
# case and the proportion models below run on n = 113.
qp_full_y12$c_pct_pd <- qp_full_y12$c/qp_full_y12$pd_prt
qp_full_y12$o_pct_pd <- qp_full_y12$o/qp_full_y12$pd_prt
qp_full_y12$f_pct_pd <- qp_full_y12$f/qp_full_y12$pd_prt
qp_full_y12$i_pct_pd <- qp_full_y12$i/qp_full_y12$pd_prt
qp_full_y12$m_pct_pd <- qp_full_y12$m/qp_full_y12$pd_prt
qp_full_y12$r_pct_pd <- qp_full_y12$r/qp_full_y12$pd_prt
qp_full_y12$t_pct_pd <- qp_full_y12$t/qp_full_y12$pd_prt
qp_full_y12$e_pct_pd <- qp_full_y12$e/qp_full_y12$pd_prt

# Regression models: forms of pedagogical questions ------------------------
# Same covariate set as all other group comparisons in this script:
# maternal education (years), child sex, and child age (months).

pedq_forms <- c("c", "o", "f", "i", "m", "r", "t", "e")
pedq_labels <- c(c = "Yes-no", o = "Wh-", f = "Fill-in-the-blank",
                 i = "Incomplete", m = "Mirroring", r = "Rhetorical/tag",
                 t = "Thinking prompt", e = "Empty prompt")

pedq_model <- function(v, measure) {
  outcome <- if (measure == "proportion") paste0(v, "_pct_pd") else v
  m <- lm(as.formula(paste(outcome, "~ chinese + momed_years + childsex + age_months")),
          data = qp_full_y12)
  s  <- summary(m)
  co <- s$coefficients["chinese", ]
  data.frame(form = pedq_labels[[v]], measure = measure,
             B = co[1], SE = co[2], t = co[3], p = co[4],
             R2 = s$r.squared, n = length(m$residuals), row.names = NULL)
}

pedq_results <- do.call(rbind, c(
  lapply(pedq_forms, pedq_model, measure = "frequency"),
  lapply(pedq_forms, pedq_model, measure = "proportion")))

# The manuscript restricts these comparisons to the four most frequent forms;
# Bonferroni-corrected alpha = .05 / 4 = .0125.
pedq_focal <- c("Wh-", "Yes-no", "Rhetorical/tag", "Mirroring")
pedq_results$p_bonf <- ifelse(pedq_results$form %in% pedq_focal,
                              pmin(pedq_results$p * 4, 1), NA)

print(pedq_results[pedq_results$measure == "frequency", ])
print(pedq_results[pedq_results$measure == "proportion", ])

# --- 4.5 Correlations between caregiver and child questions ----------------

# Correlation between parent and child questions ----------------------------------

cor.test(qp_full_y12_ca$prt_totalq, qp_full_y12_ca$chi_totalq, method = "spearman")
cor.test(qp_full_y12_ea$prt_totalq, qp_full_y12_ea$chi_totalq, method = "spearman")

cor.test(qp_full_y12_ca$o_prt_pct, qp_full_y12_ca$o_chi_pct, method = "spearman")
cor.test(qp_full_y12_ca$ix_prt_pct, qp_full_y12_ca$ix_chi_pct, method = "spearman")
cor.test(qp_full_y12_ca$pd_prt_pct, qp_full_y12_ca$pd_chi_pct, method = "spearman")

cor.test(qp_full_y12_ea$o_prt_pct, qp_full_y12_ea$o_chi_pct, method = "spearman")
cor.test(qp_full_y12_ea$ix_prt_pct, qp_full_y12_ea$ix_chi_pct, method = "spearman")
cor.test(qp_full_y12_ea$pd_prt_pct, qp_full_y12_ea$pd_chi_pct, method = "spearman")

# --- 4.6 Questions and achievement (Tables 2 and 3) ------------------------

# Questions predicting reading scores - CI ----------------------------------

m_rwd_pd_prt_ca <- lm(y1rdw ~ log(pd_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ca)
summary(m_rwd_pd_prt_ca) 
m_rwd_ix_prt_ca <- lm(y1rdw ~ log(ix_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ca)
summary(m_rwd_ix_prt_ca) 
m_rwd_o_prt_ca <- lm(y1rdw ~ log(o_prt_pct + 1) + momed_years + age_months + childsex, data = qp_full_y12_ca)
summary(m_rwd_o_prt_ca) 

m_rwd_pd_prt_ca_al <- lm(y1rdw ~ log(pd_prt_pct + 1) + momed_years + age_months + childsex + assess_language, data = qp_full_y12_ca)
summary(m_rwd_pd_prt_ca_al) 
m_rwd_ix_prt_ca_al <- lm(y1rdw ~ log(ix_prt_pct + 1) + momed_years + age_months + childsex + assess_language, data = qp_full_y12_ca)
summary(m_rwd_ix_prt_ca_al) 
m_rwd_o_prt_ca_al <- lm(y1rdw ~ log(o_prt_pct + 1) + momed_years + age_months + childsex + assess_language, data = qp_full_y12_ca)
summary(m_rwd_o_prt_ca_al)

# Questions predicting math scores - CI ---------------------------------------------

m_mhw_pd_prt_ca <- lm(y1mhw ~ log(pd_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ca)
summary(m_mhw_pd_prt_ca) 
m_mhw_ix_prt_ca <- lm(y1mhw ~ log(ix_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ca)
summary(m_mhw_ix_prt_ca) 
m_mhw_o_prt_ca <- lm(y1mhw ~ log(o_prt_pct + 1) + momed_years + age_months + childsex, data = qp_full_y12_ca)
summary(m_mhw_o_prt_ca) 

# how many parents used pedagogical questions 

table(qp_full_y12_ca$pd_prt) #all 
table(qp_full_y12_ea$pd_prt) #all but 1


# Questions predicting reading scores - EA ----------------------------------

m_rwd_pd_prt_ea <- lm(y1rdw ~ log(pd_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ea)
summary(m_rwd_pd_prt_ea) 
m_rwd_ix_prt_ea <- lm(y1rdw ~ log(ix_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ea)
summary(m_rwd_ix_prt_ea) 
m_rwd_o_prt_ea <- lm(y1rdw ~ log(o_prt_pct + 1) + momed_years + age_months + childsex, data = qp_full_y12_ea)
summary(m_rwd_o_prt_ea) 

# Questions predicting math scores - EA ---------------------------------------------

m_mhw_pd_prt_ea <- lm(y1mhw ~ log(pd_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ea)
summary(m_mhw_pd_prt_ea) 
m_mhw_ix_prt_ea <- lm(y1mhw ~ log(ix_prt_pct + 1) + momed_years + age_months + childsex , data = qp_full_y12_ea)
summary(m_mhw_ix_prt_ea) 
m_mhw_o_prt_ea <- lm(y1mhw ~ log(o_prt_pct + 1) + momed_years + age_months + childsex, data = qp_full_y12_ea)
summary(m_mhw_o_prt_ea) 

# exporting model statistics

# Significance stars

sig_mark <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*",
                              ifelse(p < .1, "~", "")))))
}

# Format "B (SE)" with significance

fmt_coef <- function(est, se, p, digits = 2) {
  ifelse(is.na(est), "",
         paste0(
           format(round(est, digits), nsmall = digits),
           sig_mark(p),
           " (", format(round(se, digits), nsmall = digits), ")"
         ))
}


# Build one model column as a named vector

model_to_col <- function(mod, model_name, digits = 2) {
  td <- tidy(mod) %>%
    mutate(cell = fmt_coef(estimate, std.error, p.value, digits)) %>%
    dplyr::select(term, cell)
  
  gl <- glance(mod)
  
  # R2
  r2  <- format(round(gl$r.squared, 3), nsmall = 3)
  
  # F-statistic with df
  # glance(mod) returns statistic (=F), df (=df1), df.residual (=df2) for lm
  f_stat <- gl$statistic
  df1    <- gl$df
  df2    <- gl$df.residual
  f_txt  <- paste0(
    "F(", df1, ", ", df2, ") = ",
    format(round(f_stat, 2), nsmall = 2)
  )
  
  # Return as a data frame for joining
  bind_rows(
    td %>% rename(Effect = term) %>% mutate(Effect = ifelse(Effect == "(Intercept)", "Intercept", Effect)),
    tibble(Effect = "R2", cell = r2),
    tibble(Effect = "F-statistic", cell = f_txt)
  ) %>%
    rename(!!model_name := cell)
}


# Define six lm models 

models <- list(
  Model1 = m_rwd_pd_prt_ea,
  Model2 = m_rwd_ix_prt_ea,
  Model3 = m_rwd_o_prt_ea,
  Model4 = m_mhw_pd_prt_ea,
  Model5 = m_mhw_ix_prt_ea,
  Model6 = m_mhw_o_prt_ea
)

# Build the wide table

cols <- Map(model_to_col, models, names(models))
reg_table <- Reduce(function(x, y) full_join(x, y, by = "Effect"), cols)

# Force row order

row_order <- c(
  "Intercept",
  "log(pd_prt_pct + 1)",
  "log(ix_prt_pct + 1)",
  "log(o_prt_pct + 1)",
  "momed_years",
  "age_months",
  "childsex",
  "R2",
  "F-statistic"
)

reg_table <- reg_table %>%
  mutate(Effect = ifelse(Effect == "(Intercept)", "Intercept", Effect),
         Effect = factor(Effect, levels = row_order)) %>%
  arrange(Effect) %>%
  mutate(Effect = as.character(Effect))

# Write to Excel with basic formatting

wb <- createWorkbook()
addWorksheet(wb, "Regression")

writeData(wb, "Regression", reg_table)

# Basic styling (optional)
headerStyle <- createStyle(textDecoration = "bold")
addStyle(wb, "Regression", headerStyle, rows = 1, cols = 1:ncol(reg_table), gridExpand = TRUE)

# Set column widths
setColWidths(wb, "Regression", cols = 1, widths = 24)
setColWidths(wb, "Regression", cols = 2:ncol(reg_table), widths = 18)

saveWorkbook(wb, "lm_6models_table.xlsx", overwrite = TRUE)

# Table 2: CI families ------------------------------------------------------
# Same layout as Table 3 above, using the six CI models. Reuses model_to_col(),
# row_order, and headerStyle defined in the Table 3 block.

models_ci <- list(
  Model1 = m_rwd_pd_prt_ca,
  Model2 = m_rwd_ix_prt_ca,
  Model3 = m_rwd_o_prt_ca,
  Model4 = m_mhw_pd_prt_ca,
  Model5 = m_mhw_ix_prt_ca,
  Model6 = m_mhw_o_prt_ca
)

cols_ci      <- Map(model_to_col, models_ci, names(models_ci))
reg_table_ci <- Reduce(function(x, y) full_join(x, y, by = "Effect"), cols_ci)

reg_table_ci <- reg_table_ci %>%
  mutate(Effect = ifelse(Effect == "(Intercept)", "Intercept", Effect),
         Effect = factor(Effect, levels = row_order)) %>%
  arrange(Effect) %>%
  mutate(Effect = as.character(Effect))

reg_table_ci

wb_ci <- createWorkbook()
addWorksheet(wb_ci, "Regression")
writeData(wb_ci, "Regression", reg_table_ci)
addStyle(wb_ci, "Regression", headerStyle, rows = 1,
         cols = 1:ncol(reg_table_ci), gridExpand = TRUE)
setColWidths(wb_ci, "Regression", cols = 1, widths = 24)
setColWidths(wb_ci, "Regression", cols = 2:ncol(reg_table_ci), widths = 18)

saveWorkbook(wb_ci, "lm_6models_table_CI.xlsx", overwrite = TRUE)

# --- 4.7 Figure 1 ----------------------------------------------------------
# Data preparation below is also used by Figures S1-S3 in Section 5.

# Plotting parental and child question frequency --------------------------

# Calculating averages and SEs 
qp_full_plot <- qp_full_y12

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_prt_mean = mean(c_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_prt_mean = mean(o_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_prt_mean = mean(r_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_prt_mean = mean(t_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_prt_mean = mean(m_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_prt_mean = mean(f_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_prt_mean = mean(e_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_prt_mean = mean(i_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_prt_mean = mean(ix_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_prt_mean = mean(pd_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_prt_mean = mean(ha_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_prt_mean = mean(ch_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_prt_mean = mean(ac_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_prt_mean = mean(at_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_prt_mean = mean(co_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_prt_mean = mean(cl_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_prt_mean = mean(em_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_prt_mean = mean(st_prt)) %>% 
  dplyr::ungroup()


qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_chi_mean = mean(c_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_chi_mean = mean(o_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_chi_mean = mean(r_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_chi_mean = mean(t_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_chi_mean = mean(m_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_chi_mean = mean(f_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_chi_mean = mean(e_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_chi_mean = mean(i_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_chi_mean = mean(ix_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_chi_mean = mean(pd_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_chi_mean = mean(ha_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_chi_mean = mean(ch_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_chi_mean = mean(ac_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_chi_mean = mean(at_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_chi_mean = mean(co_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_chi_mean = mean(cl_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_chi_mean = mean(em_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_chi_mean = mean(st_chi)) %>% 
  dplyr::ungroup()


# calculating standard errors 

# sample size 
qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(sample_n = length(o_prt)) %>% 
  dplyr::ungroup()

# sd 
qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_prt_sd = sd(c_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_prt_sd = sd(o_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_prt_sd = sd(r_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_prt_sd = sd(t_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_prt_sd = sd(m_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_prt_sd = sd(f_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_prt_sd = sd(e_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_prt_sd = sd(i_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_prt_sd = sd(ix_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_prt_sd = sd(pd_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_prt_sd = sd(ha_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_prt_sd = sd(ch_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_prt_sd = sd(ac_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_prt_sd = sd(at_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_prt_sd = sd(co_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_prt_sd = sd(cl_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_prt_sd = sd(em_prt)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_prt_sd = sd(st_prt)) %>% 
  dplyr::ungroup()


qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_chi_sd = sd(c_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_chi_sd = sd(o_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_chi_sd = sd(r_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_chi_sd = sd(t_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_chi_sd = sd(m_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_chi_sd = sd(f_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_chi_sd = sd(e_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_chi_sd = sd(i_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_chi_sd = sd(ix_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_chi_sd = sd(pd_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_chi_sd = sd(ha_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_chi_sd = sd(ch_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_chi_sd = sd(ac_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_chi_sd = sd(at_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_chi_sd = sd(co_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_chi_sd = sd(cl_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_chi_sd = sd(em_chi)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_chi_sd = sd(st_chi)) %>% 
  dplyr::ungroup()

# Proportion data ----------------------------------------------------------------

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_prt_pct_mean = mean(c_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_prt_pct_mean = mean(o_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_prt_pct_mean = mean(r_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_prt_pct_mean = mean(t_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_prt_pct_mean = mean(m_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_prt_pct_mean = mean(f_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_prt_pct_mean = mean(e_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_prt_pct_mean = mean(i_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_prt_pct_mean = mean(ix_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_prt_pct_mean = mean(pd_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_prt_pct_mean = mean(ha_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_prt_pct_mean = mean(ch_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_prt_pct_mean = mean(ac_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_prt_pct_mean = mean(at_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_prt_pct_mean = mean(co_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_prt_pct_mean = mean(cl_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_prt_pct_mean = mean(em_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_prt_pct_mean = mean(st_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_chi_pct_mean = mean(c_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_chi_pct_mean = mean(o_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_chi_pct_mean = mean(r_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_chi_pct_mean = mean(t_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_chi_pct_mean = mean(m_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_chi_pct_mean = mean(f_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_chi_pct_mean = mean(e_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_chi_pct_mean = mean(i_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_chi_pct_mean = mean(ix_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_chi_pct_mean = mean(pd_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_chi_pct_mean = mean(ha_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_chi_pct_mean = mean(ch_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_chi_pct_mean = mean(ac_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_chi_pct_mean = mean(at_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_chi_pct_mean = mean(co_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_chi_pct_mean = mean(cl_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_chi_pct_mean = mean(em_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_chi_pct_mean = mean(st_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()


# calculating standard errors - Proportion data

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_prt_pct_sd = sd(c_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_prt_pct_sd = sd(o_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_prt_pct_sd = sd(r_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_prt_pct_sd = sd(t_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_prt_pct_sd = sd(m_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_prt_pct_sd = sd(f_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_prt_pct_sd = sd(e_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_prt_pct_sd = sd(i_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_prt_pct_sd = sd(ix_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_prt_pct_sd = sd(pd_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_prt_pct_sd = sd(ha_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_prt_pct_sd = sd(ch_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_prt_pct_sd = sd(ac_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_prt_pct_sd = sd(at_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_prt_pct_sd = sd(co_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_prt_pct_sd = sd(cl_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_prt_pct_sd = sd(em_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_prt_pct_sd = sd(st_prt_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()


qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(c_chi_pct_sd = sd(c_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(o_chi_pct_sd = sd(o_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(r_chi_pct_sd = sd(r_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(t_chi_pct_sd = sd(t_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(m_chi_pct_sd = sd(m_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(f_chi_pct_sd = sd(f_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(e_chi_pct_sd = sd(e_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(i_chi_pct_sd = sd(i_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ix_chi_pct_sd = sd(ix_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(pd_chi_pct_sd = sd(pd_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ha_chi_pct_sd = sd(ha_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ch_chi_pct_sd = sd(ch_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(ac_chi_pct_sd = sd(ac_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(at_chi_pct_sd = sd(at_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(co_chi_pct_sd = sd(co_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(cl_chi_pct_sd = sd(cl_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(em_chi_pct_sd = sd(em_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(st_chi_pct_sd = sd(st_chi_pct, na.rm = TRUE)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(totalq_prt_mean = mean(prt_totalq)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(totalq_chi_mean = mean(chi_totalq)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(totalq_prt_sd = sd(prt_totalq)) %>% 
  dplyr::ungroup()

qp_full_plot <- qp_full_plot %>%
  dplyr::group_by(chinese) %>%
  dplyr::mutate(totalq_chi_sd = sd(chi_totalq)) %>% 
  dplyr::ungroup()

qp_full_plot <- as.data.frame(qp_full_plot)
qp_full_plot <- qp_full_plot[!duplicated(qp_full_plot$chinese),]

qp_full_summary <- dplyr::select(qp_full_plot, c("chinese", "sample_n",
                                                 "o_prt_mean", "c_prt_mean", "r_prt_mean",
                                                 "t_prt_mean", "m_prt_mean", "f_prt_mean",
                                                 "e_prt_mean", "i_prt_mean", 
                                                 "ix_prt_mean", "pd_prt_mean", "ha_prt_mean",
                                                 "ch_prt_mean", "ac_prt_mean", "at_prt_mean",
                                                 "co_prt_mean", "cl_prt_mean", "em_prt_mean",
                                                 "st_prt_mean",
                                                 "o_prt_sd", "c_prt_sd", "r_prt_sd",
                                                 "t_prt_sd", "m_prt_sd", "f_prt_sd",
                                                 "e_prt_sd", "i_prt_sd", 
                                                 "ix_prt_sd", "pd_prt_sd", "ha_prt_sd",
                                                 "ch_prt_sd", "ac_prt_sd", "at_prt_sd",
                                                 "co_prt_sd", "cl_prt_sd", "em_prt_sd",
                                                 "st_prt_sd", 
                                                 "o_chi_mean", "c_chi_mean", "r_chi_mean",
                                                 "t_chi_mean", "m_chi_mean", "f_chi_mean",
                                                 "e_chi_mean", "i_chi_mean", 
                                                 "ix_chi_mean", "pd_chi_mean", "ha_chi_mean",
                                                 "ch_chi_mean", "ac_chi_mean", "at_chi_mean",
                                                 "co_chi_mean", "cl_chi_mean", "em_chi_mean",
                                                 "st_chi_mean",
                                                 "o_chi_sd", "c_chi_sd", "r_chi_sd",
                                                 "t_chi_sd", "m_chi_sd", "f_chi_sd",
                                                 "e_chi_sd", "i_chi_sd", 
                                                 "ix_chi_sd", "pd_chi_sd", "ha_chi_sd",
                                                 "ch_chi_sd", "ac_chi_sd", "at_chi_sd",
                                                 "co_chi_sd", "cl_chi_sd", "em_chi_sd",
                                                 "st_chi_sd",
                                                 "totalq_prt_mean", "totalq_chi_mean",
                                                 "totalq_prt_sd", "totalq_chi_sd",
                                                 "o_prt_pct_mean", "c_prt_pct_mean", "r_prt_pct_mean",
                                                 "t_prt_pct_mean", "m_prt_pct_mean", "f_prt_pct_mean",
                                                 "e_prt_pct_mean", "i_prt_pct_mean", 
                                                 "ix_prt_pct_mean", "pd_prt_pct_mean", "ha_prt_pct_mean",
                                                 "ch_prt_pct_mean", "ac_prt_pct_mean", "at_prt_pct_mean",
                                                 "co_prt_pct_mean", "cl_prt_pct_mean", "em_prt_pct_mean",
                                                 "st_prt_pct_mean",
                                                 "o_prt_pct_sd", "c_prt_pct_sd", "r_prt_pct_sd",
                                                 "t_prt_pct_sd", "m_prt_pct_sd", "f_prt_pct_sd",
                                                 "e_prt_pct_sd", "i_prt_pct_sd", 
                                                 "ix_prt_pct_sd", "pd_prt_pct_sd", "ha_prt_pct_sd",
                                                 "ch_prt_pct_sd", "ac_prt_pct_sd", "at_prt_pct_sd",
                                                 "co_prt_pct_sd", "cl_prt_pct_sd", "em_prt_pct_sd",
                                                 "st_prt_pct_sd", 
                                                 "o_chi_pct_mean", "c_chi_pct_mean", "r_chi_pct_mean",
                                                 "t_chi_pct_mean", "m_chi_pct_mean", "f_chi_pct_mean",
                                                 "e_chi_pct_mean", "i_chi_pct_mean", 
                                                 "ix_chi_pct_mean", "pd_chi_pct_mean", "ha_chi_pct_mean",
                                                 "ch_chi_pct_mean", "ac_chi_pct_mean", "at_chi_pct_mean",
                                                 "co_chi_pct_mean", "cl_chi_pct_mean", "em_chi_pct_mean",
                                                 "st_chi_pct_mean",
                                                 "o_chi_pct_sd", "c_chi_pct_sd", "r_chi_pct_sd",
                                                 "t_chi_pct_sd", "m_chi_pct_sd", "f_chi_pct_sd",
                                                 "e_chi_pct_sd", "i_chi_pct_sd", 
                                                 "ix_chi_pct_sd", "pd_chi_pct_sd", "ha_chi_pct_sd",
                                                 "ch_chi_pct_sd", "ac_chi_pct_sd", "at_chi_pct_sd",
                                                 "co_chi_pct_sd", "cl_chi_pct_sd", "em_chi_pct_sd",
                                                 "st_chi_pct_sd"))

qp_long <- gather(qp_full_summary, q_type, meannum, o_prt_mean:st_chi_pct_sd, factor_key=TRUE)
qp_long[c('form_function_type', 'speaker', "stat")] <- str_split_fixed(qp_long$q_type, '_', 3)

vec_form <- c("c", "o", "r", "t", "m", "f", "e", "i")
vec_function <- c("ix", "pd", "ha", "ch", "ac", "at", "co", "cl", "em", "st", "i")
vec_total <- c("totalq")
vec_pct <- c("pct_mean", "pct_sd")
vec_nonpct <- c("mean", "sd")


# plotting total
total_long <- qp_long[qp_long$form_function_type %in% vec_total, ]
total_long_mean <- total_long[total_long$stat == "mean",]
total_long_sd <- total_long[total_long$stat == "sd",]
total_long_sd$se <- total_long_sd$meannum/sqrt(total_long_sd$sample_n)
total_long_sd$uniqueid <- str_c(total_long_sd$chinese, "_", 
                                total_long_sd$total_function_type,"_",
                                total_long_sd$speaker)

total_long_sd_attach <- dplyr::select(total_long_sd, c("uniqueid", "se"))

total_long_mean$uniqueid <- str_c(total_long_mean$chinese, "_", 
                                  total_long_mean$total_function_type,"_",
                                  total_long_mean$speaker)
total_mean_sd_se_plot <- merge(total_long_mean, total_long_sd_attach, by="uniqueid", all = TRUE)

# this determines how many bars are there (as.factor in ggplot):
total_mean_sd_se_plot$bars <- str_c(total_mean_sd_se_plot$speaker, "_", total_mean_sd_se_plot$chinese)

# relevel the factor based on frequency 

total_mean_sd_se_plot$bars <- as.character(total_mean_sd_se_plot$bars)
total_mean_sd_se_plot$bars[total_mean_sd_se_plot$bars == "prt_0"] <- "EA Parents"
total_mean_sd_se_plot$bars[total_mean_sd_se_plot$bars == "prt_1"] <- "CI Parents"
total_mean_sd_se_plot$bars[total_mean_sd_se_plot$bars == "chi_0"] <- "EA Children"
total_mean_sd_se_plot$bars[total_mean_sd_se_plot$bars == "chi_1"] <- "CI Children"
total_mean_sd_se_plot$bars <- as.factor(total_mean_sd_se_plot$bars)

# reordering the bars
total_mean_sd_se_plot <- total_mean_sd_se_plot %>% 
  mutate(bars = bars %>% 
           fct_relevel("CI Parents", "EA Parents", "CI Children", "EA Children"))
total_mean_sd_se_plot$bars %>% levels()
total_mean_sd_se_plot$color_group <- factor(rep(c("Group1", "Group2"), each = 2, length.out = nrow(total_mean_sd_se_plot)))

# Stacked bar plots: linguistic form proportion score -------------------------------------------------------

formpct_long <- qp_long[qp_long$stat %in% vec_pct, ]
formpct_long[c('pct', "stat")] <- str_split_fixed(formpct_long$stat, '_', 2)
formpct_long <- formpct_long[formpct_long$form_function_type %in% vec_form, ]

formpct_long_mean <- formpct_long[formpct_long$stat == "mean",]
formpct_long_sd <- formpct_long[formpct_long$stat == "sd",]
formpct_long_sd$se <- formpct_long_sd$meannum/sqrt(formpct_long_sd$sample_n)

formpct_long_sd$uniqueid <- str_c(formpct_long_sd$chinese, "_", 
                                  formpct_long_sd$form_function_type,"_",
                                  formpct_long_sd$speaker)

formpct_long_sd_attach <- dplyr::select(formpct_long_sd, c("uniqueid", "se"))

formpct_long_mean$uniqueid <- str_c(formpct_long_mean$chinese, "_", 
                                    formpct_long_mean$form_function_type,"_",
                                    formpct_long_mean$speaker)
formpct_mean_sd_se_plot <- merge(formpct_long_mean, formpct_long_sd_attach, by="uniqueid", all = TRUE)

# this determines how many bars are there (as.factor in ggplot):
formpct_mean_sd_se_plot$bars <- str_c(formpct_mean_sd_se_plot$speaker, "_", formpct_mean_sd_se_plot$chinese)
# need to relevel the factor based on frequency 


formpct_mean_sd_se_plot <- formpct_mean_sd_se_plot %>% 
  mutate(form_function_type = form_function_type %>% 
           fct_relevel("c", "o", "r", "t", "m", "f", "e", "i"))
formpct_mean_sd_se_plot$form_function_type %>% levels()

formpct_mean_sd_se_plot$bars <- as.character(formpct_mean_sd_se_plot$bars)
formpct_mean_sd_se_plot$bars[formpct_mean_sd_se_plot$bars == "prt_0"] <- "EA Parents"
formpct_mean_sd_se_plot$bars[formpct_mean_sd_se_plot$bars == "prt_1"] <- "CI Parents"
formpct_mean_sd_se_plot$bars[formpct_mean_sd_se_plot$bars == "chi_0"] <- "EA Children"
formpct_mean_sd_se_plot$bars[formpct_mean_sd_se_plot$bars == "chi_1"] <- "CI Children"
formpct_mean_sd_se_plot$bars <- as.factor(formpct_mean_sd_se_plot$bars)
formpct_mean_sd_se_plot <- formpct_mean_sd_se_plot %>% 
  mutate(bars = bars %>% 
           fct_relevel("CI Parents", "EA Parents", "CI Children", "EA Children"))
formpct_mean_sd_se_plot$bars %>% levels()

# need to sort the dataset based on factor level for se calculating 
formpct_mean_sd_se_plot <- with(formpct_mean_sd_se_plot, formpct_mean_sd_se_plot[order(bars, form_function_type),])


# calculating error bar position 
# for each bar, the position requires some rolling addition
# for example, "i" = mean, whereas e = M(i) + M(e)

## codes need to run SEQUENTIALLY 
n <- 0
formpct_mean_sd_se_plot$seplus[1+n] <- formpct_mean_sd_se_plot$meannum[1+n] + 
  formpct_mean_sd_se_plot$meannum[2+n] + formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[2+n] <- formpct_mean_sd_se_plot$meannum[2+n] + 
  formpct_mean_sd_se_plot$meannum[3+n] + formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[3+n] <- formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] +
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[4+n] <- formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[5+n] <- formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[6+n] <- formpct_mean_sd_se_plot$meannum[6+n] + 
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[7+n] <- formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[8+n] <-  formpct_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
formpct_mean_sd_se_plot$seplus[1+n]

n <- 8
formpct_mean_sd_se_plot$seplus[1+n] <- formpct_mean_sd_se_plot$meannum[1+n] + 
  formpct_mean_sd_se_plot$meannum[2+n] + formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[2+n] <- formpct_mean_sd_se_plot$meannum[2+n] + 
  formpct_mean_sd_se_plot$meannum[3+n] + formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[3+n] <- formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] +
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[4+n] <- formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[5+n] <- formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[6+n] <- formpct_mean_sd_se_plot$meannum[6+n] + 
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[7+n] <- formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[8+n] <-  formpct_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
formpct_mean_sd_se_plot$seplus[1+n]

n <- 16
formpct_mean_sd_se_plot$seplus[1+n] <- formpct_mean_sd_se_plot$meannum[1+n] + 
  formpct_mean_sd_se_plot$meannum[2+n] + formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[2+n] <- formpct_mean_sd_se_plot$meannum[2+n] + 
  formpct_mean_sd_se_plot$meannum[3+n] + formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[3+n] <- formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] +
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[4+n] <- formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[5+n] <- formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[6+n] <- formpct_mean_sd_se_plot$meannum[6+n] + 
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[7+n] <- formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[8+n] <-  formpct_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
formpct_mean_sd_se_plot$seplus[1+n]

n <- 24
formpct_mean_sd_se_plot$seplus[1+n] <- formpct_mean_sd_se_plot$meannum[1+n] + 
  formpct_mean_sd_se_plot$meannum[2+n] + formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[2+n] <- formpct_mean_sd_se_plot$meannum[2+n] + 
  formpct_mean_sd_se_plot$meannum[3+n] + formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[3+n] <- formpct_mean_sd_se_plot$meannum[3+n] +
  formpct_mean_sd_se_plot$meannum[4+n] + formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] +
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[4+n] <- formpct_mean_sd_se_plot$meannum[4+n] + 
  formpct_mean_sd_se_plot$meannum[5+n] + formpct_mean_sd_se_plot$meannum[6+n] +
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[5+n] <- formpct_mean_sd_se_plot$meannum[5+n] + 
  formpct_mean_sd_se_plot$meannum[6+n] + formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[6+n] <- formpct_mean_sd_se_plot$meannum[6+n] + 
  formpct_mean_sd_se_plot$meannum[7+n] + formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[7+n] <- formpct_mean_sd_se_plot$meannum[7+n] + 
  formpct_mean_sd_se_plot$meannum[8+n] 

formpct_mean_sd_se_plot$seplus[8+n] <-  formpct_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
formpct_mean_sd_se_plot$seplus[1+n]


# plotting 
stackedpct_form <- ggplot(formpct_mean_sd_se_plot, aes(x=as.factor(bars), 
                                                       y= meannum, 
                                                       fill= as.factor(form_function_type))) + 
  geom_bar(colour="black", 
           stat="identity",
           width = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y= " ", x = " ") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values = c("#c3e4fd", "#ffe699", "#a7ba59", "#175a9e", 
                               "#e2cbf7", "#f5c8bf", 
                               "#af6e4e", "#f9f8f3"),
                    name="Linguistic Form",
                    labels=c("Yes-No Questions", "Wh- Questions", 
                             "Rhetorical/Tag Questions", "Thinking Prompts", 
                             "Mirroring Questions", "Fill-in-the-Blanks", 
                             "Empty Prompts", "Incomplete")) + 
  geom_errorbar(aes(ymax = seplus + se, ymin = seplus - se), position = "identity", width = 0.3)




# Stacked bar plots: social-pragmatic function proportion score ---------------------------------------------------

functionpct_long <- qp_long[qp_long$stat %in% vec_pct, ]
functionpct_long[c('pct', "stat")] <- str_split_fixed(functionpct_long$stat, '_', 2)
functionpct_long <- functionpct_long[functionpct_long$form_function_type %in% vec_function, ]

functionpct_long_mean <- functionpct_long[functionpct_long$stat == "mean",]
functionpct_long_sd <- functionpct_long[functionpct_long$stat == "sd",]
functionpct_long_sd$se <- functionpct_long_sd$meannum/sqrt(functionpct_long_sd$sample_n)

functionpct_long_sd$uniqueid <- str_c(functionpct_long_sd$chinese, "_", 
                                      functionpct_long_sd$form_function_type,"_",
                                      functionpct_long_sd$speaker)

functionpct_long_sd_attach <- dplyr::select(functionpct_long_sd, c("uniqueid", "se"))

functionpct_long_mean$uniqueid <- str_c(functionpct_long_mean$chinese, "_", 
                                        functionpct_long_mean$form_function_type,"_",
                                        functionpct_long_mean$speaker)
functionpct_mean_sd_se_plot <- merge(functionpct_long_mean, functionpct_long_sd_attach, by="uniqueid", all = TRUE)

# this determines how many bars are there (as.factor in ggplot):
functionpct_mean_sd_se_plot$bars <- str_c(functionpct_mean_sd_se_plot$speaker, "_", functionpct_mean_sd_se_plot$chinese)
# need to relevel the factor based on frequency 

functionpct_mean_sd_se_plot <- functionpct_mean_sd_se_plot %>% 
  mutate(form_function_type = form_function_type %>% 
           fct_relevel("ix", "pd", "ha", "ch", "ac", "at", "co", "cl", "em", "st", "i"))
functionpct_mean_sd_se_plot$form_function_type %>% levels()

functionpct_mean_sd_se_plot$bars <- as.character(functionpct_mean_sd_se_plot$bars)
functionpct_mean_sd_se_plot$bars[functionpct_mean_sd_se_plot$bars == "prt_0"] <- "EA Parents"
functionpct_mean_sd_se_plot$bars[functionpct_mean_sd_se_plot$bars == "prt_1"] <- "CI Parents"
functionpct_mean_sd_se_plot$bars[functionpct_mean_sd_se_plot$bars == "chi_0"] <- "EA Children"
functionpct_mean_sd_se_plot$bars[functionpct_mean_sd_se_plot$bars == "chi_1"] <- "CI Children"
functionpct_mean_sd_se_plot$bars <- as.factor(functionpct_mean_sd_se_plot$bars)
functionpct_mean_sd_se_plot <- functionpct_mean_sd_se_plot %>% 
  mutate(bars = bars %>% 
           fct_relevel("CI Parents", "EA Parents", "CI Children", "EA Children"))
functionpct_mean_sd_se_plot$bars %>% levels()

# need to sort the dataset based on factor level for se calculating 
functionpct_mean_sd_se_plot <- with(functionpct_mean_sd_se_plot, functionpct_mean_sd_se_plot[order(bars, form_function_type),])


# calculating error bar position 
# for each bar, the position requires some rolling addition
# for example, "i" = mean, whereas e = M(i) + M(e)

## codes need to run SEQUENTIALLY 
n <- 0
functionpct_mean_sd_se_plot$seplus[1+n] <- functionpct_mean_sd_se_plot$meannum[1+n] + 
  functionpct_mean_sd_se_plot$meannum[2+n] + functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[2+n] <- functionpct_mean_sd_se_plot$meannum[2+n] + 
  functionpct_mean_sd_se_plot$meannum[3+n] + functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[3+n] <- functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] +
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[4+n] <- functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[5+n] <- functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[6+n] <- functionpct_mean_sd_se_plot$meannum[6+n] + 
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[7+n] <- functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[8+n] <-  functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[9+n] <-  functionpct_mean_sd_se_plot$meannum[9+n] + 
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[10+n] <-  functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[11+n] <-  functionpct_mean_sd_se_plot$meannum[11+n]

# sanity check: 
functionpct_mean_sd_se_plot$seplus[1+n]

n <- 11
functionpct_mean_sd_se_plot$seplus[1+n] <- functionpct_mean_sd_se_plot$meannum[1+n] + 
  functionpct_mean_sd_se_plot$meannum[2+n] + functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[2+n] <- functionpct_mean_sd_se_plot$meannum[2+n] + 
  functionpct_mean_sd_se_plot$meannum[3+n] + functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[3+n] <- functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] +
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[4+n] <- functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[5+n] <- functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[6+n] <- functionpct_mean_sd_se_plot$meannum[6+n] + 
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[7+n] <- functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[8+n] <-  functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[9+n] <-  functionpct_mean_sd_se_plot$meannum[9+n] + 
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[10+n] <-  functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[11+n] <-  functionpct_mean_sd_se_plot$meannum[11+n]

# sanity check: 
functionpct_mean_sd_se_plot$seplus[1+n]

n <- 22
functionpct_mean_sd_se_plot$seplus[1+n] <- functionpct_mean_sd_se_plot$meannum[1+n] + 
  functionpct_mean_sd_se_plot$meannum[2+n] + functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[2+n] <- functionpct_mean_sd_se_plot$meannum[2+n] + 
  functionpct_mean_sd_se_plot$meannum[3+n] + functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[3+n] <- functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] +
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[4+n] <- functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[5+n] <- functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[6+n] <- functionpct_mean_sd_se_plot$meannum[6+n] + 
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[7+n] <- functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[8+n] <-  functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[9+n] <-  functionpct_mean_sd_se_plot$meannum[9+n] + 
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[10+n] <-  functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[11+n] <-  functionpct_mean_sd_se_plot$meannum[11+n]

# sanity check: 
functionpct_mean_sd_se_plot$seplus[1+n]

n <- 33
functionpct_mean_sd_se_plot$seplus[1+n] <- functionpct_mean_sd_se_plot$meannum[1+n] + 
  functionpct_mean_sd_se_plot$meannum[2+n] + functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[2+n] <- functionpct_mean_sd_se_plot$meannum[2+n] + 
  functionpct_mean_sd_se_plot$meannum[3+n] + functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n] 

functionpct_mean_sd_se_plot$seplus[3+n] <- functionpct_mean_sd_se_plot$meannum[3+n] +
  functionpct_mean_sd_se_plot$meannum[4+n] + functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] +
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[4+n] <- functionpct_mean_sd_se_plot$meannum[4+n] + 
  functionpct_mean_sd_se_plot$meannum[5+n] + functionpct_mean_sd_se_plot$meannum[6+n] +
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[5+n] <- functionpct_mean_sd_se_plot$meannum[5+n] + 
  functionpct_mean_sd_se_plot$meannum[6+n] + functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[6+n] <- functionpct_mean_sd_se_plot$meannum[6+n] + 
  functionpct_mean_sd_se_plot$meannum[7+n] + functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[7+n] <- functionpct_mean_sd_se_plot$meannum[7+n] + 
  functionpct_mean_sd_se_plot$meannum[8+n] + functionpct_mean_sd_se_plot$meannum[9+n] +
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[8+n] <-  functionpct_mean_sd_se_plot$meannum[8+n] +
  functionpct_mean_sd_se_plot$meannum[9+n] + functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[9+n] <-  functionpct_mean_sd_se_plot$meannum[9+n] + 
  functionpct_mean_sd_se_plot$meannum[10+n] + functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[10+n] <-  functionpct_mean_sd_se_plot$meannum[10+n] + 
  functionpct_mean_sd_se_plot$meannum[11+n]

functionpct_mean_sd_se_plot$seplus[11+n] <-  functionpct_mean_sd_se_plot$meannum[11+n]

# sanity check: 
functionpct_mean_sd_se_plot$seplus[1+n]

stackedpct_function <- ggplot(functionpct_mean_sd_se_plot, aes(x=as.factor(bars), 
                                                               y= meannum, 
                                                               fill= as.factor(form_function_type))) + 
  geom_bar(colour="black", 
           stat="identity",
           width = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y= " ", x = " ") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values = c("#1790ab", "#fabe0e", "#ffd9d5",
                               "#8ab661", 
                               "#f28775", "#b6c7dd", 
                               "#cba2cb", "#6b4e55",   
                               "#b9bbb6", "#2f5d8c", "#f9f8f3"),
                    name="Social-Pragmatic Function",
                    labels=c("Information Seeking", "Pedagogical", 
                             "Harmonization", "Choice Seeking", 
                             "Action Seeking", "Attention Seeking", 
                             "Conversation Prompting", "Clarification Seeking",
                             "Emoting Aloud", "Status Checking", "Incomplete")) + 
  geom_errorbar(aes(ymax = seplus + se, ymin = seplus - se), position = "identity", width = 0.3)

grid.arrange(stackedpct_form, stackedpct_function, nrow = 2, ncol = 1)

# ===========================================================================
# 5. SUPPLEMENTARY
# ===========================================================================

# --- 5.1 SI Section 2: toy use ---------------------------------------------

# SI Section 2: Toy use ---------------------------------------------------

toys <- c("lego", "doctor", "fishing")

qp_full_y12 <- qp_full_y12 %>%
  mutate(
    # group from leading digit of id: 1 = CI, 2 = EA
    group = factor(ifelse(substr(as.character(id), 1, 1) == "1", "CI", "EA"),
                   levels = c("CI", "EA")),
    # number of distinct toys each dyad engaged (1-3)
    n_toys = rowSums(across(all_of(toys)))
  )

cat("Group sizes:\n"); print(table(qp_full_y12$group))
cat("\n")

# Descriptives: % of each group who played each toy 
cat("Proportion of dyads who played with each toy, by group:\n")
desc <- qp_full_y12 %>%
  group_by(group) %>%
  summarise(across(all_of(toys), ~ round(mean(.) * 100, 1)), .groups = "drop")
print(desc)
cat("\n")

# Per-toy group comparison 
# Binary outcome (played vs not) x group (CI vs EA).
# Fisher's exact test, chi-square (with Yates correction) is reported alongside for reference.
# Three tests -> Bonferroni-corrected alpha = .05 / 3 = .0167.

cat("Per-toy group comparisons (rows = group, cols = 0/1 toy use):\n\n")
toy_p <- setNames(numeric(length(toys)), toys)

for (t in toys) {
  tab <- table(qp_full_y12$group, qp_full_y12[[t]])
  fish <- fisher.test(tab)
  chi  <- suppressWarnings(chisq.test(tab))  # warning if expected counts small
  toy_p[t] <- fish$p.value
  
  cat(sprintf("Toy: %s\n", t))
  print(tab)
  cat(sprintf("  Fisher exact:  OR = %.3f, p = %.4f\n",
              unname(fish$estimate), fish$p.value))
  cat(sprintf("  Chi-square:    X2(%d) = %.3f, p = %.4f (min expected = %.2f)\n\n",
              chi$parameter, chi$statistic, chi$p.value, min(chi$expected)))
}

# Multiple-comparison correction across the three toys
toy_p_bonf <- p.adjust(toy_p, method = "bonferroni")
cat("P-values across the three toys (Fisher), raw and Bonferroni-adjusted:\n")
print(data.frame(toy = toys,
                 p_raw  = round(toy_p, 4),
                 p_bonf = round(toy_p_bonf, 4),
                 sig_bonf = toy_p_bonf < .05))
cat("\n(Bonferroni-corrected alpha for 3 tests = .0167)\n\n")

# Number of toys used, by group 
# Did one group engage more toy sets (i.e., switch among sets more)?
# n_toys is an ordinal count (1-3): Mann-Whitney U as the primary test,
# with a 3x2 chi-square on the group x n_toys table for reference.

cat("Number of toys used per dyad, by group:\n")
print(table(qp_full_y12$group, qp_full_y12$n_toys))
cat("\n")

mw <- wilcox.test(n_toys ~ group, data = qp_full_y12, exact = FALSE)
cat(sprintf("Mann-Whitney U (n_toys by group): W = %.1f, p = %.4f\n",
            mw$statistic, mw$p.value))
cat(sprintf("  Median n_toys: CI = %g, EA = %g | Mean: CI = %.2f, EA = %.2f\n\n",
            median(qp_full_y12$n_toys[qp_full_y12$group == "CI"]),
            median(qp_full_y12$n_toys[qp_full_y12$group == "EA"]),
            mean(qp_full_y12$n_toys[qp_full_y12$group == "CI"]),
            mean(qp_full_y12$n_toys[qp_full_y12$group == "EA"])))

ntoy_tab <- table(qp_full_y12$group, qp_full_y12$n_toys)
ntoy_chi <- suppressWarnings(chisq.test(ntoy_tab))
cat(sprintf("Chi-square (group x n_toys): X2(%d) = %.3f, p = %.4f (min expected = %.2f)\n",
            ntoy_chi$parameter, ntoy_chi$statistic, ntoy_chi$p.value,
            min(ntoy_chi$expected)))

# the most prominent play activity chosen by the CI and the EA fam --------

summary(qp_full_y12_ea$lego)
summary(qp_full_y12_ea$doctor)
summary(qp_full_y12_ea$fishing)

summary(qp_full_y12_ca$lego)
summary(qp_full_y12_ca$doctor)
summary(qp_full_y12_ca$fishing)

# --- 5.2 SI Section 2: robustness checks -----------------------------------

# SI Section 2: robustness check of group level differences ---------------

m1 <- lm(c_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(o_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(r_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(t_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(m_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(f_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(e_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(i_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)

m1 <- lm(o_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(c_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(t_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(e_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(r_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(f_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(m_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(i_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(co_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(at_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(em_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(st_prt_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(at_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(co_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(em_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)
m1 <- lm(st_chi_pct ~ chinese + momed_years + childsex + age_months + fishing, data = qp_full_y12)
summary(m1)

outcomes <- c(
  # parent forms
  "c_prt_pct","o_prt_pct","r_prt_pct","t_prt_pct","m_prt_pct","f_prt_pct","e_prt_pct","i_prt_pct",
  # child forms
  "o_chi_pct","c_chi_pct","t_chi_pct","e_chi_pct","r_chi_pct","f_chi_pct","m_chi_pct","i_chi_pct",
  # parent functions
  "ix_prt_pct","pd_prt_pct","ha_prt_pct","ch_prt_pct","ac_prt_pct","co_prt_pct","at_prt_pct",
  "cl_prt_pct","em_prt_pct","st_prt_pct",
  # child functions
  "ix_chi_pct","pd_chi_pct","ha_chi_pct","ch_chi_pct","ac_chi_pct","at_chi_pct","co_chi_pct",
  "cl_chi_pct","em_chi_pct","st_chi_pct"
)

covariates <- "chinese + momed_years + childsex + age_months + fishing"

# Run every model and collect full coefficient tables
all_results <- do.call(rbind, lapply(outcomes, function(dv) {
  f  <- as.formula(paste(dv, "~", covariates))
  m  <- lm(f, data = qp_full_y12)
  td <- broom::tidy(m)                 # term, estimate, std.error, statistic, p.value
  gl <- broom::glance(m)               # r.squared, adj.r.squared, etc.
  td$outcome     <- dv
  td$n           <- nobs(m)
  td$adj_r2      <- gl$adj.r.squared
  td
}))

all_results <- all_results[, c("outcome","term","estimate","std.error",
                               "statistic","p.value","adj_r2","n")]

# Export
write.csv(all_results, "toy_robustness_all_models.csv", row.names = FALSE)


# SI Section 2: robustness check of group level differences, without child sex controlled ---------------

m1 <- lm(c_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(o_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(o_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(c_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(t_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(e_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(r_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(f_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(m_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(i_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(co_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_prt_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)

m1 <- lm(ix_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(pd_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ha_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ch_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(ac_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(at_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
lm.beta(m1)
m1 <- lm(co_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(cl_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(em_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)
m1 <- lm(st_chi_pct ~ chinese + momed_years + age_months, data = qp_full_y12)
summary(m1)

outcomes <- c(
  # parent forms
  "c_prt_pct","o_prt_pct","r_prt_pct","t_prt_pct","m_prt_pct","f_prt_pct","e_prt_pct","i_prt_pct",
  # child forms
  "o_chi_pct","c_chi_pct","t_chi_pct","e_chi_pct","r_chi_pct","f_chi_pct","m_chi_pct","i_chi_pct",
  # parent functions
  "ix_prt_pct","pd_prt_pct","ha_prt_pct","ch_prt_pct","ac_prt_pct","co_prt_pct","at_prt_pct",
  "cl_prt_pct","em_prt_pct","st_prt_pct",
  # child functions
  "ix_chi_pct","pd_chi_pct","ha_chi_pct","ch_chi_pct","ac_chi_pct","at_chi_pct","co_chi_pct",
  "cl_chi_pct","em_chi_pct","st_chi_pct"
)

covariates <- "chinese + momed_years + age_months"

coef_list <- list()
fit_list  <- list()

for (dv in outcomes) {
  f <- as.formula(paste(dv, "~", covariates))
  m <- lm(f, data = qp_full_y12)
  
  # coefficient table
  td <- broom::tidy(m)                       # term, estimate, std.error, statistic, p.value
  
  # standardized betas (aligned to the same term order)
  sb <- tryCatch({
    b <- lm.beta::lm.beta(m)$standardized.coefficients
    b[match(td$term, names(b))]
  }, error = function(e) rep(NA_real_, nrow(td)))
  td$std_beta <- as.numeric(sb)
  
  td$outcome <- dv
  td <- td[, c("outcome", "term", "estimate", "std.error", "statistic", "p.value", "std_beta")]
  coef_list[[dv]] <- td
  
  # model-fit table
  gl <- broom::glance(m)                      # r.squared, adj.r.squared, statistic, df, p.value, ...
  gl$outcome <- dv
  gl$n <- nobs(m)
  fit_list[[dv]] <- gl[, c("outcome","r.squared","adj.r.squared","statistic",
                           "df","df.residual","p.value","n")]
}

coefficients <- dplyr::bind_rows(coef_list)
model_fit    <- dplyr::bind_rows(fit_list)

coefficients <- coefficients %>%
  mutate(across(c(estimate, std.error, statistic, p.value, std_beta), ~ round(., 5)))
model_fit <- model_fit %>%
  mutate(across(c(r.squared, adj.r.squared, statistic, p.value), ~ round(., 5)))

writexl::write_xlsx(
  list(coefficients = coefficients,
       model_fit    = model_fit),
  path = "model_results.xlsx"
)

# --- 5.3 Figure S1: total question frequency -------------------------------

barplot_freq <- ggplot(total_mean_sd_se_plot, aes(x = as.factor(bars), y = meannum, fill = color_group)) +  
  geom_bar(colour = "black", stat = "identity", width = 0.5) +  
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = 0, add = 0)) +  
  scale_fill_manual(values = c("Group1" = "#4D4D4D", "Group2" = "#D9D9D9")) +  
  labs(y = "Frequency", x = "Group") +  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", family = "sans"),  
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  
        panel.background = element_blank(),  
        axis.line = element_line(colour = "black")) +  
  geom_errorbar(aes(ymax = meannum + se, ymin = meannum - se), position = "identity", width = 0.3)

# --- 5.4 Figure S2: distribution of question counts ------------------------

# Figure S2. Distribution of caregiver and child questions by ethn --------


CI_FILL <- "#8F5774"
EA_FILL <- "#036264"

theme_qp <- theme_minimal(base_family = "sans") +
  theme(
    text         = element_text(size = 14),
    axis.title   = element_text(color = "black", size = 14),
    axis.text    = element_text(color = "black", size = 12),
    plot.margin  = margin(10, 14, 10, 10),
    panel.grid   = element_blank(),
    panel.background = element_blank(),
    axis.line    = element_line(color = "black", linewidth = 0.5),
    axis.ticks   = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(3, "pt")
  )

qp_hist <- function(data, var, fill, xlab,
                    binwidth, xmax, xby, ymax, yby) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(binwidth = binwidth, boundary = 0,
                   fill = fill, color = "white", linewidth = 0.6) +
    labs(x = xlab, y = "Number of Families") +
    scale_x_continuous(breaks = seq(0, xmax, by = xby),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(breaks = seq(0, ymax, by = yby),
                       expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax)) +
    theme_qp
}

# caregiver / parent panels 
hist_ci_prt <- qp_hist(qp_full_y12_ca, "prt_totalq", CI_FILL,
                       "Total Caregiver Questions (CI)",
                       binwidth = 5, xmax = 150, xby = 30, ymax = 12, yby = 3)

hist_ea_prt <- qp_hist(qp_full_y12_ea, "prt_totalq", EA_FILL,
                       "Total Parent Questions (EA)",
                       binwidth = 5, xmax = 150, xby = 30, ymax = 12, yby = 3)

# child panels 
hist_ci_chi <- qp_hist(qp_full_y12_ca, "chi_totalq", CI_FILL,
                       "Total Child Questions (CI)",
                       binwidth = 2, xmax = 40, xby = 10, ymax = 15, yby = 3)

hist_ea_chi <- qp_hist(qp_full_y12_ea, "chi_totalq", EA_FILL,
                       "Total Child Questions (EA)",
                       binwidth = 2, xmax = 40, xby = 10, ymax = 15, yby = 3)

# assemble: adults on the top row, children on the bottom
fig <- arrangeGrob(hist_ci_prt, hist_ea_prt,
                   hist_ci_chi, hist_ea_chi, nrow = 2, ncol = 2)
grid::grid.newpage(); grid::grid.draw(fig)

# check the y-limits actually clear the data 
bin_max <- function(v, bw) max(table(cut(v, breaks = seq(0, max(v) + bw, by = bw))))
cat("tallest bin - CI caregivers:", bin_max(qp_full_y12_ca$prt_totalq, 5), "\n")
cat("tallest bin - EA parents:   ", bin_max(qp_full_y12_ea$prt_totalq, 5), "\n")
cat("tallest bin - CI children:  ", bin_max(qp_full_y12_ca$chi_totalq, 2), "\n")
cat("tallest bin - EA children:  ", bin_max(qp_full_y12_ea$chi_totalq, 2), "\n")

# --- 5.5 Figure S3: stacked frequency plots --------------------------------

# Stacked bar plots: linguistic form frequency-----------------------------------------------------------

form_long <- qp_long[qp_long$form_function_type %in% vec_form, ]
form_long <- form_long[form_long$stat %in% vec_nonpct, ]

form_long_mean <- form_long[form_long$stat == "mean",]
form_long_sd <- form_long[form_long$stat == "sd",]
form_long_sd$se <- form_long_sd$meannum/sqrt(form_long_sd$sample_n)

form_long_sd$uniqueid <- str_c(form_long_sd$chinese, "_", 
                               form_long_sd$form_function_type,"_",
                               form_long_sd$speaker)
form_long_sd_attach <- dplyr::select(form_long_sd, c("uniqueid", "se"))

form_long_mean$uniqueid <- str_c(form_long_mean$chinese, "_", 
                                 form_long_mean$form_function_type,"_",
                                 form_long_mean$speaker)
form_mean_sd_se_plot <- merge(form_long_mean, form_long_sd_attach, by="uniqueid", all = TRUE)

# this determines how many bars are there (as.factor in ggplot):
form_mean_sd_se_plot$bars <- str_c(form_mean_sd_se_plot$speaker, "_", form_mean_sd_se_plot$chinese)

# Re-level the factor based on frequency 

form_mean_sd_se_plot <- form_mean_sd_se_plot %>% 
  mutate(form_function_type = form_function_type %>% 
           fct_relevel("c", "o", "r", "t", "m", "f", "e", "i"))
form_mean_sd_se_plot$form_function_type %>% levels()

form_mean_sd_se_plot$bars <- as.character(form_mean_sd_se_plot$bars)
form_mean_sd_se_plot$bars[form_mean_sd_se_plot$bars == "prt_0"] <- "EA Parents"
form_mean_sd_se_plot$bars[form_mean_sd_se_plot$bars == "prt_1"] <- "CI Parents"
form_mean_sd_se_plot$bars[form_mean_sd_se_plot$bars == "chi_0"] <- "EA Children"
form_mean_sd_se_plot$bars[form_mean_sd_se_plot$bars == "chi_1"] <- "CI Children"
form_mean_sd_se_plot$bars <- as.factor(form_mean_sd_se_plot$bars)
form_mean_sd_se_plot <- form_mean_sd_se_plot %>% 
  mutate(bars = bars %>% 
           fct_relevel("CI Parents", "EA Parents", "CI Children", "EA Children"))
form_mean_sd_se_plot$bars %>% levels()

# Sort the dataset based on factor level for se calculating 
form_mean_sd_se_plot <- with(form_mean_sd_se_plot, form_mean_sd_se_plot[order(bars, form_function_type),])


# calculating error bar position 
# for each bar, the position requires some rolling addition
# for example, "i" = mean, whereas e = M(i) + M(e)

## codes need to run SEQUENTIALLY 
n <- 0
form_mean_sd_se_plot$seplus[1+n] <- form_mean_sd_se_plot$meannum[1+n] + 
  form_mean_sd_se_plot$meannum[2+n] + form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[2+n] <- form_mean_sd_se_plot$meannum[2+n] + 
  form_mean_sd_se_plot$meannum[3+n] + form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[3+n] <- form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] +
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[4+n] <- form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[5+n] <- form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[6+n] <- form_mean_sd_se_plot$meannum[6+n] + 
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[7+n] <- form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[8+n] <-  form_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
mean(qp_full_y12_ea$prt_totalq)
form_mean_sd_se_plot$seplus[1+n]

n <- 8
form_mean_sd_se_plot$seplus[1+n] <- form_mean_sd_se_plot$meannum[1+n] + 
  form_mean_sd_se_plot$meannum[2+n] + form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[2+n] <- form_mean_sd_se_plot$meannum[2+n] + 
  form_mean_sd_se_plot$meannum[3+n] + form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[3+n] <- form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] +
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[4+n] <- form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[5+n] <- form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[6+n] <- form_mean_sd_se_plot$meannum[6+n] + 
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[7+n] <- form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[8+n] <-  form_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
mean(qp_full_y12_ca$prt_totalq)
form_mean_sd_se_plot$seplus[1+n]

n <- 16
form_mean_sd_se_plot$seplus[1+n] <- form_mean_sd_se_plot$meannum[1+n] + 
  form_mean_sd_se_plot$meannum[2+n] + form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[2+n] <- form_mean_sd_se_plot$meannum[2+n] + 
  form_mean_sd_se_plot$meannum[3+n] + form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[3+n] <- form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] +
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[4+n] <- form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[5+n] <- form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[6+n] <- form_mean_sd_se_plot$meannum[6+n] + 
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[7+n] <- form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[8+n] <-  form_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
mean(qp_full_y12_ea$chi_totalq)
form_mean_sd_se_plot$seplus[1+n]

n <- 24
form_mean_sd_se_plot$seplus[1+n] <- form_mean_sd_se_plot$meannum[1+n] + 
  form_mean_sd_se_plot$meannum[2+n] + form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[2+n] <- form_mean_sd_se_plot$meannum[2+n] + 
  form_mean_sd_se_plot$meannum[3+n] + form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[3+n] <- form_mean_sd_se_plot$meannum[3+n] +
  form_mean_sd_se_plot$meannum[4+n] + form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] +
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[4+n] <- form_mean_sd_se_plot$meannum[4+n] + 
  form_mean_sd_se_plot$meannum[5+n] + form_mean_sd_se_plot$meannum[6+n] +
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[5+n] <- form_mean_sd_se_plot$meannum[5+n] + 
  form_mean_sd_se_plot$meannum[6+n] + form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[6+n] <- form_mean_sd_se_plot$meannum[6+n] + 
  form_mean_sd_se_plot$meannum[7+n] + form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[7+n] <- form_mean_sd_se_plot$meannum[7+n] + 
  form_mean_sd_se_plot$meannum[8+n] 

form_mean_sd_se_plot$seplus[8+n] <-  form_mean_sd_se_plot$meannum[8+n] 

# sanity check: 
mean(qp_full_y12_ca$chi_totalq)
form_mean_sd_se_plot$seplus[1+n]


# plotting 
form_mean_sd_se_plot$bars <- factor(form_mean_sd_se_plot$bars, 
                                    levels = c("CI Parents", "EA Parents", "CI Children", "EA Children")) 
stacked_form <- ggplot(form_mean_sd_se_plot, aes(x=as.factor(bars), 
                                                 y= meannum, 
                                                 fill= as.factor(form_function_type))) + 
  geom_bar(colour="black", 
           stat="identity",
           width = 0.5) +
  scale_y_continuous(limits=c(0, 80), expand = expansion(mult = 0, add = 0)) +
  labs(y= " ", x = " ") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values = c("#c3e4fd", "#ffe699", "#a7ba59", "#175a9e", 
                               "#e2cbf7", "#f5c8bf", 
                               "#af6e4e", "#f9f8f3"),
                    name="Linguistic Form",
                    labels=c("Yes-No Questions", "Wh- Questions", 
                             "Rhetorical Questions", "Thinking Prompts", 
                             "Mirroring Questions", "Fill-in-the-Blanks", 
                             "Empty Prompts", "Incomplete")) + 
  geom_errorbar(aes(ymax = seplus + se, ymin = seplus - se), position = "identity", width = 0.3)


# Stacked bar plots: social-pragmatic function frequency-----------------------------------------------------------

function_long <- qp_long[qp_long$form_function_type %in% vec_function, ]

function_long_mean <- function_long[function_long$stat == "mean",]
function_long_sd <- function_long[function_long$stat == "sd",]
function_long_sd$se <- function_long_sd$meannum/sqrt(function_long_sd$sample_n)

function_long_sd$uniqueid <- str_c(function_long_sd$chinese, "_", 
                                   function_long_sd$form_function_type,"_",
                                   function_long_sd$speaker)
function_long_sd_attach <- dplyr::select(function_long_sd, c("uniqueid", "se"))

function_long_mean$uniqueid <- str_c(function_long_mean$chinese, "_", 
                                     function_long_mean$form_function_type,"_",
                                     function_long_mean$speaker)
function_mean_sd_se_plot <- merge(function_long_mean, function_long_sd_attach, by="uniqueid", all = TRUE)

# this determines how many bars are there (as.factor in ggplot):
function_mean_sd_se_plot$bars <- str_c(function_mean_sd_se_plot$speaker, "_", function_mean_sd_se_plot$chinese)
# need to relevel the factor based on frequency 

function_mean_sd_se_plot <- function_mean_sd_se_plot %>% 
  mutate(form_function_type = form_function_type %>% 
           fct_relevel("ix", "pd", "ha", "ch", "ac", "at", "co", "cl", "em", "st", "i"))
function_mean_sd_se_plot$form_function_type %>% levels()

function_mean_sd_se_plot$bars <- as.character(function_mean_sd_se_plot$bars)
function_mean_sd_se_plot$bars[function_mean_sd_se_plot$bars == "prt_0"] <- "EA Parents"
function_mean_sd_se_plot$bars[function_mean_sd_se_plot$bars == "prt_1"] <- "CI Parents"
function_mean_sd_se_plot$bars[function_mean_sd_se_plot$bars == "chi_0"] <- "EA Children"
function_mean_sd_se_plot$bars[function_mean_sd_se_plot$bars == "chi_1"] <- "CI Children"
function_mean_sd_se_plot$bars <- as.factor(function_mean_sd_se_plot$bars)
function_mean_sd_se_plot <- function_mean_sd_se_plot %>% 
  mutate(bars = bars %>% 
           fct_relevel("CI Parents", "EA Parents", "CI Children", "EA Children"))
function_mean_sd_se_plot$bars %>% levels()

# need to sort the dataset based on factor level for se calculating 
function_mean_sd_se_plot <- with(function_mean_sd_se_plot, function_mean_sd_se_plot[order(bars, form_function_type),])


# calculating error bar position 
# for each bar, the position requires some rolling addition
# for example, "i" = mean, whereas e = M(i) + M(e)

## codes need to run SEQUENTIALLY 
n <- 0
function_mean_sd_se_plot$seplus[1+n] <- function_mean_sd_se_plot$meannum[1+n] + 
  function_mean_sd_se_plot$meannum[2+n] + function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[2+n] <- function_mean_sd_se_plot$meannum[2+n] + 
  function_mean_sd_se_plot$meannum[3+n] + function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[3+n] <- function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] +
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[4+n] <- function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[5+n] <- function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[6+n] <- function_mean_sd_se_plot$meannum[6+n] + 
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[7+n] <- function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[8+n] <-  function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[9+n] <-  function_mean_sd_se_plot$meannum[9+n] + 
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[10+n] <-  function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[11+n] <-  function_mean_sd_se_plot$meannum[11+n]

# sanity check: 
mean(qp_full_y12_ea$prt_totalq)
function_mean_sd_se_plot$seplus[1+n]

n <- 11
function_mean_sd_se_plot$seplus[1+n] <- function_mean_sd_se_plot$meannum[1+n] + 
  function_mean_sd_se_plot$meannum[2+n] + function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[2+n] <- function_mean_sd_se_plot$meannum[2+n] + 
  function_mean_sd_se_plot$meannum[3+n] + function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[3+n] <- function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] +
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[4+n] <- function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[5+n] <- function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[6+n] <- function_mean_sd_se_plot$meannum[6+n] + 
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[7+n] <- function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[8+n] <-  function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[9+n] <-  function_mean_sd_se_plot$meannum[9+n] + 
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[10+n] <-  function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[11+n] <-  function_mean_sd_se_plot$meannum[11+n]

# sanity check: 
mean(qp_full_y12_ca$prt_totalq)
function_mean_sd_se_plot$seplus[1+n]

n <- 22
function_mean_sd_se_plot$seplus[1+n] <- function_mean_sd_se_plot$meannum[1+n] + 
  function_mean_sd_se_plot$meannum[2+n] + function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[2+n] <- function_mean_sd_se_plot$meannum[2+n] + 
  function_mean_sd_se_plot$meannum[3+n] + function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[3+n] <- function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] +
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[4+n] <- function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[5+n] <- function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[6+n] <- function_mean_sd_se_plot$meannum[6+n] + 
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[7+n] <- function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[8+n] <-  function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[9+n] <-  function_mean_sd_se_plot$meannum[9+n] + 
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[10+n] <-  function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[11+n] <-  function_mean_sd_se_plot$meannum[11+n]

# sanity check: 
mean(qp_full_y12_ea$chi_totalq)
function_mean_sd_se_plot$seplus[1+n]

n <- 33
function_mean_sd_se_plot$seplus[1+n] <- function_mean_sd_se_plot$meannum[1+n] + 
  function_mean_sd_se_plot$meannum[2+n] + function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[2+n] <- function_mean_sd_se_plot$meannum[2+n] + 
  function_mean_sd_se_plot$meannum[3+n] + function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n] 

function_mean_sd_se_plot$seplus[3+n] <- function_mean_sd_se_plot$meannum[3+n] +
  function_mean_sd_se_plot$meannum[4+n] + function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] +
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[4+n] <- function_mean_sd_se_plot$meannum[4+n] + 
  function_mean_sd_se_plot$meannum[5+n] + function_mean_sd_se_plot$meannum[6+n] +
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[5+n] <- function_mean_sd_se_plot$meannum[5+n] + 
  function_mean_sd_se_plot$meannum[6+n] + function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[6+n] <- function_mean_sd_se_plot$meannum[6+n] + 
  function_mean_sd_se_plot$meannum[7+n] + function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[7+n] <- function_mean_sd_se_plot$meannum[7+n] + 
  function_mean_sd_se_plot$meannum[8+n] + function_mean_sd_se_plot$meannum[9+n] +
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[8+n] <-  function_mean_sd_se_plot$meannum[8+n] +
  function_mean_sd_se_plot$meannum[9+n] + function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[9+n] <-  function_mean_sd_se_plot$meannum[9+n] + 
  function_mean_sd_se_plot$meannum[10+n] + function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[10+n] <-  function_mean_sd_se_plot$meannum[10+n] + 
  function_mean_sd_se_plot$meannum[11+n]

function_mean_sd_se_plot$seplus[11+n] <-  function_mean_sd_se_plot$meannum[11+n]

# sanity check: 
mean(qp_full_y12_ca$chi_totalq)
function_mean_sd_se_plot$seplus[1+n]


# plotting 

function_mean_sd_se_plot$bars <- factor(function_mean_sd_se_plot$bars, 
                                        levels = c("CI Parents", "EA Parents", "CI Children", "EA Children"))  # Customize order

stacked_function <- ggplot(function_mean_sd_se_plot, aes(x=as.factor(bars), 
                                                         y= meannum, 
                                                         fill= as.factor(form_function_type))) + 
  geom_bar(colour="black", 
           stat="identity",
           width = 0.5) +
  scale_y_continuous(limits=c(0, 80), expand = expansion(mult = 0, add = 0)) +
  labs(y= " ", x = " ") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_fill_manual(values = c("#1790ab", "#fabe0e", 
                               "#8ab661", "#f28775", 
                               "#ffd9d5", "#6b4e55", 
                               "#cba2cb", "#b6c7dd", 
                               "#E39828", "#2f5d8c", "#f9f8f3"),
                    name="Social-Pragmatic Function",
                    labels=c("Information Seeking", "Pedagogical", 
                             "Harmonization", "Choice Seeking", 
                             "Action Seeking", "Attention Seeking", 
                             "Conversation Prompting", "Clarification Seeking",
                             "Emoting Aloud", "Status Checking", "Incomplete")) + 
  geom_errorbar(aes(ymax = seplus + se, ymin = seplus - se), position = "identity", width = 0.3)

grid.arrange(stacked_form, stacked_function, nrow = 2, ncol = 1)

# --- 5.6 Supplementary plots without error bars ----------------------------

# Proportion-based plots without error bars -------------------------------

# form plot
stackedpct_form_nobar <- ggplot(formpct_mean_sd_se_plot, aes(x = as.factor(bars),
                                                       y = meannum,
                                                       fill = as.factor(form_function_type))) +
  geom_bar(colour = "black", stat = "identity", width = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y = " ", x = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("#c3e4fd", "#ffe699", "#a7ba59", "#175a9e",
                               "#e2cbf7", "#f5c8bf", "#af6e4e", "#f9f8f3"),
                    name = "Linguistic Form",
                    labels = c("Yes-No Questions", "Wh- Questions",
                               "Rhetorical/Tag Questions", "Thinking Prompts",
                               "Mirroring Questions", "Fill-in-the-Blanks",
                               "Empty Prompts", "Incomplete"))

# function plot
stackedpct_function_nobar <- ggplot(functionpct_mean_sd_se_plot, aes(x = as.factor(bars),
                                                               y = meannum,
                                                               fill = as.factor(form_function_type))) +
  geom_bar(colour = "black", stat = "identity", width = 0.5) +
  scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
  labs(y = " ", x = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("#1790ab", "#fabe0e", "#ffd9d5", "#8ab661",
                               "#f28775", "#b6c7dd", "#cba2cb", "#6b4e55",
                               "#b9bbb6", "#2f5d8c", "#f9f8f3"),
                    name = "Social-Pragmatic Function",
                    labels = c("Information Seeking", "Pedagogical",
                               "Harmonization", "Choice Seeking",
                               "Action Seeking", "Attention Seeking",
                               "Conversation Prompting", "Clarification Seeking",
                               "Emoting Aloud", "Status Checking", "Incomplete"))
grid.arrange(stackedpct_form_nobar, stackedpct_function_nobar, nrow = 2, ncol = 1)


# Table S3, frequency figures without error bars --------------------------

# linguistic form frequency 

form_long <- qp_long[qp_long$form_function_type %in% vec_form, ]
form_long <- form_long[form_long$stat %in% vec_nonpct, ]

form_long_mean <- form_long[form_long$stat == "mean", ]

form_long_mean$uniqueid <- str_c(form_long_mean$chinese, "_",
                                 form_long_mean$form_function_type, "_",
                                 form_long_mean$speaker)

form_mean_plot <- form_long_mean

# this determines how many bars there are (as.factor in ggplot):
form_mean_plot$bars <- str_c(form_mean_plot$speaker, "_", form_mean_plot$chinese)

# Re-level the fill factor based on frequency
form_mean_plot <- form_mean_plot %>%
  mutate(form_function_type = form_function_type %>%
           fct_relevel("c", "o", "r", "t", "m", "f", "e", "i"))

# Label the bars
form_mean_plot$bars <- as.character(form_mean_plot$bars)
form_mean_plot$bars[form_mean_plot$bars == "prt_0"] <- "EA Parents"
form_mean_plot$bars[form_mean_plot$bars == "prt_1"] <- "CI Parents"
form_mean_plot$bars[form_mean_plot$bars == "chi_0"] <- "EA Children"
form_mean_plot$bars[form_mean_plot$bars == "chi_1"] <- "CI Children"
form_mean_plot$bars <- factor(form_mean_plot$bars,
                              levels = c("CI Parents", "EA Parents",
                                         "CI Children", "EA Children"))

# plotting
stacked_form <- ggplot(form_mean_plot, aes(x = as.factor(bars),
                                           y = meannum,
                                           fill = as.factor(form_function_type))) +
  geom_bar(colour = "black", stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = 0, add = 0)) +
  labs(y = " ", x = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("#c3e4fd", "#ffe699", "#a7ba59", "#175a9e",
                               "#e2cbf7", "#f5c8bf", "#af6e4e", "#f9f8f3"),
                    name = "Linguistic Form",
                    labels = c("Yes-No Questions", "Wh- Questions",
                               "Rhetorical Questions", "Thinking Prompts",
                               "Mirroring Questions", "Fill-in-the-Blanks",
                               "Empty Prompts", "Incomplete"))


# social-pragmatic function frequency 

function_long <- qp_long[qp_long$form_function_type %in% vec_function, ]

function_long_mean <- function_long[function_long$stat == "mean", ]

function_long_mean$uniqueid <- str_c(function_long_mean$chinese, "_",
                                     function_long_mean$form_function_type, "_",
                                     function_long_mean$speaker)

function_mean_plot <- function_long_mean

# this determines how many bars there are (as.factor in ggplot):
function_mean_plot$bars <- str_c(function_mean_plot$speaker, "_", function_mean_plot$chinese)

# Re-level the fill factor based on frequency
function_mean_plot <- function_mean_plot %>%
  mutate(form_function_type = form_function_type %>%
           fct_relevel("ix", "pd", "ha", "ch", "ac", "at", "co", "cl", "em", "st", "i"))

# Label the bars
function_mean_plot$bars <- as.character(function_mean_plot$bars)
function_mean_plot$bars[function_mean_plot$bars == "prt_0"] <- "EA Parents"
function_mean_plot$bars[function_mean_plot$bars == "prt_1"] <- "CI Parents"
function_mean_plot$bars[function_mean_plot$bars == "chi_0"] <- "EA Children"
function_mean_plot$bars[function_mean_plot$bars == "chi_1"] <- "CI Children"
function_mean_plot$bars <- factor(function_mean_plot$bars,
                                  levels = c("CI Parents", "EA Parents",
                                             "CI Children", "EA Children"))

# plotting
stacked_function <- ggplot(function_mean_plot, aes(x = as.factor(bars),
                                                   y = meannum,
                                                   fill = as.factor(form_function_type))) +
  geom_bar(colour = "black", stat = "identity", width = 0.5) +
  scale_y_continuous(limits = c(0, 80), expand = expansion(mult = 0, add = 0)) +
  labs(y = " ", x = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold", family = "sans"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("#1790ab", "#fabe0e", "#8ab661", "#f28775",
                               "#ffd9d5", "#6b4e55", "#cba2cb", "#b6c7dd",
                               "#E39828", "#2f5d8c", "#f9f8f3"),
                    name = "Social-Pragmatic Function",
                    labels = c("Information Seeking", "Pedagogical",
                               "Harmonization", "Choice Seeking",
                               "Action Seeking", "Attention Seeking",
                               "Conversation Prompting", "Clarification Seeking",
                               "Emoting Aloud", "Status Checking", "Incomplete"))

grid.arrange(stacked_form, stacked_function, nrow = 2, ncol = 1)

# save at publication resolution instead of just drawing:
g <- arrangeGrob(stacked_form, stacked_function, nrow = 2, ncol = 1)
ggsave("figure1_stacked.png", g, width = 8, height = 10, dpi = 300)
