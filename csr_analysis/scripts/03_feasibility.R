# Step 5: test the feasibility of every candidate basic/intermediate analysis on the cleaned data.
# For each analysis the script computes the quantities that decide feasibility (group sizes, expected cell counts,
# sparse cells, skewness, structural zeros imposed by Codebook rules) and assigns a verdict:
#   FEASIBLE                 - assumptions of the planned test are met
#   FEASIBLE (exact/MC test) - chi-square expected counts too small; Fisher/Monte-Carlo exact p used instead
#   DESCRIPTIVE ONLY         - groups/cells too small for any inferential test; n(%) only
#   NOT FEASIBLE             - the data needed is absent
# Output: output/step5_feasibility.csv (read into the plan document)

suppressPackageStartupMessages({ library(dplyr) })
root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), ".."))
d <- readRDS(file.path(root, "clean", "csr_clean.rds"))
ns <- d$NeedsSurvey; bb <- d$BatteryBus; vs <- d$VishramSadan; it <- d$items

rows <- list()
add <- function(id, obj, sheet, analysis, test, verdict, evidence) {
  rows[[length(rows) + 1]] <<- data.frame(ID = id, Objective = obj, Sheet = sheet, Analysis = analysis, Test = test,
                                         Verdict = verdict, Evidence = evidence, stringsAsFactors = FALSE)
}
chisq_feas <- function(x, y) {
  tb <- table(x, y); tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  if (min(dim(tb)) < 2) return(list(v = "DESCRIPTIVE ONLY", e = "one level only"))
  ex <- suppressWarnings(chisq.test(tb, correct = FALSE)$expected)
  pct <- mean(ex < 5) * 100; mn <- min(ex)
  zeros <- sum(tb == 0)
  e <- sprintf("%dx%d table; min expected %.2f; %.0f%% cells expected <5; %d empty cells", nrow(tb), ncol(tb), mn, pct, zeros)
  v <- if (pct <= 20 && mn >= 1) "FEASIBLE" else "FEASIBLE (exact/MC test)"
  list(v = v, e = e)
}
grp_feas <- function(y, g, test) {
  n <- tapply(!is.na(y), g, sum); n <- n[n > 0]
  sk <- tapply(y, g, function(v) { v <- v[!is.na(v)]; if (length(v) < 3) NA else mean((v - mean(v))^3) / sd(v)^3 })
  e <- paste0("n per group ", paste(names(n), n, sep = "=", collapse = ", "), "; skewness ",
              paste(sprintf("%.2f", sk), collapse = "/"))
  v <- if (min(n) >= 10) "FEASIBLE" else if (min(n) >= 3) "FEASIBLE (low power; report with caution)" else "DESCRIPTIVE ONLY"
  list(v = v, e = e)
}

# ------------------------------------------------------------------------------------------------ PRIMARY
pop <- c("Administration" = 5, "Finance & Account Division" = 23, "Clinical, Para clinical & Non-clinical Departments" = 754,
         "Engineering Services Division" = 21, "Medical Social Service Officers Unit" = 49)
resp <- table(ns$Department_Category_std)[names(pop)]
add("P1", "Primary", "NeedsSurvey", "Response rate by Table 1 stratum (respondents / population)", "n (%) with 95% CI",
    "FEASIBLE", paste0("respondents ", paste(resp, collapse = "/"), " of ", paste(pop, collapse = "/"), " (total ", sum(resp), "/", sum(pop), ")"))
m <- rbind(resp, pop - resp); ex <- suppressWarnings(chisq.test(m)$expected)
add("P2", "Primary", "NeedsSurvey", "Do response rates differ between the 5 strata?", "Chi-square / Fisher exact (responded vs not)",
    if (min(ex) < 5) "FEASIBLE (exact/MC test)" else "FEASIBLE", sprintf("2x5 table; min expected %.2f (Administration)", min(ex)))
cat_ns <- c("Gender_std", "Age_Band_std", "Education_std", "Service_Band_std", "Response_Mode_std", "Department_Category_std",
            "Faculty_Subgroup_std", "Analytic_Stratum_std", "Designation_std", "CSR_Committee_Member_std",
            "Prior_CSR_Project_Involvement_std", "Top_Theme_Choice_std", "Budget_Band_For_Top_Need_std", "Response_Month")
add("P3", "Primary", "NeedsSurvey", paste("Distribution n (%) of", length(cat_ns), "categorical variables + Department (51 levels)"),
    "n (%)", "FEASIBLE", paste(cat_ns, collapse = ", "))
add("P4", "Primary", "NeedsSurvey", "Age_Years and Service_Years_AIIMS", "mean (SD), median (IQR), range", "FEASIBLE",
    sprintf("age %d-%d, service %d-%d, no blanks", min(ns$Age_Years), max(ns$Age_Years), min(ns$Service_Years_AIIMS), max(ns$Service_Years_AIIMS)))
for (v in c("Gender_std", "Age_Band_std", "Education_std", "Service_Band_std", "Response_Mode_std", "CSR_Committee_Member_std",
            "Prior_CSR_Project_Involvement_std")) {
  f <- chisq_feas(ns[[v]], ns$Analytic_Stratum_std)
  add(paste0("P5.", v), "Primary", "NeedsSurvey", paste("Profile:", v, "by Analytic_Stratum (4 groups)"), "Chi-square", f$v, f$e)
}
f <- grp_feas(ns$Age_Years, ns$Analytic_Stratum_std); add("P6a", "Primary", "NeedsSurvey", "Age_Years by Analytic_Stratum", "One-way ANOVA + Kruskal-Wallis", f$v, f$e)
f <- grp_feas(ns$Service_Years_AIIMS, ns$Analytic_Stratum_std); add("P6b", "Primary", "NeedsSurvey", "Service years by Analytic_Stratum", "One-way ANOVA + Kruskal-Wallis", f$v, f$e)
add("P7", "Primary", "NeedsSurvey", "Each of 16 needs: 5-category n (%), mean (SD), median (IQR), % agree (4-5) with Wilson 95% CI; rank by mean",
    "Descriptive", "FEASIBLE", "16 items x 510, no blanks")
al <- sapply(it$themes, function(cols) { x <- as.matrix(ns[, cols]); k <- ncol(x); k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))) })
add("P8", "Primary", "NeedsSurvey", "Internal consistency of each theme and all 16 items", "Cronbach's alpha, item-total r, alpha-if-deleted",
    if (all(al >= 0.6)) "FEASIBLE" else "FEASIBLE (some themes weak: scores reported with caution)",
    paste0("alpha ", paste(names(al), sprintf("%.2f", al), sep = "=", collapse = ", ")))
add("P9", "Primary", "NeedsSurvey", "Theme scores compared within respondents (4 related means)", "Repeated-measures ANOVA + Friedman; Holm-adjusted paired t / Wilcoxon",
    "FEASIBLE", "510 complete sets of 4 theme scores")
for (v in c("Gender_std", "Age_Band_std", "Education_std", "Service_Band_std", "Analytic_Stratum_std", "CSR_Committee_Member_std",
            "Prior_CSR_Project_Involvement_std", "Response_Mode_std")) {
  f <- grp_feas(ns$Overall_Importance, ns[[v]])
  add(paste0("P10.", v), "Primary", "NeedsSurvey", paste("4 theme scores + overall importance by", v),
      if (nlevels(ns[[v]]) == 2) "Independent t-test (Welch) + Mann-Whitney" else "One-way ANOVA + Kruskal-Wallis", f$v, f$e)
}
worst <- max(sapply(it$imp, function(i) mean(suppressWarnings(chisq.test(table(ns[[paste0(i, "_Agree")]], ns$Analytic_Stratum_std))$expected) < 5)))
add("P11", "Primary", "NeedsSurvey", "% agreeing with each of 16 needs by Analytic_Stratum", "Chi-square (Holm/BH across 16 items)",
    if (worst <= 0.2) "FEASIBLE" else "FEASIBLE (exact/MC test)", sprintf("2x4 tables; worst share of expected<5 cells %.0f%%", worst * 100))
f <- chisq_feas(ns$Top_Theme_Choice_std, ns$Analytic_Stratum_std); add("P12", "Primary", "NeedsSurvey", "Forced-choice top theme by stratum", "Chi-square", f$v, f$e)
for (v in c("Gender_std", "Age_Band_std", "Education_std", "Service_Band_std", "CSR_Committee_Member_std", "Prior_CSR_Project_Involvement_std")) {
  f <- chisq_feas(ns$Top_Theme_Choice_std, ns[[v]]); add(paste0("P12.", v), "Primary", "NeedsSurvey", paste("Top theme by", v), "Chi-square", f$v, f$e)
}
tb <- table(ns$Top_Theme_Choice_std, ns$Budget_Band_For_Top_Need_std)
add("P13", "Primary", "NeedsSurvey", "Budget band by chosen theme", "Chi-square (structural zeros noted)", "FEASIBLE (structural zeros imposed by Codebook rule)",
    paste0(sum(tb == 0), " empty cells: Value added never >10 Cr and Infrastructure never <=1 Cr by Codebook rule, so the association is partly built in"))
add("P14", "Primary", "NeedsSurvey", "Agreement between forced-choice top theme and top-rated theme", "n (%) agreement; Cohen's kappa on non-tied rows",
    "FEASIBLE", sprintf("%d of 510 rows have a tie for top-rated theme", sum(ns$Top_Rated_Theme == "Tie")))
add("P15", "Primary", "NeedsSurvey", "Age / service years vs theme scores", "Spearman correlation (with Pearson)", "FEASIBLE", "510 pairs each")
add("P16", "Primary", "NeedsSurvey", "Designation (15 posts) or Department (51) as comparison groups", "-", "DESCRIPTIVE ONLY",
    sprintf("smallest designation n=%d; %d departments have n<5", min(table(ns$Designation)), sum(table(ns$Department) < 5)))

# ------------------------------------------------------------------------------------------------ SO1 sustainability
add("S1", "SO1", "Projects", "8 CSR projects by theme, contributor sector, operational since; months operational at survey start", "n (%), descriptive", "FEASIBLE", "8 rows")
add("S2", "SO1", "Projects", "3 representative projects: CAPEX, OPEX payer, annual cost to AIIMS, % of electricity bill, sustainability category", "Descriptive table", "FEASIBLE", "P01, P04, P08")
add("S3", "SO1", "Projects", "Derived cost indicators: bus electricity arithmetic, seat-trip capacity, AIIMS cost per seat-trip; HSCT fund per patient; lodging monthly occupancy trend",
    "Arithmetic, descriptive trend", "FEASIBLE", "inputs present for P08 (fleet), P04 (5 patients), P01 (7 monthly counts)")
add("S4", "SO1", "Projects", "Statistical test across projects", "-", "NOT FEASIBLE", "n = 3 representative projects")
add("S5", "SO1", "NeedsSurvey", "Sustainability confidence for each of 16 needs: n (%), mean (SD), median (IQR), % confident (4-5)", "Descriptive", "FEASIBLE", "16 items x 510")
add("S6", "SO1", "NeedsSurvey", "Importance vs sustainability for the same need", "Paired t-test (protocol) + Wilcoxon signed-rank, effect size dz; Holm across 16",
    "FEASIBLE (interpret with caution)", "Codebook rule forces each need's mean sustainability below its mean importance; the two ratings use different anchors (agreement vs confidence)")
add("S7", "SO1", "NeedsSurvey", "Importance-sustainability priority matrix (16 needs plotted by mean importance and mean confidence)", "Descriptive quadrant plot", "FEASIBLE", "16 points")
for (v in c("Analytic_Stratum_std", "CSR_Committee_Member_std", "Prior_CSR_Project_Involvement_std", "Gender_std", "Service_Band_std")) {
  f <- grp_feas(ns$Overall_SUS, ns[[v]])
  add(paste0("S8.", v), "SO1", "NeedsSurvey", paste("Sustainability theme scores by", v),
      if (nlevels(ns[[v]]) == 2) "Welch t-test + Mann-Whitney" else "ANOVA + Kruskal-Wallis", f$v, f$e)
}
al2 <- sapply(it$themes, function(cols) { x <- as.matrix(ns[, paste0("SUS_", cols)]); k <- ncol(x); k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x))) })
add("S9", "SO1", "NeedsSurvey", "Internal consistency of sustainability ratings by theme", "Cronbach's alpha", "FEASIBLE",
    paste0("alpha ", paste(names(al2), sprintf("%.2f", al2), sep = "=", collapse = ", ")))

# ------------------------------------------------------------------------------------------------ SO2 impact
imp_block <- function(df, sheet, pre, items, prof_vars, num_vars) {
  for (v in prof_vars) {
    f <- chisq_feas(df[[v]], df$Group_std)
    add(paste0(pre, "1.", v), "SO2", sheet, paste("Test vs Control profile:", v), "Chi-square", f$v, f$e)
  }
  for (v in num_vars) {
    f <- grp_feas(df[[v]], df$Group_std)
    add(paste0(pre, "2.", v), "SO2", sheet, paste("Test vs Control:", v), "Welch t-test + Mann-Whitney", f$v, f$e)
  }
  add(paste0(pre, "3"), "SO2", sheet, paste("Each of", length(items), "impact items: 0-5 n (%), mean (SD), median (IQR) by group"), "Descriptive", "FEASIBLE",
      paste(length(items), "items"))
  ev <- sapply(items, function(i) { x <- df[[i]]; paste0(sub("_.*", "", i), " n=", sum(!is.na(x))) })
  add(paste0(pre, "4"), "SO2", sheet, "Test vs Control on each item", "Student's/Welch t-test (protocol) + Mann-Whitney; Cohen's d; Holm",
      "FEASIBLE", paste(ev, collapse = ", "))
  worst <- max(sapply(items, function(i) min(suppressWarnings(chisq.test(table(df[[paste0(i, "_Agree")]], df$Group_std))$expected))))
  add(paste0(pre, "5"), "SO2", sheet, "% agree (3-5) by group per item; risk difference and OR with 95% CI", "Chi-square", "FEASIBLE",
      sprintf("2x2 tables, smallest expected count across items %.1f", min(sapply(items, function(i) min(suppressWarnings(chisq.test(table(df[[paste0(i, "_Agree")]], df$Group_std))$expected))))))
  x <- as.matrix(df[, if (pre == "B") paste0(items, "_i") else items]); k <- ncol(x)
  a <- k / (k - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x)))
  add(paste0(pre, "6"), "SO2", sheet, "Overall impact score (mean of items): reliability and Test vs Control", "Cronbach's alpha; Welch t-test", "FEASIBLE", sprintf("alpha all items = %.2f", a))
  add(paste0(pre, "7"), "SO2", sheet, "Group difference in overall score adjusted for pre-use characteristics", "Multiple linear regression; logistic regression for 'satisfied' (agree)",
      "FEASIBLE", sprintf("n=%d, events (satisfied) %d; covariates gender, age, education, profile, visit type, distance class, income", nrow(df),
                          sum(df[[paste0(items[length(items) - 1], "_Agree")]], na.rm = TRUE)))
  add(paste0(pre, "8"), "SO2", sheet, "Does the group difference vary by participant profile / visit type / gender?", "Two-way ANOVA with interaction", "FEASIBLE",
      paste0("smallest group x profile cell n=", min(table(df$Group_std, df$Participant_Profile_std))))
}
imp_block(bb, "BatteryBus", "B", it$bb,
          c("Gender_std", "Age_Band_test", "Education_std", "Participant_Profile_std", "Centre_Visited_std", "Transport_Mode_std",
            "Visit_Type_std", "Household_Income_Band_std", "Distance_Class"),
          c("Age_Years", "Distance_From_Home_km", "IntraCampus_Trips_Today", "Rs_Spent_IntraCampus_Today", "Minutes_IntraCampus_Today"))
add("B9", "SO2", "BatteryBus", "Users only: bus trips, rupees saved, waiting minutes; wait vs availability ratings", "Descriptive; Spearman correlation", "FEASIBLE",
    sprintf("431 users; wait 2-15 min; Rs saved 0-%d", max(bb$Rs_Saved_Today_SelfEstimate, na.rm = TRUE)))
add("B10", "SO2", "BatteryBus", "Alternative within-campus mode by group", "Within-group n (%) only", "DESCRIPTIVE ONLY",
    "Codebook: Test rows mix used and hypothetical modes; a between-group test is not valid")
add("B11", "SO2", "BatteryBus", "Users only: predictors of satisfaction (bus trips, wait, Rs saved, profile, visit type)", "Linear / ordinal regression", "FEASIBLE",
    "431 users; note several Codebook consistency rules tie ratings to these variables")
imp_block(vs, "VishramSadan", "V", it$vs,
          c("Gender_std", "Age_Band_test", "Education_std", "Participant_Profile_std", "Centre_Visited_std", "Transport_Mode_std",
            "Visit_Type_std", "Household_Income_Band_std", "Distance_Class"),
          c("Age_Years", "Distance_From_Home_km", "Nights_Stayed", "Lodging_Rs_Per_Night", "Food_Rs_Per_Day", "Companions_N", "Travel_Minutes_Stay_To_AIIMS"))
f <- grp_feas(vs$VS_Overall_Mean[vs$Group == "Control group"], droplevels(vs$Accommodation_std[vs$Group == "Control group"]))
add("V9", "SO2", "VishramSadan", "Control group: accommodation type n (%); ratings, cost, travel time by accommodation type", "Kruskal-Wallis / ANOVA", f$v, f$e)
add("V10", "SO2", "VishramSadan", "Similar facility seen elsewhere (users only)", "n (%) with 95% CI", "FEASIBLE", "35 Yes / 435 No of 470")
add("V11", "SO2", "VishramSadan", "Users only: predictors of satisfaction (nights, tariff, companions, profile)", "Linear / ordinal regression", "FEASIBLE", "470 users")

# ------------------------------------------------------------------------------------------------ not feasible
add("X1", "Primary", "-", "Content validity of the needs questionnaire (CVI/CVR), pilot results", "-", "NOT FEASIBLE", "no expert ratings or pilot data in the file")
add("X2", "Primary/SO1", "-", "Focus group discussions, records review, unstructured interviews", "-", "NOT FEASIBLE", "qualitative data not in the file")
add("X3", "SO2", "-", "Impact of the HSCT project (P04)", "-", "NOT FEASIBLE", "no user-level data; 5 patients treated")
add("X4", "SO2", "-", "Before-after, difference-in-differences, regression discontinuity, instrumental variables", "-", "NOT FEASIBLE", "single cross-section, no baseline, no cut-off, no instrument")
add("X5", "SO2", "-", "Direct comparison of the 0-5 impact scale with the 1-5 needs scale", "-", "NOT FEASIBLE", "different populations and scale formats")
add("X6", "SO1", "-", "Cost-effectiveness / cost per outcome", "-", "NOT FEASIBLE", "no outcome measured in natural units per cost; only cost-to-AIIMS indicators")
add("X7", "SO1", "-", "Bed occupancy rate of Vishram Sadan", "-", "NOT FEASIBLE", "occupancy is a count of persons, not bed-nights")

out <- bind_rows(rows)
write.csv(out, file.path(root, "output", "step5_feasibility.csv"), row.names = FALSE)
print(table(out$Verdict))
cat("\n"); print(out[, c("ID", "Verdict", "Evidence")], right = FALSE)
