# Step 4b: clean and standardise the mastersheet for analysis in R.
#
# Decisions (user, Step 4 MCQs):
#  1. Keep every original column unchanged; add numeric Codebook codes as <var>_code.
#  2. Harmonise labels across sheets in <var>_std columns; every change is listed in the Label_mapping sheet.
#  3. Household income "Not disclosed" stays a category (Income_std); Income_ord is NA for it (models only).
#  4. The 12 blank bus ratings are imputed by an ordinal model (most probable category from a proportional-odds
#     regression (ordinal::clm, logit link) of the item on the respondent's other 9 ratings and group). Original columns keep the blank;
#     imputed copies are <item>_i, and Imputed_Item names the cell that was filled.
#  5. Derived: agree/disagree splits, theme and overall scores, importance-sustainability gaps, local/outstation.
#  6. Age bands: thesis bands kept for description; Age_Band_test merges 61 and over for tests.
#  7. Similar facility: reported among users only (Similar_Facility_Users).
#
# Inputs : raw/poonam_csir_v3_1.xlsx (read only)
# Outputs: clean/csr_clean.rds, clean/csr_clean.xlsx, clean/*.csv, output/step4_cleaning_log.txt

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(openxlsx); library(ordinal)
})
select <- dplyr::select

root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), ".."))
raw_path <- file.path(root, "raw", "poonam_csir_v3_1.xlsx")
dir.create(file.path(root, "clean"), showWarnings = FALSE)
log_lines <- character()
logm <- function(...) { msg <- paste0(...); log_lines <<- c(log_lines, msg); cat(msg, "\n") }

cb <- read_excel(raw_path, sheet = "Codebook")
ns_raw <- read_excel(raw_path, sheet = "NeedsSurvey")
bb_raw <- read_excel(raw_path, sheet = "BatteryBus")
vs_raw <- read_excel(raw_path, sheet = "VishramSadan")
pj_raw <- read_excel(raw_path, sheet = "Projects")
logm("Read ", nrow(ns_raw), " NeedsSurvey, ", nrow(bb_raw), " BatteryBus, ", nrow(vs_raw), " VishramSadan, ",
     nrow(pj_raw), " Projects rows from ", basename(raw_path))

# ---------------------------------------------------------------------------------------------- codebook codes
# Parse "Label = code; Label = code" from the Codebook for one sheet/header.
cb_codes <- function(sheet, header) {
  spec <- cb$`Allowed values with numeric codes`[cb$Sheet == sheet & cb$Header == header]
  parts <- trimws(strsplit(spec, ";")[[1]])
  m <- regmatches(parts, regexec("^(.*?)\\s*=\\s*(-?[0-9]+)\\b", parts))
  m <- m[lengths(m) == 3]
  setNames(as.integer(vapply(m, `[`, "", 3)), trimws(vapply(m, `[`, "", 2)))
}
add_codes <- function(df, sheet, headers) {
  for (h in headers) {
    codes <- cb_codes(sheet, h)
    v <- df[[h]]
    if (is.numeric(v)) next
    out <- unname(codes[as.character(v)])
    unmatched <- unique(v[!is.na(v) & is.na(out)])
    if (length(unmatched)) stop(sheet, ".", h, ": labels not in Codebook: ", paste(unmatched, collapse = ", "))
    df[[paste0(h, "_code")]] <- as.integer(out)
  }
  df
}

# ------------------------------------------------------------------------------------------ label harmonisation
label_map <- data.frame(Sheet = character(), Column = character(), Original = character(), Standardised = character(),
                        Reason = character())
std_col <- function(df, sheet, col, map, levels, reason) {
  v <- as.character(df[[col]])
  s <- ifelse(v %in% names(map), map[v], v)
  bad <- setdiff(unique(s[!is.na(s)]), levels)
  if (length(bad)) stop(sheet, ".", col, ": unmapped values ", paste(bad, collapse = ", "))
  ch <- unique(data.frame(Original = v, Standardised = s)[v != s & !is.na(v), ])
  if (nrow(ch)) label_map <<- rbind(label_map, data.frame(Sheet = sheet, Column = col, ch, Reason = reason))
  df[[paste0(col, "_std")]] <- factor(s, levels = levels)
  df
}

income_levels <- c("<10,000", "10,000-25,000", "25,001-50,000", ">50,000", "Not disclosed")
visit_levels  <- c("New", "Follow-up")
age_levels_pt <- c("15-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81+")
edu_levels_pt <- c("<Primary", "High School", "Intermediate", "Graduate", "Post Graduate")
centre_levels <- c("Main Hospital", "Old RAK OPD", "New RAK OPD", "CN Centre", "Dr BRAIRCH", "Dr RPCOS", "CDER",
                   "Surgical Block", "MCH Block", "Emergency", "Trauma Centre", "Burns & Plastic Block")
age_test <- function(x) factor(ifelse(x %in% c("61-70", "71-80", "81+"), "61+", as.character(x)),
                               levels = c("15-20", "21-30", "31-40", "41-50", "51-60", "61+"))

# ================================================================================================= NeedsSurvey
imp_items <- c("PC1_LongTermFinancingBeyondScheme", "PC2_HighCostIllnessNotCovered", "PC3_NonAffordingNonScheme",
               "PC4_HomeCareHighCostEquipment", "INF1_AcademicResearchTertiaryCentres", "INF2_PublicFacilities",
               "INF3_HighCostMedicalEquipment", "INF4_HospitalSupportServices", "VAS1_ParkGreenSpaces",
               "VAS2_ExternalInterCampusTransport", "VAS3_AllWeatherWalkways", "VAS4_AirQualitySystems",
               "VAS5_InternalTransport", "RES1_ResearchChairs", "RES2_ShortTermTrainingGrants", "RES3_FellowshipResearch")
sus_items <- paste0("SUS_", imp_items)
themes <- list(PatientCare = imp_items[1:4], Infrastructure = imp_items[5:8],
               ValueAdded = imp_items[9:13], Research = imp_items[14:16])

ns <- ns_raw
ns$Response_Date <- as.Date(ns$Response_Date)
ns$Response_Month <- factor(format(ns$Response_Date, "%Y-%m"))
ns <- add_codes(ns, "NeedsSurvey", c("Response_Mode", "Gender", "Age_Band", "Education", "Service_Band",
                                     "Department_Category", "Faculty_Subgroup", "Analytic_Stratum", "Designation",
                                     "CSR_Committee_Member", "Prior_CSR_Project_Involvement", "Top_Theme_Choice",
                                     "Budget_Band_For_Top_Need"))
ns <- std_col(ns, "NeedsSurvey", "Gender", c(), c("Male", "Female"), "")
ns <- std_col(ns, "NeedsSurvey", "Age_Band", c("<35" = "<=35"), c("<=35", "36-45", "46-55", "56-65"),
              "Codebook: '<35' is read as 35 or younger")
ns <- std_col(ns, "NeedsSurvey", "Service_Band", c("<5" = "<=5"), c("<=5", "6-15", "16-25", ">25"),
              "Codebook: '<5' is read as 5 or fewer")
ns <- std_col(ns, "NeedsSurvey", "Education", c(), c("Graduates", "Post-graduates", "Doctorate/DM/MCh"), "")
ns$Response_Mode_std <- factor(ns$Response_Mode, levels = c("Google Form", "Self-administered"))
ns$Department_Category_std <- factor(ns$Department_Category, levels = names(cb_codes("NeedsSurvey", "Department_Category")))
ns$Faculty_Subgroup_std <- factor(ns$Faculty_Subgroup, levels = c("Clinical", "Para-clinical", "Non-clinical"))
ns$Analytic_Stratum_std <- factor(ns$Analytic_Stratum, levels = names(cb_codes("NeedsSurvey", "Analytic_Stratum")))
ns$Designation_std <- factor(ns$Designation, levels = names(cb_codes("NeedsSurvey", "Designation")))
ns$CSR_Committee_Member_std <- factor(ns$CSR_Committee_Member, levels = c("Yes", "No"))
ns$Prior_CSR_Project_Involvement_std <- factor(ns$Prior_CSR_Project_Involvement, levels = c("Yes", "No"))
ns$Top_Theme_Choice_std <- factor(ns$Top_Theme_Choice, levels = c("Patient care", "Infrastructure", "Value added services", "Research"))
ns$Budget_Band_For_Top_Need_std <- factor(ns$Budget_Band_For_Top_Need,
                                          levels = c("Less than or equal to INR 1 Cr", "INR 1-10 Cr", "More than 10 Cr"))
ns$Faculty <- factor(ifelse(is.na(ns$Faculty_Subgroup), "No", "Yes"), levels = c("Yes", "No"))
# derived scores
for (t in names(themes)) {
  ns[[paste0("Theme_", t)]] <- rowMeans(ns[, themes[[t]]])
  ns[[paste0("SUS_Theme_", t)]] <- rowMeans(ns[, paste0("SUS_", themes[[t]])])
}
ns$Overall_Importance <- rowMeans(ns[, imp_items])
ns$Overall_SUS <- rowMeans(ns[, sus_items])
for (i in imp_items) {
  ns[[paste0(i, "_Agree")]] <- as.integer(ns[[i]] >= 4)                       # Agree/Strongly agree (top-2-box)
  ns[[paste0("SUS_", i, "_Confident")]] <- as.integer(ns[[paste0("SUS_", i)]] >= 4)  # Probably/Certainly
  ns[[paste0("GAP_", i)]] <- ns[[i]] - ns[[paste0("SUS_", i)]]                # importance minus sustainability
}
tm <- as.matrix(ns[, paste0("Theme_", names(themes))])
top_name <- c(PatientCare = "Patient care", Infrastructure = "Infrastructure", ValueAdded = "Value added services",
              Research = "Research")
ns$Top_Rated_Theme <- apply(tm, 1, function(r) {
  w <- which(abs(r - max(r)) < 1e-9)
  if (length(w) > 1) "Tie" else unname(top_name[names(themes)[w]])
})
ns$Top_Choice_Is_Top_Rated <- factor(mapply(function(ch, r) {
  w <- names(themes)[abs(r - max(r)) < 1e-9]
  if (ch %in% top_name[w]) "Yes" else "No"
}, ns$Top_Theme_Choice, split(tm, row(tm))), levels = c("Yes", "No"))
logm("NeedsSurvey: added codes, harmonised factors, 4 theme + overall scores (importance and sustainability), ",
     "16 agree flags, 16 confidence flags, 16 gap scores, top-rated theme.")

# ================================================================================================= BatteryBus
bb_items <- c("BB1_UseOfVehicleBetweenCentres", "BB2_VehicleAvailability", "BB3_SeatAvailability",
              "BB4_ConvenientPickupDrop", "BB5_MoneySavedTransport", "BB6_TravelTimeSaved", "BB7_TimeSavedParking",
              "BB8_MoneySavedParking", "BB9_Satisfaction", "BB10_FutureUse")
vs_items <- c("VS1_UsedAccommodation", "VS2_RoomAvailability", "VS3_MoneySavedLodging", "VS4_FoodAvailability",
              "VS5_FoodReasonablePrice", "VS6_TravelTimeSaved", "VS7_LocationConnectivity", "VS8_Satisfaction",
              "VS9_FutureUse")

prep_patient_sheet <- function(df, sheet) {
  df$Interview_Date <- as.Date(df$Interview_Date)
  df$Interview_Month <- factor(format(df$Interview_Date, "%Y-%m"))
  df <- std_col(df, sheet, "Group", c("Test group" = "Test (users)", "Control group" = "Control (non-users)"),
                c("Test (users)", "Control (non-users)"), "Group labels stated with what each group is (users / non-users)")
  df <- std_col(df, sheet, "Gender", c(male = "Male", female = "Female"), c("Male", "Female"),
                "Same category spelt in lower case on the lodging sheet")
  df <- std_col(df, sheet, "Age_Band", c("<20" = "15-20", ">81" = "81+", ">80" = "81+"), age_levels_pt,
                "Codebook: '<20' holds ages 15-20; '>81' (bus) and '>80' (lodging) both mean 81 and over")
  df$Age_Band_test <- age_test(df$Age_Band_std)
  df <- std_col(df, sheet, "Education", c(), edu_levels_pt, "")
  df <- std_col(df, sheet, "Visit_Type", c("Follow Up" = "Follow-up", "Follow up" = "Follow-up"), visit_levels,
                "Same category spelt 'Follow Up' (bus) and 'Follow up' (lodging)")
  df <- std_col(df, sheet, "Centre_Visited", c(BRAIRCH = "Dr BRAIRCH", RPCOS = "Dr RPCOS", MCH = "MCH Block"),
                centre_levels, "Codebook: the same places are named differently on the two sheets")
  df$Centre_Visited_std <- droplevels(df$Centre_Visited_std)
  df <- std_col(df, sheet, "Household_Income_Band", c(), income_levels, "")
  df$Income_ord <- factor(ifelse(df$Household_Income_Band == "Not disclosed", NA, df$Household_Income_Band),
                          levels = income_levels[1:4], ordered = TRUE)
  # Codebook (BatteryBus Distance_From_Home_km): "the outstation distances start at 60 km (rounded to 10 km)",
  # so a home at 60 km or more is outstation.
  df$Distance_Class <- factor(ifelse(df$Distance_From_Home_km < 60, "Local (<60 km)", "Outstation (>=60 km)"),
                              levels = c("Local (<60 km)", "Outstation (>=60 km)"))
  df$Log10_Distance_km <- log10(df$Distance_From_Home_km)
  df
}

bb <- prep_patient_sheet(bb_raw, "BatteryBus")
bb <- add_codes(bb, "BatteryBus", c("Group", "Gender", "Age_Band", "Education", "Participant_Profile", "Centre_Visited",
                                    "Transport_Mode_To_AIIMS", "Visit_Type", "Alt_Mode_Within_Campus",
                                    "Household_Income_Band"))
bb$Participant_Profile_std <- factor(bb$Participant_Profile, levels = c("Patient", "Attendant", "Staff", "Resident", "Student", "Others"))
bb$Transport_Mode_std <- factor(bb$Transport_Mode_To_AIIMS, levels = c("Auto Rickshaw", "Bus", "Metro", "Own Vehicle", "Taxi/Cab", "Walking"))
bb$Alt_Mode_std <- factor(bb$Alt_Mode_Within_Campus, levels = names(sort(cb_codes("BatteryBus", "Alt_Mode_Within_Campus"))))
bb$NonBus_Trips_Today <- bb$IntraCampus_Trips_Today - bb$Bus_Trips_Today
bb$Bus_Share_Of_Trips <- bb$Bus_Trips_Today / bb$IntraCampus_Trips_Today

# ---- ordinal-model imputation of the 12 blank ratings
bb$Imputed_Item <- NA_character_
for (it in bb_items) bb[[paste0(it, "_i")]] <- bb[[it]]
complete <- complete.cases(bb[, bb_items])
imp_log <- data.frame()
for (r in which(!complete)) {
  miss <- bb_items[is.na(unlist(bb[r, bb_items]))]
  stopifnot(length(miss) == 1)
  others <- setdiff(bb_items, miss)
  fit_df <- bb[complete, c(miss, others, "Group")]
  fit_df[[miss]] <- factor(fit_df[[miss]], levels = 0:5, ordered = TRUE)
  fit_df[[miss]] <- droplevels(fit_df[[miss]])
  form <- as.formula(paste(miss, "~", paste(c(others, "Group"), collapse = " + ")))
  fit <- clm(form, data = fit_df, link = "logit")
  stopifnot(fit$convergence$code == 0)
  p <- predict(fit, newdata = as.data.frame(bb[r, c(others, "Group")]), type = "prob")$fit[1, ]
  val <- as.integer(names(p)[which.max(p)])
  bb[[paste0(miss, "_i")]][r] <- val
  bb$Imputed_Item[r] <- miss
  imp_log <- rbind(imp_log, data.frame(Resp_ID = bb$Resp_ID[r], Group = bb$Group[r], Item = miss, Imputed_Value = val,
                                       Probability = round(max(p), 3),
                                       Probs_0to5 = paste(sprintf("%s:%.2f", names(p), p), collapse = " ")))
}
logm("BatteryBus: imputed ", nrow(imp_log), " blank ratings by proportional-odds model (ordinal::clm, logit) fitted on the ",
     sum(complete), " complete rows; most probable category taken.")
# codebook rule checks on the imputed cells (5-level reading: 0->1, 1->2, 2/3->3, 4->4, 5->5)
five <- function(x) c(1, 2, 3, 3, 4, 5)[x + 1]
b9 <- five(bb$BB9_Satisfaction_i); b10 <- five(bb$BB10_FutureUse_i)
opp <- (b9 <= 2 & b10 >= 4) | (b10 <= 2 & b9 >= 4)
pub9 <- bb$BB9_Satisfaction_i; pub10 <- bb$BB10_FutureUse_i
pub_rule <- (pub9 <= 1 & pub10 >= 3) | (pub9 >= 4 & pub10 <= 2) | (pub10 <= 1 & pub9 >= 3) | (pub10 >= 4 & pub9 <= 2)
same_max <- apply(bb[, paste0(bb_items, "_i")], 1, function(x) max(table(x)))
logm("  rule check after imputation: BB9/BB10 opposite sides = ", sum(opp), "; BB9/BB10 published-scale rule ",
     "violations = ", sum(pub_rule), "; max identical answers per respondent = ", max(same_max), " (rule <= 7)")
imp_rows <- !is.na(bb$Imputed_Item)
logm("  violations among the 12 imputed rows: opposite sides ", sum(opp[imp_rows]), ", published rule ",
     sum(pub_rule[imp_rows]), ", >7 identical ", sum(same_max[imp_rows] > 7))

for (it in bb_items) bb[[paste0(it, "_Agree")]] <- as.integer(bb[[paste0(it, "_i")]] >= 3)
bb$BB_Overall_Mean <- rowMeans(bb[, paste0(bb_items, "_i")])
bb$BB_Overall_Mean_available <- rowMeans(bb[, bb_items], na.rm = TRUE)
bb$BB_Items_Answered <- rowSums(!is.na(bb[, bb_items]))

# ================================================================================================= VishramSadan
vs <- prep_patient_sheet(vs_raw, "VishramSadan")
vs <- add_codes(vs, "VishramSadan", c("Group", "Gender", "Age_Band", "Education", "Participant_Profile", "Visit_Type",
                                      "Centre_Visited", "Transport_Stay_To_AIIMS", "Similar_Facility_Elsewhere",
                                      "Accommodation_Type_Used", "Household_Income_Band"))
vs$Participant_Profile_std <- factor(vs$Participant_Profile, levels = c("Patient", "Attendant", "Others"))
vs$Transport_Mode_std <- factor(vs$Transport_Stay_To_AIIMS, levels = c("Auto Rickshaw", "Bus", "Metro", "Own Vehicle", "Taxi/Cab", "Shuttle by AIIMS", "Walking"))
vs$Accommodation_std <- factor(vs$Accommodation_Type_Used, levels = names(cb_codes("VishramSadan", "Accommodation_Type_Used")))
vs$Similar_Facility_Users <- factor(ifelse(vs$Group == "Test group", vs$Similar_Facility_Elsewhere, NA), levels = c("Yes", "No"))
vs$Party_Size <- vs$Companions_N + 1L
vs$Lodging_Rs_Total_So_Far <- vs$Lodging_Rs_Per_Night * vs$Nights_Stayed
for (it in vs_items) vs[[paste0(it, "_Agree")]] <- as.integer(vs[[it]] >= 3)
vs$VS_Overall_Mean <- rowMeans(vs[, vs_items])
logm("VishramSadan: added codes, harmonised factors, agree flags, overall score, users-only similar-facility answer.")

# ================================================================================================= Projects
pj <- pj_raw
pj$Annual_Cost_To_AIIMS_Rs_Lakh <- pj$Annual_Cost_To_AIIMS_Rs / 1e5
pj$Cost_Pct_Electricity_Exact <- 100 * pj$Annual_Cost_To_AIIMS_Rs / pj$AIIMS_Electricity_Bill_Rs_FY2021_22
pj$Bus_Seat_Trip_Capacity_Per_Year <- with(pj, Vehicles_N * Rounds_Per_Vehicle_Per_Day * Seats_Per_Vehicle * Operating_Days_Per_Year)
pj$Operational_Since_Date <- as.Date(paste0("01-", pj$Operational_Since), format = "%d-%b-%Y")

# ================================================================================================= verification
stopifnot(identical(ns$Resp_ID, ns_raw$Resp_ID), identical(bb$Resp_ID, bb_raw$Resp_ID), identical(vs$Resp_ID, vs_raw$Resp_ID))
for (nm in c("ns", "bb", "vs")) {
  cl <- get(nm); rw <- get(paste0(nm, "_raw"))
  for (col in names(rw)) {
    a <- cl[[col]]; b <- rw[[col]]
    if (inherits(b, "POSIXct")) b <- as.Date(b)
    if (!isTRUE(all.equal(a, b, check.attributes = FALSE))) stop(nm, ": original column changed: ", col)
  }
}
logm("Verified: every original column in all three survey sheets is unchanged cell by cell (dates converted to Date class).")
stopifnot(sum(is.na(bb[, paste0(bb_items, "_i")])) == 0, sum(is.na(bb[, bb_items])) == 12)

# ================================================================================================= derived codebook
deriv <- data.frame(
  Sheet = c("All survey sheets", "All survey sheets", rep("BatteryBus and VishramSadan", 7), rep("NeedsSurvey", 11), rep("BatteryBus", 9), rep("VishramSadan", 6), rep("Projects", 4)),
  Column = c("<var>_code", "<var>_std", "Group_std", "Age_Band_test", "Income_ord", "Distance_Class", "Log10_Distance_km",
             "Interview_Month", "<item>_Agree (0-5 items)",
             "Response_Month", "Faculty", "Theme_<theme>", "SUS_Theme_<theme>", "Overall_Importance / Overall_SUS",
             "<item>_Agree (1-5 importance)", "SUS_<item>_Confident", "GAP_<item>", "Top_Rated_Theme", "Top_Choice_Is_Top_Rated",
             "Department_Category_std etc.",
             "<item>_i", "Imputed_Item", "BB_Overall_Mean", "BB_Overall_Mean_available", "BB_Items_Answered",
             "NonBus_Trips_Today", "Bus_Share_Of_Trips", "Participant_Profile_std / Transport_Mode_std / Alt_Mode_std", "Group_code",
             "Similar_Facility_Users", "Party_Size", "Lodging_Rs_Total_So_Far", "VS_Overall_Mean", "Accommodation_std", "Transport_Mode_std",
             "Annual_Cost_To_AIIMS_Rs_Lakh", "Cost_Pct_Electricity_Exact", "Bus_Seat_Trip_Capacity_Per_Year", "Operational_Since_Date"),
  Definition = c("Numeric code from the Codebook for the original text label (codes are sheet-specific, as in the Codebook)",
                 "Harmonised label as an R factor with levels in Codebook/logical order (unordered; see Label_mapping for every changed spelling)",
                 "Test (users) / Control (non-users)",
                 "Age band for tests: 61-70, 71-80 and 81+ merged as 61+",
                 "Household income as an ordered factor; 'Not disclosed' set to NA (models only)",
                 "Local (<60 km) vs Outstation (>=60 km): the Codebook states that outstation distances start at 60 km",
                 "log10 of road distance from home (km)",
                 "Year-month of interview",
                 "1 = agree side of the 0-5 scale (3 Slightly agree, 4 Agree, 5 Strongly agree); 0 = 0-2",
                 "Year-month of response",
                 "Yes for Professor/Additional/Associate/Assistant Professor",
                 "Mean importance of the theme's items (1-5)",
                 "Mean sustainability confidence of the theme's items (1-5)",
                 "Mean of all 16 importance / 16 sustainability items",
                 "1 = Agree or Strongly agree (4-5); 0 = 1-3",
                 "1 = Probably or Certainly (4-5); 0 = 1-3",
                 "Importance minus sustainability confidence for the same need (-4 to +4)",
                 "Theme with the highest mean importance for the respondent ('Tie' when two or more share the maximum)",
                 "Yes if the forced-choice top theme is (one of) the respondent's top-rated theme(s)",
                 "Original labels as ordered factors in Codebook order",
                 "Rating with the blank filled by ordinal-model imputation (analysis copy); original column keeps the blank",
                 "Name of the imputed item for the 12 rows with a blank; NA otherwise",
                 "Mean of the 10 ratings using imputed copies",
                 "Mean of the answered ratings without imputation (sensitivity)",
                 "Number of the 10 ratings answered",
                 "IntraCampus_Trips_Today minus Bus_Trips_Today",
                 "Bus_Trips_Today / IntraCampus_Trips_Today",
                 "Original labels as factors in Codebook order",
                 "Test group = 1, Control group = 2",
                 "Similar facility seen elsewhere, users only (Control = NA, not asked)",
                 "Companions_N + 1",
                 "Lodging_Rs_Per_Night x Nights_Stayed at the time of interview",
                 "Mean of the 9 ratings",
                 "Accommodation type as factor in Codebook order",
                 "Mode between stay and AIIMS as factor in Codebook order",
                 "Annual_Cost_To_AIIMS_Rs / 100,000",
                 "100 x annual cost / AIIMS electricity bill, not truncated",
                 "Vehicles x rounds x seats x operating days",
                 "First day of the Operational_Since month"),
  stringsAsFactors = FALSE)

# ================================================================================================= write outputs
saveRDS(list(NeedsSurvey = ns, BatteryBus = bb, VishramSadan = vs, Projects = pj, Codebook = cb,
             Label_mapping = label_map, Imputation_log = imp_log, Derived_codebook = deriv,
             items = list(imp = imp_items, sus = sus_items, themes = themes, bb = bb_items, vs = vs_items)),
        file.path(root, "clean", "csr_clean.rds"))
to_sheet <- function(df) { df <- as.data.frame(df); for (c in names(df)) if (is.factor(df[[c]])) df[[c]] <- as.character(df[[c]]); df }
sheets <- list()
for (nm in c("NeedsSurvey", "BatteryBus", "VishramSadan", "Projects")) {
  d <- to_sheet(get(c(NeedsSurvey = "ns", BatteryBus = "bb", VishramSadan = "vs", Projects = "pj")[nm]))
  sheets[[nm]] <- d
  write.csv(d, file.path(root, "clean", paste0(nm, "_clean.csv")), row.names = FALSE, na = "")
}
sheets$Label_mapping <- label_map; sheets$Imputation_log <- imp_log; sheets$Derived_codebook <- deriv
sheets$Codebook_original <- as.data.frame(cb)
writexl::write_xlsx(sheets, file.path(root, "clean", "csr_clean.xlsx"))
logm("Wrote clean/csr_clean.rds, clean/csr_clean.xlsx (7 sheets + original Codebook) and 4 CSV files.")
logm("Columns: NeedsSurvey ", ncol(ns_raw), " -> ", ncol(ns), "; BatteryBus ", ncol(bb_raw), " -> ", ncol(bb),
     "; VishramSadan ", ncol(vs_raw), " -> ", ncol(vs), "; Projects ", ncol(pj_raw), " -> ", ncol(pj))
logm("Label changes recorded: ", nrow(label_map))
writeLines(c(paste("STEP 4b CLEANING LOG |", R.version.string, "|", format(Sys.time(), "%Y-%m-%d %H:%M")), log_lines,
             "", "IMPUTED CELLS:", capture.output(print(imp_log, row.names = FALSE)), "",
             "LABEL MAPPING:", capture.output(print(label_map, row.names = FALSE))),
           file.path(root, "output", "step4_cleaning_log.txt"))
