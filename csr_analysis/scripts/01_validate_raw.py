"""Step 4a: cell-by-cell validation of the raw workbook against every checkable Codebook rule.

Reads raw/poonam_csir_v3_1.xlsx (never modified) and writes output/step4_validation_report.txt.
Each check prints PASS or FAIL with the offending Resp_IDs, so every flagged cell can be traced.
"""
import re
import sys
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
RAW = ROOT / "raw" / "poonam_csir_v3_1.xlsx"
OUT = ROOT / "output" / "step4_validation_report.txt"

xl = pd.ExcelFile(RAW)
ns = xl.parse("NeedsSurvey")
bb = xl.parse("BatteryBus")
vs = xl.parse("VishramSadan")
pj = xl.parse("Projects")
cb = xl.parse("Codebook")

lines = []
n_fail = 0


def check(name, bad_mask_or_ids, df=None, id_col="Resp_ID"):
    """Record one rule. bad_mask_or_ids: boolean Series over df rows, or a list of ids."""
    global n_fail
    if isinstance(bad_mask_or_ids, pd.Series):
        ids = df.loc[bad_mask_or_ids.fillna(False).astype(bool), id_col].tolist()
    else:
        ids = list(bad_mask_or_ids)
    status = "PASS" if not ids else "FAIL"
    if ids:
        n_fail += 1
    shown = ", ".join(map(str, ids[:25])) + (" ..." if len(ids) > 25 else "")
    lines.append(f"[{status}] {name}" + (f"  -> {len(ids)} row(s): {shown}" if ids else ""))


def section(title):
    lines.append("")
    lines.append("=" * 100)
    lines.append(title)
    lines.append("=" * 100)


# ----------------------------------------------------------------------------- codebook allowed values
def allowed_labels(sheet, header):
    row = cb[(cb.Sheet == sheet) & (cb.Header == header)]
    if row.empty:
        return None
    spec = str(row.iloc[0]["Allowed values with numeric codes"])
    parts = [p.strip() for p in spec.split(";")]
    labs = {}
    for p in parts:
        m = re.match(r"^(.*?)\s*=\s*(-?\d+)\b", p)
        if m:
            labs[m.group(1).strip()] = int(m.group(2))
    return labs or None


section("0. STRUCTURE")
for name, df, n_expected, prefix in [("NeedsSurvey", ns, 510, "NS"), ("BatteryBus", bb, 844, "BB"), ("VishramSadan", vs, 875, "VS")]:
    check(f"{name}: Resp_ID unique", df.Resp_ID.duplicated(keep=False), df)
    pat = re.compile(rf"^{prefix}-\d{{4}}$")
    check(f"{name}: Resp_ID pattern {prefix}-####", ~df.Resp_ID.astype(str).str.match(pat), df)
    expected = {f"{prefix}-{i:04d}" for i in range(1, n_expected + 1)}
    missing = sorted(expected - set(df.Resp_ID))
    check(f"{name}: Resp_ID covers {prefix}-0001..{prefix}-{n_expected:04d} with no gaps", missing)
    check(f"{name}: Record_Type constant 'dt'", df.Record_Type != "dt", df)
    cb_headers = cb.loc[cb.Sheet == name, "Header"].tolist()
    check(f"{name}: every column documented in Codebook", [c for c in df.columns if c not in cb_headers])
    check(f"{name}: every Codebook header present in sheet", [c for c in cb_headers if c not in df.columns])
    # whitespace / case artefacts in text cells
    bad_ws = []
    for c in df.columns:
        if df[c].dtype == object:
            s = df[c].dropna().astype(str)
            bad_ws += [f"{c}:{v!r}" for v in s[s != s.str.strip()].unique()]
            bad_ws += [f"{c}:{v!r}" for v in s[s.str.contains(r"\s{2,}")].unique()]
    check(f"{name}: no leading/trailing/double spaces in text cells", bad_ws)

section("1. CATEGORICAL LABELS vs CODEBOOK ALLOWED VALUES")
for name, df in [("NeedsSurvey", ns), ("BatteryBus", bb), ("VishramSadan", vs)]:
    for c in df.columns:
        labs = allowed_labels(name, c)
        if labs is None:
            continue
        vals = df[c].dropna()
        if pd.api.types.is_numeric_dtype(vals):
            bad = sorted(set(vals.astype(int)) - set(labs.values()))
        else:
            bad = sorted(set(vals.astype(str)) - set(labs.keys()))
        check(f"{name}.{c}: values within Codebook set", bad)

section("2. NEEDSSURVEY RULES")
imp = ["PC1_LongTermFinancingBeyondScheme", "PC2_HighCostIllnessNotCovered", "PC3_NonAffordingNonScheme",
       "PC4_HomeCareHighCostEquipment", "INF1_AcademicResearchTertiaryCentres", "INF2_PublicFacilities",
       "INF3_HighCostMedicalEquipment", "INF4_HospitalSupportServices", "VAS1_ParkGreenSpaces",
       "VAS2_ExternalInterCampusTransport", "VAS3_AllWeatherWalkways", "VAS4_AirQualitySystems",
       "VAS5_InternalTransport", "RES1_ResearchChairs", "RES2_ShortTermTrainingGrants", "RES3_FellowshipResearch"]
sus = ["SUS_" + c for c in imp]
d = pd.to_datetime(ns.Response_Date)
check("Response_Date within 2021-03-01..2021-09-30", (d < "2021-03-01") | (d > "2021-09-30"), ns)
check("Response_Date never Sunday", d.dt.dayofweek == 6, ns)
check("Response_Date never 2021-08-15", d == "2021-08-15", ns)
for c in imp + sus:
    v = ns[c]
    check(f"{c}: integer 1-5, no blanks", v.isna() | ~v.isin([1, 2, 3, 4, 5]), ns)
age_band = {"<35": (0, 35), "36-45": (36, 45), "46-55": (46, 55), "56-65": (56, 65)}
lo = ns.Age_Band.map(lambda b: age_band[b][0]); hi = ns.Age_Band.map(lambda b: age_band[b][1])
check("Age_Years inside Age_Band (<35 read as <=35)", (ns.Age_Years < lo) | (ns.Age_Years > hi), ns)
sb = {"<5": (0, 5), "6-15": (6, 15), "16-25": (16, 25), ">25": (26, 99)}
lo = ns.Service_Band.map(lambda b: sb[b][0]); hi = ns.Service_Band.map(lambda b: sb[b][1])
check("Service_Years_AIIMS inside Service_Band (<5 read as <=5)", (ns.Service_Years_AIIMS < lo) | (ns.Service_Years_AIIMS > hi), ns)
desig_cat = {"Director": "Administration", "Deputy Director (Administration)": "Administration",
             "Medical Superintendent": "Administration", "Deputy Secretary": "Administration",
             "Chief Administrative Officer": "Administration", "Senior Financial Advisor": "Finance & Account Division",
             "Finance Adviser": "Finance & Account Division", "Finance & Accounts Officer": "Finance & Account Division",
             "Professor": "Clinical, Para clinical & Non-clinical Departments",
             "Additional Professor": "Clinical, Para clinical & Non-clinical Departments",
             "Associate Professor": "Clinical, Para clinical & Non-clinical Departments",
             "Assistant Professor": "Clinical, Para clinical & Non-clinical Departments",
             "Superintending Engineer": "Engineering Services Division", "Executive Engineer": "Engineering Services Division",
             "Assistant Engineer": "Engineering Services Division",
             "Medical Social Service Officer": "Medical Social Service Officers Unit"}
check("Department_Category fixed by Designation", ns.Designation.map(desig_cat) != ns.Department_Category, ns)
faculty = ns.Designation.isin(["Professor", "Additional Professor", "Associate Professor", "Assistant Professor"])
check("Faculty_Subgroup present for every faculty row", faculty & ns.Faculty_Subgroup.isna(), ns)
check("Faculty_Subgroup empty for every non-faculty row", ~faculty & ns.Faculty_Subgroup.notna(), ns)
dep_sub = ns[faculty].groupby("Department").Faculty_Subgroup.nunique()
check("each faculty Department maps to exactly one Faculty_Subgroup", dep_sub[dep_sub > 1].index.tolist())
check("non-faculty Department repeats its division name", ~faculty & (ns.Department != ns.Department_Category), ns)
exp_strat = np.where(ns.Faculty_Subgroup == "Clinical", "Faculty: clinical",
            np.where(ns.Faculty_Subgroup.isin(["Para-clinical", "Non-clinical"]), "Faculty: para/non-clinical",
            np.where(ns.Department_Category == "Medical Social Service Officers Unit", "Medical social service officers",
                     "Administrative, finance and engineering officials")))
check("Analytic_Stratum correctly derived", pd.Series(exp_strat != ns.Analytic_Stratum.values), ns)
top2 = ns[faculty & ns.Department.isin(["Medicine", "Surgery"]) & (ns.Faculty_Subgroup == "Clinical")]
cl_counts = ns[faculty & (ns.Faculty_Subgroup == "Clinical")].Department.value_counts()
check("Medicine and Surgery are the two largest clinical departments (ties allowed)",
      [] if set(cl_counts.index[:2]) == {"Medicine", "Surgery"} or cl_counts.iloc[1] == cl_counts.iloc[2] else list(cl_counts.index[:3]))
single = ["Director", "Deputy Director (Administration)", "Medical Superintendent", "Deputy Secretary",
          "Chief Administrative Officer", "Senior Financial Advisor", "Superintending Engineer", "Finance Adviser"]
vc = ns.Designation.value_counts()
check("single-holder posts held by at most one row", [p for p in single if vc.get(p, 0) > 1])
check("Education: faculty never 'Graduates'", faculty & (ns.Education == "Graduates"), ns)
doc_ok = faculty | ns.Designation.isin(["Director", "Deputy Director (Administration)", "Medical Superintendent"])
check("Education: Doctorate/DM/MCh only for faculty, Director, DDA, MS", (ns.Education == "Doctorate/DM/MCh") & ~doc_ok, ns)
check("Education: Doctorate holder aged >= 31", (ns.Education == "Doctorate/DM/MCh") & (ns.Age_Years < 31), ns)
check("Education: MSSO holds Post-graduates", (ns.Designation == "Medical Social Service Officer") & (ns.Education != "Post-graduates"), ns)
check("Age_Years 25-65", (ns.Age_Years < 25) | (ns.Age_Years > 65), ns)
check("no faculty row under 30", faculty & (ns.Age_Years < 30), ns)
check("Assistant Professor age <= 55", (ns.Designation == "Assistant Professor") & (ns.Age_Years > 55), ns)
check("Assistant Engineer age <= 50", (ns.Designation == "Assistant Engineer") & (ns.Age_Years > 50), ns)
check("MSSO age <= 60", (ns.Designation == "Medical Social Service Officer") & (ns.Age_Years > 60), ns)
check("non-faculty post other than Director held at <= 60", ~faculty & (ns.Designation != "Director") & (ns.Age_Years > 60), ns)
fac_ms = faculty | (ns.Designation == "Medical Superintendent")
check("Service <= Age-29 for faculty and MS", fac_ms & (ns.Service_Years_AIIMS > ns.Age_Years - 29), ns)
check("Service <= Age-23 for other posts", ~fac_ms & (ns.Service_Years_AIIMS > ns.Age_Years - 23), ns)
caps = {"Deputy Director (Administration)": 5, "Deputy Secretary": 4, "Senior Financial Advisor": 5,
        "Assistant Professor": 10, "Associate Professor": 18, "Additional Professor": 25,
        "Assistant Engineer": 15, "Finance & Accounts Officer": 30}
for post, cap in caps.items():
    check(f"Service cap: {post} <= {cap}", (ns.Designation == post) & (ns.Service_Years_AIIMS > cap), ns)
    n_post = (ns.Designation == post).sum()
    if n_post >= 20:
        share = ((ns.Designation == post) & (ns.Service_Years_AIIMS == cap)).sum() / n_post
        check(f"Service cap: {post} at most 12% exactly on cap (observed {share:.1%})", [] if share <= 0.12 else [post])
check("Service floor: Professor >= 8", (ns.Designation == "Professor") & (ns.Service_Years_AIIMS < 8), ns)
doc_cap = np.maximum(1, ns.Age_Years - 31)
check("Doctorate holder service <= max(1, Age-31)", (ns.Education == "Doctorate/DM/MCh") & (ns.Service_Years_AIIMS > doc_cap), ns)
check("Service_Years_AIIMS 1-37", (ns.Service_Years_AIIMS < 1) | (ns.Service_Years_AIIMS > 37), ns)
cm = ns.CSR_Committee_Member == "Yes"
must = ns.Designation.isin(["Deputy Director (Administration)", "Medical Superintendent", "Superintending Engineer", "Senior Financial Advisor"])
check("CSR committee: Yes for DDA, MS, SE, SFA", must & ~cm, ns)
check("CSR committee: at most 3 from Finance", [] if (cm & (ns.Department_Category == "Finance & Account Division")).sum() <= 3 else ["finance>3"])
other_ok = ns.Department_Category.isin(["Administration", "Finance & Account Division"]) | ns.Designation.isin(["Professor", "Additional Professor", "Superintending Engineer"])
check("CSR committee: outside Admin/Finance only Prof, Addl Prof, SE", cm & ~other_ok, ns)
check("Prior CSR involvement Yes for every committee member", cm & (ns.Prior_CSR_Project_Involvement != "Yes"), ns)
themes = {"Patient care": [c for c in imp if c.startswith("PC")], "Infrastructure": [c for c in imp if c.startswith("INF")],
          "Value added services": [c for c in imp if c.startswith("VAS")], "Research": [c for c in imp if c.startswith("RES")]}
tmean = pd.DataFrame({k: ns[v].mean(axis=1) for k, v in themes.items()})
chosen_mean = pd.Series([tmean.loc[i, t] for i, t in zip(ns.index, ns.Top_Theme_Choice)], index=ns.index)
none_ge3 = (tmean.max(axis=1) < 3)
top_rated_max = tmean.max(axis=1)
check("Top_Theme: chosen theme mean >= 3, or it is a top-rated theme when none reaches 3",
      ~((chosen_mean >= 3) | (none_ge3 & np.isclose(chosen_mean, top_rated_max))), ns)
is_top = pd.Series([np.isclose(tmean.loc[i, t], top_rated_max[i]) for i, t in zip(ns.index, ns.Top_Theme_Choice)], index=ns.index)
lines.append(f"    info: Top_Theme_Choice is (one of) the respondent's top-rated theme(s) in {is_top.mean():.1%} of rows (codebook: 'about 60%')")
check("Budget: Infrastructure choice never smallest band", (ns.Top_Theme_Choice == "Infrastructure") & (ns.Budget_Band_For_Top_Need == "Less than or equal to INR 1 Cr"), ns)
check("Budget: Value added services choice never largest band", (ns.Top_Theme_Choice == "Value added services") & (ns.Budget_Band_For_Top_Need == "More than 10 Cr"), ns)
check("Budget: chosen theme mean < 3 never largest band", (chosen_mean < 3) & (ns.Budget_Band_For_Top_Need == "More than 10 Cr"), ns)
check("Importance: 16 answers not all the same", ns[imp].nunique(axis=1) == 1, ns)
check("Importance: at most 10 answers of 5", (ns[imp] == 5).sum(axis=1) > 10, ns)
check("Importance: at least one answer below 4", (ns[imp] < 4).sum(axis=1) == 0, ns)
for k, v in themes.items():
    lim = 2 if k == "Patient care" else 3
    check(f"Importance: within-theme range <= {lim} ({k})", (ns[v].max(axis=1) - ns[v].min(axis=1)) > lim, ns)
check("Sustainability: >= 3 distinct answers across 16 items", ns[sus].nunique(axis=1) < 3, ns)
check("Sustainability: same code as importance on <= 11 of 16 needs", pd.Series((ns[imp].values == ns[sus].values).sum(axis=1) > 11, index=ns.index), ns)
check("Sustainability: each need's mean SUS < mean importance", [c for c in imp if ns["SUS_" + c].mean() >= ns[c].mean()])
check("Response_Mode/Gender/... no blanks", ns[["Response_Mode", "Gender", "Age_Band", "Education", "Service_Band", "Department_Category", "Analytic_Stratum", "Department", "Designation", "CSR_Committee_Member", "Prior_CSR_Project_Involvement", "Top_Theme_Choice", "Budget_Band_For_Top_Need"]].isna().any(axis=1), ns)

section("3. BATTERYBUS RULES")
holidays = pd.to_datetime(["2021-10-02", "2021-10-15", "2021-10-19", "2021-11-04", "2021-11-19", "2021-12-25", "2022-01-26"])
for name, df in [("BatteryBus", bb), ("VishramSadan", vs)]:
    d = pd.to_datetime(df.Interview_Date)
    check(f"{name}: Interview_Date within 2021-09-01..2022-02-28", (d < "2021-09-01") | (d > "2022-02-28"), df)
    check(f"{name}: Interview_Date never Sunday", d.dt.dayofweek == 6, df)
    check(f"{name}: Interview_Date never a listed gazetted holiday", d.isin(holidays), df)
    ab = {"<20": (15, 20), "21-30": (21, 30), "31-40": (31, 40), "41-50": (41, 50), "51-60": (51, 60),
          "61-70": (61, 70), "71-80": (71, 80), ">81": (81, 200), ">80": (81, 200)}
    lo = df.Age_Band.map(lambda b: ab[b][0]); hi = df.Age_Band.map(lambda b: ab[b][1])
    check(f"{name}: Age_Years inside Age_Band", (df.Age_Years < lo) | (df.Age_Years > hi), df)
    check(f"{name}: Age_Years 15-88", (df.Age_Years < 15) | (df.Age_Years > 88), df)
    check(f"{name}: Graduate >= 21 y", (df.Education == "Graduate") & (df.Age_Years < 21), df)
    check(f"{name}: Post Graduate >= 23 y", (df.Education == "Post Graduate") & (df.Age_Years < 23), df)
check("BatteryBus: Intermediate >= 17 y", (bb.Education == "Intermediate") & (bb.Age_Years < 17), bb)
bbi = [c for c in bb.columns if re.match(r"^BB\d+_", c)]
for c in bbi:
    v = bb[c]
    check(f"{c}: integer 0-5 where answered", v.notna() & ~v.isin([0, 1, 2, 3, 4, 5]), bb)
lines.append("    info: BatteryBus item blanks per item: " + str(bb[bbi].isna().sum().to_dict()))
lines.append("    info: BatteryBus blanks by group: " + str(bb[bb[bbi].isna().any(axis=1)].Group.value_counts().to_dict()))
check("Profile Student 17-30 y", (bb.Participant_Profile == "Student") & ((bb.Age_Years < 17) | (bb.Age_Years > 30)), bb)
check("Profile Student not <Primary/High School", (bb.Participant_Profile == "Student") & bb.Education.isin(["<Primary", "High School"]), bb)
check("Profile Resident 24-35 y", (bb.Participant_Profile == "Resident") & ((bb.Age_Years < 24) | (bb.Age_Years > 35)), bb)
check("Profile Resident Graduate or above", (bb.Participant_Profile == "Resident") & ~bb.Education.isin(["Graduate", "Post Graduate"]), bb)
check("Profile Staff 21-60 y", (bb.Participant_Profile == "Staff") & ((bb.Age_Years < 21) | (bb.Age_Years > 60)), bb)
em = bb.Centre_Visited == "Emergency"
lines.append(f"    info: Emergency rows that are New: {(bb[em].Visit_Type == 'New').sum()} of {em.sum()} (rule: >= 9 of 10)")
check("Emergency rows New in >= 90%", [] if (bb[em].Visit_Type == "New").mean() >= 0.9 else ["Emergency"])
mch = (bb.Centre_Visited == "MCH") & (bb.Participant_Profile == "Patient")
check("MCH patient is a woman aged 15-45", mch & ((bb.Gender != "Female") | (bb.Age_Years < 15) | (bb.Age_Years > 45)), bb)
check("Walking (to AIIMS) lives within 3 km", (bb.Transport_Mode_To_AIIMS == "Walking") & (bb.Distance_From_Home_km > 3), bb)
check("Staff/Resident/Student who walk <= 2 km", (bb.Transport_Mode_To_AIIMS == "Walking") & bb.Participant_Profile.isin(["Staff", "Resident", "Student"]) & (bb.Distance_From_Home_km > 2), bb)
check("Own Vehicle <= 400 km", (bb.Transport_Mode_To_AIIMS == "Own Vehicle") & (bb.Distance_From_Home_km > 400), bb)
check("Distance 0.3-1500 km", (bb.Distance_From_Home_km < 0.3) | (bb.Distance_From_Home_km > 1500), bb)
check("Test rows Bus_Trips >= 1", (bb.Group == "Test group") & (bb.Bus_Trips_Today < 1), bb)
check("Control rows Bus_Trips == 0", (bb.Group == "Control group") & (bb.Bus_Trips_Today != 0), bb)
check("IntraCampus_Trips >= 1 and >= Bus_Trips", (bb.IntraCampus_Trips_Today < 1) | (bb.IntraCampus_Trips_Today < bb.Bus_Trips_Today), bb)
check("IntraCampus_Trips 1-10", (bb.IntraCampus_Trips_Today > 10), bb)
all_bus = bb.IntraCampus_Trips_Today == bb.Bus_Trips_Today
check("Rs_Spent 0 only when Alt_Mode Walking or all movements by bus", (bb.Rs_Spent_IntraCampus_Today == 0) & ~((bb.Alt_Mode_Within_Campus == "Walking") | all_bus), bb)
check("Rs_Spent 0-500", (bb.Rs_Spent_IntraCampus_Today < 0) | (bb.Rs_Spent_IntraCampus_Today > 500), bb)
check("Walking alt / all-bus with Rs_Spent > 0", ((bb.Alt_Mode_Within_Campus == "Walking") | all_bus) & (bb.Rs_Spent_IntraCampus_Today > 0), bb)
check("Own vehicle alt only for a party that came by own vehicle", (bb.Alt_Mode_Within_Campus == "Own vehicle") & (bb.Transport_Mode_To_AIIMS != "Own Vehicle"), bb)
check("Rs_Saved present for Test, empty for Control", ((bb.Group == "Test group") & bb.Rs_Saved_Today_SelfEstimate.isna()) | ((bb.Group == "Control group") & bb.Rs_Saved_Today_SelfEstimate.notna()), bb)
rs = bb.Rs_Saved_Today_SelfEstimate
check("Rs_Saved multiple of 5", rs.notna() & (rs % 5 != 0), bb)
check("Rs_Saved 0 when alternative is walking", rs.notna() & (bb.Alt_Mode_Within_Campus == "Walking") & (rs != 0), bb)
fare_max = bb.Alt_Mode_Within_Campus.map({"Own vehicle": 30, "Auto or e-rickshaw": 50, "Cycle rickshaw": 30, "Walking": 0})
nonbus = bb.IntraCampus_Trips_Today - bb.Bus_Trips_Today
paid = (bb.Rs_Spent_IntraCampus_Today > 0) & (nonbus > 0)
fare_paid = (bb.Rs_Spent_IntraCampus_Today / nonbus.replace(0, np.nan))
cap_paid = np.floor((bb.Bus_Trips_Today * 2 * fare_paid) / 5) * 5
cap_unpaid = bb.Bus_Trips_Today * fare_max
check("Rs_Saved cap where a hop was paid (<= bus trips x 2 x fare per hop)", rs.notna() & paid & (rs > cap_paid + 1e-9), bb)
check("Rs_Saved cap where no hop was paid (<= one fare of alternative per bus trip)", rs.notna() & ~paid & (rs > cap_unpaid), bb)
check("Wait present for Test, empty for Control", ((bb.Group == "Test group") & bb.Wait_Minutes_For_Bus.isna()) | ((bb.Group == "Control group") & bb.Wait_Minutes_For_Bus.notna()), bb)
check("Wait 2-15 min", bb.Wait_Minutes_For_Bus.notna() & ((bb.Wait_Minutes_For_Bus < 2) | (bb.Wait_Minutes_For_Bus > 15)), bb)
check("User minutes > wait", (bb.Group == "Test group") & (bb.Minutes_IntraCampus_Today <= bb.Wait_Minutes_For_Bus), bb)
check("Staff income never <10,000", (bb.Participant_Profile == "Staff") & (bb.Household_Income_Band == "<10,000"), bb)
check("Resident income >= 25,001 when disclosed", (bb.Participant_Profile == "Resident") & bb.Household_Income_Band.isin(["<10,000", "10,000-25,000"]), bb)
lines.append("    info: max identical answers per respondent across BB1-BB10: " + str(int(bb[bbi].apply(lambda r: r.value_counts().max(), axis=1).max())) + " (rule: <= 7)")

section("4. VISHRAMSADAN RULES")
vsi = [c for c in vs.columns if re.match(r"^VS\d+_", c)]
for c in vsi:
    check(f"{c}: integer 0-5, no blanks", vs[c].isna() | ~vs[c].isin([0, 1, 2, 3, 4, 5]), vs)
T = vs.Group == "Test group"
check("Test = Power Grid Vishram Sadan; Control = other accommodation", (T & (vs.Accommodation_Type_Used != "Power Grid Vishram Sadan")) | (~T & (vs.Accommodation_Type_Used == "Power Grid Vishram Sadan")), vs)
check("Test Nights 1-14", T & ((vs.Nights_Stayed < 1) | (vs.Nights_Stayed > 14)), vs)
check("Nights 0 only for No overnight stay", ((vs.Nights_Stayed == 0) & (vs.Accommodation_Type_Used != "No overnight stay")) | ((vs.Nights_Stayed != 0) & (vs.Accommodation_Type_Used == "No overnight stay")), vs)
check("Similar_Facility 'Not applicable' for every Control row", ~T & (vs.Similar_Facility_Elsewhere != "Not applicable (control group)"), vs)
check("Similar_Facility answered Yes/No for every Test row", T & ~vs.Similar_Facility_Elsewhere.isin(["Yes", "No"]), vs)
code_map = {"Not applicable (control group)": 0, "Yes": 1, "No": 2}
check("Similar_Facility_Code equals label code", vs.Similar_Facility_Elsewhere.map(code_map) != vs.Similar_Facility_Code, vs)
acc = vs.Accommodation_Type_Used; L = vs.Lodging_Rs_Per_Night
check("VS tariff 150/300/380", (acc == "Power Grid Vishram Sadan") & ~L.isin([150, 300, 380]), vs)
check("Private lodge 500-1500 in steps of 50", (acc == "Private lodge or hotel") & ((L < 500) | (L > 1500) | (L % 50 != 0)), vs)
check("Private lodge <= 600 in lowest income band", (acc == "Private lodge or hotel") & (vs.Household_Income_Band == "<10,000") & (L > 600), vs)
check("Private lodge <= 900 in 10,000-25,000 band", (acc == "Private lodge or hotel") & (vs.Household_Income_Band == "10,000-25,000") & (L > 900), vs)
check("Other dharamshala 50-250", (acc == "Other dharamshala or NGO shelter") & ((L < 50) | (L > 250)), vs)
check("Lodging 0 for relatives / open area / no stay", acc.isin(["Relative's or friend's home", "Open area, footpath or hospital premises", "No overnight stay"]) & (L != 0), vs)
check("Food in tens of rupees", vs.Food_Rs_Per_Day % 10 != 0, vs)
check("Food 0-640", (vs.Food_Rs_Per_Day < 0) | (vs.Food_Rs_Per_Day > 640), vs)
check("Companions 0-3", ~vs.Companions_N.isin([0, 1, 2, 3]), vs)
check("Companions >= 1 for respondents under 18", (vs.Age_Years < 18) & (vs.Companions_N < 1), vs)
check("Companions >= 1 for patients at Trauma/Emergency/Burns", (vs.Participant_Profile == "Patient") & vs.Centre_Visited.isin(["Trauma Centre", "Emergency", "Burns & Plastic Block"]) & (vs.Companions_N < 1), vs)
check("No overnight stay distance <= 80 km", (acc == "No overnight stay") & (vs.Distance_From_Home_km > 80), vs)
check("Open area income never above 25,000", (acc == "Open area, footpath or hospital premises") & vs.Household_Income_Band.isin(["25,001-50,000", ">50,000"]), vs)
check("Open area never own vehicle", (acc == "Open area, footpath or hospital premises") & (vs.Transport_Stay_To_AIIMS == "Own Vehicle"), vs)
check("Shuttle only from a campus stay (facility or hospital premises)", (vs.Transport_Stay_To_AIIMS == "Shuttle by AIIMS") & ~acc.isin(["Power Grid Vishram Sadan", "Open area, footpath or hospital premises"]), vs)
check("Day-return walker within 3 km", (acc == "No overnight stay") & (vs.Transport_Stay_To_AIIMS == "Walking") & (vs.Distance_From_Home_km > 3), vs)
check("Day-return auto within 25 km", (acc == "No overnight stay") & (vs.Transport_Stay_To_AIIMS == "Auto Rickshaw") & (vs.Distance_From_Home_km > 25), vs)
check("Travel minutes 2-180", (vs.Travel_Minutes_Stay_To_AIIMS < 2) | (vs.Travel_Minutes_Stay_To_AIIMS > 180), vs)
mchp = (vs.Centre_Visited == "MCH Block") & (vs.Participant_Profile == "Patient")
check("MCH Block patient is a woman aged 15-45", mchp & ((vs.Gender != "female") | (vs.Age_Years < 15) | (vs.Age_Years > 45)), vs)
# These three rules are stated on the Codebook's 5-level reading of the published 0-5 scale
# (0 -> 1, 1 -> 2, 2 and 3 -> 3, 4 -> 4, 5 -> 5), so they are checked on that reading.
five = {0: 1, 1: 2, 2: 3, 3: 3, 4: 4, 5: 5}
v5 = vs[vsi].apply(lambda s: s.map(five))
check("VS1 and VS2 differ by <= 2 (5-level reading)", (v5.VS1_UsedAccommodation - v5.VS2_RoomAvailability).abs() > 2, vs)
check("VS6 and VS7 differ by <= 2 (5-level reading)", (v5.VS6_TravelTimeSaved - v5.VS7_LocationConnectivity).abs() > 2, vs)
check("VS4 at 1-2 never with VS5 at 4-5 (5-level reading)", v5.VS4_FoodAvailability.isin([1, 2]) & v5.VS5_FoodReasonablePrice.isin([4, 5]), vs)
lines.append(f"    info: on the PUBLISHED 0-5 scale, VS1/VS2 differ by >2 in {((vs.VS1_UsedAccommodation - vs.VS2_RoomAvailability).abs() > 2).sum()} rows, VS6/VS7 in {((vs.VS6_TravelTimeSaved - vs.VS7_LocationConnectivity).abs() > 2).sum()} rows")
check("No facility user gives 0 on VS1 or VS2", T & ((vs.VS1_UsedAccommodation == 0) | (vs.VS2_RoomAvailability == 0)), vs)
lines.append("    info: max identical answers per respondent across VS1-VS9: " + str(int(vs[vsi].apply(lambda r: r.value_counts().max(), axis=1).max())) + " (rule: <= 6)")
lines.append(f"    info: Similar_Facility_Code mean/SD over all 875 rows = {vs.Similar_Facility_Code.mean():.2f}/{vs.Similar_Facility_Code.std():.2f}; Yes among Test = {(vs.Similar_Facility_Elsewhere=='Yes').sum()}/{T.sum()}")

section("5. PROJECTS SHEET")
check("Project_ID P01-P08 unique", pj.Project_ID.duplicated(), pj, id_col="Project_ID")
rep = pj.Representative_Project == "Yes"
lines.append("    info: representative projects: " + ", ".join(pj.loc[rep, "Project_ID"] + " " + pj.loc[rep, "Project_Name"]))
calc = 100 * pj.Annual_Cost_To_AIIMS_Rs / pj.AIIMS_Electricity_Bill_Rs_FY2021_22
trunc = np.floor(calc * 1000) / 1000
check("Cost % of electricity bill = 100*cost/bill truncated to 3 dp", rep & ~np.isclose(trunc, pj.Cost_To_AIIMS_Pct_Electricity_Bill), pj, id_col="Project_ID")
bus = pj.Project_ID == "P08"
elec = float(pj.loc[bus, "Vehicles_N"].iloc[0] * pj.loc[bus, "Power_Units_Per_Vehicle_Per_Day"].iloc[0] * pj.loc[bus, "Electricity_Rs_Per_Unit"].iloc[0] * pj.loc[bus, "Operating_Days_Per_Year"].iloc[0])
lines.append(f"    info: bus electricity = 10 x 10.8 kWh x Rs 8.50 x 300 days = Rs {elec:,.0f} (sheet Annual_Cost_To_AIIMS_Rs = {pj.loc[bus,'Annual_Cost_To_AIIMS_Rs'].iloc[0]:,.0f})")
check("Bus annual cost equals Table 6 electricity arithmetic", [] if np.isclose(elec, pj.loc[bus, "Annual_Cost_To_AIIMS_Rs"].iloc[0]) else ["P08"])
cap = int(pj.loc[bus, "Vehicles_N"].iloc[0] * pj.loc[bus, "Rounds_Per_Vehicle_Per_Day"].iloc[0] * pj.loc[bus, "Seats_Per_Vehicle"].iloc[0] * pj.loc[bus, "Operating_Days_Per_Year"].iloc[0])
lines.append(f"    info: bus seat-trip capacity = {cap:,} per year (codebook: 546,000)")
occ = [99, 187, 254, 724, 619, 489, 451]
lines.append(f"    info: Vishram Sadan monthly occupancy sum = {sum(occ)} (sheet {int(pj.loc[pj.Project_ID=='P01','Total_Occupancy_Jun_Dec_2021'].iloc[0])})")

lines.insert(0, f"STEP 4a VALIDATION REPORT  |  source: {RAW.name}  |  rules failed: {n_fail}")
OUT.write_text("\n".join(lines) + "\n")
print("\n".join(lines))
