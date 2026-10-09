# Step 7 (part 1): feasibility tests for candidate advanced analyses, run before choosing which to report.
# Each block fits the model and records diagnostics that decide feasibility (convergence, fit, sparse cells,
# class sizes, overlap, calibration). Nothing here is reported as a result; it decides what Step 8 runs.
source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), "fun_helpers.R"))
suppressPackageStartupMessages({ library(lavaan); library(mirt); library(poLCA); library(mclust); library(grf); library(cobalt); library(ordinal); library(nnet); library(qgraph) })
ns <- D$NeedsSurvey; bb <- D$BatteryBus; vs <- D$VishramSadan; it <- D$items
out <- list(); add <- function(k, ...) { out[[k]] <<- paste(...); cat(sprintf("%-34s %s\n", k, paste(...))) }

# ---- 1. Ordinal CFA of the 4-theme structure (importance and sustainability)
th <- it$themes; ren <- function(x) sub("_.*", "", x)
I <- ns[, it$imp]; names(I) <- ren(names(I)); S <- ns[, it$sus]; names(S) <- paste0("S", ren(sub("^SUS_", "", names(S))))
add("cat counts (importance, min per item)", paste(sapply(I, function(x) min(table(factor(x, 1:5)))), collapse = ","))
mod4 <- paste0("PC =~ PC1 + PC2 + PC3 + PC4\nINF =~ INF1 + INF2 + INF3 + INF4\nVAS =~ VAS1 + VAS2 + VAS3 + VAS4 + VAS5\nRES =~ RES1 + RES2 + RES3")
f4 <- cfa(mod4, data = I, ordered = names(I), estimator = "WLSMV"); fm <- fitMeasures(f4, c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
add("CFA4 importance", "converged", lavInspect(f4, "converged"), "CFI", f3(fm[1]), "TLI", f3(fm[2]), "RMSEA", f3(fm[3]), "SRMR", f3(fm[4]))
fit1 <- cfa(paste("G =~", paste(names(I), collapse = " + ")), data = I, ordered = names(I), estimator = "WLSMV"); fm1 <- fitMeasures(fit1, c("cfi.scaled", "rmsea.scaled", "srmr"))
add("CFA1 importance", "CFI", f3(fm1[1]), "RMSEA", f3(fm1[2]), "SRMR", f3(fm1[3]))
modS <- gsub("([A-Z]+[0-9])", "S\\1", mod4); modS <- gsub("S(PC|INF|VAS|RES) =~", "\\1 =~", modS)
fS <- cfa(modS, data = S, ordered = names(S), estimator = "WLSMV"); fmS <- fitMeasures(fS, c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"))
add("CFA4 sustainability", "converged", lavInspect(fS, "converged"), "CFI", f3(fmS[1]), "TLI", f3(fmS[2]), "RMSEA", f3(fmS[3]), "SRMR", f3(fmS[4]))
# invariance clinical vs para/non-clinical faculty (collapse 1-2 because category 1 is sparse)
fac <- ns$Faculty == "Yes"; Ic <- as.data.frame(lapply(I[fac, ], function(x) pmax(x, 2))); Ic$grp <- droplevels(ns$Analytic_Stratum_std[fac])
cfg <- cfa(mod4, data = Ic, ordered = names(I), estimator = "WLSMV", group = "grp")
sc <- try(cfa(mod4, data = Ic, ordered = names(I), estimator = "WLSMV", group = "grp", group.equal = c("loadings", "thresholds")), silent = TRUE)
add("MG-CFA faculty strata", "configural CFI", f3(fitMeasures(cfg, "cfi.scaled")), if (!inherits(sc, "try-error")) paste("scalar CFI", f3(fitMeasures(sc, "cfi.scaled"))) else "scalar failed")

# ---- 2. Graded response model (IRT)
g <- mirt(I, 1, itemtype = "graded", verbose = FALSE); add("GRM 1-factor importance", "converged", extract.mirt(g, "converged"), "M2 RMSEA", tryCatch(f3(M2(g, type = "C2")$RMSEA), error = function(e) "n/a"))
g4 <- try(mirt(I, mirt.model("PC = 1-4\nINF = 5-8\nVAS = 9-13\nRES = 14-16\nCOV = PC*INF*VAS*RES"), itemtype = "graded", verbose = FALSE, method = "MHRM"), silent = TRUE)
add("GRM 4-dim (MHRM)", if (inherits(g4, "try-error")) "failed" else paste("converged", extract.mirt(g4, "converged")))

# ---- 3. Latent class / profile analysis of need priorities
A <- as.data.frame(lapply(I, function(x) as.integer(x >= 4) + 1L))
f <- as.formula(paste("cbind(", paste(names(A), collapse = ","), ") ~ 1"))
lca <- lapply(1:5, function(k) { set.seed(1); poLCA(f, A, nclass = k, nrep = 5, verbose = FALSE, maxiter = 3000) })
add("LCA (agree flags) BIC 1-5", paste(sapply(lca, function(m) f1(m$bic)), collapse = " / "), "| min class share k=3:", f3(min(lca[[3]]$P)), "k=4:", f3(min(lca[[4]]$P)))
T4 <- ns[, paste0("Theme_", names(th))]; mc <- Mclust(T4, G = 1:6, verbose = FALSE)
add("LPA (theme scores) mclust", "best", mc$modelName, "G =", mc$G, "| BIC", f1(max(mc$BIC, na.rm = TRUE)), "| min class share", f3(min(table(mc$classification)) / nrow(T4)))

# ---- 4. Network model of the 16 needs
pc <- cor(I, method = "spearman"); gg <- try(EBICglasso(pc, n = nrow(I), gamma = 0.5), silent = TRUE)
add("EBICglasso network", if (inherits(gg, "try-error")) "failed" else paste("edges", sum(gg[upper.tri(gg)] != 0), "of 120"))

# ---- 5. Top theme: multinomial model and correspondence analysis
mm <- multinom(Top_Theme_Choice_std ~ Analytic_Stratum_std + Gender_std + Service_Band_std + Theme_PatientCare + Theme_Infrastructure + Theme_ValueAdded + Theme_Research, data = ns, trace = FALSE)
add("Multinomial top theme", "n", nrow(ns), "params", length(coef(mm)), "events per param", f1(nrow(ns) / length(coef(mm))))

# ---- 6. Propensity-score weighting and causal forests for the impact surveys
ps_block <- function(df, out_var, nm) {
  d <- df %>% mutate(T = as.integer(Group == "Test group"), y = .data[[out_var]]) %>%
    select(T, y, Gender_std, Age_Years, Education_std, Participant_Profile_std, Visit_Type_std, Distance_Class, Household_Income_Band_std, Centre_Visited_std) %>% na.omit()
  m <- glm(T ~ . - y, data = d, family = binomial); ps <- fitted(m)
  add(paste0(nm, " PS overlap"), "PS range users", f3(min(ps[d$T == 1])), "-", f3(max(ps[d$T == 1])), "non-users", f3(min(ps[d$T == 0])), "-", f3(max(ps[d$T == 0])),
      "| share PS<0.05 or >0.95:", f3(mean(ps < 0.05 | ps > 0.95)))
  w <- ifelse(d$T == 1, 1 - ps, ps)   # overlap weights
  bt <- bal.tab(T ~ Gender_std + Age_Years + Education_std + Participant_Profile_std + Visit_Type_std + Distance_Class + Household_Income_Band_std + Centre_Visited_std, data = d, weights = w, s.d.denom = "pooled", binary = "std")
  add(paste0(nm, " max |SMD| before/after"), f3(max(abs(bt$Balance$Diff.Un))), "/", f3(max(abs(bt$Balance$Diff.Adj))))
  X <- model.matrix(~ Gender_std + Age_Years + Education_std + Participant_Profile_std + Visit_Type_std + Distance_Class + Household_Income_Band_std + Centre_Visited_std, d)[, -1]
  set.seed(20261009); cf <- causal_forest(X, d$y, d$T, num.trees = 2000)
  tc <- test_calibration(cf); ate <- average_treatment_effect(cf, target.sample = "overlap")
  add(paste0(nm, " causal forest"), "overlap ATE", f2(ate[1]), "SE", f3(ate[2]), "| calibration: mean.forest p", fp(tc[1, 4]), "differential p", fp(tc[2, 4]))
}
ps_block(bb, "BB_Overall_Mean", "BUS"); ps_block(vs, "VS_Overall_Mean", "VS")

# ---- 7. Ordinal regression of satisfaction (proportional odds check)
ob <- clm(factor(BB9_Satisfaction_i, ordered = TRUE) ~ Group_std + Gender_std + Age_Years + Participant_Profile_std + Visit_Type_std + Distance_Class, data = bb)
nt <- try(nominal_test(ob), silent = TRUE)
add("Ordinal satisfaction (bus)", "converged", ob$convergence$code == 0, "| PO test min p", if (inherits(nt, "try-error")) "failed" else fp(min(nt$`Pr(>Chi)`, na.rm = TRUE)))
ov <- clm(factor(VS8_Satisfaction, ordered = TRUE) ~ Group_std + Gender_std + Age_Years + Participant_Profile_std + Visit_Type_std + Distance_Class, data = vs)
nt2 <- try(nominal_test(ov), silent = TRUE)
add("Ordinal satisfaction (VS)", "converged", ov$convergence$code == 0, "| PO test min p", if (inherits(nt2, "try-error")) "failed" else fp(min(nt2$`Pr(>Chi)`, na.rm = TRUE)))

# ---- 8. CFA of the impact items
mb <- paste("F =~", paste(paste0(it$bb, "_i"), collapse = " + ")); fb <- cfa(mb, data = bb, ordered = paste0(it$bb, "_i"), estimator = "WLSMV")
add("CFA1 bus items", "CFI", f3(fitMeasures(fb, "cfi.scaled")), "RMSEA", f3(fitMeasures(fb, "rmsea.scaled")))
mv <- paste("F =~", paste(it$vs, collapse = " + ")); fv <- cfa(mv, data = vs, ordered = it$vs, estimator = "WLSMV")
add("CFA1 VS items", "CFI", f3(fitMeasures(fv, "cfi.scaled")), "RMSEA", f3(fitMeasures(fv, "rmsea.scaled")))

# ---- 9. Cross-classified mixed model of importance vs sustainability
long <- bind_rows(lapply(it$imp, function(i) data.frame(id = rep(ns$Resp_ID, 2), need = sub("_.*", "", i), theme = sub("[0-9]+$", "", sub("_.*", "", i)),
                                                        rating = c(ns[[i]], ns[[paste0("SUS_", i)]]), type = rep(c("Importance", "Sustainability"), each = nrow(ns)))))
lm1 <- try(lme4::lmer(rating ~ type * theme + (1 + type | id) + (1 | need), data = long), silent = TRUE)
add("Cross-classified LMM", if (inherits(lm1, "try-error")) "failed" else paste("singular", lme4::isSingular(lm1), "| obs", nrow(long)))
saveRDS(out, file.path(OUT, "registry", "advanced_feasibility.rds"))
