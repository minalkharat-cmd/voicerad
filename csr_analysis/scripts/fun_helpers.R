# Shared helpers for Steps 6-10: statistics, formatting, figure theme/saving, and the results registry.
# Every table is registered with its title, data frame, notes, figure file and a 5-sentence write-up built from the
# computed numbers (no hand-typed values), so the final text file cannot drift from the analysis.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(scales); library(forcats); library(stringr)
})
select <- dplyr::select; filter <- dplyr::filter

ROOT <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
RUN_DATE <- "2026-10-09"
OUT <- file.path(ROOT, "output", RUN_DATE)
for (d in c("tables", "figures", "registry")) dir.create(file.path(OUT, d), recursive = TRUE, showWarnings = FALSE)
D <- readRDS(file.path(ROOT, "clean", "csr_clean.rds"))
set.seed(20261009)

# ------------------------------------------------------------------------------------------------- formatting
nz <- function(s) gsub("(?<![0-9])-(0\\.0+)(?![0-9])", "\\1", s, perl = TRUE)   # "-0.00" -> "0.00"
f1 <- function(x) nz(formatC(x, format = "f", digits = 1))
f2 <- function(x) nz(formatC(x, format = "f", digits = 2))
f3 <- function(x) nz(formatC(x, format = "f", digits = 3))
fp <- function(p) ifelse(is.na(p), "-", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
fp_text <- function(p) ifelse(p < 0.001, "p < 0.001", paste0("p = ", formatC(p, format = "f", digits = 3)))
npct <- function(n, N, d = 1) paste0(n, " (", formatC(100 * n / N, format = "f", digits = d), ")")
pct <- function(n, N) 100 * n / N
ntxt <- function(n, N, d = 1) paste0(n, " (", formatC(100 * n / N, format = "f", digits = d), "%)")      # for prose: "337 (66.1%)"
ntxt2 <- function(n, N, d = 1) paste0("n = ", n, ", ", formatC(100 * n / N, format = "f", digits = d), "%")  # inside brackets
f2s <- function(x) sub("^-(0\\.0+)$", "\\1", f2(x))                                                  # no "-0.00"
msd <- function(x) paste0(f2(mean(x, na.rm = TRUE)), " (", f2(sd(x, na.rm = TRUE)), ")")
miqr <- function(x, d = 1) { q <- quantile(x, c(.5, .25, .75), na.rm = TRUE, names = FALSE)
  paste0(formatC(q[1], format = "f", digits = d), " (", formatC(q[2], format = "f", digits = d), "-", formatC(q[3], format = "f", digits = d), ")") }
wilson <- function(x, n, z = 1.959964) {
  p <- x / n; den <- 1 + z^2 / n; ctr <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  c(est = 100 * p, lo = 100 * (ctr - hw), hi = 100 * (ctr + hw))
}
fci <- function(est, lo, hi, d = 1) nz(paste0(formatC(est, format = "f", digits = d), " (", formatC(lo, format = "f", digits = d), " to ", formatC(hi, format = "f", digits = d), ")"))

# ------------------------------------------------------------------------------------------------- tests
# Chi-square with automatic exact fallback (Cochran's rule): >20% expected < 5 or any expected < 1.
cat_test <- function(x, y, B = 1e5) {
  tb <- table(x, y); tb <- tb[rowSums(tb) > 0, colSums(tb) > 0, drop = FALSE]
  if (min(dim(tb)) < 2) return(list(test = "-", stat = NA, df = NA, p = NA, V = NA, label = "-"))
  ct <- suppressWarnings(chisq.test(tb, correct = FALSE))
  ex <- ct$expected; small <- mean(ex < 5) > 0.2 || min(ex) < 1
  V <- sqrt(unname(ct$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
  if (!small) return(list(test = "Chi-square", stat = unname(ct$statistic), df = unname(ct$parameter), p = ct$p.value, V = V,
                          label = paste0("chi2(", ct$parameter, ") = ", f2(ct$statistic), ", ", fp_text(ct$p.value))))
  if (all(dim(tb) == 2)) { ft <- fisher.test(tb)
    return(list(test = "Fisher exact", stat = unname(ct$statistic), df = 1, p = ft$p.value, V = V, label = paste0("Fisher exact ", fp_text(ft$p.value)))) }
  set.seed(20261009); mc <- suppressWarnings(chisq.test(tb, simulate.p.value = TRUE, B = B))
  list(test = "Monte-Carlo exact chi-square", stat = unname(ct$statistic), df = unname(ct$parameter), p = mc$p.value, V = V,
       label = paste0("chi2 = ", f2(ct$statistic), ", Monte-Carlo exact ", fp_text(mc$p.value)))
}
two_group <- function(y, g) {
  g <- droplevels(as.factor(g)); lv <- levels(g); a <- y[g == lv[1] & !is.na(y)]; b <- y[g == lv[2] & !is.na(y)]
  tt <- t.test(a, b, var.equal = FALSE); st <- t.test(a, b, var.equal = TRUE)
  wt <- suppressWarnings(wilcox.test(a, b, exact = FALSE, correct = TRUE))
  n1 <- length(a); n2 <- length(b); sp <- sqrt(((n1 - 1) * var(a) + (n2 - 1) * var(b)) / (n1 + n2 - 2))
  d <- (mean(a) - mean(b)) / sp; se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
  U <- unname(wt$statistic); rrb <- 2 * U / (n1 * n2) - 1
  list(n1 = n1, n2 = n2, m1 = mean(a), m2 = mean(b), sd1 = sd(a), sd2 = sd(b), diff = mean(a) - mean(b),
       lo = tt$conf.int[1], hi = tt$conf.int[2], t = unname(tt$statistic), df = unname(tt$parameter), p_t = tt$p.value,
       p_student = st$p.value, t_student = unname(st$statistic), U = U, p_mw = wt$p.value, d = d, d_lo = d - 1.96 * se_d, d_hi = d + 1.96 * se_d,
       r_rb = rrb, md1 = median(a), md2 = median(b))
}
k_group <- function(y, g) {
  g <- droplevels(as.factor(g)); ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- g[ok]
  w <- oneway.test(y ~ g, var.equal = FALSE); a <- summary(aov(y ~ g))[[1]]
  eta2 <- a$`Sum Sq`[1] / sum(a$`Sum Sq`); kw <- kruskal.test(y ~ g)
  n <- length(y); eps2 <- unname(kw$statistic) / (n - 1)
  list(F = unname(w$statistic), df1 = unname(w$parameter[1]), df2 = unname(w$parameter[2]), p_welch = w$p.value,
       F_classic = a$`F value`[1], p_classic = a$`Pr(>F)`[1], eta2 = eta2, H = unname(kw$statistic), p_kw = kw$p.value, eps2 = eps2)
}
paired_test <- function(x, y) {
  ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]; dlt <- x - y
  tt <- t.test(x, y, paired = TRUE); wt <- suppressWarnings(wilcox.test(x, y, paired = TRUE, exact = FALSE))
  list(n = length(x), m1 = mean(x), m2 = mean(y), diff = mean(dlt), lo = tt$conf.int[1], hi = tt$conf.int[2],
       t = unname(tt$statistic), df = unname(tt$parameter), p_t = tt$p.value, V = unname(wt$statistic), p_w = wt$p.value,
       dz = mean(dlt) / sd(dlt), n_pos = sum(dlt > 0), n_zero = sum(dlt == 0), n_neg = sum(dlt < 0))
}
cronbach <- function(df) {
  x <- as.matrix(df); x <- x[complete.cases(x), , drop = FALSE]; k <- ncol(x)
  a <- function(m) { k <- ncol(m); k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }
  alpha <- a(x)
  item_rest <- sapply(seq_len(k), function(j) cor(x[, j], rowSums(x[, -j, drop = FALSE])))
  if_del <- sapply(seq_len(k), function(j) if (k > 2) a(x[, -j, drop = FALSE]) else NA)
  # Feldt 95% CI for alpha
  n <- nrow(x); fl <- qf(c(.975, .025), n - 1, (n - 1) * (k - 1))
  list(alpha = alpha, lo = 1 - (1 - alpha) * fl[1], hi = 1 - (1 - alpha) * fl[2], n = n, k = k,
       item_rest = setNames(item_rest, colnames(x)), if_deleted = setNames(if_del, colnames(x)))
}
holm <- function(p) p.adjust(p, "holm")
or_rd <- function(a1, n1, a2, n2) {  # a = events, n = totals; group 1 vs group 2
  p1 <- a1 / n1; p2 <- a2 / n2; rd <- 100 * (p1 - p2); se_rd <- 100 * sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  aa <- a1 + .5 * (a1 == 0 | a1 == n1 | a2 == 0 | a2 == n2); bb <- n1 - a1 + .5 * (a1 == 0 | a1 == n1 | a2 == 0 | a2 == n2)
  cc <- a2 + .5 * (a1 == 0 | a1 == n1 | a2 == 0 | a2 == n2); dd <- n2 - a2 + .5 * (a1 == 0 | a1 == n1 | a2 == 0 | a2 == n2)
  lor <- log(aa * dd / (bb * cc)); se <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
  c(rd = rd, rd_lo = rd - 1.96 * se_rd, rd_hi = rd + 1.96 * se_rd, or = exp(lor), or_lo = exp(lor - 1.96 * se), or_hi = exp(lor + 1.96 * se))
}

# ------------------------------------------------------------------------------------------------- figure style
PAL <- list(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100", magenta = "#e87ba4",
            green = "#008300", violet = "#4a3aa7", red = "#e34948",
            ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781", grid = "#e1e0d9", axis = "#c3c2b7", neutral = "#bdbcb5")
CAT <- c(PAL$blue, PAL$orange, PAL$aqua, PAL$yellow, PAL$magenta, PAL$green, PAL$violet, PAL$red)
GROUP_COL <- c("Test (users)" = PAL$blue, "Control (non-users)" = PAL$orange)
LIK5 <- c("#a52b2b", "#ec8f8e", PAL$neutral, "#86b6ef", "#1c5cab")              # 1..5 with neutral midpoint
LIK6 <- c("#a52b2b", "#e34948", "#ec8f8e", "#86b6ef", "#3987e5", "#1c5cab")    # 0..5, no midpoint
SEQ <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
FONT <- "Liberation Sans"

theme_pub <- function(base = 9) {
  theme_minimal(base_size = base, base_family = FONT) %+replace%
    theme(text = element_text(colour = PAL$ink), axis.text = element_text(colour = PAL$ink2, size = rel(0.92)),
          axis.title = element_text(colour = PAL$ink2, size = rel(0.95)),
          plot.title = element_text(face = "bold", size = rel(1.12), hjust = 0, margin = margin(b = 4)),
          plot.subtitle = element_text(colour = PAL$ink2, size = rel(0.92), hjust = 0, margin = margin(b = 6)),
          plot.caption = element_text(colour = PAL$muted, size = rel(0.78), hjust = 0, margin = margin(t = 6)),
          plot.title.position = "plot", plot.caption.position = "plot",
          panel.grid.major = element_line(colour = PAL$grid, linewidth = 0.3), panel.grid.minor = element_blank(),
          axis.line = element_blank(), axis.ticks = element_blank(),
          legend.position = "top", legend.justification = "left", legend.location = "plot", legend.title = element_blank(),
          legend.text = element_text(size = rel(0.9), colour = PAL$ink2), legend.key.size = unit(9, "pt"),
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(4, "pt"),
          strip.text = element_text(face = "bold", colour = PAL$ink, size = rel(0.95), hjust = 0, margin = margin(b = 3, t = 3)),
          plot.background = element_rect(fill = "white", colour = NA), panel.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(8, 16, 6, 8)) +
    theme(axis.ticks = element_blank(), axis.ticks.length = unit(0, "pt"))
}
save_fig <- function(p, file, w = 7, h = 4.5) {
  # R 4.6.1's own Cairo devices (the R 4.5-built ragg device is not graphics-API compatible with R 4.6).
  png <- file.path(OUT, "figures", paste0(file, ".png")); pdf <- file.path(OUT, "figures", paste0(file, ".pdf"))
  if (inherits(p, "ggplot")) {  # wrap long subtitles/captions to the figure width so nothing is clipped
    if (!is.null(p$labels$subtitle)) p$labels$subtitle <- str_wrap(gsub("\n", " ", p$labels$subtitle), floor(w * 13.5))
    if (!is.null(p$labels$caption)) p$labels$caption <- str_wrap(gsub("\n", " ", p$labels$caption), floor(w * 16))
    if (!is.null(p$labels$title)) p$labels$title <- str_wrap(p$labels$title, floor(w * 11))
  }
  grDevices::png(png, width = w, height = h, units = "in", res = 600, type = "cairo", bg = "white"); print(p); invisible(dev.off())
  grDevices::cairo_pdf(pdf, width = w, height = h, bg = "white"); print(p); invisible(dev.off())
  invisible(png)
}

# Diverging stacked bar for Likert-type items, drawn as explicit segments so the layout is deterministic.
# df: item (factor; first level plotted at the top), level (factor ordered low -> high), pct; optional facet column.
# mid: the neutral level (NULL when the scale has no midpoint). Disagree side extends left of 0, agree side right;
# a neutral level is split across 0. End labels give the summed % on each side.
likert_plot <- function(df, cols, mid = NULL, title = NULL, subtitle = NULL, caption = NULL, facet = NULL,
                        xlab = "% of respondents", lw = 4.2, label_sides = TRUE, gap = 0.35) {
  lv <- levels(df$level); k <- length(lv)
  if (is.null(mid)) { neg <- lv[1:(k / 2)]; pos <- lv[(k / 2 + 1):k] } else { m <- which(lv == mid); neg <- lv[seq_len(m - 1)]; pos <- lv[(m + 1):k] }
  df$facet_ <- if (is.null(facet)) "all" else df[[facet]]
  segs <- df %>% group_by(item, facet_) %>% group_modify(function(g, key) {
    h <- if (is.null(mid)) 0 else sum(g$pct[g$level == mid]) / 2
    out <- list(); st <- h
    for (l in pos) { v <- sum(g$pct[g$level == l]); out[[length(out) + 1]] <- data.frame(level = l, xmin = st, xmax = st + v); st <- st + v }
    st <- -h
    for (l in rev(neg)) { v <- sum(g$pct[g$level == l]); out[[length(out) + 1]] <- data.frame(level = l, xmin = st - v, xmax = st); st <- st - v }
    if (!is.null(mid)) out[[length(out) + 1]] <- data.frame(level = mid, xmin = -h, xmax = h)
    bind_rows(out)
  }) %>% ungroup() %>% filter(xmax - xmin > 0) %>%
    mutate(x0 = ifelse(xmax - xmin > 2 * gap, xmin + gap, xmin), x1 = ifelse(xmax - xmin > 2 * gap, xmax - gap, xmax),
           level = factor(level, levels = lv))
  sides <- df %>% group_by(item, facet_) %>% summarise(L = sum(pct[level %in% neg]), R = sum(pct[level %in% pos]),
                                                       M = if (is.null(mid)) 0 else sum(pct[level == mid]), .groups = "drop")
  limL <- ceiling((max(sides$L + sides$M / 2) + 12) / 10) * 10; limR <- ceiling((max(sides$R + sides$M / 2) + 12) / 10) * 10
  if (!is.null(caption)) caption <- str_wrap(caption, 120)
  p <- ggplot(segs) +
    geom_segment(aes(x = x0, xend = x1, y = item, yend = item, colour = level), linewidth = lw, lineend = "butt") +
    geom_vline(xintercept = 0, colour = PAL$ink2, linewidth = 0.3) +
    scale_colour_manual(values = setNames(cols, lv), breaks = lv, drop = FALSE) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(limits = c(-limL, limR), breaks = seq(-min(100, floor(limL / 20) * 20), min(100, floor(limR / 20) * 20), 20), labels = function(x) paste0(abs(x))) +
    labs(x = xlab, y = NULL, title = title, subtitle = subtitle, caption = caption) + theme_pub() +
    theme(panel.grid.major.y = element_blank()) +
    guides(colour = guide_legend(nrow = if (k > 5) 2 else 1, byrow = TRUE, override.aes = list(linewidth = 3.5)))
  if (label_sides) p <- p +
    geom_text(data = sides, aes(x = -(L + M / 2) - 1.2, y = item, label = paste0(f1(L), "%")), hjust = 1, size = 2.5, colour = PAL$ink2, family = FONT) +
    geom_text(data = sides, aes(x = R + M / 2 + 1.2, y = item, label = paste0(f1(R), "%")), hjust = 0, size = 2.5, colour = PAL$ink2, family = FONT)
  if (!is.null(facet)) p <- p + facet_grid(facet_ ~ ., scales = "free_y", space = "free_y") + theme(strip.text.y = element_text(angle = 0, hjust = 0))
  p
}

# ------------------------------------------------------------------------------------------------- registry
REG <- list()
reg <- function(id, section, title, table, notes = character(), figure = NA, writeup = character(), extra = NULL) {
  stopifnot(length(writeup) == 5 || length(writeup) == 0)
  REG[[id]] <<- list(id = id, section = section, title = title, table = table, notes = notes, figure = figure, writeup = writeup, extra = extra)
  if (is.data.frame(table)) write.csv(table, file.path(OUT, "tables", paste0(id, ".csv")), row.names = FALSE, na = "")
  else for (j in seq_along(table)) write.csv(table[[j]], file.path(OUT, "tables", paste0(id, "_", letters[j], ".csv")), row.names = FALSE, na = "")
  invisible(NULL)
}
save_registry <- function(name) saveRDS(REG, file.path(OUT, "registry", paste0(name, ".rds")))
