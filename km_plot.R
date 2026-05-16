library(survival)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(grDevices)
library(scales)
library(cowplot)

adtte <- read_csv("data/adtte.csv", show_col_types = FALSE)

param <- "TTDEATH"
param_label <- "Time to Death"

adtte_sub <- adtte %>%
  filter(PARAMCD == param) %>%
  mutate(
    TRT01P = factor(TRT01P, levels = c("Placebo", "Low Dose", "High Dose")),
    event = ifelse(CNSR == 0, 1L, 0L)
  )

study_label <- unique(adtte_sub$STUDYID)

fit <- survfit(Surv(AVAL, event) ~ TRT01P, data = adtte_sub, conf.type = "log-log")

# Log-rank test
lr <- survdiff(Surv(AVAL, event) ~ TRT01P, data = adtte_sub)
pval <- pchisq(lr$chisq, df = length(lr$n) - 1, lower.tail = FALSE)
pval_fmt <- ifelse(pval < 0.0001, "p < 0.0001", sprintf("p = %.4f", pval))

# Extract survival data at event times for each stratum
sf <- summary(fit, times = seq(0, 365, by = 1))
strata_names <- names(sf$strata)
strata_levels <- levels(adtte_sub$TRT01P)

plot_data <- data.frame(
  time = sf$time,
  surv = sf$surv,
  lower = sf$lower,
  upper = sf$upper,
  strata = factor(sf$strata, labels = strata_levels)
)

# Censoring: points where step flattens (surv doesn't change)
censor_data <- plot_data %>%
  group_by(strata) %>%
  filter(time > 0, abs(surv - lag(surv, default = 1)) < .Machine$double.eps) %>%
  ungroup()

at_risk_times <- seq(0, 360, by = 60)

sf_all <- summary(fit, times = at_risk_times)
strata_levels <- levels(adtte_sub$TRT01P)
nrisk_df <- data.frame(
  strata = factor(sf_all$strata, labels = strata_levels),
  time = sf_all$time,
  nrisk = sf_all$n.risk
)

# Pivot at-risk table wider
nrisk_wide <- nrisk_df %>%
  pivot_wider(names_from = time, values_from = nrisk) %>%
  as.data.frame()
rownames(nrisk_wide) <- nrisk_wide$strata
nrisk_wide$strata <- NULL

# Colors
pal <- c("Placebo" = "#1F77B4", "Low Dose" = "#FF7F0E", "High Dose" = "#2CA02C")

# ---- MAIN SURVIVAL PLOT ----
p <- ggplot(plot_data, aes(x = time, y = surv, color = strata)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = strata), alpha = 0.12, color = NA) +
  geom_step(linewidth = 0.9) +
  geom_point(data = censor_data, shape = 3, size = 1.8, stroke = 0.8, color = "black") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = at_risk_times, limits = c(0, 370), expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1), labels = percent_format()) +
  labs(
    title = paste0("Study ", study_label, "\nKaplan-Meier Plot of ", param_label),
    subtitle = paste0("Log-rank test: ", pval_fmt),
    x = "Time (Days)", y = "Survival Probability",
    color = "Treatment Arm", fill = "Treatment Arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "bottom",
    legend.margin = margin(t = -4),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
  ) +
  guides(color = guide_legend(nrow = 1))

# Top plot (keep x-axis labels, hide title — table below carries "Time (Days)")
p_top <- p +
  theme(
    axis.title.x = element_blank(),
    plot.margin = margin(5.5, 5.5, 0, 5.5)
  )

# ---- AT-RISK TABLE AS A GGPLOT (same x-scale for perfect alignment) ----
tbl_data <- expand.grid(strata = strata_levels, time = at_risk_times, stringsAsFactors = FALSE)
tbl_data$strata <- factor(tbl_data$strata, levels = rev(strata_levels))
tbl_data$nrisk <- NA_integer_
for (i in seq_len(nrow(nrisk_df))) {
  idx <- which(tbl_data$strata == nrisk_df$strata[i] & tbl_data$time == nrisk_df$time[i])
  if (length(idx) > 0) tbl_data$nrisk[idx] <- nrisk_df$nrisk[i]
}

label_cols <- pal[levels(tbl_data$strata)]

tbl_plot <- ggplot(tbl_data, aes(x = time, y = strata)) +
  geom_text(aes(label = nrisk), size = 3.2, na.rm = TRUE) +
  scale_x_continuous(breaks = at_risk_times, limits = c(0, 370), expand = c(0.01, 0)) +
  scale_y_discrete() +
  labs(x = NULL, y = NULL, title = "Number at Risk") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0, margin = margin(b = 2)),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, face = "bold", color = label_cols[rev(seq_along(label_cols))]),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(2, 5.5, 5.5, 5.5)
  )

# Combine with aligned x-axes
combined <- plot_grid(
  p_top, tbl_plot,
  ncol = 1, align = "v", axis = "lr",
  rel_heights = c(3.2, 1)
)

pdf_file <- "output/km_plot_TTDEATH.pdf"
png_file <- "output/km_plot_TTDEATH.png"
rtf_file <- "output/km_plot_TTDEATH.rtf"

ggsave(pdf_file, combined, width = 8, height = 6.5, device = "pdf", dpi = 300)
ggsave(png_file, combined, width = 8, height = 6.5, device = "png", dpi = 300)

# ---- DOCX (via officer) ----
library(officer)
doc <- read_docx()
doc <- doc %>%
  body_add_par("", style = "Normal") %>%
  body_add_img(src = png_file, width = 6.5, height = 5) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(paste0("Study ", study_label, " | Parameter: ", param_label, " | Log-rank: ", pval_fmt), style = "Normal")
print(doc, target = gsub("\\.rtf$", ".docx", rtf_file))

# ---- RTF (standard format with PNG hex encoding) ----
raw <- readBin(png_file, "raw", file.info(png_file)$size)
hex <- paste(sprintf("%02x", as.integer(raw)), collapse = "")

med_tbl <- as.data.frame(summary(fit)$table)
med_tbl$Arm <- rownames(med_tbl)
med_lines <- apply(
  format(med_tbl[, c("Arm", "records", "median")], trim = TRUE),
  1, paste, collapse = "  "
)

f <- file(rtf_file, "wb")
writeBin(charToRaw("{\\rtf1\\ansi\\deff0\r\n"), f)
writeBin(charToRaw("{\\fonttbl {\\f0 Times New Roman;} {\\f1 Courier New;}}\r\n"), f)
writeBin(charToRaw("\\paperw12240\\paperh15840\r\n"), f)
writeBin(charToRaw("\\margl1440\\margr1440\\margt1440\\margb1440\r\n"), f)
writeBin(charToRaw(paste0("\\pard\\qc\\b\\fs28 Study ", study_label, " - Kaplan-Meier Plot of ", param_label, "\\b0\\par\r\n")), f)
writeBin(charToRaw(paste0("\\pard\\qc\\fs20 Log-rank test: ", pval_fmt, "\\par\r\n")), f)
writeBin(charToRaw("\\pard\\qc\\fs16 Note: + denotes censored observations.\\par\r\n"), f)
writeBin(charToRaw(paste0("{\\pict\\pngblip\\picw2400\\pich1950\\picwgoal11520\\pichgoal9360 ", hex, "}\r\n")), f)
writeBin(charToRaw(paste0("\\pard\\fs18\\b Summary of Median Survival:\\b0\\line ", "{\\f1\\fs16 Arm            Records  Median\\line ", paste(med_lines, collapse = "\\line "), "\\par}\r\n")), f)
writeBin(charToRaw(paste0("\\pard\\qc\\fs16 Study: ", study_label, " | Parameter: ", param_label, " | Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\\par}")), f)
close(f)

cat("Output:\n")
cat(sprintf("  %s\n", pdf_file))
cat(sprintf("  %s\n", png_file))
cat(sprintf("  %s\n", rtf_file))
cat(sprintf("Log-rank test: %s\n", pval_fmt))
