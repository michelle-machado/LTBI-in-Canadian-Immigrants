library(flextable)
library(officer)
library(dplyr)
library(data.table)

#Code for the manuscript

#' Data inputs file path
path.in <- "C:/Users/miche/Downloads/ARI project/"

#' Output file path
path.out <- "C:/Users/miche/Downloads/ARI project/"


all_time <- readRDS(paste0(path.out, "censusLTBPdiff.rds"))
all_time <- as.data.table(all_time)

#============================================================================================
#define countries and years
countries <- c("CHN", "VNM", "PHL", "IND")
cnsy_values <- c(2024, 2030, 2040, 2050) 
age_groups <- c("0-14", "15-34", "35-54", "55-74", "75+") 
ari <- readRDS(paste0(path.out, "ari(all_countries2050).rds"))

ari_result <- ari  %>%
  filter(iso3 %in% countries)

ari_result <- as.data.table(ari_result)

ari_result <-  ari_result[, .(
  Median_ari_sq = round(median(ari_sq, na.rm = TRUE), 5),
  P2.5_ari_sq = round(quantile(ari_sq, 0.025, na.rm = TRUE), 5),
  P97.5_ari_sq = round(quantile(ari_sq, 0.975, na.rm = TRUE), 5),
  
  Median_ari_1 = round(median(ari_1, na.rm = TRUE), 5),
  P2.5_ari_1 = round(quantile(ari_1, 0.025, na.rm = TRUE), 5),
  P97.5_ari_1 = round(quantile(ari_1, 0.975, na.rm = TRUE), 5),
  
  Median_ari_2 = round(median(ari_2, na.rm = TRUE), 5),
  P2.5_ari_2 = round(quantile(ari_2, 0.025, na.rm = TRUE), 5),
  P97.5_ari_2 = round(quantile(ari_2, 0.975, na.rm = TRUE), 5),
  
  Median_ari_3 = round(median(ari_3, na.rm = TRUE), 5),
  P2.5_ari_3 = round(quantile(ari_3, 0.025, na.rm = TRUE), 5),
  P97.5_ari_3 = round(quantile(ari_3, 0.975, na.rm = TRUE), 5),
  
  Median_ari_4 = round(median(ari_4, na.rm = TRUE), 5),
  P2.5_ari_4 = round(quantile(ari_4, 0.025, na.rm = TRUE), 5),
  P97.5_ari_4 = round(quantile(ari_4, 0.975, na.rm = TRUE), 5),
  
  Median_ari_5 = round(median(ari_5, na.rm = TRUE), 5),
  P2.5_ari_5 = round(quantile(ari_5, 0.025, na.rm = TRUE), 5),
  P97.5_ari_5 = round(quantile(ari_5, 0.975, na.rm = TRUE), 5)
), by = .(iso3, year)]


ari_result  <-   ari_result   %>%
  mutate(
    ari_sq_summary = paste0(Median_ari_sq, " (", P2.5_ari_sq, " to ", P97.5_ari_sq, ")"),
    ari_1_summary = paste0(Median_ari_1, " (", P2.5_ari_1, " to ", P97.5_ari_1, ")"),
    ari_2_summary = paste0(Median_ari_2, " (", P2.5_ari_2, " to ", P97.5_ari_2, ")"),
    ari_3_summary = paste0(Median_ari_3, " (", P2.5_ari_3, " to ", P97.5_ari_3, ")"),
    ari_4_summary = paste0(Median_ari_4, " (", P2.5_ari_4, " to ", P97.5_ari_4, ")"),
    ari_5_summary = paste0(Median_ari_5, " (", P2.5_ari_5, " to ", P97.5_ari_5, ")"),
  )

ari_result <- ari_result %>%
  select(iso3, year, 
         Median_ari_sq, P2.5_ari_sq, P97.5_ari_sq, ari_sq_summary,
         Median_ari_1, P2.5_ari_1, P97.5_ari_1, ari_1_summary,
         Median_ari_2, P2.5_ari_2, P97.5_ari_2, ari_2_summary,
         Median_ari_3, P2.5_ari_3, P97.5_ari_3, ari_3_summary,
         Median_ari_4, P2.5_ari_4, P97.5_ari_4, ari_4_summary,
         Median_ari_5, P2.5_ari_5, P97.5_ari_5, ari_5_summary,
  )

saveRDS(ari_result, file = paste0(path.out, "ARI_results.rds"))

ari_results <- ari_result %>%
  filter(year %in% cnsy_values) %>%
  select(iso3, year, ari_sq_summary, ari_1_summary, ari_2_summary, ari_3_summary, ari_4_summary, ari_5_summary) %>%
  rename(arisq = ari_sq_summary, ari1 = ari_1_summary, ari2 = ari_2_summary, ari3 = ari_3_summary, ari4 = ari_4_summary, ari5 = ari_5_summary) %>%
  arrange(iso3) %>%
  pivot_longer(cols = starts_with("ari"), names_to = "ARI", values_to = "Value") %>% # Convert ARI columns to rows
  pivot_wider(names_from = year, values_from = Value) %>% # Spread years as columns
  mutate(ARI = recode(ARI, arisq = "ARIsq", ari1 = "ARI1", ari2 = "ARI2", ari3 = "ARI3", ari4 = "ARI4", ari5 = "ARI5")) # Rename ARI labels

# Function to create the flextable
create_ari_flextable <- function(data) {
  # Create the flextable
  ft <- flextable(data) %>%
    autofit() %>%
    theme_vanilla() %>%
    set_header_labels(values = c(iso3 = "ISO3", ARI = "ARI", "2024" = "2024", "2030" = "2030", "2040" = "2040", "2050" = "2050")) %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    height_all(height = 0.1, part = "all") %>%
    merge_v(j = "iso3") %>%
    width(j = "iso3", width = 0.50) %>%
    width(j = "ARI", width = 0.75) %>%
    width(j = c("2024", "2030", "2040", "2050"), width = 1.25) %>%
    border_inner(part = "header", border = fp_border(color = "black", width = 1)) %>%
    border_inner_v(border = fp_border(color = "black", width = 1), part = "all")
  
  return(ft)
}

# Create the flextable for ARI data
ft_ari <- create_ari_flextable(ari_results)

# Create a Word document and add the flextable
doc <- read_docx()
doc <- doc %>%
  body_add_par("Table: ARI Summary", style = "heading 1") %>%
  body_add_flextable(ft_ari)

# Save the Word document
print(doc, target = "ARI_Summary_Table.docx")

#country wise all ages===================================================================== 
countrywise <- all_time

Sum_NUMP_all_ages <- countrywise [, .(
  NUMP = sum(NUMP, na.rm = TRUE),
  LTBPsq = sum(LTBPsq, na.rm = TRUE),
  LTBP1 = sum(LTBP1, na.rm = TRUE),
  LTBP2 = sum(LTBP2, na.rm = TRUE),
  LTBP3 = sum(LTBP3, na.rm = TRUE),
  LTBP4 = sum(LTBP4, na.rm = TRUE),
  LTBP5 = sum(LTBP5, na.rm = TRUE)
), by = .(ISO3, CNSY, YARP, replicate)]

  
Sum_NUMP_all_ages <- Sum_NUMP_all_ages %>%
  mutate(probLTBPsq = LTBPsq/NUMP,
         probLTBP1 = LTBP1/NUMP,
         probLTBP2 = LTBP2/NUMP,
         probLTBP3 = LTBP3/NUMP,
         probLTBP4 = LTBP4/NUMP,
         probLTBP5 = LTBP5/NUMP,
         absdiff_LTBP1 = probLTBP1 - probLTBPsq,
         absdiff_LTBP2 = probLTBP2 - probLTBPsq,
         absdiff_LTBP3 = probLTBP3 - probLTBPsq,
         absdiff_LTBP4 = probLTBP4 - probLTBPsq,
         absdiff_LTBP5 = probLTBP5 - probLTBPsq,
         reldiff_LTBP1 = absdiff_LTBP1 / probLTBPsq,
         reldiff_LTBP2 = absdiff_LTBP2 / probLTBPsq,
         reldiff_LTBP3 = absdiff_LTBP3 / probLTBPsq,
         reldiff_LTBP4 = absdiff_LTBP4 / probLTBPsq,
         reldiff_LTBP5 = absdiff_LTBP5 / probLTBPsq,)

  
# Summary stats for TOTAL prevalence
summary_stats_all_ages <- Sum_NUMP_all_ages [, .(
  NUMP = round(mean(NUMP, na.rm = TRUE), 3),
  probMedian_LTBPsq = round(median(probLTBPsq, na.rm = TRUE), 3),
  probP2.5_LTBPsq = round(quantile(probLTBPsq, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBPsq = round(quantile(probLTBPsq, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP1 = round(median(probLTBP1, na.rm = TRUE), 3),
  probP2.5_LTBP1 = round(quantile(probLTBP1, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP1 = round(quantile(probLTBP1, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP2 = round(median(probLTBP2, na.rm = TRUE), 3),
  probP2.5_LTBP2 = round(quantile(probLTBP2, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP2 = round(quantile(probLTBP2, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP3 = round(median(probLTBP3, na.rm = TRUE), 3),
  probP2.5_LTBP3 = round(quantile(probLTBP3, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP3 = round(quantile(probLTBP3, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP4 = round(median(probLTBP4, na.rm = TRUE), 3),
  probP2.5_LTBP4 = round(quantile(probLTBP4, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP4 = round(quantile(probLTBP4, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP5 = round(median(probLTBP5, na.rm = TRUE), 3),
  probP2.5_LTBP5 = round(quantile(probLTBP5, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP5 = round(quantile(probLTBP5, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP1 = round(median(absdiff_LTBP1, na.rm = TRUE), 3),
  absdiffP2.5_LTBP1 = round(quantile(absdiff_LTBP1, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP1 = round(quantile(absdiff_LTBP1, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP2 = round(median(absdiff_LTBP2, na.rm = TRUE), 3),
  absdiffP2.5_LTBP2 = round(quantile(absdiff_LTBP2, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP2 = round(quantile(absdiff_LTBP2, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP3 = round(median(absdiff_LTBP3, na.rm = TRUE), 3),
  absdiffP2.5_LTBP3 = round(quantile(absdiff_LTBP3, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP3 = round(quantile(absdiff_LTBP3, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP4 = round(median(absdiff_LTBP4, na.rm = TRUE), 3),
  absdiffP2.5_LTBP4 = round(quantile(absdiff_LTBP4, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP4 = round(quantile(absdiff_LTBP4, 0.975, na.rm = TRUE),3),
  
  absdiffMedian_LTBP5 = round(median(absdiff_LTBP5, na.rm = TRUE), 3),
  absdiffP2.5_LTBP5 = round(quantile(absdiff_LTBP5, 0.025, na.rm = TRUE),3),
  absdiffP97.5_LTBP5 = round(quantile(absdiff_LTBP5, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP1 = round(median(reldiff_LTBP1, na.rm = TRUE), 3),
  reldiffP2.5_LTBP1 = round(quantile(reldiff_LTBP1, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP1 = round(quantile(reldiff_LTBP1, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP2 = round(median(reldiff_LTBP2, na.rm = TRUE), 3),
  reldiffP2.5_LTBP2 = round(quantile(reldiff_LTBP2, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP2 = round(quantile(reldiff_LTBP2, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP3 = round(median(reldiff_LTBP3, na.rm = TRUE), 3),
  reldiffP2.5_LTBP3 = round(quantile(reldiff_LTBP3, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP3 = round(quantile(reldiff_LTBP3, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP4 = round(median(reldiff_LTBP4, na.rm = TRUE), 3),
  reldiffP2.5_LTBP4 = round(quantile(reldiff_LTBP4, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP4 = round(quantile(reldiff_LTBP4, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP5 = round(median(reldiff_LTBP5, na.rm = TRUE), 3),
  reldiffP2.5_LTBP5 = round(quantile(reldiff_LTBP5, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP5 = round(quantile(reldiff_LTBP5, 0.975, na.rm = TRUE), 3)
), by = .(ISO3, CNSY, YARP)]



summary_stats_all_ages <- summary_stats_all_ages %>%
  mutate(
    prob_LTBPsq_summary = paste0(probMedian_LTBPsq, " (", probP2.5_LTBPsq, " to ", probP97.5_LTBPsq, ")"),
    prob_LTBP1_summary = paste0(probMedian_LTBP1, " (", probP2.5_LTBP1, " to ", probP97.5_LTBP1, ")"),
    prob_LTBP2_summary = paste0(probMedian_LTBP2, " (", probP2.5_LTBP2, " to ", probP97.5_LTBP2, ")"),
    prob_LTBP3_summary = paste0(probMedian_LTBP3, " (", probP2.5_LTBP3, " to ", probP97.5_LTBP3, ")"),
    prob_LTBP4_summary = paste0(probMedian_LTBP4, " (", probP2.5_LTBP4, " to ", probP97.5_LTBP4, ")"),
    prob_LTBP5_summary = paste0(probMedian_LTBP5, " (", probP2.5_LTBP5, " to ", probP97.5_LTBP5, ")"),
    
    absdiff_LTBP1_summary = paste0(absdiffMedian_LTBP1, " (", absdiffP2.5_LTBP1, " to ", absdiffP97.5_LTBP1, ")"),
    absdiff_LTBP2_summary = paste0(absdiffMedian_LTBP2, " (", absdiffP2.5_LTBP2, " to ", absdiffP97.5_LTBP2, ")"),
    absdiff_LTBP3_summary = paste0(absdiffMedian_LTBP3, " (", absdiffP2.5_LTBP3, " to ", absdiffP97.5_LTBP3, ")"),
    absdiff_LTBP4_summary = paste0(absdiffMedian_LTBP4, " (", absdiffP2.5_LTBP4, " to ", absdiffP97.5_LTBP4, ")"),
    absdiff_LTBP5_summary = paste0(absdiffMedian_LTBP5, " (", absdiffP2.5_LTBP5, " to ", absdiffP97.5_LTBP5, ")"),
    
    reldiff_LTBP1_summary = paste0(reldiffMedian_LTBP1, " (", reldiffP2.5_LTBP1, " to ", reldiffP97.5_LTBP1, ")"),
    reldiff_LTBP2_summary = paste0(reldiffMedian_LTBP2, " (", reldiffP2.5_LTBP2, " to ", reldiffP97.5_LTBP2, ")"),
    reldiff_LTBP3_summary = paste0(reldiffMedian_LTBP3, " (", reldiffP2.5_LTBP3, " to ", reldiffP97.5_LTBP3, ")"),
    reldiff_LTBP4_summary = paste0(reldiffMedian_LTBP4, " (", reldiffP2.5_LTBP4, " to ", reldiffP97.5_LTBP4, ")"),
    reldiff_LTBP5_summary = paste0(reldiffMedian_LTBP5, " (", reldiffP2.5_LTBP5, " to ", reldiffP97.5_LTBP5, ")")
  )

summary_stats_all_ages <- summary_stats_all_ages %>%
  select(ISO3, CNSY, YARP, NUMP, 
         probMedian_LTBPsq, probP2.5_LTBPsq, probP97.5_LTBPsq, prob_LTBPsq_summary,
         probMedian_LTBP1, probP2.5_LTBP1, probP97.5_LTBP1, prob_LTBP1_summary,
         probMedian_LTBP2, probP2.5_LTBP2, probP97.5_LTBP2, prob_LTBP2_summary,
         probMedian_LTBP3, probP2.5_LTBP3, probP97.5_LTBP3, prob_LTBP3_summary,
         probMedian_LTBP4, probP2.5_LTBP4, probP97.5_LTBP4, prob_LTBP4_summary,
         probMedian_LTBP5, probP2.5_LTBP5, probP97.5_LTBP5, prob_LTBP5_summary,
         
         absdiffMedian_LTBP1, absdiffP2.5_LTBP1, absdiffP97.5_LTBP1, absdiff_LTBP1_summary,
         absdiffMedian_LTBP2, absdiffP2.5_LTBP2, absdiffP97.5_LTBP2, absdiff_LTBP2_summary,
         absdiffMedian_LTBP3, absdiffP2.5_LTBP3, absdiffP97.5_LTBP3, absdiff_LTBP3_summary,
         absdiffMedian_LTBP4, absdiffP2.5_LTBP4, absdiffP97.5_LTBP4, absdiff_LTBP4_summary,
         absdiffMedian_LTBP5, absdiffP2.5_LTBP5, absdiffP97.5_LTBP5, absdiff_LTBP5_summary,
         
         reldiffMedian_LTBP1, reldiffP2.5_LTBP1, reldiffP97.5_LTBP1, reldiff_LTBP1_summary,
         reldiffMedian_LTBP2, reldiffP2.5_LTBP2, reldiffP97.5_LTBP2, reldiff_LTBP2_summary,
         reldiffMedian_LTBP3, reldiffP2.5_LTBP3, reldiffP97.5_LTBP3, reldiff_LTBP3_summary,
         reldiffMedian_LTBP4, reldiffP2.5_LTBP4, reldiffP97.5_LTBP4, reldiff_LTBP4_summary,
         reldiffMedian_LTBP5, reldiffP2.5_LTBP5, reldiffP97.5_LTBP5, reldiff_LTBP5_summary)

saveRDS(summary_stats_all_ages, file = paste0(path.out, "census_summary_stats_allages.rds"))


#Age group wise all time======================================================================================================
age_groups_cut <- all_time[, AgeGroup := cut(AGEP, 
                                             breaks = c(0, 15, 35, 55, 75, Inf), 
                                             right = FALSE, 
                                             labels = c("0-14", "15-34", "35-54", "55-74", "75+"))]

Sum_NUMP_agegroup <- age_groups_cut[, .(
  NUMP = sum(NUMP, na.rm = TRUE),
  LTBPsq = sum(LTBPsq, na.rm = TRUE),
  LTBP1 = sum(LTBP1, na.rm = TRUE),
  LTBP2 = sum(LTBP2, na.rm = TRUE),
  LTBP3 = sum(LTBP3, na.rm = TRUE),
  LTBP4 = sum(LTBP4, na.rm = TRUE),
  LTBP5 = sum(LTBP5, na.rm = TRUE)
), by = .(ISO3, CNSY, YARP, AgeGroup, replicate)]

Sum_NUMP_agegroup <- Sum_NUMP_agegroup %>%
  mutate(probLTBPsq = LTBPsq/NUMP,
         probLTBP1 = LTBP1/NUMP,
         probLTBP2 = LTBP2/NUMP,
         probLTBP3 = LTBP3/NUMP,
         probLTBP4 = LTBP4/NUMP,
         probLTBP5 = LTBP5/NUMP,
         absdiff_LTBP1 = probLTBP1 - probLTBPsq,
         absdiff_LTBP2 = probLTBP2 - probLTBPsq,
         absdiff_LTBP3 = probLTBP3 - probLTBPsq,
         absdiff_LTBP4 = probLTBP4 - probLTBPsq,
         absdiff_LTBP5 = probLTBP5 - probLTBPsq,
         reldiff_LTBP1 = absdiff_LTBP1 / probLTBPsq,
         reldiff_LTBP2 = absdiff_LTBP2 / probLTBPsq,
         reldiff_LTBP3 = absdiff_LTBP3 / probLTBPsq,
         reldiff_LTBP4 = absdiff_LTBP4 / probLTBPsq,
         reldiff_LTBP5 = absdiff_LTBP5 / probLTBPsq,)

summary_stats_agegroup <-  Sum_NUMP_agegroup[, .(
  NUMP = round(mean(NUMP, na.rm = TRUE), 3),
  probMedian_LTBPsq = round(median(probLTBPsq, na.rm = TRUE), 3),
  probP2.5_LTBPsq = round(quantile(probLTBPsq, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBPsq = round(quantile(probLTBPsq, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP1 = round(median(probLTBP1, na.rm = TRUE), 3),
  probP2.5_LTBP1 = round(quantile(probLTBP1, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP1 = round(quantile(probLTBP1, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP2 = round(median(probLTBP2, na.rm = TRUE), 3),
  probP2.5_LTBP2 = round(quantile(probLTBP2, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP2 = round(quantile(probLTBP2, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP3 = round(median(probLTBP3, na.rm = TRUE), 3),
  probP2.5_LTBP3 = round(quantile(probLTBP3, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP3 = round(quantile(probLTBP3, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP4 = round(median(probLTBP4, na.rm = TRUE), 3),
  probP2.5_LTBP4 = round(quantile(probLTBP4, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP4 = round(quantile(probLTBP4, 0.975, na.rm = TRUE), 3),
  
  probMedian_LTBP5 = round(median(probLTBP5, na.rm = TRUE), 3),
  probP2.5_LTBP5 = round(quantile(probLTBP5, 0.025, na.rm = TRUE), 3),
  probP97.5_LTBP5 = round(quantile(probLTBP5, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP1 = round(median(absdiff_LTBP1, na.rm = TRUE), 3),
  absdiffP2.5_LTBP1 = round(quantile(absdiff_LTBP1, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP1 = round(quantile(absdiff_LTBP1, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP2 = round(median(absdiff_LTBP2, na.rm = TRUE), 3),
  absdiffP2.5_LTBP2 = round(quantile(absdiff_LTBP2, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP2 = round(quantile(absdiff_LTBP2, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP3 = round(median(absdiff_LTBP3, na.rm = TRUE), 3),
  absdiffP2.5_LTBP3 = round(quantile(absdiff_LTBP3, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP3 = round(quantile(absdiff_LTBP3, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP4 = round(median(absdiff_LTBP4, na.rm = TRUE), 3),
  absdiffP2.5_LTBP4 = round(quantile(absdiff_LTBP4, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP4 = round(quantile(absdiff_LTBP4, 0.975, na.rm = TRUE),3),
  
  absdiffMedian_LTBP5 = round(median(absdiff_LTBP5, na.rm = TRUE), 3),
  absdiffP2.5_LTBP5 = round(quantile(absdiff_LTBP5, 0.025, na.rm = TRUE),3),
  absdiffP97.5_LTBP5 = round(quantile(absdiff_LTBP5, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP1 = round(median(reldiff_LTBP1, na.rm = TRUE), 3),
  reldiffP2.5_LTBP1 = round(quantile(reldiff_LTBP1, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP1 = round(quantile(reldiff_LTBP1, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP2 = round(median(reldiff_LTBP2, na.rm = TRUE), 3),
  reldiffP2.5_LTBP2 = round(quantile(reldiff_LTBP2, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP2 = round(quantile(reldiff_LTBP2, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP3 = round(median(reldiff_LTBP3, na.rm = TRUE), 3),
  reldiffP2.5_LTBP3 = round(quantile(reldiff_LTBP3, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP3 = round(quantile(reldiff_LTBP3, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP4 = round(median(reldiff_LTBP4, na.rm = TRUE), 3),
  reldiffP2.5_LTBP4 = round(quantile(reldiff_LTBP4, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP4 = round(quantile(reldiff_LTBP4, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP5 = round(median(reldiff_LTBP5, na.rm = TRUE), 3),
  reldiffP2.5_LTBP5 = round(quantile(reldiff_LTBP5, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP5 = round(quantile(reldiff_LTBP5, 0.975, na.rm = TRUE), 3)
), by = .(ISO3, CNSY, YARP, AgeGroup)]

#check if there are any problem rows
problem_rows <- summary_stats_agegroup %>% 
  filter( probP97.5_LTBPsq > NUMP)


summary_stats_agegroup <- summary_stats_agegroup %>%
  mutate(
    prob_LTBPsq_summary = paste0(probMedian_LTBPsq, " (", probP2.5_LTBPsq, " to ", probP97.5_LTBPsq, ")"),
    prob_LTBP1_summary = paste0(probMedian_LTBP1, " (", probP2.5_LTBP1, " to ", probP97.5_LTBP1, ")"),
    prob_LTBP2_summary = paste0(probMedian_LTBP2, " (", probP2.5_LTBP2, " to ", probP97.5_LTBP2, ")"),
    prob_LTBP3_summary = paste0(probMedian_LTBP3, " (", probP2.5_LTBP3, " to ", probP97.5_LTBP3, ")"),
    prob_LTBP4_summary = paste0(probMedian_LTBP4, " (", probP2.5_LTBP4, " to ", probP97.5_LTBP4, ")"),
    prob_LTBP5_summary = paste0(probMedian_LTBP5, " (", probP2.5_LTBP5, " to ", probP97.5_LTBP5, ")"),
    
    absdiff_LTBP1_summary = paste0(absdiffMedian_LTBP1, " (", absdiffP2.5_LTBP1, " to ", absdiffP97.5_LTBP1, ")"),
    absdiff_LTBP2_summary = paste0(absdiffMedian_LTBP2, " (", absdiffP2.5_LTBP2, " to ", absdiffP97.5_LTBP2, ")"),
    absdiff_LTBP3_summary = paste0(absdiffMedian_LTBP3, " (", absdiffP2.5_LTBP3, " to ", absdiffP97.5_LTBP3, ")"),
    absdiff_LTBP4_summary = paste0(absdiffMedian_LTBP4, " (", absdiffP2.5_LTBP4, " to ", absdiffP97.5_LTBP4, ")"),
    absdiff_LTBP5_summary = paste0(absdiffMedian_LTBP5, " (", absdiffP2.5_LTBP5, " to ", absdiffP97.5_LTBP5, ")"),
    
    reldiff_LTBP1_summary = paste0(reldiffMedian_LTBP1, " (", reldiffP2.5_LTBP1, " to ", reldiffP97.5_LTBP1, ")"),
    reldiff_LTBP2_summary = paste0(reldiffMedian_LTBP2, " (", reldiffP2.5_LTBP2, " to ", reldiffP97.5_LTBP2, ")"),
    reldiff_LTBP3_summary = paste0(reldiffMedian_LTBP3, " (", reldiffP2.5_LTBP3, " to ", reldiffP97.5_LTBP3, ")"),
    reldiff_LTBP4_summary = paste0(reldiffMedian_LTBP4, " (", reldiffP2.5_LTBP4, " to ", reldiffP97.5_LTBP4, ")"),
    reldiff_LTBP5_summary = paste0(reldiffMedian_LTBP5, " (", reldiffP2.5_LTBP5, " to ", reldiffP97.5_LTBP5, ")")
  )

summary_stats_agegroup <- summary_stats_agegroup %>%
  select(ISO3, CNSY, YARP, AgeGroup, NUMP, 
         probMedian_LTBPsq, probP2.5_LTBPsq, probP97.5_LTBPsq, prob_LTBPsq_summary,
         probMedian_LTBP1, probP2.5_LTBP1, probP97.5_LTBP1, prob_LTBP1_summary,
         probMedian_LTBP2, probP2.5_LTBP2, probP97.5_LTBP2, prob_LTBP2_summary,
         probMedian_LTBP3, probP2.5_LTBP3, probP97.5_LTBP3, prob_LTBP3_summary,
         probMedian_LTBP4, probP2.5_LTBP4, probP97.5_LTBP4, prob_LTBP4_summary,
         probMedian_LTBP5, probP2.5_LTBP5, probP97.5_LTBP5, prob_LTBP5_summary,
         
         absdiffMedian_LTBP1, absdiffP2.5_LTBP1, absdiffP97.5_LTBP1, absdiff_LTBP1_summary,
         absdiffMedian_LTBP2, absdiffP2.5_LTBP2, absdiffP97.5_LTBP2, absdiff_LTBP2_summary,
         absdiffMedian_LTBP3, absdiffP2.5_LTBP3, absdiffP97.5_LTBP3, absdiff_LTBP3_summary,
         absdiffMedian_LTBP4, absdiffP2.5_LTBP4, absdiffP97.5_LTBP4, absdiff_LTBP4_summary,
         absdiffMedian_LTBP5, absdiffP2.5_LTBP5, absdiffP97.5_LTBP5, absdiff_LTBP5_summary,
         
         reldiffMedian_LTBP1, reldiffP2.5_LTBP1, reldiffP97.5_LTBP1, reldiff_LTBP1_summary,
         reldiffMedian_LTBP2, reldiffP2.5_LTBP2, reldiffP97.5_LTBP2, reldiff_LTBP2_summary,
         reldiffMedian_LTBP3, reldiffP2.5_LTBP3, reldiffP97.5_LTBP3, reldiff_LTBP3_summary,
         reldiffMedian_LTBP4, reldiffP2.5_LTBP4, reldiffP97.5_LTBP4, reldiff_LTBP4_summary,
         reldiffMedian_LTBP5, reldiffP2.5_LTBP5, reldiffP97.5_LTBP5, reldiff_LTBP5_summary)


saveRDS(summary_stats_agegroup, file = paste0(path.out, "census_summary_stats(AgeGroups).rds"))

#-------------------------------------------------------------------------------------------------------------
# recently infected
Sum_NUMP_RI <- all_time %>%
  group_by(ISO3, replicate, YOBP) %>%
  arrange(YARP) %>%
  mutate(
    NUMP_infsq = ((LTBPsq / NUMP) - (lag(LTBPsq, 2) / lag(NUMP, 2))) *  NUMP,
    NUMP_inf1 = ((LTBP1 / NUMP) - (lag(LTBP1, 2) / lag(NUMP, 2))) *  NUMP,
    NUMP_inf2 = ((LTBP2 / NUMP) - (lag(LTBP2, 2) / lag(NUMP, 2))) *  NUMP,
    NUMP_inf3 = ((LTBP3 / NUMP) - (lag(LTBP3, 2) / lag(NUMP, 2))) *  NUMP,
    NUMP_inf4 = ((LTBP4 / NUMP) - (lag(LTBP4, 2) / lag(NUMP, 2))) *  NUMP,
    NUMP_inf5 = ((LTBP5 / NUMP) - (lag(LTBP5, 2) / lag(NUMP, 2))) *  NUMP
  ) %>%
  ungroup()

Sum_NUMP_RI <- as.data.table(Sum_NUMP_RI)   


#recently inf all ages===============================================================================================================
Sum_NUMP_RI_all_ages <- Sum_NUMP_RI [, .(
  NUMP = sum(NUMP, na.rm = TRUE),
  NUMP_infsq = sum(NUMP_infsq, na.rm = TRUE),
  NUMP_inf1 = sum(NUMP_inf1, na.rm = TRUE),
  NUMP_inf2 = sum(NUMP_inf2, na.rm = TRUE),
  NUMP_inf3 = sum(NUMP_inf3, na.rm = TRUE),
  NUMP_inf4 = sum(NUMP_inf4, na.rm = TRUE),
  NUMP_inf5 = sum(NUMP_inf5, na.rm = TRUE)
), by = .(ISO3, CNSY, YARP, replicate)]

Sum_NUMP_RI_all_ages <- Sum_NUMP_RI_all_ages %>%
  mutate(aNUMP_infsq = NUMP_infsq / NUMP,
         aNUMP_inf1 = NUMP_inf1 / NUMP,
         aNUMP_inf2 = NUMP_inf2 / NUMP,
         aNUMP_inf3 = NUMP_inf3 / NUMP,
         aNUMP_inf4 = NUMP_inf4 / NUMP,
         aNUMP_inf5 = NUMP_inf5 / NUMP) %>%
  select(-c(NUMP_inf5, NUMP_inf4, NUMP_inf3, NUMP_inf2, NUMP_inf1, NUMP_infsq))

Sum_NUMP_RI_all_ages <- as.data.table(Sum_NUMP_RI_all_ages)
#before this calculate abs_inf by dividing NUMPinf_sq by NUMP then do abd and rel
Sum_NUMP_RI_all_ages <- Sum_NUMP_RI_all_ages %>%
  mutate(abs_inf1 = aNUMP_inf1 - aNUMP_infsq,
         abs_inf2 = aNUMP_inf2 - aNUMP_infsq,
         abs_inf3 = aNUMP_inf3 - aNUMP_infsq,
         abs_inf4 = aNUMP_inf4 - aNUMP_infsq,
         abs_inf5 = aNUMP_inf5 - aNUMP_infsq,
         rel_inf1 = abs_inf1 / aNUMP_infsq,
         rel_inf2 = abs_inf2 / aNUMP_infsq,
         rel_inf3 = abs_inf3 / aNUMP_infsq,
         rel_inf4 = abs_inf4 / aNUMP_infsq,
         rel_inf5 = abs_inf5 / aNUMP_infsq)

recentlyinf_all_ages <-  Sum_NUMP_RI_all_ages[, .(
  NUMP = round(mean(NUMP, na.rm = TRUE), 0),
  Median_aNUMP_infsq = round(median(aNUMP_infsq, na.rm = TRUE), 3),
  P2.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf1 = round(median(aNUMP_inf1, na.rm = TRUE), 3),
  P2.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf2 = round(median(aNUMP_inf2, na.rm = TRUE), 3),
  P2.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf3 = round(median(aNUMP_inf3, na.rm = TRUE), 3),
  P2.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf4 = round(median(aNUMP_inf4, na.rm = TRUE), 3),
  P2.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf5 = round(median(aNUMP_inf5, na.rm = TRUE), 3),
  P2.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP1 = round(median(abs_inf1, na.rm = TRUE), 3),
  absdiffP2.5_LTBP1 = round(quantile(abs_inf1, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP1 =round(quantile(abs_inf1, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP2 = round(median(abs_inf2, na.rm = TRUE), 3),
  absdiffP2.5_LTBP2 = round(quantile(abs_inf2, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP2 =round(quantile(abs_inf2, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP3 = round(median(abs_inf3, na.rm = TRUE), 3),
  absdiffP2.5_LTBP3 = round(quantile(abs_inf3, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP3 =round(quantile(abs_inf3, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP4 = round(median(abs_inf4, na.rm = TRUE), 3),
  absdiffP2.5_LTBP4 = round(quantile(abs_inf4, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP4 =round(quantile(abs_inf4, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP5 = round(median(abs_inf5, na.rm = TRUE), 3),
  absdiffP2.5_LTBP5 = round(quantile(abs_inf5, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP5 =round(quantile(abs_inf5, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP1 = round(median(rel_inf1, na.rm = TRUE), 3),
  reldiffP2.5_LTBP1 = round(quantile(rel_inf1, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP1 = round(quantile(rel_inf1, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP2 = round(median(rel_inf2, na.rm = TRUE), 3),
  reldiffP2.5_LTBP2 = round(quantile(rel_inf2, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP2 = round(quantile(rel_inf2, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP3 = round(median(rel_inf3, na.rm = TRUE), 3),
  reldiffP2.5_LTBP3 = round(quantile(rel_inf3, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP3 = round(quantile(rel_inf3, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP4 = round(median(rel_inf4, na.rm = TRUE), 3),
  reldiffP2.5_LTBP4 = round(quantile(rel_inf4, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP4 = round(quantile(rel_inf4, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP5 = round(median(rel_inf5, na.rm = TRUE), 3),
  reldiffP2.5_LTBP5 = round(quantile(rel_inf5, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP5 = round(quantile(rel_inf5, 0.975, na.rm = TRUE), 3)
), by = .(ISO3, CNSY, YARP)]


recentlyinf_all_ages <- recentlyinf_all_ages  %>%
  mutate(
    aNUMP_infsq_summary = paste0(Median_aNUMP_infsq, " (", P2.5_aNUMP_infsq, " to ", P97.5_aNUMP_infsq, ")"),
    aNUMP_inf1_summary = paste0(Median_aNUMP_inf1, " (", P2.5_aNUMP_inf1, " to ", P97.5_aNUMP_inf1, ")"),
    aNUMP_inf2_summary = paste0(Median_aNUMP_inf2, " (", P2.5_aNUMP_inf2, " to ", P97.5_aNUMP_inf2, ")"),
    aNUMP_inf3_summary = paste0(Median_aNUMP_inf3, " (", P2.5_aNUMP_inf3, " to ", P97.5_aNUMP_inf3, ")"),
    aNUMP_inf4_summary = paste0(Median_aNUMP_inf4, " (", P2.5_aNUMP_inf4, " to ", P97.5_aNUMP_inf4, ")"),
    aNUMP_inf5_summary = paste0(Median_aNUMP_inf5, " (", P2.5_aNUMP_inf5, " to ", P97.5_aNUMP_inf5, ")"),
    
    abs_inf1_summary = paste0(absdiffMedian_LTBP1, " (", absdiffP2.5_LTBP1, " to ", absdiffP97.5_LTBP1, ")"),
    abs_inf2_summary = paste0(absdiffMedian_LTBP2, " (", absdiffP2.5_LTBP2, " to ", absdiffP97.5_LTBP2, ")"),
    abs_inf3_summary = paste0(absdiffMedian_LTBP3, " (", absdiffP2.5_LTBP3, " to ", absdiffP97.5_LTBP3, ")"),
    abs_inf4_summary = paste0(absdiffMedian_LTBP4, " (", absdiffP2.5_LTBP4, " to ", absdiffP97.5_LTBP4, ")"),
    abs_inf5_summary = paste0(absdiffMedian_LTBP5, " (", absdiffP2.5_LTBP5, " to ", absdiffP97.5_LTBP5, ")"),
    
    rel_inf1_summary = paste0(reldiffMedian_LTBP1, " (", reldiffP2.5_LTBP1, " to ", reldiffP97.5_LTBP1, ")"),
    rel_inf2_summary = paste0(reldiffMedian_LTBP2, " (", reldiffP2.5_LTBP2, " to ", reldiffP97.5_LTBP2, ")"),
    rel_inf3_summary = paste0(reldiffMedian_LTBP3, " (", reldiffP2.5_LTBP3, " to ", reldiffP97.5_LTBP3, ")"),
    rel_inf4_summary = paste0(reldiffMedian_LTBP4, " (", reldiffP2.5_LTBP4, " to ", reldiffP97.5_LTBP4, ")"),
    rel_inf5_summary = paste0(reldiffMedian_LTBP5, " (", reldiffP2.5_LTBP5, " to ", reldiffP97.5_LTBP5, ")"),
  )

recentlyinf_all_ages <- recentlyinf_all_ages  %>%
  select(ISO3, CNSY, YARP, NUMP, 
         Median_aNUMP_infsq, P2.5_aNUMP_infsq, P97.5_aNUMP_infsq, aNUMP_infsq_summary,
         Median_aNUMP_inf1, P2.5_aNUMP_inf1, P97.5_aNUMP_inf1, aNUMP_inf1_summary,
         Median_aNUMP_inf2, P2.5_aNUMP_inf2, P97.5_aNUMP_inf2, aNUMP_inf2_summary,
         Median_aNUMP_inf3, P2.5_aNUMP_inf3, P97.5_aNUMP_inf3, aNUMP_inf3_summary,
         Median_aNUMP_inf4, P2.5_aNUMP_inf4, P97.5_aNUMP_inf4, aNUMP_inf4_summary,
         Median_aNUMP_inf5, P2.5_aNUMP_inf5, P97.5_aNUMP_inf5, aNUMP_inf5_summary,
         absdiffMedian_LTBP1, absdiffP2.5_LTBP1, absdiffP97.5_LTBP1, abs_inf1_summary,
         absdiffMedian_LTBP2, absdiffP2.5_LTBP2, absdiffP97.5_LTBP2, abs_inf2_summary,
         absdiffMedian_LTBP3, absdiffP2.5_LTBP3, absdiffP97.5_LTBP3, abs_inf3_summary,
         absdiffMedian_LTBP4, absdiffP2.5_LTBP4, absdiffP97.5_LTBP4, abs_inf4_summary,
         absdiffMedian_LTBP5, absdiffP2.5_LTBP5, absdiffP97.5_LTBP5, abs_inf5_summary,
         reldiffMedian_LTBP1, reldiffP2.5_LTBP1, reldiffP97.5_LTBP1, rel_inf1_summary,
         reldiffMedian_LTBP2, reldiffP2.5_LTBP2, reldiffP97.5_LTBP2, rel_inf2_summary,
         reldiffMedian_LTBP3, reldiffP2.5_LTBP3, reldiffP97.5_LTBP3, rel_inf3_summary,
         reldiffMedian_LTBP4, reldiffP2.5_LTBP4, reldiffP97.5_LTBP4, rel_inf4_summary,
         reldiffMedian_LTBP5, reldiffP2.5_LTBP5, reldiffP97.5_LTBP5, rel_inf5_summary)

saveRDS(recentlyinf_all_ages , file = paste0(path.out, "recently_inf_summary(all_ages).rds"))


#recently inf age group===================================================================================================
Sum_NUMP_RI_agegroup <- Sum_NUMP_RI[, AgeGroup := cut(AGEP, 
                                             breaks = c(0, 15, 35, 55, 75, Inf), 
                                             right = FALSE, 
                                             labels = c("0-14", "15-34", "35-54", "55-74", "75+"))]


Sum_NUMP_RI_agegroup <- Sum_NUMP_RI_agegroup [, .(
  NUMP = sum(NUMP, na.rm = TRUE),
  NUMP_infsq = sum(NUMP_infsq, na.rm = TRUE),
  NUMP_inf1 = sum(NUMP_inf1, na.rm = TRUE),
  NUMP_inf2 = sum(NUMP_inf2, na.rm = TRUE),
  NUMP_inf3 = sum(NUMP_inf3, na.rm = TRUE),
  NUMP_inf4 = sum(NUMP_inf4, na.rm = TRUE),
  NUMP_inf5 = sum(NUMP_inf5, na.rm = TRUE)
), by = .(ISO3, CNSY, YARP, AgeGroup, replicate)]

Sum_NUMP_RI_agegroup <- Sum_NUMP_RI_agegroup %>%
  mutate(aNUMP_infsq = NUMP_infsq / NUMP,
         aNUMP_inf1 = NUMP_inf1 / NUMP,
         aNUMP_inf2 = NUMP_inf2 / NUMP,
         aNUMP_inf3 = NUMP_inf3 / NUMP,
         aNUMP_inf4 = NUMP_inf4 / NUMP,
         aNUMP_inf5 = NUMP_inf5 / NUMP) %>%
  select(-c(NUMP_inf5, NUMP_inf4, NUMP_inf3, NUMP_inf2, NUMP_inf1, NUMP_infsq))

Sum_NUMP_RI_agegroup <- as.data.table(Sum_NUMP_RI_agegroup)
#before this calculate abs_inf by dividing NUMPinf_sq by NUMP then do abd and rel
Sum_NUMP_RI_agegroup <- Sum_NUMP_RI_agegroup %>%
  mutate(abs_inf1 = aNUMP_inf1 - aNUMP_infsq,
         abs_inf2 = aNUMP_inf2 - aNUMP_infsq,
         abs_inf3 = aNUMP_inf3 - aNUMP_infsq,
         abs_inf4 = aNUMP_inf4 - aNUMP_infsq,
         abs_inf5 = aNUMP_inf5 - aNUMP_infsq,
         rel_inf1 = abs_inf1 / aNUMP_infsq,
         rel_inf2 = abs_inf2 / aNUMP_infsq,
         rel_inf3 = abs_inf3 / aNUMP_infsq,
         rel_inf4 = abs_inf4 / aNUMP_infsq,
         rel_inf5 = abs_inf5 / aNUMP_infsq)

recentlyinf_agegroup <-  Sum_NUMP_RI_agegroup[, .(
  NUMP = round(mean(NUMP, na.rm = TRUE), 0),
  Median_aNUMP_infsq = round(median(aNUMP_infsq, na.rm = TRUE), 3),
  P2.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf1 = round(median(aNUMP_inf1, na.rm = TRUE), 3),
  P2.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf2 = round(median(aNUMP_inf2, na.rm = TRUE), 3),
  P2.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf3 = round(median(aNUMP_inf3, na.rm = TRUE), 3),
  P2.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf4 = round(median(aNUMP_inf4, na.rm = TRUE), 3),
  P2.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.975, na.rm = TRUE), 3),
  
  Median_aNUMP_inf5 = round(median(aNUMP_inf5, na.rm = TRUE), 3),
  P2.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.025, na.rm = TRUE), 3),
  P97.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP1 = round(median(abs_inf1, na.rm = TRUE), 3),
  absdiffP2.5_LTBP1 = round(quantile(abs_inf1, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP1 =round(quantile(abs_inf1, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP2 = round(median(abs_inf2, na.rm = TRUE), 3),
  absdiffP2.5_LTBP2 = round(quantile(abs_inf2, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP2 =round(quantile(abs_inf2, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP3 = round(median(abs_inf3, na.rm = TRUE), 3),
  absdiffP2.5_LTBP3 = round(quantile(abs_inf3, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP3 =round(quantile(abs_inf3, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP4 = round(median(abs_inf4, na.rm = TRUE), 3),
  absdiffP2.5_LTBP4 = round(quantile(abs_inf4, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP4 =round(quantile(abs_inf4, 0.975, na.rm = TRUE), 3),
  
  absdiffMedian_LTBP5 = round(median(abs_inf5, na.rm = TRUE), 3),
  absdiffP2.5_LTBP5 = round(quantile(abs_inf5, 0.025, na.rm = TRUE), 3),
  absdiffP97.5_LTBP5 =round(quantile(abs_inf5, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP1 = round(median(rel_inf1, na.rm = TRUE), 3),
  reldiffP2.5_LTBP1 = round(quantile(rel_inf1, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP1 = round(quantile(rel_inf1, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP2 = round(median(rel_inf2, na.rm = TRUE), 3),
  reldiffP2.5_LTBP2 = round(quantile(rel_inf2, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP2 = round(quantile(rel_inf2, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP3 = round(median(rel_inf3, na.rm = TRUE), 3),
  reldiffP2.5_LTBP3 = round(quantile(rel_inf3, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP3 = round(quantile(rel_inf3, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP4 = round(median(rel_inf4, na.rm = TRUE), 3),
  reldiffP2.5_LTBP4 = round(quantile(rel_inf4, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP4 = round(quantile(rel_inf4, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP5 = round(median(rel_inf5, na.rm = TRUE), 3),
  reldiffP2.5_LTBP5 = round(quantile(rel_inf5, 0.025, na.rm = TRUE), 3),
  reldiffP97.5_LTBP5 = round(quantile(rel_inf5, 0.975, na.rm = TRUE), 3)
), by = .(ISO3, CNSY, YARP, AgeGroup)]


recentlyinf_agegroup  <- recentlyinf_agegroup   %>%
  mutate(
    aNUMP_infsq_summary = paste0(Median_aNUMP_infsq, " (", P2.5_aNUMP_infsq, " to ", P97.5_aNUMP_infsq, ")"),
    aNUMP_inf1_summary = paste0(Median_aNUMP_inf1, " (", P2.5_aNUMP_inf1, " to ", P97.5_aNUMP_inf1, ")"),
    aNUMP_inf2_summary = paste0(Median_aNUMP_inf2, " (", P2.5_aNUMP_inf2, " to ", P97.5_aNUMP_inf2, ")"),
    aNUMP_inf3_summary = paste0(Median_aNUMP_inf3, " (", P2.5_aNUMP_inf3, " to ", P97.5_aNUMP_inf3, ")"),
    aNUMP_inf4_summary = paste0(Median_aNUMP_inf4, " (", P2.5_aNUMP_inf4, " to ", P97.5_aNUMP_inf4, ")"),
    aNUMP_inf5_summary = paste0(Median_aNUMP_inf5, " (", P2.5_aNUMP_inf5, " to ", P97.5_aNUMP_inf5, ")"),
    
    abs_inf1_summary = paste0(absdiffMedian_LTBP1, " (", absdiffP2.5_LTBP1, " to ", absdiffP97.5_LTBP1, ")"),
    abs_inf2_summary = paste0(absdiffMedian_LTBP2, " (", absdiffP2.5_LTBP2, " to ", absdiffP97.5_LTBP2, ")"),
    abs_inf3_summary = paste0(absdiffMedian_LTBP3, " (", absdiffP2.5_LTBP3, " to ", absdiffP97.5_LTBP3, ")"),
    abs_inf4_summary = paste0(absdiffMedian_LTBP4, " (", absdiffP2.5_LTBP4, " to ", absdiffP97.5_LTBP4, ")"),
    abs_inf5_summary = paste0(absdiffMedian_LTBP5, " (", absdiffP2.5_LTBP5, " to ", absdiffP97.5_LTBP5, ")"),
    
    rel_inf1_summary = paste0(reldiffMedian_LTBP1, " (", reldiffP2.5_LTBP1, " to ", reldiffP97.5_LTBP1, ")"),
    rel_inf2_summary = paste0(reldiffMedian_LTBP2, " (", reldiffP2.5_LTBP2, " to ", reldiffP97.5_LTBP2, ")"),
    rel_inf3_summary = paste0(reldiffMedian_LTBP3, " (", reldiffP2.5_LTBP3, " to ", reldiffP97.5_LTBP3, ")"),
    rel_inf4_summary = paste0(reldiffMedian_LTBP4, " (", reldiffP2.5_LTBP4, " to ", reldiffP97.5_LTBP4, ")"),
    rel_inf5_summary = paste0(reldiffMedian_LTBP5, " (", reldiffP2.5_LTBP5, " to ", reldiffP97.5_LTBP5, ")"),
  )

recentlyinf_agegroup <- recentlyinf_agegroup %>%
  select(ISO3, CNSY, YARP, AgeGroup, NUMP, 
         Median_aNUMP_infsq, P2.5_aNUMP_infsq, P97.5_aNUMP_infsq, aNUMP_infsq_summary,
         Median_aNUMP_inf1, P2.5_aNUMP_inf1, P97.5_aNUMP_inf1, aNUMP_inf1_summary,
         Median_aNUMP_inf2, P2.5_aNUMP_inf2, P97.5_aNUMP_inf2, aNUMP_inf2_summary,
         Median_aNUMP_inf3, P2.5_aNUMP_inf3, P97.5_aNUMP_inf3, aNUMP_inf3_summary,
         Median_aNUMP_inf4, P2.5_aNUMP_inf4, P97.5_aNUMP_inf4, aNUMP_inf4_summary,
         Median_aNUMP_inf5, P2.5_aNUMP_inf5, P97.5_aNUMP_inf5, aNUMP_inf5_summary,
         absdiffMedian_LTBP1, absdiffP2.5_LTBP1, absdiffP97.5_LTBP1, abs_inf1_summary,
         absdiffMedian_LTBP2, absdiffP2.5_LTBP2, absdiffP97.5_LTBP2, abs_inf2_summary,
         absdiffMedian_LTBP3, absdiffP2.5_LTBP3, absdiffP97.5_LTBP3, abs_inf3_summary,
         absdiffMedian_LTBP4, absdiffP2.5_LTBP4, absdiffP97.5_LTBP4, abs_inf4_summary,
         absdiffMedian_LTBP5, absdiffP2.5_LTBP5, absdiffP97.5_LTBP5, abs_inf5_summary,
         reldiffMedian_LTBP1, reldiffP2.5_LTBP1, reldiffP97.5_LTBP1, rel_inf1_summary,
         reldiffMedian_LTBP2, reldiffP2.5_LTBP2, reldiffP97.5_LTBP2, rel_inf2_summary,
         reldiffMedian_LTBP3, reldiffP2.5_LTBP3, reldiffP97.5_LTBP3, rel_inf3_summary,
         reldiffMedian_LTBP4, reldiffP2.5_LTBP4, reldiffP97.5_LTBP4, rel_inf4_summary,
         reldiffMedian_LTBP5, reldiffP2.5_LTBP5, reldiffP97.5_LTBP5, rel_inf5_summary)

saveRDS(recentlyinf_agegroup , file = paste0(path.out, "recently_inf_summary(agegroup).rds"))

         

#Make tables================================================

#Create function
create_flextable <- function(data) {
  ft <- flextable(data) %>%
    autofit() %>%
    theme_vanilla() %>%
    set_header_labels(values = colnames(data)) %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Calibri", part = "all") %>% 
    fontsize(size = 8, part = "all") %>% 
    height_all(height = 0.1, part = "all") %>% 
    width(j = "ISO3", width = 0.50) %>% 
    width(j = "AgeGroup", width = 0.75) %>% 
    width(j = c("2024", "2030", "2040", "2050"), width = 1.25) %>% 
    merge_v(j = "ISO3") %>% 
    border_inner(part = "header", border = fp_border(color = "black", width = 1)) %>% 
    border_inner_v(border = fp_border(color = "black", width = 1), part = "all") 
  
  all_ages_rows <- which(data$AgeGroup == "All Ages")
  
  for (row in all_ages_rows) {
    ft <- ft %>%
      hline(i = row, border = fp_border(color = "black", width = 1))
  }
  
  return(ft)
}

#FOR ALL LTBP INFECTIONS=============================================================================================================================================
# Extract results for prop LTBP
result <- summary_stats_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, prob_LTBPsq_summary, prob_LTBP1_summary, 
         prob_LTBP2_summary, prob_LTBP3_summary, prob_LTBP4_summary, prob_LTBP5_summary) 

# Extract results for all age groups
result2 <- summary_stats_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values)%>%
  select(ISO3, CNSY, prob_LTBPsq_summary, prob_LTBP1_summary, 
         prob_LTBP2_summary, prob_LTBP3_summary, prob_LTBP4_summary, prob_LTBP5_summary) %>%
  mutate(AgeGroup = "All Ages") # Add an AgeGroup column with "All Ages"

# Combine result1 and result2 into a single dataset
combined_result <- bind_rows(result, result2)

# View the combined result
print(combined_result)


#Create all LTBP tables===================================================
table_prob_LTBPsq <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBPsq_summary) %>%  
  pivot_wider(names_from = CNSY, values_from = prob_LTBPsq_summary) %>%  
  arrange(ISO3, AgeGroup) 


table_prob_LTBP1 <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBP1_summary) %>%  
  pivot_wider(names_from = CNSY, values_from = prob_LTBP1_summary) %>%  
  arrange(ISO3, AgeGroup)


table_prob_LTBP2 <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBP2_summary) %>% 
  pivot_wider(names_from = CNSY, values_from = prob_LTBP2_summary) %>%  
  arrange(ISO3, AgeGroup)


table_prob_LTBP3 <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBP3_summary) %>%  
  pivot_wider(names_from = CNSY, values_from = prob_LTBP3_summary) %>%  
  arrange(ISO3, AgeGroup)


table_prob_LTBP4 <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBP4_summary) %>%  
  pivot_wider(names_from = CNSY, values_from = prob_LTBP4_summary) %>%  
  arrange(ISO3, AgeGroup)


table_prob_LTBP5 <- combined_result %>%
  select(ISO3, AgeGroup, CNSY, prob_LTBP5_summary) %>% 
  pivot_wider(names_from = CNSY, values_from = prob_LTBP5_summary) %>%  
  arrange(ISO3, AgeGroup)


# Extract absdiff results=============================================================================================================================================
absdiff_result <- summary_stats_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, absdiff_LTBP1_summary, absdiff_LTBP2_summary, absdiff_LTBP3_summary, 
         absdiff_LTBP4_summary, absdiff_LTBP5_summary)

# Extract absdiff results for all age groups
absdiff_result2 <- summary_stats_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values) %>%
  select(ISO3, CNSY, absdiff_LTBP1_summary, absdiff_LTBP2_summary, absdiff_LTBP3_summary, 
         absdiff_LTBP4_summary, absdiff_LTBP5_summary) %>%
  mutate(AgeGroup = "All Ages") 

# Combine absdiff results
combined_absdiff_result <- bind_rows(absdiff_result, absdiff_result2)

# Create tables for absdiff LTBP values
table_absdiff_LTBP1 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, absdiff_LTBP1_summary) %>%
  pivot_wider(names_from = CNSY, values_from = absdiff_LTBP1_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_LTBP2 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, absdiff_LTBP2_summary) %>%
  pivot_wider(names_from = CNSY, values_from = absdiff_LTBP2_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_LTBP3 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, absdiff_LTBP3_summary) %>%
  pivot_wider(names_from = CNSY, values_from = absdiff_LTBP3_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_LTBP4 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, absdiff_LTBP4_summary) %>%
  pivot_wider(names_from = CNSY, values_from = absdiff_LTBP4_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_LTBP5 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, absdiff_LTBP5_summary) %>%
  pivot_wider(names_from = CNSY, values_from = absdiff_LTBP5_summary) %>%
  arrange(ISO3, AgeGroup)

# Extract reldiff results============================================================================================================================
reldiff_result <- summary_stats_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, reldiff_LTBP1_summary, reldiff_LTBP2_summary, reldiff_LTBP3_summary, 
         reldiff_LTBP4_summary, reldiff_LTBP5_summary)

# Extract reldiff results for all age groups
reldiff_result2 <- summary_stats_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values) %>%
  select(ISO3, CNSY, reldiff_LTBP1_summary, reldiff_LTBP2_summary, reldiff_LTBP3_summary, 
         reldiff_LTBP4_summary, reldiff_LTBP5_summary) %>%
  mutate(AgeGroup = "All Ages") 

# Combine reldiff results
combined_reldiff_result <- bind_rows(reldiff_result, reldiff_result2)

# Create tables for reldiff LTBP values
table_reldiff_LTBP1 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, reldiff_LTBP1_summary) %>%
  pivot_wider(names_from = CNSY, values_from = reldiff_LTBP1_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_LTBP2 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, reldiff_LTBP2_summary) %>%
  pivot_wider(names_from = CNSY, values_from = reldiff_LTBP2_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_LTBP3 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, reldiff_LTBP3_summary) %>%
  pivot_wider(names_from = CNSY, values_from = reldiff_LTBP3_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_LTBP4 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, reldiff_LTBP4_summary) %>%
  pivot_wider(names_from = CNSY, values_from = reldiff_LTBP4_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_LTBP5 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, reldiff_LTBP5_summary) %>%
  pivot_wider(names_from = CNSY, values_from = reldiff_LTBP5_summary) %>%
  arrange(ISO3, AgeGroup)

#Create table on word=============================

ft_LTBPsq <- create_flextable(table_prob_LTBPsq)
ft_LTBP1 <- create_flextable(table_prob_LTBP1)
ft_LTBP2 <- create_flextable(table_prob_LTBP2)
ft_LTBP3 <- create_flextable(table_prob_LTBP3)
ft_LTBP4 <- create_flextable(table_prob_LTBP4)
ft_LTBP5 <- create_flextable(table_prob_LTBP5)

ft_absdiff_LTBP1 <- create_flextable(table_absdiff_LTBP1)
ft_absdiff_LTBP2 <- create_flextable(table_absdiff_LTBP2)
ft_absdiff_LTBP3 <- create_flextable(table_absdiff_LTBP3)
ft_absdiff_LTBP4 <- create_flextable(table_absdiff_LTBP4)
ft_absdiff_LTBP5 <- create_flextable(table_absdiff_LTBP5)

ft_reldiff_LTBP1 <- create_flextable(table_reldiff_LTBP1)
ft_reldiff_LTBP2 <- create_flextable(table_reldiff_LTBP2)
ft_reldiff_LTBP3 <- create_flextable(table_reldiff_LTBP3)
ft_reldiff_LTBP4 <- create_flextable(table_reldiff_LTBP4)
ft_reldiff_LTBP5 <- create_flextable(table_reldiff_LTBP5)

doc <- read_docx()
doc <- doc %>%
  body_add_par("Table: LTBPsq prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBPsq) %>%
  body_add_par("Table: LTBP1 prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBP1) %>%
  body_add_par("Table: LTBP2 prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBP2) %>%
  body_add_par("Table: LTBP3 prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBP3) %>%
  body_add_par("Table: LTBP4 prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBP4) %>%
  body_add_par("Table: LTBP5 prop Summary", style = "heading 1") %>%
  body_add_flextable(ft_LTBP5) %>%
  body_add_par("Table: LTBP1 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_LTBP1) %>%
  body_add_par("Table: LTBP2 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_LTBP2) %>%
  body_add_par("Table: LTBP3 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_LTBP3) %>%
  body_add_par("Table: LTBP4 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_LTBP4) %>%
  body_add_par("Table: LTBP5 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_LTBP5) %>%
  body_add_par("Table: LTBP1 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_LTBP1) %>%
  body_add_par("Table: LTBP2 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_LTBP2) %>%
  body_add_par("Table: LTBP3 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_LTBP3) %>%
  body_add_par("Table: LTBP4 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_LTBP4) %>%
  body_add_par("Table: LTBP5 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_LTBP5)

# Save the document
print(doc, target = "LTBP_Summary_Tables.docx")



#NOW FOR RECENTLY INFECTED=========================================================================================================================
# Extract prop results
prop_result <- recentlyinf_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, aNUMP_infsq_summary, aNUMP_inf1_summary, aNUMP_inf2_summary, 
         aNUMP_inf3_summary, aNUMP_inf4_summary, aNUMP_inf5_summary)

# Extract prop results for all age groups
prop_result2 <- recentlyinf_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values) %>%
  select(ISO3, CNSY, aNUMP_infsq_summary, aNUMP_inf1_summary, aNUMP_inf2_summary, 
         aNUMP_inf3_summary, aNUMP_inf4_summary, aNUMP_inf5_summary) %>%
  mutate(AgeGroup = "All Ages") 

# Combine prop results
combined_prop_result <- bind_rows(prop_result, prop_result2)

# Create tables for prop LTBP values
table_prob_infsq <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_infsq_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_infsq_summary) %>%
  arrange(ISO3, AgeGroup)

table_prob_inf1 <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_inf1_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_inf1_summary) %>%
  arrange(ISO3, AgeGroup)

table_prob_inf2 <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_inf2_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_inf2_summary) %>%
  arrange(ISO3, AgeGroup)

table_prob_inf3 <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_inf3_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_inf3_summary) %>%
  arrange(ISO3, AgeGroup)

table_prob_inf4 <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_inf4_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_inf4_summary) %>%
  arrange(ISO3, AgeGroup)

table_prob_inf5 <- combined_prop_result %>%
  select(ISO3, AgeGroup, CNSY, aNUMP_inf5_summary) %>%
  pivot_wider(names_from = CNSY, values_from = aNUMP_inf5_summary) %>%
  arrange(ISO3, AgeGroup)

# Extract absdiff results
absdiff_result <- recentlyinf_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, abs_inf1_summary, abs_inf2_summary, abs_inf3_summary, 
         abs_inf4_summary, abs_inf5_summary)

# Extract absdiff results for all age groups
absdiff_result2 <- recentlyinf_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values) %>%
  select(ISO3, CNSY, abs_inf1_summary, abs_inf2_summary, abs_inf3_summary, 
         abs_inf4_summary, abs_inf5_summary) %>%
  mutate(AgeGroup = "All Ages") # Add an AgeGroup column with "All Ages"

# Combine absdiff results
combined_absdiff_result <- bind_rows(absdiff_result, absdiff_result2)

# Create tables for absdiff LTBP values
table_absdiff_inf1 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, abs_inf1_summary) %>%
  pivot_wider(names_from = CNSY, values_from = abs_inf1_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_inf2 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, abs_inf2_summary) %>%
  pivot_wider(names_from = CNSY, values_from = abs_inf2_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_inf3 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, abs_inf3_summary) %>%
  pivot_wider(names_from = CNSY, values_from = abs_inf3_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_inf4 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, abs_inf4_summary) %>%
  pivot_wider(names_from = CNSY, values_from = abs_inf4_summary) %>%
  arrange(ISO3, AgeGroup)

table_absdiff_inf5 <- combined_absdiff_result %>%
  select(ISO3, AgeGroup, CNSY, abs_inf5_summary) %>%
  pivot_wider(names_from = CNSY, values_from = abs_inf5_summary) %>%
  arrange(ISO3, AgeGroup)

# Extract reldiff results
reldiff_result <- recentlyinf_agegroup %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values, 
         AgeGroup %in% age_groups) %>%
  select(ISO3, CNSY, AgeGroup, rel_inf1_summary, rel_inf2_summary, rel_inf3_summary, 
         rel_inf4_summary, rel_inf5_summary)

# Extract reldiff results for all age groups
reldiff_result2 <- recentlyinf_all_ages %>%
  filter(ISO3 %in% countries, 
         CNSY %in% cnsy_values) %>%
  select(ISO3, CNSY, rel_inf1_summary, rel_inf2_summary, rel_inf3_summary, 
         rel_inf4_summary, rel_inf5_summary) %>%
  mutate(AgeGroup = "All Ages") # Add an AgeGroup column with "All Ages"

# Combine reldiff results
combined_reldiff_result <- bind_rows(reldiff_result, reldiff_result2)

# Create tables for reldiff LTBP values
table_reldiff_inf1 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, rel_inf1_summary) %>%
  pivot_wider(names_from = CNSY, values_from = rel_inf1_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_inf2 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, rel_inf2_summary) %>%
  pivot_wider(names_from = CNSY, values_from = rel_inf2_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_inf3 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, rel_inf3_summary) %>%
  pivot_wider(names_from = CNSY, values_from = rel_inf3_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_inf4 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, rel_inf4_summary) %>%
  pivot_wider(names_from = CNSY, values_from = rel_inf4_summary) %>%
  arrange(ISO3, AgeGroup)

table_reldiff_inf5 <- combined_reldiff_result %>%
  select(ISO3, AgeGroup, CNSY, rel_inf5_summary) %>%
  pivot_wider(names_from = CNSY, values_from = rel_inf5_summary) %>%
  arrange(ISO3, AgeGroup)

ft_prob_infsq <- create_flextable(table_prob_infsq)
ft_prob_inf1 <- create_flextable(table_prob_inf1)
ft_prob_inf2 <- create_flextable(table_prob_inf2)
ft_prob_inf3 <- create_flextable(table_prob_inf3)
ft_prob_inf4 <- create_flextable(table_prob_inf4)
ft_prob_inf5 <- create_flextable(table_prob_inf5)

ft_absdiff_inf1 <- create_flextable(table_absdiff_inf1)
ft_absdiff_inf2 <- create_flextable(table_absdiff_inf2)
ft_absdiff_inf3 <- create_flextable(table_absdiff_inf3)
ft_absdiff_inf4 <- create_flextable(table_absdiff_inf4)
ft_absdiff_inf5 <- create_flextable(table_absdiff_inf5)

ft_reldiff_inf1 <- create_flextable(table_reldiff_inf1)
ft_reldiff_inf2 <- create_flextable(table_reldiff_inf2)
ft_reldiff_inf3 <- create_flextable(table_reldiff_inf3)
ft_reldiff_inf4 <- create_flextable(table_reldiff_inf4)
ft_reldiff_inf5 <- create_flextable(table_reldiff_inf5)

doc1 <- read_docx()
doc1 <- doc %>%
  body_add_par("Table: prop newly infsq Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_infsq) %>%
  body_add_par("Table: prop newly inf1 Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_inf1) %>%
  body_add_par("Table: prop newlyinf2 Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_inf2) %>%
  body_add_par("Table: prop newly inf3 Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_inf3) %>%
  body_add_par("Table: prop newly inf4 Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_inf4) %>%
  body_add_par("Table: prop newly inf5 Summary", style = "heading 1") %>%
  body_add_flextable(ft_prob_inf5) %>%
  body_add_par("Table: newly inf1 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_inf1) %>%
  body_add_par("Table: newly inf2 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_inf2) %>%
  body_add_par("Table: newly inf3 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_inf3) %>%
  body_add_par("Table: newly inf4 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_inf4) %>%
  body_add_par("Table: newly inf5 Abs Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_absdiff_inf5) %>%
  body_add_par("Table: newly inf1 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_inf1) %>%
  body_add_par("Table: newly inf2 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_inf2) %>%
  body_add_par("Table: newly inf3 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_inf3) %>%
  body_add_par("Table: newly inf4 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_inf4) %>%
  body_add_par("Table: newly inf5 Rel Diff Summary", style = "heading 1") %>%
  body_add_flextable(ft_reldiff_inf5)

# Save the document
print(doc1, target = "Newly_inf_Summary_Tables.docx")

#recently infected specifically for graph===================================================================================
# recently infected
#recently inf all ages===============================================================================================================
Sum_NUMP_RI_all_ages <- Sum_NUMP_RI [, .(
  NUMP = sum(NUMP, na.rm = TRUE),
  NUMP_infsq = sum(NUMP_infsq, na.rm = TRUE),
  NUMP_inf1 = sum(NUMP_inf1, na.rm = TRUE),
  NUMP_inf2 = sum(NUMP_inf2, na.rm = TRUE),
  NUMP_inf3 = sum(NUMP_inf3, na.rm = TRUE),
  NUMP_inf4 = sum(NUMP_inf4, na.rm = TRUE),
  NUMP_inf5 = sum(NUMP_inf5, na.rm = TRUE)
), by = .(ISO3, CNSY, YARP, replicate)]

Sum_NUMP_RI_all_ages <- Sum_NUMP_RI_all_ages %>%
  mutate(aNUMP_infsq = NUMP_infsq / NUMP,
         aNUMP_inf1 = NUMP_inf1 / NUMP,
         aNUMP_inf2 = NUMP_inf2 / NUMP,
         aNUMP_inf3 = NUMP_inf3 / NUMP,
         aNUMP_inf4 = NUMP_inf4 / NUMP,
         aNUMP_inf5 = NUMP_inf5 / NUMP) %>%
  select(-c(NUMP_inf5, NUMP_inf4, NUMP_inf3, NUMP_inf2, NUMP_inf1, NUMP_infsq))

Sum_NUMP_RI_all_ages <- as.data.table(Sum_NUMP_RI_all_ages)
#before this calculate abs_inf by dividing NUMPinf_sq by NUMP then do abd and rel
Sum_NUMP_RI_all_ages <- Sum_NUMP_RI_all_ages %>%
  mutate(abs_inf1 = aNUMP_inf1 - aNUMP_infsq,
         abs_inf2 = aNUMP_inf2 - aNUMP_infsq,
         abs_inf3 = aNUMP_inf3 - aNUMP_infsq,
         abs_inf4 = aNUMP_inf4 - aNUMP_infsq,
         abs_inf5 = aNUMP_inf5 - aNUMP_infsq,
         rel_inf1 = abs_inf1 / aNUMP_infsq,
         rel_inf2 = abs_inf2 / aNUMP_infsq,
         rel_inf3 = abs_inf3 / aNUMP_infsq,
         rel_inf4 = abs_inf4 / aNUMP_infsq,
         rel_inf5 = abs_inf5 / aNUMP_infsq)

recentlyinf_all_ages <-  Sum_NUMP_RI_all_ages[, .(
  NUMP = round(mean(NUMP, na.rm = TRUE), 0),
  Median_aNUMP_infsq = round(median(aNUMP_infsq, na.rm = TRUE), 6),
  P2.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_infsq = round(quantile(aNUMP_infsq, 0.975, na.rm = TRUE), 6),
  
  Median_aNUMP_inf1 = round(median(aNUMP_inf1, na.rm = TRUE), 6),
  P2.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_inf1 = round(quantile(aNUMP_inf1, 0.975, na.rm = TRUE), 6),
  
  Median_aNUMP_inf2 = round(median(aNUMP_inf2, na.rm = TRUE), 6),
  P2.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_inf2 = round(quantile(aNUMP_inf2, 0.975, na.rm = TRUE), 6),
  
  Median_aNUMP_inf3 = round(median(aNUMP_inf3, na.rm = TRUE), 6),
  P2.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_inf3 = round(quantile(aNUMP_inf3, 0.975, na.rm = TRUE), 6),
  
  Median_aNUMP_inf4 = round(median(aNUMP_inf4, na.rm = TRUE), 6),
  P2.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_inf4 = round(quantile(aNUMP_inf4, 0.975, na.rm = TRUE), 6),
  
  Median_aNUMP_inf5 = round(median(aNUMP_inf5, na.rm = TRUE), 6),
  P2.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.025, na.rm = TRUE), 6),
  P97.5_aNUMP_inf5 = round(quantile(aNUMP_inf5, 0.975, na.rm = TRUE), 6),
  
  absdiffMedian_LTBP1 = round(median(abs_inf1, na.rm = TRUE), 6),
  absdiffP2.5_LTBP1 = round(quantile(abs_inf1, 0.025, na.rm = TRUE), 6),
  absdiffP97.5_LTBP1 =round(quantile(abs_inf1, 0.975, na.rm = TRUE), 6),
  
  absdiffMedian_LTBP2 = round(median(abs_inf2, na.rm = TRUE), 6),
  absdiffP2.5_LTBP2 = round(quantile(abs_inf2, 0.025, na.rm = TRUE), 6),
  absdiffP97.5_LTBP2 =round(quantile(abs_inf2, 0.975, na.rm = TRUE), 6),
  
  absdiffMedian_LTBP3 = round(median(abs_inf3, na.rm = TRUE), 6),
  absdiffP2.5_LTBP3 = round(quantile(abs_inf3, 0.025, na.rm = TRUE), 6),
  absdiffP97.5_LTBP3 =round(quantile(abs_inf3, 0.975, na.rm = TRUE), 6),
  
  absdiffMedian_LTBP4 = round(median(abs_inf4, na.rm = TRUE), 6),
  absdiffP2.5_LTBP4 = round(quantile(abs_inf4, 0.025, na.rm = TRUE), 6),
  absdiffP97.5_LTBP4 =round(quantile(abs_inf4, 0.975, na.rm = TRUE), 6),
  
  absdiffMedian_LTBP5 = round(median(abs_inf5, na.rm = TRUE), 6),
  absdiffP2.5_LTBP5 = round(quantile(abs_inf5, 0.025, na.rm = TRUE), 6),
  absdiffP97.5_LTBP5 =round(quantile(abs_inf5, 0.975, na.rm = TRUE), 6),
  
  reldiffMedian_LTBP1 = round(median(rel_inf1, na.rm = TRUE), 6),
  reldiffP2.5_LTBP1 = round(quantile(rel_inf1, 0.025, na.rm = TRUE), 6),
  reldiffP97.5_LTBP1 = round(quantile(rel_inf1, 0.975, na.rm = TRUE), 6),
  
  reldiffMedian_LTBP2 = round(median(rel_inf2, na.rm = TRUE), 6),
  reldiffP2.5_LTBP2 = round(quantile(rel_inf2, 0.025, na.rm = TRUE), 6),
  reldiffP97.5_LTBP2 = round(quantile(rel_inf2, 0.975, na.rm = TRUE), 3),
  
  reldiffMedian_LTBP3 = round(median(rel_inf3, na.rm = TRUE), 3),
  reldiffP2.5_LTBP3 = round(quantile(rel_inf3, 0.025, na.rm = TRUE), 6),
  reldiffP97.5_LTBP3 = round(quantile(rel_inf3, 0.975, na.rm = TRUE), 6),
  
  reldiffMedian_LTBP4 = round(median(rel_inf4, na.rm = TRUE), 6),
  reldiffP2.5_LTBP4 = round(quantile(rel_inf4, 0.025, na.rm = TRUE), 6),
  reldiffP97.5_LTBP4 = round(quantile(rel_inf4, 0.975, na.rm = TRUE), 6),
  
  reldiffMedian_LTBP5 = round(median(rel_inf5, na.rm = TRUE), 6),
  reldiffP2.5_LTBP5 = round(quantile(rel_inf5, 0.025, na.rm = TRUE), 6),
  reldiffP97.5_LTBP5 = round(quantile(rel_inf5, 0.975, na.rm = TRUE), 6)
), by = .(ISO3, CNSY, YARP)]


recentlyinf_all_ages <- recentlyinf_all_ages  %>%
  mutate(
    aNUMP_infsq_summary = paste0(Median_aNUMP_infsq, " (", P2.5_aNUMP_infsq, " to ", P97.5_aNUMP_infsq, ")"),
    aNUMP_inf1_summary = paste0(Median_aNUMP_inf1, " (", P2.5_aNUMP_inf1, " to ", P97.5_aNUMP_inf1, ")"),
    aNUMP_inf2_summary = paste0(Median_aNUMP_inf2, " (", P2.5_aNUMP_inf2, " to ", P97.5_aNUMP_inf2, ")"),
    aNUMP_inf3_summary = paste0(Median_aNUMP_inf3, " (", P2.5_aNUMP_inf3, " to ", P97.5_aNUMP_inf3, ")"),
    aNUMP_inf4_summary = paste0(Median_aNUMP_inf4, " (", P2.5_aNUMP_inf4, " to ", P97.5_aNUMP_inf4, ")"),
    aNUMP_inf5_summary = paste0(Median_aNUMP_inf5, " (", P2.5_aNUMP_inf5, " to ", P97.5_aNUMP_inf5, ")"),
    
    abs_inf1_summary = paste0(absdiffMedian_LTBP1, " (", absdiffP2.5_LTBP1, " to ", absdiffP97.5_LTBP1, ")"),
    abs_inf2_summary = paste0(absdiffMedian_LTBP2, " (", absdiffP2.5_LTBP2, " to ", absdiffP97.5_LTBP2, ")"),
    abs_inf3_summary = paste0(absdiffMedian_LTBP3, " (", absdiffP2.5_LTBP3, " to ", absdiffP97.5_LTBP3, ")"),
    abs_inf4_summary = paste0(absdiffMedian_LTBP4, " (", absdiffP2.5_LTBP4, " to ", absdiffP97.5_LTBP4, ")"),
    abs_inf5_summary = paste0(absdiffMedian_LTBP5, " (", absdiffP2.5_LTBP5, " to ", absdiffP97.5_LTBP5, ")"),
    
    rel_inf1_summary = paste0(reldiffMedian_LTBP1, " (", reldiffP2.5_LTBP1, " to ", reldiffP97.5_LTBP1, ")"),
    rel_inf2_summary = paste0(reldiffMedian_LTBP2, " (", reldiffP2.5_LTBP2, " to ", reldiffP97.5_LTBP2, ")"),
    rel_inf3_summary = paste0(reldiffMedian_LTBP3, " (", reldiffP2.5_LTBP3, " to ", reldiffP97.5_LTBP3, ")"),
    rel_inf4_summary = paste0(reldiffMedian_LTBP4, " (", reldiffP2.5_LTBP4, " to ", reldiffP97.5_LTBP4, ")"),
    rel_inf5_summary = paste0(reldiffMedian_LTBP5, " (", reldiffP2.5_LTBP5, " to ", reldiffP97.5_LTBP5, ")"),
  )

recentlyinf_all_ages <- recentlyinf_all_ages  %>%
  select(ISO3, CNSY, YARP, NUMP, 
         Median_aNUMP_infsq, P2.5_aNUMP_infsq, P97.5_aNUMP_infsq, aNUMP_infsq_summary,
         Median_aNUMP_inf1, P2.5_aNUMP_inf1, P97.5_aNUMP_inf1, aNUMP_inf1_summary,
         Median_aNUMP_inf2, P2.5_aNUMP_inf2, P97.5_aNUMP_inf2, aNUMP_inf2_summary,
         Median_aNUMP_inf3, P2.5_aNUMP_inf3, P97.5_aNUMP_inf3, aNUMP_inf3_summary,
         Median_aNUMP_inf4, P2.5_aNUMP_inf4, P97.5_aNUMP_inf4, aNUMP_inf4_summary,
         Median_aNUMP_inf5, P2.5_aNUMP_inf5, P97.5_aNUMP_inf5, aNUMP_inf5_summary,
         absdiffMedian_LTBP1, absdiffP2.5_LTBP1, absdiffP97.5_LTBP1, abs_inf1_summary,
         absdiffMedian_LTBP2, absdiffP2.5_LTBP2, absdiffP97.5_LTBP2, abs_inf2_summary,
         absdiffMedian_LTBP3, absdiffP2.5_LTBP3, absdiffP97.5_LTBP3, abs_inf3_summary,
         absdiffMedian_LTBP4, absdiffP2.5_LTBP4, absdiffP97.5_LTBP4, abs_inf4_summary,
         absdiffMedian_LTBP5, absdiffP2.5_LTBP5, absdiffP97.5_LTBP5, abs_inf5_summary,
         reldiffMedian_LTBP1, reldiffP2.5_LTBP1, reldiffP97.5_LTBP1, rel_inf1_summary,
         reldiffMedian_LTBP2, reldiffP2.5_LTBP2, reldiffP97.5_LTBP2, rel_inf2_summary,
         reldiffMedian_LTBP3, reldiffP2.5_LTBP3, reldiffP97.5_LTBP3, rel_inf3_summary,
         reldiffMedian_LTBP4, reldiffP2.5_LTBP4, reldiffP97.5_LTBP4, rel_inf4_summary,
         reldiffMedian_LTBP5, reldiffP2.5_LTBP5, reldiffP97.5_LTBP5, rel_inf5_summary)

saveRDS(recentlyinf_all_ages , file = paste0(path.out, "recently_inf_summary(all_ages_graph).rds"))

