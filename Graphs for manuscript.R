library(flextable)
library(officer)
library(dplyr)
library(data.table)
library(ggplot2)


#Code for the manuscript

#' Data inputs file path
path.in <- "C:/Users/miche/Downloads/ARI project/"

#' Output file path
path.out <- "C:/Users/miche/Downloads/ARI project/"

#ARI graphs====================================================================================================
ari <- readRDS(paste0(path.out, "ARI_results.rds"))

ari_graph_data <- ari %>%
  select(-c(ari_sq_summary, ari_1_summary, ari_2_summary, ari_3_summary, ari_4_summary, ari_5_summary)) %>%
  filter(year > 2023)
colnames(ari_graph_data)  

ari_graph_long <- melt(
  ari_graph_data,
  id.vars = c("iso3", "year"),
  measure.vars = list(
    Median = grep("^Median_ari_", names(ari_graph_data)),
    Lower = grep("^P2.5_ari_", names(ari_graph_data)),
    Upper = grep("^P97.5_ari_", names(ari_graph_data))
  ),
  variable.name = "ARI_Type"
)

ari_graph_long <- ari_graph_long %>%
  mutate(
    ARI_Type = factor(ARI_Type, levels = c("1", "2", "3", "4", "5", "6")),
    ARI_Type = recode(ARI_Type,
                       "1" = "Status Quo",
                       "2" = "1% Decrease",
                       "3" = "2% Decrease",
                       "4" = "3% Decrease",
                       "5" = "4% Decrease",
                       "6" = "5% Decrease")
  )

ari_graph_long <- ari_graph_long %>%
  mutate(ISO3 = recode(iso3, "IND" = "India", "CHN" = "China", "PHL" = "Philippines", "VNM" = "Vietnam"))

# Plotting with ggplot2
ggplot(ari_graph_long, aes(x = year, y = Median, color = ARI_Type, fill = ARI_Type)) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ iso3, scales = "fixed") +
  labs(
    title = "ARI Trends by Country from 2024 to 2050",
    x = "Year",
    y = "ARI Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),  
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    panel.spacing = unit(1, "lines"), 
    axis.text = element_text(size = 9),  
    axis.title = element_text(size = 10, face = "bold")
  ) +
  guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))

#Prop graphs====================================================================================================
propdata <- readRDS(paste0(path.out, "census_summary_stats_allages.rds"))


prop_graph_data <- propdata %>%
  select(c(ISO3, CNSY, probMedian_LTBPsq, probMedian_LTBP1, probMedian_LTBP2, probMedian_LTBP3, probMedian_LTBP4, probMedian_LTBP5,
           probP2.5_LTBPsq, probP2.5_LTBP1, probP2.5_LTBP2, probP2.5_LTBP3, probP2.5_LTBP4, probP2.5_LTBP5,
           probP97.5_LTBPsq, probP97.5_LTBP1, probP97.5_LTBP2, probP97.5_LTBP3, probP97.5_LTBP4, probP97.5_LTBP5))%>%
  filter(CNSY > 2023)

prop_graph_long <- melt(
  prop_graph_data,
  id.vars = c("ISO3", "CNSY"),
  measure.vars = list(
    "Prop_Median" = grep("^probMedian_LTBP", names(prop_graph_data)),
    "CI_Lower" = grep("^probP2.5_LTBP", names(prop_graph_data)),
    "CI_Upper" = grep("^probP97.5_LTBP", names(prop_graph_data))
  ),
  variable.name = "Prop_Type"
)


# Rename Prop_Type values and reorder levels
prop_graph_long <- prop_graph_long %>%
  mutate(
    Prop_Type = factor(Prop_Type, levels = c("1", "2", "3", "4", "5", "6")),
    Prop_Type = recode(Prop_Type,
                       "1" = "Status Quo",
                       "2" = "1% Decrease",
                       "3" = "2% Decrease",
                       "4" = "3% Decrease",
                       "5" = "4% Decrease",
                       "6" = "5% Decrease")
  )

prop_graph_long <- prop_graph_long %>%
  mutate(ISO3 = recode(ISO3, "IND" = "India", "CHN" = "China", "PHL" = "Philippines", "VNM" = "Vietnam"))


# Plotting with ggplot2
ggplot(prop_graph_long, aes(x = CNSY, y = Prop_Median, color = Prop_Type, fill = Prop_Type)) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ ISO3, scales = "fixed") +
  labs(
    title = "Proportion of LTB Infections by Country from 2024 to 2050",
    x = "Year",
    y = "Proportion of LTB Infections"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold")
  ) +
  guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))

#Newly inf prop graph ====================================================================================================
newlydata <- readRDS(paste0(path.out, "recently_inf_summary(all_ages_graph).rds"))
colnames(newlydata)

newly_graph_data <- newlydata %>%
  select(c(ISO3, CNSY, Median_aNUMP_infsq, Median_aNUMP_inf1, Median_aNUMP_inf2, Median_aNUMP_inf3, Median_aNUMP_inf4, Median_aNUMP_inf5,
           P2.5_aNUMP_infsq, P2.5_aNUMP_inf1, P2.5_aNUMP_inf2, P2.5_aNUMP_inf3, P2.5_aNUMP_inf4, P2.5_aNUMP_inf5,
           P97.5_aNUMP_infsq, P97.5_aNUMP_inf1, P97.5_aNUMP_inf2, P97.5_aNUMP_inf3, P97.5_aNUMP_inf4, P97.5_aNUMP_inf5))%>%
  filter(CNSY > 2023)

newly_graph_long <- melt(
  newly_graph_data,
  id.vars = c("ISO3", "CNSY"),
  measure.vars = list(
    "Prop_Median" = grep("^Median_aNUMP_inf", names(newly_graph_data)),
    "CI_Lower" = grep("^P2.5_aNUMP_inf", names(newly_graph_data)),
    "CI_Upper" = grep("^P97.5_aNUMP_inf", names(newly_graph_data))
  ),
  variable.name = "Prop_Type",
  value.name = c("Prop_Median", "CI_Lower", "CI_Upper")
)


# Rename Prop_Type values and reorder levels
newly_graph_long <- newly_graph_long  %>%
  mutate(
    Prop_Type = factor(Prop_Type, levels = c("1", "2", "3", "4", "5", "6")),
    Prop_Type = recode(Prop_Type,
                       "1" = "Status Quo",
                       "2" = "1% Decrease",
                       "3" = "2% Decrease",
                       "4" = "3% Decrease",
                       "5" = "4% Decrease",
                       "6" = "5% Decrease")
  )

newly_graph_long <- newly_graph_long  %>%
  mutate(ISO3 = recode(ISO3, "IND" = "India", "CHN" = "China", "PHL" = "Philippines", "VNM" = "Vietnam"))


# Plotting with ggplot2
ggplot(newly_graph_long , aes(x = CNSY, y = Prop_Median, color = Prop_Type, fill = Prop_Type)) +
  geom_line(size = 1) +
  #geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ ISO3, scales = "fixed") +
  labs(
    title = "Proportion of Recent LTB Infections by Country from 2024 to 2050",
    x = "Year",
    y = "Proportion of Recent LTB Infections"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10, face = "bold")
  ) +
  guides(color = guide_legend(ncol = 1), fill = guide_legend(ncol = 1))


#Rel inf prop graph ====================================================================================================
colnames(propdata)
relprop_graph_data <- propdata %>%
  filter(CNSY > 2023) %>%
  select(ISO3, CNSY, 
         reldiffMedian_LTBP1, reldiffMedian_LTBP2, reldiffMedian_LTBP3, reldiffMedian_LTBP4, reldiffMedian_LTBP5,
         reldiffP2.5_LTBP1, reldiffP2.5_LTBP2, reldiffP2.5_LTBP3, reldiffP2.5_LTBP4, reldiffP2.5_LTBP5,
         reldiffP97.5_LTBP1, reldiffP97.5_LTBP2, reldiffP97.5_LTBP3, reldiffP97.5_LTBP4, reldiffP97.5_LTBP5) %>%
  mutate(across(starts_with("reldiff"), ~ (. + 1) * 100)) %>%  # Adjust to percentage format
  mutate(Status_Quo = 100)  # Adding the Status Quo column as 100%

# Step 2: Reshape the data to long format
# Include the Status_Quo column by binding it with the other relative difference columns
relprop_graph_long <- melt(
  relprop_graph_data,
  id.vars = c("ISO3", "CNSY"),
  measure.vars = list(
    Median = c("Status_Quo", grep("^reldiffMedian_LTBP", names(relprop_graph_data), value = TRUE)),
    Lower = c("Status_Quo", grep("^reldiffP2.5_LTBP", names(relprop_graph_data), value = TRUE)),
    Upper = c("Status_Quo", grep("^reldiffP97.5_LTBP", names(relprop_graph_data), value = TRUE))
  ),
  variable.name = "Prop_Type",
  value.name = c("RelDiff_Median", "CI_Lower", "CI_Upper")
)


relprop_graph_long <- relprop_graph_long  %>%
  mutate(
    Prop_Type = factor(Prop_Type, levels = c("1", "2", "3", "4", "5", "6")),
    Prop_Type = recode(Prop_Type,
                       "1" = "Status Quo",
                       "2" = "1% Decrease",
                       "3" = "2% Decrease",
                       "4" = "3% Decrease",
                       "5" = "4% Decrease",
                       "6" = "5% Decrease")
  )

relprop_graph_long<- relprop_graph_long  %>%
  mutate(ISO3 = recode(ISO3, "IND" = "India", "CHN" = "China", "PHL" = "Philippines", "VNM" = "Vietnam"))

ggplot(relprop_graph_long, aes(x = CNSY, y = Median, color = Prop_Type, fill = Prop_Type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ ISO3, scales = "fixed") +
  labs(
    title = "Relative Difference in Proportion of LTB Infections by Country\n(2024-2050)",
    x = "Year",
    y = "Relative Proportion (Status Quo = 100%)"
  ) +
  ylim(0, 120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )


#Rel Newly inf prop graph ====================================================================================================
colnames(newlydata)
relnewly_graph_data <- newlydata %>%
  filter(CNSY > 2023) %>%
  select(ISO3, CNSY, 
         reldiffMedian_LTBP1, reldiffMedian_LTBP2, reldiffMedian_LTBP3, reldiffMedian_LTBP4, reldiffMedian_LTBP5,
         reldiffP2.5_LTBP1, reldiffP2.5_LTBP2, reldiffP2.5_LTBP3, reldiffP2.5_LTBP4, reldiffP2.5_LTBP5,
         reldiffP97.5_LTBP1, reldiffP97.5_LTBP2, reldiffP97.5_LTBP3, reldiffP97.5_LTBP4, reldiffP97.5_LTBP5) %>%
  mutate(across(starts_with("reldiff"), ~ (. + 1) * 100)) %>%  # Adjust to percentage format
  mutate(Status_Quo = 100)  # Adding the Status Quo column as 100%

# Step 2: Reshape the data to long format
# Include the Status_Quo column by binding it with the other relative difference columns
relnewly_graph_long <- melt(
  relnewly_graph_data,
  id.vars = c("ISO3", "CNSY"),
  measure.vars = list(
    Median = c("Status_Quo", grep("^reldiffMedian_LTBP", names(relnewly_graph_data), value = TRUE)),
    Lower = c("Status_Quo", grep("^reldiffP2.5_LTBP", names(relnewly_graph_data), value = TRUE)),
    Upper = c("Status_Quo", grep("^reldiffP97.5_LTBP", names(relnewly_graph_data), value = TRUE))
  ),
  variable.name = "Prop_Type",
  value.name = c("RelDiff_Median", "CI_Lower", "CI_Upper")
)


relnewly_graph_long <- relnewly_graph_long  %>%
  mutate(
    Prop_Type = factor(Prop_Type, levels = c("1", "2", "3", "4", "5", "6")),
    Prop_Type = recode(Prop_Type,
                       "1" = "Status Quo",
                       "2" = "1% Decrease",
                       "3" = "2% Decrease",
                       "4" = "3% Decrease",
                       "5" = "4% Decrease",
                       "6" = "5% Decrease")
  )

relnewly_graph_long<- relnewly_graph_long  %>%
  mutate(ISO3 = recode(ISO3, "IND" = "India", "CHN" = "China", "PHL" = "Philippines", "VNM" = "Vietnam"))

ggplot(relnewly_graph_long, aes(x = CNSY, y = Median, color = Prop_Type, fill = Prop_Type)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  facet_wrap(~ ISO3, scales = "fixed") +
  labs(
    title = "Relative Difference in Proportion of Recent LTB Infections by Country\n(2024-2050)",
    x = "Year",
    y = "Relative Proportion (Status Quo = 100%)"
  ) +
  ylim(0, 120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )




countries <- c("CHN", "VNM", "PHL", "IND")
cnsy_values <- c(2020, 2021, 2022, 2023, 2024, 2025) 

propcheck <- propdata %>%
 filter(ISO3 %in% countries, 
        CNSY %in% cnsy_values)%>%
 select(ISO3, CNSY, prob_LTBPsq_summary, prob_LTBP1_summary, 
        prob_LTBP2_summary, prob_LTBP3_summary, prob_LTBP4_summary, prob_LTBP5_summary) 


aricheck <- ari %>%
  filter(iso3 %in% countries, 
         year %in% cnsy_values)%>%
  select(iso3, year, Median_ari_sq, Median_ari_1, 
         Median_ari_2, Median_ari_3, Median_ari_4, Median_ari_5) 
