## Analyzing results of manipulative shading experiment
## Amelia V Hesketh
## January 2021

pkgs <- c("tidyverse", "lubridate", "car", "vegan", "plotrix", "patchwork",
          "lme4", "ggrepel")
lapply(pkgs, library, character.only = T)
rm(pkgs)

# read in generally useful data for later analyses
plot.info <- read_csv("../raw_data/shade/SBHW_SHADE_plot_info.csv") # plot-level details (treatments associated with plot#)
taxa <- read_csv("./raw_data/SBHW_taxonomic_20220105.csv") # taxonomic info (species codes etc.)

# Question 1: Did shading & barnacle removal treatments drive differences in substratum temperature?

# Only temp data during low tide are important, since only then will they diverge

# All temps recorded pretty much on the hour, so round to nearest hour to join with DFO tide data
temp.rounded <- read_csv("./clean_data/SBHW_TempSahsima.csv") %>% 
  mutate(hour = hour(date_time)) %>% separate(date_time, into = c("date","time"), sep = " ") %>% 
  unite(date_time, c(date, hour), sep = " ") %>% 
  mutate(date_time = ymd_h(date_time)) %>% select(-time)  

# Load in hourly tide data, convert ft to metres above chart datum
tides_sa <- read_delim("./raw_data/tides/tides_all_summer_SA.csv", 
                       col_names = c("date","junk","time","tz","tide_height_ft"), delim = " ") %>% 
  mutate(hour = hour(time)) %>% select(-junk, -tz, -time) %>% 
  unite(date_time, c(date, hour), sep = " ") %>% 
  mutate(date_time = ymd_h(date_time), tide_height_m = tide_height_ft * 0.3048) %>% 
  select(-tide_height_ft)

# To get the temperature at low tide, filter out when tide is above 1.4 m
# since the highest recorded shore level for an experimental plot was ~1.4 m
temp.lowtide <- temp.rounded %>% left_join(tides_sa) %>% filter(tide_height_m <= 1.4)

# Creating a plot of the mean maximum temperature over the whole experimental period
temp.summary <- temp.lowtide %>% separate(date_time, into = c("date","time"), 
                                          sep = " ", remove = F) %>% 
  group_by(date, block, treatment) %>% 
  summarize(max_temp = max(temperature_C)) %>% 
  mutate(date = date(date)) %>% ungroup() %>% group_by(date, treatment) %>% 
  summarize(mean.max.temp = mean(max_temp), se.max.temp = std.error(max_temp))

fig5a <- ggplot(data = temp.summary %>% filter(date >= "2021-05-01" & date < "2021-08-15") ,
                aes(x = date, y = mean.max.temp, col = treatment, fill = treatment)) + 
  geom_line() + theme_bw() + 
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  labs(x = "Date", y = "Daily maximum temperature (ºC)", 
       color = "Treatment", fill = "Treatment") + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + 
  annotate("rect", xmin = ymd("2021-06-25"), xmax = ymd("2021-06-29"), 
           ymin = 10, ymax = 45, fill = "black",  col = NA, alpha = 0.3) 

#+ geom_ribbon(aes(ymax = mean.max.temp + se.max.temp, ymin = mean.max.temp - se.max.temp), alpha = 0.3, color = NA) 

# And a plot of just the heat wave period, this time with hourly temperature
temp.hw <- temp.rounded %>% separate(date_time, c("date", "time"), " ", remove  = F) %>% 
  filter(date >="2021-06-25" & date <= "2021-06-29") %>% 
  group_by(treatment, date_time) %>% 
  summarize(mean.temp = mean(temperature_C), se.temp = std.error(temperature_C))

fig5b <- ggplot(data = temp.hw, aes(x = date_time, y = mean.temp, col = treatment, fill = treatment)) + 
  geom_line() + scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  labs(x = "Date", y = "Temperature (ºC)", fill = "Treatment", color = "Treatment") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14)) 

#+ geom_ribbon(aes(ymax = mean.temp + se.temp, ymin = mean.temp - se.temp), alpha = 0.3, color = NA) 

# And finally, a plot showing the differences in mean daily maximum temp between treatments

temp.summary2 <- temp.sep %>% group_by(treatment, date, block) %>% 
  summarize(mdmax_temp = max(temperature_C)) %>% 
  mutate(trt_long = case_when(treatment == "SI" ~ "shaded intact",
                              treatment == "UI" ~ "unshaded intact", 
                              treatment == "SR" ~ "shaded removal", 
                              treatment == "UR" ~ "unshaded removal")) %>% 
  mutate(shading = substr(treatment,1,1), removal = substr(treatment,2,2))

S1F4 <- ggplot(data = temp.summary2 %>% filter(mdmax_temp < 50), 
               aes(x = treatment, y = mdmax_temp, col = treatment, fill = treatment)) + 
  geom_boxplot(alpha = 0.4, outlier.color = NA) + 
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  labs(x = "Treatment", y = "Daily maximum temperature (ºC)", 
       color = "Treatment", fill = "Treatment") + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + 
  geom_jitter(width = 0.2, alpha = 0.2)

#ggsave(S1F4, filename = "../outputs/S1Fig5.png", dpi = 1200, width = 7, height = 5, units = "in")


# Create a model to test treatment differences in max daily temp between treatment
temp.summary3 <- temp.summary2 %>% mutate(shading = factor(shading, levels = c("U","S")))

max.temp.model <- lmer(mdmax_temp ~ shading*removal + (1|block), data = temp.summary3)
plot(simulateResiduals(max.temp.model)) # need to solve homoscedatisicity problems

max.temp.model.glmm <- glmmTMB(mdmax_temp ~ shading*removal + (1|block), data = temp.summary3,
                               dispformula = ~shading+removal)
plot(simulateResiduals(max.temp.model.glmm)) # problem solved!

summary(max.temp.model.glmm)
Anova(max.temp.model.glmm, type = 3)


################################################################################

# Question 2: Did S. cariosus mortality differ between shaded & unshaded plots?

barnacle.mort <- read_csv("./raw_data/shade/SBHW_SHADE_semibalanusmort.csv") %>% 
  mutate(prop_mort = (number_dead)/(number_live+number_dead)) %>% filter(prop_mort != "NaN")

mort.complete <- barnacle.mort %>% left_join(plot.info) %>% 
  filter((plot_number %in% c(15,35,36,40)) == F) %>% 
  select(prop_mort, treatment_original_numeric, plot_number, block) %>% 
  rename(treatment = treatment_original_numeric) %>% 
  mutate(treatment = case_when(treatment == 1 ~ "UI", treatment == 2 ~ "SI"))

# Proportional data is most correctly modeled with a beta distribution,
# but response data must be within (0,1).
mort.beds <- mort.complete %>% filter(treatment %in% c("SI","UI")) %>% 
  mutate(mort.tr = (prop_mort*(nrow(mort.complete)-1)+0.5)/nrow(mort.complete),
         treatment = factor(treatment, levels = c("UI","SI")))

# Model with block random effect
shade.mort.model <- glmmTMB(mort.tr ~ treatment + (1|block), 
                            dispformula = ~treatment, family = beta_family(), 
                            data = mort.beds)

plot(simulateResiduals(shade.mort.model)) # no assumptions violated
summary(shade.mort.model)
Anova(shade.mort.model) # Type 2 ANOVA (Wald Chi-square)

# Boxplot of mortality
fig5c <- ggplot(mort.beds, aes(x = treatment, y = prop_mort, col = treatment, fill = treatment)) + 
  geom_boxplot(alpha = 0.4, outlier.size = NA) + theme_classic() + 
  scale_color_manual( values = c("skyblue4", "tomato4")) +
  scale_fill_manual(values = c("skyblue4", "tomato4")) + 
  labs(x = "Treatment", y = expression("Proportion"~italic("S. cariosus")~"dead"),
       col = "Treatment") + theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.position = "none") + 
  geom_jitter(width = 0.2, alpha = 0.3)


################################################################################

# Question 3: Did barnacle recruitment differ between shaded & unshaded plots?

# Need to first assemble & clean the visual survey data

taxa.rec <- taxa %>% 
  select(original_code, final_code, kingdom)

survey.files <- c("./raw_data/shade/SBHW_SHADE_visual_1_20210414.csv",
                  "./raw_data/shade/SBHW_SHADE_visual_2_20210708.csv",
                  "./raw_data/shade/SBHW_SHADE_visual_3_20210821.csv")

for (file in 1:length(survey.files)){
  survey_date <- substr(survey.files[file], 38,45)
  df <- read_csv(survey.files[file]) %>% pivot_longer(cols = 4:length(.), 
                                                      names_to = "original_code", 
                                                      values_to = "abund") %>%
    left_join(plot.info) %>% select(block, plot_number, original_code, abund, treatment_original_numeric) %>% 
    right_join(taxa.rec) %>% na.omit %>% rename(treatment = treatment_original_numeric) %>% 
    mutate(date = ymd(survey_date))
  if (file == 1){
    all_visuals <- df
  }
  else{
    all_visuals <- all_visuals %>% full_join(df)
  }
}

all_visuals <- all_visuals %>% mutate(treatment_code = case_when(treatment == 1 ~ "UI",
                                                                 treatment == 2 ~ "SI", 
                                                                 treatment == 3 ~ "UR",
                                                                 treatment == 4 ~ "SR")) %>% 
  select(-original_code)

#write_csv(all_visuals, "./clean_data/SBHW_ShadingVisualSurveys.csv")

# now want to determine when peak recruitment occurred (from the three visual surveys performed)

all_visuals <- read_csv("./clean_data/SBHW_ShadingVisualSurveys.csv")

barnacles <- all_visuals %>%
  filter(final_code %in% c("BAGL","CHDA", "SECA") & # barnacle spp
         treatment %in% c(3, 4)) %>% # barnacle removal treatments
  mutate(date = as.Date(date))

summary.bncles <- all_visuals %>% group_by(date) %>% summarize(bncle_total = sum(abund)) %>% View()

# middle survey has the most recruitment signal by far, so filter out data from this date

recruitment.peak <- barnacles %>% filter(date == "2021-07-08") %>% 
  group_by(plot_number, block, treatment_code) %>% 
  summarize(total_abund = sum(abund)) %>% # total up all three barnacle spp in each plot
  mutate(treatment_code = factor(treatment_code, levels = c("UR","SR")))

mod.recruit <- glmmTMB(total_abund ~ treatment_code + (1|block), data = recruitment.peak,
                       family = nbinom2()) # model with negative binomial error distribution

plot(simulateResiduals(mod.recruit)) # assumptions met
summary(mod.recruit)
Anova(mod.recruit)

# Boxplot of barnacle recruitment
fig5d <- ggplot(recruitment.peak, aes(x = treatment_code, y = total_abund, 
                                      fill =  treatment_code, col =  treatment_code)) + 
  geom_boxplot(alpha = 0.4, outlier.shape = NA) + geom_jitter(width = 0.2, alpha = 0.3) + 
  labs(x = "Treatment", y = "Total barnacle abundance", fill = "Treatment") + 
  theme_classic() + 
  scale_fill_manual(values = c("skyblue3", "tomato2")) + 
  scale_color_manual(values = c("skyblue3", "tomato2")) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.position = "none")

################################################################################

# Question 4: Did the species richness & diversity (at least according to visual surveys)
# differ between treatments?

# 4a: Species richness, algae + invertebrates
visuals.whole <- all_visuals %>% 
  select(-kingdom, -treatment) %>% 
  pivot_wider(names_from = final_code, values_from = abund) %>% 
  replace(is.na(.), 0) # create a wide form of community data (all NA = really 0)

richness.whole <- visuals.whole %>% select(5:length(.)) %>% as.matrix %>% specnumber() %>% 
  cbind(visuals.whole[,1:4]) %>% as.data.frame() %>% rename(richness = 1)

richness.final.timept <- richness.whole %>% filter(date == "2021-08-21") %>% 
  separate(treatment_code, c("shading","removal"), 1, remove = F)

# Model richness using Poisson distribution (discrete data) with random block variable
richness.model <- glmmTMB(richness ~ shading*removal + (1|block), data = richness.final.timept,
                          family = poisson(link = "log"))
plot(simulateResiduals(richness.model))
testDispersion(simulationOutput) # slight underdispersion. 
# underdispersion leads to more conservative model estimates, so proceed with model

summary(richness.model)
Anova(richness.model, type = 3) # still detect an effect, so this is likely real

fig5e <- ggplot(richness.whole, aes(x = treatment_code, y = richness,
                                    fill = treatment_code, col = treatment_code)) + 
  geom_boxplot(alpha = 0.4, outlier.color = NA) + geom_jitter(alpha = 0.3, width = 0.2) + 
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2"))+ 
  theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12), 
        legend.position = "none") + labs(x = "Treatment", y = "Species richness")

# 4B: How about Shannon diversity of invertebrate community?

visuals_inverts <- all_visuals %>% 
  filter(kingdom == "Animalia") %>% # filter out algae, retain inverts
  select(-kingdom, -plot_number, -treatment) %>% 
  pivot_wider(names_from = final_code, values_from = abund) %>% 
  replace(is.na(.), 0) %>% 
  filter(date == "2021-08-21")

visuals_invert_factors <- visuals_inverts %>% select(block, treatment_code)

visuals_invert_species <- visuals_inverts %>% select(4:length(.)) %>% as.matrix 

H.inverts <- diversity(visuals_invert_species, index = "shannon")

shannon.inverts <- as.data.frame(cbind(visuals_factors, H.inverts)) %>% rename(shannon = 3) %>% 
  separate(treatment_code, c("shading","removal"),1, remove = F) %>% # separate treatments
  mutate(shading = factor(shading, levels = c("U","S")))

# Model: Can just use Gaussian distribution here
diversity.inverts.lmer <- glmmTMB(shannon ~ shading*removal + (1|block), 
                                  data = shannon.inverts)

plot(simulateResiduals(diversity.inverts.lmer))
summary(diversity.inverts.lmer)
Anova(diversity.inverts.lmer, type = 3)

fig5f <- ggplot(shannon.inverts, aes(x = treatment_code, y = shannon, 
                                     fill = treatment_code, col = treatment_code)) + 
  geom_boxplot(alpha = 0.4, outlier.color = NA) + geom_jitter(alpha = 0.3, width = 0.2) + 
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) +
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2"))+ 
  theme_classic() + theme(axis.title = element_text(size = 14), 
                          axis.text = element_text(size = 12), legend.position = "none") + 
  labs(x = "Treatment", y = "Shannon diversity (invertebrates)")

# 4C: Algal Shannon diversity

visuals_algae <- all_visuals %>% 
  filter(kingdom != "Animalia") %>% 
  select(-kingdom, -plot_number, -treatment) %>% 
  pivot_wider(names_from = final_code, values_from = abund) %>% 
  replace(is.na(.), 0) %>% 
  filter(date == "2021-08-21")

visuals_algae_factors <- visuals_algae %>% select(block, treatment_code)

visuals_algae_species <- visuals_algae %>% select(4:length(.)) %>% as.matrix 

H.algae <- diversity(visuals_algae_species, index = "shannon")

shannon.algae <- as.data.frame(cbind(visuals_algae_factors, H.algae)) %>% 
  rename(shannon = 3) %>% 
  separate(treatment_code, c("shading","removal"),1, remove = F) %>% 
  mutate(shading = factor(shading, levels = c("U","S")))

ggplot(aes(y = shannon), data = shannon.algae) +
  geom_histogram()+ facet_wrap(~treatment_code) # a LOT of zeroes in these data

# Given the zero inflation here, Tweedie distribution most appropriate
diversity.algae.lmer <- glmmTMB(shannon ~ shading*removal + (1|block), 
                                family = tweedie(link = "log"), data = shannon.algae)
plot(simulateResiduals(diversity.algae.lmer))
summary(diversity.algae.lmer)
Anova(diversity.algae.lmer, type = 3)

S1Fig6A <- ggplot(shannon.algae, aes(x = treatment_code, y = shannon, 
                                     fill = treatment_code, col = treatment_code)) + 
  geom_boxplot(alpha = 0.4, outlier.color = NA) + geom_jitter(alpha = 0.3, width = 0.2) + 
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) +
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) +
  theme_classic() + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12)) + 
  labs(x = "Treatment", y = "Shannon diversity (algae)",color = "Treatment", fill = "Treatment")

# 4D: Algal cover

algal_cover <-  all_visuals %>% 
  filter(kingdom != "Animalia") %>% 
  select(-kingdom, -plot_number, -treatment) %>% 
  filter(date == "2021-08-21") %>% 
  group_by(block, treatment_code) %>% 
  summarize(cover = sum(abund)) %>% 
  separate(treatment_code, c("shading","removal"),1, remove = F) %>% 
  mutate(shading = factor(shading, levels = c("U","S")))

# transform to be on interval (0,1)
algal_cover.tr <- algal_cover %>% 
  mutate(cover = (cover*(nrow(algal_cover)-1)+0.5)/nrow(algal_cover)) 

cover.algae.glmm <- glmmTMB(cover ~ shading*removal + (1|block), 
                            family = beta_family(link = "logit"), data = algal_cover.tr)
plot(simulateResiduals(cover.algae.glmm))
summary(cover.algae.glmm)
Anova(cover.algae.glmm, type = 3)

# Create boxplot of algal cover
S1Fig6B <- ggplot(algal_cover, aes(x = treatment_code, y = cover*100, 
                                   fill = treatment_code, col = treatment_code)) + 
  geom_boxplot(alpha = 0.4, outlier.color = NA) + geom_jitter(alpha = 0.3, width = 0.2) +
  scale_color_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2")) + 
  scale_fill_manual(values = c("skyblue4","skyblue3", "tomato4", "tomato2"))+ 
  theme_classic() + theme(axis.title = element_text(size = 14), 
                          axis.text = element_text(size = 12), 
                          legend.title = element_text(size = 14), 
                          legend.text = element_text(size = 12)) + 
  labs(x = "Treatment", y = "Algal cover (%)", color = "Treatment", fill = "Treatment")

# assemble multi-panel figures

fig5 <- fig5a + fig5b + fig5c + fig5d + fig5e + fig5f + 
  plot_layout(guides = "collect", design = layout, nrow = 2) + 
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold"))

ggsave(fig5, filename = "../outputs/Fig5.png", dpi = 1200, 
       width = 7, height = 5, units = "in", scale = 1.5) 

S1Fig6 <- S1Fig6A + S1Fig6B + plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold"))

ggsave(S1Fig6, filename = "../outputs/S1Fig6.png", dpi =1200, 
       width = 7, height = 4, units = "in")


################################################################################

# Question 5: Did infaunal communities differ between treatments?

species.repair <- taxa %>% 
  select(original_code, final_code)

infauna_sahsima <- read_csv("./raw_data/shade/SBHW_SHADE_infauna.csv") %>% 
  rename(plot_number = plot_id) %>% 
  pivot_longer(cols = -plot_number, names_to = "original_code", values_to = "abund") %>% 
  left_join(species.repair) %>% select(-original_code) %>% 
  group_by(plot_number, final_code) %>% summarize(abund = sum(abund)) %>% 
  pivot_wider(id_cols = plot_number, names_from = final_code, values_from = abund) %>% 
  left_join(plot.info) %>% select( -treatment_final_numeric, -shore_level) %>% 
  rename(treatment = treatment_original_numeric) %>% ungroup() %>% select(-plot_number)

infauna_factors <- infauna_sahsima %>% select(treatment, block) %>% 
  mutate(treatment = factor(treatment), block = factor(block))

infauna_matrix <- infauna_sahsima %>% select(-treatment, -block) %>% as.matrix()

# Run MDS with autotransform (to account for things with different abundances)
infauna_bray <- metaMDS(infauna_matrix, k=2, try = 999, autotransform = T)
infauna_bray$stress # < 0.2, so stick with 2 dimensions

# only non-rare species (>5 individuals)

# Will want to re-run MDS with rare species removed 
# (everything with < 5 individuals across all samples)
abund.spec <- as.data.frame(colSums(infauna_matrix)) %>% 
  rename(total = 1) %>% filter(total >= 5) %>% rownames()
infauna_abund <- infauna_matrix[,abund.spec]

infauna_bray_abund <- invisible(metaMDS(infauna_abund, k=2, try = 999, autotransform = T))
infauna_bray_abund$stress # < 0.2, so stick with 2 dimensions

# PERMANOVA
h <- how(nperm = 999) 
setBlocks(h) <- with(infauna_factors, block) # set permutation scheme with block design
adonis(infauna_matrix ~ treatment, infauna_factors, permutations = h)

# PERMDISP
dist <- vegdist(infauna_matrix, "bray")
infauna.beta <- betadisper(dist, infauna_factors$treatment, bias.adjust = T)
anova(infauna.beta)

# Create NMDS plot of NMDS with all species present
data.scores <- as.data.frame(scores(infauna_bray))
data.scores$treatment <- infauna_factors$treatment

data.scores <- data.scores %>% group_by(treatment) %>% mutate(mean.x = mean(NMDS1), mean.y = mean(NMDS2)) %>% ungroup() %>% mutate(treatment = case_when(treatment == "1" ~ "Unshaded",treatment == "2" ~ "Shaded"))

species.scores <- as.data.frame(scores(infauna_bray, "species"))
species.scores$species <- rownames(species.scores) 

## adding ellipses to plot

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(as.factor(data.scores$treatment))){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$treatment==g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                ,group=g))
}

df_ell$treatment <- df_ell$group

## create ggplot ordination
infauna.ordination.sa <- ggplot() + 
  geom_path(data=df_ell,aes(x = NMDS1,y =NMDS2,colour=treatment)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=treatment),size=3, alpha = 0.8) + # add the point markers
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size = 4, alpha=0.7,
                  max.overlaps = 500, box.padding=0.05, min.segment.length = 5) +
  coord_equal() +
  theme_bw() +
  scale_color_manual(values = c("skyblue4","tomato4")) +
  labs(color = "Treatment", x = "Axis 1", y = "Axis 2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  annotate("text", x = 0.75, y = 0.95, label = "Stress = 0.1739", size = 5)

#ggsave(filename = "../outputs/Fig6.png", infauna.ordination.sa, dpi = 1200, width = 3.5, height = 2.5, units = "in", scale = 2)

# PERMANOVA
h <- how(nperm = 999) 
setBlocks(h) <- with(infauna_factors, block) # set permutation scheme with block design
adonis(infauna_abund ~ treatment, infauna_factors, permutations = h)

# PERMDISP
dist <- vegdist(infauna_abund, "bray")
infauna.beta <- betadisper(dist, infauna_factors$treatment, bias.adjust = T)
anova(infauna.beta)

## ggplot of NMDS axes

data.scores <- as.data.frame(scores(infauna_bray_abund))
data.scores$treatment <- infauna_factors$treatment

data.scores <- data.scores %>% group_by(treatment) %>% 
  mutate(mean.x = mean(NMDS1), mean.y = mean(NMDS2)) %>% ungroup() %>% 
  mutate(treatment = case_when(treatment == "1" ~ "Unshaded",treatment == "2" ~ "Shaded"))

species.scores <- as.data.frame(scores(infauna_bray_abund, "species"))
species.scores$species <- rownames(species.scores) 

## adding ellipses to plot

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(as.factor(data.scores$treatment))){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores[data.scores$treatment==g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                ,group=g))
}

df_ell$treatment <- df_ell$group

## create ggplot ordination
infauna.ordination.sa.abund <- ggplot() + 
  geom_path(data=df_ell,aes(x = NMDS1,y =NMDS2,colour=treatment)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=treatment),size=3, alpha = 0.8) + # add the point markers
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size = 4, alpha=0.7,
                  max.overlaps = 500, box.padding=0.05, min.segment.length = 5) +
  coord_equal() +
  theme_bw() +
  scale_color_manual(values = c("skyblue4","tomato4")) +
  labs(color = "Treatment", x = "Axis 1", y = "Axis 2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  annotate("text", x = 0.75, y = -0.4, label = "Stress = 0.1770", size = 5)
infauna.ordination.sa.abund

#ggsave(filename = "../outputs/S1Fig8.png", infauna.ordination.sa.abund, dpi = 1200,width = 3.5, height = 2.5, units = "in", scale = 2)

adonis(infauna_abund ~ treatment, permutations = h, infauna_factors)

dist <- vegdist(infauna_abund, "bray")
infauna.beta <- betadisper(dist, infauna_factors$treatment, bias.adjust = T)
anova(infauna.beta)

