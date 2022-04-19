## Characterizing fauna in destructively sampled Semibalanus cariosus beds
## Amelia V Hesketh
## January 2021

# Load all relevant packages into R.env
pkgs <- c("vegan", "tidyverse", "RColorBrewer", "plotrix", "ggrepel",
          "glmmTMB", "cowplot", "patchwork", "magick", "ggpubr", "wesanderson")
invisible(lapply(pkgs, library, character = T))
rm(pkgs)

# Load in and assemble data

# load relevant data: collection info (associates number of collection with site code and collection date)
collection.info <- read_csv("./raw_data/associated_diversity/SBHW_DIVERSITY_collections.csv")
# load taxonomic data: contains information about taxonomic groupings and authorities, species codes, etc.
taxonomic.info <- read_csv("./raw_data/SBHW_taxonomic_20220105.csv") %>%  
  select(original_code, final_code, coarse_grouping)
# will need to read all the files for all collections .. list all the file names in this directory for reading in
sample.files <- list.files("./raw_data/associated_diversity/community_data/")

# this loop takes a file and loads it in, capturing information about the 
# collection number from the numeric component of the file namem, 
# and joins these in wide format
for (file in 1: length(sample.files)){
  file.name = paste("./raw_data/associated_diversity/community_data/", sample.files[file], sep = "")
  fauna <- read_csv(file.name)
  
  # these data contain taxa identified to their lowest taxonomic level
  fauna.sum <- fauna %>% mutate(replicate = seq(1:nrow(fauna))) %>% 
    pivot_longer(cols = -replicate, names_to ="original_code", values_to = "abundance") %>% 
    left_join(taxonomic.info) %>% 
    select(-original_code) %>% # ensures 'final' correct taxonomic code is used
    group_by(replicate, final_code) %>% summarize(abundance = sum(abundance)) %>% 
    pivot_wider(names_from = final_code, values_from = abundance) %>% 
    mutate(file_name = sample.files[file])
  
  # these data contain taxa sorted into coarse taxonomic groupings
  fauna.coarse <- fauna %>% 
    pivot_longer(cols = 1:ncol(fauna), names_to ="original_code", values_to = "abundance") %>% 
    left_join(taxonomic.info) %>% 
    select(-original_code, -final_code) %>% 
    group_by(coarse_grouping) %>% summarize(mean.abund = mean(abundance),
                                            se.abund = std.error(abundance)) %>% 
    mutate(file_name = sample.files[file])
  
  if (file == 1){
    fauna.all <- fauna.sum
    fauna.all.coarse <- fauna.coarse
  }
  if (file > 1){
    fauna.all <- fauna.sum %>% full_join(fauna.all)
    fauna.all.coarse <- fauna.coarse %>% full_join(fauna.all.coarse)
  }
}

fauna.clean <- fauna.all %>% replace(is.na(.), 0) %>% # replace NA with 0 (no individuals counted)
  separate(file_name, c("code", "data", "collection_number"), sep = "_") %>%
  select(-code, -data) %>% 
  mutate(collection_number = as.numeric(str_remove_all(collection_number, ".csv"))) %>% 
  left_join(collection.info) # join with site code and date of collection

#write_csv(fauna.clean, "./clean_data/SBHW_AssociatedDiversity.csv")

fauna.clean.coarse <- fauna.all.coarse %>% 
  separate(file_name, c("code", "data", "collection_number"), sep = "_") %>% 
  select(-code, -data) %>% 
  mutate(collection_number = as.numeric(str_remove_all(collection_number, ".csv"))) %>%
  left_join(collection.info) %>% 
  filter(collection_number %in% c(3:12)) %>% 
  select(-collection_date, -collection_number)

#write_csv(fauna.clean.coarse, "./clean_data/SBHW_AssociatedCoarseDiversity.csv")

################################################################################

### Analysis 1: NMDS & PERMANOVA to look at between-site differences in faunal assemblages

# bring in color palette from maps script for eventual plot
site.palette <- read_csv("./clean_data/SBHW_SiteInformation.csv") %>% select(site_code, full.pal)

# filter by time: only collections made at the end of summer 2021 (NOT 1 and 2 or 10)
fauna.summer <- fauna.clean %>% filter(collection_number %in%c(3:9,11:12)) %>% left_join(site.palette)
col.levels <- fauna.summer$full.pal %>% unique()
site.levels <- fauna.summer$site_code %>% unique()

#get rid of everything not related to species abundance and convert to matrix for nMDS
fauna.gradient <- fauna.summer %>% ungroup() %>% 
  select(-collection_number, -site_code, -collection_date, -full.pal, -replicate) 

# convert to matrix
fauna.matrix <- as.matrix(fauna.gradient)

# ready to analyze
fauna.gradient.mds <- metaMDS(fauna.matrix, k = 3, try = 999, autotransform = T)
fauna.gradient.mds$stress # print stress of final model (~bad with 2 dimensions)
# if you try MDS with 3 dimensions, stress is <0.2, so retain 3 dimensions for analysis

# PERMANOVA model: just looking at differences in fauna as a function of collection location
adonis(fauna.matrix~site_code, data = fauna.summer, permutations = 9999)

dist.gradient <- vegdist(fauna.matrix, method = "bray")
gradient.beta <- betadisper(dist.gradient, group = fauna.summer$site_code, bias.adjust = T)
anova(gradient.beta)

### NMDS plot of community differences in ggplot

data.scores <- as.data.frame(scores(fauna.gradient.mds))
data.scores$site_code <- fauna.summer$site_code

data.scores <- data.scores %>% group_by(site_code) %>% 
  mutate(mean.x = mean(NMDS1), mean.y = mean(NMDS2)) %>% 
  ungroup() %>% mutate(site_code = factor(site_code, levels = site.levels))

species.scores <- as.data.frame(scores(fauna.gradient.mds, "species"))
species.scores$species <- rownames(species.scores) 

fauna.ordination <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=site_code),size=3, alpha = 0.8) + # add the point markers
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size = 2.5, alpha=0.7,
                  max.overlaps = 500, box.padding=0.05, min.segment.length = 5) +
  coord_equal() +
  plot_theme +
  scale_color_manual(values = col.levels) +
  geom_segment(aes(x = NMDS1, y = NMDS2, xend = mean.x, yend = mean.y, col = site_code), data = data.scores, alpha = 0.7) +
  geom_text(data=data.scores,aes(x=mean.x, mean.y,label=site_code),size=4, alpha = 0.7, col = "grey20" ) +  # add the site labels 
  geom_text(aes(x = 0.9, y = 0.6, label = "Stress = 0.1656"), cex=4) +
  labs(color = "Site code", x = "Axis 1", y = "Axis 2") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#ggsave(filename = "./outputs/S1Fig2.png", fauna.ordination, dpi = 1200)

### Plots of faunal community abundance & diversity

# associate site codes with palette colours from original map
# get into order from outer->inner coast
site.levels <- as.data.frame(c("WBS","PP","BO","KE","TP","SP","SA","SN","TE","TK")) %>% rename(site_code = 1) 
col.conversion <- site.levels %>% left_join(site.palette) %>% select(full.pal)
col.levels <- col.conversion$full.pal
site.levels <- site.levels$site_code

pal.abund <- wes_palette("Darjeeling2", n = 16, type = "continuous") # define palette for abundance figure


# Plot 2A & 2B in main text: faunal richness and Shannon diversity across sites

H <- diversity(fauna.matrix, "shannon") # compute Shannon diversity of each collection from earlier matrix of data

# calculate richness of each collection
fauna.richness <- fauna.summer %>% select(site_code, replicate)
richness <- with(fauna.richness, specnumber(fauna.matrix))

# join with informative factors for plotting
fauna.diversity <- cbind(cbind(fauna.summer, richness),H) %>% 
  rename(richness = 56, Shannon_diversity = 57) %>% 
  mutate(site_code = factor(site_code, levels = site.levels))

fauna.div.summary <- fauna.diversity %>% 
  group_by(site_code, full.pal) %>% 
  summarize(mean.rich = mean(richness), se.rich = std.error(richness), # summarize for plotting
            mean.H = mean(Shannon_diversity), se.H = std.error(Shannon_diversity)) %>% 
  mutate(site_code = factor(site_code, levels = site.levels)) # arrange from outer -> inner coast

# get colours matching map palette
color.col <- arrange(fauna.diversity, by_group = site_code) %>% ungroup() %>%
  select(full.pal) %>% unique()
col.levels <- color.col$full.pal 

fauna.div.summary2 <- fauna.div.summary %>% 
  mutate(full.pal = factor(full.pal, levels = col.levels))

# Figure 2A

Fig2A <- ggplot(aes(x = site_code, y = richness, fill = site_code), 
                        data = fauna.diversity) +
  geom_boxplot() +
  labs(x = "Collection site", y = "Taxonomic richness") + 
  scale_fill_manual(values = col.levels) +
  theme_classic()+
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20), legend.position = "none")

# Figure 2B

Fig2B <- ggplot(aes(x = site_code, y = H, fill = site_code), data = fauna.diversity) +
  geom_boxplot() +
  labs(x = "Collection site", y = "Shannon diversity") + 
  scale_fill_manual(values = col.levels) +
  theme_classic()+
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20), legend.position = "none")

# Plot 2C in main text: faunal abundance across coarse taxonomic groupings

abund.plot <- read_csv("./clean_data/SBHW_faunaCoarseGradient.csv") %>% 
  mutate(site_code = factor(site_code, levels = site.levels), 
         coarse_grouping = fct_reorder(coarse_grouping, mean.abund, max)) %>% 
  filter((site_code == "SA") == F)

Fig2C <- ggplot(aes(x = site_code, fill = coarse_grouping, y = mean.abund), 
                        data = abund.plot) +
  geom_bar(position = "stack", stat = "identity", col = "grey50", lwd = 0.2, width = 0.8) +
  theme_classic() +
  labs(y = "Mean abundance", x = "Collection site", fill = "Taxon") +
  scale_fill_discrete(type = pal.abund) +
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20),
        legend.title=element_text(size = 24), legend.text = element_text(size = 20))


# read in photo for panel 2D

fauna.photo <- image_convert(image_read("./raw_data/fauna.jpg"), "png")

Fig2D <- ggdraw() + draw_image(fauna.photo) + theme(plot.margin = margin(0,0,0,0))

Fig2 <- (Fig2A | Fig2B) / (Fig2C | Fig2D) +
  plot_annotation(tag_levels = "A") & theme(plot.tag =element_text(size = 26, face = "bold")) 

ggsave(Fig2, filename = "./outputs/Fig2.png", dpi = 1200, height = 5, width = 7, units = "in", scale = 2.5)

################################################################################

## Characterizing seasonal shifts in fauna at Prasiola Point

fauna.shift <- fauna.clean %>% filter(collection_number %in% c(1,12))

#get rid of everything not related to species abundance and convert to matrix for nMDS
fauna.matrix <- fauna.shift %>% ungroup() %>% 
  select(-collection_number, -site_code, -collection_date, -replicate) %>% as.matrix

# ready to analyze
fauna.shift.mds <- metaMDS(fauna.matrix, k = 2, try = 999, autotransform = T)
fauna.shift.mds$stress # print stress of final model, ok with two dimensions

#PERMANOVA

adonis(fauna.matrix~ collection_date, data = fauna.shift, permutations = 9999)
# no significant difference in community structure

dist <- vegdist(fauna.matrix, "bray")
fauna.beta <- betadisper(dist, fauna.shift$collection_number, bias.adjust = T)
anova(fauna.beta)

## NMDS plot of first two axes in ggplot2

data.scores <- as.data.frame(scores(fauna.shift.mds))
data.scores$treatment <- fauna.shift$collection_number
data.scores <- data.scores %>% group_by(treatment) %>% mutate(mean.x = mean(NMDS1), mean.y = mean(NMDS2)) %>% ungroup() %>% mutate(treatment = case_when(treatment == "1" ~ "PP April 2021",treatment == "2" ~ "SA April 2021", treatment == "10" ~ "SA August 2021", treatment == "12" ~ "PP September 2021"))

species.scores <- as.data.frame(scores(fauna.shift.mds, "species"))
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
fauna.ordination.shift <- ggplot() + 
  geom_path(data=df_ell,aes(x = NMDS1,y =NMDS2,colour=treatment)) +
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=treatment),size=3, alpha = 0.8) + # add the point markers
  geom_text_repel(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),size = 4, alpha=0.7,
                  max.overlaps = 500, box.padding=0.05, min.segment.length = 5) +
  plot_theme +
  labs(color = "Collection", x = "Axis 1", y = "Axis 2") +
  annotate("text", x = -0.5, y = -0.8, label = "Stress = 0.1403", size = 5) +
  scale_color_manual(values = c("#528ad9", "#9D7426", "#a9d672", "#CEAA07"))

ggsave(filename = "./outputs/S1Fig3.png", fauna.ordination.shift, dpi = 1200, width = 3.5, height = 2.5, units = "in", scale = 2)
