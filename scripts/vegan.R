png("community_shade.png", units = "in", width = 6, height = 6, res = 450)
plot(comm.infauna, type = "n")
points(comm.infauna, display = "sites", cex = 0.7, col = "black")
orditorp(comm.infauna, display="species",col="grey30", cex = 0.8, air=1)
ordiellipse(comm.infauna, groups = factors.infauna$treatment, 
            col = c("blue", "red"), label = TRUE)
dev.off()


perm.y1 <- adonis(infauna.matrix ~ treatment, data = factors.infauna,
                  perm = 99)
perm.y1

dist.y1 <- vegdist(comm_matrix_oct19, method = "bray")

disp.y1 <- betadisper(dist.y1, type = "centroid", group = comm_oct_factors$treatment)
anova(disp.y1)
boxplot(disp.y1)


####

treatments.codes <- as.data.frame(plot.info$treatment_original_numeric) %>% 
  rename(treatment_number = 1) %>% mutate(treatment_code = 
                                            case_when(treatment_number == 1 ~ "UI", 
                                                      treatment_number == 2 ~ "SI",
                                                      treatment_number == 3 ~ "UR",
                                                      treatment_number == 4 ~ "SR"))

png("community_visual_shade.png", units = "in", width = 6, height = 6, res = 450)
plot(comm.visual.shade, type = "n")
points(comm.visual.shade, display = "sites", cex = 0.7, col = "black")
orditorp(comm.visual.shade, display="species",col="grey30", cex = 0.8, air=1)
ordiellipse(comm.visual.shade, groups = treatments.codes$treatment_code, 
            col = c("tomato4", "skyblue4","tomato2","skyblue2"), label = TRUE)
dev.off()


perm.y1 <- adonis(comm.visual.matrix ~ treatment_code, data = treatments.codes,
                  perm = 99)
perm.y1

dist.y1 <- vegdist(comm_matrix_oct19, method = "bray")

disp.y1 <- betadisper(dist.y1, type = "centroid", group = comm_oct_factors$treatment)
anova(disp.y1)
boxplot(disp.y1)
