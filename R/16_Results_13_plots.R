# Load required packages
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggcorrplot)

# Set working directory
setwd("My_working_directory")

# Import data
pahipi.effectsize.generalists<-read.csv("effectsize_pahipi_generalists.csv", sep="\t")
pahipi.effectsize.specialists<-read.csv("effectsize_pahipi_specialists.csv", sep="\t")
rparhirpi.effectsize.generalists<-read.csv("effectsize_rparhirpi_generalists.csv", sep="\t")
rparhirpi.effectsize.specialists<-read.csv("effectsize_rparhirpi_specialists.csv", sep="\t")
pp.pahipi.generalists<-read.csv("predicted_probability_pahipi_generalists.csv", sep="\t")
pp.pahipi.specialists<-read.csv("predicted_probability_pahipi_specialists.csv", sep="\t")
hipi.effectsize.generalists<-read.csv("effectsize_hipi_generalists.csv", sep="\t")
hipi.effectsize.specialists<-read.csv("effectsize_hipi_specialists.csv", sep="\t")
pahipi.corr.generalists<-read.csv("corr_generalists.csv", sep="\t")
pahipi.corr.specialists<-read.csv("corr_specialists.csv", sep="\t")
pahipi.effectsize.generalists.threatened.criterionB.excluded<-read.csv("effectsize_pahipi_generalists_threatened_criterionB_excluded.csv", sep="\t")
pahipi.effectsize.specialists.threatened.criterionB.excluded<-read.csv("effectsize_pahipi_specialists_threatened_criterionB_excluded.csv", sep="\t")
rparhirpi.effectsize.generalists.threatened.criterionB.excluded<-read.csv("effectsize_rparhirpi_generalists_threatened_criterionB_excluded.csv", sep="\t")
rparhirpi.effectsize.specialists.threatened.criterionB.excluded<-read.csv("effectsize_rparhirpi_specialists_threatened_criterionB_excluded.csv", sep="\t")
pahipi.auc.6.fold.cv.by.region.generalists<-read.csv("auc_spatially_blocked_cv_pahipi_generalists.csv", sep="\t")
pahipi.auc.6.fold.cv.by.region.specialists<-read.csv("auc_spatially_blocked_cv_pahipi_specialists.csv", sep="\t")
pahipi.auc.10.fold.random.cv.generalists<-read.csv("auc_10-fold_cv_pahipi_generalists.csv", sep="\t")
pahipi.auc.10.fold.random.cv.specialists<-read.csv("auc_10-fold_cv_pahipi_specialists.csv", sep="\t")
pahipi.auc.6.fold.cv.by.region.generalists.threatened.criterionB.excluded<-read.csv("auc_spatially_blocked_cv_pahipi_generalists_threatened_criterionB_excluded.csv", sep="\t")
pahipi.auc.6.fold.cv.by.region.specialists.threatened.criterionB.excluded<-read.csv("auc_spatially_blocked_cv_pahipi_specialists_threatened_criterionB_excluded.csv", sep="\t")
pahipi.auc.10.fold.random.cv.generalists.threatened.criterionB.excluded<-read.csv("auc_10-fold_cv_pahipi_generalists_threatened_criterionB_excluded.csv", sep="\t")
pahipi.auc.10.fold.random.cv.specialists.threatened.criterionB.excluded<-read.csv("auc_10-fold_cv_pahipi_specialists_threatened_criterionB_excluded.csv", sep="\t")

#----
# Effect sizes
pahipi.effectsize.generalists$land.model<-factor(pahipi.effectsize.generalists$land.model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
pahipi.effectsize.generalists.plot<-ggplot(pahipi.effectsize.generalists, aes(x=land.model, y=std_coefficient, colour=land.model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=land.model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) + 
  scale_shape_manual(values=c(16, 16, 16), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) + 
  scale_x_discrete(name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("PA", "HI", "PI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", linewidth=0.5) +
  ylim(-1.11, 0) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pahipi.effectsize.generalists.plot

pahipi.effectsize.specialists$land.model<-factor(pahipi.effectsize.specialists$land.model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
pahipi.effectsize.specialists.plot<-ggplot(pahipi.effectsize.specialists, aes(x=land.model, y=std_coefficient, colour=land.model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=land.model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16, 16, 16), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_x_discrete(name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("PA", "HI", "PI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) +
  ylim(-1.11, 0) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pahipi.effectsize.specialists.plot

rparhirpi.effectsize.generalists$land.model<-factor(rparhirpi.effectsize.generalists$land.model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
rparhirpi.effectsize.generalists.plot<-ggplot(rparhirpi.effectsize.generalists, aes(x=land.model, y=std_coefficient, colour=land.model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=land.model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) + 
  scale_shape_manual(values=c(16, 16, 16), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) + 
  scale_x_discrete(name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("RPA", "RHI", "RPI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", linewidth=0.5) +
  ylim(-0.25, 0.33) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rparhirpi.effectsize.generalists.plot

rparhirpi.effectsize.specialists$land.model<-factor(rparhirpi.effectsize.specialists$land.model, levels=c("Patch-matrix", "Continuum", "Hybrid"))
rparhirpi.effectsize.specialists.plot<-ggplot(rparhirpi.effectsize.specialists, aes(x=land.model, y=std_coefficient, colour=land.model)) +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5, aes(shape=land.model)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", linewidth=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, linewidth = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", linewidth = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16, 16, 16), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_x_discrete(name="Landscape model", breaks=c("Patch-matrix", "Continuum", "Hybrid"), labels=c("RPA", "RHI", "RPI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) +
  ylim(-0.25, 0.33) +
  guides(shape=guide_legend(title.position = "top"), color=guide_legend(title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rparhirpi.effectsize.specialists.plot

# Combine plots
pahipi.effectsize.plot<-plot_grid(pahipi.effectsize.generalists.plot + theme(legend.position="none"),
                                  pahipi.effectsize.specialists.plot + theme(legend.position="none"),
                                  rparhirpi.effectsize.generalists.plot + theme(legend.position="none"),
                                  rparhirpi.effectsize.specialists.plot + theme(legend.position="none"),
                                  labels=c('(a)', '(b)', '(c)', '(d)'), 
                                  label_size = 7, 
                                  ncol=2, 
                                  nrow=2,
                                  align ="hv",
                                  axis="tblr", 
                                  rel_heights=c(2,2))
pahipi.effectsize.plot

# Extract legend
legend.fig.2 <- get_legend(
  pahipi.effectsize.generalists.plot + 
    theme(legend.position = "bottom")
)

# Export Fig.2
pdf("Fig.2.pdf", width=3, height=5)
#png("Fig.2.png", units="in", width=3, height=5, res=1200, type="cairo")
plot_grid(pahipi.effectsize.plot, legend.fig.2, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Predicted probabilities
pp.pahipi.generalists$predictor <- factor(pp.pahipi.generalists$predictor , levels=c("pa", "hi", "pi"))
pp.pahipi.generalists.plot<-ggplot(pp.pahipi.generalists, aes(x=x_bs, y=predicted_bt, fill=predictor, color=predictor)) +
  geom_line(size=1) +
  xlab("") + 
  ylab("Prob. of being threatened") +
  theme(legend.position="bottom", legend.title=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        legend.margin=margin(-11, 1, 1, 1),
        legend.spacing.x=unit(1, "mm"),
        legend.background = element_rect(color = NA),
        panel.border=element_rect(colour="black", fill=NA, linewidth=0.5),
        legend.text=element_text(size=7),
        panel.background=element_rect(fill="transparent", colour="grey85", size=0.5, linetype="solid"),
        panel.grid.major=element_line(linewidth=0.25, linetype='dashed', colour="grey85"),
        panel.grid.minor=element_line(linewidth=0.25, linetype='dashed', colour="grey85"), 
        plot.title=element_text(color="black", size=7, hjust=0), axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=7), axis.text.x=element_text(size=7), 
        axis.text.y=element_text(size=7), legend.key=element_rect(fill="transparent", colour="transparent"),
        strip.background = element_rect(fill="transparent", size=0.5, color="transparent"), strip.text.x = element_text(size=7), strip.text.y = element_text(size=7),
        strip.placement = "outside") +
  geom_ribbon(aes(ymin = conf.low_bt, ymax = conf.high_bt), alpha=0.3, colour=NA) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_fill_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  xlim(-0.01, 1.01) +
  ylim(0.5, 0.69) +
  guides(fill=guide_legend(title="Landscape model", title.position = "top"), color=guide_legend(title="Landscape model", title.position = "top")) +
  facet_grid(habitat_breadth~predictor, switch="x", labeller = labeller(predictor = c("pa" = "Patch area",
                                                                                      "hi" = "Habitat intactnes",
                                                                                      "pi" = "Patch intactness")), scales="free_x")
pp.pahipi.generalists.plot

pp.pahipi.specialists$predictor <- factor(pp.pahipi.specialists$predictor , levels=c("pa", "hi", "pi"))
pp.pahipi.specialists.plot<-ggplot(pp.pahipi.specialists, aes(x=x_bs, y=predicted_bt, fill=predictor, color=predictor)) +
  geom_line(size=1) +
  xlab("") + 
  ylab("Prob. of being threatened") +
  theme(legend.position="bottom", legend.title=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        legend.margin=margin(-11, 1, 1, 1),
        legend.spacing.x=unit(1, "mm"),
        legend.background = element_rect(color = NA),
        panel.border=element_rect(colour="black", fill=NA, linewidth=0.5),
        legend.text=element_text(size=7),
        panel.background=element_rect(fill="transparent", colour="grey85", size=0.5, linetype="solid"),
        panel.grid.major=element_line(linewidth=0.25, linetype='dashed', colour="grey85"),
        panel.grid.minor=element_line(linewidth=0.25, linetype='dashed', colour="grey85"), 
        plot.title=element_text(color="black", size=7, hjust=0), axis.title.x=element_text(size=7),
        axis.title.y=element_text(size=7), axis.text.x=element_text(size=7), 
        axis.text.y=element_text(size=7), legend.key=element_rect(fill="transparent", colour="transparent"),
        strip.background = element_rect(fill="transparent", size=0.5, color="transparent"), strip.text.x = element_text(size=7), strip.text.y = element_text(size=7),
        strip.placement = "outside") +
  geom_ribbon(aes(ymin = conf.low_bt, ymax = conf.high_bt), alpha=0.3, colour=NA) +
  scale_color_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  scale_fill_manual(values=c("#FB8072", "#80B1D3", "#CC79A7"), labels=c("Patch-matrix", "Continuum", "Hybrid")) +
  guides(fill=guide_legend(title="Landscape model", title.position = "top"), color=guide_legend(title="Landscape model", title.position = "top")) +
  xlim(-0.01, 1.01) +
  ylim(0.5, 0.69) +
  facet_grid(habitat_breadth~predictor, switch="x", labeller = labeller(predictor = c("pa" = "Patch area",
                                                                                      "hi" = "Habitat intactnes",
                                                                                      "pi" = "Patch intactness")), scales="free_x")
pp.pahipi.specialists.plot

# Combine plots
pp.pahipi.plot<-plot_grid(pp.pahipi.generalists.plot + theme(legend.position="none"),
                          pp.pahipi.specialists.plot + theme(legend.position="none"),
                          labels=c('(a)', '(b)'), 
                          label_size = 7, 
                          ncol=1, 
                          nrow=2,
                          align ="hv",
                          axis="tblr", 
                          rel_heights=c(2,2))
pp.pahipi.plot

# Extract legend 
legend.fig.3 <- get_legend(
  pp.pahipi.generalists.plot + 
    theme(legend.position = "bottom")
)

# Export Fig.3 
pdf("Fig.3.pdf", width=4.3, height=4.5)
#png("Fig.3.png", units="in", width=4.3, height=4.5, res=1200, type="cairo")
plot_grid(pp.pahipi.plot, legend.fig.3, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Effect sizes for continuum and hybrid models
hipi.effectsize.table<-rbind(hipi.effectsize.generalists, hipi.effectsize.specialists)
hipi.effectsize.table$parameter <- factor(hipi.effectsize.table$parameter , levels=c("5th", "10th", "50th", "90th", "95th"))
hipi.effectsize.table$group <- factor(hipi.effectsize.table$group , levels=c("hi", "pi"))
hipi.effectsize.plot<-ggplot(hipi.effectsize.table, aes(x=parameter, y=std_coefficient, colour=land.model)) +
  coord_flip() +
  geom_errorbar(width=0.1, aes(ymin=ci_lower, ymax=ci_upper)) +
  geom_point(size=1.5) +
  labs(y="Standardized coefficient", x="Percentile") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        #axis.ticks.x = element_blank(),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7),
        strip.text.y = element_text(size=7)) +
  scale_shape_manual(values=c(15, 16, 0)) + 
  scale_color_manual(values=c("#80B1D3", "#CC79A7")) + 
  geom_hline(yintercept = 0, linetype="dashed", color = "#8DD3C7", size=0.5) +
  guides(fill=guide_legend(title="Landscape model", title.position = "top"), color=guide_legend(title="Landscape model", title.position = "top")) +
  facet_grid(habitat_breadth~group, labeller = labeller(group = c("hi" = "Habitat intactness", "pi"= "Patch intactness")))
hipi.effectsize.plot

# Reduce plot margins
hipi.effectsize.plot.fv<-hipi.effectsize.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S1
png("Fig.S1.png", units="in", width=6.5, height=4, res=1200, type="cairo")
hipi.effectsize.plot.fv
dev.off()

#----
# Correlation between variables
rownames(pahipi.corr.generalists)<-c("PA", "RPA", "HI", "RHI", "PI", "RPI", "RS", "GL", "WA")
rownames(pahipi.corr.specialists)<-c("PA", "RPA", "HI", "RHI", "PI", "RPI", "RS", "GL", "WA")

# Visualize correlation  matrix
corr.generalists.plot<-ggcorrplot(pahipi.corr.generalists, 
                                  type = "lower",
                                  outline.color = "white",
                                  ggtheme = ggplot2::theme_gray,
                                  colors = c("#6D9EC1", "white", "#E46726"),
                                  lab=TRUE, 
                                  lab_size=2,
                                  legend.title = "r") +
  labs(title="Generalists") +
  theme(legend.key.width=unit(0.2, "cm"),
        legend.key.height=unit(1.25, "cm"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7),
        legend.justification = "top",
        axis.text.x=element_text(size=7, angle=0, hjust=0.5),
        axis.text.y=element_text(size=7),
        plot.margin = unit(c(0,1,0,1), "lines"),
        plot.title=element_text(size=7, hjust=0.5))
corr.generalists.plot

corr.specialists.plot<-ggcorrplot(pahipi.corr.specialists, 
                                  type = "lower",
                                  outline.color = "white",
                                  ggtheme = ggplot2::theme_gray,
                                  colors = c("#6D9EC1", "white", "#E46726"),
                                  lab=TRUE, 
                                  lab_size=2,
                                  legend.title = "r") +
  labs(title="Specialists") +
  theme(legend.key.width=unit(0.2, "cm"),
        legend.key.height=unit(1.25, "cm"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7),
        legend.justification = "top",
        axis.text.x=element_text(size=7, angle=0, hjust=0.5),
        axis.text.y=element_text(size=7),
        plot.margin = unit(c(0,1,0,1), "lines"),
        plot.title=element_text(size=7, hjust=0.5))
corr.specialists.plot

# Correlation plot
corr.plot<- ggarrange(corr.generalists.plot, 
                      corr.specialists.plot,
                      ncol=2, 
                      nrow=1, 
                      legend="right", 
                      align="hv", 
                      common.legend=TRUE,
                      font.label=list(size=7))

# Export Fig.S2
png("Fig.S2.png", units="in", width=6.5, height=3, res=1200, type="cairo")
corr.plot
dev.off()

#----
# Effect sizes - criterion B excluded
pahipi.effectsize.table<-rbind(pahipi.effectsize.generalists, pahipi.effectsize.specialists)
pahipi.effectsize.criterionB.excluded.table<-rbind(pahipi.effectsize.generalists.threatened.criterionB.excluded, pahipi.effectsize.specialists.threatened.criterionB.excluded)
pahipi.effectsize.criterionB.excluded.table$criterionB<-"Excluded"
pahipi.effectsize.table$criterionB<-"Included"
pahipi.effectsize.criterionB.includedvsexcluded.table<-rbind(pahipi.effectsize.table, pahipi.effectsize.criterionB.excluded.table)
pahipi.effectsize.criterionB.includedvsexcluded.table$id<-letters[1:12]
rparhirpi.effectsize.table<-rbind(rparhirpi.effectsize.generalists, rparhirpi.effectsize.specialists)
rparhirpi.effectsize.criterionB.excluded.table<-rbind(rparhirpi.effectsize.generalists.threatened.criterionB.excluded, rparhirpi.effectsize.specialists.threatened.criterionB.excluded)
rparhirpi.effectsize.criterionB.excluded.table$criterionB<-"Excluded"
rparhirpi.effectsize.table$criterionB<-"Included"
rparhirpi.effectsize.criterionB.includedvsexcluded.table<-rbind(rparhirpi.effectsize.table, rparhirpi.effectsize.criterionB.excluded.table)
rparhirpi.effectsize.criterionB.includedvsexcluded.table$id<-letters[1:12]

# Plot effect sizes
pahipi.effectsize.criterionB.includedvsexcluded.table$criterionB<-factor(pahipi.effectsize.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pahipi.effectsize.criterionB.includedvsexcluded.table$parameter<-factor(pahipi.effectsize.criterionB.includedvsexcluded.table$parameter, levels=c("pa", "hi", "pi"))
pahipi.effectsize.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pahipi.effectsize.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pahipi.effectsize.criterionB.includedvsexcluded.plot<-ggplot(pahipi.effectsize.criterionB.includedvsexcluded.table, aes(x=parameter, y=std_coefficient, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, position=position_dodge(width=0.7)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(-8, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16)) +
  scale_color_manual(values=c("#FF61CC", "#8494FF", "#CC79A7")) + 
  scale_x_discrete(labels=c("PA", "HI", "PI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pahipi.effectsize.criterionB.includedvsexcluded.plot

rparhirpi.effectsize.criterionB.includedvsexcluded.table$criterionB<-factor(rparhirpi.effectsize.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
rparhirpi.effectsize.criterionB.includedvsexcluded.table$parameter<-factor(rparhirpi.effectsize.criterionB.includedvsexcluded.table$parameter, levels=c("rpa", "rhi", "rpi"))
rparhirpi.effectsize.criterionB.includedvsexcluded.table$habitat_breadth<-factor(rparhirpi.effectsize.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
rparhirpi.effectsize.criterionB.includedvsexcluded.plot<-ggplot(rparhirpi.effectsize.criterionB.includedvsexcluded.table, aes(x=parameter, y=std_coefficient, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, position=position_dodge(width=0.7)) +
  labs(y="Standardized coefficient", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(-8, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'),
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_text(size=7), axis.text.x = element_text(size=7),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(16)) +
  scale_color_manual(values=c("#FF61CC", "#8494FF", "#CC79A7")) + 
  scale_x_discrete(labels=c("RPA", "RHI", "RPI")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
rparhirpi.effectsize.criterionB.includedvsexcluded.plot

# Combine plots
pahipi.effectsize.criterionB.includedvsexcluded.plot.fv<-plot_grid(pahipi.effectsize.criterionB.includedvsexcluded.plot + theme(legend.position="none"),
                                                                   rparhirpi.effectsize.criterionB.includedvsexcluded.plot + theme(legend.position="none"),
                                                                   #labels=c('(a)', '(c)', '(b)', '(d)'), 
                                                                   label_size = 7, 
                                                                   ncol=1, 
                                                                   nrow=2,
                                                                   align ="hv",
                                                                   axis="tblr", 
                                                                   rel_heights=c(2,2))
pahipi.effectsize.criterionB.includedvsexcluded.plot.fv

# Extract legend
legend.fig.S3 <- get_legend(
  pahipi.effectsize.criterionB.includedvsexcluded.plot + 
    theme(legend.position = "bottom")
)

# Export Fig.S3
png("Fig.S3.png", units="in", width=3, height=6.3, res=1200, type="cairo")
plot_grid(pahipi.effectsize.criterionB.includedvsexcluded.plot.fv, legend.fig.S3, ncol = 1, rel_heights = c(1, .1))
dev.off()

#----
# Spatially blocked cv | 10-fold cv
pahipi.auc.6.fold.cv.by.region.table<-rbind(pahipi.auc.6.fold.cv.by.region.generalists, pahipi.auc.6.fold.cv.by.region.specialists)
pahipi.auc.6.fold.cv.by.region.table$cv<-"6-fold CV (by biogeographic realm)"
pahipi.auc.10.fold.random.cv.table<-rbind(pahipi.auc.10.fold.random.cv.generalists, pahipi.auc.10.fold.random.cv.specialists)
pahipi.auc.10.fold.random.cv.table$cv<-"10-fold CV"
pahipi.auc.cv.table<-rbind(pahipi.auc.6.fold.cv.by.region.table, pahipi.auc.10.fold.random.cv.table)

# Plot AUCs ~ landscape models
pahipi.auc.cv.table$cv<-factor(pahipi.auc.cv.table$cv, levels=c("6-fold CV (by biogeographic realm)", "10-fold CV"))
pahipi.auc.cv.table$parameter<-factor(pahipi.auc.cv.table$parameter, levels=c("pa", "hi", "pi"))
pahipi.auc.cv.table$habitat_breadth<-factor(pahipi.auc.cv.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pahipi.auc.cv.plot<-ggplot(pahipi.auc.cv.table, aes(x=parameter, y=auc, color=cv)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=cv), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "vertical",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#A6D854", "#FFD92F")) + 
  scale_x_discrete(labels=c("ER ~ PA + RPA", "ER ~ HI + RHI", "ER ~ PI + RPI")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Cross-validation (CV) scheme", title.position = "top"), shape=guide_legend(title="Cross-validation (CV) scheme", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y")
pahipi.auc.cv.plot

# Reduce plot margins
pahipi.auc.cv.plot.fv<-pahipi.auc.cv.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S4
png("Fig.S4.png", units="in", width=3, height=4, res=1200, type="cairo")
pahipi.auc.cv.plot.fv
dev.off()

#----
# Spatially blocked cv - criterion B excluded
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table<-rbind(pahipi.auc.6.fold.cv.by.region.generalists.threatened.criterionB.excluded, pahipi.auc.6.fold.cv.by.region.specialists.threatened.criterionB.excluded)
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table$criterionB<-"Excluded"
pahipi.auc.6.fold.cv.by.region.table$criterionB<-"Included"
vars<-c("auc", "ci_lower", "ci_upper", "parameter", "land.model", "habitat_breadth", "criterionB")
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table<-rbind(pahipi.auc.6.fold.cv.by.region.table[,vars], pahipi.auc.6.fold.cv.by.region.threatened.criterionB.excluded.table[,vars])

# Plot AUCs ~ landscape models
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$cv<-factor(pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$parameter<-factor(pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$parameter, levels=c("pa", "hi", "pi"))
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot<-ggplot(pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.table, aes(x=parameter, y=auc, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=criterionB), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#FF61CC", "#8494FF")) + 
  scale_x_discrete(labels=c("ER ~ PA + RPA", "ER ~ HI + RHI", "ER ~ PI + RPI")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top"), shape=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y", labeller = labeller(habitat_breadth = c("Generalists" = "Generalists", "Specialists" = "Specialists")))
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot

# Reduce plot margins
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot.fv<-pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S5
png("Fig.S5.png", units="in", width=3, height=4, res=1200, type="cairo")
pahipi.auc.6.fold.cv.by.region.threatened.criterionB.includedvsexcluded.plot.fv
dev.off()

#----
# 10-fold cv - criterion B excluded
pahipi.auc.10.fold.random.cv.threatened.criterionB.excluded.table<-rbind(pahipi.auc.10.fold.random.cv.generalists.threatened.criterionB.excluded, pahipi.auc.10.fold.random.cv.specialists.threatened.criterionB.excluded)
pahipi.auc.10.fold.random.cv.threatened.criterionB.excluded.table$criterionB<-"Excluded"
pahipi.auc.10.fold.random.cv.table$criterionB<-"Included"
vars<-c("auc", "ci_lower", "ci_upper", "parameter", "land.model", "habitat_breadth", "criterionB")
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table<-rbind(pahipi.auc.10.fold.random.cv.table[,vars], pahipi.auc.10.fold.random.cv.threatened.criterionB.excluded.table[,vars])

# Plot AUCs ~ landscape models
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$cv<-factor(pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$criterionB, levels=c("Excluded", "Included"))
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$parameter<-factor(pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$parameter, levels=c("pa", "hi", "pi"))
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$habitat_breadth<-factor(pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table$habitat_breadth , levels=c("Generalists", "Specialists"))
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot<-ggplot(pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.table, aes(x=parameter, y=auc, color=criterionB)) +
  geom_errorbar(width=0.2, aes(ymin=ci_lower, ymax=ci_upper), position=position_dodge(width=0.7)) +
  geom_point(size=1.5, aes(shape=criterionB), position=position_dodge(width=0.7)) +
  labs(y="AUC", x="") +
  theme_bw() +
  theme(legend.background=element_rect(fill="white", size=0.5, linetype="solid", colour="white"),
        legend.margin=margin(1, 1, 1, 1),
        legend.spacing.x = unit(1, 'mm'), 
        legend.position="bottom", legend.title = element_text(size=7), legend.text=element_text(size=7),
        legend.justification = "left", legend.direction = "horizontal",
        panel.border = element_rect(colour = "grey85", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", colour = "grey", size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', colour = "white"), 
        plot.title = element_text(color="black", size=7, hjust = 0.5), 
        axis.title.x = element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust=1),
        axis.title.y = element_text(size=7), axis.text.y = element_text(size=7),
        strip.background = element_rect(color="grey85"),
        strip.text.x = element_text(size=7)) +
  scale_shape_manual(values=c(1, 1, 1)) + 
  scale_color_manual(values=c("#FF61CC", "#8494FF")) + 
  scale_x_discrete(labels=c("ER ~ PA + RPA", "ER ~ HI + RHI", "ER ~ PI + RPI")) +
  geom_hline(yintercept = 0.5, linetype="dashed", 
             color = "#8DD3C7", size=0.5) + 
  guides(color=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top"), shape=guide_legend(title="Threatened species under IUCN criterion B", title.position = "top")) +
  facet_grid(~habitat_breadth, scales="free_y") 
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot

# Reduce plot margins
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot.fv<-pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot + theme(plot.margin = unit(c(0,1,0,1), "lines"))

# Export Fig.S6
png("Fig.S6.png", units="in", width=3, height=4, res=1200, type="cairo")
pahipi.auc.10.fold.cv.threatened.criterionB.includedvsexcluded.plot.fv
dev.off()
