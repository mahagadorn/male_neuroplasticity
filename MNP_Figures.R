##Figures for Male Neuroplasticity study: M.genalis and B.impatiens
##MA Hagadorn
##Last Modified 3/5/2021
##Most recent iteration: adjusting file paths 

#loadlibraries
library(ggplot2)
library(ggtext)
library(reshape2)
library(cowplot)
library(lemon)
library(ggtext)

#check working dir
getwd()

#M.GENALIS
#loading in the proportion data (relative to whole brain)
mgendata <- read.table("Data/Mgenalis_MNP_proportiondata.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
mgendata <- mgendata [,c(2:3,17:23)]
#changing to make consistent with bimp data labels
txgrouping <- mgendata$tx 
txgrouping <- gsub("ne", "NE", txgrouping)
txgrouping <- gsub("sham", "AGED", txgrouping)

mgendata <- cbind(txgrouping, mgendata)
mgendata <- mgendata[,-c(3)]




#Bimp
#loading in the proportion data (relative to whole brain)
bimpdata <- read.table("Data/Bimp_MNP_proportiondata.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
bimpdata <- bimpdata [,c(2:3,15:21)]



#Melt Data into Appropriate Format
##Make subset for calyces, kenyoncells, mblobes, nk

mgen_sub <- mgendata[,c("txgrouping", "mblobe_relWB", "calyces_relWB", "neuropil_relWB","kcs_relWB")]
MG <- rep("M. genalis", 14)
mgen_sub <- cbind(MG, mgen_sub)
colnames(mgen_sub) <- c("species", "tx", "mblobe_relWB", "calyces_relWB", "neuropil_relWB","kcs_relWB")
#"calyces_relWB", "kcs_relWB", "mblobe_relWB"


bimp_sub <- bimpdata[,c("treatment", "mblobe_relWB", "calyces_relWB", "neuropil_relWB","kcs_relWB")]
BI <- rep("B. impatiens", 18)
bimp_sub <- cbind(BI, bimp_sub)
colnames(bimp_sub) <- c("species", "tx", "mblobe_relWB", "calyces_relWB", "neuropil_relWB","kcs_relWB")



#melt data
MGmelted <- melt(mgen_sub, id.vars = c("species", "tx"))
BImelted <- melt(bimp_sub, id.vars = c("species", "tx"))

#assign order levels
MGmelted$tx <- factor(MGmelted$tx, levels=c("NE", "AGED"))
BImelted$tx <- factor(BImelted$tx, levels=c("NE", "AGED"))




generalsize <- 13
legendsize <- 9
wordsize <-10
starsize <- 6
lettersize <- 3
dotsize <- 2.3
speciessize <-3.8
seed <- 2012


#Make Labels for Plot
Label <- c("NE", "Mature")

MG_struct <- ggplot(MGmelted, aes(x=variable, y=value, fill=factor(tx))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=0.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=Label, name="Megalopta") +
  geom_point(aes(color = factor(tx)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=seed), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(tx)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=seed), size=dotsize+.01, stroke=.3) +
  ggtitle("a)") +
  scale_color_manual(values=c("NE"="darkolivegreen3", "AGED"="darkviolet"), labels=Label, name="Megalopta") +
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.18), breaks=seq(0.03,.18, .05), expand = c(0,0)) +
  scale_x_discrete(name = "", labels = c("MB\nLobes", "Calyces\n", "MB\nNeuropil", "Kenyon\nCells"), expand = c(0, 0)) +
  annotate(geom="text", x=2, y=.115, color="black", size = starsize, label="*", hjust=.5) + #significance CALYCES
  annotate(geom="text", x=3, y=.165, color="black", size = starsize, label="*", hjust=.5) + #significance TOTAL NEUROPIL 
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=10, face="bold"), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size = 0.3),
        panel.spacing = unit(0, "mm"),
        legend.title = element_text(size = legendsize, face = "italic"),
        legend.position = c(.1,.86),
        legend.margin = margin(l=2, r=2, t=2, b=2),
        legend.spacing.y = unit(0.85, "pt"),
        legend.spacing.x = unit(1, "pt"),
        legend.background = element_rect(fill="white", size=0.25, linetype="solid", colour ="black"),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size=legendsize),
        legend.key.height = unit(17, "pt"),
        legend.key.width = unit(.4, "cm"),
        legend.text.align = 0,
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = 6, r = 0, b = 0, l = 0), face="bold", vjust = 3, hjust = 0))


BI_struct <- ggplot(BImelted, aes(x=variable, y=value, fill=factor(tx))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=0.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=Label, name="Bombus") +
  geom_point(aes(color = factor(tx)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=seed), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(tx)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=seed), size=dotsize+.01, stroke=.3) +
  ggtitle("c)") +
  scale_color_manual(values=c("NE"="darkorange", "AGED"="mediumblue"), labels=Label, name="Bombus") +
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.18), breaks=seq(0.03,.18, .05), expand = c(0,0)) +
  scale_x_discrete(name = "", labels = c("MB\nLobes", "Calyces\n", "MB\nNeuropil", "Kenyon\nCells"), expand = c(0, 0)) +
  annotate(geom="text", x=2, y=.105, color="black", size = starsize, label="**", hjust=.5) + #significance
  annotate(geom="text", x=3, y=.15, color="black", size = starsize, label="*", hjust=.5) + #significance TOTAL NEUROPIL p 0.0014
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=10, face="bold"), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size = 0.3),
        panel.spacing = unit(0, "mm"),
        legend.title = element_text(size = legendsize, face = "italic"),
        legend.position = c(.1,.86),
        legend.margin = margin(l=2, r=2, t=2, b=2),
        legend.spacing.y = unit(0.85, "pt"),
        legend.spacing.x = unit(1, "pt"),
        legend.background = element_rect(fill="white", size=0.25, linetype="solid", colour ="black"),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size=legendsize),
        legend.key.height = unit(17, "pt"),
        legend.key.width = unit(.4, "cm"),
        legend.text.align = 0,
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = 6, r = 0, b = 0, l = 0), face="bold", vjust = 3, hjust = 0))


a_plot <- plot_grid(MG_struct, BI_struct, ncol = 1, align='v', rel_heights = c(.5,.5))







#***********************NK

mgen_NK <- mgendata[,c("txgrouping", "NK")]
mgen_NK <- cbind(MG, mgen_NK)
colnames(mgen_NK) <- c("species", "tx", "NK")



bimp_NK <- bimpdata[,c("treatment", "nk")]
bimp_NK <- cbind(BI, bimp_NK)
colnames(bimp_NK) <- c("species", "tx", "NK")


#Melt
MG_nk_melt <- melt(mgen_NK, id.vars = c("species", "tx"))
BI_nk_melt <- melt(bimp_NK, id.vars = c("species", "tx"))

#assign order levels
MG_nk_melt$tx <- factor(MG_nk_melt$tx, levels=c("NE", "AGED"))
BI_nk_melt$tx <- factor(BI_nk_melt$tx, levels=c("NE", "AGED"))



MG_nk <- ggplot(MG_nk_melt, aes(x=tx, y=value, fill=factor(tx))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=0.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=c("Newly-emerged", "Aged")) +
  geom_point(aes(color = factor(tx)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=seed), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(tx)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=seed), size=dotsize+.01, stroke=.3) +
  ggtitle("b)") +
  scale_color_manual(values=c("NE"="darkolivegreen3", "AGED"="darkviolet"), labels=c("Newly-emerged", "Aged")) +
  scale_y_continuous(name = "Neuropil to Kenyon cell ratio", limits = c(1.3,3.0), breaks=seq(1.4,3.0,.4), expand = c(0,0)) +
  scale_x_discrete(name = "", labels = c("NE\n", "Mature\n")) +
  annotate(geom="text", x=1.5, y=2.75, color="black", size = starsize, label="**", hjust=.5) + #significance
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=10, face="bold"), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size = 0.3),
        panel.spacing = unit(0, "mm"),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = 6, r = 0, b = 0, l = 0), face="bold", vjust = 3, hjust = 0))


BI_nk <- ggplot(BI_nk_melt, aes(x=tx, y=value, fill=factor(tx))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=0.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=c("Newly-emerged", "Aged")) +
  geom_point(aes(color = factor(tx)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=seed), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(tx)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=seed), size=dotsize+.01, stroke=.3) +
  ggtitle("d)") +
  scale_color_manual(values=c("NE"="darkorange", "AGED"="mediumblue"), labels=c("Newly-emerged", "Aged")) +
  scale_y_continuous(name = "Neuropil to Kenyon cell ratio", limits = c(1.3,3.0), breaks=seq(1.4,3.0,.4), expand = c(0,0)) +
  scale_x_discrete(name = "", labels = c("NE\n", "Mature\n")) +
  annotate(geom="text", x=1.5, y=2.9, color="black", size = starsize, label="**", hjust=.5) + #significance
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=10, face="bold"), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size = 0.3),
        panel.spacing = unit(0, "mm"),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = 6, r = 0, b = 0, l = 0), face="bold", vjust = 3, hjust = 0))

b_plot <- plot_grid(MG_nk, BI_nk, ncol = 1, align='v', rel_heights = c(.5,.5))

multipanel_plot <- plot_grid(a_plot, NULL, b_plot, rel_widths = c(.825,.08,.285), nrow = 1)


ggsave("Figure2.tiff",
       plot = multipanel_plot ,
       device = "tiff",
       path ="Figures/",
       scale = 1,
       width = 6,
       height = 6,
       units = "in",
       dpi = 600,
       limitsize = TRUE)

ggsave("Figure2.pdf",
       plot = multipanel_plot ,
       device = "pdf",
       path = "Figures/",
       scale = 1,
       width = 6,
       height = 6,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

