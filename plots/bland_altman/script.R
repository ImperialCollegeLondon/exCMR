library(tidyverse)

#select variables and units
variable = "SV.BSA_.ml.m2."
unit='ESV/BSA (ml/m²)'

# Create a nice theme
theme_update(axis.line.y = element_line(colour="black"),
             axis.line.x = element_line(colour="black"),
             axis.text = element_text(colour="black"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             axis.text.x  = element_text(size=22),
             axis.text.y  = element_text(size=22),
             axis.title.x  = element_text(size=28,vjust=1, face="plain"),
             axis.title.y  = element_text(size=28, face = "plain", vjust=1, angle = 90),
             axis.line = element_line(size = 1.2, linetype = "solid"),
             axis.ticks = element_line(size = 1), legend.position="none")

#data import and cleaning
gated <- read.csv('GenScanII_sax_cine_rest.csv')
rownames(gated) <- gated$BRU
gated <- drop_na(select(gated, !!sym(variable)))

rt <- read.csv('GenScanII_sax_cine_RT_rest.csv',sep=';')
rownames(rt) <- rt$BRU
rt <- drop_na(select(rt,!!sym(variable)))

rt <-rt[rownames(rt) %in% rownames(gated), , drop = FALSE]
gated <- gated[rownames(gated) %in% rownames(rt), , drop = FALSE]


diff <- unlist(gated - rt)
mean <- unlist((gated + rt)/2)
my.data <- data.frame(mean, diff)

#plotting

# pdf("BA/ba_SV_BSA_at_rest_.pdf", width=10, height=10)
diffplotp <- ggplot(my.data, aes(mean, diff)) + 
     geom_point(size=2.5, colour = rgb(0,0,0, alpha = 0.5)) + 
     theme_update() + 
     geom_hline(yintercept = 0, linetype = 3) +
     geom_hline(yintercept = mean(my.data$diff)) +
     geom_hline(yintercept = mean(my.data$diff) + 2*sd(diff), linetype = 2) +
     geom_hline(yintercept = mean(my.data$diff) - 2*sd(diff), linetype = 2) +
     ylab(paste(unit, " - Difference")) +
     xlab("Mean")

marginal_plot <- ggMarginal(diffplotp, type="histogram", bins = 25, color= "grey60", fill = "gray90")
print(marginal_plot)
# dev.off()
