library(tidyverse)
library(ggplot2)
library(ggExtra)
library(gridExtra)
setwd('')
colnames(read.csv(''))

plot_list <- list()
variables <- c("ESV_.ml.","ESV.BSA_.ml.m2.","EDV_.ml.","EDV.BSA_.ml.m2.","SV_.ml.","SV.BSA_.ml.m2.","EF_..." , "CO_.l.min.","CI_.l.min.m2.")             
units <- c('ESV (ml)','ESV/BSA (ml/m²)','EDV (ml)','EDV/BSA (ml/m²)','SV (ml)','SV/BSA (ml/m²)','EF (%)','CO (l/min)','CI (l/min/m²)')

for (i in 1:length(variables)) {

variable = variables[i]
unit=units[i]

  # Create a nice theme
  theme_update(axis.line.y = element_line(colour="black"),
               axis.line.x = element_line(colour="black"),
               axis.text = element_text(colour="black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.text.x  = element_text(size=10),
               axis.text.y  = element_text(size=10),
               axis.title.x  = element_text(size=13,vjust=1, face="plain"),
               axis.title.y  = element_text(size=13, face = "plain", vjust=1, angle = 90),
               axis.line = element_line(size = 1.2, linetype = "solid"),
               axis.ticks = element_line(size = 1), legend.position="top")
  
  ids <-read.csv()
  
  gated <- read.csv()
  rownames(gated) <- gated$BRU
  gated <- drop_na(select(gated, !!sym(variable)))
  
  rt <- read.csv()
  rownames(rt) <- rt$BRU
  rt <- drop_na(select(rt,!!sym(variable)))
  
  rt <-rt[rownames(rt) %in% ids$BRU, , drop = FALSE]
  gated <- gated[rownames(gated) %in% ids$BRU, , drop = FALSE]
  
  rt <-rt[rownames(rt) %in% rownames(gated), , drop = FALSE]
  gated <- gated[rownames(gated) %in% rownames(rt), , drop = FALSE]
  
  
  diff<-unlist(rt[rownames(gated), , drop = FALSE] - gated[rownames(gated), , drop = FALSE])
  mean <- unlist(unlist(rt[rownames(gated), , drop = FALSE] + gated[rownames(gated), , drop = FALSE])/2)
  my.data <- data.frame(mean, diff)
  
  # pdf("BA/ba_EF.pdf", width=10, height=10)
  diffplotp <- ggplot(my.data, aes(mean, diff)) + 
       geom_point(size=1.5, colour = rgb(0,0,0, alpha = 0.5)) + 
       theme_update() + 
       geom_hline(yintercept = 0, linetype = 3) +
       geom_hline(yintercept = mean(my.data$diff)) +
       geom_hline(yintercept = mean(my.data$diff) + 2*sd(diff), linetype = 2) +
       geom_hline(yintercept = mean(my.data$diff) - 2*sd(diff), linetype = 2) +
       ylab(paste(unit, " - Difference")) +
       xlab("Mean")
  
  marginal_plot <- ggMarginal(diffplotp, type="histogram", bins = 25, color= "grey60", fill = "gray90")

  plot_list[[i]]<-marginal_plot}




grid.arrange(
  grobs = plot_list,
  ncol = 3,
  nrow = 3
)