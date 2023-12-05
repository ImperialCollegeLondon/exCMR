library(tidyverse)
library(gridExtra)


setwd()

#read in data (merged resting + exercise data)
df <- read.csv()
colnames(df)

plot_list <- list()
# Define the lists of parameters
rest_pars <- c('ESV_.ml._rest',"ESV.BSA_.ml.m2._rest","EDV_.ml._rest","EDV.BSA_.ml.m2._rest", 'SV_.ml._rest',"SV.BSA_.ml.m2._rest","EF_..._rest","CO_.l.min._rest","CI_.l.min.m2._rest")
exercise_pars <- c('ESV_.ml._exercise',"ESV.BSA_.ml.m2._exercise","EDV_.ml._exercise","EDV.BSA_.ml.m2._exercise", 'SV_.ml._exercise',"SV.BSA_.ml.m2._exercise","EF_..._exercise","CO_.l.min._exercise","CI_.l.min.m2._exercise")
units <- c(expression(paste("ESV (ml)")),expression(paste("ESV/BSA (ml/m²)")),expression(paste("EDV (ml)")),expression(paste("EDV/BSA (ml/m²)")),expression(paste("SV (ml)")),expression(paste("SV/BSA (ml/m²)")),expression(paste("EF (%)")),expression(paste("CO (l/min)")),expression(paste("CI (l/min/m²)")))
expected_changes_up <- c(FALSE, FALSE, TRUE, TRUE,TRUE, TRUE,TRUE, TRUE,TRUE)
groups <- 'decade'

# rest_pars <- c("BASELINE.HR","BASELINE.HR")
# exercise_pars <- c("HR.reached","HR.reached") 
# units <- c(expression(paste("HR (BPM)")),expression(paste("HR (BPM)")))
# expected_changes_up <- c(TRUE,TRUE)
# groupst <- c('decade','Sex')


# Assuming all lists are of the same length
for (i in 1:length(rest_pars)) {

  #choose parameters, unit, and expected change
  rest_par <- rest_pars[i]
  exercise_par <- exercise_pars[i]
  unit <- units[i]
  expected_change_up <- expected_changes_up[i]
  
  #gender comparison = 'Sex'; age group comparison = 'decade'
  # groups <- 'decade'
  
  # Select the specific columns from the existing dataframe df
  df_selected <- df %>% select(BRU, !!sym(groups), !!sym(rest_par), !!sym(exercise_par))
  
  # Check for empty/missing values and drop the rows with any empty values
  df_clean <- df_selected %>% drop_na()
  
  # Combine edv_exercise and edv_rest into a single column called edv, and create another column indicating whether the value came from edv_rest or edv_exercise
  df_final <- df_clean %>%
    pivot_longer(cols = c(!!sym(rest_par), !!sym(exercise_par)),
                 names_to = "exercise",
                 values_to = "par") %>%
    mutate(exercise = ifelse(exercise == rest_par , -1, 1))
  
  # Calculate the difference between edv_exercise and edv_rest for each individual
  df_final_diff <- df_final %>%
    group_by_at(c("BRU", groups)) %>% 
    summarize(diff = diff(par)) %>%
    ungroup()
  
  if(groups=='Sex'){
    df_final <- left_join(df_final, df_final_diff, by = c("BRU", "Sex"))
    
    # Define a custom labeller
    custom_labeller <- function(variable, value) {
      age_ranges <- c('Women','Men')
      return(setNames(age_ranges, c('F', 'M')))
    }
    
    
  } else{
    df_final <- left_join(df_final, df_final_diff, by = c("BRU", "decade"))
    
    # Define a custom labeller
    custom_labeller <- function(variable, value) {
      age_ranges <- c("20-39 years", "40-59 years", "60-79 years")
      return(setNames(age_ranges, c(2, 4, 6)))
    }
  }
  
  if (expected_change_up) {
    color_choice <- c("TRUE" = 'blue', "FALSE" = "red")
  } else {
    color_choice <- c("TRUE" = 'red', "FALSE" = "blue")
  }
  
  
  p <- ggplot(df_final, aes(y = par, group = interaction(!!sym(groups), BRU), color = diff > 0)) +
       geom_line(aes(x = exercise), size = 1, alpha = 0.5) +
       geom_point(aes(x = exercise), shape = 21, fill = "white", stroke = 1.5, size = 2.5) +
       geom_boxplot(aes(x = exercise * 2, group = as.factor(exercise)), width = 0.5, color='black') +
       scale_x_continuous(breaks = c(-1.5, 1.5), labels = c("Rest", "Exercise")) +
       ylab(unit) +
       xlab('') +
       facet_wrap(as.formula(paste0('~', groups)), ncol = 3, labeller = labeller(!!sym(groups) := custom_labeller))+
       scale_color_manual(values = c(color_choice)) +
       theme(
            axis.ticks.x = element_blank(),
            axis.text = element_text(colour = "black", size = 10),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 1.2, linetype = "solid"),
            axis.ticks = element_line(size = 1),
            axis.text.x  = element_text(size = 10, face = "bold"),
            axis.title = element_text(size = 13, face = "bold"),
            axis.text.y  = element_text(size = 10),
            legend.position = "none",
            strip.background = element_rect(fill = "transparent"),
            strip.text = element_text(size = 13),
            panel.border = element_rect(linetype = "solid", color = "black", size = .5, fill = 'transparent'),
       )
  
  plot_list[[i]]<-p}
  

grid.arrange(
  grobs = plot_list,
  ncol = 3,
  nrow = 3
)
