#######  SPSS style frequency table ################################################### 


####### Set up... ##############################################################

library(haven)
library(tidyverse)
library(labelled)
library(flextable)

# Load BSA teaching data
df <- read_sav('/Users/joecrowley/R/Data/BSA Teaching Data/spss/spss25/bsa2019_poverty_open.sav', user_na = T)

look_for(df)
df %>% count(skipmeal)

# Extract factor...
var <- df %>% pull(skipmeal)
label <- paste0(df %>% select(skipmeal) %>% names, " - ", var_label(var))

# Factor with all levels 
var_all <- var %>% to_factor(levels = "prefixed")

# Factor with all non-user NA levels
var_no_NA <- var %>% to_factor(levels = "prefixed", user_na_to_na = T) %>% fct_drop

# Factor with all user NA levels
var_only_NA <- var_all[!(var_all %in% var_no_NA)] %>% fct_drop

table <- 
  reduce(list(
  
    var_no_NA %>% 
      na.omit() %>%
      fct_count(prop = T) %>% 
      filter(!is.na(f)) %>% 
      mutate(
        cum_p = paste0(round(cumsum(p)*100,2),"%"),
        p = paste0(round(p*100,2),"%"), 
        type = "valid"),
    
    var_only_NA %>% fct_count(prop = F) %>% mutate(type = "user_na"), 
    
    var_all %>% fct_count(prop = T) %>%
      select(-n) %>% rename(total_p = p)),

    full_join) %>% 
  
  mutate(
    cum_p_total = paste0(round(cumsum(total_p)*100,2),"%"),
    total_p = paste0(round(total_p*100,2),"%")) %>% 
  
  relocate(type) 

names(table)[2] <- label

table

table %>% flextable %>% autofit() %>%
  bg(
    i = ~ type == "user_na",
    j = NULL,           # all columns
    bg = "lightgrey"         # fill color
  ) %>% 
  bg(
    i = ~ type != "user_na",
    j = NULL,           # all columns
    bg = "lightblue"         # fill color
  ) %>% 
  bg(
    part = "header",
    bg = "lightgrey"         # fill color
  ) %>% 
  delete_columns("type") %>% 
  set_header_labels(
    n = "Frequency",
    p = "%", 
    cum_p = "Cumulative %", 
    total_p = "Total \n %", 
    cum_p_total = "Total \n Cumulative %" 
  )  %>%
  align(j = -1, align = "right", part = "all")%>% 
  vline(
    j = c(1,2,4),   # Add vertical lines after each column except the last
    part = "all"
  )

na_vals <- attr(df$skipmeal, "na_values")

df$skipmeal[df$skipmeal %in% na_vals]

# Filter only the user_na labels
user_na_labels <- all_labels[names(all_labels) %in% names(all_labels[all_labels %in% na_vals])]

user_na_labels


.freq_spss <- function(data, var, format = NULL) {
  # Extract factor...
  variable <- data %>% pull({{var}})
  label <- paste0(data %>% select({{var}}) %>% names, " - ", var_label({{variable}}))
  
  # Factor with all levels 
  var_all <- variable %>% to_factor(levels = "prefixed") %>% fct_drop
  
  # Factor with all non-user NA levels
  var_no_NA <- variable %>% to_factor(levels = "prefixed", user_na_to_na = T) %>% fct_drop
  
  # Factor with all user NA levels
  user_na <- attr(variable, "na_values")
  var_only_NA <- variable[variable %in% user_na] %>% to_factor(levels = "prefixed") %>% fct_drop
  # var_only_NA <- var_all[!(var_all %in% var_no_NA)] %>% fct_drop
  
  suppressMessages(
  table <- 
    reduce(list(
      
      var_no_NA %>% 
        na.omit() %>%
        fct_count(prop = T) %>% 
        filter(!is.na(f)) %>% 
        mutate(
          cum_p = paste0(round(cumsum(p)*100,2),"%"),
          p = paste0(round(p*100,2),"%"), 
          type = "valid"),
      
      var_only_NA %>% fct_count(prop = F) %>% mutate(type = "user_na"), 
      
      var_all %>% fct_count(prop = T) %>%
        select(-n) %>% rename(total_p = p)),
      
      full_join) %>% 
    
    mutate(
      cum_p_total = paste0(round(cumsum(total_p)*100,2),"%"),
      total_p = paste0(round(total_p*100,2),"%")) %>% 
    
    relocate(type) 
  )
  
  names(table)[2] <- label
  
  if(!is.null(format)){
  table <- 
    table %>% flextable %>% autofit() %>%
      bg(
        i = ~ type == "user_na",
        j = NULL,           # all columns
        bg = "lightgrey"         # fill color
      ) %>% 
      bg(
        i = ~ type != "user_na",
        j = NULL,           # all columns
        bg = "lightblue"         # fill color
      ) %>% 
      bg(
        part = "header",
        bg = "lightgrey"         # fill color
      ) %>% 
      delete_columns("type") %>% 
      set_header_labels(
        n = "Frequency",
        p = "Percentage", 
        cum_p = "Cumulative %", 
        total_p = "Total \n %", 
        cum_p_total = "Total \n Cumulative %" 
      )  %>%
      align(j = -1, align = "right", part = "all")%>% 
      vline(
        j = c(1,2,4),   # Add vertical lines after each column except the last
        part = "all"
      )
  }
  
  return(table)
  
}

.freq_spss(data = df, var = skipmeal)
.freq_spss(data = df, var = skipmeal, format = TRUE)
.freq_spss(data = df, var = Poverty1, format = TRUE)

