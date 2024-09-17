# Author: NaLette Brodnax
# Description: helper functions to save plots and assess missingness
library(lubridate)
library(extrafont)
loadfonts()

# color blind friendly palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

# pdf device wrapper
# for CM Roman https://github.com/wch/fontcm
ggplot_tex <- function(plot_obj, path, w = 6, h = 4){
  name <- deparse(substitute(plot_obj))
  filename = paste(path, name, ".pdf", sep = "")
  pdf(file = filename, width = w, height = h, family = "CM Roman")
  plot_obj
  print(plot_obj)
  dev.off()
  embed_fonts(filename, outfile = filename)
}

copy_offset <- function(df, from_col, start = 1, offset = 1){
  copy_seq <- seq(from = start, to = nrow(df), by = 1 + offset)
  to_col <- rep(as.numeric(NA), nrow(df))
  for (i in copy_seq){
    to_col[i] <- df[(i + offset), from_col]
  }
  return(unlist(to_col))
}

xalign_row <- function(chr_vec, align = "c"){
  chr_vec %>% 
    lapply(function(x) 
      paste("\\multicolumn{1}{", align, "}{", x, "}", sep = "")) %>% 
    unlist()
}

xadd_row <- function(row_contents, positions){
  addtorow <- list(list(positions), as.character(row_contents))
  return(addtorow)
  }

tally_missing <- function(df){
  missing <- df %>% 
    mutate_all(is.na) %>% 
    summarise_all(sum) %>% 
    gather(variable, missing)
  return(missing)
}

calc_age <- function(birth_date, to_date){
  dob <- mdy(birth_date)
  yr = duration(num = 1, units = "years")
  age <- as.period(interval(dob, mdy(to_date))/yr)
  return(as.numeric(age))
}
