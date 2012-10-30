library(ggplot2)

#source("nopvo_comparison_script.R")

setwd("C:/Users/Rebecca/Desktop/testing brew/graphs")

plot_total_errors = function(x){
  x$category = rownames(x)
  x = melt(x)
  ggplot(x, aes(x=variable, y=value, fill=category)) + geom_histogram(position = "dodge") +
    scale_x_discrete("Categories") +
    scale_y_continuous("Estimate errors") +
    opts(title = paste(variable, sep = ""))
}

l_ply(nopvo_regvars.err, plot_total_errors)


#prints out errors into a .pdf, not very exciting
pdf("test_report.pdf")
lapply(nopvo_regvars.err, plot_total_errors)
dev.off()

get_errors_by_bureau <- function(l){
  ld = ldply(l, function(df){df$.categories = rownames(df); melt(df)})
  dl = dlply(ld, .variables="variable")
  return(dl)
}

nopvo_regvars_bb.err = get_errors_by_bureau(nopvo_regvars.err)


plotter = function(temp){
  d_ply(temp, .(.id), function(dat){
    
    filename <- function(y){
      paste("graphs_", dat$variable, "_", dat$.id, y, ".pdf", sep = "")
    }
    
    p = ggplot(dat, aes(x = .categories, y = value)) + 
      geom_histogram(position = "dodge") +
      scale_x_discrete("Categories") +
      scale_y_continuous("Estimate errors") +
      opts(
        title = paste("NOPVO Bureau ", dat$variable, ": ", dat$.id, sep = ""))
    ggsave(filename("_test"), p, dpi = 100)
  })
}

l_ply(nopvo_regvars_bb.err, plotter)