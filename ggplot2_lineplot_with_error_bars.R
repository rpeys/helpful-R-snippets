##How to plot a line graph with error bars in ggplot2
#source: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a tidy data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#summarize data
df <- data_summary(gene.rpkm, varname="RPKM", 
                   groupnames=c("condition", "timepoint", "gene"))
head(df)

#plot
# Use position_dodge to move overlapped errorbars horizontally
ggplot(df, aes(x=timepoint, y=RPKM, group=condition, color=condition)) + 
  geom_errorbar(aes(ymin=RPKM-sd, ymax=RPKM+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point() + facet_wrap(~gene, scales = "free") +
  theme(axis.text = element_text(size = 12, face = "bold"), axis.title = element_text(size = 12, face = "bold"), axis.text.x = element_text(angle=45, hjust =1, vjust = 1, size = 12), strip.text.x = element_text(size = 12, face = "bold"), legend.text=element_text(size=12), legend.title=element_text(size=12))
#scale_color_brewer(palette="Paired")+theme_minimal()
