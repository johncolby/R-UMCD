library(ggplot2)

source('/Users/jcolby/Dropbox/UMCD.R')

# Setup requests
network_names = umcdListNetworks('ADHD200_CC200')$networks
network_names = network_names[grep('NeuroIMAGE', network_names)]

requests = data.frame(study_name   = 'ADHD200_CC200', 
                      network_name = network_names,
                      density      = '20',
                      orientation  = 'Axial',
                      weight       = 'Binary',
                      stringsAsFactors=F)

# Submit requests and obtain results
results = umcdAnalyze(requests)

# Format results
results$info = results$info[,-7]
results$info$age = as.numeric(gsub('(.+)-.+', '\\1', results$info$`Age Range`))
results$info$group = factor(ifelse(grepl('ADHD', results$info$`Subject Pool`), 'ADHD', 'TD'))
results$global.measures = results$global.measures[!is.infinite(results$global.measures$value), ]

# Check for group differences in all global metrics
ddply(results$global.measures, 'measure', function(x) summary(lm(value ~ group, data=x))$coef[2,])

# Plot all global metrics
dev.new(width=7, height=6)
qplot(age, value, color=group, data=results$global.measures) +
  facet_wrap(facets=~measure, scales='free_y') +
  geom_smooth(method='lm')
