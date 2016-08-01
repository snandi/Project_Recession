Seed <- 20
set.seed(Seed)
hhids <- unique(Data_forIncPov$hhid)

samplehhids <- sample(x = hhids, size = 500, replace = FALSE)
IncData <- Data_forIncPov
# fn_PlotSnapshot <- function(
#   IncData = Data_forIncPov, 
#   samplehhids = hhids
# ){

SnapshotUserData <- subset(IncData, hhid %in% samplehhids)

# PlotSnapshot <- qplot() + 
#   geom_line(aes(x = as.numeric(yearqtr), y = thtotinc, col = adult_disb, group = hhid), 
#             data = SnapshotUserData) 
# PlotSnapshot <- PlotSnapshot + 
#   theme(
#     legend.position = 'none', 
#     axis.text = element_text(size = 10, face = 'bold'), 
#     plot.title = element_text(size = 12, face = 'bold')
#   )

PlotSnapshot <- qplot() + 
  geom_boxplot(aes(x = factor(yearqtr), y = log(thtotinc), fill = adult_disb), 
               data = subset(SnapshotUserData, thtotinc > 1), size = 0.2,
               outlier.size = 0.4
  )
PlotSnapshot <- PlotSnapshot + xlab(label = '') + ylab(label = '') + 
  ggtitle(label = 'log(Monthly Household Income)')
#PlotSnapshot <- PlotSnapshot + theme_solarized_2(light = TRUE)
PlotSnapshot <- PlotSnapshot + 
  theme(
    legend.position = 'top', 
    axis.text.x = element_text(angle = 90, vjust = 0) #,
    # axis.text = element_text(size = 28, face = 'bold'), 
    # plot.title = element_text(size = 36, face = 'bold'),
    # legend.text = element_text(size = 24, face = 'bold'),
    # legend.title = element_text(size = 24, face = 'bold')
  ) 
PlotSnapshot <- PlotSnapshot + ylim(c(6.5, 9.5))

#PlotSnapshot

PlotWithRace <- PlotSnapshot + facet_wrap(~ race, nrow = 1) 
#PlotWithRace

PlotWithGenderMS <- PlotSnapshot + facet_wrap(~ gender_ms, nrow = 2)

#PlotWithGenderMS
PlotFilename <- paste0(SlidePath, 'BoxPlot_MnthlyInc', '.pdf')
pdf(file = PlotFilename, width = 8, height = 6, pointsize = 24, onefile = T)
PlotSnapshot
PlotWithRace
PlotWithGenderMS
dev.off()


PlotFilename <- paste0(SlidePath, 'BoxPlot_MnthlyInc', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = PlotSnapshot, 
  device = 'jpg', 
  width = 40,
  height = 30,
  units = 'cm'
)

PlotFilename <- paste0(SlidePath, 'BoxPlot_MnthlyInc_byRace', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = PlotWithRace, 
  device = 'jpg', 
  width = 90,
  height = 60,
  units = 'cm'
)

PlotFilename <- paste0(SlidePath, 'BoxPlot_MnthlyInc_byGenderMS', '.jpeg')
ggsave(
  filename = PlotFilename, 
  plot = PlotWithGenderMS, 
  device = 'jpg', 
  width = 90,
  height = 60,
  units = 'cm'
)
