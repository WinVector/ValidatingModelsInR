source('../WVPlots/sourceAllWVPlots.R')

# find the y value that approximately corresponds to an x value on the gain curve
get_gainy = function(frame, xvar, truthVar, gainx) {
  # The sort order for predicted salary, decreasing
  ord = order(frame[[xvar]], decreasing=TRUE)

  # top 25 predicted salaries
  n = round(nrow(frame)*gainx)
  topN = ord[1:n]

  truth_topN= sum(frame[topN, truthVar])
  totalY = sum(frame[[truthVar]])
  round(100*truth_topN/totalY)/100  # two sig figs
}

# take the standard WVPlots gain curve and add extra notation
GainCurvePlotWithExtras = function(frame, xvar, truthVar, title, gainx, labelfun) {
  gainy = get_gainy(frame, xvar, truthVar, gainx)
  label = labelfun(gainx, gainy)
  gp = GainCurvePlot(frame, xvar, truthVar, title) +
    geom_vline(xintercept=gainx, color="red", alpha=0.5) +
    geom_hline(yintercept=gainy, color="red", alpha=0.5) +
    scale_shape_discrete(guide=FALSE) +
    geom_text(x=gainx+0.01, y=gainy-0.01,
               color="black", label=label, vjust="top", hjust="left", size=5)
  gp
}
