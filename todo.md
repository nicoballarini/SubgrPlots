# Bugs

Forest plot. Third panel colors for treatment and control in legend.
Fix xaxis in forest plots.
added eff.scale for showing HR instead of logHR

# Additional features

Forest Plot: 
  Allow to have only two panels 
  Warning messages:
  1: In doTryCatch(return(expr), name, parentenv, handler) :
    calling par(new=TRUE) with no plot
  #416 -> col.line duplicated
 
 Allow for results in HR scale
 Check time = NA option. Give error if not specified. Dont care about it if the KM TRUE
 Check for tibble and trnasform to data.frame
 Document labels in forest plot function
