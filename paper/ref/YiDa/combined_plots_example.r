##############################################################################################################################
#
#   This script just shows an example of combining two graphical displays.  To do this, the function of each display
#   should be changed so that they will not generate a new plot window and produce a display under the original layout setup! 
#   After this procedure, one can run the following code to combine the two displays.
#
#   Note that a few graphical displays which are created under the grid system are not compatible with others which are created  
#   under the graphics system. So, they may not be able to combine.
#
#   created by yi-da chiu 31/08/2017
#
##############################################################################################################################


# produce a new plot window

dev.new(width=10,height=10,noRStudioGD = TRUE)    

# make layout setup

layout(matrix(c(1,2,3, 1,2,3, 1,2,3, 4, 4, 4, 4, 4, 4,
                4, 4, 4, 0, 5, 0), byrow = TRUE, nrow= 7, ncol=3), widths=c(1,1, 1), heights=c(2.5, 2.5, 2.5, 2, 2, 2, 1))

main.title = list("", "Forestplot of subgroups", "Forestplot of subgroups (T | C)") 
               label.x = list("", "Log odds ratio", "Log odds")
               forestplt(dat = dat2, covari.sel = c(2, 3), trt.sel = 4, resp.sel = 5, outcome.type = "binary", title = main.title,
               lab.x = label.x)
               
main.title = "Relative overlapping proportions of pairwise subgroups";
mat.subgrp.op(dat2, c(2,3), mode = 1, font.size = c(1.2, 1), title = main.title)