# ######################################################################################################################################
# #                                                                                                                                    #
# #                                         network overlap plot  (only works for binary variables)                                    #
# #                                                                                                                                    #
# #  Note that this graphical display has a flaw because it does not show all the relative overlap proportion where the complement of  #
# #  the sets defined by certain values of the selected covariate.                                                                     #
# #                                                                                                                                    #
# #  created by Yi-Da, Chiu   22/07/17                                                                                                 #
# #  revised by Yi-Da, Chiu   18/01/18                                                                                                 #
# #                                                                                                                                    #
# ######################################################################################################################################
#
# load("D:/Desktop in Lancaster uni/project in Lancaster/simulations/script_graphs for Jacks request/sys.data.Rda")
#
# ##################################################  1. subgroup data set-up  ########################################################
#
# covari.sel = c(1, 2, 6, 7, 10)   # the covariates we select
# n.covari = length(covari.sel)    # the number of the covariates we select
# cond = list()
# ss.subgrp = matrix(rep(0, (n.covari * 2) * (n.covari * 2)), nrow = (n.covari * 2))
# for (i in 1 : (n.covari)) {
#
#   #cond[[i]] = which(synthetic.data[ , covari.sel[i]] == 1)
#   #cond[[n.covari + i]] = which(synthetic.data[ , covari.sel[i]] == 0)
#
#   cond[[i]] = which(synthetic.data[ , covari.sel[i]] > 0)
#   cond[[n.covari + i]] = which(synthetic.data[ , covari.sel[i]] < 0)
#   ss.subgrp[i, i] = length(cond[[i]])
#   ss.subgrp[n.covari + i, n.covari + i] = length(cond[[n.covari + i]])
# }
#
# r.prop = diag(n.covari * 2)
# for (i in 1 : (n.covari - 1) ){
#
#   for (j in (i + 1) : (n.covari) ){
#
#       ss.subgrp[i, j] = length(intersect(cond[[i]], cond[[j]]))
#       ss.subgrp[j, i] = ss.subgrp[i, j]
#       ss.subgrp[n.covari + i, j] = length(intersect(cond[[n.covari + i]], cond[[j]]))
#       ss.subgrp[j, n.covari + i] = ss.subgrp[n.covari + i, j]
#       ss.subgrp[i, n.covari + j] = length(intersect(cond[[i]], cond[[n.covari + j]]))
#       ss.subgrp[n.covari + j, i] = ss.subgrp[i, n.covari + j]
#       ss.subgrp[n.covari +i, n.covari + j] = length(intersect(cond[[n.covari +i]], cond[[n.covari + j]]))
#       ss.subgrp[n.covari + j, n.covari + i] = ss.subgrp[n.covari + i, n.covari + j]
#     }
# }
# r.prop = ss.subgrp/diag(ss.subgrp)   # the subgroups in the row are the baseline to calculate the corresponding relative overlap proportions
#
# lab.subgrp2 = vector()
# for (i in 1: n.covari){
#
#  lab.subgrp2[i]  = paste(LETTERS[i], "1", sep = "")
#  lab.subgrp2[i + n.covari]  = paste(LETTERS[i], "2", sep = "")
# }
#
#
#
# ############################################################## 2. produce a graph  ##########################################################
#
# dev.new(width=10,height=10,noRStudioGD = TRUE)
# layout(matrix(c(1,1, 1,1, 1, 1, 2, 2), byrow = TRUE, nrow=4, ncol=2), heights=c(3,1))
#
# par(mar=c(1,2,2,2))
# plot(0, 0, xlim = c(-1.5,1.5), ylim = c(0,5.1), type="n", col="blue", xaxt="n", yaxt="n", xlab="",ylab=" ",bty = "o")
# box()
#
# text(0.1, 5.05, labels = " Overlap proportions of pairwise subgroups ", adj = c(0.5, 1), cex = 1.3, font=4)
#
# y1.pos = rep(seq(4.65, 0.15, len = 4)[1], n.covari); y2.pos = rep(seq(4.65, 0.15, len = 4)[2], n.covari);
# y3.pos = rep(seq(4.65, 0.15, len = 4)[3], n.covari); y4.pos = rep(seq(4.65, 0.15, len = 4)[4], n.covari);
# x1.pos = seq(-1.4, 1.4, len = n.covari)
#
# r.prop.tol = c(0,1)
# pal.2=colorRampPalette(c("black", "red", "yellow"), space="rgb")
# breaks <- seq(min(r.prop.tol, na.rm = T)-1e-8 , max(r.prop.tol, na.rm = T)+1e-8, length.out=100)
# levs=breaks
# col.vec = pal.2(length(breaks)-1)
# col.idx = matrix(0, nrow = n.covari * 2, ncol = n.covari * 2)
# for (i in 1 : (n.covari)){
#   for (j in 1 : n.covari){
#
#     col.idx1 = which(r.prop[i,j] <= breaks)[1]
#     col.idx[i, j] = col.vec[col.idx1 - 1]
#
#     col.idx1 = which(r.prop[i+n.covari,j] <= breaks)[1]
#     col.idx[i+n.covari, j] = col.vec[col.idx1 - 1]
#
#     col.idx1 = which(r.prop[i,j+n.covari] <= breaks)[1]
#     col.idx[i, j+n.covari] = col.vec[col.idx1 - 1]
#
#     col.idx1 = which(r.prop[i+n.covari,j+n.covari] <= breaks)[1]
#     col.idx[i+n.covari, j+n.covari] = col.vec[col.idx1 - 1]
#   }
# }
# segments(x1.pos %x% rep(1, n.covari), rep(y1.pos, n.covari), rep(x1.pos, n.covari), rep(y2.pos, n.covari),  lwd = 2, col = c(t(col.idx[1:n.covari,1:n.covari ])) )
# segments(x1.pos %x% rep(1, n.covari), rep(y2.pos, n.covari), rep(x1.pos, n.covari), rep(y3.pos, n.covari),  lwd = 2, col = c(t(col.idx[1:n.covari,(n.covari+1):(2*n.covari)  ])) )
# segments(x1.pos %x% rep(1, n.covari), rep(y3.pos, n.covari), rep(x1.pos, n.covari), rep(y4.pos, n.covari),  lwd = 2, col = c(t(col.idx[(n.covari+1):(2*n.covari),(n.covari+1):(2*n.covari) ])) )
#
#
# text(x1.pos-0.05, y1.pos, labels = lab.subgrp2[1:(n.covari)],   cex = 0.9, col = "blue")
# text(x1.pos-0.05, y2.pos, labels = lab.subgrp2[1:(n.covari)],   cex = 0.9, col = "blue")
# text(x1.pos-0.05, y3.pos, labels = lab.subgrp2[(n.covari+1):(n.covari*2)],   cex = 0.9, col = "blue")
# text(x1.pos-0.05, y4.pos, labels = lab.subgrp2[(n.covari+1):(n.covari*2)],   cex = 0.9, col = "blue")
#
# ### creeat plot 3
#
# par(mar=c(3.8,2,1,2))
# image.scale(r.prop.tol, col=pal.2(length(breaks)-1), breaks=breaks-1e-8,axis.pos=1)
# box()