#' @importFrom plyr count

## Counts the frequency of each intersection being looked at and sets up data for main bar plot.
## Also orders the data for the bar plot and matrix plot
Counter_all <- function(data, num_sets, start_col, name_of_sets, nintersections, mbar_color, order_mat,
                    aggregate, cut, empty_intersects, decrease){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  #gets indices of columns containing sets used
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  
  all_comb = expand.grid(data.frame(matrix(rep(c(2,1,0),num_sets), ncol = num_sets)))
  names(all_comb) = name_of_sets
  # all_comb$freq = 0
  
  # i = nrow(all_comb)
  # all_comb[i, ]
  # all_comb[1, ]
  
  
  # freq.list = vector("list", nrow(all_comb))
  # intersect.list = vector("list", num_sets+1)
  # intersections = 0:num_sets
  # i=2
  # for (i in intersections){
  #   intersect.list[[i+1]] = choose(num_sets,i)
  #   
  #   if (i==0) {
  #     freq.dat = data.frame(freq = nrow(data))
  #     kk = 1
  #     freq.list[[kk]] = freq.dat
  #     next()  
  #   }
  #   intt = combn(1:num_sets, i)
  #   for (j in 1:ncol(intt)){
  #     kk = kk + 1
  #     sel.vars = intt[,j]
  #     not.sel.vars = setdiff(1:6,sel.vars)
  #     freq.dat = count(data[name_of_sets[sel.vars]])
  #     freq.list[[kk]] = freq.dat
  #   }
  # }
  # i=5
  # j=1
  # data[name_of_sets[sel.vars]]
  # all_comb[name_of_sets[sel.vars]]
  # 
  # all_comb_filter = all_comb[which(all_comb[name_of_sets[not.sel.vars]] == 2),name_of_sets[sel.vars]]
  # merge(all_comb_filter, freq.dat)
  # 
  
  
  i=1
  all_comb1 = all_comb
  for (i in 1:nrow(all_comb)){
    values = all_comb[i,]
    sel.vars = which(values!=2)
    not.sel.vars = setdiff(1:num_sets, sel.vars)  
    if (length(sel.vars)==0) {
      all_comb[i, "freq"] = nrow(data)
      next()
    }
    name_of_sets[sel.vars]
    name_of_sets[not.sel.vars]
    values[sel.vars]
    sel.subjects = apply(data[name_of_sets[sel.vars]],MARGIN = 1,
                         function(x) all(x == values[sel.vars]))
    # data[sel.subjects, ]
    all_comb[i, "freq"] = nrow(data[sel.subjects, ])
  }
  
  all_comb
  # freq.list[[1]]
  # freq.list[[2]]
  # freq.list[[10]]
  # 
  # template = cbind(all_comb[1,], freq = NA)
  # length(unlist(lapply(freq.list, function(xx) xx$freq)))
  # 
  # Freqs <- data.frame(count(data[ ,as.integer(temp_data)]))
  Freqs <- all_comb
  colnames(Freqs)[1:num_sets] <- name_of_sets
  #Adds on empty intersections if option is selected
  if(is.null(empty_intersects) == F){
    empty <- rep(list(c(0,1)), times = num_sets)
    empty <- data.frame(expand.grid(empty))
    colnames(empty) <- name_of_sets
    empty$freq <- 0
    all <- rbind(Freqs, empty)
    Freqs <- data.frame(all[!duplicated(all[1:num_sets]), ], check.names = F)
  }
  #Remove universal empty set
  # Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  #Aggregation by degree
  if(tolower(aggregate) == "degree"){
    for(i in 1:nrow(Freqs)){
      Freqs$degree[i] <- rowSums(Freqs[ i ,1:num_sets])
    }
    order_cols <- c()
    for(i in 1:length(order_mat)){
      order_cols[i] <- match(order_mat[i], colnames(Freqs))
    }
    # if(length(order_cols)==2 && order_cols[1]>order_cols[2]){decrease <- rev(decrease)}
    for(i in 1:length(order_cols)){
      logic <- decrease[i]
      Freqs <- Freqs[order(Freqs[ , order_cols[i]], decreasing = logic), ]
    }
  }
  #Aggregation by sets
  else if(tolower(aggregate) == "sets")
  {
    Freqs <- Get_aggregates(Freqs, num_sets, order_mat, cut)
  }
  #delete rows used to order data correctly. Not needed to set up bars.
  delete_row <- (num_sets + 2)
  Freqs <- Freqs[ , -delete_row]
  for( i in 1:nrow(Freqs)){
    Freqs$x[i] <- i
    Freqs$color <- mbar_color
  }
  if(is.na(nintersections)){
    nintersections = nrow(Freqs)
  }
  Freqs <- Freqs[1:nintersections, ]
  Freqs <- na.omit(Freqs)
  return(Freqs)
}
