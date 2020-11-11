#' Plots an lmSubsets object
#'
#' This function plots an lmSubsets object
#' @param lmSub an lmSubsets object
#' @param criterion the specified criterion: "AIC", "BIC", or "RSS"
#' @return a plot of the lmSubsets object
#' @export

plot.lmSubsets = function(lmSub, criterion){
  require(plotly)
  require(lmSubsets)

  mod = lmSub

  if (criterion == "RSS"){
    df = mod$submodel
    df$SIZE = factor(df$SIZE, levels = mod$size)
    df$BEST = factor(df$BEST, levels = c(1:mod$nbest))
    names(df) = c("SIZE", "BEST", paste(criterion))
  } else if (criterion == "AIC"){
    IC = matrix(nrow = length(mod$size),ncol = mod$nbest)
    SIZE = matrix(nrow = length(mod$size),ncol = mod$nbest)
    BEST = matrix(nrow = length(mod$size),ncol = mod$nbest)
    for (i in 1:length(mod$size)){
      for (j in 1:mod$nbest){
        IC[i,j] = AIC(refit(mod, size = mod$size[i], best = j))
        SIZE[i,j] = mod$size[i]
      }
      IC[i,] = sort(IC[i,])
      BEST[i,] = rank(IC[i,], ties.method = "first")
    }
    IC = as.vector(t(IC))
    BEST = as.vector(t(BEST))
    SIZE = as.vector(t(SIZE))
    df = data.frame(cbind(SIZE, BEST, IC))
    df$SIZE = factor(df$SIZE, levels = mod$size)
    df$BEST = factor(df$BEST, levels = c(1:mod$nbest))
    names(df) = c("SIZE", "BEST", paste(criterion))
  } else if (criterion == "BIC"){
    IC = matrix(nrow = length(mod$size),ncol = mod$nbest)
    SIZE = matrix(nrow = length(mod$size),ncol = mod$nbest)
    BEST = matrix(nrow = length(mod$size),ncol = mod$nbest)
    for (i in 1:length(mod$size)){
      for (j in 1:mod$nbest){
        IC[i,j] = BIC(refit(mod, size = mod$size[i], best = j))
        SIZE[i,j] = mod$size[i]
      }
      IC[i,] = sort(IC[i,])
      BEST[i,] = rank(IC[i,], ties.method = "first")
    }
    IC = as.vector(t(IC))
    BEST = as.vector(t(BEST))
    SIZE = as.vector(t(SIZE))
    df = data.frame(SIZE, BEST, IC)
    df$SIZE = factor(df$SIZE, levels = mod$size)
    df$BEST = factor(df$BEST, levels = c(1:mod$nbest))
    names(df) = c("SIZE", "BEST", paste(criterion))
  }

  crit = names(df)[3]

  plot_ly(data = df,
          y =~ get(crit),
          x =~ SIZE,
          color =~(BEST),
          hoverinfo = "text",
          hovertext = ~paste("Best: ", BEST, "<br>", criterion, ": ", get(crit)),
          marker = list(size = 12),
          type = 'scatter') %>%
    layout(legend=list(title=list(text='<b> Best </b>'))) %>%
    layout(title = paste(criterion, 'of All subsets'),
           xaxis = list(showgrid = FALSE, title = "Size", zeroline = FALSE, showline = FALSE),
           yaxis = list(showgrid = FALSE, title = criterion, zeroline = FALSE, showline = FALSE))
}
