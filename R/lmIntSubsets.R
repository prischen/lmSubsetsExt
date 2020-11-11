#' Finds exhaustively the "best" strong hierarchical model
#'
#' This function converts input temperatures in Fahrenheit to Celsius.
#' @param ... forwarded to lmSubsets()
#' @return The best strong hierarchical model
#' @export

lmIntSubsets = function(formula, data, ...) {

  tt = terms(formula, data = data)
  te = attr(tt, "term.labels")
  re = attr(tt, "variables")[[2]]

  interactions = te[grep(":",te)]
  not_interactions = te[!te %in% interactions]

  res = lmSubsets::lmSubsets(formula, data, ...)

  sub = data.frame(cbind(res$submodel, res$subset))
  colnames(sub) = c(colnames(res$submodel), colnames(res$subset))
  terms = not_interactions
  violate = rep(FALSE,dim(sub)[1])

  for (i in 1:dim(sub)[1]){
    for (j in 1:(length(terms)-1)){
      for (k in (j+1):length(terms)){
        if (!violate[i]){
          if (!is.na(sub[i,"RSS"])){
            int_term = paste(terms[j], ":", terms[k], sep = "")
            if (int_term %in% interactions){
              violate[i] = !((sub[i,terms[j]] & sub[i,terms[k]]) == sub[i, int_term])
            } else{
              int_term = paste(terms[k], ":", terms[j], sep = "")
              violate[i] = !((sub[i,terms[k]] & sub[i,terms[j]]) == sub[i, int_term])
            }
          }
        }
      }
    }
  }

  res$submodel[violate == TRUE, "RSS"] = NA
  res$subset[violate == TRUE,] = NA

  class(res) = c("lmSubsetsInt", class(res))

  return(res)
}

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
