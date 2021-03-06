# p_lin: Predict Linear
# Returns a 100 row (x,y) data frame, which describe a line.
p_lin <- function(df, response, term) {
  fm <- lm(as.formula(paste(response, "~", term)), data = df)
  df_p <- data.frame(x = (1:100)/100)
  df_p[[term]] <- (1:100)/100
  curve <- data.frame(x = (1:100)/100, y = predict(fm, df_p))
  return(curve)
}

p_supsmu <- function(df, response, term, b=10) {
  curve = supsmu(df[[term]], df[[response]], bass=b)
  return(curve)
}

n_clip <- function(x, a = 0, b = 1) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}

t_color <- function(x, levels, colors) {
  #debug
  #x = c(0.23, 0.45, 0.95, 0.01)
  #l = tibble(levels = c(0, 0.2,0.55,0.85,1.1),
  #           colors = c("g0","g1", "g2", "g3", "g4"))
  #levels = l$levels
  #colors = l$colors
  
  toMatch = sapply(x, function(x) min(levels[levels>=x]))
  
  match = match(toMatch, levels)
  c <- colors[match]
  
  return(c)
}
