#' @title myddt function
#'
#' @param df ddt data set
#' @param x x variable is length of fish
#' @param y y variable is weight of fish
#' @param cond condition that can be fish bigger than a specified length
#' @param species variable used to select the species of fish used in the graph
#' @param col just a variable to pick the colors later
#'
#' @return A sophisticated plot, csv files, prints list of tables
#' @export
#'
#' @examples
myddt <- function(df, x, y, cond, species, col){

  df1 <- df %>% filter(grepl(species,ddt$SPECIES)) # Note the use of {{}}

  result = paste("\\Users\\user\\Desktop\\Projects\\Project 1", species, sep = "")
  write.csv(df1, result, row.names=FALSE)


  g <- ggplot(df1, aes_string(x=x,y=y)) + # Note the use of aes_string
    geom_point(aes_string(color = col )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    labs(title = "Sarah Karamitis")
  print(g)
  head(df1)
  rf <- table(ddt$RIVER)/length(ddt$RIVER)

  #data <- list(df, df1, rf)


}
