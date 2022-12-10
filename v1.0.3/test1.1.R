


library(rlang)


eval(parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))"))


txt <- paste0(
  "mtcars %>% dplyr::rename(", 
  paste0(
    mapply(paste, paste0(names(mtcars)[1:4], "_new"), names(mtcars)[1:4], 
           sep = " = "), collapse = ", "
  ), 
  ")"
)
txt
str(txt)
eval(parse_exprs(txt))

eval(parse(text = txt))


log_code <- function(..., plog = NULL) {
  if (is.null(plog)) plog <- list()
  plog[[length(plog)+1]] <- paste0(..., collapse = "")
  plog
}

txt <- log_code("mtcars %>% dplyr::rename(", 
         paste0(
           mapply(paste, paste0(names(mtcars)[1:4], "_new"), names(mtcars)[1:4], 
                  sep = " = "), collapse = ", "
         ), 
         ")")
eval(parse(text = txt))
