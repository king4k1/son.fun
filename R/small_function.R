## small function

### replace na to 0
replace_na_0 <- function(x) ifelse(is.na(x), 0, x) %>% as.numeric() 

### replace null to na
replace_null_na <- function(x) ifelse(x==""|x==" ", NA, x)

### replace na to null
replace_na_null <- function(x)ifelse(is.na(x), "", x)

### remove info in bracket
remove_bracketinfo <- function(x){
  str_sub(x, 1, ifelse(str_detect(x, fixed("(")),
                       str_locate(x, fixed("("))-1,
                       100))}