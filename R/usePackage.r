usePackage <- function(p) {
  newPackages <- p[!(p %in% installed.packages()[, "Package"])]
  if(length(newPackages))
    install.packages(newPackages, dependencies = TRUE)
  cat("Packages successfully loaded:\n")
  sapply(p, require, character.only = TRUE, quietly = TRUE)
}

# devtools old_ver included.