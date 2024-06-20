demo <- function() {
  for (i in seq(10)) {
    message(paste("Processing iteration:", i))
    Sys.sleep(0.5)  # Simulando tarea larga
  }
  return(quantile(rnorm(1000)))
}

log_file <- "temp.txt"

con <- file(log_file, open = "a")
sink(con, type = "message")
demo()

