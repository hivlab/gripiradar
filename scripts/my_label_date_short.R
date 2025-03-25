
force_all <- function(...) list(...)
changed <- function(x) c(TRUE, is.na(x[-length(x)]) | x[-1] != x[-length(x)])

my_label_date_short <- function (format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n") 
{
  force_all(format, sep)
  function(x) {
    dt <- unclass(as.POSIXlt(x))
    changes <- cbind(year = changed(dt$year), month = changed(dt$mon), 
                     day = changed(dt$mday))
    changes <- t(apply(changes, 1, cumsum)) >= 1
    if (inherits(x, "Date") || all(dt$hour == 0 & dt$min == 
                                   0, na.rm = TRUE)) {
      format[[4]] <- NA
      if (all(dt$mday == 1, na.rm = TRUE)) {
        format[[3]] <- NA
        if (all(dt$mon == 0, na.rm = TRUE)) {
          format[[2]] <- NA
        }
      }
    }
    for_mat <- cbind(
      ifelse(changes[, 1], format[[1]], NA), 
      format[[2]], 
      format[[3]], 
      format[[4]]
    )
    format <- apply(for_mat, 1, function(x) paste(rev(x[!is.na(x)]), collapse = sep))
    format(x, format)
  }
}


