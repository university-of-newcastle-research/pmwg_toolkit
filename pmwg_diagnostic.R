### Plotting function for diagnosing covariance matrix problems
#### Contributed by Gavin Cooper

diagnostic_plot <- function(
      sampler,
      from = 1,
      to = sampler$samples$idx,
      inv = FALSE,
      diag = FALSE,
      det = FALSE) {
  plot_title <- ifelse(inv, "Inverse of Covariance Matrix", "Covariance Matrix")
  flags <- c(inv & diag, inv & !diag, !inv & diag, !inv & !diag)
  ax <- c("diagonal", "inverse matrix", "variance vector", "covariance matrix")

  if (det) {
    plot_yaxis <- "Matrix Determinant"
  } else {
    plot_yaxis <- paste("Mean of", ax[which(flags)])
  }

  #Main plot
  plot(
    apply(sampler$samples$theta_sig[, , from:to], 3, function(x) {
      if (inv) {
        x <- MASS::ginv(x)
      }
      if (det) {
        det(x, log = TRUE)
      } else {
        if (diag) {
          x <- diag(x)
        }
        mean(x)
      }
    }),
    main = plot_title, xlab = "Iteration", ylab = plot_yaxis, type = "l"
  )

  # Stage markers
  stages <- data.frame(unclass(rle(sampler$samples$stage[from:to])))
  stages$cumsum <- cumsum(stages$lengths)
  abline(v = stages$cumsum)
  coords <- par("usr")
  ypos <- coords[3] + (coords[4] - coords[3]) / 100
  text(stages$cumsum, ypos, paste(stages$values, "<-"), adj = c(1, 0))
}
