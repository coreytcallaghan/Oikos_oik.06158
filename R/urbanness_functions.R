get_urbanness_table <- function () {
  readRDS('Data/urbanness/urbanness.RDS')
}

urbanness_pdf <- function (urbanness_table) {
  pdf("figures/urbanness.pdf")
  hist(urbanness_table$avg_rad)
  hist(urbanness_table$EVI)
  dev.off()
}
  