get_urbanness_table <- function () {
  urbanness <- readRDS('Data/urbanness/urbanness.RDS')
  if(ncol(urbanness) == 19){
    colnames(urbanness)[c(5,14)] <- c('EVI', 'avg_rad')
    urbanness <- urbanness[, -c(1, 19)]
  }
  urbanness
}

urbanness_pdf <- function (urbanness_table, print_pdf = T) {
  require(ggplot2)
  if(print_pdf) pdf('figures/urbanness.pdf')
  print(
    ggplot(urbanness_table, aes(EVI)) + geom_histogram() + theme_bw()
  )
  print(
    ggplot(urbanness_table, aes(avg_rad)) + geom_histogram(bins = 50) + theme_bw() + scale_x_log10()
  )
  print(
    ggplot(urbanness_table, aes(avg_rad_stdDev)) + geom_histogram(bins = 50) + theme_bw() + scale_x_log10()
  )
  print(
    ggplot(urbanness_table, aes(avg_rad, avg_rad_stdDev)) + geom_point() + theme_bw() + scale_x_log10()
  )
  smoothScatter(log(urbanness_table$avg_rad), urbanness_table$avg_rad_stdDev)
  smoothScatter(log(urbanness_table$avg_rad), urbanness_table$EVI)
  smoothScatter(log(urbanness_table$avg_rad_stdDev), urbanness_table$EVI_stdDev)
  smoothScatter(urbanness_table$avg_rad_count, urbanness_table$avg_rad_stdDev)
  dev.off()
}
  

