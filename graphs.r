require(ggplot2)
require(reshape2)

final_rates = readRDS(file="../simulation/paths.rda")
melted = melt(final_rates, id='simulation')
summary(final_rates)


theme_set(theme_bw())
terms = c("index_precision",
          "index_dual_precision",
          "indexperf_1_20",
          "index_guard",
          "index_protection")

n = length(final_rates$index_precision)
ggplot(final_rates) +
    xlab("Percentile") +
    ylab("% Cumulative Return After 6 Years") +
    ggtitle("1-Year Term Strategy Return Percentiles") +
    theme(plot.title = element_text(face = "bold", size = 18)) +
    #xlim(0, 10) +
    #ylim(-70, 50) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_20) * 100,
                 col = "Index Performance, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_30) * 100,
                  col = "Index Performance, 30% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_precision) * 100,
                  col = "Index Precision")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_dual_precision) * 100,
                 col = "Index Dual Precision")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_guard) * 100,
                 col = "Index Guard")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_protection) * 100,
                 col = "Index Protection")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = 35.55))

ggsave("output/percentiles_multiple.png", dpi=480)

ggplot(final_rates) +
    xlab("Percentile") +
    ylab("% Cumulative Return After 6 Years") +
    ggtitle("1-Year Term Strategy Return Percentiles") +
    theme(plot.title = element_text(face = "bold", size = 18)) +
    xlim(0, 10) +
    ylim(-70, 50) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_20) * 100,
                 col = "Index Performance, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_30) * 100,
                  col = "Index Performance, 30% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_precision) * 100,
                  col = "Index Precision")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_dual_precision) * 100,
                 col = "Index Dual Precision")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_guard) * 100,
                 col = "Index Guard")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(index_protection) * 100,
                 col = "Index Protection")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = 35.55))

ggsave("output/percentiles_multiplebottom10.png", dpi=480)

ggplot(final_rates) +
    xlab("Percentile") +
    ylab("% Cumulative Return After 6 Years") +
    ggtitle("Index Performance Series Return Percentiles") +
    #xlim(10, 90) +
    #ylim(-40, 300) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_6_10) * 100,
                  col = "6-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_6_20) * 100,
                  col = "6-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_3_10) * 100,
                  col = "3-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_3_20) * 100,
                  col = "3-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_10) * 100,
                  col = "1-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_20) * 100,
                  col = "1-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_30) * 100,
                  col = "1-Year Term, 30% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = 35.55)) +
    theme(plot.title = element_text(face = "bold", size = 18))

ggsave("output/percentiles_indexperf.png", dpi=480)

ggplot(final_rates) +
    xlab("Percentile") +
    ylab("% Cumulative Return After 6 Years") +
    ggtitle("Index Performance Series Return Percentiles") +
    xlim(0, 10) +
    ylim(-75, 50) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_6_10) * 100,
                  col = "6-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_6_20) * 100,
                  col = "6-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_3_10) * 100,
                  col = "3-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_3_20) * 100,
                  col = "3-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_10) * 100,
                  col = "1-Year Term, 10% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_20) * 100,
                  col = "1-Year Term, 20% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = sort(indexperf_1_30) * 100,
                  col = "1-Year Term, 30% Buffer")) +
    geom_line(aes(x = (1:n - 1)/(n - 1) * 100,
                  y = 35.55)) +
    theme(plot.title = element_text(face = "bold", size = 18))

ggsave("output/percentiles_indexperfbottom10.png", dpi=480)
