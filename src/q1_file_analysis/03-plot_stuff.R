library(dplyr)
library(ggplot2)

ldf <- readRDS('data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS')

# File counts and size ----

## Some analysis things

## number of files by group and total number of bytes
ct_size <- ldf %>% 
  group_by(group) %>%
  summarize(
    count = n(),
    total_size = sum(size)) %>%
  arrange(total_size)

ct_size

ct_size$group <- factor(ct_size$group, levels = ct_size$group[order(ct_size$total_size)])


#ct_size$mb <- ct_size$total_size / 1048576

ct_size_plot <- na.omit(ct_size)

pdf("./output/projectsize.pdf", width = 13.33, height = 7.5)
ggplot(data = ct_size_plot, aes(x = group,
                                y = total_size/(2^20),
                                fill = total_size/(2^20))) +
  geom_bar(stat = 'identity') +
  annotate("text", x = ct_size_plot$group, y = 10000,
           label = paste0("(", ct_size_plot$count, ")"), hjust = 1, size = 5) +
  theme_minimal() + 
  labs(y = "Total Size in MB (Number of Files)", 
       x = "Project Directory") + 
  guides(fill = F) + 
  theme(text = element_text(size = 20)) +
  coord_flip()
dev.off()

# File extensions and size ----
