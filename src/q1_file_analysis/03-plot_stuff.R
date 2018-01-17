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

pdf("./output/projectsize.pdf", width = 13.33, height = 7.5)
ggplot(data = na.omit(ct_size), aes(x = group, y = total_size/(2^20), fill = total_size/(2^20))) +
  geom_bar(stat = 'identity') +
  #geom_hline(yintercept = 10e+9, show.legend = T) + # 10 gb
  #geom_hline(yintercept = 1e+9, show.legend = T) +  # 1 gb
  #geom_hline(yintercept = 5e+8) +  # 500 mb
  #geom_hline(yintercept = 1e+6, show.legend = T) +  # 1 mb
  #scale_y_log10() +
  coord_flip() +
  theme_minimal() + 
  labs(y = "Total Size in MB", 
       x = "Project Directory", 
       title = "Storage Requirements for Selected SDAL Projects") + 
  guides(fill = F) + 
  theme(text = element_text(size = 20))
dev.off()

# File extensions and size ----

## total number of bytes by file extension

ext_size <- ldf %>%
  group_by(fext) %>%
  summarize(
    count = n(),
    total_size = sum(size)
  ) %>%
  arrange(total_size)

ext_size$fext <- factor(ext_size$fext, levels = ext_size$fext[order(ext_size$total_size)])

# View(ext_size)

ggplot(data = ext_size, aes(x = fext, y = total_size)) +
  geom_bar(stat = 'identity') +
  #geom_hline(yintercept = 10e+9) + # 10 gb
  #geom_hline(yintercept = 1e+9) +  # 1 gb
  #geom_hline(yintercept = 5e+8) +  # 500 mb
  #geom_hline(yintercept = 1e+6) +  # 1 mb
  #scale_y_log10() +
  coord_flip() +
  theme_minimal()

ggplot(data = ext_size, aes(x = fext, y = total_size)) + geom_boxplot() + coord_flip()

# Overal decriptive calculations ----

sum(ldf$size) / 1e+9 # number of gb of data

max(ldf$size) / 1e+9
