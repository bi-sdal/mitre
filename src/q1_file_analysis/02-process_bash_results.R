library(readr)
library(stringr)
#library(data.table)
library(dplyr)
library(ggplot2)

# df <- read_table('data/mitre/working/q1_files_and_sizes/files_and_sizes.txt', col_names = c('size', 'filepath'))
# df <- na.omit(df)

# using read_lines becuase read_table was causing issues
flines <- read_lines('data/mitre/working/q1_files_and_sizes/files_and_sizes.txt')
ldf <- as.data.frame(flines, stringsAsFactors = FALSE)
ldf <- na.omit(ldf)
splits <- as.data.frame(str_split_fixed(str_trim(ldf$flines), pattern = ' ', n = 2), stringsAsFactors = FALSE)

head(splits)

ldf <- cbind(ldf, splits)
head(ldf)

names(ldf) <- c('original', 'size', 'filepath')
head(ldf)

ldf$size <- as.numeric(ldf$size)
head(ldf)

s <- dim(ldf)
ldf <- ldf[!str_detect(ldf$filepath, '\\.Rproj\\.user'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.git'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Aa][Tt][Aa]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Ss]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Aa]'), ]
ldf <- ldf[!str_detect(ldf$filepath, 'total'), ]
ldf <- ldf[ldf$size > 0, ]
e <- dim(ldf)

# number of rows dropped
print(s - e)

# file_path_group <- function(filepath) {
#   if (str_detect(filepath, pattern = '^/home/sdal/projects/arl/arlington911/data/original/corelogic/')) {
#     return("corelogic")
#   } else if (str_detect(filepath, pattern = '^/home/sdal/projects/arl/arlington911/data/original/fire/031716')) {
#     return("fire incident")
#   } else if (str_detect(filepath, pattern = '^/home/sdal/projects/arl/arlington911/data/original/fire/Operation_FireSafe')) {
#     return("operation firesafe")
#   } else {
#     return("uncategorized")
#   }
# }

# ldf <- head(ldf, 5000)

## add group to files

ldf$group <- NA

ldf[str_detect(ldf$filepath, 'corelogic'), 'group'] <- 'corelogic'
ldf[str_detect(ldf$filepath, 'housing'), 'group'] <- 'housing'

ldf[str_detect(ldf$filepath, 'fire'), 'group'] <- 'fire'

ldf[str_detect(ldf$filepath, 'gis'), 'group'] <- 'gis'

ldf[str_detect(ldf$filepath, 'police'), 'group'] <- 'police'

ldf[str_detect(ldf$filepath, 'ems'), 'group'] <- 'ems'

ldf[str_detect(ldf$filepath, 'cad'), 'group'] <- 'cad'

ldf[str_detect(ldf$filepath, 'eoc'), 'group'] <- 'eoc'

ldf[str_detect(ldf$filepath, 'arlington_alerts'), 'group'] <- 'arlington_alerts'

ldf[str_detect(ldf$filepath, 'arlington_dhs'), 'group'] <- 'arlington_dhs'
ldf[str_detect(ldf$filepath, 'dhs_link_char'), 'group'] <- 'arlington_dhs'

ldf[str_detect(ldf$filepath, 'arlington_qecw'), 'group'] <- 'arlington_qecw'

ldf[str_detect(ldf$filepath, 'comm_arlington'), 'group'] <- 'comm_arlington'

ldf[str_detect(ldf$filepath, 'synth_pop'), 'group'] <- 'synth_pop'

ldf[str_detect(ldf$filepath, 'response_time'), 'group'] <- 'response_time'


ldf[str_detect(ldf$filepath, 'data_dict'), 'group'] <- 'data_dict'

table(ldf$group, useNA = 'always')

## add original working final status

ldf$owf <- NA

ldf[str_detect(ldf$filepath, 'original'), 'owf'] <- 'original'
ldf[str_detect(ldf$filepath, 'working'), 'owf'] <- 'working'
ldf[str_detect(ldf$filepath, 'final'), 'owf'] <- 'final'

table(ldf$owf)

addmargins(table(ldf$group, ldf$owf))


## Get file extension

ldf$fext <- sapply(X = ldf$filepath, tools::file_ext)

table(ldf$fext)

addmargins(table(ldf$fext, ldf$owf))


missing <- ldf[is.na(ldf$group), ]

addmargins(sort(table(missing$fext)))


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

sum(ldf$size) / 1e+9 # number of gb of data

max(ldf$size) / 1e+9
