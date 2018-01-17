library(readr)
library(stringr)
#library(data.table)
library(dplyr)
library(ggplot2)

# df <- read_table('data/mitre/working/q1_files_and_sizes/files_and_sizes.txt', col_names = c('size', 'filepath'))
# df <- na.omit(df)

# using read_lines becuase read_table was causing issues
flines <- read_lines('data/mitre/working/q1_files_and_sizes/files_and_sizes.txt')
ldf <- as.data.frame(flines)
ldf <- na.omit(ldf)
splits <- str_split_fixed(str_trim(ldf$flines), pattern = ' ', n = 2)

ldf <- cbind(ldf, splits)

names(ldf) <- c('original', 'size', 'filepath')
ldf$size <- as.numeric(ldf$size)
head(ldf)

dim(ldf)
ldf <- ldf[!str_detect(ldf$filepath, '\\.Rproj\\.user'), ]
ldf <- ldf[!str_detect(ldf$filepath, 'total'), ]
ldf <- ldf[ldf$size > 1, ]
dim(ldf)

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


missing <- ldf[is.na(ldf$group),]


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

ggplot(data = ct_size, aes(x = group, y = total_size)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1e+9) +
  geom_hline(yintercept = 1e+6) +
  scale_y_log10() +
  coord_flip() +
  theme_minimal()


## total number of bytes by file extension

ext_size <- ldf %>%
  group_by(fext) %>%
  summarize(
    count = n(),
    total_size = sum(size)
  ) %>%
  arrange(total_size)

ext_size$fext <- factor(ext_size$fext, levels = ext_size$fext[order(ext_size$total_size)])

View(ext_size)

ggplot(data = ext_size, aes(x = fext, y = total_size)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 1e+9) +
  geom_hline(yintercept = 1e+6) +
  scale_y_log10() +
  coord_flip() +
  theme_minimal()



ggplot(data = ldf, aes(x = size)) + geom_histogram() +
  geom_vline(xintercept = 1e+9) +
  geom_vline(xintercept = 1e+6)


sum(ldf$size)
