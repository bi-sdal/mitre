library(readr)
library(stringr)

# Loading data ----

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

# Dropping invalid files and directories ----

s <- dim(ldf)
ldf <- ldf[!str_detect(ldf$filepath, '\\.Rproj\\.user'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.git'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Aa][Tt][Aa]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Ss]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Aa]'), ]
ldf <- ldf[!str_detect(ldf$filepath, 'total'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Jj][Pp][Ee]?[Gg]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Pp][Nn][Gg]'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr]$'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Xx]$'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Dd][Bb]$'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr][Mm][Dd]$'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Hh][Tt][Mm][Ll]$'), ]
ldf <- ldf[!str_detect(ldf$filepath, '\\.[Rr]history$'), ]
ldf <- ldf[ldf$size > 0, ]
e <- dim(ldf)

# number of rows dropped
print(s - e)

# Add project group info ----

## add group to files

ldf$group <- NA

ldf[str_detect(ldf$filepath, 'corelogic'), 'group'] <- 'Corelogic Housing Data'
ldf[str_detect(ldf$filepath, 'housing'), 'group'] <- 'Arlington Housing Data'

ldf[str_detect(ldf$filepath, 'fire'), 'group'] <- 'Fire'

ldf[str_detect(ldf$filepath, 'gis'), 'group'] <- 'Shapefiles'

ldf[str_detect(ldf$filepath, 'police'), 'group'] <- 'Police'

ldf[str_detect(ldf$filepath, 'ems'), 'group'] <- 'Emergency Medical Services'

ldf[str_detect(ldf$filepath, 'cad'), 'group'] <- 'Computer Aided Dispatch'

ldf[str_detect(ldf$filepath, 'eoc'), 'group'] <- 'Emergency Operations Center'

ldf[str_detect(ldf$filepath, 'arlington_alerts'), 'group'] <- 'Arlington Alerts User Activity'

ldf[str_detect(ldf$filepath, 'arlington_dhs'), 'group'] <- 'Department of Human Services'
ldf[str_detect(ldf$filepath, 'dhs_link_char'), 'group'] <- 'Department of Human Services'

ldf[str_detect(ldf$filepath, 'arlington_qecw'), 'group'] <- 'County Building Water Useage'

# commm_arlington data is all from public data
# ldf[str_detect(ldf$filepath, 'comm_arlington'), 'group'] <- 'comm_arlington'

ldf[str_detect(ldf$filepath, 'synth_pop'), 'group'] <- 'Corelogic Synthetic Data'

ldf[str_detect(ldf$filepath, 'response_time'), 'group'] <- 'Fire Response Time'

ldf[str_detect(ldf$filepath, 'data_dict'), 'group'] <- 'Data Dictionaries'

table(ldf$group, useNA = 'always')

## add original working final status

ldf$owf <- NA

ldf[str_detect(ldf$filepath, 'original'), 'owf'] <- 'original'
ldf[str_detect(ldf$filepath, 'working'), 'owf'] <- 'working'
ldf[str_detect(ldf$filepath, 'final'), 'owf'] <- 'final'

table(ldf$owf)

addmargins(table(ldf$group, ldf$owf, useNA = 'always'))

# File extensions ----

## Get file extension

ldf$fext <- sapply(X = ldf$filepath, tools::file_ext)

table(ldf$fext)

addmargins(table(ldf$fext, ldf$owf, useNA = 'always'))


missing <- ldf[is.na(ldf$group), ]

addmargins(sort(table(missing$fext, useNA = 'always')))

# Save out the file ----

saveRDS(ldf, file = 'data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS')
