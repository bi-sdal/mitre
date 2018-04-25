library(stringr)
library(jsonlite)

ems_2013 <- list.files(path = '/home/sdal/projects/arl/arlington911/data/working/ems/EMSCharts/2013/Usable/', pattern = '*.txt', full.names = TRUE)

tmp = fromJSON("./data/arlington911/data/working/ems/EMSCharts/2013/fieldDump2013.json")

ems_txt <- lapply(X = ems_2013[1:200], FUN = function(x){paste(readLines(x), collapse = ' ')})
ems_txt_v <- unlist(ems_txt)

# work with a subset for now

emsSub = ems_txt_v

# Do some cleaning

emsSub = lapply(emsSub, function(x) gsub("\xa0", "", x))


assault_terms <- c('assault', 'battery', 'domestic', 'violence', 'abuse')
substance_terms <- c('drunk', 'drunken', 'intoxi', 'drug', 'substance', 'disorderly', 'alcohol')
peep_terms <- c('child', 'elder', 'spouse', 'wife', 'husband')

all_terms <- c(assault_terms, substance_terms, peep_terms)

reg_all <- sprintf('.{0,50}%s.{0,50}', all_terms)

reg_assault <- sprintf('.{0,50}%s.{0,50}', assault_terms)
reg_substance <- sprintf('.{0,50}%s.{0,50}', substance_terms)
reg_peep <- sprintf('.{0,50}%s.{0,50}', peep_terms)

extractMatchWithContext <- lapply(emsSub, FUN = str_extract_all, pattern = reg_all)


assault_extracted <- lapply(emsSub, FUN = str_extract_all, pattern = reg_assault)
  #str_extract_all(ems_txt_v, reg_assault)
names(assault_extracted) <- assault_terms
assault_extracted

substance_extracted <- str_extract_all(ems_txt_v, reg_substance)
#names(substance_extracted) <- substance_terms
#substance_extracted

peep_extracted <- str_extract_all(ems_txt_v, reg_peep)
#names(peep_extracted) <- peep_terms
#peep_extracted

# Extract GPS coords

dispGpsRegex = "(?<=Disp\\.GPS:\\s{1,3})(-*\\d{2}\\.\\d{4,6},*-*\\d{2}\\.\\d{4,6})"
refGpsRegex = "(?<=Ref\\.\\s{1,3}GPS:\\s{1,3})(-*\\d{2}\\.\\d{4,6},*-*\\d{2}\\.\\d{4,6})"
destGpsRegex = "(?<=Dest\\.\\s{1,3}GPS:\\s{1,3})(-*\\d{2}\\.\\d{4,6},*-*\\d{2}\\.\\d{4,6})"
str_extract(emsSub, dispGpsRegex)
str_extract(emsSub, refGpsRegex)
str_extract(emsSub, destGpsRegex)

dateRegex = "(?<=Date:\\s{1,3})([a-zA-Z]{3,9}\\s*\\d{1,2},*\\s*\\d{4})"
str_extract(emsSub, dateRegex)

timeReceivedRegex = "(?<=Received:)(\\d{2}:\\d{2}:\\d{2})"
str_extract(emsSub, timeReceivedRegex)

# Experimental regex

serviceLevelRegex = "(?<=Service Level:)(.*?)(?=\\s{5})"
str_extract(emsSub, serviceLevelRegex)

complaintRegex = "(?<=Category:).*?(?=\\){1,2})"
str_extract(emsSub, complaintRegex)

# Find patterns for 'supercategories', determine which records have those, assign blocks of text between supercategories to the leading category