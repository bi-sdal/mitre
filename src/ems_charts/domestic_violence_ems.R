library(stringr)

ems_2013 <- list.files(path = '/home/sdal/projects/arl/arlington911/data/working/ems/EMSCharts/2013/Usable/', pattern = '*.txt', full.names = TRUE)

ems_txt <- lapply(X = ems_2013, FUN = function(x){paste(readLines(x), collapse = ' ')})
ems_txt_v <- unlist(ems_txt)

# txt <- readLines('/home/sdal/projects/arl/arlington911/data/working/ems/EMSCharts/2013/Usable/newfile22286813.txt')
# 
# txt
# 
# txt_lower <- str_to_lower(txt)
# 
# txt1l <- paste(txt_lower, collapse = ' ')
# 
# txt1l

assault_terms <- c('assault', 'battery', 'domestic violence', 'domestic', 'violence', 'abuse')
substance_terms <- c('drunk', 'drunken', 'intoxi', 'drug', 'substance', 'disorderly', 'alcohol')
peep_terms <- c('child', 'elder', 'spouse', 'wife', 'husband')

all_terms <- c(assault_terms, substance_terms, peep_terms)

reg_all <- sprintf('.{0,50}%s.{0,50}', all_terms)

# reg_assault <- sprintf('.{0,50}%s.{0,50}', assault_terms)
# reg_substance <- sprintf('.{0,50}%s.{0,50}', substance_terms)
# reg_peep <- sprintf('.{0,50}%s.{0,50}', peep_terms)

assault_extracted <- lapply(ems_txt_v, FUN = str_extract_all, pattern = reg_assault)
  #str_extract_all(ems_txt_v, reg_assault)
names(assault_extracted) <- assault_terms
#assault_extracted

substance_extracted <- str_extract_all(ems_txt_v, reg_substance)
#names(substance_extracted) <- substance_terms
#substance_extracted

peep_extracted <- str_extract_all(ems_txt_v, reg_peep)
#names(peep_extracted) <- peep_terms
#peep_extracted

m <- lapply(ems_txt_v[1:10], FUN = str_extract_all, pattern = all_terms)
m
