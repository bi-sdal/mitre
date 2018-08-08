get_police_house_dist <- function(row_num,
                                  file = './data/mitre/working/PoliceData/distMatrix.csv',
                                  out = './data/mitre/working/PoliceData/temp_police_lookup.csv') {
  cmd <- sprintf('bash src/functions/police_distance_nrow.sh %s %s %s', row_num, file, out)
  print(cmd)
  system(cmd)
  read.csv(out)
}

df <- get_police_house_dist(100)
