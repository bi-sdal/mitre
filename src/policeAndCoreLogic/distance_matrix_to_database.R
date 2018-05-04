library(data.table)

dt <- fread(input = 'data/mitre/working/PoliceData/distMatrix.csv')

dt_long <- melt.data.table(dt,
                           id.vars = 'V1',
                           variable.name = 'house', value.name = 'distance')
setnames(dt_long, 'V1', 'police')


con <- sdalr::con_db('mitre')
DBI::dbWriteTable(conn = con, name = 'police_distances_long', value = dt_long,
                  row.names = FALSE, overwrite = TRUE)
