library(data.table)

dt <- fread(input = 'data/mitre/working/PoliceData/distMatrix.csv')

dt_long <- melt.data.table(dt,
                           id.vars = 'V1',
                           variable.name = 'house', value.name = 'distance')
setnames(dt_long, 'V1', 'police')


con <- sdalr::con_db('mitre')
DBI::dbWriteTable(conn = con, name = 'ca_ipv_ea_events', value = dt_long,
                  row.names = FALSE, overwrite = TRUE)

# make data science go vroom vroom
DBI::dbSendQuery(con, 'CREATE INDEX police_ca_ipv_ea ON ca_ipv_ea_events (police);')
DBI::dbSendQuery(con, 'CREATE INDEX house_ca_ipv_ea ON ca_ipv_ea_events (house);')

# probably don't need to index on both at the same time,
# takes really long, and you're pretty much indexing the entire table
#DBI::dbSendQuery(con, 'CREATE INDEX police_house ON police_distances_long (police, house);')
