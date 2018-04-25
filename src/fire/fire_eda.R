# fire <- read.csv('./data/arlington911/data/original/fire/merged_incidents.csv', stringsAsFactors = FALSE)

library(data.table)

get_series_from_type <- function(incident_type) {
  if (is.na(incident_type)) {
    return(NA)
  } else if (nchar(incident_type) == 3) {
    return(as.numeric(paste0(substr(incident_type, 1, 1), '00')))
  } else {
    return(NA)
  }
}

fire_dt <- fread('./data/arlington911/data/original/fire/merged_incidents.csv')
fire_series_codes <- fread('./data/arlington911/data/original/fire/fire_incidentID_series_heading.csv')

head(fire_dt)

fire_dt$series <- sapply(fire_dt$incidenttype, get_series_from_type)

df <- merge(fire_dt, fire_series_codes, by.x = 'series', by.y = 'SERIES', all.x = TRUE)
# has geolocation
5


#311
#Medical assist. Includes incidents where medical assistance is provided to another group/agency that has pri-		
#  mary EMS responsibility. (Example, providing assistance to another agency-assisting EMS with moving a heavy
#                            patient.)


#320
#Emergency medical service incident, other.

#510
#Person in distress, other.


# 552
#Police matter. Includes incidents where FD is called to a scene that should be handled by the police.

# 661
# EMS call where injured party has been transported by a non-fire service agency or left the scene prior to arrival.

# 911 Citizen’s complaint. Includes reports of code or ordinance violation.


df_incidents <- df[df$incidenttype %in% c("311", "320", "510", "552", "661", "911"), ]

df_incidents$year <- year(as.Date(df_incidents$incidentdate.y))

addmargins(table(df_incidents$incidenttype, df_incidents$year))

library(lubridate)
unique(year(as.Date(unique(fire_dt$incidentdate.y))))
#2010 to 2016


as.data.frame(table(fire_dt$incidenttype, useNA = 'always'))



# plot things on a map


library(ggplot2)
library(gganimate)
library(ggmap)

map_arl <- get_map(c(lon = -77.100703, lat = 38.878337), zoom = 12)

codes <- c(311, 320, 510, 552, 661, 911)

ggmap(map_arl) +
  geom_point(
    data = fire_dt[fire_dt$incidenttype %in% codes & year(as.Date(fire_dt$incidentdate.y)) == 2016, ],
    aes(x = longitude, y = latitude, color = incidenttype))
