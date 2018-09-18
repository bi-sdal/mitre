library(data.table)

domeZipCodes = fread("./data/mitre/working/policeDataZipCodes.csv")

cpsByZipCode = fread("./data/mitre/original/caByZip.csv")
colnames(cpsByZipCode) = c('zip', 'count')

domeZipTab = na.omit(domeZipCodes[,.N, by = 'x'])

twoWay = merge(cpsByZipCode, domeZipTab, by.x = 'zip', by.y = 'x')
colnames(twoWay) = c("zip", "cpsCount", "domeCount")

. = fisher.test(as.matrix(twoWay[,.(cpsCount, domeCount)]), simulate.p.value = TRUE, B = 50000)
.

tmp = melt(twoWay, id.vars = 'zip')

ggplot(data = tmp, aes(x = zip, fill = variable)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), position = 'dodge')

