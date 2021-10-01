start_dates = seq( as.Date("2019-09-28"), to = as.Date("2020-09-28"), by = 'day' )[ seq(from = 1, by = 7, length.out = 52) ]
end_dates = seq( as.Date("2019-09-28"), to = as.Date("2020-09-28"), by = 'day' )[ seq(from = 1, by = 7, length.out = 52) ] + 6

dates_mat = noquote(cbind(as.character(start_dates), as.character(end_dates)))
write.table(dates_mat, file="start_end_dates.txt", quote = F, row.names=FALSE, col.names=FALSE)
