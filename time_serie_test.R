theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))


hour.df <- as.data.table(hour.df)
hour.ts <- ts(hour.df$Load_Mw[train],#[,!colnames(hour.df) %in% "Date.s"],
                 start= c(2003,1,1),#decimal_date(ymd_hms("2003-01-01 01:00:00")), #hour.I.df[1,'Date.s']
                 end = c(2016,12,31),#decimal_date(ymd_hms("2016-12-31 23:00:00")), # hour.I.df[nrow(hour.I.df),'Date.s']),
                 frequency=(24*30)*12)

decomp_ts <- stl(hour.ts, s.window = "periodic", robust = TRUE)$time.series


decomp_stl <- data.table(Load = c(hour.df$Load_Mw[train], as.numeric(decomp_ts)),
                         Date = rep(hour.df[train,Date.s], ncol(decomp_ts)+1),
                         Type = factor(rep(c("original data", colnames(decomp_ts)),
                                           each = nrow(decomp_ts)),
                                       levels = c("original data", colnames(decomp_ts))))


ggplot(decomp_stl, aes(x = Date, y = Load)) +
  geom_line() + 
  facet_grid(Type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Time Series Decomposition by STL") +
  theme_ts

