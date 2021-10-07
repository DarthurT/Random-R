library(MatchIt)
library(data.table)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


old.data <- data.frame(Group = c(1,1,1,2,2,2), value = c(1,2,2,4,4,5),measure = c(10,10,11,15,16,16))
current.data <- data.frame(Group = c(3,3,3), value = c(4,4,4), measure = c(16,16,16))

old.data$case <- 0
current.data$case <- 1

df <- rbind(old.data,current.data)

df$value <- as.factor(df$value)

dt <- setDT(df)
dt <- dt[,.(Mode(value),Mode(measure),max(case)), by = Group]
colnames(dt) <- colnames(df)
df <- setDF(dt)
mod_match <- matchit(case ~ value +  measure, method = "nearest", data = df)
matches <- mod_match$match.matrix
df_match <- match.data(mod_match)