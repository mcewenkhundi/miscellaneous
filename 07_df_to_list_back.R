#How to convert a dataframe to a list of rows and then convert the
#list back to a data frame

df = data.frame(x=c('a','b','c'), y=3:1)

# 'convert' into a list of data.frames
ldf = lapply(as.list(1:nrow(df)), function(x) df[x,])

map(1:nrow(df), function(x) df[x,])

#do the below with a map function
df %>% group_split(x) 

df_remade <- do.call("rbind", ldf)
             bind_rows(ldf)

## using map with more than two lists

list1 <- as.list(rep(1, 50))
list2 <- as.list(rep(1, 50))

map2(list1, list2, sum)
mapply(sum, X=list1, Y=list2,  SIMPLIFY = FALSE)
