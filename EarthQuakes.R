library(ggmap)
library(dplyr)

# home page: http://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
# data: http://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt
# codebook: http://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225

df.raw <- read.csv(
  "signif.txt", 
  sep="\t")

df <- df.raw[,c("YEAR","EQ_PRIMARY","LATITUDE","LONGITUDE", "DEATHS")]

df <- df[complete.cases(df),]

set.seed(2)

km <- kmeans(df[,3:4], 3)

df$cluster <- km$cluster

df$x <- sapply(df$cluster, function(i) km$centers[i,2])
df$y <- sapply(df$cluster, function(i) km$centers[i,1])


df$ERA = ifelse(df$YEAR > 0, "new age", "old age")
df2 <- df %>% 
#  filter(YEAR > 2010) %>%
  group_by (x,y, cluster) %>% summarize(value=mean(EQ_PRIMARY)) 


world <- map_data("world")

gg <- ggplot(world, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="lightgrey") + 
  geom_point(
    data=df, 
    aes(
      x=LONGITUDE, 
      y=LATITUDE,
      color= DEATHS,
      group=1
    ),
    size = 1
  ) +
  geom_point(
    data=df2, 
    aes(
      x=x, 
      y=y, 
      group=value
    ),
    color="black",
    size = 4,
    shape=12
  )+
  scale_color_gradient(low="#FF9999", high="#FF0000") 


print(gg)





