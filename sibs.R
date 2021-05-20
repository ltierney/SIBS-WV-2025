## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(collapse=TRUE)


## ---- message = FALSE---------------------------------------------------------
library(tidyverse)


## ---- include = FALSE---------------------------------------------------------
library(nomnoml)


## #padding: 25

## #fontsize: 18

## #fill: #E1DAFF; #D4A9FF

## #stroke: #8515C7

## #linewidth: 2

## 
## [Import] -> [Understand]

## [Understand |

##   [Wrangle] -> [Visualize]

##   [Visualize] -> [Model]

##   [Model] -> [Wrangle]

## ]

## [Understand] -> [Communicate]


## -----------------------------------------------------------------------------
data(geyser, package = "MASS")
dim(geyser)
head(geyser)


## -----------------------------------------------------------------------------
ggplot(geyser) +
    geom_histogram(aes(x = duration), bins = 15, color = "black", fill = "grey")


## -----------------------------------------------------------------------------
ggplot(geyser) + geom_point(aes(x = lag(duration), y = waiting))


## -----------------------------------------------------------------------------
p <- ggplot(geyser) +
    geom_histogram(aes(x = duration, y = stat(density)),
                   fill = "grey", color = "black", bins = 50)
p


## -----------------------------------------------------------------------------
d <- geyser$duration
d_short <- d[d < 3]
d_long <- d[d >= 3]
mean(d_short)
sd(d_short)
mean(d_long)
sd(d_long)
mean(d >= 3)


## -----------------------------------------------------------------------------
geyser <- mutate(geyser, type = ifelse(duration < 3, "short", "long"))


## -----------------------------------------------------------------------------
sgd <- summarize(group_by(geyser, type),
                 mean = mean(duration),
                 sd = sd(duration),
                 n = n())
(sgd <- mutate(sgd, prop = n / sum(n)))


## -----------------------------------------------------------------------------
p <- p +
    stat_function(color = "red",
                  fun = function(x)
                          sgd$prop[1] * dnorm(x, sgd$mean[1], sgd$sd[1])) +
    stat_function(color = "blue",
                  fun = function(x)
                          sgd$prop[2] * dnorm(x, sgd$mean[2], sgd$sd[2]))
p


## -----------------------------------------------------------------------------
geyser2 <- filter(geyser, duration != 2, duration != 4)
sgd2 <- summarize(group_by(geyser2, type),
                  mean = mean(duration),
                  sd = sd(duration),
                  n = n())
(sgd2 <- mutate(sgd2, prop = n / sum(n)))


## -----------------------------------------------------------------------------
p <- p +
    stat_function(color = "red",
                  linetype = 2,
                  fun = function(x)
                          sgd2$prop[1] * dnorm(x, sgd2$mean[1], sgd2$sd[1])) +
    stat_function(color = "blue",
                  linetype = 2,
                  fun = function(x)
                          sgd2$prop[2] * dnorm(x, sgd2$mean[2], sgd2$sd[2]))
p


## ---- eval = FALSE, echo = FALSE----------------------------------------------
## ## Fancier version that gets a color legend.
## ## Could also get a line type legend.
## p <- ggplot(geyser) +
##     geom_histogram(aes(x = duration, y = stat(density)),
##                    fill = "grey", color = "black", bins = 50)
## p <- p +
##     stat_function(aes(color = type),
##                   data = filter(sgd, type == "long"),
##                   fun = function(x)
##                           sgd$prop[1] * dnorm(x, sgd$mean[1], sgd$sd[1])) +
##     stat_function(aes(color = type),
##                   data = filter(sgd, type == "short"),
##                   fun = function(x)
##                           sgd$prop[2] * dnorm(x, sgd$mean[2], sgd$sd[2]))
## p
## 
## p <- p +
##      stat_function(aes(color = type),
##                   data = filter(sgd2, type == "long"),
##                   linetype = 2,
##                   fun = function(x)
##                           sgd2$prop[1] * dnorm(x, sgd2$mean[1], sgd2$sd[1])) +
##     stat_function(aes(color = type),
##                   data = filter(sgd2, type == "short"),
##                   linetype = 2,
##                   fun = function(x)
##                           sgd2$prop[2] * dnorm(x, sgd2$mean[2], sgd2$sd[2]))
## p


## -----------------------------------------------------------------------------
data(barley, package = "lattice")
head(barley)


## ---- fig.width = 10----------------------------------------------------------
p1 <- ggplot(barley) + geom_point(aes(x = yield, y = variety))
p2 <- ggplot(barley) + geom_point(aes(x = yield, y = site))
cowplot::plot_grid(p1, p2)


## ---- fig.width = 10----------------------------------------------------------
p1 <- ggplot(barley) + geom_point(aes(x = yield, y = variety, color = year))
p2 <- ggplot(barley) + geom_point(aes(x = yield, y = site, color = year))
cowplot::plot_grid(p1, p2)



## -----------------------------------------------------------------------------
ggplot(barley) +
    geom_point(aes(x = yield, y = variety, color = year, shape = site))


## -----------------------------------------------------------------------------
ggplot(barley) +
    geom_point(aes(x = yield, y = variety, color = year, shape = site),
               position = position_jitter(height = 0.15, width = 0),
               size = 2)


## ---- fig.width = 10----------------------------------------------------------
ggplot(barley) +
    geom_point(aes(x = yield, y = variety, color = year)) +
    facet_wrap(~site)


## ---- fig.width = 10----------------------------------------------------------
barley_site_year <- summarize(group_by(barley, site, year),
                              yield = mean(yield))
p1 <- ggplot(barley_site_year) +
    geom_point(aes(y = site, x = yield, color = year), size = 3)
p2 <- ggplot(barley_site_year) +
    geom_col(aes(x = site, y = yield, fill = year),
             size = 3,
             position = "dodge", width = .4) +
    coord_flip()
cowplot::plot_grid(p1, p2)


## -----------------------------------------------------------------------------
HairEyeDF <- as.data.frame(HairEyeColor)
head(HairEyeDF)


## -----------------------------------------------------------------------------
eye <- summarize(group_by(HairEyeDF, Eye), Freq = sum(Freq))
ggplot(eye) + geom_col(aes(x = Eye, y = Freq), position = "dodge")


## -----------------------------------------------------------------------------
ggplot(eye) + geom_col(aes(x = Eye, y = Freq, fill = Eye), position = "dodge")


## -----------------------------------------------------------------------------
hazel_rgb <- col2rgb("brown") * 0.75 + col2rgb("green") * 0.25
hazel <- do.call(rgb, as.list(hazel_rgb / 255))

cols <- c(Blue = colorspace::lighten(colorspace::desaturate("blue", 0.3), 0.3),
          Green = colorspace::lighten("forestgreen", 0.1),
          Brown = colorspace::lighten("brown", 0.0001), ## 0.3?
          Hazel = colorspace::lighten(hazel, 0.3))

pb <- ggplot(eye) +
    geom_col(aes(x = Eye, y = Freq, fill = Eye), position = "dodge") +
    scale_fill_manual(values = cols)
pb


## -----------------------------------------------------------------------------
psb <- ggplot(eye) +
    geom_col(aes(x = "", y = Freq, fill = Eye), color = "lightgrey") +
    scale_fill_manual(values = cols)
psb


## -----------------------------------------------------------------------------
(pp <- psb + coord_polar("y"))


## -----------------------------------------------------------------------------
(pp <- pp + theme_void())


## ---- fig.width = 10----------------------------------------------------------
cowplot::plot_grid(pb, pp)


## ---- fig.width = 10----------------------------------------------------------
eye_hairsex <- mutate(group_by(HairEyeDF, Hair, Sex), Prop = Freq / sum(Freq))
p1 <- ggplot(eye_hairsex) +
    geom_col(aes(x = Eye, y = Prop, fill = Eye)) +
    scale_fill_manual(values = cols) +
    facet_grid(Hair~Sex)
p2 <- ggplot(eye_hairsex) +
    geom_col(aes(x = "", y = Prop, fill = Eye)) +
    scale_fill_manual(values = cols) +
    coord_polar("y")+facet_grid(Hair~Sex) +
    theme_void()
cowplot::plot_grid(p1, p2)


## -----------------------------------------------------------------------------
river <- scan("data/river.dat")
rd <- data.frame(flow = river, month = seq_along(river))
(pp <- ggplot(rd) + geom_point(aes(x = month, y = flow)))


## ---- eval = FALSE------------------------------------------------------------
## (pl <- ggplot(rd) + geom_line(aes(x = month, y = flow)))


## ---- eval = FALSE------------------------------------------------------------
## pp + coord_fixed(3.5)


## ---- eval = FALSE------------------------------------------------------------
## pl + coord_fixed(3.5)


## -----------------------------------------------------------------------------
fname <- "data/Invasive-Cancer-Incidence-Rates-by-County-in-Iowa-Lung-and-Bronchus-2011.csv"
d <- read_csv(fname, skip = 2)
head(d)


## -----------------------------------------------------------------------------
d <- select(d, county = 1, population = 2, count = 3, crude_rate = 4)
tail(d)


## -----------------------------------------------------------------------------
d <- filter(d, ! is.na(population))
tail(d)


## -----------------------------------------------------------------------------
d <- mutate(d, count = as.numeric(count), crude_rate = as.numeric(crude_rate))


## -----------------------------------------------------------------------------
count(d, count == 0)
any(d$count == 0, na.rm = TRUE)


## -----------------------------------------------------------------------------
d <- replace_na(d, list(count = 0, crude_rate = 0))


## ---- message = FALSE---------------------------------------------------------
m <- map_data("county", "iowa")
head(m)
m <- select(m, -region)
m <- rename(m, county = subregion)
head(m)


## -----------------------------------------------------------------------------
d <- mutate(d, cname = county, county = tolower(county))

setdiff(d$county, m$county)
setdiff(m$county, d$county)

d <- mutate(d, county = sub("'", "", county))
d <- filter(d, county != "state")

setdiff(d$county, m$county)
setdiff(m$county, d$county)


## -----------------------------------------------------------------------------
d <- mutate(d, rate1K = 1000 * (count / population))
md <- left_join(m, d, "county")
head(md)


## -----------------------------------------------------------------------------
library(ggthemes)
library(viridis)
ggplot(md) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = rate1K))


## -----------------------------------------------------------------------------
library(ggthemes)
library(viridis)
ggplot(md) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill = rate1K),
                 color = "grey") +
    scale_fill_viridis(name = "Rate per 1000") +
    theme_map() + coord_quickmap()


## -----------------------------------------------------------------------------
mdl <- mutate(md,
              label = paste(cname, round(rate1K, 1), population, sep = "\n"))
p <- ggplot(mdl) +
    geom_polygon(aes(x = long, y = lat, fill = rate1K, group = group,
                     text = label), 
                 color = "grey") +
    scale_fill_viridis(name = "Rate per 1000") +
    theme_map() + coord_quickmap()

plotly::ggplotly(p, tooltip = "text")


## -----------------------------------------------------------------------------
ggplot(d, aes(map_id = county, fill = count/population)) +
    geom_map(map = rename(m, id = county) , color = "grey") +
    with(m, expand_limits(x = long, y = lat)) +
    scale_fill_viridis() +
    theme_map() + coord_quickmap()


## -----------------------------------------------------------------------------
lausURL <- "data/laucntycur14-2017.txt"
lausUS <- read.table(lausURL,
                     col.names = c("LAUSAreaCode", "State", "County",
                                   "Title", "Period",
                                   "LaborForce", "Employed",
                                   "Unemployed", "UnempRate"),
                     quote = '"', sep = "|", skip = 6,
                     stringsAsFactors = FALSE, strip.white = TRUE,
                     fill = TRUE)
footstart <- grep("------", lausUS$LAUSAreaCode)
lausUS <- lausUS[1:(footstart - 1),]


## -----------------------------------------------------------------------------
lausUS <- separate(lausUS, Title, c("cname", "scode"),
                   sep = ", ", fill = "right")


## -----------------------------------------------------------------------------
lausUS <- mutate(lausUS, UnempRate = as.numeric(UnempRate))


## -----------------------------------------------------------------------------
sapply(lausUS, function(x) any(is.na(x)))


## -----------------------------------------------------------------------------
unique(filter(select(lausUS, cname, scode), is.na(scode)))


## -----------------------------------------------------------------------------
unique(filter(select(lausUS, scode, Period, UnempRate), is.na(UnempRate)))


## -----------------------------------------------------------------------------
avgUS <- summarize(group_by(lausUS, County, State),
                   avg_unemp = mean(UnempRate),
                   cname = unique(cname),
                   scode = unique(scode))
head(avgUS)


## -----------------------------------------------------------------------------
avgUS <- mutate(avgUS, fips = 1000 * State + County)
head(avgUS)


## -----------------------------------------------------------------------------
library(maps)
head(county.fips)


## -----------------------------------------------------------------------------
filter(county.fips, grepl("florida,o", polyname))
head(select(filter(lausUS, scode == "LA"), cname))


## -----------------------------------------------------------------------------
county.fips <- separate(county.fips, polyname,
                        c("state", "county", "part"),
                        sep = "[,:]", fill = "right")
head(county.fips)


## -----------------------------------------------------------------------------
counties_US <- map_data("county")
counties_US <- rename(counties_US, state = region, county = subregion)
counties_US <- left_join(counties_US, county.fips, c("state", "county"))


## -----------------------------------------------------------------------------
ggplot(left_join(counties_US, avgUS, "fips")) +
    geom_polygon(aes(x = long, y = lat, fill = avg_unemp, group = group)) +
    scale_fill_viridis(name = "Rate", na.value = "red") +
    theme_map() + coord_map() + 
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = map_data("state"), col = "grey", fill = NA)


## -----------------------------------------------------------------------------
ggplot(avgUS, aes(fill = avg_unemp, map_id = fips)) +
    geom_map(map = mutate(counties_US, id = fips)) +
    with(counties_US, expand_limits(x = long, y = lat)) +
    scale_fill_viridis(name = "Rate", na.value = "red") +
    theme_map() + coord_map()


## -----------------------------------------------------------------------------
library(readxl)
gcm <- read_excel("data/gapminder-under5mortality.xlsx")
names(gcm)[1]
names(gcm)[1] <- "country"


## -----------------------------------------------------------------------------
tgcm <- gather(gcm, year, u5mort, -1)
head(tgcm)
tgcm <- mutate(tgcm, year = as.numeric(year))
head(tgcm)


## -----------------------------------------------------------------------------
library(lattice)
p <- ggplot(tgcm) + geom_line(aes(year, u5mort, group = country), alpha = 0.3)
p
plotly::ggplotly(p)


## -----------------------------------------------------------------------------
countries <- c("United States", "United Kingdom", "Germany", "China", "Egypt")
tcgm1 <- filter(tgcm, country %in% countries)
ggplot(tcgm1) + geom_line(aes(x = year, y = u5mort, color = country))


## -----------------------------------------------------------------------------
tgcm_miss <- summarize(group_by(tgcm, country), anyNA = any(is.na(u5mort)))
tgcm_miss <- filter(tgcm_miss, anyNA)$country
p <- ggplot(filter(tgcm, country %in% tgcm_miss)) +
    geom_line(aes(x = year, y = u5mort, group = country), na.rm = TRUE)
p
plotly::ggplotly(p)

