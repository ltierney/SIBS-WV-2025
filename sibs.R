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
wind_turbines <- read.csv("data/us_wind.csv", comment = "#")


## -----------------------------------------------------------------------------
wt_IA <- filter(wind_turbines, t_fips %/% 1000 == 19)


## -----------------------------------------------------------------------------
wt_IA <- filter(wt_IA, ! is.na(xlong), ! is.na(ylat))


## -----------------------------------------------------------------------------
wt_IA <- mutate(wt_IA, p_year = replace(p_year, p_year < 0, NA))


## ---- eval = FALSE------------------------------------------------------------
## iowa_sf <- sf::st_as_sf(maps::map("county", "iowa", plot = FALSE, fill = TRUE))


## ---- fig.width = 8-----------------------------------------------------------
iowa_sf <-
    sf::st_as_sf(maps::map("county", "iowa",
                           plot = FALSE,
                           fill = TRUE))
p <- ggplot() +
    geom_sf(data = iowa_sf) +
    ggthemes::theme_map()
p


## ---- fig.width = 8-----------------------------------------------------------
p + geom_point(aes(xlong, ylat), data = wt_IA)


## ---- fig.width = 8-----------------------------------------------------------
year_brk <-c(0, 2005, 2010, 2015, 2020)
year_lab <- c("before 2005",
              "2005-2009",
              "2010-2014",
              "2015-2020")
wt_IA <-
    mutate(wt_IA,
           year = cut(p_year,
                      breaks = year_brk,
                      labels = year_lab,
                      right = FALSE))
p + geom_point(aes(xlong,
                   ylat,
                   color = year),
               data = wt_IA,
               size = 3)


## ----eval = FALSE, echo = FALSE-----------------------------------------------
## library(tidyverse)
## p <- ggplot() + geom_sf(data = iowa_sf) + ggthemes::theme_map()
## p + geom_point(aes(xlong, ylat), data = wt_IA)
## 
## wt_IA_sf <- sf::st_as_sf(wt_IA, coords = c("xlong", "ylat"), crs = 4326)
## 
## p + geom_sf(data = filter(wt_IA_sf, year <= 2020))
## 
## library(gganimate)
## pa <- p + geom_sf(data = wt_IA_sf) +
##     transition_manual(year, cumulative = TRUE) +
##     labs(title = "Wind turbines in Iowa",
##          subtitle = "Year = {current_frame}")
## anim_save("foo.gif", animate(pa, fps = 10, nframes = 100))


## ---- message = FALSE---------------------------------------------------------
fname <- "data/Invasive-Cancer-Incidence-Rates-by-County-in-Iowa-Lung-and-Bronchus-2011.csv"
d <- read_csv(fname, skip = 2)
head(d)


## -----------------------------------------------------------------------------
d <- select(d, county = 1, population = 2, count = 3, crude_rate = 4)


## -----------------------------------------------------------------------------
tail(d)


## -----------------------------------------------------------------------------
d <- filter(d, ! is.na(population))
d <- filter(d, county != "STATE")
tail(d)


## -----------------------------------------------------------------------------
d <- mutate(d, count = as.numeric(count), crude_rate = as.numeric(crude_rate))


## -----------------------------------------------------------------------------
count(d, count == 0)
any(d$count == 0, na.rm = TRUE)


## -----------------------------------------------------------------------------
d <- replace_na(d, list(count = 0, crude_rate = 0))


## -----------------------------------------------------------------------------
d$county[1]
iowa_sf$ID[1]

d <- mutate(d, cname = county, county = tolower(county))
iowa_sf <- mutate(iowa_sf, county = sub("iowa,", "", ID))

setdiff(d$county, iowa_sf$county)
setdiff(iowa_sf$county, d$county)

d <- mutate(d, county = sub("'", "", county))

setdiff(d$county, iowa_sf$county)
setdiff(iowa_sf$county, d$county)


## -----------------------------------------------------------------------------
d <- mutate(d, rate1K = 1000 * (count / population))
md <- left_join(iowa_sf, d, "county")
head(md)


## -----------------------------------------------------------------------------
library(ggthemes)
library(viridis)
ggplot(md) + geom_sf(aes(fill = rate1K))


## -----------------------------------------------------------------------------
library(ggthemes)
library(viridis)
ggplot(md) +
    geom_sf(aes(fill = rate1K),
            color = "grey") +
    scale_fill_viridis(name = "Rate per 1000") +
    theme_map()


## -----------------------------------------------------------------------------
mdl <- mutate(md,
              label = paste(cname, round(rate1K, 1), population, sep = "\n"))
p <- ggplot(mdl) +
    geom_sf(aes(fill = rate1K,
                text = label), 
            color = "grey") +
    scale_fill_viridis(name = "Rate per 1000") +
    theme_map()

plotly::ggplotly(p, tooltip = "text")


## -----------------------------------------------------------------------------
library(leaflet)
pal <- colorNumeric(
    palette = "viridis",
    domain = md$rate1K)
lab <- lapply(paste0(tools::toTitleCase(md$county), "<BR>",
                     "Rate: ", round(md$rate1K, 1), "<BR>",
                     "Pop: ", scales::comma(md$population,
                                            accuracy = 1)),
              htmltools::HTML)
leaflet(sf::st_transform(md, 4326)) %>%
    addPolygons(weight = 2,
                color = "grey",
                fillColor = ~ pal(rate1K),
                fillOpacity = 1,
                highlightOptions =
                    highlightOptions(color = "white",
                                     weight = 2,
                                     bringToFront = TRUE),
                label = lab) %>%
    addLegend(pal = pal, values = ~ rate1K)


## -----------------------------------------------------------------------------
lausURL <- "data/laucntycur14-2020.txt"
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
ggpoly2sf <- function(poly, coords = c("long", "lat"),
                      id = "group", region = "region", crs = 4326) {
    sf::st_as_sf(poly, coords = coords, crs = crs) %>%
    group_by(!! as.name(id), !! as.name(region)) %>%
    summarize(do_union = FALSE) %>%
    sf::st_cast("POLYGON") %>%
    ungroup() %>%
    group_by(!! as.name(region)) %>%
    summarize(do_union = FALSE) %>%
    ungroup()
}
m_sf <- ggpoly2sf(socviz::county_map, c("long", "lat"), "group", "id")
m_sf <- mutate(m_sf, fips = as.numeric(id))
m_sf <- mutate(m_sf, fips = replace(fips, fips == 46113, 46102))
ggplot(m_sf) + geom_sf()
lausUS <- mutate(lausUS, fips = State * 1000 + County)
au <- group_by(lausUS, fips) %>% summarize(avg_ur = mean(UnempRate, na.rm = TRUE))
mu <- group_by(lausUS, fips) %>% summarize(max_ur = max(UnempRate, na.rm = TRUE))
da <- left_join(m_sf, au, "fips")
dm <- left_join(m_sf, mu, "fips")
ggplot(da, aes(fill = avg_ur)) + geom_sf(size = 0.1) + scale_fill_viridis(name = "Rate", na.value = "red")
d <- left_join(m_sf, lausUS, "fips")



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

