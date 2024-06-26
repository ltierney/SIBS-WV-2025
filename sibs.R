## ----setup, include = FALSE---------------------------------------------------
if ("xaringan" %in% loadedNamespaces()) {
    options(htmltools.dir.version = FALSE)
    knitr::opts_chunk$set(fig.height = 5, fig.width = 6)
    xaringanExtra::use_tile_view()
    xaringanExtra::use_clipboard()
    xaringanExtra::use_search(show_icon = TRUE)
}
knitr::opts_chunk$set(collapse = TRUE, warning = FALSE)
library(ggplot2)
theme_set(theme_minimal() +
          theme(text = element_text(size = 16)) +
          theme(panel.border = element_rect(color = "grey30", fill = NA)))
here_rel <- function(path)
    if (file.exists(path)) path else file.path("..", path)


## .content-box-blue { background-color: lightblue; }
## .small-font { font-size: 70%; }
## .width-20 { width: 20% }
## .width-30 { width: 30% }
## .width-60 { width: 60% }
## .width-70 { width: 70% }
## .note {
## 	padding: 15px;
## 	margin-bottom: 20px;
## 	border: 1px solid transparent;
## 	border-radius: 4px;
## 	background-color: #d9edf7;
## 	border-color: #bce8f1;
## 	color: #31708f;
## }
## .hljs-comment {
## 	color: navy !important;
## }

## ----message = FALSE----------------------------------------------------------
library(tidyverse)


## ----include = FALSE----------------------------------------------------------
tutorial <- here_rel("tutorial/penguins.Rmd")


## ----prompt = TRUE, comment = ""----------------------------------------------
1 + 2


## ----prompt = TRUE, comment = ""----------------------------------------------
c(2, 4, 6)

## ----prompt = TRUE, comment = ""----------------------------------------------
1 : 4


## ----prompt = TRUE, comment = ""----------------------------------------------
x <- c(2, 4, 6)


## ----prompt = TRUE, comment = ""----------------------------------------------
x + 1


## ----prompt = TRUE, comment = ""----------------------------------------------
x + x


## ----prompt = TRUE, comment = ""----------------------------------------------
log(x)


## ----prompt = TRUE, comment = ""----------------------------------------------
x[[1]]


## ----prompt = TRUE, comment = ""----------------------------------------------
x[1 : 2]


## ----prompt = TRUE, comment = ""----------------------------------------------
x[x > 2]


## ----prompt = TRUE, comment = ""----------------------------------------------
d <- data.frame(x, y = log(x))
d


## ----prompt = TRUE, comment = ""----------------------------------------------
d$x


## ----include = FALSE----------------------------------------------------------
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
head(geyser, 4)


## ----geyser-hist, echo = FALSE------------------------------------------------
ggplot(geyser) +
    geom_histogram(aes(x = duration),
                   bins = 15,
                   color = "black",
                   fill = "grey")

## ----geyser-hist, eval = FALSE------------------------------------------------
## ggplot(geyser) +
##     geom_histogram(aes(x = duration),
##                    bins = 15,
##                    color = "black",
##                    fill = "grey")


## ----geyser-scatter, echo = FALSE---------------------------------------------
ggplot(geyser) +
    geom_point(aes(x = lag(duration),
                   y = waiting))

## ----geyser-scatter, eval = FALSE---------------------------------------------
## ggplot(geyser) +
##     geom_point(aes(x = lag(duration),
##                    y = waiting))


## ----geyser-hist-narrow, echo = FALSE-----------------------------------------
p <- ggplot(geyser) +
    geom_histogram(aes(x = duration,
                       y = stat(density)),
                   fill = "grey",
                   color = "black",
                   binwidth = 0.1)
p

## ----geyser-hist-narrow, eval = FALSE-----------------------------------------
## p <- ggplot(geyser) +
##     geom_histogram(aes(x = duration,
##                        y = stat(density)),
##                    fill = "grey",
##                    color = "black",
##                    binwidth = 0.1)
## p


## -----------------------------------------------------------------------------
d <- geyser$duration
d_short <- d[d < 3]
d_long <- d[d >= 3]


## -----------------------------------------------------------------------------
mean(d_short)
sd(d_short)

## -----------------------------------------------------------------------------
mean(d_long)
sd(d_long)

## -----------------------------------------------------------------------------
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
sgd <-
    group_by(geyser, type) |>
    summarize(mean = mean(duration),
              sd = sd(duration),
              n = n()) |>
    ungroup() |>
    mutate(prop = n / sum(n))
sgd


## ----geyser-hist-dens, echo = FALSE-------------------------------------------
f1 <- function(x)
    sgd$prop[1] *
        dnorm(x, sgd$mean[1], sgd$sd[1])
f2 <- function(x)
    sgd$prop[2] *
        dnorm(x, sgd$mean[2], sgd$sd[2])
p <- p +
    stat_function(color = "red", fun = f1) +
    stat_function(color = "blue", fun = f2)
p

## ----geyser-hist-dens, eval = FALSE-------------------------------------------
## f1 <- function(x)
##     sgd$prop[1] *
##         dnorm(x, sgd$mean[1], sgd$sd[1])
## f2 <- function(x)
##     sgd$prop[2] *
##         dnorm(x, sgd$mean[2], sgd$sd[2])
## p <- p +
##     stat_function(color = "red", fun = f1) +
##     stat_function(color = "blue", fun = f2)
## p


## -----------------------------------------------------------------------------
sgd2 <-
    filter(geyser, duration != 2, duration != 4) |>
    group_by(type) |>
    summarize(mean = mean(duration),
              sd = sd(duration),
              n = n()) |>
    ungroup() |>
    mutate(prop = n / sum(n))
sgd2


## ----geyser-hist-dens-2, echo = FALSE-----------------------------------------
f1_2 <- function(x)
    sgd2$prop[1] *
        dnorm(x, sgd2$mean[1], sgd2$sd[1])
f2_2 <- function(x)
    sgd2$prop[2] *
        dnorm(x, sgd2$mean[2], sgd2$sd[2])
p <- p +
    stat_function(color = "red",
                  linetype = 2,
                  fun = f1_2) +
    stat_function(color = "blue",
                  linetype = 2,
                  fun = f2_2)
p

## ----geyser-hist-dens-2, eval = FALSE-----------------------------------------
## f1_2 <- function(x)
##     sgd2$prop[1] *
##         dnorm(x, sgd2$mean[1], sgd2$sd[1])
## f2_2 <- function(x)
##     sgd2$prop[2] *
##         dnorm(x, sgd2$mean[2], sgd2$sd[2])
## p <- p +
##     stat_function(color = "red",
##                   linetype = 2,
##                   fun = f1_2) +
##     stat_function(color = "blue",
##                   linetype = 2,
##                   fun = f2_2)
## p


## ----eval = FALSE, echo = FALSE-----------------------------------------------
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


## ----fig.width = 10-----------------------------------------------------------
p1 <- ggplot(barley) + geom_point(aes(x = yield, y = variety))
p2 <- ggplot(barley) + geom_point(aes(x = yield, y = site))
library(patchwork)
p1 + p2


## ----fig.width = 12-----------------------------------------------------------
p1 <- ggplot(barley) + geom_point(aes(x = yield, y = variety, color = year))
p2 <- ggplot(barley) + geom_point(aes(x = yield, y = site, color = year))
p1 + p2



## ----barley-color-sym, echo = FALSE, fig.width = 7----------------------------
ggplot(barley) +
    geom_point(aes(x = yield,
                   y = variety,
                   color = year,
                   shape = site))

## ----barley-color-sym, eval = FALSE-------------------------------------------
## ggplot(barley) +
##     geom_point(aes(x = yield,
##                    y = variety,
##                    color = year,
##                    shape = site))


## ----barley-color-sym-2, echo = FALSE, fig.width = 7--------------------------
ggplot(barley) +
    geom_point(aes(x = yield,
                   y = variety,
                   color = year,
                   shape = site),
               size = 2.5)

## ----barley-color-sym-2, eval = FALSE-----------------------------------------
## ggplot(barley) +
##     geom_point(aes(x = yield,
##                    y = variety,
##                    color = year,
##                    shape = site),
##                size = 2.5)


## ----barley-color-sym-3, echo = FALSE, fig.width = 7--------------------------
ggplot(barley) +
    geom_point(aes(x = yield,
                   y = variety,
                   color = year,
                   shape = site),
               size = 2.5,
               position =
                   position_jitter(
                       height = 0.15,
                       width = 0))

## ----barley-color-sym-3, eval = FALSE-----------------------------------------
## ggplot(barley) +
##     geom_point(aes(x = yield,
##                    y = variety,
##                    color = year,
##                    shape = site),
##                size = 2.5,
##                position =
##                    position_jitter(
##                        height = 0.15,
##                        width = 0))


## ----barley-facet, eval = FALSE-----------------------------------------------
## ggplot(barley) +
##     geom_point(aes(x = yield,
##                    y = variety,
##                    color = year)) +
##     facet_wrap(~site, ncol = 2)

## ----barley-facet, echo = FALSE, fig.width = 7, fig.height = 7----------------
ggplot(barley) +
    geom_point(aes(x = yield,
                   y = variety,
                   color = year)) +
    facet_wrap(~site, ncol = 2)


## ----barley-avg-dot, echo = FALSE, message = FALSE, fig.width = 7-------------
barley_site_year <-
    group_by(barley, site, year) |>
    summarize(yield = mean(yield)) |>
    ungroup()

ggplot(barley_site_year) +
    geom_point(aes(y = site,
                   x = yield,
                   color = year),
               size = 3)

## ----barley-avg-dot, eval = FALSE---------------------------------------------
## barley_site_year <-
##     group_by(barley, site, year) |>
##     summarize(yield = mean(yield)) |>
##     ungroup()
## 
## ggplot(barley_site_year) +
##     geom_point(aes(y = site,
##                    x = yield,
##                    color = year),
##                size = 3)


## ----barley-avg-dot-2, echo = FALSE, message = FALSE, fig.width = 7-----------
barley_site_year <-
    group_by(barley, site, year) |>
    summarize(yield = mean(yield)) |>
    ungroup()

ggplot(barley_site_year) +
    geom_line(aes(y = site,
                  x = yield,
                  group = site),
              color = "darkgrey",
              size = 2) +
    geom_point(aes(y = site,
                   x = yield,
                   color = year),
               size = 4)

## ----barley-avg-dot-2, eval = FALSE-------------------------------------------
## barley_site_year <-
##     group_by(barley, site, year) |>
##     summarize(yield = mean(yield)) |>
##     ungroup()
## 
## ggplot(barley_site_year) +
##     geom_line(aes(y = site,
##                   x = yield,
##                   group = site),
##               color = "darkgrey",
##               size = 2) +
##     geom_point(aes(y = site,
##                    x = yield,
##                    color = year),
##                size = 4)


## ----class.source = "fold-hide"-----------------------------------------------
library(ggrepel)
barley_site_year <-
    mutate(barley_site_year, year = fct_rev(year))
barley_site_year_1932 <-
    filter(barley_site_year, year == "1932")
ggplot(barley_site_year,
       aes(x = year, y = yield, group = site)) +
    geom_line() +
    geom_text_repel(aes(label = site),
                    data = barley_site_year_1932,
                    hjust = "left",
                    direction = "y") +
    scale_x_discrete(expand = expansion(mult = c(0.1, .25)),
                     position = "top") +
    labs(x = NULL, y = "Average Yield")


## ----barley-avg-bar, echo = FALSE, message = FALSE, fig.width = 7-------------
ggplot(barley_site_year) +
    geom_col(aes(x = yield,
                 y = site,
                 fill = year),
             size = 3,
             position = "dodge",
             width = .4)

## ----barley-avg-bar, eval = FALSE---------------------------------------------
## ggplot(barley_site_year) +
##     geom_col(aes(x = yield,
##                  y = site,
##                  fill = year),
##              size = 3,
##              position = "dodge",
##              width = .4)


## -----------------------------------------------------------------------------
HairEyeDF <- as.data.frame(HairEyeColor)
head(HairEyeDF)


## ----eye-bar, echo = FALSE----------------------------------------------------
eye <-
    group_by(HairEyeDF, Eye) |>
    summarize(Freq = sum(Freq)) |>
    ungroup()

ggplot(eye) +
    geom_col(aes(x = Eye,
                 y = Freq),
             position = "dodge")

## ----eye-bar, eval = FALSE----------------------------------------------------
## eye <-
##     group_by(HairEyeDF, Eye) |>
##     summarize(Freq = sum(Freq)) |>
##     ungroup()
## 
## ggplot(eye) +
##     geom_col(aes(x = Eye,
##                  y = Freq),
##              position = "dodge")


## ----eye-bar-2, echo = FALSE--------------------------------------------------
eye <-
    group_by(HairEyeDF, Eye) |>
    summarize(Freq = sum(Freq)) |>
    ungroup()

ggplot(eye) +
    geom_col(aes(x = Eye,
                 y = Freq,
                 fill = Eye),
             position = "dodge")

## ----eye-bar-2, eval = FALSE--------------------------------------------------
## eye <-
##     group_by(HairEyeDF, Eye) |>
##     summarize(Freq = sum(Freq)) |>
##     ungroup()
## 
## ggplot(eye) +
##     geom_col(aes(x = Eye,
##                  y = Freq,
##                  fill = Eye),
##              position = "dodge")


## ----eye-bar-3, echo = FALSE--------------------------------------------------
hazel_rgb <-
    col2rgb("brown") * 0.75 + col2rgb("green") * 0.25
hazel <-
    do.call(rgb, as.list(hazel_rgb / 255))

cols <-
    c(Blue = colorspace::lighten(colorspace::desaturate("blue", 0.3), 0.3),
      Green = colorspace::lighten("forestgreen", 0.1),
      Brown = colorspace::lighten("brown", 0.0001), ## 0.3?
      Hazel = colorspace::lighten(hazel, 0.3))

pb <- ggplot(eye) +
    geom_col(aes(x = Eye,
                 y = Freq,
                 fill = Eye),
             position = "dodge") +
    scale_fill_manual(values = cols)
pb

## ----eye-bar-3, eval = FALSE--------------------------------------------------
## hazel_rgb <-
##     col2rgb("brown") * 0.75 + col2rgb("green") * 0.25
## hazel <-
##     do.call(rgb, as.list(hazel_rgb / 255))
## 
## cols <-
##     c(Blue = colorspace::lighten(colorspace::desaturate("blue", 0.3), 0.3),
##       Green = colorspace::lighten("forestgreen", 0.1),
##       Brown = colorspace::lighten("brown", 0.0001), ## 0.3?
##       Hazel = colorspace::lighten(hazel, 0.3))
## 
## pb <- ggplot(eye) +
##     geom_col(aes(x = Eye,
##                  y = Freq,
##                  fill = Eye),
##              position = "dodge") +
##     scale_fill_manual(values = cols)
## pb


## ----eye-bar-stacked, echo = FALSE--------------------------------------------
psb <- ggplot(eye) +
    geom_col(aes(x = "",
                 y = Freq,
                 fill = Eye),
             color = "lightgrey") +
    scale_fill_manual(values = cols)
psb

## ----eye-bar-stacked, eval = FALSE--------------------------------------------
## psb <- ggplot(eye) +
##     geom_col(aes(x = "",
##                  y = Freq,
##                  fill = Eye),
##              color = "lightgrey") +
##     scale_fill_manual(values = cols)
## psb


## ----eye-pie, echo = FALSE----------------------------------------------------
(pp <- psb + coord_polar("y"))

## ----eye-pie, eval = FALSE----------------------------------------------------
## (pp <- psb + coord_polar("y"))


## ----eye-pie-2, echo = FALSE--------------------------------------------------
(pp <- pp + theme_void())

## ----eye-pie-2, eval = FALSE--------------------------------------------------
## (pp <- pp + theme_void())


## ----echo = FALSE, fig.height = 4, fig.width = 8------------------------------
cowplot::plot_grid(pb, pp)


## ----fig.width = 14, fig.height = 6, class.source = "fold-hide"---------------
eye_hairsex <-
    group_by(HairEyeDF, Hair, Sex) |>
    mutate(Prop = Freq / sum(Freq)) |>
    ungroup()

p1 <- ggplot(eye_hairsex) +
    geom_col(aes(x = Eye, y = Prop, fill = Eye)) +
    scale_fill_manual(values = cols) +
    facet_grid(Hair ~ Sex)
p2 <- ggplot(eye_hairsex) +
    geom_col(aes(x = "", y = Prop, fill = Eye)) +
    scale_fill_manual(values = cols) +
    coord_polar("y") +
    facet_grid(Hair ~ Sex) +
    theme_void()
cowplot::plot_grid(p1, p2)


## ----fig.width = 8, class.source = "fold-hide"--------------------------------
library(ggplot2)
river <- scan(here::here("data/river.dat"))
rd <- data.frame(flow = river, month = seq_along(river))
(pp <- ggplot(rd) + geom_point(aes(x = month, y = flow)))


## ----fig.width = 12, fig.height = 4, class.source = "fold-hide"---------------
pp + coord_fixed(3.5)


## ----fig.width = 12, fig.height = 4, class.source = "fold-hide"---------------
pl <- ggplot(rd) + geom_line(aes(x = month, y = flow))
pl + coord_fixed(3.5)


## ----fig.width = 8, class.source = "fold-hide"--------------------------------
pl


## -----------------------------------------------------------------------------
wind_turbines <- read.csv(here::here("data/us_wind.csv"), comment = "#")


## -----------------------------------------------------------------------------
wt_IA <- filter(wind_turbines, t_fips %/% 1000 == 19)


## -----------------------------------------------------------------------------
wt_IA <- filter(wt_IA, ! is.na(xlong), ! is.na(ylat))


## -----------------------------------------------------------------------------
wt_IA <- mutate(wt_IA, p_year = replace(p_year, p_year < 0, NA))


## ----iowa_sf_map, eval = FALSE------------------------------------------------
## iowa_sf <-
##     sf::st_as_sf(maps::map("county", "iowa",
##                            plot = FALSE,
##                            fill = TRUE))
## 
## p <- ggplot() +
##     geom_sf(data = iowa_sf) +
##     ggthemes::theme_map()
## p


## ----iowa_sf_map, echo = FALSE, fig.width = 8---------------------------------
iowa_sf <-
    sf::st_as_sf(maps::map("county", "iowa",
                           plot = FALSE,
                           fill = TRUE))

p <- ggplot() +
    geom_sf(data = iowa_sf) +
    ggthemes::theme_map()
p


## ----wt-IA-all, eval = FALSE--------------------------------------------------
## p + geom_point(aes(xlong, ylat),
##                data = wt_IA)

## ----wt-IA-all, echo = FALSE, fig.width = 8-----------------------------------
p + geom_point(aes(xlong, ylat),
               data = wt_IA)


## ----wt-IA-color, eval = FALSE------------------------------------------------
## year_brk <- c(0, 2005, 2010, 2015, 2020, Inf)
## year_lab <- c("before 2005",
##               "2005-2009",
##               "2010-2014",
##               "2015-2020",
##               "2021 and later")
## wt_IA <-
##     mutate(wt_IA,
##            year = cut(p_year,
##                       breaks = year_brk,
##                       labels = year_lab,
##                       right = FALSE))
## p + geom_point(aes(xlong,
##                    ylat,
##                    color = year),
##                data = wt_IA,
##                size = 3)

## ----wt-IA-color, echo = FALSE, fig.width = 8---------------------------------
year_brk <- c(0, 2005, 2010, 2015, 2020, Inf)
year_lab <- c("before 2005",
              "2005-2009",
              "2010-2014",
              "2015-2020",
              "2021 and later")
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


## ----include = FALSE----------------------------------------------------------
cancer_data_file <- here_rel("data/Invasive-Cancer-Incidence-Rates-by-County-in-Iowa-Lung-and-Bronchus-2011.csv")


## ----message = FALSE----------------------------------------------------------
fname <- here::here("data/Invasive-Cancer-Incidence-Rates-by-County-in-Iowa-Lung-and-Bronchus-2011.csv")
d <- read_csv(fname, skip = 2)
head(d)


## -----------------------------------------------------------------------------
d <- select(d, county = 1, population = 2, count = 3)


## -----------------------------------------------------------------------------
tail(d)


## -----------------------------------------------------------------------------
d <- filter(d, ! is.na(population))
d <- filter(d, county != "STATE")


## -----------------------------------------------------------------------------
d <- mutate(d, count = as.numeric(count))


## -----------------------------------------------------------------------------
count(d, count == 0)

## -----------------------------------------------------------------------------
any(d$count == 0, na.rm = TRUE)


## -----------------------------------------------------------------------------
d <- replace_na(d, list(count = 0))


## -----------------------------------------------------------------------------
d$county[1]
iowa_sf$ID[1]


## -----------------------------------------------------------------------------
d <- mutate(d, cname = county, county = tolower(county))
iowa_sf <- mutate(iowa_sf, ID = sub("iowa,", "", ID))
iowa_sf <- rename(iowa_sf, county = ID)


## -----------------------------------------------------------------------------
setdiff(d$county, iowa_sf$county)
setdiff(iowa_sf$county, d$county)


## -----------------------------------------------------------------------------
d <- mutate(d, county = sub("'", "", county))

setdiff(d$county, iowa_sf$county)
setdiff(iowa_sf$county, d$county)


## -----------------------------------------------------------------------------
d <- mutate(d, rate1K = 1000 * (count / population))
md <- left_join(iowa_sf, d, "county")
head(md)


## ----cancer-map-1, eval = FALSE-----------------------------------------------
## ggplot(md) +
##     geom_sf(aes(fill = rate1K))

## ----cancer-map-1, echo = FALSE, fig.width = 8--------------------------------
ggplot(md) +
    geom_sf(aes(fill = rate1K))


## ----cancer-map-2, eval = FALSE-----------------------------------------------
## library(ggthemes)
## library(viridis)
## ggplot(md) +
##     geom_sf(aes(fill = rate1K),
##             color = "grey") +
##     scale_fill_viridis(
##         name = "Rate per 1000") +
##     theme_map()


## ----cancer-map-2, echo = FALSE, fig.width = 8, message = FALSE---------------
library(ggthemes)
library(viridis)
ggplot(md) +
    geom_sf(aes(fill = rate1K),
            color = "grey") +
    scale_fill_viridis(
        name = "Rate per 1000") +
    theme_map()


## ----cancer-map-plotly, eval = FALSE------------------------------------------
## mdl <- mutate(md,
##               label = paste(cname,
##                             round(rate1K, 1),
##                             population,
##                             sep = "\n"))
## p <- ggplot(mdl) +
##     geom_sf(aes(fill = rate1K,
##                 text = label),
##             color = "grey") +
##     scale_fill_viridis(
##         name = "Rate per 1000") +
##     theme_map()
## 
## plotly::ggplotly(p, tooltip = "text")


## ----cancer-map-plotly, echo = FALSE, fig.width = 8---------------------------
mdl <- mutate(md,
              label = paste(cname,
                            round(rate1K, 1),
                            population,
                            sep = "\n"))
p <- ggplot(mdl) +
    geom_sf(aes(fill = rate1K,
                text = label),
            color = "grey") +
    scale_fill_viridis(
        name = "Rate per 1000") +
    theme_map()

plotly::ggplotly(p, tooltip = "text")


## ----fig.height = 6.5, fig.width = 9, class.source = "fold-hide"--------------
library(leaflet)
pal <- colorNumeric(palette = "viridis", domain = md$rate1K)
lab <- lapply(paste0(md$cname, "<BR>",
                     "Rate: ", round(md$rate1K, 1), "<BR>",
                     "Pop: ", scales::comma(md$population,
                                            accuracy = 1)),
              htmltools::HTML)
leaflet(sf::st_transform(md, 4326)) |>
    addPolygons(weight = 2,
                color = "grey",
                fillColor = ~ pal(rate1K),
                fillOpacity = 1,
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                label = lab) |>
    addLegend(pal = pal, values = ~ rate1K, opacity = 1)


## -----------------------------------------------------------------------------
lausURL <- here::here("data/laucntycur14-2020.txt")
lausUS <- read.table(lausURL,
                     col.names = c("LAUSAreaCode", "State", "County",
                                   "Title", "Period",
                                   "LaborForce", "Employed",
                                   "Unemployed", "UnempRate"),
                     quote = '"', sep = "|", skip = 6,
                     stringsAsFactors = FALSE, strip.white = TRUE,
                     fill = TRUE)
footstart <- grep("------", lausUS$LAUSAreaCode)
lausUS <- lausUS[1 : (footstart - 1), ]


## -----------------------------------------------------------------------------
lausUS <- separate(lausUS, Title, c("cname", "scode"),
                   sep = ", ", fill = "right")


## -----------------------------------------------------------------------------
sapply(lausUS, class)


## -----------------------------------------------------------------------------
lausUS <- mutate(lausUS, UnempRate = as.numeric(UnempRate))


## -----------------------------------------------------------------------------
select_if(lausUS, anyNA) |> names()


## -----------------------------------------------------------------------------
select(lausUS, cname, scode) |>
    filter(is.na(scode)) |>
    unique()


## -----------------------------------------------------------------------------
select(lausUS, scode, Period, UnempRate) |>
    filter(is.na(UnempRate)) |>
    unique()


## -----------------------------------------------------------------------------
lausUS <- mutate(lausUS,
                 Period = fct_inorder(Period),
                 LaborForce = as.numeric(gsub(",", "", LaborForce)),
                 Unemployed = as.numeric(gsub(",", "", Unemployed)))


## ----fig.width = 10, class.source = "fold-hide"-------------------------------
group_by(lausUS, Period) |>
    summarize(Unemployed = sum(Unemployed, na.rm = TRUE),
              LaborForce = sum(LaborForce, na.rm = TRUE),
              UnempRate = 100 * (Unemployed / LaborForce)) |>
    ggplot(aes(Period, UnempRate, group = 1)) +
    geom_line()


## -----------------------------------------------------------------------------
lausUS <- mutate(lausUS, fips = State * 1000 + County)


## -----------------------------------------------------------------------------
counties_sf <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
county.fips <-
    mutate(maps::county.fips, polyname = sub(":.*", "", polyname)) |>
    unique()
counties_sf <- left_join(counties_sf, county.fips, c("ID" = "polyname"))
states_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))


## -----------------------------------------------------------------------------
summaryUS <- group_by(lausUS, County, State, fips) |>
    summarize(avg_unemp = mean(UnempRate, na.rm = TRUE),
              max_unemp = max(UnempRate, na.rm = TRUE),
              apr_unemp = UnempRate[Period == "Apr-20"]) |>
    ungroup()
head(summaryUS)


## ----fig.width = 9, fig.height = 6, class.source = "fold-hide"----------------
left_join(counties_sf, summaryUS, "fips") |>
    ggplot() +
    geom_sf(aes(fill = apr_unemp)) +
    scale_fill_viridis(name = "Rate", na.value = "red") +
    theme_map() +
    geom_sf(data = states_sf, col = "grey", fill = NA)


## -----------------------------------------------------------------------------
anti_join(counties_sf, summaryUS, "fips")


## ----eval = FALSE, echo = FALSE-----------------------------------------------
## ## old version
## counties_sf <- mutate(counties_sf, fips = replace(fips, fips == 46113, 46102))


## -----------------------------------------------------------------------------
counties_sf <- mutate(counties_sf,
                      fips = replace(fips, grepl("oglala", ID), 46102))


## ----fig.width = 9, fig.height = 6, class.source = "fold-hide"----------------
left_join(counties_sf, summaryUS, "fips") |>
    ggplot() +
    geom_sf(aes(fill = apr_unemp)) +
    scale_fill_viridis(name = "Rate", na.value = "red") +
    theme_map() +
    geom_sf(data = states_sf, col = "grey", fill = NA)


## ----echo = FALSE, eval = FALSE-----------------------------------------------
## ggpoly2sf <- function(poly, coords = c("long", "lat"),
##                       id = "group", region = "region", crs = 4326) {
##     sf::st_as_sf(poly, coords = coords, crs = crs) |>
##     group_by(!! as.name(id), !! as.name(region)) |>
##     summarize(do_union = FALSE) |>
##     sf::st_cast("POLYGON") |>
##     ungroup() |>
##     group_by(!! as.name(region)) |>
##     summarize(do_union = FALSE) |>
##     ungroup()
## }
## m_sf <- ggpoly2sf(socviz::county_map, c("long", "lat"), "group", "id")
## m_sf <- mutate(m_sf, fips = as.numeric(id))
## m_sf <- mutate(m_sf, fips = replace(fips, fips == 46113, 46102))
## ggplot(m_sf) + geom_sf()
## au <- group_by(lausUS, fips) |>
##     summarize(avg_ur = mean(UnempRate, na.rm = TRUE))
## mu <- group_by(lausUS, fips) |>
##     summarize(max_ur = max(UnempRate, na.rm = TRUE))
## da <- left_join(m_sf, au, "fips")
## dm <- left_join(m_sf, mu, "fips")
## ggplot(da, aes(fill = avg_ur)) +
##     geom_sf(size = 0.1) +
##     scale_fill_viridis(name = "Rate", na.value = "red")
## ggplot(dm, aes(fill = max_ur)) +
##     geom_sf(size = 0.1) +
##     scale_fill_viridis(name = "Rate", na.value = "red")
## ggplot(left_join(m_sf, filter(lausUS, Period == "Apr-20"), "fips"),
##        aes(fill = UnempRate)) +
##     geom_sf(size = 0.1) +
##     scale_fill_viridis(name = "Rate", na.value = "red")


## -----------------------------------------------------------------------------
library(readxl)
gcm <- read_excel(here::here("data/gapminder-under5mortality.xlsx"))


## -----------------------------------------------------------------------------
head(gcm, 3)


## -----------------------------------------------------------------------------
names(gcm)[1] <- "country"


## -----------------------------------------------------------------------------
tgcm <-
    pivot_longer(gcm, -1, names_to = "year", values_to = "u5mort") |>
    mutate(year = as.numeric(year))
head(tgcm, 3)


## ----u5-1, eval = FALSE-------------------------------------------------------
## p <- ggplot(tgcm) +
##     geom_line(aes(year,
##                   u5mort,
##                   group = country),
##               alpha = 0.3)
## plotly::ggplotly(p)

## ----u5-1, echo = FALSE, fig.height = 6, fig.width = 8------------------------
p <- ggplot(tgcm) +
    geom_line(aes(year,
                  u5mort,
                  group = country),
              alpha = 0.3)
plotly::ggplotly(p)


## ----u5-2, eval = FALSE-------------------------------------------------------
## countries <- c("United States",
##                "United Kingdom",
##                "Germany",
##                "China",
##                "Egypt")
## filter(tgcm, country %in% countries) |>
##     ggplot() +
##     geom_line(aes(x = year,
##                   y = u5mort,
##                   color = country))

## ----u5-2, echo = FALSE, fig.height = 6, fig.width = 8------------------------
countries <- c("United States",
               "United Kingdom",
               "Germany",
               "China",
               "Egypt")
filter(tgcm, country %in% countries) |>
    ggplot() +
    geom_line(aes(x = year,
                  y = u5mort,
                  color = country))


## ----u5-3, eval = FALSE-------------------------------------------------------
## tgcm_miss <-
##     group_by(tgcm, country) |>
##     summarize(anyNA = anyNA(u5mort)) |>
##     filter(anyNA) |>
##     pull(country)
## 
## p <- filter(tgcm,
##             country %in% tgcm_miss) |>
##     ggplot(aes(x = year,
##                y = u5mort,
##                group = country)) +
##     geom_line(na.rm = TRUE) +
##     xlim(c(1940, 2020))
## plotly::ggplotly(p)

## ----u5-3, echo = FALSE, fig.height = 6, fig.width = 7------------------------
tgcm_miss <-
    group_by(tgcm, country) |>
    summarize(anyNA = anyNA(u5mort)) |>
    filter(anyNA) |>
    pull(country)

p <- filter(tgcm,
            country %in% tgcm_miss) |>
    ggplot(aes(x = year,
               y = u5mort,
               group = country)) +
    geom_line(na.rm = TRUE) +
    xlim(c(1940, 2020))
plotly::ggplotly(p)

