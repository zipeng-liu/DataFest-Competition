library(tidyverse)
library(dplyr)
library(ggmap)
library(geoR)
library(sf)
library(tidyverse)
library(sp)
library(rgdal)
library(tmap)
library(raster)
library(geodist)
library(INLA)
library(MASS)
library(stringr)
library(maps)

usa = read_csv("./US-19/nmurx_us_19Q1.csv")
attach(usa)
usa_m <- getData("GADM", country = "United States", level = 2)

###Hard code for now, demographical data for californian counties, arranged###
###alphabetically###

mean_ic_ca = c(44283, 29041, 30100, 27537, 32494, 27336, 45524, 22832, 40382, 23284, 21736,
                 26747, 17540, 31540, 22553, 21186, 25404, 21897, 32469, 21394, 69275,
                 29776, 27395, 21634, 23235, 33421, 28836, 42677, 37145, 39590, 41508, 32710, 27142, 
                 31311, 31475, 23956, 36156, 64157, 26145, 35832, 57375, 34229, 52451,
                 39001, 27983, 31972, 28130, 33700, 39929, 25101, 26070, 23126,
                 25964, 20421, 33685, 36891, 32598, 23867)

white_pop_ca = 0.01 * c(31.1, 64.1, 77.4, 71.6, 80.7, 34.3, 43.2, 62.4, 77.6, 29.0, 50.8, 74.1, 10.4, 61.8,
                          33.5, 31.8, 69.7, 65.1, 26.1, 33.7, 71.5, 79.7, 64.7, 27.1, 77.7, 65.3, 29.6,
                          52.0, 84.9, 40.1, 72.1, 83.4, 34.7, 44.2, 33.5, 27.9, 45.2, 40.3, 31.0, 68.6, 38.9,
                          44.1, 31.0, 56.9, 79.6, 82.8, 75.7, 37.6, 63.1, 41.1, 45.3, 67.6, 81.8, 28.1, 79.8,
                          45.0, 46.3, 54.3)



bach_ca = 0.001 * c(460, 300, 203, 265, 187, 154, 417, 143, 337, 207, 139, 298, 145, 261, 161, 135, 158,
                      120, 318, 145, 588, 234, 247, 140, 159, 306, 245, 349, 366, 399, 386, 235, 218, 304, 197, 
                      203, 381, 571, 184, 346, 499, 338, 513, 400, 222, 187, 225, 262, 349, 169, 177, 155,
                      197, 143, 200, 331, 416, 167)

ny = subset(usa, usa$DEM_STATE == "NY")
we = subset(usa, usa$DEM_REGION == 4)
we = subset(we , !(we$DEM_STATE == "HI" | we$DEM_STATE == "AK"))

ggmap::register_google(key = "")

ny_d = data.frame(unique(ny$DEM_POSTAL))
we_d = data.frame(unique(we$DEM_POSTAL))

we_sta = c()
for (i in 1:length(we_d$unique.we.DEM_POSTAL.)){
  j = which(we$DEM_POSTAL == we_d$unique.we.DEM_POSTAL.[i])[1]
  we_sta = c(we_sta, we$DEM_STATE[j])
}

we_d = cbind(we_d, we_sta)
lon = c()
lat = c()

for (i in 1:length(we_d$unique.we.DEM_POSTAL.)){
  temp = ggmap::geocode(paste(we_d$we_sta[i], "-",we_d$unique.we.DEM_POSTAL.[i], "01", sep = ""))
  lon = c(lon, temp$lon)
  lat = c(lat, temp$lat)
}

we_d = cbind(we_d, lon, lat)

we <- we %>% replace_na(list(BHYD_NMUYR = 0, BHYD_NMU = 0, BUP_NMUYR = 0, BUP_NMU = 0,
                             COD_NMUYR = 0, COD_NMU = 0, DIHY_NMUYR = 0, DIHY_NMU = 0,
                             FENT_NMUYR = 0, FENT_NMU = 0, HYD_NMUYR = 0, HYD_NMU = 0,
                             HYDM_NMUYR = 0, HYDM_NMU = 0, METH_NMUYR = 0, METH_NMU = 0,
                             MORPH_NMUYR = 0, MORPH_NMU = 0, OXY_NMUYR = 0, OXY_NMU = 0,
                             OXYM_NMUYR = 0, OXYM_NMU = 0, SUF_NMUYR = 0, SUF_NMU = 0,
                             TAP_NMUYR = 0, TAP_NMU = 0, TRAM_NMUYR = 0, TRAM_NMU = 0)) %>% 
  mutate(opioid_NMU = BHYD_NMUYR + BHYD_NMU + BUP_NMUYR + BUP_NMU + COD_NMUYR + COD_NMU + 
           DIHY_NMUYR + DIHY_NMU + FENT_NMUYR + FENT_NMU + HYD_NMUYR + HYD_NMU + HYDM_NMUYR + HYDM_NMU + 
           METH_NMUYR + METH_NMUYR + MORPH_NMUYR + MORPH_NMU + OXY_NMUYR + OXY_NMU +
           OXYM_NMUYR + OXYM_NMUYR + OXYM_NMU + SUF_NMUYR + SUF_NMU + TAP_NMUYR + TAP_NMU + TRAM_NMUYR + TRAM_NMU)

for (i in 1:nrow(we)) {
  if (we$opioid_NMU[i] > 0) {
    we$opioid_NMU[i] <- 1 
  }
}

postal_opioid_abuse <- function(data){
  l = c()
  v = c()
  u = c()
  for (i in 1:nrow(data)){
    if (!(data$DEM_POSTAL[i] %in% l)){
      l = c(l, data$DEM_POSTAL[i])
      v = c(v, data$opioid_NMU[i])
      u = c(u, 1)
    }
    else{
      j = which(l == data$DEM_POSTAL[i])
      v[j] = v[j] + data$opioid_NMU[i]
      u[j] = u[j] + 1
    }
  }
  return(list(v,u))
}

we_d = cbind(we_d, postal_opioid_abuse(we))

names(we_d)[1] = "Postal_Code"
names(we_d)[2] = "States"
names(we_d)[5] = "Opioid_Abuse"
names(we_d)[6] = "Total"

ca_index = which(we_d$States == "CA")

ca_d = we_d[ca_index, ]
ca_d = na.omit(ca_d)
sps <-SpatialPoints(ca_d[,c("lon", "lat")],
                    proj4string =CRS("+proj=utm +zone=28"))
spst <-spTransform(sps,CRS("+proj=longlat +datum=WGS84"))
ca_d[,c("lon", "lat")] <- sps@coords
ca.sf <-st_as_sf(ca_d, coords =c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
ca_m<-usa_m[usa_m@data$NAME_1 == "California", ]
coo <-cbind(ca_d$lon, ca_d$lat)
coo_sp <- SpatialPointsDataFrame(coo, data.frame(id=1:nrow(coo)))

crs(coo_sp) <- crs(ca_m)

over_county = over(coo_sp, ca_m)
ca_d$county = over_county$NAME_2

cali_county = ca_m$NAME_2


mean_ic = c()
bach = c()
white_pop = c()
for (i in 1:nrow(we_d)){
  j = which(cali_county == ca_d$county[i])
  mean_ic = c(mean_ic, mean_ic_ca[j])
  white_pop = c(white_pop, white_pop_ca[j])
  bach = c(bach, bach_ca[j])
}

ca_d$mean_ic = mean_ic
ca_d$bach = bach
ca_d$white_pop = white_pop

mesh <-inla.mesh.2d(
  loc = coo, 
  max.edge =c(0.2, 0.4),
  cutoff = 0.1
)


spde <-inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

indexs <-inla.spde.make.index("s", spde$n.spde)

A <-inla.spde.make.A(mesh = mesh, loc = coo)

bb <- c(min(ca_d$lon), min(ca_d$lat), max(ca_d$lon), max(ca_d$lat))
x <- seq(bb[1] - 0.1, bb[3] + 0.1, length.out = 64)
y <- seq(bb[2] - 0.1, bb[4] + 0.1, length.out = 64)
coop <- as.matrix(expand.grid(x, y))
names(coop)[1] = "lon"
names(coop)[2] = "lat"
coop_sp <- SpatialPointsDataFrame(coop, data.frame(id=1:nrow(coop)))
crs(coop_sp) <- crs(ca_m)

over_res = over(coop_sp, ca_m)

ind = which(is.na(over_res$NAME_2))


ca_pdf = data.frame(coop)
ca_pdf$county = over_res$NAME_2

coop = coop[-ind, ]
ca_pdf = ca_pdf[-ind, ]

plot(coop$lat, coop$lon)

bach_p = c()
white_p = c()
ic_p = c()
for (i in 1:nrow(ca_pdf)){
  j = which(cali_county == ca_pdf$county[i])
  bach_p = c(bach_p, bach_ca[j])
  white_p = c(white_p, white_pop_ca[j])
  ic_p = c(ic_p, mean_ic_ca[j])
}

ca_pdf$bach = bach_p
ca_pdf$white_pop = white_p
ca_pdf$mean_ic = ic_p

ca_pdf$white_pop = log(ca_pdf$white_pop) - log(1 - ca_pdf$white_pop)
ca_pdf$bach = log(ca_pdf$bach) - log(1 - ca_pdf$bach)

ca_d$white_pop = log(ca_d$white_pop) - log(1 - ca_d$white_pop)
ca_d$bach = log(ca_d$bach) - log(1 - ca_d$bach)

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)


stk.e <-inla.stack(tag = "est",
                   data =list(y = ca_d$Opioid_Abuse, numtrials = ca_d$Total),
                   A =list(1, A),
                   effects =list(data.frame(b0 = rep(1, nrow(coo)), mean_ic = ca_d$mean_ic,
                                            bach = ca_d$bach,
                                            white_pop = ca_d$white_pop), s = indexs))


stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = rep(1, nrow(coop)), mean_ic = ca_pdf$mean_ic,
                                     bach = ca_pdf$bach,
                                     white_pop = ca_pdf$white_pop), s = indexs))


stk.full <-inla.stack(stk.e, stk.p)

formula <- y ~ 0 + b0 + mean_ic + bach + white_pop + f(s, model = spde)

res <-inla(formula,
           family = "binomial", 
           Ntrials = numtrials,
           control.family =list(link = "logit"),
           data =inla.stack.data(stk.full),
           control.predictor =list(compute = TRUE, link = 1,A =inla.stack.A(stk.full))
)

index <- inla.stack.index(stk.full, tag = "pred")$data
pred_mean <- res$summary.fitted.values[index, "mean"]
pred_ll <- res$summary.fitted.values[index, "0.025quant"]
pred_ul <- res$summary.fitted.values[index, "0.975quant"]


dpm <- rbind(
  data.frame(
    longitude= coop[, 1], latitude = coop[, 2],
    value = pred_mean, variable = "Predicted Prevalence"
  ),
  data.frame(
    longitude= coop[, 1], latitude = coop[, 2],
    value = pred_ll, variable = "Lower CI at Quantile 0.025"
  ),
  data.frame(
    longitude= coop[, 1], latitude = coop[, 2],
    value = pred_ul, variable = "Upper CI at Quantile 0.975"
  )
)



dpm$variable <- as.factor(dpm$variable)

ggplot(dpm) + geom_tile(aes(longitude, latitude, fill = value)) +
  facet_wrap(~variable, nrow = 1) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(
    name = "Prev",
    low = "blue", high = "orange"
  ) + labs(title = "Opioids Abuse Prevalence Prediction in California",
              y = "Latitude", x = "Longitude")
  theme_bw()
  

exc_prev <- sapply(res$marginals.fitted.values[index],
                   FUN = function(marg){1 - inla.pmarginal(q = 0.15, marginal = marg)})


dem <-  data.frame(
  longitude= coop[, 1], latitude = coop[, 2],
  value = exc_prev, variable = "Probability of Prevalence > 0.15"
)


ggplot(dem) + geom_tile(aes(longitude, latitude, fill = value)) +
  facet_wrap(~variable, nrow = 1) +
  coord_fixed(ratio = 1) +
  scale_fill_gradient(
    name = "P(Prev > 0.15)",
    low = "blue", high = "orange"
  ) + labs(title = "Probability of Prevalence Exceed 15%", 
           y = "Latitude", x = "Longitude")
theme_bw()

ca_d$Prevalence = ca_d$Opioid_Abuse / ca_d$Total

ggplot() + 
  geom_polygon(data=ca_m, aes(long,lat,group=group), fill="whitesmoke")+
  geom_path(data=ca_m, aes(long,lat, group=group), color="black",
            size=0.1) +
  geom_point(data = ca_d, aes(lon, lat, color = Prevalence), size = 2) +
  coord_fixed(ratio = 1) +
  scale_color_gradient(low = "blue", high = "orange") +
  labs(title= "Sample Prevalence of Opioids Abuse in California",
      y="Latitude", x = "Longitude")
  theme_bw()
