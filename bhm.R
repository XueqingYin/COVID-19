library(spdep)
library(rgdal)
library(ggplot2)
library(INLA)
library(dplyr)
library(raster)
library(inlabru)
# --- load raw data
covidat<-read.csv("cases.csv")
nbhoods<-data.frame(MSOA11CD=unique(covidat[,c("MSOA11CD")]))

# --- load shapefile
shape<-readOGR(dsn='MSOA_(Dec_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp')
proj4string(shape)
shape2<-spTransform(shape,CRS("+proj=longlat +datum=WGS84 +no_defs"))
shape2$area_sqkm<-raster::area(shape2)/1000000
sp.dat<-sp::merge(shape2,nbhoods,all.x=FALSE,
                  by.x="MSOA11CD",
                  by.y="MSOA11CD",
                  duplicateGeoms = TRUE)

nb.loc<-unique(sp.dat@data[,c("MSOA11CD","MSOA11NM","LONG","LAT","area_sqkm")])
sp.dat.sf<-sp.dat %>%
  st_as_sf() 

# --- prepare data

dates<- unique(covidat$date)
df <- data.frame(Date = as.Date(dates, "%Y-%m-%d"))
df$week<-rev(c(1,1+cumsum(diff(df$Date)!=0)))
covidat$date<-as.Date(covidat$date, "%Y-%m-%d")
covidat<- merge(covidat, df,by.x="date",by.y="Date")
covid.dat<-covidat[,c("date","week","MSOA11CD", "cases","Population","carebeds","AandE", "IMD")]
covid.dat$prevalence<-covid.dat$cases/covid.dat$Population

covid.dat<-cbind.data.frame(covid.dat,
                            nb.loc[match(covid.dat$MSOA11CD,nb.loc$MSOA11CD),
                                   c("LONG","LAT","area_sqkm")]
)
n_groups<-length(unique(covidat[,"week"])) 

# -------------------------------------------------------------------------
# look up geography changed slightly for 2021 
# -------------------------------------------------------------------------
lookup<-read.csv("lookupV1.csv")

lookup<-unique(data.frame(
  "MSOA11CD"=covid.dat$MSOA11CD,
  "MSOA21CD"=lookup[match(covid.dat$MSOA11CD,lookup$MSOA11CD),]$MSOA21CD))

# -------------------------------------------------------------------------
# load in ethnicity data 
# -------------------------------------------------------------------------

ethnic<-read.csv("census2021-ts021-msoa.csv")

prop.chinese<-(ethnic$Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Chinese/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.indian<-(ethnic$Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Indian/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.bangladeshi<-(ethnic$Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Bangladeshi/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.pakistani<-(ethnic$Ethnic.group..Asian..Asian.British.or.Asian.Welsh..Pakistani/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.ba<-(ethnic$Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African..African/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.bc<-(ethnic$Ethnic.group..Black..Black.British..Black.Welsh..Caribbean.or.African..Caribbean/ethnic$Ethnic.group..Total..All.usual.residents*100)
prop.wb<-(ethnic$Ethnic.group..White..English..Welsh..Scottish..Northern.Irish.or.British/ethnic$Ethnic.group..Total..All.usual.residents*100)

ethnic.dat<-data.frame("MSOA21CD"=ethnic$geography.code,
                       "prop.chinese"=prop.chinese,    
                       "prop.indian"=prop.indian,  
                       "prop.bangladeshi"=prop.bangladeshi,  
                       "prop.pakistani"=prop.pakistani,  
                       "prop.ba"=prop.ba ,
                       "prop.bc"=prop.bc,
                       "prop.wb"=prop.wb
)

ethnic.dat<-merge(lookup, ethnic.dat, by.x= "MSOA21CD",by.y="MSOA21CD")
covid.dat<-cbind.data.frame(covid.dat,
                            ethnic.dat[match(covid.dat$MSOA11CD,ethnic.dat$MSOA11CD),
                                       c("MSOA21CD",
                                         "prop.chinese",
                                         "prop.indian",
                                         "prop.bangladeshi",
                                         "prop.pakistani", 
                                         "prop.ba",
                                         "prop.bc",
                                         "prop.wb"
                                       )])

# -------------------------------------------------------------------------
# load in population density data 
# -------------------------------------------------------------------------
covid.dat$pop_den<-log(covid.dat$Population/covid.dat$area_sqkm)

# -------------------------------------------------------------------------
# load in age data 
# -------------------------------------------------------------------------

age1<-(covidat$age18.21+covidat$age22.24+covidat$age25.29)/covidat$Population*100
age2<-(covidat$age30.34+covidat$age35.39+covidat$age40.44)/covidat$Population*100
age3<-(covidat$age45.49+covidat$age50.54+covidat$age55.59+covidat$age60.64)/covidat$Population*100
age4<-(covidat$age65.69+covidat$age70.74+covidat$age75.79+covidat$age80.84+
         covidat$age85.89+covidat$age90plus)/covidat$Population*100

age.dat<-data.frame(
  "age1"=age1,
  "age2"=age2,
  "age3"=age3,
  "age4"=age4
)

covid.dat<-cbind.data.frame(covid.dat,
                            age.dat)


##### care home beds ratio
covid.dat$carebeds.ratio<-covidat$carebeds/(covidat$Population- covidat$age0.4-covidat$age12.17)

## environment data
load(file="envdat.RData")
covid.dat<-cbind.data.frame(covid.dat,
                            pm25.poly[match(covid.dat$MSOA11CD,pm25.poly$MSOA11CD),c("pm25")]
)


## unemployment data
unemploy<-read.csv("census2021-ts065-msoa.csv")

unemploy<-merge(lookup, unemploy, by.x= "MSOA21CD",by.y="MSOA21CD")
colnames(unemploy)[5]<-"U_T"
covid.dat<-cbind.data.frame(covid.dat,
                            "U_T"= unemploy[match(covid.dat$MSOA11CD,unemploy$MSOA11CD),
                                            c("U_T"
                                            )])

covid.dat$UER<-covid.dat$U_T/covid.dat$Population*100


## income data
income<-read.csv("income.csv")
covid.dat<-cbind.data.frame(covid.dat,
                            "income"=income[match(covid.dat$MSOA11CD,income$MSOA.code),
                                            c("annualincome")])

covid.dat$income<-covid.dat$income/1000

# -------------------------------------------------------------------------
# convert the factor variable into dummy variable
# -------------------------------------------------------------------------

covid.dat<-covid.dat %>% 
  model.matrix(object = ~AandE) %>%
  as.data.frame() %>%
  dplyr::select(-1) %>%
  bind_cols(covid.dat)

covid.dat$logprevalence<-log(covid.dat$prevalence)

##### --- create mesh
initial.range<-diff(range(st_coordinates(sp.dat.sf)[,1]))/5
max.edge<-initial.range/8
inner.max.edge<-max.edge*2
outer.max.edge<-initial.range
mesh <- INLA::inla.mesh.2d(loc=nb.loc[,c("LONG","LAT")],
                           max.edge = c(1,2)*max.edge,
                           offset=c(inner.max.edge, outer.max.edge),
                           cutoff = max.edge/7
)

prev.group<-covid.dat$week
sp::coordinates(covid.dat)<- c("LONG","LAT")

# -------------------------------------------------------------------------
# Make SPDE
# -------------------------------------------------------------------------
prior.range<-initial.range
spde <- inla.spde2.pcmatern(
  mesh = mesh, 
  prior.range = c(prior.range/5, 0.5), 
  prior.sigma = c(1, 0.01) 
)

rhoprior <- list(theta = list(prior = 'pccor1', 
                              param = c(0, 0.9)))

# -------------------------------------------------------------------------
# Define the model
# -------------------------------------------------------------------------

cmp <- cases ~ 0 + Intercept(1) + income + UER+
  carebeds.ratio + AandETRUE +
  prop.chinese+prop.indian+prop.bangladeshi+prop.pakistani+prop.ba+prop.bc+prop.wb+
  age1+age2+age3+age4+
  pm25+
  f(main=coordinates,  
    model=spde,
    group=prev.group,
    ngroup=n_groups,
    control.group=list(model="ar1",
                       hyper=rhoprior))
# -------------------------------------------------------------------------
# run the model
# -------------------------------------------------------------------------

inlabru.model<-bru(cmp, data = covid.dat, 
                   family = "poisson",
                   E = covid19_data$Population,
                   control.family = list(link = "log"),
                   options = list( 
                     verbose = TRUE,
                     inla.mode="experimental" 
                   )
)

