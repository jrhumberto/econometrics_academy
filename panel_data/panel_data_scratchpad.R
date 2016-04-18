library(dplyr)

# http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/

# load packages
require(plm)
require(lmtest)  # for waldtest()
# get data and load as pdata.frame
url <- "http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt"
p.df <- read.table(url)
names(p.df) <- c("firmid", "year", "x", "y")
p.df <- pdata.frame(p.df, index = c("firmid", "year"), drop.index = F, row.names = T)
head(p.df)

# fit pooled OLS
m1 <- lm(y ~ x, data = p.df)
# fit same model with plm (needed for clustering)
pm1 <- plm(y ~ x, data = p.df, model = "pooling")

coeftest(m1)
waldtest(m1)


coeftest(pm1)
waldtest(pm1)

# compute Stata like df-adjustment
G <- length(unique(p.df$firmid))
N <- length(p.df$firmid)
dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual

# display with cluster VCE and df-adjustment
firm_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "group", adjust = T)
coeftest(pm1, vcov = firm_c_vcov)

# test mastering metrics data, ch 5.2, DD of MLDA
# http://masteringmetrics.com/resources/
setwd("C:/Users/Stephen/Desktop/stata/mastering_metrics/deaths_ch5_DD")

library(multiwayvcov)
library(readstata13)
library(plm)
library(lmtest)
mlda <- read.dta13("deaths.dta")
dim(mlda)

# convert to panel df
# http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
p_mlda <- pdata.frame(mlda, index = c("state", "year"), drop.index = F, row.names = T)

# trim data to dtype = 1 (all deaths) and year <= 1983 and agegr = 2 (per MM)
unique(p_mlda$dtype)
p_mlda$year <- as.numeric(as.character(p_mlda$year))
p_mlda <- filter(p_mlda, dtype == "all", agegr == "18-20 yrs", year <= 1983)

# estimate simple pooled ols 
# fit pooled OLS
m1 <- lm(mrate ~ legal + factor(state) + factor(year), data = p_mlda)
coeftest(m1) # this matches stata without clustered SE on state

# fit same model with plm (needed for clustering)
pm1 <- plm(mrate ~ legal + factor(state) + factor(year), data = p_mlda, model = "pooling")
coeftest(pm1) # this matches as well

# attempt same model using plm fixed effects by specifying "within" as model
# can't get it to work properly, trying multiple different specifications
# here are notes: http://www.princeton.edu/~otorres/Panel101R.pdf
pm2 <- plm(mrate ~ legal, data = p_mlda, model = "within")
coeftest(pm2)

# for clustered SE's in R: 
# http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html
m1.vcovCL <- cluster.vcov(m1, p_mlda$state)

# compare OLS SE with clustered SE
# it works, same output as MM in stata
coeftest(m1)
coeftest(m1, m1.vcovCL)


