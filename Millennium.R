
unique(DGRsD$SWC_Type)
DGRsD %>% select(Climate) %>% unique() %>% arrange(Climate)

# detect threshold of gwc, vwc, and Pm
results %>% filter(var_swc == "GWC" & poly_c < 0) %>% 
  select (poly_a, poly_b, poly_c) %>% summarise(a = mean(poly_a), b = mean(poly_b), c = mean(poly_c)) -> sum

plot(DGRsD_SWC$RS_Norm ~ DGRsD_SWC$SWC
     , main = ""
     , xlab = ""
     , ylab = ""
     , pch = 16
     , col = "gray" )

# add SLR curve
curve( sum$a + sum$b * x + sum$c * x^2, min(DGRsD_SWC$SWC), max(DGRsD_SWC$SWC), col = "black", lty = 3, lwd = 2, add = T )

mtext(side = 1, text = paste0( var_swc, " ( % )" ), line = 1.75, cex=1, outer = F)
mtext(side = 2, text = expression(Soil~respiration~"("~g~C~m^{-2}~day^{-1}~")"), line = 2.0, cex=1.0, outer = F, las = 0)


#*****************************************************************************************************************
# plot all models curve in one figure, not used
#*****************************************************************************************************************

# x <- 1:80
# 
# plot( 0.2*x ~ x
#       , cex = 1.15
#       # ,xlab = ""
#       # ,ylab = "" 
#       # ,xaxt = "n"
#       # ,yaxt = "n"
#       , col = "white"
#       , las = 1
#       , xlim = c(0,80)
#       , ylim = c(0,20)
#       , type = "l"
# )
# 
# results %>% filter( p_b <= 0.05 ) %>% select(first_a, first_b, p_b) %>% mutate(ID = row_number()) -> subdata
# 
# for (i in 1:nrow(subdata)) {
#   curve( subdata$first_a[i] + subdata$first_b[i] * x, 0, 80, col = "gray", lty = 1, lwd = 1, add = T )
# }
# 
# # p2 - swc vs Rs
# plot( 0.2*x ~ x
#       , cex = 1.15
#       # ,xlab = ""
#       # ,ylab = "" 
#       # ,xaxt = "n"
#       # ,yaxt = "n"
#       , col = "white"
#       , las = 1
#       , xlim = c(0,80)
#       , ylim = c(0,20)
#       , type = "l"
# )
# 
# results %>% filter( p_pm_b <= 0.05 ) %>% select(first_pm_a, first_pm_b, p_pm_b) %>% mutate(ID = row_number()) -> subdata
# 
# for (i in 1:nrow(subdata)) {
#   curve( subdata$first_pm_a[i] + subdata$first_pm_b[i] * x, 0, 80, col = "gray", lty = 1, lwd = 1, add = T )
# }
# 
# # p3 - swc vs Rs polynomial curve
# plot( 0.2*x ~ x
#       , cex = 1.15
#       # ,xlab = ""
#       # ,ylab = "" 
#       # ,xaxt = "n"
#       # ,yaxt = "n"
#       , col = "white"
#       , las = 1
#       , xlim = c(0,80)
#       , ylim = c(0,20)
#       , type = "l" )
# 
# results %>% filter( p_c <= 0.05 ) %>% select(poly_a, poly_b, poly_c, p_c) %>% mutate(ID = row_number()) -> subdata
# 
# for (i in 1:nrow(subdata)) {
#   curve( subdata$poly_a[i] + subdata$poly_b[i] * x + subdata$poly_c[i]^2, 0, 80, col = "gray", lty = 1, lwd = 1, add = T )
# }


#*****************************************************************************************************************
# Modeling and model parameters summary
#*****************************************************************************************************************
# i = 289
# model_sum <- function () {
#   
#   var_study <- unique (DGRsD_SWC$Study_number) %>% sort()
#   results <- data.frame()
#   
#   for (i in c(1:288, 290:333)) {
#     # for (i in 1:length(var_study)) {
#     sub_study <- subset(DGRsD_SWC, Study_number == var_study[i])
#     var_site <- unique (sub_study$SiteID)
#     
#     for (j in 1:length(var_site)) {
#       
#       sub_site <- subset(sub_study, SiteID == var_site[j])
#       var_swc <- sub_site$SWC_Type[1]
#       
#       if (nrow(sub_site)<6) {next} else {
#         # first order lm model
#         first_lm <- lm(RS_Norm ~ SWC, data = sub_site)
#         first_a <- summary(first_lm)$coefficients[1,1] %>% round(6)
#         first_b <- summary(first_lm)$coefficients[2,1] %>% round(6)
#         p_b <- summary(first_lm)$coefficients[2,4]%>% round(6)
#         first_R2 <- summary(first_lm)$r.squared %>% round(6)
#         
#         # polynomial equation
#         poly_lm <- lm(RS_Norm ~ SWC + I(SWC^2), data = sub_site)
#         # poly_lm <- lm(RS_Norm ~ poly(SWC,2) , data = sub_site)
#         poly_a <- summary(poly_lm)$coefficients[1,1] %>% round(6)
#         poly_b <- summary(poly_lm)$coefficients[2,1] %>% round(6)
#         poly_c <- summary(poly_lm)$coefficients[3,1] %>% round(6)
#         p_poly_b <- summary(poly_lm)$coefficients[2,4] %>% round(6)
#         p_c <- summary(poly_lm)$coefficients[3,4] %>% round(6)
#         poly_R2 <- summary(poly_lm)$r.squared %>% round(6)
#         obs <- nrow(sub_site)
#         
#         # first order lm model of SWC ~ Pm
#         first_pm <- lm(RS_Norm ~ Pm_Del, data = sub_site)
#         first_pm_a <- summary(first_pm)$coefficients[1,1] %>% round(6)
#         first_pm_b <- summary(first_pm)$coefficients[2,1] %>% round(6)
#         p_pm_b <- summary(first_pm)$coefficients[2,4]%>% round(6)
#         first_pm_R2 <- summary(first_pm)$r.squared %>% round(6)
#         
#         # polynomial equation of SWC~Pm
#         # poly_pm <- try(lm(RS_Norm ~ poly(Pm_Del,2) , data = sub_site))
#         poly_pm <- try(lm(RS_Norm ~ Pm_Del + I(Pm_Del^2), data = sub_site))
#         pm_c_try <- try(summary(poly_pm)$coefficients[3,1])
#         
#         if (is(poly_pm, "try-error") | is(pm_c_try, "try-error") ) {
#           poly_pm_a <- NA
#           poly_pm_b <- NA
#           poly_pm_c <- NA
#           p_poly_pm_b <- NA
#           p_pm_c <- NA
#           poly_pm_R2 <- NA } 
#         else {
#           poly_pm_a <- summary(poly_pm)$coefficients[1,1] %>% round(6)
#           poly_pm_b <- summary(poly_pm)$coefficients[2,1] %>% round(6)
#           poly_pm_c <- summary(poly_pm)$coefficients[3,1] %>% round(6)
#           p_poly_pm_b <- summary(poly_pm)$coefficients[2,4] %>% round(6)
#           p_pm_c <- summary(poly_pm)$coefficients[3,4] %>% round(6)
#           poly_pm_R2 <- summary(poly_pm)$r.squared %>% round(6) }
#         
#         lm_sum = data.frame(sub_site$Study_number[1], var_site[j], var_swc
#                             , first_a, first_b, p_b, first_R2
#                             , poly_a, poly_b, p_poly_b, poly_c, p_c, poly_R2
#                             , first_pm_a, first_pm_b, p_pm_b, first_pm_R2
#                             , poly_pm_a, poly_pm_b, p_poly_pm_b, poly_pm_c, p_pm_c, poly_pm_R2
#                             , obs)
#         
#         results = rbind(results, lm_sum)
#       }
#       print(paste0("*****", i, "-----", j))
#     }
#   }
#   return(results)
# }


# var_study[289]
# var_study[296]



#*****************************************************************************************************************
# 5 Ways to Do 2-D Histograms in R
#*****************************************************************************************************************

# (Plus 1 Bonus Figure)
#
# Myles Harrison
# http://www.everydayanalytics.ca

# Color housekeeping
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

# Create normally distributed data for plotting
x <- rnorm(mean=1.5, 5000)
y <- rnorm(mean=1.6, 5000)
df <- data.frame(x,y)

# Plot
plot(df, pch=16, col='black', cex=0.5)

##### OPTION 1: hexbin from package 'hexbin' #######
library(hexbin)
# Create hexbin object and plot
h <- hexbin(df)
plot(h)
plot(h, colramp=rf)

# hexbinplot function allows greater flexibility
hexbinplot(y~x, data=df, colramp=rf)

# Setting max and mins
hexbinplot(y~x, data=df, colramp=rf, mincnt=2, maxcnt=60)

# Scaling of legend - must provide both trans and inv functions
hexbinplot(y~x, data=df, colramp=rf, trans=log, inv=exp)

##### OPTION 2: hist2d from package 'gplots' #######
install.packages("gplots")
library(gplots)

# Default call
h2 <- hist2d(df)

# Coarser binsizing and add colouring
h2 <- hist2d(df, nbins=25, col=r)

# Scaling with log as before
h2 <- hist2d(df, nbins=25, col=r, FUN=function(x) log(length(x)))

##### OPTION 3: stat_bin2d from package 'ggplot' #######
library(ggplot2)

# Default call (as object)
p <- ggplot(df, aes(x,y))
h3 <- p + stat_bin2d()
h3

# Default call (using qplot)
qplot(x,y,data=df, geom='bin2d')

# Add colouring and change bins
h3 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r)
h3

# Log scaling
h3 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r, trans="log")
h3

##### OPTION 4: kde2d from package 'MASS' #######
# Not a true heatmap as interpolated (kernel density estimation)
library(MASS)

# Default call 
k <- kde2d(df$x, df$y)
image(k, col=r)

# Adjust binning (interpolate - can be computationally intensive for large datasets)
k <- kde2d(df$x, df$y, n=200)
image(k, col=r)

##### OPTION 5: The Hard Way (DIY) #######
# http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data

nbins <- 25
x.bin <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins)
y.bin <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins)

freq <-  as.data.frame(table(findInterval(df[,1], x.bin),findInterval(df[,2], y.bin)))
freq[,1] <- as.numeric(freq[,1])
freq[,2] <- as.numeric(freq[,2])

freq2D <- diag(nbins)*0
freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]

# Normal
image(x.bin, y.bin, freq2D, col=r)

# Log
image(x.bin, y.bin, log(freq2D), col=r)


##### Addendum: 2D Histogram + 1D on sides (from Computational ActSci w R) #######
#http://books.google.ca/books?id=YWcLBAAAQBAJ&pg=PA60&lpg=PA60&dq=kde2d+log&source=bl&ots=7AB-RAoMqY&sig=gFaHSoQCoGMXrR9BTaLOdCs198U&hl=en&sa=X&ei=8mQDVPqtMsi4ggSRnILQDw&redir_esc=y#v=onepage&q=kde2d%20log&f=false

h1 <- hist(df$x, breaks=25, plot=F)
h2 <- hist(df$y, breaks=25, plot=F)
top <- max(h1$counts, h2$counts)
k <- kde2d(df$x, df$y, n=25)

# margins
oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
image(k, col=r) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)
