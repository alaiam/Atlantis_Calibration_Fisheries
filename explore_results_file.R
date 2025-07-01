a <- readRDS('/home/atlantis/psatlantismodel/Atlantis_Calibration_fishing/restart_file.results')
b <- read.csv('/home/atlantis/psatlantismodel/Atlantis_Calibration_fishing/calibration-parameters-complete.csv')
c <- read.csv('/home/atlantis/psatlantismodel/Atlantis_Calibration_fishing/calibration_settings.csv')
c <- c[c$calibrate ==T,]
par(mfrow = c(1,2))

lkh <- apply(FUN = as.numeric, X = a$trace$fitness, MARGIN = 1)
row.names(lkh) <- c$variable

apply(FUN = sum, X = lkh, MARGIN = 2)
col <- b$names
col[regexpr("mum", b$names)>0] <- "red"
col[regexpr("KDENR", b$names)>0] <- "darkred"
col[regexpr("BHalpha", b$names)>0] <- "darkorange"
col[regexpr("mQ", b$names)>0] <- "brown"
col[regexpr("mfc", b$names)>0] <- "green3"
col[regexpr("mFC", b$names)>0] <- "green3"


gen <- length((a$trace$best))

plot(0:gen, c(6000, apply(FUN = sum, X = lkh, MARGIN = 2)), type = "l", lwd = 2, xlab = "Generations", ylab = "Objective function value")
plot(0:gen, c(0, a$trace$par[,1]), type = "l", col = "brown", ylim = c(-1,1), lwd = 2, xlab = "Generations", ylab = "Parameter factor")




for (i in 1:length(b$names)){
  # if(col[i] == "brown")
  lines(0:gen, c(0, a$trace$par[,i]), type = "l", col = col[i], lwd = 2)
  
}

plot(density(a$trace$par[4,]))
param <- a$trace$par
colnames(param) <- b$names
exp(param)

# legend("bottomright", 
#        legend = c("Parameter 1 (mQ_SZ)", "Parameter 2 (mQ_MZ)", "Parameter 3 (mQ_LZ)"), 
#        col = c("darkorange", "red", "darkred"), 
#        lwd = 2, 
#        bty = "n")
biomass_index = 1:71
waa_index = 72:384
catch_index = 385:431
stability_index = 432:502


apply(FUN = sum, X = lkh[biomass_index,], MARGIN = 2)
apply(FUN = sum, X = lkh[waa_index,], MARGIN = 2)
apply(FUN = sum, X = lkh[catch_index,], MARGIN = 2)*2
apply(FUN = sum, X = lkh[stability_index,], MARGIN = 2)

apply(FUN = sum, X = lkh[biomass_index,], MARGIN = 2)+
apply(FUN = sum, X = lkh[waa_index,], MARGIN = 2)+
apply(FUN = sum, X = lkh[catch_index,], MARGIN = 2)*2+
apply(FUN = sum, X = lkh[stability_index,], MARGIN = 2)


