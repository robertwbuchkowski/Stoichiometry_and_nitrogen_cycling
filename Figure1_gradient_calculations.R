# Code to verify calculations over gradient

# WARNING: This code takes a significant amount of time to run
# WARNING: Each individual gradient file was 2GB, so the raw data are available upon request. They can also be generated using the Mathematica code provided.
# WARNING: Data output to create the supplemental figure is supplied 

require(diagram) # version 1.6.4
require(fmsb) # version 0.6.3
require(abind) # version 1.4-5
source("plotmat_RWB.R")
source("foodweb_allANDfoodweb_all_err.R")
source("association_function.R")
source("Figure1_gradient.R")
require(tidyverse) # version 1.2.1
require(ggpubr) # version 0.2

# Draw omega gradient trajectories ----------------------------------------------

perscenario = 1400000

omega = rbind(read_csv("Data/Sc_gradient_older/Sc1_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc1_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc2_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc2_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc3_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc3_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc4_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc4_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc5_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc5_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc6_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc6_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc7_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc7_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc8_omega_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc8_omega_gradient_1pt5_.csv", col_names = FALSE))
colnames(omega) = c("P", "M", "Dn", "Dc", "Iorg", "H", "Dv", "Omega", "r5", "phck5",
                    "Wnf5", "Wnh5", "Wnm5", "Wcf5", "Wch5", "Wcm5")
omega["Scenario"] = rep(c("1","2","3","4","5","6","7","8"), each=perscenario)
omega["GradID"] = rep(seq(1,dim(omega)[1]/100), each=100)

colnames(omega)

omegafinaldata = matrix(NA, nrow=length(unique(omega$GradID)), ncol=7)
omegafinaldatasd = matrix(NA, nrow=length(unique(omega$GradID)), ncol=7)
Scenariodata = rep(NA, length(unique(omega$GradID)))

length(unique(omega$GradID)) # Number of gradients to run

for(j in 1:length(unique(omega$GradID))){
  
  current = subset(omega, GradID == j & P > 0)
  
  if(dim(current)[1]>=10){
    secstorun = floor(seq(1,dim(current)[1], length=10))
    dtatsecs = matrix(NA, nrow= 9, ncol= 7)
    for(i in 1:9){
      dtatsecs[i,] = as.numeric((current[secstorun[i+1],1:7]-current[secstorun[i],1:7])/
                                  rep(as.numeric((current[secstorun[i+1],"Omega"]-current[secstorun[i],"Omega"])),7))
      
    }
    
    omegafinaldata[j,] = colMeans(dtatsecs)
    omegafinaldatasd[j,] = apply(dtatsecs, 2, sd)
    Scenariodata[j] = unique(current$Scenario)
  }
  
  if(j%%1000 ==0) print(paste("Done Gradient", j, "of",length(unique(omega$GradID))))
  
}

omegafinaldata = as.data.frame(omegafinaldata)
colnames(omegafinaldata) = paste0("d",colnames(current)[1:7],"_dOmega")
omegafinaldata["Scenario"] = Scenariodata
write_csv(omegafinaldata, "omegafinaldata.csv")

# Draw mu gradient trajectories ----------------------------------------------

perscenario = 1400000

mu = rbind(read_csv("Data/Sc_gradient_older/Sc1_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc1_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc2_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc2_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc3_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc3_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc4_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc4_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc5_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc5_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc6_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc6_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc7_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc7_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc8_mu_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc8_mu_gradient_1pt5_.csv", col_names = FALSE))
colnames(mu) = c("P", "M", "Dn", "Dc", "Iorg", "H", "Dv", "Mu", "r5", "phck5",
                    "Wnf5", "Wnh5", "Wnm5", "Wcf5", "Wch5", "Wcm5")
mu["Scenario"] = rep(c("1","2","3","4","5","6","7","8"), each=perscenario)
mu["GradID"] = rep(seq(1,dim(mu)[1]/100), each=100)

colnames(mu)

mufinaldata = matrix(NA, nrow=length(unique(mu$GradID)), ncol=7)
mufinaldatasd = matrix(NA, nrow=length(unique(mu$GradID)), ncol=7)
Scenariodata = rep(NA, length(unique(mu$GradID)))

length(unique(mu$GradID)) # Number of gradients to run

for(j in 1:length(unique(mu$GradID))){
  
  current = subset(mu, GradID == j & P > 0)
  
  if(dim(current)[1]>=10){
    secstorun = floor(seq(1,dim(current)[1], length=10))
    dtatsecs = matrix(NA, nrow= 9, ncol= 7)
    for(i in 1:9){
      dtatsecs[i,] = as.numeric((current[secstorun[i+1],1:7]-current[secstorun[i],1:7])/
                                  rep(as.numeric((current[secstorun[i+1],"Mu"]-current[secstorun[i],"Mu"])),7))
      
    }
    
    mufinaldata[j,] = colMeans(dtatsecs)
    mufinaldatasd[j,] = apply(dtatsecs, 2, sd)
    Scenariodata[j] = unique(current$Scenario)
  }
  if(j%%1000 ==0) print(paste("Done Gradient", j, "of",length(unique(omega$GradID))))
}

mufinaldata = as.data.frame(mufinaldata)
colnames(mufinaldata) = paste0("d",colnames(current)[1:7],"_dMu")
mufinaldata["Scenario"] = Scenariodata
write_csv(mufinaldata, "mufinaldata.csv")

# Draw IN gradient trajectories ----------------------------------------------

perscenario = 1400000

IN = rbind(read_csv("Data/Sc_gradient_older/Sc1_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc1_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc2_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc2_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc3_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc3_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc4_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc4_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc5_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc5_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc6_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc6_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc7_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc7_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_older/Sc8_IN_gradient_1pt5_.csv", col_names = FALSE),
              read_csv("Data/Sc_gradient_newer/Sc8_IN_gradient_1pt5_.csv", col_names = FALSE))
colnames(IN) = c("P", "M", "Dn", "Dc", "Iorg", "H", "Dv", "IN", "r5", "phck5",
                    "Wnf5", "Wnh5", "Wnm5", "Wcf5", "Wch5", "Wcm5")
IN["Scenario"] = rep(c("1","2","3","4","5","6","7","8"), each=perscenario)
IN["GradID"] = rep(seq(1,dim(IN)[1]/100), each=100)

colnames(IN)

INfinaldata = matrix(NA, nrow=length(unique(IN$GradID)), ncol=7)
INfinaldatasd = matrix(NA, nrow=length(unique(IN$GradID)), ncol=7)
Scenariodata = rep(NA, length(unique(IN$GradID)))

length(unique(IN$GradID)) # Number of gradients to run

for(j in 1:length(unique(IN$GradID))){
  
  current = subset(IN, GradID == j & P > 0)
  
  if(dim(current)[1]>=10){
    secstorun = floor(seq(1,dim(current)[1], length=10))
    dtatsecs = matrix(NA, nrow= 9, ncol= 7)
    for(i in 1:9){
      dtatsecs[i,] = as.numeric((current[secstorun[i+1],1:7]-current[secstorun[i],1:7])/
                                  rep(as.numeric((current[secstorun[i+1],"IN"]-current[secstorun[i],"IN"])),7))
      
    }
    
    INfinaldata[j,] = colMeans(dtatsecs)
    INfinaldatasd[j,] = apply(dtatsecs, 2, sd)
    Scenariodata[j] = unique(current$Scenario)
  }
  if(j%%1000 ==0) print(paste("Done Gradient", j, "of",length(unique(omega$GradID))))
}

INfinaldata = as.data.frame(INfinaldata)
colnames(INfinaldata) = paste0("d",colnames(current)[1:7],"_dIN")
INfinaldata["Scenario"] = Scenariodata
write_csv(INfinaldata, "INfinaldata.csv")


# Merge all three files together to create a plotting file ----

finaldata = cbind(omegafinaldata[,1:7], mufinaldata[,1:7], INfinaldata)
write_csv(finaldata, "gradient_data.csv")
