# Code for figures

all = cbind(rbind(qualitycontrol,qualitycontrol,qualitycontrol),rbind(INfinaldata[,-8],finaldata[,-8],mufinaldata[,-8]))
all["Gradient"] = rep(c("IN", "omega", "mu"),each=dim(qualitycontrol)[1])
all["Dvpos"] = ifelse(all$Dv > 0, "Increase", "Decrease/Zero")
all["Mpos"] = ifelse(all$M > 0, "Increase", "Decrease/Zero")
all["Dnpos"] = ifelse(all$Dn > 0, "Increase", "Decrease/Zero")
all["Iorgpos"] = ifelse(all$Iorg > 0, "Increase", "Decrease/Zero")
all["Hpos"] = ifelse(all$H > 0, "Increase", "Decrease/Zero")
all["Mpos2"] = ifelse(all$M > 0, "Increase", ifelse(all$M == 0,"Zero","Decrease"))
all["Dnpos2"] = ifelse(all$Dn > 0, "Increase", ifelse(all$Dn == 0,"Zero","Decrease"))
all["Dvpos2"] = ifelse(all$Dv > 0, "Increase", ifelse(all$Dv == 0,"Zero","Decrease"))
all["Iorgpos2"] = ifelse(all$Iorg > 0, "Increase", ifelse(all$Iorg == 0,"Zero","Decrease"))
all["Ppos2"] = ifelse(all$P > 0, "Increase", ifelse(all$P == 0,"Zero","Decrease"))

pos = cbind(c(0.1, 0.1, 0.25, 0.65, 0.5, 0.8),
            c(0.9, 0.1, 0.6, 0.7, 0.4, 0.1))

Mcur = matrix(nrow = 6, ncol=6, byrow = T, data=NA)
Mcur[2,1] = 0
Mcur[3,2] = -0.07
Mcur[2,6] = -0.1
Mcur[6,4] = -0.4
Mcur[4,6] = 0.05
Mcur[4,1] = -0.2
Mcur[5,4] = 0
Mcur[4,3] = -0.4
Mcur[5,4] = 0.15
Mcur[2,5] = 0.05

col = Mcur
col[] = "black"

col[2,1] = col[2,5]= col[2,6]= "purple"
col[4,1] = col[4,5]= col[4,6]=col[3,6]= "red"


# Get the food web fluxes
foodweb <- function(selecteddata){
  
  M = matrix(nrow = 6, ncol=6, byrow = T, data=0)
  colnames(M) = c("H", "N", "P", "D", "M","F")
  rownames(M) = c("H", "N", "P", "D", "M","F")
  
  M["D", "H"] = round(mean(selecteddata$th*selecteddata$Hstar + 
                             (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
  
  M["N", "H"] = round(mean(selecteddata$Wnh*selecteddata$Hstar))
  
  M["H", "P"] = round(mean(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
  
  M["D", "P"] = round(mean(selecteddata$tp*selecteddata$Pstar))
  
  M["P", "N"] = round(mean(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar))
  
  M["M", "N"] = round(mean(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar))
  
  M["N", "M"] = round(mean(selecteddata$Wnm*selecteddata$Mstar))
  
  M["M", "D"] = round(mean(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar))
  
  M["D", "M"] = round(mean(selecteddata$tm*selecteddata$Mstar))
  
  M["F", "D"] = round(mean(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar))
  
  M["D", "F"] = round(mean(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar))
  
  M["F", "M"] = round(mean(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)) # M to F
  
  M["N", "F"] = round(mean(selecteddata$Wnf*selecteddata$Dvstar)) # F to N
  
  return(M)
  
}

#Middle Quantile
foodweb <- function(selecteddata){
  
  M = matrix(nrow = 6, ncol=6, byrow = T, data=0)
  colnames(M) = c("H", "N", "P", "D", "M","F")
  rownames(M) = c("H", "N", "P", "D", "M","F")
  
  M["D", "H"] = paste0(round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),
                       "-",round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4]))
  
  M["N", "H"] = paste0(round(quantile(selecteddata$Wnh*selecteddata$Hstar)[2]),
                       "-",round(quantile(selecteddata$Wnh*selecteddata$Hstar)[4]))
  
  M["H", "P"] = paste0(round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),
                       "-",round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4]))
  
  M["D", "P"] = paste0(round(quantile(selecteddata$tp*selecteddata$Pstar)[2]),
                       "-",round(quantile(selecteddata$tp*selecteddata$Pstar)[4]))
  
  M["P", "N"] = paste0(round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[2]),
                       "-",round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[4]))
  
  M["M", "N"] = paste0(round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[2]),
                       "-",round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[4]))
  
  M["N", "M"] = paste0(round(quantile(selecteddata$Wnm*selecteddata$Mstar)[2]),
                       "-",round(quantile(selecteddata$Wnm*selecteddata$Mstar)[4]))
  
  M["M", "D"] = paste0(round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[2]),
                       "-",round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[4]))
  
  M["D", "M"] = paste0(round(quantile(selecteddata$tm*selecteddata$Mstar)[2]),
                       "-",round(quantile(selecteddata$tm*selecteddata$Mstar)[4]))
  
  M["F", "D"] = paste0(round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[2]),
                       "-",round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[4]))
  
  M["D", "F"] = paste0(round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),
                       "-",round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4]))
  
  M["F", "M"] = paste0(round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),
                       "-",round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4]))
  
  M["N", "F"] = paste0(round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[2]),
                       "-",round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[4]))
  
  return(M)
  
}

# Median -> line width

foodweb2 <- function(selecteddata){
  
  M = matrix(nrow = 6, ncol=6, byrow = T, data=0)
  colnames(M) = c("H", "N", "P", "D", "M","F")
  rownames(M) = c("H", "N", "P", "D", "M","F")
  
  M["D", "H"] = round(median(selecteddata$th*selecteddata$Hstar + 
                               (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
  
  M["N", "H"] = round(median(selecteddata$Wnh*selecteddata$Hstar))
  
  M["H", "P"] = round(median(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
  
  M["D", "P"] = round(median(selecteddata$tp*selecteddata$Pstar))
  
  M["P", "N"] = round(median(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar))
  
  M["M", "N"] = round(median(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar))
  
  M["N", "M"] = round(median(selecteddata$Wnm*selecteddata$Mstar))
  
  M["M", "D"] = round(median(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar))
  
  M["D", "M"] = round(median(selecteddata$tm*selecteddata$Mstar))
  
  M["F", "D"] = round(median(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar))
  
  M["D", "F"] = round(median(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar))
  
  M["F", "M"] = round(median(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)) # M to F
  
  M["N", "F"] = round(median(selecteddata$Wnf*selecteddata$Dvstar)) # F to N
  
  return(M)
  
}

#Middle Quantile
foodweb3 <- function(selecteddata){
  
  M = matrix(nrow = 6, ncol=6, byrow = T, data=0)
  colnames(M) = c("H", "N", "P", "D", "M","F")
  rownames(M) = c("H", "N", "P", "D", "M","F")
  if(dim(selecteddata)[1]>2){
    M["D", "H"] = paste0(round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),
                         "-",round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[3]),
                         "-",round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4]))
    
    M["N", "H"] = paste0(round(quantile(selecteddata$Wnh*selecteddata$Hstar)[2]),
                         "-",round(quantile(selecteddata$Wnh*selecteddata$Hstar)[3]),
                         "-",round(quantile(selecteddata$Wnh*selecteddata$Hstar)[4]))
    
    M["H", "P"] = paste0(round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),
                         "-",round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[3]),
                         "-",round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4]))
    
    M["D", "P"] = paste0(round(quantile(selecteddata$tp*selecteddata$Pstar)[2]),
                         "-",round(quantile(selecteddata$tp*selecteddata$Pstar)[3]),
                         "-",round(quantile(selecteddata$tp*selecteddata$Pstar)[4]))
    
    M["P", "N"] = paste0(round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[2]),
                         "-",round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[3]),
                         "-",round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[4]))
    
    M["M", "N"] = paste0(round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[2]),
                         "-",round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[3]),
                         "-",round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[4]))
    
    M["N", "M"] = paste0(round(quantile(selecteddata$Wnm*selecteddata$Mstar)[2]),
                         "-",round(quantile(selecteddata$Wnm*selecteddata$Mstar)[3]),
                         "-",round(quantile(selecteddata$Wnm*selecteddata$Mstar)[4]))
    
    M["M", "D"] = paste0(round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[2]),
                         "-",round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[3]),
                         "-",round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[4]))
    
    M["D", "M"] = paste0(round(quantile(selecteddata$tm*selecteddata$Mstar)[2]),
                         "-",round(quantile(selecteddata$tm*selecteddata$Mstar)[3]),
                         "-",round(quantile(selecteddata$tm*selecteddata$Mstar)[4]))
    
    M["F", "D"] = paste0(round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[2]),
                         "-",round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[3]),
                         "-",round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[4]))
    
    M["D", "F"] = paste0(round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),
                         "-",round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[3]),
                         "-",round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4]))
    
    M["F", "M"] = paste0(round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),
                         "-",round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[3]),
                         "-",round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4]))
    
    M["N", "F"] = paste0(round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[2]),
                         "-",round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[3]),
                         "-",round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[4]))
  }
  if(dim(selecteddata)[1]<=2){
    M["D", "H"] = round(median(selecteddata$th*selecteddata$Hstar + 
                                 (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
    
    M["N", "H"] = round(median(selecteddata$Wnh*selecteddata$Hstar))
    
    M["H", "P"] = round(median(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
    
    M["D", "P"] = round(median(selecteddata$tp*selecteddata$Pstar))
    
    M["P", "N"] = round(median(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar))
    
    M["M", "N"] = round(median(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar))
    
    M["N", "M"] = round(median(selecteddata$Wnm*selecteddata$Mstar))
    
    M["M", "D"] = round(median(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar))
    
    M["D", "M"] = round(median(selecteddata$tm*selecteddata$Mstar))
    
    M["F", "D"] = round(median(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar))
    
    M["D", "F"] = round(median(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar))
    
    M["F", "M"] = round(median(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)) # M to F
    
    M["N", "F"] = round(median(selecteddata$Wnf*selecteddata$Dvstar)) # F to N
  }
  return(M)
  
}

#Median-IQR Quantile
foodweb4 <- function(selecteddata){
  
  M = matrix(nrow = 6, ncol=6, byrow = T, data=0)
  colnames(M) = c("H", "N", "P", "D", "M","F")
  rownames(M) = c("H", "N", "P", "D", "M","F")
  if(dim(selecteddata)[1]>2){
    M["D", "H"] = paste0(round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[3]),
                         " (",round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4])-
                           round(quantile(selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),")")
    
    M["N", "H"] = paste0(round(quantile(selecteddata$Wnh*selecteddata$Hstar)[3]),
                         " (",round(quantile(selecteddata$Wnh*selecteddata$Hstar)[4])-
                           round(quantile(selecteddata$Wnh*selecteddata$Hstar)[2]),")")
    
    M["H", "P"] = paste0(round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[3]),
                         " (",round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[4])-
                           round(quantile(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar)[2]),")")
    
    M["D", "P"] = paste0(round(quantile(selecteddata$tp*selecteddata$Pstar)[3]),
                         " (",round(quantile(selecteddata$tp*selecteddata$Pstar)[4])-
                           round(quantile(selecteddata$tp*selecteddata$Pstar)[2]),")")
    
    M["P", "N"] = paste0(round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[3]),
                         " (",round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[4])-
                           round(quantile(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar)[2]),")")
    
    M["M", "N"] = paste0(round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[3]),
                         " (",round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[4])-
                           round(quantile(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar)[2]),")")
    
    M["N", "M"] = paste0(round(quantile(selecteddata$Wnm*selecteddata$Mstar)[3]),
                         " (",round(quantile(selecteddata$Wnm*selecteddata$Mstar)[4])-
                           round(quantile(selecteddata$Wnm*selecteddata$Mstar)[2]),")")
    
    M["M", "D"] = paste0(round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[3]),
                         " (",round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[4])-
                           round(quantile(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar)[2]),")")
    
    M["D", "M"] = paste0(round(quantile(selecteddata$tm*selecteddata$Mstar)[3]),
                         " (",round(quantile(selecteddata$tm*selecteddata$Mstar)[4])-
                           round(quantile(selecteddata$tm*selecteddata$Mstar)[2]),")")
    
    M["F", "D"] = paste0(round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[3]),
                         " (",round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[4]),
                         round(quantile(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar)[2]),")")
    
    M["D", "F"] = paste0(round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[3]),
                         " (",round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4])-
                           round(quantile(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),")")
    
    M["F", "M"] = paste0(round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[3]),
                         " (",round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[4])-
                           round(quantile(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)[2]),")")
    
    M["N", "F"] = paste0(round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[3]),
                         " (",round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[4])-
                           round(quantile(selecteddata$Wnf*selecteddata$Dvstar)[2]),")")
  }
  if(dim(selecteddata)[1]<=2){
    M["D", "H"] = round(median(selecteddata$th*selecteddata$Hstar + 
                                 (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
    
    M["N", "H"] = round(median(selecteddata$Wnh*selecteddata$Hstar))
    
    M["H", "P"] = round(median(selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar))
    
    M["D", "P"] = round(median(selecteddata$tp*selecteddata$Pstar))
    
    M["P", "N"] = round(median(selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar))
    
    M["M", "N"] = round(median(selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar))
    
    M["N", "M"] = round(median(selecteddata$Wnm*selecteddata$Mstar))
    
    M["M", "D"] = round(median(selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar))
    
    M["D", "M"] = round(median(selecteddata$tm*selecteddata$Mstar))
    
    M["F", "D"] = round(median(selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar))
    
    M["D", "F"] = round(median(selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar))
    
    M["F", "M"] = round(median(selecteddata$ef*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar)) # M to F
    
    M["N", "F"] = round(median(selecteddata$Wnf*selecteddata$Dvstar)) # F to N
  }
  return(M)
  
}

col = Mcur
col[] = "black"

col[2,1] = col[2,5]= col[2,6]= "purple"
col[4,1] = col[4,5]= col[4,6]=col[3,6]= "red"

Marrpos = Mcur
Marrpos[]=0.5
Marrpos[2,1] = 0.3
Marrpos[4,6] = 0.4
Marrpos[6,4] = 0.5
Marrpos[3,2] = 0.6
Marrpos[4,5] = 0.45
# Mdtext = Mcur
# Mdtext[which(Mcur!="NA")]=0.3
# Mdtext[2,1] = 0
# Mdtext[4,1] = 0

selsc = c("1", "2", "3", "4", "5", "6", "7", "8")
plotname =c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited",
            "H C-limited", "F C-limited", "H & F C-limited")
seldir = c("Increase", "Decrease","Zero")
colmat = matrix(c("red", "white", "white", "white", "red", "red",
                  "red", "white", "white", "white", "grey", "red",
                  "grey", "white", "white", "white", "grey", "red",
                  "red", "white", "white", "white", "grey", "grey",
                  "grey", "white", "white", "white", "grey", "grey",
                  "grey", "white", "white", "white", "red", "red",
                  "red", "white", "white", "white", "red", "grey",
                  "grey", "white", "white", "white", "red", "grey"), 
                nrow=6,ncol=8)

circlesign=c("(+)", "(-)", "(0)")

pdf("foodwebmotif_examples_omega_all.pdf", width=11, height=6.5)
par(mfrow=c(2,3))

for(h in 1:8){
  for(i in 1:3){
    for(j in 1:3){
      for(k in 1:3){
        for(m in 1:3){
          for(n in 1:3){
            selecteddata = subset(all, Scenario==selsc[h] & Gradient=="omega" &
                                    Mpos2 == seldir[i] & Dvpos2==seldir[j] & Dnpos2==seldir[k] &
                                    Iorgpos2==seldir[m] & Ppos2==seldir[n])
            
            
            
            circlelabel = c(paste("H", circlesign[m]), 
                            paste("N", circlesign[m]), 
                            paste("P", circlesign[n]), 
                            paste("D", circlesign[k]), 
                            paste("M", circlesign[i]),
                            paste("F", circlesign[j]))
            
            #print(dim(selecteddata))
            if(dim(selecteddata)[1]>0){
              
              M = foodweb4(selecteddata = selecteddata)
              Mlwd = foodweb2(selecteddata = selecteddata)
              Mlwd = Mlwd/(max(Mlwd))*5
              Mlwd[which(M!="0"& Mlwd==0)]=0.1
              pp <- plotmat(M, pos = pos, name = circlelabel,
                            lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
                            box.size = 0.1, box.type = "circle", box.prop = 0.5,
                            main = paste0(plotname[h],": Motif ",i,".",k,".",j, ".",m, ".",n), shadow.size = 0, arr.lcol = col,
                            arr.length = 0.1, arr.lwd=Mlwd,arr.col=col,
                            box.lcol=c("green", "black", "green","brown", "brown", "brown"),
                            box.col=colmat[,h], arr.pos = Marrpos)
              
              text(0.88,0.88, labels=paste("N =", dim(selecteddata)[1]))
            }
          }
        } 
      }
    }
  }
}
dev.off()


pdf("foodwebmotif_examples_mu_all.pdf", width=11, height=6.5)
par(mfrow=c(2,3))

for(h in 1:8){
  for(i in 1:3){
    for(j in 1:3){
      for(k in 1:3){
        for(m in 1:3){
          for(n in 1:3){
            selecteddata = subset(all, Scenario==selsc[h] & Gradient=="mu" &
                                    Mpos2 == seldir[i] & Dvpos2==seldir[j] & Dnpos2==seldir[k] &
                                    Iorgpos2==seldir[m] & Ppos2==seldir[n])
            
            
            
            circlelabel = c(paste("H", circlesign[m]), 
                            paste("N", circlesign[m]), 
                            paste("P", circlesign[n]), 
                            paste("D", circlesign[k]), 
                            paste("M", circlesign[i]),
                            paste("F", circlesign[j]))
            
            if(dim(selecteddata)[1]>0){
              
              M = foodweb4(selecteddata = selecteddata)
              Mlwd = foodweb2(selecteddata = selecteddata)
              Mlwd = Mlwd/(max(Mlwd))*5
              Mlwd[which(M!="0"& Mlwd==0)]=0.1
              pp <- plotmat(M, pos = pos, name = circlelabel,
                            lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
                            box.size = 0.1, box.type = "circle", box.prop = 0.5,
                            main = paste0(plotname[h],": Motif ",i,".",k,".",j, ".",m, ".",n), shadow.size = 0, arr.lcol = col,
                            arr.length = 0.1, arr.lwd=Mlwd,arr.col=col,
                            box.lcol=c("green", "black", "green","brown", "brown", "brown"),
                            box.col=colmat[,h], arr.pos = Marrpos)
              
              text(0.88,0.88, labels=paste("N =", dim(selecteddata)[1]))
            }
          }
        } 
      }
    }
  }
}
dev.off()


pdf("foodwebmotif_examples_IN_all.pdf", width=11, height=6.5)
par(mfrow=c(2,3))

for(h in 1:8){
  for(i in 1:3){
    for(j in 1:3){
      for(k in 1:3){
        for(m in 1:3){
          for(n in 1:3){
            selecteddata = subset(all, Scenario==selsc[h] & Gradient=="IN" &
                                    Mpos2 == seldir[i] & Dvpos2==seldir[j] & Dnpos2==seldir[k] &
                                    Iorgpos2==seldir[m] & Ppos2==seldir[n])
            
            
            
            circlelabel = c(paste("H", circlesign[m]), 
                            paste("N", circlesign[m]), 
                            paste("P", circlesign[n]), 
                            paste("D", circlesign[k]), 
                            paste("M", circlesign[i]),
                            paste("F", circlesign[j]))
            
            if(dim(selecteddata)[1]>0){
              
              M = foodweb4(selecteddata = selecteddata)
              Mlwd = foodweb2(selecteddata = selecteddata)
              Mlwd = Mlwd/(max(Mlwd))*5
              Mlwd[which(M!="0"& Mlwd==0)]=0.1
              pp <- plotmat(M, pos = pos, name = circlelabel,
                            lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
                            box.size = 0.1, box.type = "circle", box.prop = 0.5,
                            main = paste0(plotname[h],": Motif ",i,".",k,".",j, ".",m, ".",n), shadow.size = 0, arr.lcol = col,
                            arr.length = 0.1, arr.lwd=Mlwd,arr.col=col,
                            box.lcol=c("green", "black", "green","brown", "brown", "brown"),
                            box.col=colmat[,h], arr.pos = Marrpos)
              
              text(0.88,0.88, labels=paste("N =", dim(selecteddata)[1]))
            }
          }
        } 
      }
    }
  }
}
dev.off()
