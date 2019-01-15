# Functions to output the flow rate matrix for QCQA and analyzing food web properties

foodweb_all <- function(selecteddata){
  
  M = array(0, dim=c(7,7,dim(selecteddata)[1]))
  colnames(M) = c("H", "N", "P", "D", "M","F", "EX")
  rownames(M) = c("H", "N", "P", "D", "M","F", "EX")
  
  M["D", "H",] = selecteddata$th*selecteddata$Hstar + (1-selecteddata$eh)*selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar
  
  M["N", "H",] = selecteddata$Wnh*selecteddata$Hstar
  
  M["H", "P",] = selecteddata$Vhp*selecteddata$Pstar*selecteddata$Hstar
  
  M["D", "P",] = selecteddata$tp*selecteddata$Pstar
  
  M["P", "N",] = selecteddata$Vpn*selecteddata$Pstar*selecteddata$Iorgstar
  
  M["M", "N",] = selecteddata$Vmn*selecteddata$Mstar*selecteddata$Iorgstar
  
  M["N", "M",] = selecteddata$Wnm*selecteddata$Mstar
  
  M["M", "D",] = selecteddata$Vmd*selecteddata$Mstar*selecteddata$Dnstar
  
  M["D", "M",] = selecteddata$tm*selecteddata$Mstar
  
  M["F", "D",] = selecteddata$ef*selecteddata$Vfd*selecteddata$Dvstar*selecteddata$Dnstar
  
  M["D", "F",] = selecteddata$tf*selecteddata$Dvstar + (1-selecteddata$ef)*selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar
  
  M["F", "M",] = selecteddata$Vfm*selecteddata$Mstar*selecteddata$Dvstar # M to F
  
  M["N", "F",] = selecteddata$Wnf*selecteddata$Dvstar # F to N
  
  M["N", "EX",] = selecteddata$IN
  
  M["EX", "N",] = selecteddata$q*selecteddata$Iorgstar
  
  M["EX", "D",] = selecteddata$l*selecteddata$Dnstar
  
  return(M)
  
}

# Function for distance from mass balance given output from fooodweb_all

foodweb_all_err <- function(M, selecteddata){
  
  ttt = matrix(NA, nrow=dim(M)[3], ncol=7)
  
  ttt[,1]=M["N", "EX",] - M["EX", "N",] - M["EX", "D",]
  ttt[,2]=M["H", "P",] - M["D", "H",] - M["N", "H",]
  ttt[,3]=M["P", "N",] - M["D", "P",] - M["H", "P",]
  ttt[,4]=M["M", "D",] + M["M", "N",] - M["D", "M",] - M["N", "M",]- M["F", "M",]
  ttt[,5]=M["F", "D",] + M["F", "M",] - M["D", "F",] - M["N", "F",]
  ttt[,6]=M["N", "EX",] + M["N", "H",] + M["N", "M",] + M["N", "F",]- M["P", "N",] - M["M", "N",] - M["EX", "N",]
  ttt[,7]=M["D", "P",] + M["D", "H",] + M["D", "M",] + M["D", "F",]- M["F", "D",] - M["M", "D",] - M["EX", "D",]
  
  print(as.data.frame(rbind(c("Sys", "H", "P", "M", "F", "N", "D"),apply(ttt,2,FUN=max),
                            apply(ttt,2, FUN=which.max),
                            selecteddata$Scenario[apply(ttt,2, FUN=which.max)])))
  
  tttt = apply(ttt,1,FUN=max)
  
  return(tttt)
  
}