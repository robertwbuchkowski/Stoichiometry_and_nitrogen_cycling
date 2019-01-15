association_function <-function(qc2=qc2){par(mfrow=c(2,2), mar=c(1,1,1,1))
  
  #Global variables/vectors
  Sclist = as.character(seq(1,8,1))
  pos = cbind(c(0.25, 0.25, 0.25, 0.7, 0.7, 0.7),
              c(0.3, 0.6, 0.9, 0.3, 0.6, 0.9))
  
  nvec = c("Herbivore", "Inorganic N", "Detritus N", "Microbes","Micobi- \n detritivore", "Detritus C")
  
  M = array(0, dim=c(6,6,8))
  colnames(M)= rownames(M) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
  tocheck = expand.grid(colnames(M), colnames(M))
  tocheck = tocheck[tocheck[,1] != tocheck[,2],]
  rownames(tocheck) = seq(1,30,1)
  Mcur = matrix(0, nrow=6, ncol=6)
  
  colnames(Mcur)= rownames(Mcur) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
  BF1 = 0.28
  Mcur["H", "Dn"] = BF1
  Mcur["Dn","H"] = -BF1
  Mcur["M", "Dc"] = -BF1
  Mcur["Dc","M"] = BF1
  
  # Iorganic N
  for(j in 1:8){
    qc2_2 = subset(qc2, Scenario==Sclist[j])
    
    for(i in 1:30){
      
      A = ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dIN")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dIN")]<0,"-", "0"))
      B = ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dIN")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dIN")]<0,"-", "0"))
      
      testtable = table(A,B)
      
      # print(testtable)
      
      if(sum(dim(testtable))==2 & !(0 %in% colnames(testtable) | 0 %in% rownames(testtable))) {
        
        if(colnames(testtable) %in% rownames(testtable)) {
          M[tocheck[i,1],tocheck[i,2],j] = "same"
        }else{
          M[tocheck[i,1],tocheck[i,2],j] = "different"
        } 
        
      }
      
      if(sum(dim(testtable))==3) M[tocheck[i,1],tocheck[i,2],j] = "unrelated"
      
      if("-" %in% colnames(testtable) & "-" %in% rownames(testtable) &
         "+" %in% colnames(testtable) & "+" %in% rownames(testtable)){
        M[tocheck[i,1],tocheck[i,2],j] = ifelse(testtable["-", "-"]!=0 & testtable["+", "+"]!=0 & 
                                                  testtable["-", "+"]==0 & testtable["+", "-"]==0,
                                                "same", ifelse(testtable["-", "-"]==0 & testtable["+", "+"]==0 & 
                                                                 testtable["-", "+"]!=0 & testtable["+", "-"]!=0,
                                                               "different", "unrelated"))
        
        
      }
      # print(M[tocheck[i,1],tocheck[i,2],j])
    }
  }
  
  M2 = matrix(0, nrow=6, ncol=6)
  colnames(M2)= rownames(M2) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
  
  M3 = M2
  
  for(i in 1:30){
    
    if("same" %in% M[tocheck[i,1],tocheck[i,2],])  {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))==1)){
        M2[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))
      }else{M2[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"], sep="", collapse="/")}
    }
    if("different" %in% M[tocheck[i,1],tocheck[i,2],]) {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))==1)){
        M3[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))
      }else{M3[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"], sep="", collapse="/")}
    }
    
    
  }
  
  M2[upper.tri(M2)] =0
  M3[upper.tri(M3)] =0
  
  plotmat(M2, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          main = "Increasing Inorganic N", shadow.size = 0, arr.lcol = "black",
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2)
  
  plotmat(M3, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          shadow.size = 0, arr.lcol = "grey", 
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2, add=T)
  legend("topleft", legend="a", bty="n")
  
  
  #Omega
  
  for(j in 1:8){
    qc2_2 = subset(qc2, Scenario==Sclist[j])
    
    for(i in 1:30){
      
      A = ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dOmega")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dOmega")]<0,"-", "0"))
      B = ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dOmega")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dOmega")]<0,"-", "0"))
      
      testtable = table(A,B)
      
      # print(testtable)
      
      if(sum(dim(testtable))==2 & !(0 %in% colnames(testtable) | 0 %in% rownames(testtable))) {
        
        if(colnames(testtable) %in% rownames(testtable)) {
          M[tocheck[i,1],tocheck[i,2],j] = "same"
        }else{
          M[tocheck[i,1],tocheck[i,2],j] = "different"
        } 
        
      }
      
      if(sum(dim(testtable))==3) M[tocheck[i,1],tocheck[i,2],j] = "unrelated"
      
      if("-" %in% colnames(testtable) & "-" %in% rownames(testtable) &
         "+" %in% colnames(testtable) & "+" %in% rownames(testtable)){
        M[tocheck[i,1],tocheck[i,2],j] = ifelse(testtable["-", "-"]!=0 & testtable["+", "+"]!=0 & 
                                                  testtable["-", "+"]==0 & testtable["+", "-"]==0,
                                                "same", ifelse(testtable["-", "-"]==0 & testtable["+", "+"]==0 & 
                                                                 testtable["-", "+"]!=0 & testtable["+", "-"]!=0,
                                                               "different", "unrelated"))
        
        
      }
      # print(M[tocheck[i,1],tocheck[i,2],j])
    }
  }
  
  M2 = matrix(0, nrow=6, ncol=6)
  colnames(M2)= rownames(M2) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
  
  M3 = M2
  
  for(i in 1:30){
    
    if("same" %in% M[tocheck[i,1],tocheck[i,2],])  {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))==1)){
        M2[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))
      }else{M2[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"], sep="", collapse="/")}
    }
    if("different" %in% M[tocheck[i,1],tocheck[i,2],]) {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))==1)){
        M3[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))
      }else{M3[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"], sep="", collapse="/")}
    }
    
    
  }
  
  M2[upper.tri(M2)] =0
  M3[upper.tri(M3)] =0
  
  plotmat(M2, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          main = "Increasing Plant C:N", shadow.size = 0, arr.lcol = "black",
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2)
  
  plotmat(M3, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          shadow.size = 0, arr.lcol = "grey", 
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2, add=T)
  legend("topleft", legend="b", bty="n")
  
  # Mu Diagram
  for(j in 1:8){
    qc2_2 = subset(qc2, Scenario==Sclist[j])
    
    for(i in 1:30){
      
      A = ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dMu")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,1], "_dMu")]<0,"-", "0"))
      B = ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dMu")]>0,"+", ifelse(qc2_2[,paste0("d", tocheck[i,2], "_dMu")]<0,"-", "0"))
      
      testtable = table(A,B)
      
      # print(testtable)
      
      if(sum(dim(testtable))==2 & !(0 %in% colnames(testtable) | 0 %in% rownames(testtable))) {
        
        if(colnames(testtable) %in% rownames(testtable)) {
          M[tocheck[i,1],tocheck[i,2],j] = "same"
        }else{
          M[tocheck[i,1],tocheck[i,2],j] = "different"
        } 
        
      }
      
      if(sum(dim(testtable))==3) M[tocheck[i,1],tocheck[i,2],j] = "unrelated"
      
      if("-" %in% colnames(testtable) & "-" %in% rownames(testtable) &
         "+" %in% colnames(testtable) & "+" %in% rownames(testtable)){
        M[tocheck[i,1],tocheck[i,2],j] = ifelse(testtable["-", "-"]!=0 & testtable["+", "+"]!=0 & 
                                                  testtable["-", "+"]==0 & testtable["+", "-"]==0,
                                                "same", ifelse(testtable["-", "-"]==0 & testtable["+", "+"]==0 & 
                                                                 testtable["-", "+"]!=0 & testtable["+", "-"]!=0,
                                                               "different", "unrelated"))
        
        
      }
      # print(M[tocheck[i,1],tocheck[i,2],j])
    }
  }
  
  M2 = matrix(0, nrow=6, ncol=6)
  colnames(M2)= rownames(M2) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
  
  M3 = M2
  
  for(i in 1:30){
    
    if("same" %in% M[tocheck[i,1],tocheck[i,2],])  {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))==1)){
        M2[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"]))
      }else{M2[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="same"], sep="", collapse="/")}
    }
    if("different" %in% M[tocheck[i,1],tocheck[i,2],]) {
      if(all((diff(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))==1)){
        M3[tocheck[i,1],tocheck[i,2]] = paste0(min(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]),"-", max(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"]))
      }else{M3[tocheck[i,1],tocheck[i,2]] = paste(seq(1,8,1)[M[tocheck[i,1],tocheck[i,2],] =="different"], sep="", collapse="/")}
    }
    
    
  }
  
  M2[upper.tri(M2)] =0
  M3[upper.tri(M3)] =0
  
  plotmat(M2, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          main = "Increasing Microbial C:N", shadow.size = 0, arr.lcol = "black",
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2)
  
  plotmat(M3, pos = pos, name = nvec,
          lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
          box.size = 0.1, box.type = "circle", box.prop = 0.5,
          shadow.size = 0, arr.lcol = "grey", 
          arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
          box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
          txt.col ="black",
          box.col="white", lty = 2, add=T)
  legend("topleft", legend="c", bty="n")}