# Analysis of the Data Required for Buchkowski, Leroux, and Schmitz ECY18-0705.R2

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Code tested in:
# R version 3.5.2 (2018-12-20) -- "Eggshell Igloo"
# Copyright (C) 2018 The R Foundation for Statistical Computing
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Libraries required to conduct the analysis

require(diagram) # version 1.6.4
require(fmsb) # version 0.6.3
require(abind) # version 1.4-5
source("plotmat_RWB.R")
source("foodweb_allANDfoodweb_all_err.R")
source("association_function.R")
source("Figure1_gradient.R")
require(tidyverse) # version 1.2.1
require(ggpubr) # version 0.2

# Import Data ----

qualitycontrol2 = rbind(read_csv("Data/ParamEqbmDERout_Sc1_20185171859.csv", col_names = FALSE),
                        read_csv("Data/ParamEqbmDERout_Sc1_201810241027.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc2_2018518113.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc2_201810241255.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc3_20185181210.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc3_20181025926.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc4_2018518926.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc4_201810241042.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc5_20185181223.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc5_201810241154.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc6_20185181157.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc6_20181025514.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc7_20185181119.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc7_20181024205.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc8_20185182046.csv", col_names = FALSE),
                       read_csv("Data/ParamEqbmDERout_Sc8_201810241233.csv", col_names = FALSE))

# Data includes 4000 extra scenarios for best quality subsetting.

colnames(qualitycontrol2) = c("IN", "q", "l", "Vpn", "Vhp", "Vfd", "Vfm", "Vmd","Vmn", "rm", "rh", "rf", 
                             "exm", "exf", "exh", "tp", "th", 
                             "tm", "tf", "Eta", "Omega", "Mu", "Phi", "ef", "eh",
                             "positivesolutions", "aa", "r", "phck",  "Wnf", "Wnh", "Wnm", "Wcf", "Wch", "Wcm",
                             "Pstar", "Mstar", "Dnstar", "Dcstar", "Iorgstar", "Hstar", "Dvstar",
                             "dP_dOmega","dM_dOmega","dDn_dOmega","dDc_dOmega","dIorg_dOmega","dH_dOmega","dDv_dOmega",
                             "dP_dIN","dM_dIN","dDn_dIN","dDc_dIN","dIorg_dIN","dH_dIN","dDv_dIN",
                             "dP_dMu","dM_dMu","dDn_dMu","dDc_dMu","dIorg_dMu","dH_dMu","dDv_dMu",
                             "dP_dVhp","dM_dVhp","dDn_dVhp","dDc_dVhp","dIorg_dVhp","dH_dVhp","dDv_dVhp",
                             "dP_dVfd","dM_dVfd","dDn_dVfd","dDc_dVfd","dIorg_dVfd","dH_dVfd","dDv_dVfd",
                             "dP_dVfm","dM_dVfm","dDn_dVfm","dDc_dVfm","dIorg_dVfm","dH_dVfm","dDv_dVfm",
                             "dP_dVmd","dM_dVmd","dDn_dVmd","dDc_dVmd","dIorg_dVmd","dH_dVmd","dDv_dVmd",
                             "dP_dVmn","dM_dVmn","dDn_dVmn","dDc_dVmn","dIorg_dVmn","dH_dVmn","dDv_dVmn")
qualitycontrol2["Scenario"] = rep(c("1", "2","3","4", "5","6","7","8"), each=14000)

# Quality Control Filter Using Food Web flow rates -------------------------

qc_flowmat = foodweb_all(qualitycontrol2)

qc_err = foodweb_all_err(M=qc_flowmat, selecteddata = qualitycontrol2)

qc_err2 = qc_err < 0.000000001

qc2 = qualitycontrol2[qc_err2,]
qc_flowmat2 = qc_flowmat[,,qc_err2]

Sclist = as.character(seq(1,8,1))
qc3 = subset(qc2, Scenario == Sclist[1])[1:10000,]
qc_flowmat3 = qc_flowmat2[,,qc2$Scenario == Sclist[1]][,,1:10000]
for(i in 2:8) {
  qc3 = rbind(qc3, subset(qc2, Scenario == Sclist[i])[1:10000,])
  qc_flowmat3 = abind(qc_flowmat3, qc_flowmat2[,,qc2$Scenario == Sclist[i]][,,1:10000], along=3)
}
qc2 = qc3
qc_flowmat2 = qc_flowmat3
rm(qc3, Sclist, qc_flowmat3, qc_err)

qc2["ParamID"] = seq(1,dim(qc2)[1],1)
qc2["Nmin"] = qc2$Wnm - qc2$Vmn*qc2$Iorgstar
qc2["BrownLoop"] = 100*(qc_flowmat2["F", "M", ] + qc_flowmat2["D", "F", ] + qc_flowmat2["M", "D", ])/apply(qc_flowmat2,3, sum)
qc2["DegreeofOmnivory"] = qc_flowmat2["F", "D", ]/(qc_flowmat2["F", "D", ] + qc_flowmat2["F", "M", ])
names(qc2)


qualitycontrol2 %>% select(Scenario, positivesolutions) %>% 
  mutate(FoodwebSel = ifelse(qc_err2, 0,1)) %>% 
  group_by(Scenario) %>%  
  summarise (ps = sum(positivesolutions), n = n(), FoodwebSel= sum(FoodwebSel)) %>% 
  mutate(freq = (n-FoodwebSel)/ps) # drew "ps" parameter sets, ended up with "n" that fit the criteria in Mathematica. In R,"FoodwebSel" number of webs were rejected based on mass flow calculations, so "n-FoodwebSel" is the number of final webs achieved, divided by ps to get the proportion accepted.

# Plot Figure 2: Trends across Scenario and State Variables ---------------

#***************Omega
togo = qc2[,c(43:49,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
       substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool,togo$Scenario)

# Summarize and check signs
omega.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

omega.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                             ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

#xaxislabels = as.character(rep(seq(1,8,1), 7))
xaxislabels = rep("", 8*7)
tt = c("Detritus C", "Detritus N", "Microbi- \n detritivore", "Herbivore", "Inorganic N", "Microbes", "Plants")

for(i in 1:7) xaxislabels[(8*i-4)] = tt[i]
names(xaxislabels) = sort(unique(togo$PoolSc))

omegajitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=omega.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=omega.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Plant~C:N~(partialdiff*X/partialdiff*omega)))  + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

# omegajitter

#*************** MU
togo = qc2[,c(57:63,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
                  substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool, togo$Scenario)

mu.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

mu.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                         ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

mujitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=mu.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=mu.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Microbial~C:N~(partialdiff*X/partialdiff*mu))) + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

# mujitter 

#***************IN
togo = qc2[,c(50:57,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
                  substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool, togo$Scenario)

# Check which are only positive
with(togo, table(PoolSc,Change>0))

IN.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

IN.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                         ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

INjitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=IN.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=IN.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Inorganic~N~(partialdiff*X/partialdiff*I[N])))  + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + theme(panel.grid.major = element_blank(), 
                                                                                           panel.grid.minor = element_blank(),
                                                                                           panel.background = element_blank(), 
                                                                                           axis.line = element_line(colour = "black"))

# INjitter 


jpeg("Figure2.jpeg", width=8, height=11, units = "in",res=1000)
ggarrange(INjitter,omegajitter,mujitter, ncol=1, nrow=3, labels = "auto", 
          common.legend = T, legend="bottom")
dev.off()

# Plot Figure 1b-d: Associations -------------------------------------------------------

association_function(qc2=qc2) # this code plots a version of the Figure from first principles by subsetting data sequentially. Numbers refer to Scenarios 1-8 as described in the main text.

# A manual verison used in the MS that is more reader friendly.
pdf("Figure1.pdf", width=10, height=8)
par(mfrow=c(2,2), mar=c(1,1,1,1))

plot.new()
legend("topleft", legend="a", bty="n")

Sclist = as.character(seq(1,8,1))

pos = cbind(c(0.25, 0.25, 0.25, 0.7, 0.7, 0.7),
            c(0.9, 0.6, 0.3, 0.6, 0.9, 0.3))

nvec = c("Herbivore", "Inorganic N", "Detritus N", "Microbes","Microbi- \n detritivore", "Detritus C")

M = array(0, dim=c(6,6,8))
colnames(M)= rownames(M) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
tocheck = expand.grid(colnames(M), colnames(M))
tocheck = tocheck[tocheck[,1] != tocheck[,2],]
rownames(tocheck) = seq(1,30,1)
Mcur = matrix(0, nrow=6, ncol=6)

colnames(Mcur)= rownames(Mcur) = c("H", "Iorg", "Dn", "M","Dv", "Dc")
Mcol = MIN = Mcur
BF1 = 0.28
Mcur["H", "Dn"] = -BF1
Mcur["Dn","H"] = BF1
Mcur["Dc", "Dv"] = -BF1
Mcur["Dv","Dc"] = BF1

Mcol[] = "grey"
Mcol["Dv","Dc"]=Mcol["M","Iorg"]=Mcol["M","H"]= Mcol["Iorg","H"]= "black"

Mlty = Mcol

Mlty = ifelse(Mlty =="black", 1,2)

# Inorganic N
MIN["M","Dn"] = "F-N"
MIN["Dc","M"] = "F-C"
MIN["Dv","M"] = "M/F-C"
MIN["Dv","Dc"] = "M-C"
MIN["Iorg","H"] = "All"

plotmat_RWB(MIN, pos = pos, name = nvec, arr.lty = Mlty,
        lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
        box.size = 0.1, box.type = "circle", box.prop = 0.5,
        main = "Increasing Inorganic N", shadow.size = 0, arr.lcol = Mcol,
        arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
        box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
        txt.col ="black",
        box.col="white")
legend("topleft", legend="b", bty="n")

# Omega
MIN["M","Iorg"]= "F-N"
MIN["M","H"]= "F-N"
MIN["Dn","Iorg"]= "All"
MIN["Dn","H"]= "All"


plotmat_RWB(MIN, pos = pos, name = nvec, arr.lty = Mlty,
        lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
        box.size = 0.1, box.type = "circle", box.prop = 0.5,
        main = "Increasing Plant C:N", shadow.size = 0, arr.lcol = Mcol,
        arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
        box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
        txt.col ="black",
        box.col="white", lty = 2)
legend("topleft", legend="c", bty="n")

#Mu
MIN["Dc","M"] = 0
MIN["Dv","M"] = 0
MIN["Dv","Dc"] = 0
MIN["Dc","M"] = 0

plotmat_RWB(MIN, pos = pos, name = nvec, arr.lty = Mlty,
        lwd = 1, box.lwd = 2, cex.txt = 0.8, curve = Mcur,
        box.size = 0.1, box.type = "circle", box.prop = 0.5,
        main = "Increasing Microbial C:N", shadow.size = 0, arr.lcol = Mcol,
        arr.length = 0, arr.lwd=2, arr.pos=0.5, dtext = 0.1,
        box.lcol=c("green", "black","brown", "brown", "brown", "brown"),
        txt.col ="black",
        box.col="white", lty = 2)
legend("topleft", legend="d", bty="n")
dev.off()

# ...Plot supplemental figure on Microbi-detritivore Diet Appendix S3 ----

# C:N RATIO GRADIENTS
torun = c(seq(44,47),49, seq(58,61),63)
colvec = c("orange","grey","blue")
names(colvec) = c(-1,0,1)

titles = c(expression(Microbes~w.r.t.~Plant~C:N~(partialdiff*M[N]/partialdiff*omega)),
           expression(Detritus~N~w.r.t.~Plant~C:N~(partialdiff*D[N]/partialdiff*omega)),
           expression(Detritus~C~w.r.t.~Plant~C:N~(partialdiff*D[C]/partialdiff*omega)),
           expression(Inoranic~N~w.r.t.~Plant~C:N~(partialdiff*N/partialdiff*omega)),
           expression(Microbi-detiritovre~w.r.t.~Plant~C:N~(partialdiff*"F"[N]/partialdiff*omega)),
           expression(Microbes~w.r.t.~Microbial~C:N~(partialdiff*M[N]/partialdiff*mu)),
           expression(Detritus~N~w.r.t.~Microbial~C:N~(partialdiff*D[N]/partialdiff*mu)),
           expression(Detritus~C~w.r.t.~Microbial~C:N~(partialdiff*D[C]/partialdiff*mu)),
           expression(Inoranic~N~w.r.t.~Microbial~C:N~(partialdiff*N/partialdiff*mu)),
           expression(Microbi-detiritovre~w.r.t.~Microbial~C:N~(partialdiff*"F"[N]/partialdiff*mu)))

for(i in 1:length(torun)){
  qc2["Group"] = apply(sign(qc2[,torun[i]]), 1, FUN=paste0, collapse = "")
  assign(paste0("p",i), ggplot(aes(x=Scenario, y=DegreeofOmnivory*100, fill=Group), data=qc2) + geom_boxplot(width=0.5) + theme_bw() + ylab("") + xlab("") + scale_fill_manual(values = colvec, labels=c("Decrease", "Zero", "Increase")) + ggtitle(titles[i]) + theme(plot.title = element_text(size = 10)))
}


figure = ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, common.legend = T, ncol=2, nrow=5, labels = "auto")

pdf("AppendixS3_FigureS3.pdf", width=8.5, height=11)
annotate_figure(figure,
                bottom = text_grob("Scenario"),
                left = text_grob("Detritus in Microbi-detritivore Diet (%)", color = "black", rot = 90)
                )
dev.off()

# INORGRANIC NITROGEN 
torun = c(seq(51,54),56)
colvec = c("orange","grey","blue")
names(colvec) = c(-1,0,1)

titles = c(expression(Microbes~w.r.t.~Inorganic~N~(partialdiff*M[N]/partialdiff*I[N])),
           expression(Detritus~N~w.r.t.~Inorganic~N~(partialdiff*D[N]/partialdiff*I[N])),
           expression(Detritus~C~w.r.t.~Inorganic~N~(partialdiff*D[C]/partialdiff*I[N])),
           expression(Inoranic~N~w.r.t.~Inorganic~N~(partialdiff*N/partialdiff*I[N])),
           expression(Microbi-detiritovre~w.r.t.~Inorganic~N~(partialdiff*"F"[N]/partialdiff*I[N])))

for(i in 1:length(torun)){
  qc2["Group"] = apply(sign(qc2[,torun[i]]), 1, FUN=paste0, collapse = "")
  assign(paste0("p",i), ggplot(aes(x=Scenario, y=DegreeofOmnivory*100, fill=Group), data=qc2) + geom_boxplot(width=0.5) + theme_bw() + ylab("") + xlab("") + scale_fill_manual(values = colvec, labels=c("Decrease", "Increase")) + ggtitle(titles[i])+ theme(plot.title = element_text(size = 10)))
}


figure = ggarrange(p1,p2,p3,p4,p5, common.legend = T, ncol=2, nrow=3, labels = "auto")

pdf("AppendixS3_FigureS2.pdf", width=8.5, height=6.6)
annotate_figure(figure,
                bottom = text_grob("Scenario"),
                left = text_grob("Detritus in Microbi-detritivore Diet (%)", color = "black", rot = 90)
)
dev.off()

rm(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

# Calculate the correlation between the microbial loop ("Brown Loop") and 
with(qc2,cor(BrownLoop, DegreeofOmnivory))





# Model trends along gradients ---------------------------

# can calculate gradients using the Figure1_gradient_calculations.R script

rm(INjitter,omegajitter,mujitter)

INfinaldata = read_csv("Data/INfinaldata.csv")[qc_err2,] %>% group_by(Scenario) %>% filter(row_number() <=10000) %>% ungroup()
mufinaldata = read.csv("Data/mufinaldata.csv")[qc_err2,] %>% group_by(Scenario) %>% filter(row_number() <=10000)%>% ungroup()
finaldata = read.csv("Data/omegafinaldata.csv")[qc_err2,] %>% group_by(Scenario) %>% filter(row_number() <=10000)%>% ungroup()


#***************Omega
togo = finaldata %>% gather(key=Pool, value=Change, -Scenario) %>% mutate(PoolSc = paste0(Pool, Scenario)) %>% filter(!is.na(Change)) %>% mutate(Scenario= as.character(Scenario))

# Summarize and check signs
omega.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

omega.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                         ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

xaxislabels = rep("", 8*7)
tt = c("Detritus C", "Detritus N", "Microbi- \n detritivore", "Herbivore", "Inorganic N", "Microbes", "Plants")

for(i in 1:7) xaxislabels[(8*i-4)] = tt[i]
names(xaxislabels) = sort(unique(togo$PoolSc))

omegajitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=omega.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=omega.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Plant~C:N~(Delta*X/Delta*omega)))  + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

# omegajitter

#*************** MU
togo = mufinaldata %>% gather(key=Pool, value=Change, -Scenario) %>% mutate(PoolSc = paste0(Pool, Scenario)) %>% filter(!is.na(Change)) %>% mutate(Scenario= as.character(Scenario))

mu.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

mu.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                      ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

xaxislabels = rep("", 8*7)
tt = c("Detritus C", "Detritus N", "Microbi- \n detritivore", "Herbivore", "Inorganic N", "Microbes", "Plants")

for(i in 1:7) xaxislabels[(8*i-4)] = tt[i]
names(xaxislabels) = sort(unique(togo$PoolSc))

mujitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=mu.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=mu.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Microbial~C:N~(Delta*X/Delta*mu))) + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

# mujitter 

#***************IN
togo = INfinaldata %>% gather(key=Pool, value=Change, -Scenario) %>% mutate(PoolSc = paste0(Pool, Scenario)) %>% filter(!is.na(Change)) %>% mutate(Scenario= as.character(Scenario))

IN.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

IN.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                      ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

xaxislabels = rep("", 8*7)
tt = c("Detritus C", "Detritus N", "Microbi- \n detritivore", "Herbivore", "Inorganic N", "Microbes", "Plants")

for(i in 1:7) xaxislabels[(8*i-4)] = tt[i]
names(xaxislabels) = sort(unique(togo$PoolSc))

INjitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=IN.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=IN.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Inorganic~N~(Delta*X/Delta*I[N])))  + coord_cartesian(ylim=c(-5,10)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + theme(panel.grid.major = element_blank(), 
                                                                                           panel.grid.minor = element_blank(),
                                                                                           panel.background = element_blank(), 
                                                                                           axis.line = element_line(colour = "black"))

# INjitter 

jpeg("AppendixS3_FigureS1.jpeg", width=8, height=11, units = "in",res=1000)
ggarrange(INjitter,omegajitter,mujitter, ncol=1, nrow=3, labels = "auto", 
          common.legend = T, legend="bottom")
dev.off()

# Appendix S3: Partial Derivatives ----

newdata = qc2[,c("dIorg_dVfd", "dIorg_dVfm","dIorg_dVhp", "dIorg_dVmd","dIorg_dVmn", "Scenario", "Nmin")]
newdata$Nmin = newdata$Nmin/100

newdata["cf1"] = ifelse(newdata$Scenario =="1"|newdata$Scenario =="2"|
                          newdata$Scenario =="3"|newdata$Scenario =="4", 0,1)
newdata["cf2"] = ifelse(newdata$Scenario =="1"|newdata$Scenario =="5",1,
                               ifelse(newdata$Scenario =="2"|newdata$Scenario =="6",2,
                                      ifelse(newdata$Scenario =="3"|newdata$Scenario =="7",3,4)))


lowhigh = c(0.0001,0.9999)

namedata = names(newdata)

dernames = c("Detritivory", "Microbivory", "Herbivory", "Microbial Decomposition", "Microbial Mineralization")
names(newdata) = namedata

for(i in 1:5){
  names(newdata)= namedata
  names(newdata)[i] = "Sel"
  
  limity = max(abs(quantile(newdata$Sel, na.rm=T, probs=lowhigh)))
  limitx = max(abs(quantile(newdata$Nmin, na.rm=T, probs=lowhigh)))
  der.summary <- aggregate(cbind(Sel, Nmin) ~ Scenario, quantile, data=newdata,probs=c(0.001, 0.25, 0.5, 0.75, 0.999))
  der.summary["cf1"] = c(0,0,0,0,1,1,1,1)
  der.summary["cf2"] = rep(seq(1,4), 2)
  
  ppp = ggplot(aes(x=Nmin[,3], y=Sel[,3]), shape=19, color="grey50", size=3, data=der.summary) + geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) + 
    geom_point(aes(x=Nmin, y=Sel, color=Scenario), data=newdata,size=2) + 
    theme_bw()  + facet_grid(cf1~cf2) +
    xlab("N mineralization (x 100)") + ylab(dernames[i])  + coord_cartesian(ylim=c(-limity,limity),xlim=c(-limitx,limitx)) + 
    
    geom_errorbar(aes(ymin=Sel[,1],ymax=Sel[,5]),color= "grey50", data=der.summary, size=2) +
    geom_errorbarh(aes(xmin=Nmin[,1],xmax=Nmin[,5]),color= "grey50", data=der.summary, size=2) +
    geom_point(aes(x=Nmin[,3], y=Sel[,3]),  shape=19, color="white", size=1, data=der.summary) +
    
    scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited",
                                  "H C-limited", "F C-limited", "H & F C-limited"),
                       values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
    theme(panel.grid.major = element_blank(), 
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          strip.text.y = element_blank(),
          strip.text.x = element_blank())
  assign(paste0("der",i), ppp)
}
rm(ppp, limitx, limity, der.summary, dernames,namedata, newdata)

jpeg("AppendixS3_FigureS4.jpeg", height = 8.5, width = 11, units = "in",res=1000)
ggarrange(der1,der2,der3,der4,labels="auto", common.legend = T)
dev.off()

# Appendix S6: Donor Control Paritial Derivative Results ------------------

rm(qc2, togo,omega.summary,mu.summary, IN.summary,INjitter,omegajitter,mujitter)

qc2 = rbind(read_csv("Data_DonorControl/ParamEqbmDERout_Sc1_20185221113.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc2_20185221119.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc3_20185221139.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc4_20185221142.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc5_20185221146.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc6_20185221147.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc7_20185221150.csv", col_names = FALSE),
            read_csv("Data_DonorControl/ParamEqbmDERout_Sc8_20185221157.csv", col_names = FALSE))

colnames(qc2) = c("IN", "q", "l", "Vpn", "Vhp", "Vfd", "Vfm", "Vmd","Vmn", "rm", "rh", "rf", 
                  "exm", "exf", "exh", "tp", "th", 
                  "tm", "tf", "Eta", "Omega", "Mu", "Phi", "ef", "eh",
                  "positivesolutions", "aa", "r", "phck",  "Wnf", "Wnh", "Wnm", "Wcf", "Wch", "Wcm",
                  "Pstar", "Mstar", "Dnstar", "Dcstar", "Iorgstar", "Hstar", "Dvstar",
                  "dP_dOmega","dM_dOmega","dDn_dOmega","dDc_dOmega","dIorg_dOmega","dH_dOmega","dDv_dOmega",
                  "dP_dIN","dM_dIN","dDn_dIN","dDc_dIN","dIorg_dIN","dH_dIN","dDv_dIN",
                  "dP_dMu","dM_dMu","dDn_dMu","dDc_dMu","dIorg_dMu","dH_dMu","dDv_dMu",
                  "dP_dVhp","dM_dVhp","dDn_dVhp","dDc_dVhp","dIorg_dVhp","dH_dVhp","dDv_dVhp",
                  "dP_dVfd","dM_dVfd","dDn_dVfd","dDc_dVfd","dIorg_dVfd","dH_dVfd","dDv_dVfd",
                  "dP_dVfm","dM_dVfm","dDn_dVfm","dDc_dVfm","dIorg_dVfm","dH_dVfm","dDv_dVfm",
                  "dP_dVmd","dM_dVmd","dDn_dVmd","dDc_dVmd","dIorg_dVmd","dH_dVmd","dDv_dVmd",
                  "dP_dVmn","dM_dVmn","dDn_dVmn","dDc_dVmn","dIorg_dVmn","dH_dVmn","dDv_dVmn")
qc2["Scenario"] = rep(c("1", "2","3","4", "5","6","7","8"), each=10000)

#Omega
togo = qc2[,c(43:49,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
                  substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool,togo$Scenario)

# Summarize and check signs
omega.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

omega.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                         ifelse(check2[,1]==-1, "Decrease","Increase")))

shapevec = c("Zero"=19, "Both"=18, "Decrease"=25, "Increase"=17)

#xaxislabels = as.character(rep(seq(1,8,1), 7))
tt = c("Detritus C", "Detritus N", "Microbi- \n detritivore", "Herbivore", "Inorganic N", "Microbes", "Plants")
xaxislabels = rep("", 7*8)
for(i in 1:7) xaxislabels[(8*i-4)] = tt[i]
names(xaxislabels) = sort(unique(togo$PoolSc))

omegajitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=omega.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=omega.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Plant~C:N~(partialdiff*X/partialdiff*omega)))  + coord_cartesian(ylim=c(-1,2)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

# omegajitter 

# MU
togo = qc2[,c(57:63,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
                  substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool, togo$Scenario)

mu.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

mu.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                      ifelse(check2[,1]==-1, "Decrease","Increase")))

mujitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=mu.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=mu.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Microbial~C:N~(partialdiff*X/partialdiff*mu)))  + coord_cartesian(ylim=c(-2,5)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))

#mujitter 

# IN
togo = qc2[,c(50:57,99)]

togo = tidyr::gather(togo, key=Pool, value=Change, -Scenario)

togo$Pool= ifelse(substr(togo$Pool, 3, 3)=="_",
                  substr(togo$Pool, 2, 2),paste0(substr(togo$Pool, 2, 2),substr(togo$Pool, 3, 3)))

togo["PoolSc"] = paste0(togo$Pool, togo$Scenario)

# Check which are only positive
with(togo, table(PoolSc,Change>0))

IN.summary = aggregate(Change~PoolSc + Scenario, data=togo, FUN=mean)

check1 = apply(sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change),1,prod)
check2 = sign(aggregate(Change~PoolSc + Scenario, data=togo, FUN=range)$Change)

IN.summary["sign"] = ifelse(check1==0, "Zero", ifelse(check1==-1, "Both",
                                                      ifelse(check2[,1]==-1, "Decrease","Increase")))

INjitter = ggplot(aes(x=PoolSc, y=Change,color=Scenario), data=togo) + 
  xlab("State Variable (X)") + geom_hline(yintercept = 0, linetype="dashed", color="grey") + 
  geom_jitter(size = 0.5) + theme_bw() + scale_x_discrete(labels=xaxislabels) + 
  geom_point(aes(x=PoolSc, y=Change, shape=sign), fill= "grey50", color="grey50", size=3, data=IN.summary) +
  geom_point(aes(x=PoolSc, y=Change, shape=sign),fill= "white", color="white", size=1, data=IN.summary) +
  scale_color_manual(labels = c("N-limited", "M C-limited", "M & H C-limited", "M & F C-limited", "M & H & F C-limited","H C-limited", "F C-limited", "H & F C-limited"),
                     values=c("black", "red","orange", "brown", "blue","green", "purple","cyan")) + 
  scale_shape_manual(values = shapevec) + guides(shape=FALSE)+
  ylab(expression(X~w.r.t.~Inorganic~N~(partialdiff*X/partialdiff*I[N])))  + coord_cartesian(ylim=c(-0.5,3)) + 
  geom_vline(xintercept = seq(8.5,length=6,by=8), linetype="dashed", color="grey") + theme(panel.grid.major = element_blank(), 
                                                                                           panel.grid.minor = element_blank(),
                                                                                           panel.background = element_blank(), 
                                                                                           axis.line = element_line(colour = "black"))

INjitter 


jpeg("AppendixS6_FigureS1.jpeg", width=8, height=11, units = "in",res=1000)
ggarrange(INjitter,omegajitter,mujitter, ncol=1, nrow=3, labels = "auto", 
          common.legend = T, legend="bottom")
dev.off()
