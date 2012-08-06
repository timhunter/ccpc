files <- list.files(pattern = "\\.csv$")
cjkdata <- do.call("rbind", lapply(files, read.csv, header = TRUE))
#colnames(cjkdata) <- c("language","grammar","type","pos1","pos2","pos3","pos4")

cn.sr.promo <- as.numeric(subset(cjkdata,Language=="Chinese" & Grammar == "Promotion" & Type == "SR",select=c(W1,W2,W3,W4))) 
cn.or.promo <- as.numeric(subset(cjkdata,Language=="Chinese" & Grammar == "Promotion" & Type == "OR",select=c(W1,W2,W3,W4))) 
cn.promo <- rbind(cn.sr.promo,cn.or.promo)

postscript("cnpromo.ps")
barplot(matrix(cn.promo,ncol=4),beside=T,ylim=c(0,1.5),cex.axis=1.5)
title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
mtext(c("Vt(SR)/N(OR)","N(SR)/Vt(OR)","de","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),cn.promo+0.05,round(cn.promo,2),cex=1.3)
legend("topleft",c("CN-SR","CN-OR"),cex=1.5,bty="n", fill=c("black","grey"))
dev.off()

kr.sr.promo <- as.numeric(subset(cjkdata,Language=="Korean" & Grammar == "Promotion" & Type == "SR",select=c(W1,W2,W3,W4))) 
kr.or.promo <- as.numeric(subset(cjkdata,Language=="Korean" & Grammar == "Promotion" & Type == "OR",select=c(W1,W2,W3,W4))) 
kr.promo <- rbind(kr.sr.promo,kr.or.promo)

postscript("krpromo.ps")
barplot(matrix(kr.promo,ncol=4),beside=T,ylim=c(0,2.8),cex.axis=1.5)
title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
mtext(c("N","Acc(SR)/Nom(OR)","V","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),kr.promo+0.05,round(kr.promo,2),cex=1.3)
legend("topleft",c("KR-SR","KR-OR"),cex=1.5,bty="n", fill=c("black","grey"))
dev.off()


jp.sr.promo <- as.numeric(subset(cjkdata,Language=="Japanese" & Grammar == "Promotion" & Type == "SR",select=c(W1,W2,W3,W4))) 
jp.or.promo <- as.numeric(subset(cjkdata,Language=="Japanese" & Grammar == "Promotion" & Type == "OR",select=c(W1,W2,W3,W4))) 
jp.promo <- rbind(jp.sr.promo,jp.or.promo)

postscript("jppromo.ps")
barplot(matrix(jp.promo,ncol=4),beside=T,ylim=c(0,1.5),cex.axis=1.5)
title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
mtext(c("N","(o(SR)/ga(OR)","V","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),jp.promo+0.05,round(jp.promo,2),cex=1.3)
legend("topleft",c("JP-SR","JP-OR"),cex=1.5,bty="n", fill=c("black","grey"))
dev.off()




### previous data input by hand

# # Chinese predictions


# ## promotion analysis
 # cnsrprom <- c(0.9354548788926, 0.5857488703575, 0, 0.4450675233410) 
 # cnorprom <- c(1.0801227759358, 0.7007575131962, 0, 0.9076961653608)

# ## adjunction analysis
 # cnsradj <- c(0.9352021805069, 0.5665285979680, 0, 0.5780053041424) 
 # cnoradj <- c(1.0850098466572, 0.6982535168137, 0, 0.9271671861228)

# ## pronominal analysis
 # cnsrpro <- c(1.0378421910942, 0.5064311007726, 0, 0.5765373414337) 
 # cnorpro <- c(1.0251140408377, 0.6038795240366, 0, 0.9191736301840)
 
# ## bind sr&or side-by-side 
 # cnrcadj <- rbind(cnsradj,cnoradj)
 # cnrcprom <- rbind(cnsrprom,cnorprom)
 # cnrcpro <- rbind(cnsrpro,cnorpro)
 
# postscript("cnpro.ps")
# barplot(matrix(cnrcpro,ncol=4),beside=T,ylim=c(0,1.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("Vt(SR)/N(OR)","N(SR)/Vt(OR)","de","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(1.08,1.07,0.55,0.64,0.04,0.04,0.62,0.96),c("1.04","1.03","0.51","0.60","0.0","0.0","0.58","0.92"),cex=1.3)
# legend("topleft",c("CN-SR-Pronominal","CN-OR-Pronominal"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("cnprom.ps")
# barplot(matrix(cnrcprom,ncol=4),beside=T,ylim=c(0,1.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("Vt(SR)/N(OR)","N(SR)/Vt(OR)","de","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.97,1.12,0.63,0.74,0.04,0.04,0.48,0.95),c("0.94","1.08","0.59","0.70","0.0","0.0","0.45","0.91"),cex=1.3)
# legend("topleft",c("CN-SR-Promotion","CN-OR-Promotion"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("cnadj.ps")
# barplot(matrix(cnrcadj,ncol=4),beside=T,ylim=c(0,1.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("Vt(SR)/N(OR)","N(SR)/Vt(OR)","de","N(head)"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5,13.5,14.5),c(0.98,1.13,0.61,0.74,0.04,0.04,0.62,0.97),c("0.94","1.09","0.57","0.70","0.0","0.0","0.58","0.93"),cex=1.3)
# legend("topleft",c("CN-SR-Adjunction","CN-OR-Adjunction"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()



# # Korean predictions

# ## promotion analysis
# krsrprom <- c(0.1204263776872, 0, 0, 0) 
# krorprom <- c(0.1204263776872, 1.5867774648689, 0, 0.216331779475)

# ## adjunction analysis
# krsradj <- c(0.1519865210475, 0, 0, 0.754941133660)
# kroradj <- c(0.1519865210475, 2.2035224838667, 0, 0.363476683793)

# ## pronominal analysis
# krsrpro <- c(0.3795345794261, 0, 0, 0.845856608093) 
# krorpro <- c(0.3795345794261, 2.0848290865979, 0, 0.360837789265)

# krrcprom <- rbind(krsrprom,krorprom)
# krrcadj <- rbind(krsradj,kroradj)
# krrcpro <- rbind(krsrpro,krorpro)

# postscript("krprom.ps")
# barplot(matrix(krrcprom,ncol=4),beside=T,ylim=c(0,2.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.17,0.17,0.05,1.64,0.05,0.05,0.05,0.27),c("0.12","0.12","0.0","1.59","0.0","0.0","0.0","0.22"),cex=1.3)
# legend("topleft",c("KR-SR-Promotion","KR-OR-Promotion"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("kradj.ps")
# barplot(matrix(krrcadj,ncol=4),beside=T,ylim=c(0,2.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11,14,17),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.2,0.2,0.05,2.25,0.05,0.05,0.8,0.41),c("0.15","0.15","0.0","2.20","0.0","0.0","0.75","0.36"),cex=1.3)
# legend("topleft",c("KR-SR-Adjunction","KR-OR-Adjunction"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("krpro.ps")
# barplot(matrix(krrcpro,ncol=4),beside=T,ylim=c(0,2.5),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.43,0.43,0.05,2.13,0.05,0.05,0.9,0.41),c("0.38","0.38","0.0","2.08","0.0","0.0","0.85","0.36"),cex=1.3)
# legend("topleft",c("KR-SR-Pronominal","KR-OR-Pronominal"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# # Japanese predictions

# jpsrprom <- c(0.0547940238251, 0.3058138550312, 0, 0)
# jporprom <- c(0.0547940238251, 1.1202811158657, 0, 0)

# jpsradj <- c(0.1529153672956, 0.0333691692526, 0, 0)
# jporadj <- c(0.1529153672956, 1.6407092106540, 0, 0.2081908132542)

# jpsrpro <- c(0.5467498156336, 0.1248069038683, 0, 0)
# jporpro <- c(0.5467498156336, 1.4554665555796, 0, 0)

# jprcprom <- rbind(jpsrprom,jporprom)
# jprcadj <- rbind(jpsradj,jporadj)
# jprcpro <- rbind(jpsrpro,jporpro)

# postscript("jpprom.ps")
# barplot(matrix(jprcprom,ncol=4),beside=T,ylim=c(0,2),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11,14,17),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.11,0.11,0.37,1.18,0.06,0.06,0.06,0.06),c("0.05","0.05","0.31","1.12","0.0","0.0","0.0","0.0"),cex=1.3)
# legend("topleft",c("JP-SR-Promotion","JP-OR-Promotion"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("jpadj.ps")
# barplot(matrix(jprcadj,ncol=4),beside=T,ylim=c(0,2),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11,14,17),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.21,0.21,0.09,1.7,0.06,0.06,0.06,0.27),c("0.15","0.15","0.03","1.64","0.0","0.0","0.0","0.21"),cex=1.3)
# legend("topleft",c("JP-SR-Adjunction","JP-OR-Adjunction"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()

# postscript("jppro.ps")
# barplot(matrix(jprcpro,ncol=4),beside=T,ylim=c(0,2),cex.axis=1.5)
# title(ylab="Entropy Reduction (bit)",cex.lab=1.4)
# mtext(c("N","Acc(SR)/Nom(OR)","Vt","N"),at=c(2,5,8,11,14,17),side=1,line=1,cex=1.4)
# text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),c(0.6,0.6,0.17,1.51,0.05,0.05,0.05,0.05),c("0.55","0.55","0.12","1.45","0.0","0.0","0.0","0.0"),cex=1.3)
# legend("topleft",c("JP-SR-Pronominal","JP-OR-Pronominal"),cex=1.5,bty="n", fill=c("black","grey"))
# dev.off()
