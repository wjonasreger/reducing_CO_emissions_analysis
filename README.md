# stat443

pred.mb2 = function(x, mbc, chng, i, name){

  m = 2*sd(chng)

  print(sd(chng))

  ms = seq(mean(chng)-m, mean(chng)+m, length.out = 100)

  c=numeric(100)

  for(k in 1:100){

    xvals = x

    xvals[i] = ms[k]

    jmp = c(

      predict(object = poly(gt_mid_all$AP, 2), newdata = xvals[1]),

      predict(object = poly(gt_mid_all$AT, 2), newdata = xvals[2]),

      predict(object = poly(gt_mid_all$AH, 2), newdata = xvals[3]),

      predict(object = poly(gt_mid_all$AFDP, 2), newdata = xvals[4]),

      predict(object = poly(gt_mid_all$TAT, 2), newdata = xvals[5]),

      predict(object = poly(gt_mid_all$TIT, 2), newdata = xvals[6]),

      predict(object = poly(gt_mid_all$NOX, 2), newdata = xvals[7])

            )

    jm = jmc[1] + 

      jmc[2]*jmp[1] + jmc[3]*jmp[2] +

      jmc[4]*jmp[3] + jmc[5]*jmp[4] +

      jmc[6]*jmp[5] + jmc[7]*jmp[6] +

      jmc[8]*jmp[7] + jmc[9]*jmp[8] +

      jmc[10]*jmp[9] + jmc[11]*jmp[10] +

      jmc[12]*jmp[11] + jmc[13]*jmp[12] +

      jmc[14]*jmp[13] + jmc[15]*jmp[14]

    c[k] = jm^2 - 1.325417^2

  }

  minval = ms[which.min(c)]

  minval1se = ms[which.min(c[26:75])+25]

  plot(ms, c, main=paste(name, "vs. CO predicted change"),

       xlab=paste0(name, " (CO Min at ", name, " = ", round(minval, 1), ")"), 
       
       ylab = paste0("CO diff (", round(max(c)-min(c), 2), ")"), 

       sub = paste0("CO diff Min = ", round(min(c), 2), ", Min 1se = ", round(min(c[26:75]), 2)),

       type='l', col="#13294b")

  abline(v=minval, col="#e84a27")

  abline(v=minval1se, col="#e84a27")

  abline(v=mean(chng), col="dodgerblue", lty=2)

  abline(h=0, col="dodgerblue", lty=2)
  
}

xvals = c(mean(gt_mid_all$AP), mean(gt_mid_all$AT), mean(gt_mid_all$AH), mean(gt_mid_all$AFDP), mean(gt_mid_all$TAT), mean(gt_mid_all$TIT), mean(gt_mid_all$NOX))

jmc = coef(j_mod)

par(mfrow=c(2,2))

pred.mb2(xvals, jmc, gt_mid_all$AT, 2, "AT")

pred.mb2(xvals, jmc, gt_mid_all$AP, 1, "AP")

pred.mb2(xvals, jmc, gt_mid_all$AH, 3, "AH")

pred.mb2(xvals, jmc, gt_mid_all$AFDP, 4, "AFDP")

pred.mb2(xvals, jmc, gt_mid_all$TIT, 6, "TIT")

pred.mb2(xvals, jmc, gt_mid_all$TAT, 5, "TAT")

pred.mb2(xvals, jmc, gt_mid_all$NOX, 7, "NOX")