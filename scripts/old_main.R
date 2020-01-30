library(GA)
library(ggplot2)
#library(plotrix)


sharpe_ratio = function(x) { 
  return (mean(portfolio_returns(x))/sqrt(var(portfolio_returns(x))))
  
}

obj  = function(x) {
  return (-sharpe_ratio(x)+100*penalty(x))
}

penalty = function(x) {
  penalties = (sum(x)-1)*(sum(x)-1)   
  
  for (i in 1:length(x)) {
    penalties = penalties +  max(c(0,x[i]-1))*max(c(0,x[i]-1)) +  max(c(0,-x[i]))*max(c(0,-x[i]))     
  }
  
  return (penalties)
}


portfolio_returns = function(x) {
  port.returns = 0
  
  for (i in 1:length(x)) {
    port.returns = port.returns + profit[,i] * x[i]
  }
  
  return (port.returns)
}


#setwd("/home/ghost/Desktop/Port-Opt/new_york")
#csv_files = c("AAPL.csv","TSLA.csv","AMZN.csv","GOOG.csv","NFLX.csv","FB.csv")
csv_files = c("A.csv","AAL.csv","AAP.csv","AAPL.csv","ABC.csv","ABT.csv")
#csv_files = c("a.csv", "aal.csv", "aap.csv", "aapl.csv", "abbv.csv", "abc.csv", "abt.csv", "acn.csv", "adbe.csv", "adi.csv", "adm.csv", "adp.csv", "ads.csv", "adsk.csv", "aee.csv", "aep.csv", "aes.csv", "aet.csv", "afl.csv", "agn.csv", "aig.csv", "aiv.csv", "aiz.csv", "ajg.csv", "akam.csv", "alb.csv", "alk.csv", "all.csv", "alle.csv", "alxn.csv", "amat.csv", "ame.csv", "amg.csv", "amgn.csv", "amp.csv", "amt.csv", "amzn.csv", "an.csv", "antm.csv", "aon.csv", "apa.csv", "apc.csv", "apd.csv", "aph.csv", "arnc.csv", "atvi.csv", "avb.csv", "avgo.csv", "avy.csv", "awk.csv", "axp.csv", "ayi.csv", "azo.csv", "ba.csv", "bac.csv", "bax.csv", "bbby.csv", "bbt.csv", "bby.csv", "bcr.csv", "bdx.csv", "ben.csv", "bhi.csv", "biib.csv", "bk.csv", "blk.csv", "bll.csv", "bmy.csv", "bsx.csv", "bwa.csv", "bxp.csv", "c.csv", "ca.csv", "cag.csv", "cah.csv", "cat.csv", "cb.csv", "cbg.csv", "cbs.csv", "cci.csv", "ccl.csv", "celg.csv", "cern.csv", "cf.csv", "cfg.csv", "chd.csv", "chk.csv", "chrw.csv", "chtr.csv", "ci.csv", "cinf.csv", "cl.csv", "clx.csv", "cma.csv", "cmcsa.csv", "cme.csv", "cmg.csv", "cmi.csv", "cms.csv", "cnc.csv", "cnp.csv", "cof.csv", "cog.csv", "coh.csv", "col.csv", "coo.csv", "cop.csv", "cost.csv", "coty.csv", "cpb.csv", "crm.csv", "csco.csv", "csra.csv", "csx.csv", "ctas.csv", "ctl.csv", "ctsh.csv", "ctxs.csv", "cvs.csv", "cvx.csv", "cxo.csv", "d.csv", "dal.csv", "dd.csv", "de.csv", "dfs.csv", "dg.csv", "dgx.csv", "dhi.csv", "dhr.csv", "dis.csv", "disca.csv", "disck.csv", "dlph.csv", "dlr.csv", "dltr.csv", "dnb.csv", "dov.csv", "dow.csv", "dps.csv", "dri.csv", "dte.csv", "duk.csv", "dva.csv", "dvn.csv", "ea.csv", "ebay.csv", "ecl.csv", "ed.csv", "efx.csv", "eix.csv", "el.csv", "emn.csv", "emr.csv", "endp.csv", "eog.csv", "eqix.csv", "eqr.csv", "eqt.csv", "es.csv", "esrx.csv", "ess.csv", "etfc.csv", "etn.csv", "etr.csv", "evhc.csv", "ew.csv", "exc.csv", "expd.csv", "expe.csv", "exr.csv", "f.csv", "fast.csv", "fb.csv", "fbhs.csv", "fcx.csv", "fdx.csv", "fe.csv", "ffiv.csv", "fis.csv", "fisv.csv", "fitb.csv", "fl.csv", "flir.csv", "flr.csv", "fls.csv", "fmc.csv", "fox.csv", "foxa.csv", "frt.csv", "fslr.csv", "fti.csv", "ftr.csv", "ftv.csv", "gd.csv", "ge.csv", "ggp.csv", "gild.csv", "gis.csv", "glw.csv", "gm.csv", "goog.csv", "googl.csv", "gpc.csv", "gpn.csv", "gps.csv", "grmn.csv", "gs.csv", "gt.csv", "gww.csv", "hal.csv", "har.csv", "has.csv", "hban.csv", "hbi.csv", "hca.csv", "hcn.csv", "hcp.csv", "hd.csv", "hes.csv", "hig.csv", "hog.csv", "holx.csv", "hon.csv", "hp.csv", "hpe.csv", "hpq.csv", "hrb.csv", "hrl.csv", "hrs.csv", "hsic.csv", "hst.csv", "hsy.csv", "hum.csv", "ibm.csv", "ice.csv", "idxx.csv", "iff.csv", "ilmn.csv", "intc.csv", "intu.csv", "ip.csv", "ipg.csv", "ir.csv", "irm.csv", "isrg.csv", "itw.csv", "ivz.csv", "jbht.csv", "jci.csv", "jec.csv", "jnj.csv", "jnpr.csv", "jpm.csv", "jwn.csv", "k.csv", "key.csv", "khc.csv", "kim.csv", "klac.csv", "kmb.csv", "kmi.csv", "kmx.csv", "ko.csv", "kors.csv", "kr.csv", "kss.csv", "ksu.csv", "l.csv", "lb.csv", "leg.csv", "len.csv", "lh.csv", "lkq.csv", "lll.csv", "lltc.csv", "lly.csv", "lmt.csv", "lnc.csv", "lnt.csv", "low.csv", "lrcx.csv", "luk.csv", "luv.csv", "lvlt.csv", "lyb.csv", "m.csv", "ma.csv", "maa.csv", "mac.csv", "mar.csv", "mas.csv", "mat.csv", "mcd.csv", "mchp.csv", "mck.csv", "mco.csv", "mdlz.csv", "mdt.csv", "met.csv", "mhk.csv", "mjn.csv", "mkc.csv", "mlm.csv", "mmc.csv", "mmm.csv", "mnk.csv", "mnst.csv", "mo.csv", "mon.csv", "mos.csv", "mpc.csv", "mrk.csv", "mro.csv", "msft.csv", "msi.csv", "mtb.csv", "mtd.csv", "mu.csv", "mur.csv", "myl.csv", "navi.csv", "nbl.csv", "ndaq.csv", "nee.csv", "nem.csv", "nflx.csv", "nfx.csv", "ni.csv", "nke.csv", "nlsn.csv", "noc.csv", "nov.csv", "nrg.csv", "nsc.csv", "ntap.csv", "ntrs.csv", "nue.csv", "nvda.csv", "nwl.csv", "nws.csv", "nwsa.csv", "o.csv", "oke.csv", "omc.csv", "orcl.csv", "orly.csv", "oxy.csv", "payx.csv", "pbct.csv", "pbi.csv", "pcar.csv", "pcg.csv", "pcln.csv", "pdco.csv", "peg.csv", "pep.csv", "pfe.csv", "pfg.csv", "pg.csv", "pgr.csv", "ph.csv", "phm.csv", "pki.csv", "pld.csv", "pm.csv", "pnc.csv", "pnr.csv", "pnw.csv", "ppg.csv", "ppl.csv", "prgo.csv", "pru.csv", "psa.csv", "psx.csv", "pvh.csv", "pwr.csv", "px.csv", "pxd.csv", "pypl.csv", "qcom.csv", "qrvo.csv", "r.csv", "rai.csv", "rcl.csv", "regn.csv", "rf.csv", "rhi.csv", "rht.csv", "rig.csv", "rl.csv", "rok.csv", "rop.csv", "rost.csv", "rrc.csv", "rsg.csv", "rtn.csv", "sbux.csv", "scg.csv", "schw.csv", "se.csv", "see.csv", "shw.csv", "sig.csv", "sjm.csv", "slb.csv", "slg.csv", "sna.csv", "sni.csv", "so.csv", "spg.csv", "spgi.csv", "spls.csv", "srcl.csv", "sre.csv", "sti.csv", "stt.csv", "stx.csv", "stz.csv", "swk.csv", "swks.csv", "swn.csv", "syf.csv", "syk.csv", "symc.csv", "syy.csv", "t.csv", "tap.csv", "tdc.csv", "tdg.csv", "tel.csv", "tgna.csv", "tgt.csv", "tif.csv", "tjx.csv", "tmk.csv", "tmo.csv", "trip.csv", "trow.csv", "trv.csv", "tsco.csv", "tsn.csv", "tso.csv", "tss.csv", "twx.csv", "txn.csv", "txt.csv", "uaa.csv", "ual.csv", "udr.csv", "uhs.csv", "ulta.csv", "unh.csv", "unm.csv", "unp.csv", "ups.csv", "urbn.csv", "uri.csv", "usb.csv", "utx.csv", "v.csv", "var.csv", "vfc.csv", "viab.csv", "vlo.csv", "vmc.csv", "vno.csv", "vrsk.csv", "vrsn.csv", "vrtx.csv", "vtr.csv.csv", "vz.csv.csv", "wat.csv.csv", "wba.csv.csv", "wdc.csv.csv", "wec.csv.csv", "wfc.csv.csv", "wfm.csv", "whr.csv", "wltw.csv", "wm.csv", "wmb.csv", "wmt.csv", "wrk.csv", "wu.csv", "wy.csv", "wyn.csv", "wynn.csv", "xec.csv", "xel.csv", "xl.csv", "xlnx.csv", "xom.csv", "xray.csv", "xrx.csv", "xyl.csv", "yhoo.csv", "yum.csv", "zbh.csv", "zion.csv", "zts.csv")
merged_file = NULL

n = length(csv_files)

for (i in 1:n) {
  csv = read.csv(csv_files[i])
  csv = csv[,c("date","close")]
  names(csv) = c("date",csv_files[i])
  if (i == 1) merged_file = csv
  else merged_file = merge(merged_file,csv)
}

write.csv(merged_file, file = "merged-file.csv")

n = ncol(merged_file)
for (i in 2:n) {
  stock_prices = merged_file[,i] 
  
  stock_prices_prev = c(NA,stock_prices[1:(length(stock_prices)-1)]) 
  
  returns = (stock_prices-stock_prices_prev)/stock_prices_prev 
  
  merged_file[,i] = returns 
}

profit = merged_file[2:nrow(merged_file),2:ncol(merged_file)]


# Without local search
ga_res = ga(
  type="real-valued", 
  function(x){-obj(x)}, 
  lower = rep(0,ncol(profit)), 
  upper = rep(1,ncol(profit)), 
  maxiter = 1000,
  run=100, 
  monitor=TRUE,
  seed=1
)


# With local search
new_ga = ga(
            type="real-valued", 
            function(x){-obj(x)}, 
            lower = rep(0,ncol(profit)), 
            upper = rep(1,ncol(profit)),
            pcrossover = 0.8, 
            pmutation = 0.1, 
            updatePop = FALSE,
            postFitness = NULL,
            maxiter = 10000,
            run = 100,
            maxFitness = Inf,
            names = NULL,
            suggestions = NULL, 
            optim = TRUE,
            optimArgs = list(method = "L-BFGS-B", 
                             poptim = 0.05,
                             pressel = 0.5,
                             control = list(fnscale = -1, maxit = 100)),
            keepBest = FALSE,
            parallel = TRUE,
            monitor = if(interactive()) gaMonitor else FALSE,
            seed = NULL)

summary(ga_res)
sol = as.vector(summary(ga_res)$solution)

cbind(names(profit),sol)

results = portfolio_returns(sol)

results
jpeg("asset-alloc - 1.jpeg", width = 1100, height = 700) 
piepercent<- round(100*sol/sum(sol), 1)
companies = c("A", "AAL", "AAP", "AAPL", "ABC", "ABT")
pie(sol, labels = piepercent, col = rainbow(length(sol)), main = "Asset Allocation")
legend("topright", companies, cex = 0.8, fill = rainbow(length(sol)))
dev.off() 
