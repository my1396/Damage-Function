## This script bootstraps historical observational datasets to gain 
## a distribution of parameter estimates.

library(broom)
library(plm) 
library(SMUT)
library(plyr)
library(tidyverse)

f_name <- "~/Documents/GDP/Shared folder/data/GDP_reg_panelData.csv"
f_name
Pdata <- read_csv(f_name)
Pdata %>% head(5) %>% as.data.frame()
colnames(Pdata)

target_v <- "logD_gdp"
obs <- sum(!is.na(Pdata[[target_v]])) ## non-NA obs.
obs

N.obs <- Pdata$iso %>% n_distinct()
T.obs <- Pdata$year %>% n_distinct()
cat (sprintf("No.sample: %s \nNo.time: %s \n", N.obs, T.obs) )


## Filter out countries w/ nonstationary gdp and climate ##
CC_list <- Pdata$iso %>% unique()
CC_list
CC_list %>% length() # 122 stationary countries
"KIR" %in% CC_list

## Bootstrap regressions =======================================================
bootstrap_size <- 1000
eff_seed <- sample(1:2^15, 1)
IEres_1000 <- data.frame()
print(sprintf("Seed for session: %s", eff_seed))
# set.seed(eff_seed)
for (bt in 1:bootstrap_size){
    print (bt)
    CC_sample <- CC_list[sample(1:length(CC_list), replace=TRUE)]
    CC_sample
    
    Data             = read.csv("data/cntry_ann_climate_gdpKD_1961to2019.csv", sep = ","
                                , na.strings = "..", dec=".")
    Data             = Data[,c("ISO_C3","year","NY.GDP.PCAP.KD","mean_tmp","mean_pre",
                               "mean_rad")]
    colnames(Data)   = c("iso","year","gdp","tmp","pre","rad")
    Data             = pdata.frame(Data,c("iso","year"), drop.index = FALSE)
    Data             = make.pbalanced(Data, balance.type = "fill")
    Data[,"gdp"]     = diff(log(Data[,"gdp"]), lag = 1)
    Data             = Data[!Data[,"year"]%in%1961,]
    nrow(Data) # 10,324
    
    Data_bootstrap <- data.frame()
    buffer <- 1
    for (CC in CC_sample){
        CC_data <- Data %>% 
            filter(iso == CC) %>% 
            mutate(iso=paste0(iso, buffer))
        Data_bootstrap <- bind_rows(Data_bootstrap, CC_data)
        buffer <- buffer+1
    }
    
    Data <- pdata.frame(Data_bootstrap, c("iso","year"), drop.index = FALSE)
    # print (paste0("Sample size: ", nrow(Data)))
    
    
    BData             = cbind(Data[,c("iso","year","gdp","tmp")],
                              Data$tmp^2, Data$pre*12/1000, (Data$pre*12/1000)^2,
                              as.numeric(Data$year),
                              as.numeric(factor(Data[,c("iso")])) )
    colnames(BData)   = c("iso","year","gdp","tmp","tmp2","pre","pre2",
                          "i.year","i.iso")
    # add interactive terms
    BData             = cbind(BData,
                              BData$tmp*BData$pre, BData$tmp^2*BData$pre,
                              BData$tmp*BData$pre^2, BData$tmp^2*BData$pre^2 )
    colnames(BData)   = c("iso","year","gdp","tmp","tmp2","pre","pre2",
                          "i.year","i.iso",
                          "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2")
    BData[!!rowSums(is.na(BData)),3:ncol(BData)] = NA
    countna   = ddply(BData[,c("iso","gdp")], .(iso), 
                      function(x) sum(is.na(x[,-1])))
    t         = length(unique(BData[,"year"])) 
    BData     = BData[!BData$iso %in% countna[countna[,2]==t,1],]
    # n         = length(unique(BData[,"iso"])) 
    n         = length(CC_sample) 
    obs       = sum(!is.na(BData[,"gdp"]))
    tpcs      = ddply(BData[,c("iso","gdp")], .(iso), 
                      function(x) sum(!is.na(x[,-1])))
    ttrend    = matrix(0,n*t,n)
    for(i in 1:n){ttrend[,i]    = replace(ttrend[,i],(1+t*(i-1)):(t*i),1:t)}
    ttrend2    = matrix(0,n*t,n)
    for(i in 1:n){ttrend2[,i]    = replace(ttrend2[,i],(1+t*(i-1)):(t*i),(1:t)^2)}
    BData             = cbind(BData,ttrend,ttrend2)
    BData[!!rowSums(is.na(BData)),3:ncol(BData)] = NA
    colnames(BData) =  c("iso","year","gdp","tmp","tmp2","pre",
                         "pre2","i.year","i.iso",
                         "tmp_pre", "tmp2_pre", "pre2_tmp", "tmp2_pre2",
                         paste("T",colnames(BData[,-c(1:13)]),sep = "_"))
    
    ## ====================================================================== ##
    # Now we start setting up the PC estimator
    # Obtaining the first round of factors
    # Estimating the within model with defcatored dependent variable
    ## ====================================================================== ##
    ntf        = 2
    nvar       = 8 # No. indep. variables
    CData      = BData[,-(8:9)]
    tdemean    = numcolwise(function(x) mean(x,na.rm = TRUE))
    DTData     = ddply(CData, .(iso), tdemean) #mean over time
    DTData     = pdata.frame(merge(CData[,(1:ntf)],DTData, by.x = "iso", 
                                   by.y= "iso", sort = FALSE), c("iso","year"))
    DNData     = ddply(CData, .(year), tdemean) #mean over csu
    DNData     = pdata.frame(merge(CData[,(1:ntf)],DNData, by.x = "year", 
                                   by.y= "year", sort = FALSE),c("iso","year"))
    DDData     = colMeans(CData[,-(1:ntf)], na.rm = TRUE) #overall mean
    DDData     = pdata.frame(merge(CData[,(1:ntf)],t(DDData)),c("iso","year"))
    AData      = cbind(CData[,1:2],(CData[,-(1:ntf)] - DTData[,-(1:ntf)] - 
                                        DNData[,-(1:ntf)] + DDData[,-(1:ntf)]))
    AData      = pdata.frame(AData,c("iso","year"))
    XData      = as.matrix(AData[,-(1:3)])
    XData[!!rowSums(is.na(CData)),] = NA
    XData[is.na(XData)]   = 0
    YData      = AData[,"gdp"]
    YData[is.na(YData)]   = 0
    Bai1       = solve(t(XData)%*%XData) #does not change across iterations.
    Bai2       = Bai1%*%t(XData)%*%YData #initial estimates of coef.
    resid      = YData - XData%*%Bai2    #residuals
    resid0     = t(matrix(resid, ncol = length(resid)/t , nrow = t)) #NxT
    eiv        = eigen((t(resid0)%*%resid0)/obs) #principal components
    Fmat       = sqrt(t)*eiv$vectors[,1:4] #Fhat Txk
    Lambda     = t((t(Fmat)%*%t(resid0))/t) #Lambdahat Nxk
    LFact      = as.vector(Fmat%*%t(Lambda)) #initial estimates of the factor comp.
    Betas      = Bai2[1:nvar]
    ## ====================================================================== ##
    # Loop for the outer iterations of Bai (2009) for unbalanced panels
    # This loop yields the iterated PC estimator of Bai (2009)
    ## ====================================================================== ##
    reppc = 0
    rescoef = NULL
    repeat
    { bit     = Bai2
    YData0    = YData - LFact
    YData0[is.na(YData)]    = 0
    Bai2      = Bai1%*%t(XData)%*%YData0 #initial estimates of coef.
    resid     = YData - XData%*%Bai2
    resid1    = t(matrix(resid, ncol = length(resid)/t , nrow = t)) #NxT
    resid1[resid0==0]  = LFact[resid0==0] #replacing missing values with pcs 
    eiv       = eigen((t(resid1)%*%resid1)/obs) #principal components
    Fmat      = sqrt(t)*eiv$vectors[,1:4] #Fhat Txk
    Lambda    = t((t(Fmat)%*%t(resid1))/t) #Lambdahat Nxk
    LFact     = as.vector(Fmat%*%t(Lambda)) #NTx1
    reppc     = reppc + 1
    Betas     = rbind(Betas,Bai2[1:nvar])
    if (sqrt(t(Bai2-bit)%*%(Bai2-bit))<=0.0005) break
    }
    Betas %>% str()
    head(Betas,5)
    tail(Betas,5)
    Bai2[1:nvar]
    
    ## ====================================================================== ##
    # Obtaining the standard error of the Bai (2009) estimator
    # we obtained the standard errors as explained on pages 1251- 1252 of Bai (2009)
    ## ====================================================================== ##
    # First we obtain hat(Zit). For this purpose we need hat(F), hat(Lambda)
    # Zit is defined on page 1245. We need Mf and aik. 
    # Below we obtain hat(Mf)
    mfh    = diag(t) - Fmat%*%t(Fmat)/t
    # Below we obain hat(Mf)Xik
    defac  = numcolwise(function(x) mfh%*%x)
    AData0 = AData
    AData0[is.na(AData)]   = 0
    xpc    = pdata.frame(cbind(AData0[,1:ntf],ddply(AData0[,-3], .(iso), defac)[-1]),
                         c("iso","year")) #remove the dependent variable and defactor
    xpc1   = unlist(xpc[-(1:ntf)]) #long vector of all regressors
    xpcar   = array(xpc1,dim = c(t,n,(length(xpc1)/(n*t)))) #array w dim: TxNxk
    # Below we obtain aik and then obtain the second term in the definition of zit
    aik     = Lambda%*%solve(t(Lambda)%*%Lambda/n)%*%t(Lambda)
    xpaik   = array(numeric(),c(n,t,(length(xpc1)/(n*t)))) # array of zeros of NxTxk
    for(i in 1:(length(xpc1)/(n*t))){
        xpaik[,,i] =  t(eigenMapMatMult(xpcar[,,i],aik)) #faster way for multiply
    }
    rm(aik)
    xpaik   = aperm(xpaik, perm = c(2,1,3), resize = TRUE) # size(N,T,k)
    # Final step to obtain zit as in the definition by subtracting the second term
    zit     = xpcar - xpaik/n
    ZData   = matrix(zit, nrow = n*t, ncol = (length(xpc1)/(n*t)))
    rm(xpaik,zit)
    ZData   = pdata.frame(cbind(CData[,1:2],ZData),c("iso","year"))
    colnames(ZData) = colnames(CData[,-3])
    ZData[is.na(AData[,-3])]     = 0 #replacing NAs with zeros
    # Now we have obtained zit. D0 is obtained as follows
    insum   = function(x) t(x[,3:ncol(x)])%*%t(t(x[,3:ncol(x)])) #SofS for i
    insm    = aperm(daply(ZData, "iso", insum),c(2,3,1))
    d0      = apply(insm,c(1,2),sum)/obs
    # Now we obtain the residuals and the variances of the residuals
    residf  = YData - XData%*%Bai2 - LFact
    residf  = pdata.frame(cbind(CData[,1:2],residf)
                          ,c("iso","year"))
    residf[is.na(AData[,1])]     = 0 #replacing NAs with zeros
    eps2    = as.matrix(daply(residf,"iso",insum))/tpcs[,2] #sigma square i
    # this is to obtain d1 
    insm2   = apply(insm, c(1,2), function(x) x*eps2)
    d1      = apply(insm2,c(2,3),sum)/obs
    # standard errors and t values 
    sterpc  = as.matrix(sqrt(diag(solve(d0)%*%d1%*%solve(d0)/obs)))
    tvalpc  = Bai2/sterpc
    plot(Betas[,3])
    
    pvalpc  = 2*pnorm(-abs(tvalpc))
    IEres   = cbind(Bai2[1:10],sterpc[1:10,],tvalpc[1:10,],pvalpc[1:10,])
    colnames(IEres) <- c("estimate", "std.error", "t.stat", "p.value")
    IEres <- as_tibble(IEres) %>% 
        mutate(pval.symbol = addPval.symbol(p.value),
               item = rownames(Bai2)[1:10] ) %>% 
        select(item, everything() )
    IEres <- IEres %>% mutate(bootstrap=bt) # add bootstrap identifier
    IEres
    IEres_1000 <- bind_rows(IEres_1000, IEres)
}

IEres_1000 %>% colnames()
IEres_1000 %>% dim()
IEres_1000$bootstrap %>% unique()
IEres_1000 %>% filter(bootstrap==58)

f_name <- "./data/bootstrap58_coef.csv"
write_csv(IEres_1000, f_name)
#




