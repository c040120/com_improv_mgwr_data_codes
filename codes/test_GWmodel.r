library(sp)
library(GWmodel)
setwd('/Users/Ziqi/Desktop/MGWR computational improvements')


for (size in c(1000,2000,5000,10000)){
    for (k in c(10)){
        name = paste0("test0314_sub_", size, "_", k, ".csv")
        df = read.csv(name)
		    names(df)[1] = 'id'
        coordinates(df) <- ~x_coord+y_coord
        start_time <- Sys.time()
		    start_mem <- memory.size()
        DM<-gw.dist(dp.locat=coordinates(df))
        bw<-bw.gwr(Y_new~.-id - x_coord - y_coord - 1, approach='aic',data=df,kernel='bisquare',adaptive=T,dMat=DM)
        mgwr_result<- gwr.multiscale(Y_new~.-id - x_coord - y_coord -1, data=df,criterion="dCVR",
						kernel="bisquare",adaptive=T,threshold=1e-5,max.iterations=200,bws0=rep(bw, k),
						dMats=rep(list(DM), k), bw.seled=rep(F, k),approach='aic',hatmatrix=T)
        end_time <- Sys.time()
		    end_mem <- memory.size()
        dt <- difftime(end_time,start_time,units="sec")
		    dmem <- end_mem - start_mem
        write.csv(data.frame(c(dt,start_mem,end_mem,dmem)),paste0("GW_model_rslt_", size, "_", k, ".csv"))
    }
    
}

