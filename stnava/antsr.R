

booleval<-FALSE
library(knitr)
options(width=50)
opts_chunk$set(fig.path='figure/antsr-',fig.align='center',fig.show='hold',size='footnotesize')
knit_theme$set(  knit_theme$get("seashell")  )
getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")
library( ANTsR )
library( oro.nifti )
library( visreg )
library( ggplot2 )
library( boot )
library( candisc )
library( pheatmap ) 
myregion<-"CentralSulcus"



dd<-read.csv('RMI_Data/RMI.csv')
mdl<-lm( NPublications ~ Year +I(Year^2), data = dd)
visreg(mdl,main='Total Publications In Year X')



# some setup
options(width=60)  # make the printing fit on the page
set.seed(1121)   # make the results repeatable
library( ANTsR )
library( oro.nifti )



pkgnames <- c( "visreg" , "boot" , "rgl" , "knitr" , "ggplot2"
,"oro.nifti" , "candisc" , "pheatmap" )
k<-length( pkgnames )



## install.packages( pkgnames )



# par(las=1,mar=c(4,4,.1,.1))  # tick labels direction



x <- rnorm( 20 )  
boxplot( x ) 
hist( x , main='', col="blue", probability = TRUE ) 
lines( density( x ), col = "red" )



as.numeric( as.character( c("0.5",0.1,0.6,"A") ) )



mtcars[c(1,13,28),1:6]



myform<-paste( colnames( mtcars )[2:ncol(mtcars)] , collapse='+'  )
myform<-as.formula( paste( "mpg~", myform ) )
mdl <- lm(  myform , data = mtcars ) 
mdla<-stepAIC( mdl , direction =  c("both" ) )



print( summary( lm(  mdla$call$formula , data = mtcars ) ) )



mdl <- lm(  mdla$call$formula , data = mtcars ) 
visreg( mdl, xvar="wt")
visreg( mdl, xvar="qsec")
visreg( mdl, xvar="am")



mtcars$am<-as.factor( mtcars$am )
mdl <- lm(  mdla$call$formula , data = mtcars ) 
visreg( mdl, xvar="wt")
visreg( mdl, xvar="qsec")
visreg( mdl, xvar="am")



 coplot(mpg ~ wt | qsec , data = mtcars, panel = panel.smooth, rows = 1)



nvox    <- 100
imgvec <- rnorm( nvox )
mydat<-data.frame( space = 1:nvox, imgvec = imgvec )
ggplot(data=mydat, aes(x=space, y=imgvec, group=1)) + geom_line()



nSubjectsOrTimePoints <- 5
imgmat <- matrix( rep( NA, nSubjectsOrTimePoints * nvox ) , ncol =
nvox )
for ( i in 1:nSubjectsOrTimePoints ) {
  imgmat[ i , ] <- rnorm( nvox )
}
image( imgmat )  # try antsImageWrite( as.antsImage( imgmat ), "imgmat.mha" )



fn<-getANTsRData( "ch2" , usefixedlocation = FALSE  )
print( fn )

# oro.nifti 
colin <- readNIfTI( fn )

# antsr
colina <- antsImageRead( fn, 3 )



orthographic( as.array( colina ), oma=rep(2,4))



fn<-"figure/antsrviz.jpeg"
plotANTsImage( as.antsImage( colin ), slices="50x140x5",
  outname=fn)



imgvec<-colina[  colina > 50  ]
print( length( imgvec ) )



imgvec<-colin[  colin > 50  ]
print( length( imgvec ) )



predictor<-as.factor( read.csv("RMI_Data/phantpredictors.csv")$dx )
gvol<-read.csv("RMI_Data/globalvols.csv")
attach( gvol )
mdl<-lm( vol ~  predictor  )





summary(mdl) 





visreg(mdl) 




mask<-antsImageRead( "RMI_Data/phantmask.nii.gz", 2 )
logjac<-read.csv("RMI_Data/phantomGlogjacs.csv") # a population of images 
attach( logjac )
nvox<-ncol(logjac)
pvals<-rep(NA,nvox)
for ( x in c(1:nvox) ) 
{ 
  voxels<-logjac[,x]
  lmres<-summary(lm( voxels ~  predictor  ))
  coeff<-coefficients( lmres )
  pval<-coeff[2,4]
  pvals[x]<-pval
}
qvals<-p.adjust(pvals,method="BH")
print(min(qvals))
pvali<-antsImageClone( mask )
pvali[ mask > 0 ]<-1-qvals 
plotANTsImage( mask, functional= list( pvali ), threshold="0.99x1" ,
outname="figure/lmreg.jpeg" )





continuousDX <- 1-as.numeric(predictor) 
continuousDX2 <- gvol
mypreds<-as.matrix( cbind( continuousDX, continuousDX2 ) )
sccan<-sparseDecom2( inmatrix=list( as.matrix(logjac), mypreds ),
inmask = c( mask , NA ) ,
   sparseness=c( 0.25, -1 ), nvecs=1, its=2, smooth=1, perms=200 ) 
sccansol <- sccan$eig1[[1]]
sccansol[ mask > 0 ] <- sccansol[ mask > 0 ] / max( sccansol[ mask > 0
] )
plotANTsImage( mask, functional= list( sccansol ),
  threshold="0.05x1", outname="figure/mvarreg.jpeg" )




nki<-read.csv('RMI_Data/labelresultsN.csv')
print(names(nki)[1:8])
image(cor(as.matrix(nki[,4:37])))



mdl1<-lm( LABEL_14 ~ SEX + VOLUME , data=nki )
mdl2<-lm( LABEL_14 ~ SEX + VOLUME + AGE + I(AGE^2) , data=nki )
print( anova(mdl1,mdl2) )
jpeg(file="./figure/antsrAge.jpeg",quality = 95, width=900,height=450)
visreg(mdl2,xvar="AGE")
dev.off()



## mysolution=svd(X)  # or prcomp(X) if X not centered



PCbiplot <- function(PC, x="PC1", y="PC2") {
    # PC being a prcomp object
    data <- data.frame(obsnames=row.names(PC$x), PC$x)
    plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
    plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
        )
    datapc <- transform(datapc,
            v1 = .7 * mult * (get(x)),
            v2 = .7 * mult * (get(y))
            )
    plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), alpha=0.75, color="red")
    plot
}
fit <- prcomp(USArrests, scale=T)



PCbiplot(fit)



 enginedata<-mtcars[,c(2,3,4,11)]
 outputdata<-mtcars[,c(1,7)]
 mycca<-cancor( enginedata, outputdata )



print( mycca )



heplot(mycca, xpd=TRUE, scale=0.3)



pbacc<-list.files(path = "./RMI_Data", pattern = glob2rx("pbac*csv")  ,
full.names = T )
pbacTEcog<-read.csv(pbacc[1])
pbacTRcog<-read.csv(pbacc[2])
pbaci<-list.files(path = "./RMI_Data", pattern = glob2rx("pbac*mha") ,
full.names = T )
pbacTEimg<-as.matrix( antsImageRead(pbaci[1], 2 ) )
pbacTRimg<-as.matrix( antsImageRead(pbaci[2], 2 ) )



pbacTRcog[c(1,13,28),1:6]
# also pbac imaging data comes from this mask 
mask<-antsImageRead( list.files(path = "./RMI_Data", pattern=glob2rx("gmask_2mmb.nii.gz") , full.names=T ) , 3 )
# with anatomical labels 
pbacaal<-antsImageRead( list.files(path = "./RMI_Data",
pattern=glob2rx("pbac_aal.nii.gz"), full.names=T ) , 3 )
data("aal",package="ANTsR") # description of aal



inmask <-  mask > 0.5 
mylabs<-sort( unique( pbacaal[ inmask  &  pbacaal > 0.5 &  pbacaal <
91   &  pbacaal != 51 &  pbacaal != 52 &  pbacaal != 53 &  pbacaal != 54 ] ) )
roimatrix<-matrix( rep( NA, length( mylabs ) * nrow( pbacTRimg ) ) ,
ncol=length(mylabs ) )
for ( i in 1:length(mylabs) ) {
# get vector for this label 
  labelVec <- as.numeric( pbacaal[ inmask ] == mylabs[ i ] )   
  roimatrix[   , i  ] <- pbacTRimg %*% ( labelVec / sum( labelVec ) ) 
  }
colnames( roimatrix ) <- aal$label_name[ mylabs ]
mydf<-data.frame( pbacTRcog, roimatrix ) 



pheatmap( cor( pbacTRcog ) , cluster_rows = F , cluster_cols =  F )



stars( pbacTRcog,
       labels = row.names(mydf), cex = 0.2, scale = TRUE, radius = FALSE, full = TRUE, flip.labels = FALSE,
       mar = c( 0, 0, 2, 0 ), main = "Brain Constellation Map of PBAC Cognition" )



pheatmap( cor( roimatrix ) , cluster_rows = F , cluster_cols =  F )



stars( roimatrix,
       labels = row.names(mydf), cex = 0.2, scale = TRUE, radius = FALSE, full = TRUE, flip.labels = FALSE,
       mar = c( 0, 0, 2, 0 ), main = "Brain Constellation Map of PBAC ROIs" )



pheatmap( cor( pbacTRcog , roimatrix ) , cluster_rows = F , cluster_cols =  F )



myform<-paste( colnames( roimatrix ), collapse='+'  )
myform<-as.formula( paste( "delay_free_adj~", myform , "+edu") )
mydf<-data.frame( pbacTRcog, roimatrix ) 
row.names(mydf)<- paste( c(1:nrow(pbacTRcog)),"_",as.character( pbacTRcog$mmse ),sep='')
mdl <- lm(  myform , data = mydf ) 
mdla<-stepAIC( mdl , direction =  c("forward" ) , k = 20 , steps= 20  )
ageregions<-gsub("_","",as.character(mdla$call$formula)[3])



visreg( mdla, xvar="Angular_L")
visreg( mdla, xvar="Frontal_Mid_R")
visreg( mdla, xvar="Temporal_Pole_Sup_L")



 coplot( delay_free_adj ~ Angular_L + Frontal_Mid_R + Temporal_Pole_Sup_L | age , data = mydf , panel = panel.smooth, rows = 1)



nv<-5
mysccan<-sparseDecom2( inmatrix=list( as.matrix(pbacTRcog), pbacTRimg
) , inmask=c( NA , mask ), smooth = 1 , statdir ="/tmp/" , robust = 1 ,
  sparseness=c( -0.07, 0.2 ), nvecs=nv, its=3 , perms=0, cthresh=c(0,250) ) 



for ( ind in 1:nv ) {
  mytests<-names( pbacTRcog )[ abs( mysccan$eig1[,ind] ) > 0 ]
  myform<-paste( mytests , collapse="+" )
  vec<-antsImageRead( paste("/tmp/sccaView2vec00",ind-1,".nii.gz",sep='') ,  3 )
  vec<-vec[ inmask ]
  traindf<-data.frame( gm= pbacTRimg %*% vec, pbacTRcog )
  myform<-as.formula( paste( "gm~",myform) )
  predlm<-lm(  myform , data=traindf )
  predcog<-predict( predlm , newdata=pbacTRcog )
  gmtest<-c( pbacTRimg %*% vec )
  print( myform ) 
  print( paste("Train Correlation:",ind, cor.test( gmtest, predcog)$est  ) )
}



testroi<-matrix( rep( NA, length( mylabs ) * nrow( pbacTEimg ) ) ,ncol=length(mylabs ) )
for ( i in 1:length(mylabs) ) {
  labelVec <- as.numeric( pbacaal[ inmask ] == mylabs[ i ] )   
  testroi[   , i  ] <- pbacTEimg %*% ( labelVec / sum( labelVec ) ) 
  }
colnames( testroi )<-colnames( roimatrix )
testdf<-data.frame( testroi , edu = pbacTEcog$edu )



predcog<-predict( mdla , newdata=testdf )
print( paste("Test Correlation:", cor.test( pbacTEcog$delay_free_adj, predcog)$est  ) )
predmdl<-lm(  predcog ~ 1 + delay_free_adj, data = pbacTEcog )
visreg( predmdl )





for ( ind in 1:nv ) {
  mytests<-names( pbacTRcog )[ abs( mysccan$eig1[,ind] ) > 0 ]
  myform<-paste( mytests , collapse="+" )
  vec<-antsImageRead( paste("/tmp/sccaView2vec00",ind-1,".nii.gz",sep='') ,  3 )
  vec<-vec[ inmask ]
  traindf<-data.frame( gm= pbacTRimg %*% vec, cog = as.matrix( pbacTRcog) %*% mysccan$eig1[,ind ] )
  cogtest <- as.matrix( pbacTEcog) %*% mysccan$eig1[,ind ] 
  gmtest<-c( pbacTEimg %*% vec )
  print( myform ) 
  print( paste("Test Correlation:",ind, cor.test( gmtest, cogtest)$est  ) )
}



predmdl<-lm(  cogtest ~ c(gmtest) , data = pbacTEcog )
print( paste("Test Correlation:", cor.test( cogtest, gmtest )$est  ) )
visreg( predmdl )



dx<-as.factor( pbacTRcog$mmse < 26  )
dx<-as.factor( pbacTRcog$fluency_adj < 2.6  )
traindata<-data.frame( dx=dx, roimatrix )
myform<-paste( "dx~", paste( colnames( roimatrix )[1:20] , collapse='+' ) )
mdl<-glm( as.formula(myform), data=traindata, family="binomial")
dd<-0 ; ntests<-20
for ( i in 1:ntests ) dd<-dd+cv.glm(traindata, mdl,K=5)$delta[1]* ( 1/ ntests )



print(paste("prediction % misclassification" , dd * ntests ) )



avgimg <- function(  mylist , mask )
{
avg<-antsImageClone( mylist[[1]] )
avg[ mask == 1 ]<-0
for ( i in 1:length(mylist) ) 
  {
  avg[ mask == 1 ] <- avg[ mask == 1 ] + mylist[[i]][ mask == 1 ] * 1/length(mylist)
  }
 return( avg )
}



sdimg <- function(  mylist , mask )
{
avg<-avgimg( mylist , mask )
sdi<-antsImageClone( avg )
sdi[ mask == 1 ]<-0
for ( i in 1:length(mylist) ) 
  {
  sdi[ mask == 1 ] <- sdi[ mask == 1 ] + abs( mylist[[i]][ mask == 1 ] - avg[ mask == 1 ] )  * 1/length(mylist)
  }
 return( sdi )
}



interleave <- function(v1,v2)
{
ord1 <- 2*(1:length(v1))-1
ord2 <- 2*(1:length(v2))
c(v1,v2)[order(c(ord1,ord2))]
}



blockfing = c(0, 36, 72, 108,144)
blockfoot = c(12, 48, 84, 120, 156)
ct<-1 
fn<-c('RMI_Data/fmri_motor_sub1_s1.nii.gz','RMI_Data/fmri_motor_sub1_s2.nii.gz')[1]
pre<-paste('fmri_motor_',ct,sep='')



  fmri<-antsImageRead( fn , 4 )
  hrf <- hemodynamicRF( scans=dim(fmri)[4] , onsets=blockfing , durations=rep(  12,  length( blockfing ) ) ,  rt=2.5 )
  hrf[1:4]<-NA # first few frames are junk 
  myvars<-getfMRInuisanceVariables( fmri, moreaccurate = FALSE ,
  maskThresh=100 )



  mat<-myvars$matrixTimeSeries
  avg<-myvars$avgImage
  mask<-myvars$mask 
  nuis<-( myvars$nuisancevariables )
  print( colnames( nuis ) )
  antsImageWrite(avg,paste(pre,"avg.nii.gz",sep=""))
  plotANTsImage(myantsimage = avg, functional = list(mask), slices =
  "12x20x3",    axis = 3, threshold = "0.5x1.5")



  globsig <- myvars$globalsignal 
  betas<-rep(NA, ncol( mat ) )
  for ( i in 1:ncol(mat) ) {
    vox<-mat[ , i ]
    mdl<-lm( vox ~  hrf + globsig + motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data = data.frame( nuis )  ) 
    betas[i]<-coefficients(summary(mdl))[2,3] # probably better way
    }
  betaimg<-antsImageClone( mask ) # put beta vals in image space
  betaimg[ mask > 0.5 ] <- betas 
  print( max( abs( betas ) ) )  # around 10 or so 
  # much much faster but i haven't figured out how to get results out easily
  fastResults<-lm( mat[,1:2] ~  hrf +  myvars$globalsignal  + motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data = data.frame( nuis )  ) 
  antsImageWrite(betaimg, paste(pre,"betas.nii.gz",sep="") )



  mx<-max( betas ) 
  plotANTsImage(myantsimage = avg, functional = list(betaimg), slices ="15x20x1",    axis = 3, threshold = paste("9x",mx,sep=''),outname="figure/antsrLMmot1.jpeg")
 if ( ! exists("mymni") ) {
    mymni<-list( antsImageRead(getANTsRData('mni'),3), 
                 antsImageRead(getANTsRData('mnib'),3), 
                 antsImageRead(getANTsRData('mnia'),3) )
  }
  clust<-antsImageClone( betaimg )
  clust <- labelClusters( clust , minClusterSize=10, minThresh=5, maxThresh=100)
  ofn <- paste("/tmp/",pre,sep='')



##  gcoords<-getTemplateCoordinates( list( avg , clust ) , mymni ,  convertToTal = TRUE , outprefix = ofn )
##  print( gcoords$templatepoints )
##  myregion<-sub("_","",gcoords$templatepoints$AAL[1])



  # quick multivariate version 
  rmat<-residuals( lm( mat ~  myvars$globalsignal + motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data = data.frame( nuis )  ) )
  cblock <- as.numeric(hrf[5:length(hrf)]) 
  cblock2 <- as.numeric( cblock > 0 )
  mypreds<-as.matrix( cbind( cblock, cblock2 ) )
  sccan<-sparseDecom2( inmatrix=list( rmat[5:length(hrf),] , mypreds ), inmask = c( mask , NA ) ,
  sparseness=c( 0.03, 1 ), nvecs=1, its=3, smooth=0, perms=10, cthresh = c(20, 0) ) 
  antsImageWrite( sccan$eig1[[1]] ,  paste(pre,"sccan.nii.gz",sep="" )  )
  eigimg <- sccan$eig1[[1]]
  ImageMath(3,eigimg,"Normalize",eigimg)
  plotANTsImage(myantsimage = avg, functional = list(eigimg), slices ="15x20x1",    axis = 3, threshold = "0.1x1",outname="figure/antsrsccanmot1.jpeg")



nvecs<-11
ff <- sparseDecom(rmat[!is.na(hrf), ], mask, 1.25/nvecs, nvecs,
    its = 5, cthresh = 5, smooth = 1, z = -0.9 , statdir ="/tmp/" )
for ( i in 1:nvecs ) {
  print(paste("Test",i))
  mdl<-lm( ff$projections[,i]  ~  cblock + myvars$globalsignal[  !is.na(hrf)] +  motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data =  data.frame( nuis[!is.na(hrf),]  ) )
  print( summary( mdl ) )
}
dat<-data.frame(  time = ( (1:length(hrf[  !is.na(hrf)] ))*2.5  ), signal = ff$projections[, 2],
nuis= ff$projections[, 3] , hrf = hrf[  !is.na(hrf)] ) 



jpeg(file="./figure/anstrboldsig1.jpeg",width=1024,height=256,quality=90)
ggplot( data=dat, aes(x=time, y=signal) ) + geom_line() + ylab("BOLD
Signal") + xlab( "time (ms)" )
dev.off()
jpeg(file="./figure/anstrboldsigN.jpeg",width=1024,height=256,quality=90)
ggplot( data=dat, aes(x=time, y=nuis) ) + geom_line() + ylab("BOLD
Nuisance") + xlab( "time (ms)" )
dev.off()
jpeg(file="./figure/anstrboldHRF.jpeg",width=1024,height=256,quality=90)
ggplot( data=dat, aes(x=time, y=hrf) ) + geom_line() + ylab("HRF") + xlab( "time (ms)" )
dev.off()



  mdl<-lm( ff$projections[,2]  ~  cblock + myvars$globalsignal[
  !is.na(hrf)] +  motion1 + motion2 + motion3 + compcorr1 + compcorr2
  + compcorr3, data =  data.frame( nuis[!is.na(hrf),]  ) )
 print( coefficients(summary(mdl) )[,4] )



  mdl<-lm( ff$projections[,3]  ~  cblock + myvars$globalsignal[  !is.na(hrf)] +  motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data =  data.frame( nuis[!is.na(hrf),]  ) )
 print( coefficients(summary(mdl) )[,4] )



  eigimg <- ff$eigenanatomyimages[[2]]
  ImageMath(3,eigimg,"Normalize",eigimg)
plotANTsImage(myantsimage = avg, functional = list(eigimg), slices ="1x20x1",    axis = 3, threshold = "0.01x1",col="red",outname="figure/antsrsdecom2.jpeg")



  eigimg <- ff$eigenanatomyimages[[3]]
  ImageMath(3,eigimg,"Normalize",eigimg)
plotANTsImage(myantsimage = avg, functional = list(eigimg), slices ="1x20x1",    axis = 3, threshold = "0.01x1",col="blue",outname="figure/antsrsdecom3.jpeg")



if ( FALSE ) {
fmri<-antsImageRead('RMI_Data/fmri_covert_verb_generation_sub1_s2.nii.gz',4)
blocko = c(1,24, 48, 72, 96, 120, 144 )
hrf <- hemodynamicRF( scans=dim(fmri)[4] , onsets=blocko , durations=rep(  12,  length( blocko ) ) ,  rt=2.5 )
hrf[1:4]<-NA # first few frames are junk 
myvars<-getfMRInuisanceVariables( fmri, moreaccurate = TRUE ,maskThresh=100 )
avg<-myvars$avgImage
antsImageWrite(avg,"avg_lang.nii.gz")
mask<-myvars$mask 
mat<-myvars$matrixTimeSeries
# fmri2<-antsImageClone(fmri)
# SmoothImage(4,fmri,1,fmri2)
# mat<-timeseries2matrix( fmri2, mask ) # 
nuis<-( myvars$nuisancevariables )
print( colnames( nuis ) )
plotANTsImage(myantsimage = avg, functional = list(mask), slices = "12x20x3",    axis = 3, threshold = "0.5x1.5")
betas<-rep(NA, ncol( mat ) )
for ( i in 1:ncol(mat) ) 
  {
  vox<-mat[ , i ]
  mdl<-lm( vox ~  hrf + myvars$globalsignal + motion1 + compcorr1 + compcorr2 + compcorr3, data = data.frame( nuis )  )
  betas[i]<-coefficients(summary(mdl))[2,3]
  }
betaimg<-antsImageClone( mask )
betaimg[ mask > 0 ] <- betas 
mx<-max( betas ) 
print( mx )
th<-paste("3x",mx,sep='')
plotANTsImage(myantsimage = avg, functional = list(betaimg), slices ="12x20x3",    axis = 3, threshold = th )
antsImageWrite(betaimg,"betas_lang.nii.gz")
#
# quick multivariate version 
#
mat<-timeseries2matrix(  fmri ,  mask  )
mat<-myvars$matrixTimeSeries
rmat<-residuals( lm( mat ~  myvars$globalsignal + motion1 +  motion2 +  motion3 + compcorr1   + compcorr2+ compcorr3,  data = data.frame( nuis )  ) )
#
#,
cblock <- as.numeric(hrf[5:length(hrf)]) 
cblock2 <- as.numeric( cblock > 0 )
mypreds<-as.matrix( cbind( cblock, cblock2 ) )
sccan<-sparseDecom2( inmatrix=list( rmat[5:length(hrf),] , mypreds ), inmask = c( mask , NA ) ,
   sparseness=c( 0.1, 0.5 ), nvecs=1, its=3, smooth=1, perms=5,
   cthresh = c(11, 0), statdir ="/tmp/" )
antsImageWrite( sccan$eig1[[1]] , 'sccan_lang.nii.gz' )
#
# eigenanatomy 
#
ff<-sparseDecom( rmat[!is.na(hrf),] , mask, 0.1,10,its=5,cthresh=5,smooth=1,z=-0.7)
for ( i in 1:10 ) {
  print(paste("ASSS",i))
  mdl<-lm( ff$projections[,i]  ~  cblock + myvars$globalsignal[  !is.na(hrf)] +  motion1 + motion2 + motion3 + compcorr1 + compcorr2 + compcorr3, data =  data.frame( nuis[!is.na(hrf),]  ) )
  print( summary( mdl ) )
}
}



fns<-Sys.glob( file.path( "./RMI_Data/eld*nii.gz" ) )
asl<-antsImageRead( fns[1] , 4 )
perf<-aslPerfusion( asl, maskThresh=300, moreaccurate=FALSE )
param <- list( sequence="pcasl", m0=perf$m0 )
cbf <- quantifyCBF( perf$perfusion, perf$mask, param )
plotANTsImage( cbf$meancbf ,slices="5x17x3",axis=3,outname="figure/antsrsimpcbf.jpeg")



fns<-Sys.glob(file.path("./RMI_Data/eld*nii.gz"))
asl<-antsImageRead( fns[1] ,4)
seg<-antsImageRead( fns[3] ,3)
mask<-antsImageClone( seg )
mask[ seg > 0 ]<-1
mat<-timeseries2matrix( asl, mask )
cbflist<-list( ) 



for ( i in 1:4 ) {
  timeinds<-sample( 2:nrow(mat) ,
     round( nrow(mat) )*0.3 ) 
  timeinds<-( timeinds %% 2 )+timeinds
  timeinds<-interleave( timeinds-1, timeinds )
  aslarr<-as.array( asl ) 
  aslarr2<-aslarr[,,,timeinds]
  aslsub<-as.antsImage( aslarr2 )
  antsCopyImageInfo( aslsub , asl )
  proc <- aslPerfusion( aslsub, mask=mask, moreaccurate=FALSE ,  dorobust=0 )
  param <- list( sequence="pcasl", m0=proc$m0 )
  cbf <- quantifyCBF( proc$perfusion, mask, param )
  antsImageWrite( cbf$meancbf, 'temp1.nii.gz')
  cbflist<-lappend( cbflist, cbf$meancbf )
}



avgcbf<-avgimg( cbflist , mask )
sdi<-sdimg( cbflist , mask )
avgcbft<-antsImageClone( avgcbf )
avgcbft[ sdi > 25 ] <- 0 
plotANTsImage( avgcbft,
slices="5x17x3",axis=3,outname="figure/antsravgcbft.jpeg")
plotANTsImage( avgcbft, functional=list(sdi), color="red", slices="5x17x3",axis=3 ,threshold="20x55",outname="figure/antsrsdcbf.jpeg")


