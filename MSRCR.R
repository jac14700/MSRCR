library(imager)
setwd("E:\\School stuffs\\Github\\MSRCR")
#load imgs which undergo Gussain filter
im<-as.cimg(load.image("W_1.JPG"))
im_Gaussian1 <- as.cimg(load.image("4out_put.png"))
im_Gaussian2 <- as.cimg(load.image("10out_put.png"))
im_Gaussian3 <- as.cimg(load.image("70out_put.png"))
im_Gaussian4 <- as.cimg(load.image("100out_put.png"))
im_Gaussian5 <- as.cimg(load.image("130out_put.png"))
im_Gaussian6 <- as.cimg(load.image("160out_put.png"))

modify_value<-0.0000000001
{
  {
    im_Gaussian1[which(im_Gaussian1==0)]<-modify_value
    log_im<-log10(im+0.01)
    log_im_Gaussian1<-log10(im_Gaussian1)
    SSR1<-log_im -log_im_Gaussian1
  }  
  {
    im_Gaussian2[which(im_Gaussian2==0)]<-modify_value
    log_im_Gaussian2<-log10(im_Gaussian2)
    SSR2<-log_im -log_im_Gaussian2
  }  
  {
    im_Gaussian3[which(im_Gaussian3==0)]<-modify_value
    log_im_Gaussian3<-log10(im_Gaussian3)
    SSR3<-log_im -log_im_Gaussian3
  }  
  {
    im_Gaussian4[which(im_Gaussian4==0)]<-modify_value
    log_im_Gaussian4<-log10(im_Gaussian4)
    SSR4<-log_im -log_im_Gaussian4
  }  
  {
    im_Gaussian5[which(im_Gaussian5==0)]<-modify_value
    log_im_Gaussian5<-log10(im_Gaussian5)
    SSR5<-log_im -log_im_Gaussian5
  }  
  {
    im_Gaussian6[which(im_Gaussian6==0)]<-modify_value
    log_im_Gaussian6<-log10(im_Gaussian6)
    SSR6<-log_im -log_im_Gaussian6
  }  
}

retinex<-(SSR1+SSR2+SSR3+SSR4+SSR5+SSR6)/6
{
    temp<-retinex
    temp[,,,1]<-(retinex[,,,1]-min(retinex[,,,1])) / (max(retinex[,,,1])-min(retinex[,,,1]))
    temp[,,,2]<-(retinex[,,,2]-min(retinex[,,,2])) / (max(retinex[,,,2])-min(retinex[,,,2]))
    temp[,,,3]<-(retinex[,,,3]-min(retinex[,,,3])) / (max(retinex[,,,3])-min(retinex[,,,3]))
    plot(temp)
}


{
  beta<-46
  alpha<-125
  G<-5
  b<-25
  
  #color restore
  {
    img_sum<-(im[,,,1]+im[,,,2]+im[,,,3])
    channel_1<-log10(im[,,,1]*alpha)-log10(img_sum)
    channel_2<-log10(im[,,,2]*alpha)-log10(img_sum)
    channel_3<-log10(im[,,,3]*alpha)-log10(img_sum)
    color_Restoration<-im
    color_Restoration[,,,1]<-channel_1
    color_Restoration[,,,2]<-channel_2
    color_Restoration[,,,3]<-channel_3
    
    color_Restoration<-beta*(color_Restoration)
    
    img_msrcr<-G*(retinex*color_Restoration+b)
    
    img_msrcr[,,,1]<-(img_msrcr[,,,1]-min(img_msrcr[,,,1])) / (max(img_msrcr[,,,1])-min(img_msrcr[,,,1]))
    img_msrcr[,,,2]<-(img_msrcr[,,,2]-min(img_msrcr[,,,2])) / (max(img_msrcr[,,,2])-min(img_msrcr[,,,2]))
    img_msrcr[,,,3]<-(img_msrcr[,,,3]-min(img_msrcr[,,,3])) / (max(img_msrcr[,,,3])-min(img_msrcr[,,,3]))
    plot(img_msrcr)
  }
  
  {
    width<-length(img_msrcr[,1,1,1])
    height<-length(img_msrcr[1,,1,1])
    total<-width*height
  }
  
  {
    low_clip<-0.01
    hight_clip<-0.99
    
    for (channel in 1:3) 
      {
        part<-c(table(img_msrcr[,,,channel]))
        current<-0
        for (idx in 1:length(part)) {
         
          if((current/total)<low_clip)
            low_value<-as.numeric(names(part[idx]))
          
          if((current/total)>hight_clip)
            hight_value<-as.numeric(names(part[idx]))
          
          current<-as.numeric(part[idx])+current
        } 
        img_msrcr[,,,channel] [which(img_msrcr[,,,channel]>=hight_value)]<-hight_value
        img_msrcr[,,,channel] [which(img_msrcr[,,,channel]<=low_value)]<-low_value
    }
    plot(img_msrcr)
  }
}

save.image(temp, "goodmsrcr.png", quality = 11)