#HURDAT2 data
#libraries
if(!require('data.table')){install.packages('data.table',dependencies = TRUE); library('data.table')}
#if(!require('maps')) { install.packages('maps', dependencies = TRUE); require('maps')}
#if(!require('MASS')) { install.packages('MASS', dependencies = TRUE); require('MASS')}
if(!require('ggplot2')) { install.packages('ggplot2', dependencies = TRUE); require('ggplot2')}
if(!require('ggmap')) { install.packages('ggmap', dependencies = TRUE); require('ggmap')}
if(!require('colorRamps')) { install.packages('colorRamps', dependencies = TRUE); library('colorRamps')}
if(!require('HURDAT')) { install.packages('HURDAT', dependencies = TRUE); require('HURDAT')}
if(!require('dplyr')) { install.packages('dplyr', dependencies = TRUE); require('dplyr')}
#set up twitter
if(!require('twitteR')){install.packages('twitteR', dependencies =TRUE); library('twitteR')}
if(!require(httr)){install.packages('httr', dependencies = TRUE);library(httr)}
#login to twitter API: Tropical Cyclone
#setup_twitter_oauth(ckey, csecret, atoken, asecret)
setup_twitter_oauth('YxHk6vMBofPUKTzn3lbnuhWSY',
                    'E5Aj7UodHyjM5yP6eGjs54KGSXKOGRJ9BnzmMe6N5V2X5XZfhC', '4745829336-J3vndMsTB1oBdMgnEH8YISmaqVRCzXa16zRlIZe', 'x7VF1cOcxuj0WvSvElJK1yYe7gQWiZfmiE05U2TKzehsY')
#working directory
if(!file.exists('ca')){dir.create('ca');setwd('ca')};setwd('ca')
#HURDAT2 data
if(file.exists('ca.RDS')){ca=readRDS('ca.RDS')}
ca=data.table(get_hurdat()) #dim 76559    21
dim(ca)
#Arrange data
colnames(ca)=c('ID','Name','Date','Record', 'Type', 'Latitude', 
               'Longitude', 'Wind', 'Pressure', 
               'NE34','SE34','SW34','NW34',
               'NE50','SE50','SW50','NW50',
               'NE64','SE64','SW64','NW64')
ca$Basin=substr(ca$ID, 1, 2) # add Basin column
ca$Ordinality=as.numeric(substr(ca$ID,3,4)) # add year's storm number column
ca$Year=factor(format(ca$Date,'%Y'))
setcolorder(ca, c('ID','Basin','Ordinality','Name','Year','Date','Record', 'Type',
                  'Latitude', 'Longitude', 'Wind', 'Pressure', 
                  'NE34','SE34','SW34','NW34',
                  'NE50','SE50','SW50','NW50',
                  'NE64','SE64','SW64','NW64')) #rearrange columns
if(!file.exists('ca.RDS')){saveRDS(ca,'ca.RDS')}
#daily plots
repeat{Sys.sleep(1)
#timer
  if(format(Sys.time(),"%H%M%S")=='080000'){
#daily data
  year=format(Sys.Date(),'%Y')
  month=format(Sys.Date(),'%m')
  day=format(Sys.Date(),'%d')
  ca.d=ca[format(ca$Date,'%m')==month&format(ca$Date,'%d')==day]
  xl=c(min(ca.d$Longitude),max(ca.d$Longitude))
  yl=c(min(ca.d$Latitude),max(ca.d$Latitude))
  ca.d.max=ca[ID==ca.d$ID[which.max(ca.d$Wind)]]
#Trajectories
  f=paste('caNAEP',month,day ,'.jpg',sep='')
  dev.new()
  jpeg(f, height=3, width=6, units='in', res=600)
  ggplot(data = map_data("world"), aes(long, lat, group = group)) +
    geom_path(color='gray') + theme_bw() +
    geom_point(data=ca.d, aes(x=Longitude, y=Latitude,group=ID,color=Wind))+
    scale_color_gradientn(colours=matlab.like(10)) +
    geom_path(data=ca.d, aes(x=Longitude, y=Latitude,group=ID) ) + 
    labs(title=paste('Historical tropical cyclone advices close America on',month.name[as.integer(month)],day),x='Longitude',y='Latitude',color='Wind (kt)', subtitle=paste('The strongest is ',ca.d.max$Name[1],ca.d.max$Year[1],'*\nRoca Flores E @eliorocaflores elioroca@gmail.com  2017. HURDAT2 data',sep="")  )+
    geom_text(data=ca.d.max[which(format(ca.d.max$Date,'%d')==day)][1], aes(x=Longitude, y=Latitude,label='*',group =ID)  ) +
    scale_x_continuous(limits=xl) +scale_y_continuous(limits=yl)
  dev.off() #close plot window #always close device  
  tweet(paste('Historical tropical cyclone (#hurricane) activity in #NorthAmerica for #',month.name[as.integer(month)],day,' (#HURDAT2) @timtrice @philklotzbach @JBElsner @ricobert1',sep=''), mediaPath = f)
#Mexico
  ca.d.mx=ca.d[-120<=Longitude&Longitude<=-80&5<=Latitude&Latitude<=35]
  ca.d.mx.max=ca[ID==ca.d.mx$ID[which.max(ca.d.mx$Wind)]]
  f=paste('caMX',month,day ,'.jpeg',sep='')
  jpeg(f, height=3, width=5, units='in', res=100) #the image must have labs inside to work tweet
  ggmap(get_map(location=c(-100,20) , zoom = 4, maptype = 'terrain'))+
      geom_point(data=ca.d.mx, aes(x=Longitude, y=Latitude,group=ID,color=Wind))+
      scale_color_gradientn(colours=matlab.like(10)) +
      geom_path(data=ca.d.mx, aes(x=Longitude, y=Latitude,group=ID) ) +
      labs(title=paste(ca.d.mx.max$Name,format(ca.d.mx.max$Date,'%Y'),"* is the strongest tropical cyclone \nnear Mexico for ",month.name[as.integer(month)]," ",day," from 1851 to 2016",sep=""),x='Longitude',y='Latitude',color='Wind (kn)', subtitle='Roca Flores E @eliorocaflores 2017. HURDAT2 data') +
      geom_text(data=ca.d.mx.max[which(format(ca.d.mx.max$Date,'%d')==day)][1], aes(x=Longitude, y=Latitude,label='*' ) ) +
      scale_x_continuous(limits=c(-120,-80)) +scale_y_continuous(limits=c(5,35))
  dev.off()
  tweet(paste('Reportes históricos de ciclones tropicales cerca de México para #',month.name[as.integer(month)],day,'. #HURDAT2 @ciclotrop @conagua_clima',sep=''), mediaPath = f)
  ca.d.mx[Type=='HU'&Record=='L']
#Caribbean
  ca.d.qroo=ca.d[15<=Latitude&Latitude<=25&-90<=Longitude&Longitude<=-80]
  f=paste('caQROO',month,day ,'.jpeg',sep='')
  jpeg(f, height=4, width=7, units='in', res=100)
  ggmap(get_map(location=c(-85,20) , zoom = 6, maptype = 'terrain')) +
  geom_point(data=ca.d.qroo, aes(x=Longitude, y=Latitude,color=Wind,shape=Year) ) +
  geom_path(data=ca.d.qroo, aes(x=Longitude, y=Latitude,group=ID,color=Wind) ) +
  scale_shape_manual(values=1:length(unique(ca.d.qroo$Year))) +
  labs(title=paste("Historical cyclone advices on",month.name[as.integer(month)],day,"from 1851 to 2016"),x='Longitude',y='Latitude',color='Wind (kt)',shape='Year', subtitle='Roca Flores E @eliorocaflores elioroca@gmail.com  2017. HURDAT2 data')  +
  scale_color_gradientn(colours=matlab.like(10)) + scale_x_continuous(limits = c(-90, -80))+ scale_y_continuous(limits = c(15, 25))
  dev.off()
  tweet(paste('Reportes históricos de ciclones tropicales (#huracanes) cerca de #QRoo para #',month.name[as.integer(month)],day,'. #HURDAT2 @ciclotrop @conagua_clima',sep=''), mediaPath = f)
#Strongest hurricane
  xl=c(min(ca.d.max$Longitude),max(ca.d.max$Longitude))
  yl=c(min(ca.d.max$Latitude),max(ca.d.max$Latitude))
  mapart=get_map(location=c(mean(xl),mean(yl)), zoom=4, maptype='watercolor', source="stamen") #zoom higher than 3
  f=paste('camax',month,day ,'.jpeg',sep='')
  jpeg(f, height=4, width=8, units='in', res=100)
  ggmap(mapart)+ geom_path(data=ca.d.max, aes(x=Longitude, y=Latitude,group=Year) )+ 
    geom_point(data=ca.d.max, aes(x=Longitude, y=Latitude,color=Wind) )+ 
    geom_point(data=ca.d.max, aes(x=Longitude, y=Latitude,color=Wind,size=NE34) )+
    geom_point(data=ca.d.max, aes(x=Longitude, y=Latitude,size=NE34),color='blue',alpha=.01,size=30)+
    labs(title=paste(ca.d.max$Name,format(ca.d.max$Date,'%Y')," is the strongest Tropical Cyclone \nnear America for ",month.name[as.integer(month)]," ",day," from 1851 to 2016",sep=""),x='Longitude',y='Latitude',color='Wind (kn)',size='NE 34kt radius \n(nm)', subtitle='Roca Flores E @eliorocaflores elioroca@gmail.com  2017. HURDAT2 data') +
    geom_text(data=ca.d.max[which(format(ca.d.max$Date,'%d')==day)[1]], aes(x=Longitude, y=Latitude, label='o')  )  +
    scale_color_gradientn(colours=matlab.like(10)) 
  dev.off()
  tweet(paste('Strongest #hurricane near #America for #',month.name[as.integer(month)],day ,' #HURDAT2  @timtrice @philklotzbach @JBElsner @ricobert1 @tjagger',sep=''), mediaPath = f)
  }
}
#TAKE 10 MINUTES
#close
  setwd("C:/Users/e205/Documents/R")
