res<-read.delim("data/LengthResults - ID copy.txt",stringsAsFactors=F)
tax<-read.csv("data/morphimage.csv",header=F,stringsAsFactors=F)
colnames(tax)<-c("sample","count","difficulty")
tax$sample<-paste("M",tax$sample,sep="")
res$Image<-sapply(strsplit(as.character(res$Image),".",fixed=T),function(x) x[1])
res<-res[res$Taxon!="Debris",]
rescounts<-table(res$Image)
rescounts<-c(rescounts,rescounts["M23-3R4ne-tray1"]+rescounts["M23-3R4ne-tray2"])
names(rescounts)[length(rescounts)]<-"M23-3R4ne"
tax$rescounts<-rescounts[tax$sample]
tax$props<-(tax$rescounts-tax$count)/tax$count

hist(res$Length,breaks=50)

library(ggplot2)

ggplot( tax, aes( y=rescounts, x=count ) ) + 
  stat_smooth( method="lm", mapping=aes(colour=difficulty),size=1.2,fullrange=F) +
  geom_point( aes(fill=difficulty),shape=21,size=5 ) + 
  ylab("Microscope Count") + xlab("Image Count") +
  geom_segment(x=0,y=0,xend=4000,yend=4000) + scale_colour_discrete(breaks=c("High","Med","Low")) +
  scale_fill_discrete(breaks=c("High","Med","Low")) +
  #scale_fill_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("white","grey","black")) + 
  #scale_linetype_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("dashed", "solid"))+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16)) + theme_bw()

taxlow<-tax[tax$difficulty=="Low",]
taxmed<-tax[tax$difficulty=="Med",]
taxhigh<-tax[tax$difficulty=="High",]

ggplot( taxlow, aes( y=rescounts, x=count ) ) + 
  stat_smooth( method="lm",size=1.2,fullrange=F) +
  geom_point( shape=21,size=5,fill="black" ) + 
  ylab("Microscope Count") + xlab("Image Count") +
  geom_segment(x=0,y=0,xend=4000,yend=4000) +
  #scale_fill_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("white","grey","black")) + 
  #scale_linetype_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("dashed", "solid"))+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16)) + theme_bw()

ggplot( taxmed, aes( y=rescounts, x=count ) ) + 
  stat_smooth( method="lm",size=1.2,fullrange=F) +
  geom_point( shape=21,size=5,fill="black" ) + 
  ylab("Microscope Count") + xlab("Image Count") +
  geom_segment(x=0,y=0,xend=4000,yend=4000) +
  #scale_fill_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("white","grey","black")) + 
  #scale_linetype_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("dashed", "solid"))+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16)) + theme_bw()

ggplot( na.omit(taxhigh), aes( y=rescounts, x=count ) ) + 
  stat_smooth( method="lm",size=1.2,fullrange=F) +
  geom_point( shape=21,size=5,fill="black" ) + 
  ylab("Microscope Count") + xlab("Image Count") +
  geom_segment(x=0,y=0,xend=2000,yend=2000) +
  #scale_fill_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("white","grey","black")) + 
  #scale_linetype_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("dashed", "solid"))+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16)) + theme_bw()


ggplot( tax, aes( y=props, x=count ) ) + 
  stat_smooth( method="lm", mapping=aes(colour=difficulty),size=1.2,fullrange=F) +
  geom_point( aes(fill=difficulty),shape=21,size=5 ) + 
  ylab("Proportional Undercount") + xlab("Insects in Sample") +
  geom_segment(x=0,y=0,xend=4000,yend=0) +
  geom_text(aes(label=sample),hjust=0, vjust=0) +
  #scale_fill_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("white","grey","black")) + 
  #scale_linetype_manual(name="Difficulty (Amount of scales, Collembola, and Mites)",values=c("dashed", "solid"))+theme_bw()+
  theme(axis.title.x = element_text(  size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),axis.title.y = element_text( size=20),
        axis.text.y  = element_text(vjust=0.5, size=16),legend.text = element_text( size = 16)) + theme_bw()

res$Image[res$Image=="M23-3R4ne-tray1"]<-"M23-3R4ne"
res$Image[res$Image=="M23-3R4ne-tray2"]<-"M23-3R4ne"
rownames(tax)<-tax$sample
res$Difficulty<-tax[as.character(res$Image),"difficulty"]

p <- ggplot(res, aes(factor(Image), Length))

#p + geom_violin()

p + geom_violin(aes(fill = factor(Difficulty)),scale="count") + coord_flip() + theme_bw() +
 theme(axis.title.x = element_text(  size=50),
            axis.text.x  = element_text(vjust=0.5, size=30),axis.title.y = element_text( size=50),
            axis.text.y  = element_text(vjust=0.5, size=30),legend.text = element_text( size = 30))
