xaringanExtra::use_clipboard()

treatment <- c(0.02,1.8, 17.5, 55,75.7, 80)

plot(treatment)

plot(treatment, type = 'o')

plot(treatment, type="l")

plot(treatment, main="My Plot", sub="a plot")

plot(treatment, xlab="Position", ylab="score")

plot(treatment, las=1) # change the orientation of the axis 
plot(treatment, las=2)

plot(treatment, cex=2)
plot(treatment, cex=0.5)

plot(treatment, pch=1) # the symbol of the point 
plot(treatment, pch=20)

plot(treatment, type="l",lwd=10)
plot(treatment, type="l",lwd=0.5)

plot(treatment, type="l",lty=1)
plot(treatment, type="l",lty=2)

plot(treatment, type="l", col="red")
plot(treatment, type="l", col="dodgerblue") # if using html colour, must add # 

myRed <- rgb(1,0,0, alpha=0.5)
plot(treatment, type="p", cex=4, pch=20, col=myRed)

par(mfrow = c(1, 2))
plot(treatment, type="p", cex=4, pch=20, col=myRed)
plot(treatment, type="p", cex=4, pch=20, col='#87736f')

dev.off()

control <- c(0, 20, 40, 60, 80,100)
plot(control,treatment)

plot(treatment, type="o", col="blue")
lines(control, type="o", pch=22, lty=2, col="red")

plot(treatment, type="o", col="blue",ylim=c(0,100))
lines(control, type="o", pch=22, lty=2, col="red")

g_range <- range(0, treatment, control)
g_range

plot(treatment, type="o", col="blue", ylim=g_range)

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))
box()

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))
lines(control, type="o", pch=22, lty=2, col="red")
box()

plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))
box()

legend("topleft",legend=c("treatment","control"), col=c("blue","red"), pch=21:22, lty=1:2) 

plot(treatment, type="o", col="blue", lwd=1, ylim=g_range,axes=FALSE, ann=FALSE)
axis(1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=20*0:g_range[2])
box()

lines(control, type="o", pch=22, lty=2, col="red", lwd=2.5)
legend("topleft",legend=c("treatment","control"),col=c("blue","red"), pch=21:22, lty=1:2, lwd=c(1,2.5))

# install.packages('viridis')
library(viridis)
viridis(5)

hist(treatment)

hist(treatment, col="lightblue", ylim=c(0,5),cex.main=0.8)

par(mfrow = c(1, 2))
hist(treatment, col="lightblue", ylim=c(0,5), cex.main=0.8, breaks = 2)
hist(treatment, col="lightblue", ylim=c(0,5), cex.main=0.8, breaks = 10)
dev.off()

data<-data.frame(treatment, control)
row.names(data)<-c("Mon","Tue","Wed","Thu","Fri","Sat")

dotchart(as.matrix(t(data)))

dotchart(as.matrix(t(data)), color=c("red","blue"),main="Dotchart", cex=0.5)

knitr::include_graphics("illustrations/boxplot.png")

exprs <- read.delim("data/gene_data.txt",sep="\t",h=T,row.names = 1)
head(exprs)

boxplot(exprs)

boxplot(log2(exprs),ylab="log2 Expression", col=c("red","red","blue","blue"))

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        horizontal=T, las= 1,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "tooth length",
        ylab = "Vitamin C dose mg",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs='i')

plot(control, treatment)
text(20,60, 'THIS AREA OF \n THE PLOT HAS \n NO SAMPLE', col='red')
text(control, treatment, letters[1:6], adj=c(0,-1), col='blue')

plot(control, treatment)
abline(h=10, col='blue')
abline(v=50, col='red', lwd=2)
abline(a=0, b=1, lty=2)

plot(control, treatment)
polygon(c(50,50,100,100),c(50,80,80,50), col='gray', density=5)

## bmp(filename, width = 480, height = 480, units = "px",
##     pointsize = 12)
## jpeg(filename, width = 480, height = 480, units = "px",
##      pointsize  = 12, quality = 75)

bmp(file = "outputs/control.bmp")
plot(control)
dev.off()

jpeg(file = "control.jpg", quality = 20)
plot(control)
dev.off()

postscript(file = "control.ps")
plot(control)
dev.off()

pdf(file = "control.pdf", paper = "A4")
plot(control)
dev.off()

knitr::include_graphics("illustrations/iris-plot.png")

knitr::include_graphics("illustrations/base-lattice.jfif")

library(ggplot2)

knitr::include_graphics("illustrations/ggplot-map.jfif")

## ggplot(data = <default data set>,
##        aes(x = <default x axis variable>,
##            y = <default y axis variable>,
##            ... <other default aesthetic mappings>),
##        ... <other plot defaults>) +
## 
##        geom_scatter(aes(size = <size variable for this geom>,
##                       ... <other aesthetic mappings>),
##                   data = <data for this point geom>,
##                   stat = <statistic string or function>,
##                   position = <position string or function>,
##                   color = <"fixed color specification">,
##                   <other arguments, possibly passed to the _stat_ function) +
## 
##   scale_<aesthetic>_<type>(name = <"scale label">,
##                      breaks = <where to put tick marks>,
##                      labels = <labels for tick marks>,
##                      ... <other options for the scale>) +
## 
##   ggtitle("Graphics/Plot")+
##   xlab("Weight")+
##   ylab("Height")+
## 
##   theme(plot.title = element_text(color = "gray"),
##         ... <other theme elements>)

knitr::include_graphics("illustrations/ggplot-map-simple.jfif")

ggiris <- ggplot(data=iris)
class(ggiris)
ggiris$data[1:4,]

ggiris$mapping

ggiris$theme

ggiris$layers

ggiris <- ggplot(data=iris)
ggiris2 <- ggiris+aes(x=Petal.Length,y=Petal.Width)
ggiris2$mapping

ggiris2$theme

ggiris2$layers

ggiris <- ggplot(data=iris)
ggiris2 <- ggiris+aes(x=Petal.Length,y=Petal.Width)
ggiris3 <- ggiris2+geom_point()

ggiris3$mapping

ggiris3$theme

ggiris3$layers

ggiris3

ggiris3 <- ggplot(data=iris,
                 mapping=aes(x=Petal.Length,y=Petal.Width))
ggiris3+geom_point()

ggiris_line<- ggiris3 + geom_line()
ggiris_line

ggiris_smooth<- ggiris3 + geom_smooth()
ggiris_smooth

ggiris_bar<- ggplot(data=iris, mapping=aes(x=Species))
ggiris_bar2<- ggiris_bar + geom_bar()
ggiris_bar2

ggiris_hist <- ggplot(data=iris,
        mapping=aes(x=Petal.Length))
ggiris_hist2 <- ggiris_hist + geom_histogram() 
ggiris_hist2

ggiris_dens <- ggiris_hist + geom_density() 
ggiris_dens

ggiris_box <- ggplot(data=iris,
        mapping=aes(x=Species,y=Petal.Length))
ggiris_box2 <- ggiris_box+geom_boxplot() 
ggiris_box2

pcPlot_violin <- ggiris_box+geom_violin() 
pcPlot_violin

ggiris_scatter <- ggplot(data=iris,
                 mapping=aes(x=Petal.Length,y=Petal.Width))
ggiris_scatter+geom_point(colour="red")

ggiris_scatter <- ggplot(data=iris,
                 mapping=aes(x=Petal.Length,y=Petal.Width))
ggiris_scatter+geom_point()

ggiris_scatter2 <- ggplot(data=iris,
                  mapping=aes(x=Petal.Length,y=Petal.Width,color=Species))
ggiris_scatter2 + geom_point()

ggiris_scatter3 <- ggplot(data=iris,
                  mapping=aes(x=Petal.Length,y=Petal.Width,shape=Species))
ggiris_scatter3 + geom_point()

ggiris_scatter4 <- ggplot(data=iris)
ggiris_scatter4 + geom_point(aes(x=Petal.Length,y=Petal.Width,color=Species, shape=Species))

## ?geom_point

patients_clean<-read.delim('data/patients_clean.txt')
pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight, 
                                         colour=Sex))+geom_point()
pcPlot + facet_grid(Smokes~Sex)

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot + facet_grid(~Sex)

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot + facet_grid(Sex~.)

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot + facet_wrap(~Smokes)

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot + facet_wrap(~Pet+Smokes+Sex)

pcPlot + facet_grid(Smokes~Sex+Pet)

ggplot(patients_clean, aes(x=Sex, y=Weight)) + geom_boxplot()

levels(patients_clean$Sex)

patients_clean$Sex <- factor(patients_clean$Sex, 
                             levels=c("Male","Female"))
ggplot(patients_clean,aes(x=Sex, y=Weight)) + geom_boxplot()

pcPlot + geom_point() + facet_grid(Smokes~Sex)+
  scale_x_continuous(name="height ('cm')",
                     limits = c(100,200),
                     breaks=c(125,150,175),
                     labels=c("small","justright","tall"))

pcPlot <- ggplot(data=patients_clean,aes(x=Sex,y=Height))
pcPlot +
  geom_violin(aes(x=Sex,y=Height)) +
  scale_x_discrete(labels=c("Women", "Men"))

pcPlot <- ggplot(data=patients_clean,aes(x=Sex,y=Height,fill=Smokes))
pcPlot +
  geom_violin(aes(x=Sex,y=Height)) +
  scale_x_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(breaks=c(160,180),labels=c("Short", "Tall"))

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=Sex))
pcPlot + geom_point(size=4)

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=Sex))
pcPlot + geom_point(size=4) + 
  scale_colour_manual(values = c("Green","Purple"),
                     name="Gender")

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=Pet))
pcPlot + geom_point(size=4) + 
  scale_colour_brewer(palette = "Set2")

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,alpha=BMI))
pcPlot + geom_point(size=4) + 
  scale_alpha_continuous(range = c(0.5,1))

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,size=BMI))
pcPlot + geom_point(alpha=0.8) + 
  scale_size_continuous(range = c(3,6))

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,size=BMI))
pcPlot + geom_point() + scale_size_continuous(range = c(3,6),
                                              limits = c(25,40))

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,size=BMI))
pcPlot + geom_point() + 
  scale_size_continuous(range = c(3,6), 
                        breaks=c(25,30), 
                        labels=c("Good","Good but not 25")) 

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=BMI))
pcPlot + geom_point(size=4,alpha=0.8) + 
  scale_colour_gradient(low = "White",high="Red")

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=BMI))
pcPlot + geom_point(size=4,alpha=0.8) + 
  scale_colour_gradient2(low = "Blue",mid="Black", high="Red",
                         midpoint = median(patients_clean$BMI))

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=BMI))
pcPlot + geom_point(size=4,alpha=0.8) + 
  scale_colour_gradient2(low = "Blue",
                         mid="Black",
                         high="Red",
                         midpoint = median(patients_clean$BMI),
                         breaks=c(25,30),labels=c("Low","High"),
                         name="Body Mass Index")

pcPlot <- ggplot(data=patients_clean,
                 aes(x=Height,y=Weight,colour=BMI,shape=Sex))
pcPlot + geom_point(size=4,alpha=0.8)+ 
  scale_shape_discrete(name="Gender") +
  scale_colour_gradient2(low = "Blue",mid="Black",high="Red",
                         midpoint = median(patients_clean$BMI),
                         breaks=c(25,30),labels=c("Low","High"),
                         name="Body Mass Index")

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))
pcPlot+geom_point()+stat_smooth()

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))
pcPlot+geom_point()+stat_smooth(method="lm")

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height,colour=Sex))
pcPlot+geom_point()+stat_smooth(method="lm")

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height,colour=Sex))
pcPlot+geom_point()+stat_smooth(aes(x=Weight,y=Height),method="lm",
                                inherit.aes = F)

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Sex,y=Height)) + geom_jitter()
pcPlot + stat_summary(fun=quantile, geom="point",shape=8,
                    colour="purple", size=4)

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+geom_point()
pcPlot

pcPlot+theme_minimal()

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+
  geom_point()
pcPlot+
  theme(
    text = element_text(colour="red")
      )

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+geom_point()
pcPlot + theme(text = element_text(colour="red"),
        axis.title.y = element_text(angle=0))

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+
  geom_point()+
  facet_grid(Sex~Smokes)
pcPlot+
  theme(
    text = element_text(colour="red"),
    axis.title.y = element_text(angle=0),
    axis.line = element_line(linetype = 0),
    panel.background=element_rect(fill="white"),
    strip.background=element_rect(fill="cyan")
    )

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot+theme(legend.position="left")

pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot+theme(legend.text = element_text(colour="darkred"),
             legend.title = element_text(size=20),
             legend.position = "bottom"
             )

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+geom_point()+theme_bw()
pcPlot+
  theme(text = element_text(colour="red"))

oldTheme <- theme_bw()
newTheme_Plus <- theme_bw() +
  theme(text = element_text(colour="red"))
  
newTheme_Replace <- theme_bw() %+replace%
  theme(text = element_text(colour="red"))
  
oldTheme$text
newTheme_Plus$text

newTheme_Replace$text

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+geom_point()
pcPlot+labs(title="Weight vs Height",y="Height (cm)")

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Height,y=Weight))+geom_point()
pcPlot+ggtitle("Weight vs Height")+ylab("Height (cm)")

pcPlot <- ggplot(data=patients_clean,
        mapping=aes(x=Weight,y=Height))+geom_point()
        
ggsave(pcPlot,filename = "outputs/anExampleplot.png",width = 15,
       height = 15,units = "cm")
