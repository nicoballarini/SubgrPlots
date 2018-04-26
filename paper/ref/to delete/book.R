demo()
utils::install.packages(
  "http://astro.ocis.temple.edu/~rmh/Deming2016rmh/Deming2016rmh_1.0-1.tar.gz"
)

library(Deming2016rmh)
help(Deming2016rmh)
demo()
demo("Deming2016-rmh")


mosaic


library(ggplot2)
df = expand.grid(x=1:100,y=1:100)


ggplot(df) + coord_equal() + my_theme +
  geom_tile(aes(x = x, y = y, fill = NULL),alpha = 0, colour="black")

ggplot(df) + coord_equal()+ my_theme +
  geom_tile(aes(x = x, y = y, fill = 1:nrow(df)),colour="white",size=0.11)


my_theme = theme_bw() +
  #theme options
  theme(
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    panel.grid=element_blank(),
    #remove plot border
    panel.border=element_blank())

#modified ggplot
p <- ggplot(m3,aes(x=Year,y=State,fill=Incidence))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25) +
  #remove x and y axis labels
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  #define new breaks on x-axis
  scale_x_discrete(expand=c(0,0),
                   breaks=c("1930","1940","1950","1960","1970","1980","1990","2000"))+
  #one unit on x-axis is equal to one unit on y-axis.
  #maintains aspect ratio.
  coord_fixed()+
  #set a base size for all fonts
  theme_grey(base_size=8)+
  #theme options
  theme(
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())

#save with dpi 150 and cairo type
ggsave(filename="measles-mod1.png",plot = p,dpi=150,type="cairo")
