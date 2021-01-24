# library
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(ggtree)
library(phytools)
library(plyr)
library(ggpubr)
library("extrafont")




theme_set(theme_bw(base_size = 16))
dev.off()

dat = read.csv("mistic-consurf.csv")
nrow(dat)
#select residues based on MI score and conservation profile
d1 = (dat %>%
          filter(i.conservation < -0.5) %>%
          select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))
d2 = (d1 %>%
        filter(j.conservation < -0.5) %>%
        select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))

d3 = (d2 %>%
        filter(MI >4 ) %>%
        select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))

d4 = (dat %>%
             filter(MI >5 ) %>%
             select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))

d5 = (d4 %>%
        filter(i.conservation < -0.3) %>%
        select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))

d6 = (d5 %>%
        filter(j.conservation < -0.3) %>%
        select(i.mouse,j.mouse,MI, i.conservation,j.conservation, i.b.e, j.b.e))



d7 = unique(rbind(d3,d6))


a <- subset(d7,i.mouse>278 & i.mouse < 289)
b <-subset(d7, i.mouse > 399)
c <-subset(d7, i.mouse > 224 & i.mouse < 236)

dx = rbind(a,b,c)

e <- subset(dx,j.mouse>278 & j.mouse < 289)
f <-subset(dx, j.mouse > 399)
g <-subset(dx, j.mouse > 224 & j.mouse < 236)

d0 = rbind(e,f,g)



dotplot1 <- ggplot(d0, aes(x = i.mouse, y = j.mouse, color=i.b.e, shape=j.b.e)) +
  geom_point(size=3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("i", labels = as.character(d0$i.mouse), breaks = d0$i.mouse) +
  scale_y_continuous("j", labels = as.character(d0$j.mouse), breaks = d0$j.mouse)


dotplot1






d7$i.b.e.2 <- if_else(d7$i.mouse>399,"e", as.vector(d7$i.b.e))
d7$j.b.e.2 <- if_else(d7$j.mouse>399,"e", as.vector(d7$j.b.e))



d7$exposed = paste(d7$i.b.e.2,d7$j.b.e.2)

d7$exposed

(d7 <- d7 %>%
    mutate(color = case_when(
      exposed == "e e" ~ "both residues exposed",
      exposed == "e b" ~ "i residue exposed, j residue buried",
      exposed == "b e" ~ "i residue buried, j residue exposed",
      exposed == "b b" ~ "both residues buried"
    )))


nrow(d7)


d7$color

dotplot2 <- ggplot(d7, aes(x = i.mouse, y = j.mouse,colour = color)) +
  geom_segment(aes(x=0,xend=450,y=50,yend=50), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=100,yend=100), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=150,yend=150), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=200,yend=200), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=250,yend=250), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=300,yend=300), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=350,yend=350), color = 'lightgrey') +
  geom_segment(aes(x=0,xend=450,y=400,yend=400), color = 'lightgrey') +
  geom_segment(aes(x=50,xend=50,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=100,xend=100,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=150,xend=150,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=200,xend=200,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=250,xend=250,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=300,xend=300,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=350,xend=350,y=0,yend=450), color = 'lightgrey') +
  geom_segment(aes(x=400,xend=400,y=0,yend=450), color = 'lightgrey') +
  geom_point(size=2.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x = element_text(vjust=6)) +
  theme(axis.title.y = element_text(vjust=-10)) +
  xlim(0,450) +
  ylim(0,450) +
  theme(legend.text = element_text(size=20)) +
  xlab("mouse residue i") +
  ylab("mouse residue j") +
  theme(legend.position = c(0.6, 0.15)) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x=element_text(angle=0, hjust=0.5, size =16, vjust = 8.5),
        axis.text.y=element_text(size =16, hjust = 2.3),
        axis.title=element_text(size=20,face="bold"),
        axis.ticks = element_blank()) +
  theme(panel.border = element_blank()) +
  geom_segment(aes(x=0,xend=450,y=0,yend=0), color = 'black') +
  geom_segment(aes(x=0,xend=0,y=0,yend=450), color = 'black') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



loadfonts(device = "pdf")

dotplot2

postscript("Figure S6.eps", family = "Arial",width=8.5,height=11)
dotplot2

dev.off()


d9 = (d7 %>%
        filter(color == 'both residues exposed' & j.mouse>399) %>%
        select(i.mouse,j.mouse))
d9
