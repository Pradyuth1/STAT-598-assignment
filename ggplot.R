library(ggplot2)
str(diamonds)

ggplot() +
  layer(
    data = diamonds,
    mapping = aes(x = carat, y = price),
    geom = "point", stat = "identity",
    position = "identity" ) +
  scale_y_continuous() + scale_x_continuous() +
  coord_cartesian()

ggplot(diamonds, aes(carat, price)) + geom_point() +
  geom_smooth()

mix2norm <- data.frame(x = c(rnorm(1000),rnorm(1000,3)),
                       grp = as.factor(rep(c(1,2),each=1000)))

ggplot(mix2norm, aes(x=x, color = grp, fill= grp)) +
  geom_density(alpha=.4, adjust=1/2, size=1, stat="bin")

ggplot(mix2norm, aes(x=x, color = grp, fill= grp)) +
  geom_density(alpha=.4, adjust=1/2, size=2, stat="bin")

ggplot(mix2norm, aes(x=x, color = grp, fill = grp)) +
  stat_density(alpha = 0.4, adjust=1/2)

library('tibble')
my_waves <- tibble(x=seq(0,6.28,.1),y1=sin(x),y2=sin(x)^2)

ggplot(my_waves) + geom_ribbon(aes(x=x,ymax=y1,ymin=y2))

ggplot(mix2norm, aes(x=x, color = grp)) +
  stat_density(adjust=1/2, size=2, position = "identity",
               geom = "line")

ggplot(mix2norm, aes(x=x, color = grp)) +
  stat_density(adjust=1/2, size=2, position = "identity",
               geom = "line") + coord_polar()

ggplot(mix2norm, aes(x=x, color = grp)) +
  stat_density(adjust=1/2, size=2, position = "identity",
               geom = "line") + scale_y_log10(limits = c(1e-5,1))



mean <- function(x,y) {
  #print("Hello")
  return(x+y)
}


a(1,2)

my_func <- function(a=1,b,c,d=3) {
  b/a + d/c
}
a<-2; b<-3; c<-1;d<-4
my_func(d,c,b,a)

my_func(,c=2,3)
my_func(c = 2,3)
