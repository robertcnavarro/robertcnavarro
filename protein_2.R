install.packages("magick")
library(magick)
install.packages("cvAUC")
library(ROCit)
ROCit_obj <- rocit(score=data$Albuminuria,class=data$`CODIGO DEL PACIENTE_NLA`)
plot(ROCit_obj)

ROCit_obj <- rocit(score=data$NGAL,class=data$`CODIGO DEL PACIENTE_NLA`)
plot(ROCit_obj)

# install a single package
install_github("DeveloperName/PackageName")
devtools::install_github("hadley/ggplot2")
devtools::install_github("sachsmc/plotROC")
library(plotROC)
install.packages("ggplot2")
library(ggplot2)


library(plyr)
mu <- ddply(data, "`CODIGO DEL PACIENTE`", summarise, grp.mean=mean(CREATINURIA))
head(mu)


# Interleaved histograms
ggplot(data, aes(x=data$NGAL, color=data$`CODIGO DEL PACIENTE`)) +
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position="top")

# Change line colors by groups NGAL
ggplot(data, aes(x=NGAL, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="NGAL",x="NGAL", y = "Density")+
  theme_classic()
# Change line colors by groups ACR
ggplot(data, aes(x=ACR, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="ACR",x="ACR", y = "Density")+
  theme_classic()

# Change line colors by groups ACR
ggplot(data, aes(x=ACR, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="ACR",x="ACR", y = "Density")+
  theme_classic()

# Change line colors by groups Albuminuria
ggplot(data, aes(x=Albuminuria, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="Albuminuria",x="Albuminuria", y = "Density")+
  theme_classic()
#mean grupo
mu <- ddply(data, "`CODIGO DEL PACIENTE`", summarise, grp.mean=mean(Clusterina))
head(mu)


# Change line colors by groups Clusterina
ggplot(data, aes(x=Clusterina, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="Clusterina",x="Clusterina", y = "Density")+
  theme_classic()

#mean grupo
mu <- ddply(data, "`CODIGO DEL PACIENTE`", summarise, grp.mean=mean(`Cistatina C`))
head(mu)


# Change line colors by groups `Cistatina C`
ggplot(data, aes(x=`Cistatina C`, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="`Cistatina C`",x="`Cistatina C`", y = "Density")+
  theme_classic()
#mean grupo
mu <- ddply(data, "`CODIGO DEL PACIENTE`", summarise, grp.mean=mean(`KIM 1`))
head(mu)
data$`KIM 1`

# Change line colors by groups `Cistatina C`
ggplot(data, aes(x=data$`KIM 1`, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="`KIM 1`",x="`KIM 1`", y = "Density")+
  theme_classic()

#mean grupo
mu <- ddply(data, "`CODIGO DEL PACIENTE`", summarise, grp.mean=mean(Osteopontina))
head(mu)
data$Osteopontina

# Change line colors by groups Osteopontina
ggplot(data, aes(x=Osteopontina, color=`CODIGO DEL PACIENTE`, fill=`CODIGO DEL PACIENTE`)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=`CODIGO DEL PACIENTE`),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#b71c1c","#006064","#4a138c"))+
  labs(title="Osteopontina",x="Osteopontina", y = "Density")+
  theme_classic()
