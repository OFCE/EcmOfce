rm(list =ls())
library(tidyverse)
library(lmtest)

library(tseries)
library(FinTS)
library(EcmOfce)
library(ggiraph)
library(ofce)



data<-readRDS("data.rds")
data<-mutate(data, log_compet=log(p6_d2)-log(ipimmdf)-log(dollareuro),
                    #logarithme d'un ratio de prix relatifs corrigé du taux de change : prix domestiques à l'exportation (déflateur des exportations) - prix des concurrents à l'exportation corrigé du taux de change 
                    ecart_export_lt=log(lag(p6_d1,1))-log(lag(iqimmsf,1)),
                    #exportations en volumes (période t-1) - demande mondiale adressée à la France (correcteur dans le MCE)
                    demint_gap=log(dintf_d1)-log(pibpot), #demande intérieure finale pour l'ensemble des biens et services - PIB potentiel
                    #terme de court terme dans le MCE
                    iqimmsf_gap=log(iqimmsf)-log(pibpot),
                    date=as.Date(date),
                    i2020q1 = ifelse(date == "2020-01-01", 1, 0),
                    i2020q2 = ifelse(date == "2020-04-01", 1, 0),
                    i2020q3 = ifelse(date == "2020-07-01", 1, 0),
                    i2022q1 = ifelse(date == "2022-01-01", 1, 0),
                    i2023q2 = ifelse(date == "2023-04-01", 1, 0))


model<-delta(1,log(p6_d1))~offset(delta(1,log(iqimmsf)))+I(log(lag(p6_d1,1))-log(lag(iqimmsf,1)))+lag(log_compet,1)+ 
  lag(iqimmsf_gap,1)+
  i2020q1 + i2020q2 +i2020q3+i2023q2+i2022q1

data<-data%>%filter(date>"1996-10-01" & date<as.Date("2024-01-01"))

estim<-lm(model,data)
summary(estim)

make_tests(estim=estim, banque=data)

ofce::girafy(make_plot_estim2(estim=estim,data=data)[["plot_fit"]])
ofce::girafy(make_plot_estim2(estim=estim,data=data)[["plot_resid"]])

simul_dyn<-simulation_dynamique(estim=estim,data=data)
ofce::girafy(make_plot_simul_dynamique(estim=estim,data=data)[["plot_niveau"]])
