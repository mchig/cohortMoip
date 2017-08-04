library(zoo)
library(dplyr)
library(tidyr)
library(lubridate)
library(gmodels)
library(reshape2)
library(gridExtra)
library(ggplot2)


cohortSummary <- function (dataRaw = NULL, tpv_group = "all", path = NULL) {
  start_time <- Sys.time()
  print("Working on it...")

  if (is.null(dataRaw)){
      if (is.null(path)) {
          path <- readline(prompt="Enter complete file path: ")
      }
      dataRaw<- read.csv(path,stringsAsFactors = FALSE,encoding = "UTF-8")

      dataRaw$tpv.group[dataRaw$tpv.group=="Large"]<-"F5"

      names(dataRaw) <- c("account_id","tpv_group_bd","login","payment_date","account_fauth","state","super_integrator", "tpv","receita",
                          "n_payments_submitted", "n_payments_canceled", "n_payments_authorized", "n_payments_analysis", "n_payments_pre",
                          "n_payments")
  }

      dataRaw$tpv.group[dataRaw$tpv.group=="Large"]<-"F5"

      names(dataRaw) <- c("account_id","tpv_group_bd","login","payment_date","account_fauth","state","super_integrator", "tpv","receita",
                       "n_payments_submitted", "n_payments_canceled", "n_payments_authorized", "n_payments_analysis", "n_payments_pre",
                       "n_payments", "owner_name")

      grp_data <- group_by(dataRaw, account_id, login, tpv_group_bd, account_fauth, payment_date)
      sum_data <- summarise(grp_data, tpv = sum(tpv), receita = sum(receita), n_payments_submitted = sum(n_payments_submitted),
                            n_payments_canceled = sum(n_payments_canceled), n_payments_authorized = sum(n_payments_authorized),
                            n_payments_analysis = sum(n_payments_analysis), n_payments_pre = sum(n_payments_pre),
                            n_payments = sum(n_payments))

      fatData <- sum_data[!(sum_data$payment_date==""),]
      grp_fat <- group_by(fatData, account_id)
      sum_fat <- summarise(grp_fat, tpv = max(tpv, na.rm = TRUE))

      sum_fat$tpv_group<-NA
      sum_fat$tpv_group[sum_fat$tpv >=0 & sum_fat$tpv <1000] <- "F0"
      sum_fat$tpv_group[sum_fat$tpv >=1000 & sum_fat$tpv <5000] <- "F1"
      sum_fat$tpv_group[sum_fat$tpv >=5000 & sum_fat$tpv <20000] <- "F2"
      sum_fat$tpv_group[sum_fat$tpv >=20000 & sum_fat$tpv <100000] <- "F3"
      sum_fat$tpv_group[sum_fat$tpv >=100000 & sum_fat$tpv <500000] <- "F4"
      sum_fat$tpv_group[sum_fat$tpv >=500000 & sum_fat$tpv <1000000] <- "F5"
      sum_fat$tpv_group[sum_fat$tpv >=1000000 & sum_fat$tpv <5000000] <- "F6"
      sum_fat$tpv_group[sum_fat$tpv >=5000000 & sum_fat$tpv] <- "F7"

      sum_fat <- sum_fat[,c(which(names(sum_fat)=="account_id"),which(names(sum_fat)=="tpv_group"))]

      sum_data <- merge(sum_data, sum_fat, by = "account_id", all = TRUE)

      if (tpv_group == "all") {
          tpv_group <- unique(sum_data$tpv_group)
      }

      data <- sum_data[sum_data$tpv_group%in%tpv_group&!(sum_data$payment_date==""),]
      #data <- dataRaw[dataRaw$tpv.group%in%tpv_group,]



      data$payment_date <- as.yearmon(data$payment_date,"%m/%Y")
      data$account_fauth <- as.Date(data$account_fauth)
      data$account_fauth <- as.yearmon(data$account_fauth,"%m/%Y")

      ##arrange date first payment
      data <- data[order(data$account_id),]
      grp <- group_by(data,account_id)
      fdTPV <- summarise(grp, first_payment = min(payment_date))

      data <- merge(data,fdTPV,by = "account_id", all = TRUE)

      data$m <- ((year(data$payment_date) - year(data$first_payment))*12 + (month(data$payment_date) - month(data$first_payment)))

      #organize mX
      all_m <- 0:max(data$m)
      diff_m <- setdiff(all_m,unique(data$m))

      if (length(diff_m)>0){
          for (i in 1:length(diff_m)){
              data[dim(data)[1]+1,] <- NA
              data[dim(data)[1],which(names(data)=="m")] <- diff_m[i]
          }
      }

      #organize first_payment
      max_date <- max(unique(data$payment_date),na.rm = TRUE)
      min_date <- min(unique(data$first_payment),na.rm = TRUE)
      all_date <- as.yearmon(seq(as.numeric(min_date),as.numeric(max_date),by = 1/12))
      diff_date <- as.yearmon(setdiff(all_date,unique(data$first_payment)))

      if (length(diff_date)>0){
          for (i in 1:length(diff_date)){
              data[dim(data)[1]+1,] <- NA
              data[dim(data)[1],which(names(data)=="first_payment")] <- diff_date[i]
          }
      }

      ##client count
      client_table <- xtabs(~first_payment+m,data = data, na.action = na.pass)
      client_count <- colSums(client_table)
      client_count_perc <- round((client_count/client_count[1]),digits = 4)

      ##tpv count/perc
      tpv_table <- xtabs(tpv~first_payment+m,data = data, na.action = na.pass)
      client_tpv_sum <- colSums(tpv_table)
      client_tpv <- colSums(tpv_table)
      client_tpv[1] <- client_tpv[1]/client_count[1]
      for (i in 2:length(client_tpv)){

          client_tpv[i] <- (client_tpv[i]/client_count[1]) + client_tpv[i-1]
      }
      client_tpv_perc <- round((client_tpv/client_tpv[1]),digits = 4)

      client_tpv_perc <- client_tpv_sum
      for (i in 1:(length(client_tpv_perc)-1)){
          if(client_tpv_sum[i]!=0){
              client_tpv_perc[i] <- (client_tpv_sum[i])/sum(tpv_table[(1:(length(tpv_table[,1])-i+1)),1])
          }
          else {
              client_tpv_perc[i] <- 0
          }
      }

      ##receita count/perc
      receita_table <- xtabs(receita~first_payment+m,data = data, na.action = na.pass)
      client_receita_sum <- colSums(receita_table)
      client_receita <- colSums(receita_table)
      client_receita[1] <- client_receita[1]/client_count[1]
      for (i in 2:length(client_receita)){

          client_receita[i] <- (client_receita[i]/client_count[1]) + client_receita[i-1]
      }

      client_receita_perc <- client_receita_sum
      for (i in 1:length(client_receita_perc)){
          if(client_receita_sum[i]!=0){
              client_receita_perc[i] <- (client_receita_sum[i])/sum(receita_table[(1:(length(receita_table[,1])-i+1)),1])
          }
          else {
              client_receita_perc[i] <- 0
          }
      }


      ##churn mes
      churn_perc <- client_count
      for (i in 1:(length(churn_perc)-1)){
        if(client_count[i]!=0){
          churn_perc[i] <- ((client_count[i] - client_count[i+1] - client_table[(length(client_table[,1])-i+1),i])/client_count[i])
        }
        else {
          churn_perc[i] <- 0
        }
      }


      churn_perc[-length(churn_perc)]

      ##summary data
      summary <- list (data = data,
                       client_table = client_table, client_count = client_count,client_count_perc = client_count_perc,
                       tpv_table = tpv_table, client_tpv = client_tpv, client_tpv_perc = client_tpv_perc,
                       receita_table = receita_table, client_receita = client_receita, client_receita_perc = client_receita_perc,
                       churn_perc = churn_perc)

      print(Sys.time()-start_time)

      return(summary)
}

churnBase <- function(base , churn_time = 4, tpv_group = c("F0","F1","F2","F3","F4","F5", "F6", "F7"), type = "all") {

    comercial<- read.csv("C:/Users/mauricio.chigutti/Google Drive/Clientes do Comercial/clientesComercial.csv",
                         stringsAsFactors = FALSE, sep = ";")
    comercial <- comercial[comercial$Account.Id!=0,c(2,3)]
    names(comercial) <- c("owner_name","account.id")

    base <- merge(base,comercial,by = "account.id", all = TRUE)

    source("C:/Users/mauricio.chigutti/Google Drive/Cohort/cohortSummary.R")

    if ( type == "touch"){
        base <- base[!is.na(base$owner_name),]
    }else if(type == "touchless"){
        base <- base[is.na(base$owner_name),]
    }

    table <- cohortSummary(base,tpv_group = tpv_group)

    month_now <- as.yearmon(now())
    month_min <- month_now - (churn_time-1)/12

    n <- 1
    mymonths <- vector()
    mymonths[1] <- month_now
    while(churn_time-n > 0){
        mymonths[n+1] <- month_now - (n)/12
        n = n + 1
    }
    mymonths <- as.character(as.yearmon(mymonths))

    final <- list()
    final$table <- table$client_table[rownames(table$client_table)%in%mymonths,c(1:churn_time)]

    ##churn mes
    client_perc <- colSums(final$table)
    client_sum <- colSums(final$table)
    for (i in 1:(length(client_perc))){
        if(client_sum[i]!=0){
            client_perc[i] <- (client_sum[i])/sum(final$table[(1:(length(final$table[,1])-i+1)),1])
        }
        else {
            client_perc[i] <- 0
        }
    }

    final$client_perc <- 1-client_perc

    return (final)
}

plotSafra <- function (table){
    cohortPerc <- data.frame(m = paste("M",names(table$client_perc),sep=""), percentage = round(100*table$client_perc,digits = 0))

    ggplot(cohortPerc, aes(x=m, y=percentage, group = 1)) +
        geom_line() +
        geom_point() +
        #geom_text(data=cohortPerc,
        #          aes(y=percentage, label= paste(percentage,"%",sep=""),vjust=-0.5),size = 5) +
        ggtitle("teste")
}

plotSafraTable <- function (table){
    tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    tbl <- tableGrob(table$table, theme=tt)
    grid.arrange(tbl)
}

getBaseData <- function (File = "C:/Users/mauricio.chigutti/Google Drive/Cohort/Churn4.csv") {
    base<- read.csv(File, stringsAsFactors = FALSE,encoding = "UTF-8")
}
