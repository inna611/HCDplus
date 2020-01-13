rm(list = ls())
### Set the path with folder containing files "sex-specific.csv";
### "Asterisk.csv", "Post-procedural.csv"; "age-specific.csv"; "Full_list.csv"
### with causes to be checked



### Set the path with folder XXX_input
### if it is the same folder as in the previous step
### this line can be missed


### Function that checks files for medical consistency

medical_consistency <- function (country){

  data1 <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data2 <- read.csv(paste(country,"_input\\",country,"_d_full_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)

  x <- menu(c("icd10_4", "icd10_3", "baltic", "russia", "ukraine", "belarus"), title = "Please spesify which classification is used in full lists")

  if(x %in% 1:2){
    age_spec <- read.csv("age-specific_icd3.csv",header = T, stringsAsFactors = F )
    asterisk <- read.csv("asterisk.csv")
    post_procedural <- read.csv("post-procedural.csv")
    full_list <- read.csv("Full_list_icd3.csv", header = T, stringsAsFactors = F)
  }

  if(x==1){
    sex_spec <- read.csv("sex-specific_icd4.csv", header = T, stringsAsFactors = F)
  }

  if(x==2){
    sex_spec <- read.csv("sex-specific_icd3.csv", header = T, stringsAsFactors = F)
  }

  if(x==3){
    sex_spec <- read.csv("sex-specific_baltic.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_baltic.csv")
  }

  if(x==4){
    sex_spec <- read.csv("sex-specific_russia.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_russia.csv")
  }

  if(x==5){
    sex_spec <- read.csv("sex-specific_ukraine.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_ukraine.csv")
  }

  if(x==6){
    sex_spec <- read.csv("sex-specific_belarus.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_belarus.csv")
  }

  males <- subset(data1, data1$sex==1)
  males <- subset(males, males$cause %in% sex_spec$females)
  males <- subset(males, males$total!=0)
  females <- subset(data1, data1$sex==2)
  females <- subset(females, females$cause %in% sex_spec$males)
  females <- subset(females, females$total!=0)
  sexes <- rbind(males, females)
  if(sum(sexes$total)==0){
    cat("No deaths with impossible cause/sex-dependence were found", fill = T)
  } else {
    message(paste(country,"_d_full.csv contains deaths with impossible cause/sex dependence. The report was created in ", country, "_sex-specific codes.csv", sep = ""))
    write.table(sexes, file = paste(country, "_input\\", country, "_sex-specific codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
  }

  ### Checking for HIV before 1982

  if(x==1){
    data1$cause3 <- substr(data1$cause, start = 1, stop = 3)
    data1 <- aggregate(data1[,7:32], by = list(country = data1$country, year = data1$year, sex=data1$sex, list = data1$list, agf = data1$agf, cause = data1$cause3), FUN = sum)
  }

  data1 <- subset(data1, data1$sex==3)

  if(x %in% 1:2){
    HIV <- data1[grep("B2[0,1,2,3,4]",data1[,6]),]
  }

  if(x==3){
    HIV <- subset(data1, data1$cause==24)
  }

  if(x==4){
    HIV <- subset(data1, data1$cause==44)
  }

  if(x==5){
    HIV <- subset(data1, data1$cause==45)
  }

  if(x==6){
    HIV <- subset(data1, data1$cause==47)
  }

  HIV <- subset(HIV, HIV$year < 1982 & HIV$total >0)
  if(nrow(HIV) >0) {
    message("There are HIV deaths before the year 1982")}

  ### Checking for deaths with wrong cause/age dependence


  perinatal <- subset(data1, data1$cause %in% age_spec$perinatal)
  perinatal <- subset(perinatal, rowSums(perinatal[,12:32], na.rm = T)>0)
  perinatal$total <- perinatal$total - rowSums(perinatal[,8:11])
  if(nrow(perinatal) >0) {
    perinatal[,8:11] <- "."
    perinatal$list <- "perinatal"}

  maternal <- subset(data1, data1$cause %in% age_spec$maternal)
  maternal <- subset(maternal, rowSums(maternal[,c(8:10,21:32)], na.rm = T)>0)
  maternal$total <- maternal$total - rowSums(maternal[,11:20])
  if(nrow(maternal)>0){maternal[,11:20] <- "."
  maternal$list <- "maternal"}

  old <- subset(data1, data1$cause %in% age_spec$old_age)
  old <- subset(old, rowSums(old[,8:18])>0)
  old$total <- old$total - rowSums(old[,19:26])
  if(nrow(old)>0){old[,19:32] <- "."
  old$list <- "old_age"}

  suicide <- subset(data1, data1$cause %in% age_spec$suicides)
  suicide <- subset(suicide, rowSums(suicide[,8:9], na.rm = T)>0)
  suicide$total <- suicide$total - rowSums(suicide[,10:26])
  if(nrow(suicide)>0){suicide[,10:32] <- "."
  suicide$list <- "suicide"}

  ages <- rbind(perinatal, maternal, old, suicide)

  if(sum(ages$total)==0){
    cat("No deaths with impossible age/sex-dependence were found", fill = T)
  } else {
    message(paste(country,"_d_full.csv contains deaths with impossible age/sex dependence. The report was created in ", country, "_age-specific codes.csv", sep = ""))
    write.table(ages, file = paste(country, "_input\\", country, "_age-specific codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
  }

  if(x %in% 1:2){
    data_ast <- subset(data1, data1$cause %in% asterisk[,1])
    data_ast <- subset(data_ast, data_ast$total!=0)
    if(sum(data_ast$total)==0){
      cat("No deaths with asterisk codes were found", fill = T)
    } else {
      message(paste(country,"_d_full.csv contains deaths with asterisk codes. The report was created in ", country, "_asterisk codes.csv", sep = ""))
      write.table(data_ast, file = paste(country, "_input\\", country, "_asterisk codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    data_post <- subset(data1, data1$cause %in% post_procedural[,1])
    data_post <- subset(data_post, data_post$total!=0)
    if(sum(data_post$total)==0){
      cat("No deaths with post-proceduarl codes were found",fill = T)
    } else {
      message(paste(country,"_d_full.csv contains deaths with post-procedural codes. The report was created in ", country, "_post-procedural codes.csv", sep = ""))
      write.table(data_post, file = paste(country, "_input\\", country, "_post-procedural codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    ### Checking that the full list contains all the codes and no extra codes

    extra <- subset (data1, !data1$cause %in% full_list$Icd10)
    missed <- subset(full_list, !full_list$Icd10 %in% data1$cause)

    if(nrow(extra)==0 & nrow(missed)==0){
      cat(paste("The ICD10 codes in ", country, "_d_full.csv correspocnd to the HCD full list correctly", sep = ""), fill = T)
    }
    if(nrow(extra)>0){
      message(paste("There are extra ICD10 codes in ", country, "_d_full.csv. The output was created in ", country, "_d_full extra codes.csv", sep = ""))
      write.table(extra, file = paste(country, "_input\\", country, "_d_full extra codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }
    if(nrow(missed)>0){
      message(paste("There are missed ICD10 codes in ", country, "_d_full.csv. The output was created in ", country, "_d_full missed codes.csv", sep = ""))
      write.table(missed, file = paste(country, "_input\\", country, "_d_full missed codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    ### Checking that the full_idr list contains all the codes and no extra codes

    data2$cause3 <- substr(data2$cause, start = 1, stop = 3)
    data2 <- aggregate(data2[,7:32], by = list(country = data2$country, year = data2$year, sex=data2$sex, list = data2$list, agf = data2$agf, cause = data2$cause3), FUN = sum)
    R_codes <- data2[grep("R",data2[,6]),]
    if(unique(R_codes$cause)!="R95"){
      message(paste("Check ill-defined causes (R-codes) in ", country, "_d_full_idr.csv.", sep = ""))
    }
    data1 <- data1[!grep("R",data1[,6]),]
    data2 <- data2[!grep("R",data2[,6]),]
    if (identical(data1$cause,data2$cause)==FALSE){
      message(paste("ICD10 codes in ", country, "_d_full.csv. and ", country, "_d_full_idr.csv are not the same", sep = ""))
    }
  }
}

consistency_checks <- function(country){

  #### Check whether country name is specified correctly by user
  countries_list <- c ("BLR", "CZE", "GBRTENW", "EST", "FRATNP", "DEUTNP", "JPN", "LVA",
                       "LTU", "MDA", "POL", "ROU", "RUS", "ESP", "UKR", "USA")
  if(!(country %in% countries_list)){
    message ("ERROR! specify the name of the country correctly (BLR, CZE, GBRTENW, EST, FRATNP, DEUTNP, JPN, LVA, LTU, MDA, POL, ROU, RUS, ESP, UKR, USA)")
    stop ()
  }

  ### Downloading and processing the data

  data1 <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data2 <- read.csv(paste(country,"_input\\",country,"_d_full_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data3 <- read.csv(paste(country,"_input\\",country,"_d_interm.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data4 <- read.csv(paste(country,"_input\\",country,"_d_interm_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data5 <- read.csv(paste(country,"_input\\",country,"_d_short.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data6 <- read.csv(paste(country,"_input\\",country,"_d_short_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)

  data1 <- data1[order(data1$year,data1$sex,data1$cause),]
  data2 <- data2[order(data2$year,data2$sex,data2$cause),]
  data3 <- data3[order(data3$year,data3$sex,data3$cause),]
  data4 <- data4[order(data4$year,data4$sex,data4$cause),]
  data5 <- data5[order(data5$year,data5$sex,data5$cause),]
  data6 <- data6[order(data6$year,data6$sex,data6$cause),]

  data1[data1=="."] <-NA
  data2[data2=="."] <-NA
  data3[data3=="."] <-NA
  data4[data4=="."] <-NA
  data5[data5=="."] <-NA
  data6[data6=="."] <-NA


  ### column names are specified correctly
  columns <- c("country", "year", "sex", "list", "agf", "cause", "total", "d0", "d1", "d5",
               "d10", "d15", "d20", "d25", "d30", "d35", "d40", "d45", "d50", "d55", "d60",
               "d65", "d70", "d75", "d80", "d85p", "d85", "d90p", "d90", "d95p", "d95", "d100p")
  if(all(colnames(data1)==columns, colnames(data2)==columns, colnames(data3)==columns,
         colnames(data4)==columns, colnames(data5)==columns, colnames(data6)==columns)==T){
    cat("Names of the columns in the files are specified correctly", fill = T)
  } else {
    if (all(colnames(data1)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_full.csv", sep = ""))
    }
    if (all(colnames(data2)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_full_idr.csv", sep = ""))
    }
    if (all(colnames(data3)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_interm.csv", sep = ""))
    }
    if (all(colnames(data4)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_interm_idr.csv", sep = ""))
    }
    if (all(colnames(data5)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_short.csv", sep = ""))
    }
    if (all(colnames(data6)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_short_idr.csv", sep = ""))
    }
  }

  ### Country name is correct in all files

  variable1 <- c(data1$country, data2$country, data3$country, data4$country, data5$country, data6$country)
  c_name <- unique(variable1)

  if(length(c_name)==1 & c_name[1]==country){
    cat(paste("Country name is correct : ",c_name, sep=""), fill = T)
  } else {
    if (length(c_name)>1){
      message ("ERROR! variable ~country~ is not the same across the files")
    }
    if (length(c_name)==1 & c_name[1] != country){
      message ("ERROR! Name of the country is not identical between the files' names and variable ~country~ within the files")
    }
  }

  ### Year is between 1900 and 2018
  years <- data.frame(unique(data1$year),unique(data2$year),unique(data3$year),
                      unique(data4$year),unique(data5$year),unique(data6$year))

  if(all(sweep(years, MARGIN=1, STATS=years[,1], FUN = "=="))==T & sum(years[,] < 1900 | years >2019) ==0){
    cat (paste("The range of years is identical in all the lists : ",years[1,1],"-", years[nrow(years),1], sep = ""), fill = T)
  } else{
    if (sum(years[,] < 1900 | years >2020) >0) {
      message ("ERROR! The range of years NOT between 1900 and 2020")
    }
    if (all(sweep(years, MARGIN=1, STATS=years[,1], FUN = "=="))!=T) {
      message ("ERROR! The range of years is not the same across the lists")
    }
  }

  ### Sex is from 1 to 3
  sexes <- data.frame(unique(data1$sex),unique(data2$sex),unique(data3$sex),
                      unique(data4$sex),unique(data5$sex),unique(data6$sex))


  if(all(sexes[,1]== c(1,2,3))==T & all(sweep(sexes, MARGIN=1, STATS=sexes[,1], FUN = "=="))==T){
    cat ("Variable ~sex~ is from 1 to 3 in all the lists", fill = T)
  } else {
    message ("ERROR! Check the range of sex variables in the lists")
  }

  ### Checking list name

  full_lists <- c("icd10_4", "icd10_3", "baltic", "russia", "ukraine", "belarus")
  x <- menu(full_lists, title = "Please spesify which classification is used in full lists")

  if (all(data1$list == full_lists[x]) & all(data2$list == full_lists[x]) &
      all(data3$list=="interm") &  all(data4$list=="interm") &
      all(data5$list=="short") & all(data6$list == "short")){
    cat("The variables ~list~ are specified correctly", fill = T)
  } else {
    if(all(data1$list == full_lists[x])!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_full.csv", sep = ""))
    }
    if(all(data2$list == full_lists[x])!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_full_idr.csv", sep = ""))
    }
    if(all(data3$list == "interm")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_interm.csv", sep  =""))
    }
    if(all(data4$list == "interm")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_interm_idr.csv", sep = ""))
    }
    if(all(data5$list == "short")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_short.csv", sep = ""))
    }
    if(all(data6$list == "short")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_short_idr.csv", sep = ""))
    }
  }

  ###Check age group format
  age_formats <- data.frame(unique(data1$agf),unique(data2$agf),unique(data3$agf),
                            unique(data4$agf),unique(data5$agf),unique(data6$agf))

  if  (all(sweep(age_formats, MARGIN=1, STATS=age_formats[,1], FUN = "=="))==F){
    message ("ERROR! The age formats (variable ~agf~ ) are not the same across the lists")
  }
  if(sum(age_formats[,1]<1 |age_formats [,1] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_full.csv"))
  }
  if(sum(age_formats[,2]<1 |age_formats [,2] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_full_idr.csv"))
  }
  if(sum(age_formats[,3]<1 |age_formats [,3] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_interm.csv"))
  }
  if(sum(age_formats[,4]<1 |age_formats [,4] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_interm_idr.csv"))
  }
  if(sum(age_formats[,5]<1 |age_formats [,5] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_short.csv"))
  }
  if(sum(age_formats[,6]<1 |age_formats [,6] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_short_idr.csv"))
  }


  ### Check that agf corresponds to data correctly

  data <- rbind (data1, data2, data3, data4, data5, data6)

  data_agf1 <- subset (data, data$agf==1)
  data_agf2 <- subset (data, data$agf==2)
  data_agf3 <- subset (data, data$agf==3)
  data_agf4 <- subset(data, data$agf==4)


  if (all(is.na(data_agf1[,27:32]))==T & all(is.na(data_agf2[,29:32]))==T &
      all(is.na(data_agf3[,31:32]))==T & sum(is.na(data_agf2[,27:28]))==0 & sum(is.na(data_agf3[,27:30]))==0 &
      sum(is.na(data_agf4[,27:32]))==0){
    cat ("Age formats correspond to the data correctly", fill = T)
  }else{
    data1_agf1 <- subset (data1, data1$agf==1)
    data1_agf2 <- subset (data1, data1$agf==2)
    data1_agf3 <- subset (data1, data1$agf==3)
    data1_agf4 <- subset (data1, data1$agf==4)
    data2_agf1 <- subset (data2, data2$agf==1)
    data2_agf2 <- subset (data2, data2$agf==2)
    data2_agf3 <- subset (data2, data2$agf==3)
    data2_agf4 <- subset (data2, data2$agf==4)
    data3_agf1 <- subset (data3, data3$agf==1)
    data3_agf2 <- subset (data3, data3$agf==2)
    data3_agf3 <- subset (data3, data3$agf==3)
    data3_agf4 <- subset (data3, data3$agf==4)
    data4_agf1 <- subset (data4, data4$agf==1)
    data4_agf2 <- subset (data4, data4$agf==2)
    data4_agf3 <- subset (data4, data4$agf==3)
    data4_agf4 <- subset (data4, data4$agf==4)
    data5_agf1 <- subset (data5, data5$agf==1)
    data5_agf2 <- subset (data5, data5$agf==2)
    data5_agf3 <- subset (data5, data5$agf==3)
    data5_agf4 <- subset (data5, data5$agf==4)
    data6_agf1 <- subset (data6, data6$agf==1)
    data6_agf2 <- subset (data6, data6$agf==2)
    data6_agf3 <- subset (data6, data6$agf==3)
    data6_agf4 <- subset (data6, data6$agf==4)
    if (all(is.na(data1_agf1[,27:32]))==F | all(is.na(data1_agf2[,29:32]))==F |
        all(is.na(data1_agf3[,31:32]))==F | sum(is.na(data1_agf2[,27:28]))>0 & sum(is.na(data1_agf3[,27:30]))>0 |
        sum(is.na(data1_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_full.csv", sep = ""))
    }
    if (all(is.na(data2_agf1[,27:32]))==F | all(is.na(data2_agf2[,29:32]))==F |
        all(is.na(data2_agf3[,31:32]))==F | sum(is.na(data2_agf2[,27:28]))>0 & sum(is.na(data2_agf3[,27:30]))>0 |
        sum(is.na(data2_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_full_idr.csv", sep = ""))
    }
    if (all(is.na(data3_agf1[,27:32]))==F | all(is.na(data3_agf2[,29:32]))==F |
        all(is.na(data3_agf3[,31:32]))==F | sum(is.na(data3_agf2[,27:28]))>0 & sum(is.na(data3_agf3[,27:30]))>0 |
        sum(is.na(data3_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_interm.csv", sep = ""))
    }
    if (all(is.na(data4_agf1[,27:32]))==F | all(is.na(data4_agf2[,29:32]))==F |
        all(is.na(data4_agf3[,31:32]))==F | sum(is.na(data4_agf2[,27:28]))>0 & sum(is.na(data4_agf3[,27:30]))>0 |
        sum(is.na(data4_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_interm_idr.csv", sep = ""))
    }
    if (all(is.na(data5_agf1[,27:32]))==F | all(is.na(data5_agf2[,29:32]))==F |
        all(is.na(data5_agf3[,31:32]))==F | sum(is.na(data5_agf2[,27:28]))>0 & sum(is.na(data5_agf3[,27:30]))>0 |
        sum(is.na(data5_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_short.csv", sep = ""))
    }
    if (all(is.na(data6_agf1[,27:32]))==F | all(is.na(data6_agf2[,29:32]))==F |
        all(is.na(data6_agf3[,31:32]))==F | sum(is.na(data6_agf2[,27:28]))>0 & sum(is.na(data6_agf3[,27:30]))>0 |
        sum(is.na(data6_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_short_idr.csv", sep = ""))
    }
  }


  ### Check that there are no missing values at age before 85p
  if(sum (is.na(data[,1:26]))==0){
    cat("There are no missing values before age 85p", fill = T)
  } else {
    if(sum(is.na(data1[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_full.csv"))
    }
    if(sum(is.na(data2[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_full_idr.csv"))
    }
    if(sum(is.na(data3[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_interm.csv"))
    }
    if(sum(is.na(data4[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_interm_idr.csv"))
    }
    if(sum(is.na(data5[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_short.csv"))
    }
    if(sum(is.na(data6[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_short_idr.csv"))
    }
  }

  ##Checks that the lists are rectangular and contain correct number of entries

  if(x==1){
    data1$cause3 <- substr(data1$cause, start = 1, stop = 3)
    data1 <- aggregate(data1[,7:32], by = list(country = data1$country, year = data1$year, sex=data1$sex, list = data1$list, agf = data1$agf, cause = data1$cause3), FUN = sum)
    data2$cause3 <- substr(data2$cause, start = 1, stop = 3)
    data2 <- aggregate(data2[,7:32], by = list(country = data2$country, year = data2$year, sex=data2$sex, list = data2$list, agf = data2$agf, cause = data2$cause3), FUN = sum)
  }

  if(x %in% 1:2){
    p1<-1659
    p2<-1570
  }
  if(x==3){
    p1<-211
    p2<-208
  }
  if(x==4){
    p1<-239
    p2<-236
  }
  if(x==5){
    p1 <- 269
    p2 <- 266
  }
  if(x==6){
    p1 <- 277
    p2 <- 275
  }

  if(nrow(data1)==3*p1*nrow(years) & nrow(data2)==3*p2*nrow(years) &
     nrow(data3)==3*106*nrow(years) & nrow(data4)==3*103*nrow(years) &
     nrow(data5)==3*17*nrow(years) & nrow(data6)==3*16*nrow(years)){
    cat ("The number of rows in the files is correct in all lists", fill = T)
  }else{
    if(nrow(data1)!=3*p1*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_full.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data2)!=3*p2*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_full_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data3)!=3*106*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_interm.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data4)!=3*103*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_interm_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data5)!=3*17*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_short.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data6)!=3*16*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_short_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
  }
}


check_totals <- function(country){
  data1 <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data2 <- read.csv(paste(country,"_input\\",country,"_d_full_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data3 <- read.csv(paste(country,"_input\\",country,"_d_interm.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data4 <- read.csv(paste(country,"_input\\",country,"_d_interm_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data5 <- read.csv(paste(country,"_input\\",country,"_d_short.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data6 <- read.csv(paste(country,"_input\\",country,"_d_short_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)

  head(data1)
  years <- unique(data1$year)
  n_years <- length(years)

  data1 <- aggregate(data1[,7:32], by = list(sex=data1$sex, year = data1$year), FUN = sum)
  data2 <- aggregate(data2[,7:32], by = list(sex=data2$sex, year = data2$year), FUN = sum)
  data3 <- aggregate(data3[,7:32], by = list(sex=data3$sex, year = data3$year), FUN = sum)
  data4 <- aggregate(data4[,7:32], by = list(sex=data4$sex, year = data4$year), FUN = sum)
  data5 <- aggregate(data5[,7:32], by = list(sex=data5$sex, year = data5$year), FUN = sum)
  data6 <- aggregate(data6[,7:32], by = list(sex=data6$sex, year = data6$year), FUN = sum)

  report <- rbind(data1-data2, data1-data3, data1-data4, data1-data5, data1-data6)
  report$sex <- rep(1:3)
  report$year <- rep(years, each = 3)
  report$file <- rep(c("full_idr", "interm", "interm_idr", "short", "short_idr"), each = 3*n_years)
  report$country <- country
  report <- report[,c(30,1,2,29,3:28)]
  report <- subset(report, rowSums(abs(round(report[,5:30],0)), na.rm = T)>0)
  report <- subset(report, report$sex<=2)
  if(nrow(report[,5:30])==0){
    cat("Death counts by ages are identical in all the files", fill = T)
  } else {
    x <- menu(c("yes", "no"), title = paste("The difference between the files is up to ", max(abs(report[,6:30]),na.rm = T), " deaths. (Single sex, age group and year). Save the output? (yes / no)", sep = ""))
    if(x==1){
      write.table(report, paste(country,"_input\\",country,"_files_dif.csv",sep=""), col.names=TRUE, row.names=FALSE,  quote = FALSE, sep=",")
      cat(paste("The outpu1t is saved in ", country, "_input folder in the file: ", country, "_files_dif.csv", sep = ""), fill = T)
    }
  }
}


#' compare HCD aggregate data with HMD
#'
#' @description write stuff here describing what the function does.
#' @details write tips here, stuff to watch out for
#' @param country character string with HCD country code
#' @export


compare_HMD <- function(country){
  data <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE)

  HMD_data <- read.csv(paste(country,"_input\\",country,"_d_HMD.csv",sep=""), header = TRUE)
  year <- unique(HMD_data$year)
  year1 <- unique(data$year)

  year2<- intersect(year, year1)


  births <- read.csv(paste(country,"_input\\",country,"_b.csv",sep=""), header = TRUE)
  births_HMD <- read.csv(paste(country,"_input\\",country,"_b_HMD.csv",sep=""), header = TRUE)

  data_m <- data[data$sex==1,]

  births_m <- births[births$sex==1,]
  births_HMD_m <- births_HMD[births_HMD$sex==1,]
  HMD_data_m <- HMD_data[HMD_data$sex==1 & HMD_data$year %in% year2,]


  data_f <- data[data$sex==2,]

  births_f <- births[births$sex==2,]
  births_HMD_f <- births_HMD[births_HMD$sex==2,]
  HMD_data_f <- HMD_data[HMD_data$sex==2 & HMD_data$year %in% year2,]

  difference <- HMD_data_f[,c(1:2,8:9)]

  for(i in year2){
    difference[which(difference$year==i),3] <- sum(HMD_data_m[which(HMD_data_m$year==i),7])+
      births_m[which(births_m$year==i),4]-births_HMD_m[which(births_HMD_m$year==i),4]-
      sum(data_m[which(data_m$year==i),7])
  }

  for(i in year2){
    difference[which(difference$year==i),4] <- sum(HMD_data_f[which(HMD_data_f$year==i),7])+
      births_f[which(births_f$year==i),4]-births_HMD_f[which(births_HMD_f$year==i),4]-
      sum(data_f[which(data_f$year==i),7])
  }

  difference$total <- rowSums(difference[,3:4])
  colnames(difference) <- c("country", "year", "males", "females", "total")
  difference <- subset(difference, rowSums(round(abs(difference[,3:5])),0)>0)
  if(nrow(difference)==0){
    cat("Deaths counts in the HCD (full list compared) are identical to the HMD", fill = T)
  }else{
    x <- menu(c("yes", "no"), title = paste("The difference between HMD and HCD full list (adjusted for difference in birth counts) is up to ", max(abs(difference[,3:4])), " deaths. (Single sex and year). \n Save the output?",sep = ""))
    if(x==1){
      write.table(difference, paste(country,"_input\\",country,"_HMD_dif.csv",sep=""), col.names=TRUE, row.names=FALSE,  quote = FALSE, sep=",")
      cat(paste("The output is saved in ", country, "_input folder in the file: ", country, "_HMD_dif.csv", sep = ""), fill = T)
    }
  }
}

check_totals_by_columns <- function(country){

  data1 <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data2 <- read.csv(paste(country,"_input\\",country,"_d_full_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data3 <- read.csv(paste(country,"_input\\",country,"_d_interm.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data4 <- read.csv(paste(country,"_input\\",country,"_d_interm_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data5 <- read.csv(paste(country,"_input\\",country,"_d_short.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data6 <- read.csv(paste(country,"_input\\",country,"_d_short_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)

  data1$diffTot <- data1$total - rowSums(data1[,8:26])
  data1$diffd95p <- data1$d95p-data1$d95-data1$d100p
  data1$diffd90p <- data1$d90p-data1$d90-data1$d95p
  data1$diffd85p <- data1$d85p-data1$d85-data1$d90p

  data2$diffTot <- data2$total - rowSums(data2[,8:26])
  data2$diffd95p <- data2$d95p-data2$d95-data2$d100p
  data2$diffd90p <- data2$d90p-data2$d90-data2$d95p
  data2$diffd85p <- data2$d85p-data2$d85-data2$d90p

  data3$diffTot <- data3$total - rowSums(data3[,8:26])
  data3$diffd95p <- data3$d95p-data3$d95-data3$d100p
  data3$diffd90p <- data3$d90p-data3$d90-data3$d95p
  data3$diffd85p <- data3$d85p-data3$d85-data3$d90p

  data4$diffTot <- data4$total - rowSums(data4[,8:26])
  data4$diffd95p <- data4$d95p-data4$d95-data4$d100p
  data4$diffd90p <- data4$d90p-data4$d90-data4$d95p
  data4$diffd85p <- data4$d85p-data4$d85-data4$d90p

  data5$diffTot <- data5$total - rowSums(data5[,8:26])
  data5$diffd95p <- data5$d95p-data5$d95-data5$d100p
  data5$diffd90p <- data5$d90p-data5$d90-data5$d95p
  data5$diffd85p <- data5$d85p-data5$d85-data5$d90p

  data6$diffTot <- data6$total - rowSums(data6[,8:26])
  data6$diffd95p <- data6$d95p-data6$d95-data6$d100p
  data6$diffd90p <- data6$d90p-data6$d90-data6$d95p
  data6$diffd85p <- data6$d85p-data6$d85-data6$d90p

  data1$list <- "full"
  data2$list <- "full_idr"
  data3$list <- "interm"
  data4$list <- "interm_idr"
  data5$list <- "short"
  data6$list <- "short_idr"


  data <- rbind(data1, data2, data3, data4, data5, data6)

  data[,33:36] <- round(data[,33:36],0)

  if(sum(abs(data[,33:36]), na.rm = T)==0){
    cat("Columns with totals are equal to the sums of corresponding columns in all the files", fill = T)
  }else{
    error <- subset(data, round(rowSums(abs(data[,33:36]), na.rm = T))>0)
    message(paste("ERROR! Columns with totals are not equal to the sums of corresponding columns. The report with mistaking lines was created in ", country,"_totals_errors.csv", sep = ""))
    write.csv(error, paste(country,file = "_input\\",country,"_totals_errors.csv", sep = ""))
  }
}

HCD_checks <- function(country){

  #### Check whether country name is specified correctly by user
  countries_list <- c ("BLR", "CZE", "GBRTENW", "EST", "FRATNP", "DEUTNP", "JPN", "LVA",
                       "LTU", "MDA", "POL", "ROU", "RUS", "ESP", "UKR", "USA")
  if(!(country %in% countries_list)){
    message ("ERROR! specify the name of the country correctly (BLR, CZE, GBRTENW, EST, FRATNP, DEUTNP, JPN, LVA, LTU, MDA, POL, ROU, RUS, ESP, UKR, USA)")
    stop ()
  }

  ### Downloading and processing the data

  data1 <- read.csv(paste(country,"_input\\",country,"_d_full.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data2 <- read.csv(paste(country,"_input\\",country,"_d_full_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data3 <- read.csv(paste(country,"_input\\",country,"_d_interm.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data4 <- read.csv(paste(country,"_input\\",country,"_d_interm_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data5 <- read.csv(paste(country,"_input\\",country,"_d_short.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)
  data6 <- read.csv(paste(country,"_input\\",country,"_d_short_idr.csv",sep=""), header = TRUE, stringsAsFactors = FALSE)

  data1 <- data1[order(data1$year,data1$sex,data1$cause),]
  data2 <- data2[order(data2$year,data2$sex,data2$cause),]
  data3 <- data3[order(data3$year,data3$sex,data3$cause),]
  data4 <- data4[order(data4$year,data4$sex,data4$cause),]
  data5 <- data5[order(data5$year,data5$sex,data5$cause),]
  data6 <- data6[order(data6$year,data6$sex,data6$cause),]

  data1[data1=="."] <-NA
  data2[data2=="."] <-NA
  data3[data3=="."] <-NA
  data4[data4=="."] <-NA
  data5[data5=="."] <-NA
  data6[data6=="."] <-NA


  ### column names are specified correctly
  columns <- c("country", "year", "sex", "list", "agf", "cause", "total", "d0", "d1", "d5",
               "d10", "d15", "d20", "d25", "d30", "d35", "d40", "d45", "d50", "d55", "d60",
               "d65", "d70", "d75", "d80", "d85p", "d85", "d90p", "d90", "d95p", "d95", "d100p")

  if(all(colnames(data1)==columns, colnames(data2)==columns, colnames(data3)==columns,
         colnames(data4)==columns, colnames(data5)==columns, colnames(data6)==columns)==T){
    cat("Names of the columns in the files are specified correctly", fill = T)
  } else {
    if (all(colnames(data1)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_full.csv", sep = ""))
    }
    if (all(colnames(data2)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_full_idr.csv", sep = ""))
    }
    if (all(colnames(data3)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_interm.csv", sep = ""))
    }
    if (all(colnames(data4)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_interm_idr.csv", sep = ""))
    }
    if (all(colnames(data5)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_short.csv", sep = ""))
    }
    if (all(colnames(data6)==columns)==F) {
      message (paste ("ERROR! Check the names of the columns in ", country, "_d_short_idr.csv", sep = ""))
    }
  }

  ### Country name is correct in all files

  variable1 <- c(data1$country, data2$country, data3$country, data4$country, data5$country, data6$country)
  c_name <- unique(variable1)

  if(length(c_name)==1 & c_name[1]==country){
    cat(paste("Country name is correct : ",c_name, sep=""), fill = T)
  } else {
    if (length(c_name)>1){
      message ("ERROR! variable ~country~ is not the same across the files")
    }
    if (length(c_name)==1 & c_name[1] != country){
      message ("ERROR! Name of the country is not identical between the files' names and variable ~country~ within the files")
    }
  }

  ### Year is between 1900 and 2018
  years <- data.frame(unique(data1$year),unique(data2$year),unique(data3$year),
                      unique(data4$year),unique(data5$year),unique(data6$year))

  if(all(sweep(years, MARGIN=1, STATS=years[,1], FUN = "=="))==T & sum(years[,] < 1900 | years >2019) ==0){
    cat (paste("The range of years is identical in all the lists : ",years[1,1],"-", years[nrow(years),1], sep = ""), fill = T)
  } else{
    if (sum(years[,] < 1900 | years >2020) >0) {
      message ("ERROR! The range of years NOT between 1900 and 2020")
    }
    if (all(sweep(years, MARGIN=1, STATS=years[,1], FUN = "=="))!=T) {
      message ("ERROR! The range of years is not the same across the lists")
    }
  }

  ### Sex is from 1 to 3
  sexes <- data.frame(unique(data1$sex),unique(data2$sex),unique(data3$sex),
                      unique(data4$sex),unique(data5$sex),unique(data6$sex))


  if(all(sexes[,1]== c(1,2,3))==T & all(sweep(sexes, MARGIN=1, STATS=sexes[,1], FUN = "=="))==T){
    cat ("Variable ~sex~ is from 1 to 3 in all the lists", fill = T)
  } else {
    message ("ERROR! Check the range of sex variables in the lists")
  }

  ### Checking list name

  full_lists <- c("icd10_4", "icd10_3", "baltic", "russia", "ukraine", "belarus")
  x <- menu(full_lists, title = "Please spesify which classification is used in full lists")

  if (all(data1$list == full_lists[x]) & all(data2$list == full_lists[x]) &
      all(data3$list=="interm") &  all(data4$list=="interm") &
      all(data5$list=="short") & all(data6$list == "short")){
    cat("The variables ~list~ are specified correctly", fill = T)
  } else {
    if(all(data1$list == full_lists[x])!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_full.csv", sep = ""))
    }
    if(all(data2$list == full_lists[x])!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_full_idr.csv", sep = ""))
    }
    if(all(data3$list == "interm")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_interm.csv", sep  =""))
    }
    if(all(data4$list == "interm")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_interm_idr.csv", sep = ""))
    }
    if(all(data5$list == "short")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_short.csv", sep = ""))
    }
    if(all(data6$list == "short")!=T) {
      message(paste("ERROR! Check the variable ~list~ in the file ", country,"_d_short_idr.csv", sep = ""))
    }
  }

  ###Check age group format
  age_formats <- data.frame(unique(data1$agf),unique(data2$agf),unique(data3$agf),
                            unique(data4$agf),unique(data5$agf),unique(data6$agf))

  if  (all(sweep(age_formats, MARGIN=1, STATS=age_formats[,1], FUN = "=="))==F){
    message ("ERROR! The age formats (variable ~agf~ ) are not the same across the lists")
  }
  if(sum(age_formats[,1]<1 |age_formats [,1] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_full.csv"))
  }
  if(sum(age_formats[,2]<1 |age_formats [,2] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_full_idr.csv"))
  }
  if(sum(age_formats[,3]<1 |age_formats [,3] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_interm.csv"))
  }
  if(sum(age_formats[,4]<1 |age_formats [,4] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_interm_idr.csv"))
  }
  if(sum(age_formats[,5]<1 |age_formats [,5] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_short.csv"))
  }
  if(sum(age_formats[,6]<1 |age_formats [,6] >4) > 0){
    message (paste("ERROR! The range of age formats is out of [1,4] in ", country, "_d_short_idr.csv"))
  }


  ### Check that agf corresponds to data correctly

  data <- rbind (data1, data2, data3, data4, data5, data6)

  data_agf1 <- subset (data, data$agf==1)
  data_agf2 <- subset (data, data$agf==2)
  data_agf3 <- subset (data, data$agf==3)
  data_agf4 <- subset(data, data$agf==4)


  if (all(is.na(data_agf1[,27:32]))==T & all(is.na(data_agf2[,29:32]))==T &
      all(is.na(data_agf3[,31:32]))==T & sum(is.na(data_agf2[,27:28]))==0 & sum(is.na(data_agf3[,27:30]))==0 &
      sum(is.na(data_agf4[,27:32]))==0){
    cat ("Age formats correspond to the data correctly", fill = T)
  }else{
    data1_agf1 <- subset (data1, data1$agf==1)
    data1_agf2 <- subset (data1, data1$agf==2)
    data1_agf3 <- subset (data1, data1$agf==3)
    data1_agf4 <- subset (data1, data1$agf==4)
    data2_agf1 <- subset (data2, data2$agf==1)
    data2_agf2 <- subset (data2, data2$agf==2)
    data2_agf3 <- subset (data2, data2$agf==3)
    data2_agf4 <- subset (data2, data2$agf==4)
    data3_agf1 <- subset (data3, data3$agf==1)
    data3_agf2 <- subset (data3, data3$agf==2)
    data3_agf3 <- subset (data3, data3$agf==3)
    data3_agf4 <- subset (data3, data3$agf==4)
    data4_agf1 <- subset (data4, data4$agf==1)
    data4_agf2 <- subset (data4, data4$agf==2)
    data4_agf3 <- subset (data4, data4$agf==3)
    data4_agf4 <- subset (data4, data4$agf==4)
    data5_agf1 <- subset (data5, data5$agf==1)
    data5_agf2 <- subset (data5, data5$agf==2)
    data5_agf3 <- subset (data5, data5$agf==3)
    data5_agf4 <- subset (data5, data5$agf==4)
    data6_agf1 <- subset (data6, data6$agf==1)
    data6_agf2 <- subset (data6, data6$agf==2)
    data6_agf3 <- subset (data6, data6$agf==3)
    data6_agf4 <- subset (data6, data6$agf==4)

    if (all(is.na(data1_agf1[,27:32]))==F | all(is.na(data1_agf2[,29:32]))==F |
        all(is.na(data1_agf3[,31:32]))==F | sum(is.na(data1_agf2[,27:28]))>0 & sum(is.na(data1_agf3[,27:30]))>0 |
        sum(is.na(data1_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_full.csv", sep = ""))
    }
    if (all(is.na(data2_agf1[,27:32]))==F | all(is.na(data2_agf2[,29:32]))==F |
        all(is.na(data2_agf3[,31:32]))==F | sum(is.na(data2_agf2[,27:28]))>0 & sum(is.na(data2_agf3[,27:30]))>0 |
        sum(is.na(data2_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_full_idr.csv", sep = ""))
    }
    if (all(is.na(data3_agf1[,27:32]))==F | all(is.na(data3_agf2[,29:32]))==F |
        all(is.na(data3_agf3[,31:32]))==F | sum(is.na(data3_agf2[,27:28]))>0 & sum(is.na(data3_agf3[,27:30]))>0 |
        sum(is.na(data3_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_interm.csv", sep = ""))
    }
    if (all(is.na(data4_agf1[,27:32]))==F | all(is.na(data4_agf2[,29:32]))==F |
        all(is.na(data4_agf3[,31:32]))==F | sum(is.na(data4_agf2[,27:28]))>0 & sum(is.na(data4_agf3[,27:30]))>0 |
        sum(is.na(data4_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_interm_idr.csv", sep = ""))
    }
    if (all(is.na(data5_agf1[,27:32]))==F | all(is.na(data5_agf2[,29:32]))==F |
        all(is.na(data5_agf3[,31:32]))==F | sum(is.na(data5_agf2[,27:28]))>0 & sum(is.na(data5_agf3[,27:30]))>0 |
        sum(is.na(data5_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_short.csv", sep = ""))
    }
    if (all(is.na(data6_agf1[,27:32]))==F | all(is.na(data6_agf2[,29:32]))==F |
        all(is.na(data6_agf3[,31:32]))==F | sum(is.na(data6_agf2[,27:28]))>0 & sum(is.na(data6_agf3[,27:30]))>0 |
        sum(is.na(data6_agf4[,27:32]))>0){
      message (paste("ERROR! Check age formats in ", country, "_d_short_idr.csv", sep = ""))
    }
  }


  ### Check that there are no missing values at age before 85p
  if(sum (is.na(data[,1:26]))==0){
    cat("There are no missing values before age 85p", fill = T)
  } else {
    if(sum(is.na(data1[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_full.csv"))
    }
    if(sum(is.na(data2[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_full_idr.csv"))
    }
    if(sum(is.na(data3[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_interm.csv"))
    }
    if(sum(is.na(data4[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_interm_idr.csv"))
    }
    if(sum(is.na(data5[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_short.csv"))
    }
    if(sum(is.na(data6[,1:26]))>0){
      message(paste("ERROR! There are missing values before age 85p in ", country, "_d_short_idr.csv"))
    }
  }

  ##Checks that the lists are rectangular and contain correct number of entries
  data1s <- data1
  data2s <- data2

  if(x==1){
    data1s$cause3 <- substr(data1s$cause, start = 1, stop = 3)
    data1s <- aggregate(data1s[,7:32], by = list(country = data1s$country, year = data1s$year, sex=data1s$sex, list = data1s$list, agf = data1s$agf, cause = data1s$cause3), FUN = sum)
    data2s$cause3 <- substr(data2s$cause, start = 1, stop = 3)
    data2s <- aggregate(data2s[,7:32], by = list(country = data2s$country, year = data2s$year, sex=data2s$sex, list = data2s$list, agf = data2s$agf, cause = data2s$cause3), FUN = sum)
  }

  if(x %in% 1:2){
    p1<-1659
    p2<-1570
  }
  if(x==3){
    p1<-211
    p2<-208
  }
  if(x==4){
    p1<-239
    p2<-236
  }
  if(x==5){
    p1 <- 269
    p2 <- 266
  }
  if(x==6){
    p1 <- 277
    p2 <- 275
  }

  if(nrow(data1s)==3*p1*nrow(years) & nrow(data2s)==3*p2*nrow(years) &
     nrow(data3)==3*106*nrow(years) & nrow(data4)==3*103*nrow(years) &
     nrow(data5)==3*17*nrow(years) & nrow(data6)==3*16*nrow(years)){
    cat ("The number of rows in the files is correct in all lists", fill = T)
  }else{
    if(nrow(data1s)!=3*p1*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_full.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data2s)!=3*p2*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_full_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data3)!=3*106*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_interm.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data4)!=3*103*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_interm_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data5)!=3*17*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_short.csv is not rectangular or has missed or extra lines", sep = ""))
    }
    if(nrow(data6)!=3*16*nrow(years)){
      message(paste("ERROR! The file ", country, "_d_short_idr.csv is not rectangular or has missed or extra lines", sep = ""))
    }
  }


  if(x %in% 1:2){
    age_spec <- read.csv("age-specific_icd3.csv",header = T, stringsAsFactors = F )
    asterisk <- read.csv("asterisk.csv")
    post_procedural <- read.csv("post-procedural.csv")
    full_list <- read.csv("Full_list_icd3.csv", header = T, stringsAsFactors = F)
  }

  if(x==1){
    sex_spec <- read.csv("sex-specific_icd4.csv", header = T, stringsAsFactors = F)
  }

  if(x==2){
    sex_spec <- read.csv("sex-specific_icd3.csv", header = T, stringsAsFactors = F)
  }

  if(x==3){
    sex_spec <- read.csv("sex-specific_baltic.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_baltic.csv")
  }

  if(x==4){
    sex_spec <- read.csv("sex-specific_russia.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_russia.csv")
  }

  if(x==5){
    sex_spec <- read.csv("sex-specific_ukraine.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_ukraine.csv")
  }

  if(x==6){
    sex_spec <- read.csv("sex-specific_belarus.csv", header = T, stringsAsFactors = F)
    age_spec <- read.csv("age-specific_belarus.csv")
  }

  males <- subset(data1, data1$sex==1)
  males <- subset(males, males$cause %in% sex_spec$females)
  males <- subset(males, males$total!=0)
  females <- subset(data1, data1$sex==2)
  females <- subset(females, females$cause %in% sex_spec$males)
  females <- subset(females, females$total!=0)
  sexes <- rbind(males, females)
  if(sum(sexes$total)==0){
    cat("No deaths with impossible cause/sex-dependence were found", fill = T)
  } else {
    message(paste(country,"_d_full.csv contains deaths with impossible cause/sex dependence. The report was created in ", country, "_sex-specific codes.csv", sep = ""))
    write.table(sexes, file = paste(country, "_input\\", country, "_sex-specific codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
  }

  ### Checking for HIV before 1982

  data1s <- subset(data1s, data1$sex==3)

  if(x %in% 1:2){
    HIV <- data1s[grep("B2[0,1,2,3,4]",data1s[,6]),]
  }

  if(x==3){
    HIV <- subset(data1, data1$cause==24)
  }

  if(x==4){
    HIV <- subset(data1, data1$cause==44)
  }

  if(x==5){
    HIV <- subset(data1, data1$cause==45)
  }

  if(x==6){
    HIV <- subset(data1, data1$cause==47)
  }

  HIV <- subset(HIV, HIV$year < 1982 & HIV$total >0)
  if(nrow(HIV) >0) {
    message("There are HIV deaths before the year 1982")}

  ### Checking for deaths with wrong cause/age dependence


  perinatal <- subset(data1s, data1s$cause %in% age_spec$perinatal)
  perinatal <- subset(perinatal, rowSums(perinatal[,12:32], na.rm = T)>0)
  perinatal$total <- perinatal$total - rowSums(perinatal[,8:11])
  if(nrow(perinatal) >0) {
    perinatal[,8:11] <- "."
    perinatal$list <- "perinatal"}

  maternal <- subset(data1s, data1s$cause %in% age_spec$maternal)
  maternal <- subset(maternal, rowSums(maternal[,c(8:10,21:32)], na.rm = T)>0)
  maternal$total <- maternal$total - rowSums(maternal[,11:20])
  if(nrow(maternal)>0){maternal[,11:20] <- "."
  maternal$list <- "maternal"}

  old <- subset(data1s, data1s$cause %in% age_spec$old_age)
  old <- subset(old, rowSums(old[,8:18])>0)
  old$total <- old$total - rowSums(old[,19:26])
  if(nrow(old)>0){old[,19:32] <- "."
  old$list <- "old_age"}

  suicide <- subset(data1s, data1s$cause %in% age_spec$suicides)
  suicide <- subset(suicide, rowSums(suicide[,8:9], na.rm = T)>0)
  suicide$total <- suicide$total - rowSums(suicide[,10:26])
  if(nrow(suicide)>0){suicide[,10:32] <- "."
  suicide$list <- "suicide"}

  ages <- rbind(perinatal, maternal, old, suicide)

  if(sum(ages$total)==0){
    cat("No deaths with impossible age/sex-dependence were found", fill = T)
  } else {
    message(paste(country,"_d_full.csv contains deaths with impossible age/sex dependence. The report was created in ", country, "_age-specific codes.csv", sep = ""))
    write.table(ages, file = paste(country, "_input\\", country, "_age-specific codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
  }

  if(x %in% 1:2){
    data_ast <- subset(data1s, data1s$cause %in% asterisk[,1])
    data_ast <- subset(data_ast, data_ast$total!=0)
    if(sum(data_ast$total)==0){
      cat("No deaths with asterisk codes were found", fill = T)
    } else {
      message(paste(country,"_d_full.csv contains deaths with asterisk codes. The report was created in ", country, "_asterisk codes.csv", sep = ""))
      write.table(data_ast, file = paste(country, "_input\\", country, "_asterisk codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    data_post <- subset(data1s, data1s$cause %in% post_procedural[,1])
    data_post <- subset(data_post, data_post$total!=0)
    if(sum(data_post$total)==0){
      cat("No deaths with post-proceduarl codes were found",fill = T)
    } else {
      message(paste(country,"_d_full.csv contains deaths with post-procedural codes. The report was created in ", country, "_post-procedural codes.csv", sep = ""))
      write.table(data_post, file = paste(country, "_input\\", country, "_post-procedural codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    ### Checking that the full list contains all the codes and no extra codes

    extra <- subset (data1s, !data1s$cause %in% full_list$Icd10)
    missed <- subset(full_list, !full_list$Icd10 %in% data1$cause)

    if(nrow(extra)==0 & nrow(missed)==0){
      cat(paste("The ICD10 codes in ", country, "_d_full.csv correspocnd to the HCD full list correctly", sep = ""), fill = T)
    }
    if(nrow(extra)>0){
      message(paste("There are extra ICD10 codes in ", country, "_d_full.csv. The output was created in ", country, "_d_full extra codes.csv", sep = ""))
      write.table(extra, file = paste(country, "_input\\", country, "_d_full extra codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }
    if(nrow(missed)>0){
      message(paste("There are missed ICD10 codes in ", country, "_d_full.csv. The output was created in ", country, "_d_full missed codes.csv", sep = ""))
      write.table(missed, file = paste(country, "_input\\", country, "_d_full missed codes.csv", sep = ""), col.names = T, row.names = F, sep = ",")
    }

    ### Checking that the full_idr list contains all the codes and no extra codes

    R_codes <- data2s[grep("R",data2s[,6]),]
    if(unique(R_codes$cause)!="R95"){
      message(paste("Check ill-defined causes (R-codes) in ", country, "_d_full_idr.csv.", sep = ""))
    }
    data1s <- data1s[!grep("R",data1[,6]),]
    data2s <- data2s[!grep("R",data2[,6]),]
    if (identical(data1s$cause,data2s$cause)==FALSE){
      message(paste("ICD10 codes in ", country, "_d_full.csv. and ", country, "_d_full_idr.csv are not the same", sep = ""))
    }
  }

  n_years <- nrow(years)

  data1_agg <- aggregate(data1[,7:32], by = list(sex=data1$sex, year = data1$year), FUN = sum)
  data2_agg <- aggregate(data2[,7:32], by = list(sex=data2$sex, year = data2$year), FUN = sum)
  data3_agg <- aggregate(data3[,7:32], by = list(sex=data3$sex, year = data3$year), FUN = sum)
  data4_agg <- aggregate(data4[,7:32], by = list(sex=data4$sex, year = data4$year), FUN = sum)
  data5_agg <- aggregate(data5[,7:32], by = list(sex=data5$sex, year = data5$year), FUN = sum)
  data6_agg <- aggregate(data6[,7:32], by = list(sex=data6$sex, year = data6$year), FUN = sum)

  report <- rbind(data1_agg-data2_agg, data1_agg-data3_agg, data1_agg-data4_agg, data1_agg-data5_agg, data1_agg-data6_agg)
  report$sex <- rep(1:3)
  report$year <- rep(years[1], each = 3)
  report$file <- rep(c("full_idr", "interm", "interm_idr", "short", "short_idr"), each = 3*n_years)
  report$country <- country
  report <- report[,c(30,1,2,29,3:28)]
  report <- subset(report, rowSums(abs(round(report[,5:30],0)), na.rm = T)>0)
  report <- subset(report, report$sex<=2)
  if(nrow(report[,5:30])==0){
    cat("Death counts by ages are identical in all the files", fill = T)
  } else {
    x <- menu(c("yes", "no"), title = paste("The difference between the files is up to ", max(abs(report[,6:30]),na.rm = T), " deaths. (Single sex, age group and year). Save the output? (yes / no)", sep = ""))
    if(x==1){
      write.table(report, paste(country,"_input\\",country,"_files_dif.csv",sep=""), col.names=TRUE, row.names=FALSE,  quote = FALSE, sep=",")
      cat(paste("The outpu1t is saved in ", country, "_input folder in the file: ", country, "_files_dif.csv", sep = ""), fill = T)
    }
  }



  HMD_data <- read.csv(paste(country,"_input\\",country,"_d_HMD.csv",sep=""), header = TRUE)
  year <- unique(HMD_data$year)
  year1 <- unique(data1$year)

  year2<- intersect(year, year1)


  births <- read.csv(paste(country,"_input\\",country,"_b.csv",sep=""), header = TRUE)
  births_HMD <- read.csv(paste(country,"_input\\",country,"_b_HMD.csv",sep=""), header = TRUE)

  data_m <- data1[data1$sex==1,]

  births_m <- births[births$sex==1,]
  births_HMD_m <- births_HMD[births_HMD$sex==1,]
  HMD_data_m <- HMD_data[HMD_data$sex==1 & HMD_data$year %in% year2,]


  data_f <- data1[data1$sex==2,]

  births_f <- births[births$sex==2,]
  births_HMD_f <- births_HMD[births_HMD$sex==2,]
  HMD_data_f <- HMD_data[HMD_data$sex==2 & HMD_data$year %in% year2,]

  difference <- HMD_data_f[,c(1:2,8:9)]

  for(i in year2){
    difference[which(difference$year==i),3] <- sum(HMD_data_m[which(HMD_data_m$year==i),7])+
      births_m[which(births_m$year==i),4]-births_HMD_m[which(births_HMD_m$year==i),4]-
      sum(data_m[which(data_m$year==i),7])
  }

  for(i in year2){
    difference[which(difference$year==i),4] <- sum(HMD_data_f[which(HMD_data_f$year==i),7])+
      births_f[which(births_f$year==i),4]-births_HMD_f[which(births_HMD_f$year==i),4]-
      sum(data_f[which(data_f$year==i),7])
  }

  difference$total <- rowSums(difference[,3:4])
  colnames(difference) <- c("country", "year", "males", "females", "total")
  difference <- subset(difference, rowSums(round(abs(difference[,3:5])),0)>0)
  if(nrow(difference)==0){
    cat("Deaths counts in the HCD (full list compared) are identical to the HMD", fill = T)
  }else{
    x <- menu(c("yes", "no"), title = paste("The difference between HMD and HCD full list (adjusted for difference in birth counts) is up to ", max(abs(difference[,3:4])), " deaths. (Single sex and year). \n Save the output?",sep = ""))
    if(x==1){
      write.table(difference, paste(country,"_input\\",country,"_HMD_dif.csv",sep=""), col.names=TRUE, row.names=FALSE,  quote = FALSE, sep=",")
      cat(paste("The output is saved in ", country, "_input folder in the file: ", country, "_HMD_dif.csv", sep = ""), fill = T)
    }
  }

  data1$diffTot <- data1$total - rowSums(data1[,8:26])
  data1$diffd95p <- data1$d95p-data1$d95-data1$d100p
  data1$diffd90p <- data1$d90p-data1$d90-data1$d95p
  data1$diffd85p <- data1$d85p-data1$d85-data1$d90p

  data2$diffTot <- data2$total - rowSums(data2[,8:26])
  data2$diffd95p <- data2$d95p-data2$d95-data2$d100p
  data2$diffd90p <- data2$d90p-data2$d90-data2$d95p
  data2$diffd85p <- data2$d85p-data2$d85-data2$d90p

  data3$diffTot <- data3$total - rowSums(data3[,8:26])
  data3$diffd95p <- data3$d95p-data3$d95-data3$d100p
  data3$diffd90p <- data3$d90p-data3$d90-data3$d95p
  data3$diffd85p <- data3$d85p-data3$d85-data3$d90p

  data4$diffTot <- data4$total - rowSums(data4[,8:26])
  data4$diffd95p <- data4$d95p-data4$d95-data4$d100p
  data4$diffd90p <- data4$d90p-data4$d90-data4$d95p
  data4$diffd85p <- data4$d85p-data4$d85-data4$d90p

  data5$diffTot <- data5$total - rowSums(data5[,8:26])
  data5$diffd95p <- data5$d95p-data5$d95-data5$d100p
  data5$diffd90p <- data5$d90p-data5$d90-data5$d95p
  data5$diffd85p <- data5$d85p-data5$d85-data5$d90p

  data6$diffTot <- data6$total - rowSums(data6[,8:26])
  data6$diffd95p <- data6$d95p-data6$d95-data6$d100p
  data6$diffd90p <- data6$d90p-data6$d90-data6$d95p
  data6$diffd85p <- data6$d85p-data6$d85-data6$d90p

  data1$list <- "full"
  data2$list <- "full_idr"
  data3$list <- "interm"
  data4$list <- "interm_idr"
  data5$list <- "short"
  data6$list <- "short_idr"


  data <- rbind(data1, data2, data3, data4, data5, data6)

  data[,33:36] <- round(data[,33:36],0)

  if(sum(abs(data[,33:36]), na.rm = T)==0){
    cat("Columns with totals are equal to the sums of corresponding columns in all the files", fill = T)
  }else{
    error <- subset(data, round(rowSums(abs(data[,33:36]), na.rm = T))>0)
    message(paste("ERROR! Columns with totals are not equal to the sums of corresponding columns. The report with mistaking lines was created in ", country,"_totals_errors.csv", sep = ""))
    write.csv(error, paste(country,file = "_input\\",country,"_totals_errors.csv", sep = ""))
  }
}

HMD_downloads <- function(country, HMD_login, HMD_password){

  if(missing(HMD_login)){
    HMD_login <- readline("type in HMD username (usually your email): ")
    HMD_login <- as.character(HMD_login)
  }

  if(missing(HMD_password)){
    HMD_password <- readline("type in HMD password: ")
    HMD_password <- as.character(HMD_password)
  }

  require(reshape2)
  require(HMDHFDplus)

  data_new <- readHMDweb(country, "Deaths_5x1", HMD_login, HMD_password, fixup = T)

  males <- data_new[,c(1:2,4)]
  males <- dcast(males, Year ~ Age, value.var = "Male")
  males$sex <- 1

  females <- data_new[,c(1:2,3)]
  females <- dcast(females, Year ~ Age, value.var = "Female")
  females$sex <- 2

  totals <- data_new[,c(1:2,5)]
  totals <- dcast(totals, Year ~ Age, value.var = "Total")
  totals$sex <- 3

  deaths <- rbind(males, females, totals)
  colnames(deaths) <- c("year", "d0", "d1", "d5", "d10", "d15", "d20", "d25", "d30", "d35", "d40", "d45", "d50", "d55",
                        "d60", "d65", "d70", "d75", "d80", "d85", "d90", "d95", "d100", "d105", "d110p", "sex")

  deaths$country <- country
  deaths$list <- "HMD"
  deaths$agf <- 4
  deaths$cause <- 0
  deaths$total <- rowSums(deaths[,2:25])
  deaths$d100p <- rowSums(deaths[,23:25])
  deaths$d95p <- deaths$d95+deaths$d100p
  deaths$d90p <- deaths$d90+deaths$d95p
  deaths$d85p <- deaths$d85+deaths$d90p
  deaths <- deaths[,c("country", "year", "sex", "list", "agf", "cause", "total", "d0", "d1", "d5", "d10", "d15", "d20",
                      "d25", "d30", "d35", "d40", "d45", "d50", "d55", "d60", "d65", "d70", "d75","d80", "d85p", "d85",
                      "d90p", "d90", "d95p", "d95", "d100p")]
  write.csv(deaths, file = paste(country,"_input\\", country, "_d_HMD.csv", sep = ""), row.names = F)


  data_new <- readHMDweb(country, "Exposures_5x1", HMD_login, HMD_password, fixup = T)

  males <- data_new[,c(1:2,4)]
  males <- dcast(males, Year ~ Age, value.var = "Male")
  males$sex <- 1

  females <- data_new[,c(1:2,3)]
  females <- dcast(females, Year ~ Age, value.var = "Female")
  females$sex <- 2

  totals <- data_new[,c(1:2,5)]
  totals <- dcast(totals, Year ~ Age, value.var = "Total")
  totals$sex <- 3

  exposures <- rbind(males, females, totals)
  colnames(exposures) <- c("year", "e0", "e1", "e5", "e10", "e15", "e20", "e25", "e30", "e35", "e40", "e45", "e50", "e55",
                           "e60", "e65", "e70", "e75", "e80", "e85", "e90", "e95", "e100", "e105", "e110p", "sex")

  exposures$country <- country
  exposures$agf <- 4
  exposures$total <- rowSums(exposures[,2:25])
  exposures$e100p <- rowSums(exposures[,23:25])
  exposures$e95p <- exposures$e95+exposures$e100p
  exposures$e90p <- exposures$e90+exposures$e95p
  exposures$e85p <- exposures$e85+exposures$e90p
  exposures <- exposures[,c("country", "year", "sex", "agf", "total", "e0", "e1", "e5", "e10", "e15", "e20",
                            "e25", "e30", "e35", "e40", "e45", "e50", "e55", "e60", "e65", "e70", "e75","e80", "e85p", "e85",
                            "e90p", "e90", "e95p", "e95", "e100p")]
  write.csv(exposures, file = paste(country,"_input\\", country,"_e.csv", sep = ""), row.names = F)


  data_new <- readHMDweb(country, "Births", HMD_login, HMD_password, fixup = T)

  births <- data.frame("country" = country, "year" = rep(data_new$Year, 3), "sex" = rep(1:3, each = length(data_new$Year)),"b" = 0)
  births$b <- c(data_new$Male, data_new$Female, data_new$Total)
  write.csv(births, file = paste(country,"_input\\", country, "_b_HMD.csv", sep = ""), row.names = F)
  cat("The files were created in the country's folder", fill = T)
}



