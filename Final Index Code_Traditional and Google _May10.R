library(data.table)
library(truncnorm)

setwd ("C:\\Users\Ramesh Narayanan\\Desktop\\FINAL FINAL CODEs\\HO")
Orig_Variables= read.csv(file.choose())

head(Orig_Variables)
#removing rows that are missing zillowhousesales' data (eg. December 2017 or before March 2003)
Orig_Variables_2=Orig_Variables[complete.cases(Orig_Variables[ , 3]),]

#Comment out this part for traditional data
#removing rows that are missing Google Trends' data (eg. Lakeland)
Orig_Variables_2=Orig_Variables_2[complete.cases(Orig_Variables_2[ , 23]),]
head(Orig_Variables_2)

####DELETE COLUMNS IF ONLY GOOGLE TRENDS / TRADITIONAL NEEDS TO BE USED.Or
####Load DATASET only with the appropriate columns


#Normalize the traditional variables between 0 and 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


Orig_Variables_2[,5:21]<- as.data.frame(lapply(Orig_Variables_2[,5:21], normalize))
head(Orig_Variables_2)






#-------------------------------------------------------------------------------------
#                 IDENTIFY DYNAMIC VARIABLES WITHIN THIS SPACE

# Number of variables desired in creating Index
num_index_var_wanted = 5

# Correlation Cutoff point for a good ranking in the MSA Rankings evaluation with RAW numbers
Raw_corr_cutoff=0.6

# Correlation Cutoff point for a good ranking in the MSA Rankings evaluation with MONTH OVER MONTH Percentage change numbers
MoM_perc_corr_cutoff=0.6

# Correlation Cutoff point for a good ranking in the MSA Rankings evaluation with YEAR OVER YEAR Percentage change numbers
YoY_perc_corr_cutoff=0.6
#-----------------------------------------------------------------------------------------------------------------------




# Creating list of MSA names
MSA_MAE_names=list()
MSA_DIR_names=list()

MSA_names=unique(Orig_Variables_2$MSA)
MSA_names

Num_of_MSA = length(MSA_names)
Num_of_MSA

for (MSA in MSA_names){
  #MSA_1=c("MAE_", MSA)
  MSA_1=paste("MAE_", MSA, sep = "", collapse = NULL)
  MSA_MAE_names=c(MSA_MAE_names, MSA_1)
  
  MSA_2=paste("dir_", MSA, sep = "", collapse = NULL)
  MSA_DIR_names=c(MSA_DIR_names, MSA_2)
}

MSA_MAE_names
MSA_DIR_names


# Creating list of weight names
weight_col_names=list()
for (g in 1:num_index_var_wanted){
  
  weight_name=paste("weight", g, sep = "", collapse = NULL)
  weight_col_names=c(weight_col_names, weight_name)
  
}




#Choosing the highest correlated variables to zillowhousesales to make up our index
table_of_var=NULL

testdata<-Orig_Variables_2
testdata$MSA=NULL
testdata$Date=NULL
testdata$Population=NULL
head(testdata)


indexdata<-testdata
indexdata$zillowhousesales=NULL
head(indexdata)


bestcol=NULL
colnameslist=NULL

for (i in 1:num_index_var_wanted){
  
  correlation=cor(testdata$zillowhousesales,indexdata,use='complete.obs')
  correlation

  df=data.frame(correlation)
 
  #find the variable with the best correlated column
  number=which(df == max(df), arr.ind = TRUE)
  number
  number=as.list(number)
  number=number[-1]
  number=as.numeric(number)
  number
  bestcol=cbind(bestcol,indexdata[,number])
  bestcol
  colname=names(df)[which.max(apply(df,MARGIN=2,min))]
  colname
  indexdata=indexdata[,-number]
  colnameslist=cbind(colnameslist,colname)
  colnameslist=as.list(colnameslist)
}

# Name the columns
colnames(bestcol)=colnameslist
head(bestcol)

table_of_var<-bestcol

#------------------------------------------------------------------
#Create output NULL matrices

index_table=NULL
output=NULL
weight_table=NULL

#Creating table that will store zillowhousesales values and index values

MSA=Orig_Variables_2[,1]
Date=Orig_Variables_2[,2]
Population=Orig_Variables_2[,4]
zillowhousesales=Orig_Variables_2[,3]

index_table=data.frame(MSA,
                       Date,
                       Population,
                       zillowhousesales)

head(index_table)


nrow_index=nrow(table_of_var)
nrow_index




# ****************************************LARGE LOOP THAT TAKES WEIGHT,
#                                           CREATE INDEX,
#                                            GOES THROUGH EVALUATIONS
#                                                 STARTS HERE ********************************



#Creating the Index

for (f in 1:20){ 

  
  Index=NULL
  un_weights_for_index=rtruncnorm(n=num_index_var_wanted, a=0, b=1)
  un_weights_for_index
  weights_for_index = un_weights_for_index/sum(un_weights_for_index)
  weights_for_index
  
  
  
  for (m in 1:nrow_index){
    var_row= table_of_var[m,]

    Product_weight_variables=weights_for_index*var_row
    Product_weight_variables=data.frame(t(Product_weight_variables))
    Index_row=rowSums(Product_weight_variables)
    Index=rbind(Index,Index_row)
  }

  Table_of_Indexes=NULL
  
  Table_of_Indexes=data.frame(MSA,
                         Date,
                         Population,
                         zillowhousesales)
  
  head(Table_of_Indexes)
  
  Index_t<-Index
  colnames(Index_t)<-c("Index")
  
  nams=c(1:(nrow(Index_t)))
  row.names(Index_t) = nams
  row.names(Index_t) = make.names(nams, unique=TRUE)

   
  Table_of_Indexes=cbind(Table_of_Indexes,Index_t)
  head(Table_of_Indexes)
  
 
  
  ##############################################################################
  #Evaluation #1 - MSA Ranking 
  
  data=Table_of_Indexes
  #Normalize zillow sales in new column in a new data frame
  
  data2=data
  data2$norm_zillow=data$zillowhousesales/data$Population
  data2$Index=data2$Index/data2$Population
  data2$Index=data2$Index*100000
  
  zillowrankmatrix=NULL
  indexrankmatrix=NULL
  correlation_allMSA=NULL
  correlation_3MSA=NULL
  
  
  for (row in unique(data2$Date)) {
      subsetdata <- subset(data2, row==data2$Date)
      #Ranking the zillow house data across MSA's at monthly level
      zillowrankrow= rank(-subsetdata$norm_zillow,ties.method = "first")
      zillowrankmatrix= rbind(zillowrankmatrix,zillowrankrow)
      # Ranking the index across MSA's at monthly level
      indexrankrow = rank(-subsetdata$Index,ties.method = "first")
      indexrankmatrix= rbind(indexrankmatrix,indexrankrow)
      # Creating the correlation matrix 
      correlationrow_allMSA=cor(zillowrankrow,indexrankrow)
      correlation_allMSA=rbind(correlation_allMSA,correlationrow_allMSA)
      
      correlationrow_3MSA=cor(zillowrankrow[1:3],indexrankrow[1:3])
      correlation_3MSA=rbind(correlation_3MSA,correlationrow_3MSA)
  }
  
  
  
  #Percent of good rankings for all MSA's
  accuracy_allMSA=sum(correlation_allMSA>Raw_corr_cutoff,na.rm=TRUE)/length(correlation_allMSA)
  #Percent of good ranking for the select 3 MSA's
  accuracy_3MSA=sum(correlation_3MSA>Raw_corr_cutoff,na.rm=TRUE)/length(correlation_3MSA)
  
  
  #Rename the column names
  colnames(correlation_allMSA)<-c("Correlation_All_MSA")
  colnames(correlation_3MSA)<-c("Correlation_3_MSA")
  
  

  
  ######################################################################################
  # Evaluation #2 - Month over Month Evaluation
  
  
  #MoM for Housing
  mom_housing = NULL
  for (row in unique(data$MSA)) {
    bar <- subset(data, row==data$MSA)
    mom_hous_diff = diff(bar$zillowhousesales)
    
    # Creating zillowhousesales with one less row to calculate percentage without error
    nrow_4_perc_MoM=nrow(bar)-1
    zillowhousesales_4_perc_MoM=data.frame(bar$zillowhousesales)
    zillowhousesales_4_perc_MoM=zillowhousesales_4_perc_MoM[1:nrow_4_perc_MoM,]
    
    mom_hous_perc=(mom_hous_diff/zillowhousesales_4_perc_MoM)*100
    mom_housing = cbind(mom_housing,mom_hous_perc)
  }
  
  
  #MoM for Index
  mom_index = NULL
  for (row in unique(data$MSA)) {
    bar <- subset(data, row==data$MSA)
    mom_index_diff = (diff(bar$Index))
    
    # Creating Index with one less row to calculate percentage without error
    nrow_4_perc_MoM=nrow(bar)-1
    Index_4_perc_MoM=data.frame(bar$Index)
    Index_4_perc_MoM=Index_4_perc_MoM[1:nrow_4_perc_MoM,]
    
    mom_index_perc=(mom_index_diff/Index_4_perc_MoM)*100
    mom_index = cbind(mom_index,mom_index_perc)
  }
  
  
  
  
  #Mean Absolute Error Calculation
  n=nrow(mom_index-1)
  k=ncol(mom_index)
    
  mom_abserror = matrix(0,n,k)
  for (i in 1:n-1) {
    for (j in 1:k){
      mom_abserror[i,j] = abs(mom_index[i,j] - mom_housing[i,j])
   }
  }
  
  colnames(mom_abserror) <- MSA_names
  
    
  #Calculating Mean Absolute Error for each MSA
  mom_MAE = colMeans(mom_abserror)
  mom_MAE=data.matrix(mom_MAE)
  mom_MAE=t(mom_MAE)
  
  colnames(mom_MAE)=MSA_MAE_names
  
   
  #Calculating Mean Absolute Error as an average of all MSA's
  mom_averageMAE=rowMeans(mom_MAE)
    
    
  # Directional Co incidence 
  
  n=nrow(mom_index-1)
  k=ncol(mom_housing)
  
  mom_direction = matrix(0,n,k)
  for (i in 1:n ){
    for (j in 1:k){
      if ((mom_index[i,j]>0) & (mom_housing[i,j]>0)) {
        mom_direction[i,j] = 1
      }
      else if ((mom_index[i,j])<0 & (mom_housing[i,j]<0)){
        mom_direction[i,j] = 1
      }
      else {
        mom_direction[i,j] = 0
      }
    }
  }
  
  
  
  
  #Remove one row from mom_direction since zillow hosing data is not available for the last month
  mom_direction=head(mom_direction,-1)
  
  #Direction accuracy for each MSA
  mom_samedir_accuracy = colSums(mom_direction)/nrow(mom_direction)
  mom_samedir_accuracy=data.matrix(mom_samedir_accuracy)
  mom_samedir_accuracy=t(mom_samedir_accuracy)
  colnames(mom_samedir_accuracy) <- MSA_DIR_names
  
  #Direction accuracy as an average of all MSA's
  mom_average_dir_accuracy=rowMeans(mom_samedir_accuracy)
    
    
    
  #Creating rank matrix for index
  mom_index_rank=apply(-mom_index,1,rank,ties.method='first')
  mom_index_rank=t(mom_index_rank)
    
  #Creating rank matrix for housing
  mom_house_rank=apply(-mom_housing,1,rank,ties.method='first')
  mom_house_rank=t(mom_house_rank)
  mom_house_rank=head(mom_house_rank,-1)
    
    
  #Correlations
  mom_cor_allMSA=NULL
  mom_cor_3MSA=NULL
    
  #Correlation for all MSA's
  mom_cor_allMSA=sapply(seq.int(dim(mom_house_rank)[1]), function(i) cor(mom_house_rank[i,], mom_index_rank[i,]))
  #Correlation of 3 MSA's
  mom_cor_3MSA=sapply(seq.int(dim(mom_house_rank)[1]), function(i) cor(mom_house_rank[i,1:3], mom_index_rank[i,1:3]))
    
  #FINDING THE PERCENT OF GOOD RANKINGS
  #FOR ALL MSA's
  mom_cor_acc_all=sum(mom_cor_allMSA>MoM_perc_corr_cutoff,na.rm=TRUE)/length(mom_cor_allMSA)
  #FOR SELECT 3 MSA's
  mom_cor_acc_3MSA=sum(mom_cor_3MSA>MoM_perc_corr_cutoff,na.rm=TRUE)/length(mom_cor_3MSA)
    
  
  #Column bind the two correlation accuracies  
  mom_cor_acc = cbind(mom_cor_acc_all, mom_cor_acc_3MSA)
  colnames(mom_cor_acc) <- c("mom_cor_acc_all","mom_cor_acc_3MSA")

    
    
    
  ###############################################################################
  # Evaluation # 3 - Year over Year Evaluation 
  
  #CALCULATING YOY CHANGE FOR ZILLOW HOUSE SALES
  yoy_housing = NULL
  for (row in unique(data$MSA)) {    
    subsetdata2 <- subset(data, row==data$MSA)
    yoy_hous_diff = diff(subsetdata2$zillowhousesales,lag=12)
    
    # Creating zillowhousesales with 12 less row to calculate percentage without error
    nrow_4_perc_YoY=nrow(subsetdata2)-12
    zillowhousesales_4_perc_YoY=data.frame(subsetdata2$zillowhousesales)
    zillowhousesales_4_perc_YoY=zillowhousesales_4_perc_YoY[1:nrow_4_perc_YoY,]
    
    yoy_hous_perc=(yoy_hous_diff/zillowhousesales_4_perc_YoY)*100
    yoy_housing = cbind(yoy_housing,yoy_hous_perc)
  }
    
  colnames(yoy_housing) <- MSA_names
  
  

  
  #CALCULATING YOY CHANGE FOR INDEX
  yoy_index = NULL
  for (row in unique(data$MSA)) {
    subsetdata3 <- subset(data, row==data$MSA)
    yoy_index_diff = diff(subsetdata3$Index,lag=12)


    # Creating Index with 12 less row to calculate percentage without error
    nrow_4_perc_YoY=nrow(subsetdata3)-12
    Index_4_perc_YoY=data.frame(subsetdata3$Index )
    Index_4_perc_YoY=Index_4_perc_YoY[1:nrow_4_perc_YoY,]
    
    yoy_index_perc=(yoy_index_diff/Index_4_perc_YoY)*100
    yoy_index = cbind(yoy_index,yoy_index_perc)
  }
    
  
  colnames(yoy_index) <- MSA_names
  
  

  
  
  #FINDING THE ABSOLUTE ERROR MATRIX
    
  n=nrow(yoy_index)
  k=ncol(yoy_index)
      
  yoy_abserror= matrix(0,n,k)
  for (i in 1:n) {
    for (j in 1:k){
      yoy_abserror[i,j] = abs(yoy_index[i,j] - yoy_housing[i,j])
      }
  }
      
  colnames(yoy_abserror) <- MSA_names    
  
      
  #CALCULATE MEAN ABSOLUTE ERROR
  yoy_MAE = colMeans(yoy_abserror)
  yoy_MAE=data.matrix(yoy_MAE)
  yoy_MAE=t(yoy_MAE)
  colnames(yoy_MAE) <- MSA_MAE_names
  
  #Calculating Mean Absolute Error as an average of all MSA's
  yoy_averageMAE=rowMeans(yoy_MAE)
  
  #DIRECTION Co incidence
  
  n=nrow(yoy_index)
  k=ncol(yoy_index)
  yoy_direction = matrix(0,n,k)
  for (i in 1:n) {
    for (j in 1:k){
      if ((yoy_index[i,j]>0 & yoy_housing[i,j]>0) | (yoy_index[i,j]<0 & yoy_housing[i,j]<0)) {
        yoy_direction[i,j] = 1
      }
      else {
        yoy_direction[i,j] = 0
      }
    }
  }
  
  colnames(yoy_direction) <- MSA_names

    
  #DIRECTION ACCURACY
  yoy_samedir_accuracy=colSums(yoy_direction)/nrow(yoy_direction)

  
  
  #Direction accuracy for each MSA
  yoy_samedir_accuracy=data.matrix(yoy_samedir_accuracy)
  yoy_samedir_accuracy=t(yoy_samedir_accuracy)
  colnames(yoy_samedir_accuracy) <- MSA_DIR_names
  
  #Direction accuracy as an average of all MSA's
  yoy_average_dir_accuracy=rowMeans(yoy_samedir_accuracy)
  
  
  #Creating rank matrix for index
  yoy_index_rank=apply(-yoy_index,1,rank,ties.method='first')
  yoy_index_rank=t(yoy_index_rank)
  
  #Creating rank matrix for housing
  yoy_house_rank=apply(-yoy_housing,1,rank,ties.method='first')
  yoy_house_rank=t(yoy_house_rank)
  
  
  #CORRELATIONS
  
  # Correlation for ALL MSA's
  yoy_cor_allMSA=sapply(seq.int(dim(yoy_house_rank)[1]), function(i) cor(yoy_house_rank[i,], yoy_index_rank[i,]))
  
  #Correlation of 3 MSA's
  yoy_cor_3MSA=sapply(seq.int(dim(yoy_house_rank)[1]), function(i) cor(yoy_house_rank[i,1:3], yoy_index_rank[i,1:3]))
  
  
  
  #FINDING THE PERCENT OF GOOD RANKINGS
  
  #FOR ALL MSA's
  yoy_cor_acc_all=sum(yoy_cor_allMSA>YoY_perc_corr_cutoff,na.rm=TRUE)/length(yoy_cor_allMSA)
  
  #FOR SELECT 3 MSA's
  yoy_cor_acc_3MSA=sum(yoy_cor_3MSA>YoY_perc_corr_cutoff,na.rm=TRUE)/length(yoy_cor_3MSA)
  
  
  #Column bind the two correlation accuracies
  yoy_cor_acc = cbind(yoy_cor_acc_all, yoy_cor_acc_3MSA)
  colnames(yoy_cor_acc) <- c("yoy_cor_acc_all","yoy_cor_acc_3MSA")
  
  
  ###################################################################################
  # Evaluation # 4 - Leading Evaluation (formerly Quarter over Quarter Evaluation)
  
  
  #Creating the standardized data for Index and Housing
  data$scaled_housing = scale(data$zillowhousesales)
  data$scaled_index = scale(data$Index)
  
  
  #Calculating the Quarterly Mean Absolute Error
    
  qoq_abserror_df=NULL
  for (row in unique(data$MSA)){ 
    qoqsubsetdata <- subset(data, row==data$MSA)
    count=nrow(qoqsubsetdata)-4

    for (i in 1:count) 
    {
      qoqabserror = abs(qoqsubsetdata$scaled_index[i] - qoqsubsetdata$scaled_housing[i+3])
      qoq_abserror_df=rbind(qoq_abserror_df,qoqabserror)
    }
  }

  #Split the column into equal columns(based on mumber of MSA) with equal rows
  
  qoq_abserror_df=split(qoq_abserror_df,rep(1:Num_of_MSA,each=count))
  qoq_MAE=data.frame(qoq_abserror_df)
  

  
  #CALCULATE MEAN ABSOLUTE ERROR
  colnames(yoy_MAE) <- MSA_names
  qoq_MAE = colMeans(qoq_MAE)
  qoq_MAE=t(qoq_MAE)
  colnames(qoq_MAE) <- MSA_MAE_names
  
  
  #Calculating Mean Absolute Error as an average of all MSA's
  qoq_averageMAE=rowMeans(qoq_MAE)
  
    
  #####################################################################################    
    
  #FORMATTING THE OUTPUT
    
  #COMBINE WHATEVER IS TO BE DISPLAYED IN THE RESULT
    
  weights_for_index=as.matrix(weights_for_index)
  weights_for_index=t(weights_for_index)
  
  
  colnames(weights_for_index)<-c(weight_col_names)

  final_output=cbind(mom_cor_acc,mom_averageMAE,mom_average_dir_accuracy,yoy_cor_acc,yoy_averageMAE,yoy_average_dir_accuracy)
  final_output=cbind(final_output,qoq_averageMAE,accuracy_allMSA,accuracy_3MSA,weights_for_index)   
  
  #To combine results during each iteration
  
  Index_name=paste("Index", f, sep = "", collapse = NULL)
  #Index=data.frame(Index)
  colnames(Index)=Index_name
  index_table=cbind(index_table,Index)
  
  weight_table=rbind(weight_table,weights_for_index)
  output=rbind(output,final_output)

    
}

# ****************************************LARGE LOOP THAT SELECTS WEIGHT,
#                                           CREATE INDEX,
#                                            GOES THROUGH EVALUATIONS
#                                                 ENDS HERE ********************************

warnings()
head(output)


output=data.frame(output)


#Now we have x weights (x rows, we can sort and remove based on several criteria in parts)


#Index based on MSA difference
output_msadiff=output[order(-output$accuracy_allMSA,-output$accuracy_3MSA,output$mom_average_dir_accuracy),]

#Index based on MoM evaluation
output_mom=output[order(-output$mom_cor_acc_all,-output$mom_average_dir_accuracy,output$mom_averageMAE),]

#Index based on YoY evaluation
output_yoy=output[order(-output$yoy_cor_acc_all,-output$yoy_average_dir_accuracy,output$yoy_averageMAE),]

#Index based on QoQ evaluation
output_qoq=output[order(output$qoq_averageMAE,-output$mom_cor_acc_all,-output$yoy_cor_acc_all),]



#Print results into csv

write.csv(colnameslist,file="Traditioanl_Variables_selected5a.csv")
write.csv(weight_table, file="weight_tablea.csv")
write.csv(index_table, file="index_tablea.csv")


write.csv(output_mom,file="Traditional_MoM_Best_weights5a.csv")
write.csv(output_msadiff,file="Traditional_MSAdiff_Best_weights5a.csv")
write.csv(output_yoy,file="Tradional_YoY_Best_weights5a.csv")
write.csv(output_qoq,file="Traditional_QoQ_Best_weights5a.csv")


