
Orig_Variables= read.csv(file.choose())

head(Orig_Variables)

#----------SELECT YOUR 5 VARIABLES BELOW----------------------------------
x1_var= Orig_Variables$homeshousescondo
x2_var= Orig_Variables$dmv
x3_var= Orig_Variables$flight
x4_var= Orig_Variables$unemploymentbenefits
x5_var= Orig_Variables$starbucksapple

#----------SELECT YOUR WEIGHTS MATRIX BELOW----------------------------------
weights_selection= data.frame(expand.grid(x1 = seq(0, 1, 0.5), 
            x2 = seq(0, 1, 0.5), 
            x3 = seq(0, 1, 0.5),  
            x4 = seq(0, 1, 0.5),  
            x5 = seq(0, 1, 0.5)))

head(weights_selection)

Good_Weights=NULL
Good_Index=NULL
Results=NULL

num_weight_rows=nrow(weights_selection)
num_weight_rows

for (w in 1:num_weight_rows) {
  #Take each row of weights and muliplty is by the 5 columns / variables
  #Add up the reseults from multiplying the 5 columns and variables to get one column for the index
  Index=(weights_selection[w,1]*x1_var) + 
        (weights_selection[w,2]*x2_var) +
        (weights_selection[w,3]*x3_var) +
        (weights_selection[w,4]*x4_var) +
        (weights_selection[w,5]*x5_var)
  Index
  
  Calculations=data.frame(cbind(Orig_Variables[,1:3],Index))
  head(Calculations)
  
  #removing rows that are missing zillowdata
  Calculations_2=Calculations[complete.cases(Calculations[ , 3]),]
  
  #removing rows that are missing Index data
  Calculations_2=Calculations_2[complete.cases(Calculations_2[ , 4]),]
  head(Calculations_2)
  Calculations_2$D_Error=Calculations_2$zillowhousesales-Calculations_2$Index
  
  # Calculated the mean absolute error at each point when compared to zillow housing
  MAE=mean(abs(Calculations_2$D_Error))
  MAE
  # calculate the standard deviation.
  STD=sd(Calculations_2$D_Error)
  STD
  
  
  # If those numbers do not meet a certain threshold then don't record.
  # If those numbers meet the threshold, then :
        # 1.) record the trial number and the weights in a matrix, 
        #2.) and save the column of the index in a database.
  ifelse( MAE<1750 & STD<1600, (Good_Weights = rbind(Good_Weights,weights_selection[w,])), (Bad_weights=0))
  ifelse( MAE<1750 & STD<1600, (Good_Index = cbind(Good_Index,Index)), (Bad_Index=0))
  ifelse( MAE<1750 & STD<1600, (Results = rbind(Results, (cbind(w,MAE,STD)))), (Bad_Index=0))

}

Good_Index = data.frame(Good_Index)
Results = data.frame(Results)
min(Results$MAE)
max(Results$MAE)


min(Results$STD)
max(Results$STD)

write.csv(Results, file="Good Indexes Results.csv")
write.csv(Good_Index, file="Good Indexes.csv")
write.csv(Good_Weights, file="Weights of Good Indexes.csv")


