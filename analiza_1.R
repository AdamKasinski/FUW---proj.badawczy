
filenames = list.files("dane_csv_a", pattern="*.csv", full.names=TRUE)
ldf = lapply(filenames, read.csv)

ile_AX = function(){
  ile = 0
  for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:(nrow(data))){
        if (data[row, "ď.żlit2"] == "A" & data[row, "lit3"] =="X")
          ile = ile+1
          }
  }
  return(ile)
  }


ile_AX_correct = function(){
    ile = 0
    for (i in 1:length(ldf)){
        data = ldf[[i]]
        for (row in 1:nrow(data)){
          if (data[row, "ď.żlit2"] == "A" & data[row, "lit3"] =="X" & data[row, "key_resp_4.keys"] == "z")
            ile = ile +1}} 
            return(ile)
    } 

ile_AX_wrong = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] == "A" & data[row, "lit3"] =="X" & data[row, "key_resp_4.keys"] != "z")
          ile = ile+1}} 
      return(ile)
    } 
    
ile_smX = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] =="X")
          ile = ile+1 }} 
      return(ile)
    }
    
ile_smX_correct = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] =="X" & data[row, "key_resp_4.keys"] != "z" )
          ile = ile+1}} 
      return(ile)
    }

ile_smX_wrong = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] =="X" & data[row, "key_resp_4.keys"] == "z" )
          ile = ile+1}}
      return(ile)
    }
    
ile_Asm = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "lit3"] =="A")
          ile = ile+1}} 
      return(ile)
    }
    
ile_Asm_correct = function(){
      ile = 0
      for (i in 1:length(ldf)){
        data = ldf[[i]]
        for (row in 1:nrow(data)){
          if (data[row, "lit3"] =="A" & data[row, "key_resp_4.keys"] != "z" )
            ile = ile+1}}
      return(ile)
    }
    
ile_Asm_wrong = function(){
      ile = 0
      for (i in 1:length(ldf)){
        data = ldf[[i]]
        for (row in 1:nrow(data)){
          if (data[row, "lit3"] =="A" & data[row, "key_resp_4.keys"] == "z" )
            ile = ile +1}}
      return(ile)
    }
    
    
ile_smsm = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
        for (row in 1:nrow(data)){
          if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] !="X")
            ile = ile+1}}
      return(ile)
    }
ile_smsm()    
ile_smsm_correct = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] !="X" & data[row, "key_resp_4.keys"] != "z" )
          ile = ile+1}} 
      return(ile)
    }
    
ile_smsm_wrong = function(){
    ile = 0
    for (i in 1:length(ldf)){
      data = ldf[[i]]
      for (row in 1:nrow(data)){
        if (data[row, "ď.żlit2"] != "A" & data[row, "lit3"] !="X" & data[row, "key_resp_4.keys"] == "z" )
          ile = ile+1}} 
      return(ile)
    }
    
library(dplyr)
  

x= data.frame("ile ogólem" =c(ile_AX(), ile_smX(), ile_Asm(), ile_smsm()) , "poprawne" = c(ile_AX_correct(), ile_smX_correct(),ile_Asm_correct(), ile_smsm_correct()), 
                   "bledne" = c(ile_AX_wrong(), ile_smX_wrong(), ile_Asm_wrong(), ile_smsm_wrong()), "procent poprawnych" = c(ile_AX_correct()/ile_AX()*100, 
                   ile_smX_correct()/ile_smX() *100, ile_Asm_correct()/ile_Asm()*100, ile_smsm_correct()/ile_smsm()*100))
                   
rownames(x)= c("A-->X (poprawne)", "sm --> X", "sm-->A", "sm-->sm")  
x
