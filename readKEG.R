# This script converts a .keg file to a well organized .csv file. 

# It is designed for researchers who do not have any bioinformatics background.

# This script is edited by Dr. Bin Zhu at Virginia Commonwealth University.

# Please make sure that you have only one .keg file put together with 
# the readKEG.R in the same folder.

# Any bug report or suggestion could be sent to Dr. Bin Zhu via Email: binzhu0824@gmail.com


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

FILE <- list.files(pattern = '.keg')

data <- read.delim(FILE)

len <- dim(data)[1]
n=1

max = "A"
for (a in 1:len) {
  c = data[a,1]
  c = toString(c)
  if (c==""){
    next
  }
  string <- strsplit(c, " ")[[1]]     
  start <- strsplit(string, NULL)[[1]]    
  if (start[1]>max) {
    max = start[1]
  }
}


switch(max,
      "C"={
        output <- t(matrix(c('level1','level2','info','info2')))
        for (a in 1:len) {
          c = data[a,1]
          c = toString(c)
          if (c==""){
            next
          }
          string <- strsplit(c, " ")[[1]]     
          start <- strsplit(string, NULL)[[1]]    
        
          if (toString(start)==""){
            next
          }
          
          switch(start[1],
                 "A" = {level1 = c},
                 "B" = {level2 = c},
                 "C" = {
                   level3 = c
                   level4 = toString(data[a,2])
                 
                 output <- rbind(output, c(level1,level2,level3,level4))  
                 n=n+1})
        }

        output <- data.frame(output)
        colnames(output) <- as.character(unlist(output[1,]))   
        output <- output[-1,]    
        rownames(output) <- NULL
        write.csv(output,'output.csv')
      },
      
      
      "D"={
        output <- t(matrix(c('level1','level2','level3','info1','info2'))) ###
        for (a in 1:len) {
          c = data[a,1]
          c = toString(c)
          c = as.character(c)
          if (c==""){
            next
          } else if (object.size(c) < 2000) {
            string <- strsplit(c, " ")[[1]]
            start <- strsplit(string, NULL)[[1]]  
            
            if (toString(start)==""){
              next
            }
            
            switch(start[1],
                   "A" = {level1 = c},
                   "B" = {level2 = c},
                   "C" = {level3 = c},
                   "D" = {
                     level4 = c
                     level5 = toString(data[a,2])
                     
                     output <- rbind(output, c(level1,level2,level3,level4,level5))   ###

                     n=n+1})
          } else {
            d = strsplit(c, "\n")
            d = d[[1]]
            
            for (x in 1: length(d)) {
              
              string <- strsplit(d[x], " ")[[1]]
              start <- strsplit(string, NULL)[[1]]  
              
              if (toString(start)==""){
                next
              }
              
              switch(start[1],
                     "A" = {level1 = d},
                     "B" = {level2 = d},
                     "C" = {level3 = d},
                     "D" = {
                       level4 = d
                       level5 = toString(data[a,2])
                       
                       output <- rbind(output, c(level1,level2,level3,level4,level5)) 
                       n=n+1})
            }
          }
            
          
          
        }
        
        output <- data.frame(output)

        colnames(output) <- as.character(unlist(output[1,]))   
        
        output <- output[-1,] 
        rownames(output) <- NULL
        
        output$x = paste0(output$level1,output$level2,output$level3,output$info1)
        output = output[!duplicated(output$x),]
        output$x = NULL
        write.csv(output,'output.csv')
      },
      "E" = {
        output <- t(matrix(c('level1','level2','level3','level4','info','info2')))
        for (a in 1:len) {
          c = data[a,1]
          c = toString(c)
          if (c==""){
            next
          }
          string <- strsplit(c, " ")[[1]]     
          start <- strsplit(string, NULL)[[1]]
          
          if (toString(start)==""){
            next
          }
          
          switch(start[1],
                 "A" = {level1 = c},
                 "B" = {level2 = c},
                 "C" = {level3 = c},
                 "D" = {level4 = c},
                 "E" = {
                   level5 = c
                   level6 = toString(data[a,2])
                 
                 output <- rbind(output, c(level1,level2,level3,level4,level5,level6))  
                 n=n+1})
          
        }
        
        output <- data.frame(output)
        
        colnames(output) <- as.character(unlist(output[1,]))  
        
        output <- output[-1,]    
        rownames(output) <- NULL
        
        
        write.csv(output,'output.csv')
        
      }
  
  
)


