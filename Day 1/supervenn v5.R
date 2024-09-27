library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtools)

mylabel <- "a= Living Alone, b= Single, c= Feeling Lonely"

supervenn_random <- function(cases, vennlabel){
  

    #cases is how many random supervenns we create next to one another
    # perm is number of permutations... between 3 and 5 is possible. A higher number is more accurate (see the accuracy underneath the produced charts) but takes longer to compute
  perm <- 3
  label_scale <- max(1, (2/cases)) #this attempts to scale the size of the rectangle labels
  
  dt <- matrix(data=0,nrow=8, ncol=(cases))  
  dt <- data.table(dt)
  
#This loop Generates fake data for all cases
#to use your own data, just put it in the same format as the dt generated below
#Fake data Generation (disable if you want to use your own data instead)####
  for (i in 1:cases){
      cutoff<-sample(1:99, 6)
  
  cutoff<-sort(cutoff/100)
  
  a<- cutoff[1]
  b<- cutoff[2]-cutoff[1]
  c<- cutoff[3]-cutoff[2]
  ab<-cutoff[4]-cutoff[3]
ac<-cutoff[5]-cutoff[4]
  bc<-cutoff[6]-cutoff[5]
 abc<-1-cutoff[6]
  overall <- sample(3:40, 1)/100
  
  supervenn <- c(a, b, c, ab, ac, bc, abc, overall)
  #overall is a parameter representing the proportion of all dt[[7,i]] combination from the total population
  # we then can scale the overall squares for each country in relation to one another
  
  n <- c("a", "b", "c", "ab", "ac", "bc", "abc", "overall")
  dt$names <- n
  
  dt[,i] <- supervenn
  }  #end fake data generation
#FAKE DATA GENERATION  OVER
# if you want to use your own data, disable the above section and call on your own data in the same format instead 
  

myplots <- list()


gg_data <- data.table( t(dt[,1:cases]))
colnames(gg_data) <- dt$names
#now sort it 
gg_data <- gg_data[order(gg_data$overall, decreasing=FALSE),]

#now covert all the data directly so that 1 is the smallest and others are scaled up

#Rescaling####
#This rescales all the values for all cases (in this example, a case is a country) so that we can also compare across cases
   for (k in 1:cases){
  
  scale_factor <- (gg_data[k,8])
  gg_data[k, 1:7] <-  (gg_data[k, 1:7])*c(scale_factor)
  
}



#OPTIMIZATION procedure for individual case Venns####
testset <- permutations(perm, 4, c(.5, .35, .65, .20, .80), repeats.allowed=TRUE)

for (o in 1:cases)  {
 
  a <- gg_data[o,a]  #note that A, B and C here are exclusive of the other categories. A means A only, and not b or c. 
  b<- gg_data[o,b]
  c<- gg_data[o,c]
  ac<- gg_data[o,ac]
  ab<- gg_data[o,ab]
  bc<- gg_data[o,bc]
  abc<- gg_data[o,abc]
  overall<- gg_data[o,overall]
  
#this generates and test permutations and compares them against the true areas 
  for (p in 1:nrow(testset)) {
    
  wr_A <- testset[p, 1] #width-height ratio for A rectangle
    wr_B <- testset[p, 2]#width-height ratio for A rectangle
    wr_C <- testset[p, 3]#width-height ratio for A rectangle
    wr_ABC <- testset[p, 4]#width-height ratio for A rectangle. Start will all permutations of A, B, C, and ABC
 
    
    #later the fitness of success is measured by whether AC, BC, and AB match the optimal after calculating A-all, B-all, C-all, and ABC according to the ratios from the permutations
  
#    start with ABC dimensions
    abc_width <- sqrt(abc)*   wr_ABC
  abc_height <- abc/abc_width
  abc_height*abc_width==abc  #check calculations to be correct
  abc_xmin <- 0-(abc_width/2) #abc mid coordinates
    abc_xmax <- 0+(abc_width/2) 
  abc_ymin <- 0- (abc_height/2) 
  abc_ymax <- 0+ (abc_height/2) 
    
  #now C_all 
  c_width <- max((sqrt(c+ac+bc+abc)*   wr_C) ,   abc_width)
  c_height <- (c+ac+bc+abc)/c_width
  
  if (c_height <abc_height){
    c_height <- abc_height
    c_width <- (c+ac+bc+abc)/c_height
  }
  
  c_height*c_width==(c+ac+bc+abc) 
  c_xmin <- 0-(c_width/2) #abc mid coordinates
  c_xmax <- 0+(c_width/2) 
  c_ymax <-  abc_ymax
  c_ymin <-  c_ymax-  c_height
   
  #now A_all this is All A categories in combination (a, ab, ac, abc)
  a_width <- max((sqrt(a+ac+ab+abc)*   wr_A), abc_width)
  a_height <- (a+ac+ab+abc)/a_width
  
  if (a_height<abc_height){
    a_height<- abc_height
    a_width <- (a+ac+ab+abc)/a_height
  }
  
  a_height*a_width==(a+ac+ab+abc) 
  a_xmin <- abc_xmax -a_width #abc mid coordinates
  a_xmax <- abc_xmax 
  a_ymax <-  abc_ymin+a_height
  a_ymin <-  abc_ymin
  

  #now B_all 
  b_width <- max((sqrt(b+bc+ab+abc)*   wr_B), abc_width)
  b_height <- (b+bc+ab+abc)/b_width
  
  if (b_height<abc_height){
    b_height<- abc_height
    b_width <- (b+bc+ab+abc)/b_height
    
    
  }
  
      b_height*b_width==(b+bc+ab+abc) 
  b_xmin <- abc_xmin
  b_xmax <-  b_xmin +b_width
  b_ymin <-  abc_ymin
  b_ymax <-    b_ymin+ b_height
 
  
  ## Now factcheck AB, AC, and BC.. 
  bc_depicted <- abs(abc_xmax-min(unlist(c_xmax, b_xmax))) *abc_height
  ab_depicted <- abc_width*abs(abc_ymax-   min(unlist(b_ymax, a_ymax)))
    ##add check  IF a_xmin<c_xmin
    
  ac_depicted <- abs(max(a_xmin, c_xmin)-abc_xmin)*abc_height
  
  #now calculate residual value. we want this at zero####
  residualvenn <- abs(ac_depicted-ac)+abs(ab_depicted-ab)+abs(bc_depicted-bc)
  
  testset <- data.table(testset)

    testset$residuals[p] <- as.numeric(residualvenn)
  
  }  #END this FOR loop finds the optimal configuration for one particular case
  #It ends up that one run through creates accuracy over 90% every time. This is good enough for the naked eye. If you want to maximize this, increase 'perm' to 5. Or modify the permutation "testset" even so that you are testing a wider range of possibilities. The cost of this though is exponentially higher running time. 
  
  promising <- which(testset$residuals== min(unlist(testset$residuals))) #The most promising configuration is saved
  useme <- testset[promising][1] #even if multiple solutions have the highest accuracy, the first one is nonetheless chosen
  

  #Now these values are calculated again for the 'winning' accurate configuration
  #save values for that particular case configuration
  wr_A <- useme[1, 1] #width-height ratio for A rectangle
  wr_B <- useme[1, 2]#width-height ratio for A rectangle
  wr_C <- useme[1, 3]#width-height ratio for A rectangle
  wr_ABC <- useme[1, 4]#width-height ratio for A rectangle. Start will all permutations of A, B, C, and ABC
  
  #    start with ABC dimensions
  abc_width <- as.numeric(sqrt(abc)*   wr_ABC)
  abc_height <- as.numeric(abc/abc_width)
  round((abc_height*abc_width), digits=6)== round(abc, digits=6)  #ERROR... this returns false somehow
  abc_xmin <- 0-(abc_width/2) #abc mid coordinates
  abc_xmax <- 0+(abc_width/2) 
  abc_ymin <- 0- (abc_height/2) 
  abc_ymax <- 0+ (abc_height/2) 
  
  #now C_all 
  c_width <- max((sqrt(c+ac+bc+abc)*   wr_C) ,   abc_width)
  c_height <- (c+ac+bc+abc)/c_width
  
  if (c_height <abc_height){
    c_height <- abc_height
    c_width <- (c+ac+bc+abc)/c_height
  }
  
  c_height*c_width==(c+ac+bc+abc) 
  c_xmin <- 0-(c_width/2) #abc mid coordinates
  c_xmax <- 0+(c_width/2) 
  c_ymax <-  abc_ymax
  c_ymin <-  c_ymax-  c_height
  
  #now A_all 
  a_width <- max((sqrt(a+ac+ab+abc)*   wr_A), abc_width)
  a_height <- (a+ac+ab+abc)/a_width
  
  if (a_height<abc_height){
    a_height<- abc_height
    a_width <- (a+ac+ab+abc)/a_height
  }
  
  a_height*a_width==(a+ac+ab+abc) 
  a_xmin <- abc_xmax -a_width #abc mid coordinates
  a_xmax <- abc_xmax 
  a_ymax <-  abc_ymin+a_height
  a_ymin <-  abc_ymin
  
  
  #now B_all 
  b_width <- max((sqrt(b+bc+ab+abc)*   wr_B), abc_width)
  b_height <- (b+bc+ab+abc)/b_width
  
  if (b_height<abc_height){
    b_height<- abc_height
    b_width <- (b+bc+ab+abc)/b_height
  }

  b_height*b_width==(b+bc+ab+abc) 
  b_xmin <- abc_xmin
  b_xmax <-  b_xmin +b_width
  b_ymin <-  abc_ymin
  b_ymax <-    b_ymin+ b_height
  
  
  #Those configurations are now used to update the case data for this particular case
  gg_data$abc_xmin[o] <- as.numeric(abc_xmin)
    gg_data$abc_xmax[o] <-  as.numeric(abc_xmax)
  gg_data$abc_ymin[o] <- as.numeric( abc_ymin)
  gg_data$abc_ymax[o] <-  as.numeric(abc_ymax)
  gg_data$abc_width[o] <- as.numeric(abc_width)
  gg_data$abc_height[o] <- as.numeric(abc_height)
  
    gg_data$a_xmin[o] <-  as.numeric(a_xmin)
    gg_data$a_xmax[o] <-  as.numeric(a_xmax)
    gg_data$a_ymin[o] <-  as.numeric(a_ymin)
    gg_data$a_ymax[o] <-  as.numeric(a_ymax)
    gg_data$a_width[o] <- as.numeric(a_width)
    gg_data$a_height[o] <- as.numeric(a_height)
    
    gg_data$b_xmin[o] <-  as.numeric(b_xmin)
    gg_data$b_xmax[o] <-  as.numeric(b_xmax)
    gg_data$b_ymin[o] <-  as.numeric(b_ymin)
    gg_data$b_ymax[o] <-  as.numeric(b_ymax)
    gg_data$b_width[o] <- as.numeric(b_width)
    gg_data$b_height[o] <- as.numeric(b_height)
  
    gg_data$c_xmin[o] <-  as.numeric(c_xmin)
    gg_data$c_xmax[o] <- as.numeric( c_xmax)
    gg_data$c_ymin[o] <-  as.numeric(c_ymin)
    gg_data$c_ymax[o] <-  as.numeric(c_ymax)
    gg_data$c_width[o] <- as.numeric(c_width)
    gg_data$c_height[o] <- as.numeric(c_height)
    
    gg_data$residuals[o] <- as.numeric(useme[1, 5])

}  


print(gg_data)  

gg_data <- gg_data[order(gg_data$overall, decreasing=FALSE),]

scale_ymax <- max(gg_data[,c(18, 24, 30)])
scale_xmax<- max(gg_data[,c(16, 22, 28)])
scale_xmin<- min(gg_data[,c(15, 21, 27)])
scale_ymin<- min(gg_data[,c(17, 23, 29)])

#GRAPHING####
# once gg_data is ready with cases and their optimized coordinates
for (gg in 1:cases) { 
  

  
  
  ### Graphing
   tempdt <- gg_data[gg,]
   
   
   

  g <- ggplot(data=tempdt)   + xlim(scale_xmin,scale_xmax)+ylim(scale_ymin,scale_ymax)
  

  g1 <- 
    g +
    
    geom_rect(data=tempdt, aes(xmin = abc_xmin, xmax = abc_xmax,   ymin = abc_ymin, ymax=abc_ymax), alpha=.25,  fill = "brown", color="blue") + #This is ABC right now. use it to doublecheck
    geom_rect(data=tempdt, aes(xmin = c_xmin, xmax =c_xmax,   ymin = c_ymin, ymax=c_ymax), alpha=.25,  fill = "magenta", color="red") + #This is C
    geom_rect(data=tempdt, aes(xmin = b_xmin, xmax=b_xmax,   ymin = b_ymin, ymax=b_ymax), alpha=.25,  fill = "yellow", color="brown") + #this is B
    geom_rect(data=tempdt, aes(xmin = a_xmin, xmax=a_xmax,   ymin = a_ymin, ymax = a_ymax), alpha=.2,  fill = "cyan", color="blue") + #this is A all (a+ab+ac+abc)
   theme_minimal()+
   annotate("text", x=0, y=gg_data[gg, c_ymin]+.1*gg_data[gg, c_height], label=colnames(gg_data)[3], size=(5*label_scale)) +  
   annotate("text", x=0 , y=0, label=colnames(tempdt)[7], size=(4*label_scale)) +  
   annotate("text", x=as.numeric(gg_data[gg, a_xmin]+gg_data[gg,a_width]*.1) , y=as.numeric(gg_data[gg,a_ymax]-gg_data[gg, a_height]*.1), label=colnames(gg_data)[1], size=(5*label_scale)) +  
    annotate("text", x=as.numeric(gg_data[gg, b_xmax]-gg_data[gg,b_width]*.1), y=as.numeric(gg_data[gg,b_ymax]-gg_data[gg, b_height]*.1), label=colnames(tempdt)[2], size=(5*label_scale)) +
   labs(y="", x = paste0("Country ",gg, "\n ", signif(1-tempdt$residuals, digits=2)*100, "% accuracy")) 

  
  
  myplots[[gg]] <- g1
  print(tempdt)
  
}

grid.arrange(grobs=myplots, ncol=round(sqrt(cases)), bottom=vennlabel, top=textGrob("Country Comparisons Proportional Rectangular Venn",gp=gpar(fontsize=20,font=3)))  #of course, change labels as you see fit
print(gg_data)

}


#Now run it!
supervenn_random(9, mylabel)

