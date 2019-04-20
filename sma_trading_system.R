sma_trade_system<-function(z)
{
  newdate<-as.numeric()
  newopen<-as.numeric()
  newclose<-as.numeric()
  newhigh<-as.numeric()
  newlow<-as.numeric()
  
  print("This program can execute the following functions:")
  print("1 - The user insert date, open, close, high and low")
  print("2 - The system export the new data base to excel")
  print("3 - The system calculate a trade signal and show on the console")
  
  print("Insert a new day data? YES --> 1 NO --> 0")
  j<-scan(n=1)
  
  while (j == 1)
  {
    #charging librarys
    library(data.table)
    library(quantmod)
    library(TTR)
    
    #import que database
    project<-fread("crypto_trading_project4.csv",header = TRUE,sep=";")
    project<-data.frame(project)
    names(project)
    
    #scaning the new data
    print("Insert the newdate with no divisions by . or / exemple 01012019:")
    newdate<-scan(n = 1)
    print("Insert the newopen:")
    newopen<-scan(n=1)
    print("Insert the newclose:")
    newclose<-scan(n=1)
    print("Insert the newhigh:")
    newhigh<-scan(n=1)
    print("Insert the newlow:")
    newlow<-scan(n=1)

    #creating a vector with the new data
    newday<-c(newdate,newopen,newclose,newhigh,newlow)
    
    #adding the new data to database that was imported
    project<-rbind(project,newday)
    
    #exporting the new data to be used tomorrow
    write.table(project, file='crypto_trading_project4.csv', sep=';', dec='.', row.names=FALSE)
    
    print("New data are exported to excel, continue to generate trade signal? YES --> 1 NO --> 0")
    j<-scan(n=1)
    
    #creating indicators
    project$sma15<-SMA(project$close,n=15)
    
    #separating the database that will be really used
    project2<-project[15:length(project$open),]
    
    #creating the traderule
    for (y in 1:length(project2$open)) {
      if(project2$close[y] > project2$sma15[y]){
        project2$long[y]<-1
      }else{
        project2$long[y]<--1
      }
      if(project2$close[y] < project2$sma15[y]){
        project2$short[y]<-1
      }else{
        project2$short[y]<--1
      }
      if(project2$long[y]==1 && project2$short[y]== -1){
        project2$trade[y]<-1
      }
      if(project2$short[y]==1 && project2$long[y]== -1){
        project2$trade[y]<--1
      }
      
      y<-y+1
      
    }
    
    #generate the signal
    lastline<-tail(project2$trade,n= 1L)
    
    if(lastline == 1)
    {
      print("BUY!")
    }
    if(lastline == -1)
    {
      print("SELL!")
    }
    
    print("End of this program, add a newday? YES --> 1 NO --> 0")
    j<-scan(n=1)
    
  }
  #end of while
  print("END!")
  print("Thank You!")
  
}
    
