registration = function(directory,file_name){
  student = as.character()
  test1 = as.numeric()
  test2 = as.numeric()
  test3 = as.numeric()
  test4 = as.numeric()
  meann = as.numeric()
  situation = as.character()
  
  students = data.frame(student,test1,test2,test3,test4,meann,situation)
  students$student = as.character(students$student)
  students$situation = as.character(students$situation)
  
  setwd(directory)
  if(file.exists(file_name)){
    students = read.csv(file_name,header = T,sep = ";")
    students$student = as.character(students$student)
    students$situation = as.character(students$situation)
    print("File with previous records found and imported!")
    
    i = length(row.names(students))+1
    z = 1
  }else{
    print("There are no files with records in this directory yet!")
    i = 1
    z = 0
  }
  x = 1
  while (x == 1) {
    if(i==1){
      print("Start registration? YES -> 1 NO -> 0")
      x = scan(n=1)
    }else if(z == 1){
      print("Start the registration of new students? -> 1 NO -> 0")
      x = scan(n=1)
    }
    
    print("Enter student name:")
    student = scan(what = character(),nmax=1)
    
    print("Enter the test note 1:")
    test1 = scan(n=1)
    
    print("Enter the test note 2:")
    test2 = scan(n=1)
    
    print("Enter the test note 3:")
    test3 = scan(n=1)
    
    print("Enter the test note 4:")
    test4 = scan(n=1)
    
    meann = (test1 + test2 + test3 + test4)/4
    
    if(meann >= 7){
      situation = "APROVED!"
      print("Student aproved!")
    }else{
      situation = "REPROVED!"
      print("Student reproved!")
    }
    
    print("Insert new student? YES -> 1 NO -> 0")
    x = scan(n=1)
    
    students[i,] = c(student,test1,test2,test3,test4,meann,situation)
    
    i = i+1
    z = z+2
  }
  write.table(students,file_name,sep = ";",row.names = F)
  print("The data has been saved correctly, Thank you!")
  return(students)
}
