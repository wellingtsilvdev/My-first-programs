registro = function(diretorio,nome_arquivo){
  aluno = as.character()
  prova1 = as.numeric()
  prova2 = as.numeric()
  prova3 = as.numeric()
  prova4 = as.numeric()
  media = as.numeric()
  situacao = as.character()
  
  alunos = data.frame(aluno,prova1,prova2,prova3,prova4,media,situacao)
  alunos$aluno = as.character(alunos$aluno)
  alunos$situacao = as.character(alunos$situacao)
  
  setwd(diretorio)
  if(file.exists(nome_arquivo)){
    alunos = read.csv(nome_arquivo,header = T,sep = ";")
    alunos$aluno = as.character(alunos$aluno)
    alunos$situacao = as.character(alunos$situacao)
    print("Arquivo com registros anteriores encontrado e importado")
    
    i = length(row.names(alunos))+1
    z = 1
  }else{
    print("Ainda não existe nenhum arquivo com registros nesse diretório!")
    i = 1
    z = 0
  }
  x = 1
  while (x == 1) {
    if(i==1){
      print("Iniciar cadastro? SIM --> 1 NÃO --> 0")
      x = scan(n=1)
    }else if(z == 1){
      print("Iniciar o cadastro de novos alunos? --> 1 NÃO --> 0")
      x = scan(n=1)
    }
    
    print("Insira o nome do aluno:")
    aluno = scan(what = character(),nmax=1)
    
    print("Insira a nota da prova1:")
    prova1 = scan(n=1)
    
    print("Insira a nota da prova2:")
    prova2 = scan(n=1)
    
    print("Insira a nota da prova3:")
    prova3 = scan(n=1)
    
    print("Insira a nota da prova4:")
    prova4 = scan(n=1)
    
    media = (prova1 + prova2 + prova3 + prova4)/4
    
    if(media >= 7){
      situacao = "APROVADO!"
      print("Aluno aprovado!")
    }else{
      situacao = "REPROVADO!"
      print("Aluno reprovado!")
    }
    
    print("Inserir novo aluno? SIM --> 1 NÃO --> 0")
    x = scan(n=1)
    
    alunos[i,] = c(aluno,prova1,prova2,prova3,prova4,media,situacao)
    
    i = i+1
    z = z+2
  }
  write.table(alunos,nome_arquivo,sep = ";",row.names = F)
  print("Os dados foram salvos corretamente, Obrigado!")
  return(alunos)
}
