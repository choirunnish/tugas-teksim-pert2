##TUGAS 1 TEKNIK SIMULASI PERTEMUAN 2##
##CHOIRUNNISA HASNA - B2A020071##
##NIM GANJIL##

#Diket : Z0= 21139, a=45, m=417, c=437, n=150, p=0.83
#Dit: Gunakan Additive dan Gunakan Bernouli_2
#Jawab:

###Additive###

Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for (i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(45,21139,437,417,150)


##Menampilkan Z1, Z2, ..., Zn##

Additive_RNG<-function(a,z0,c,m,n)
{
  z<-rep(0,150)
  for(i in 1:150)
  {
    z[i]<-((a*z0)+c)%%m
    z0<-z[i]
  }
  print(z)
}
Additive_RNG(45,21139,437,417,150)


##Bernoulli 2##

i<-150
p<-.83
X<-runif(i)
Y<-(X<=p)+0
(tabel<-table(Y)/length(Y))
barplot(tabel,main="Bernoulli", xlab = 'X', ylab = 'Probability')