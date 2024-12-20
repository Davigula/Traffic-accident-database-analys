#Seminar-David Dujmoviæ

nassCDS <-read.csv2("nassCDS.csv",header=TRUE)
attach(nassCDS)
str(nassCDS)
occRole

#Ukupan broj zena i muskaraca
sum(sex=="m")
sum(sex=="f")
table(sex)
sum(sex=="m")+sum(sex=="f")

#Tipovi varijabli:
#1.Kvalitativna- nominalna-dead,airbag,seatbelt,frontal,sex,abcat,occRole,deploy,injSeverity
#              - ordinalna- dvcat,yearacc, yearVeh    
#2.Numericka- diskretna-
#           -neprekidna-weight,ageOFocc,caseidz



#Spol
table(sex)
#Prema podacima koji se nalaze u varijabli sex, 13969 je muskaraca,a 12248 zena

barplot(c(13969/26217,12248/26217),ylim =c(0,1),names.arg =c("Muškarci","žene"))#,col=c("white","black"))

#Da li je proporcija muskaraca statisticki znajcno veca od proporcije zena, na razini znacajnosti 0.05
#theta-proporcija muskaraca
#Hipoteze:
#H0: theta = 0.5
#H1: theta > 0.5

binom.test(13969,26217,p=0.5,alternative = "greater")

#p-value < 2.2e-16 < 0.05 pa na razini znaèajnosti 0.05 odbacujemo hipotezu H0 i prihvaæamo hipotezu H1
#Tj. mozemo tvrditi da je proporcija muskaraca statistièki veæa od proporcije zena



#Dob
#str(nassCDS)
#table(ageOFocc)
do_devet_g<-0
sum(ageOFocc==c(16:24))
ageOFocc==24
for(i in 16: 24){
  print(i)
}
do_dvadesetcetiri_g <-ageOFocc[16:24][1]
do_tridesetdevet_g <-8075
do_pedesetcetiri_g <-5352
preko_pedesetpet_g <-4520
tablica1 <-(c(0,8270,8075,5352,4520))
prop.table(tablica1)
barplot(c(do_devet_g/26217,do_dvadesetcetiri_g/26217,do_tridesetdevet_g/26217,do_pedesetcetiri_g/26217,preko_pedesetpet_g/26217),names.arg =c("1-9","10-24","25-34","35-54","55+"))
#Prema dostupnim podacima sa stranice "Median age of the U.S.population"ocekivana dob stanovnika koji je zivio u SAD-u je 35.3,2000. godine
#Provjerimo je li oèekivana dob osobe koja je sudjelovala u prometoj nesreci statistièki
#znaèajno manja od oèekivane dobi staovnika SAD-a
#Radi se o jako velikom uzorku,pa koristimo t-test
#Hipoteze:
#HO: mu = 35.3
#H1: mu < 35.3

t.test(ageOFocc,mu=35.3,alternative = "less")

#p-value=1 > 0.05 pa na razini znaèajnosti 0,05 ne odbacujemo hipotezu H0
#Mozemo tvrditi kako je oèekivana dob osobe koja je sudjelovala u prometnoj nesreci statistièki priblizno jednaka
#oèekivanoj dobi stanovnika SAD-a, podaci iz 2000.-te godine 



#Godina prometne nezgode
table(yearacc)         
#Najveci broj prometnih nezgoda se dogodio 2002. godine i to njih 4764
barplot(table(yearacc),ylim=c(0,6000), xlab = "Godina nezgode",ylab="Broj nezgoda")
#Na razini znacajnosti 0.05, zelim provjeriti postoji li preferencija koje godine ce se dogoditi prometna
#nezgoda ili je to sve plod slucajnosti
# Hipoteze:
# H0:   1     2    3    4    5    6 
# X ~ (                              )
#       1/6  1/6  1/6  1/6  1/6  1/6  
# H1: ne H0
frek1 <-c(3975,4427,4516,4420,4115,4764)
vjer1 <-c(1/6,1/6,1/6,1/6,1/6,1/6)

chisq.test(frek1,p=vjer1)

#p-value < 2.2e-16 < 0.05 pa na razini znaèajnosti 0.05 odbacujemo H0 i prihvaæamo H1, 
#tj. možemo tvrditi da postoji preferencija godine kad ce se dogoditi prometna nezgoda



#povezanost izmedu spola i starost
sum(ageOFocc[sex=="m"])
#Graficki prikaz:

boxplot(ageOFocc~sex, names=c("zene","muskarci"))

#primjecujemo da su muskarci mladi od zena
#mu1-ocekivana starost zena
#mu2-ocekivana starost muskaraca
#HO:mu1=mu2
#H1:mu1>mu2

t.test(ageOFocc[sex=="f"],ageOFocc[sex=="m"],alternative = "greater")

#p-vrijednost=2.079e-15 <0.05 pa na razini znacajnosti 0.05 odbacujemo HO i prihvacamo H1,
#tj. mozemo tvrditi da je ocekivana starost zena veca od ocekivane starosti muskaraca



#povezanost izmedu pojasa i starosti
sum(ageOFocc[seatbelt=="belted"])
ageOFocc
seatbelt
#Graficki prikaz:

boxplot(ageOFocc~seatbelt, names=c("vezani pojas","bez pojasa"))

#primjecujemo da za osobe koje su vezane sigurnosnim pojasem vrijedi da su starije u prosjeku od ostalih
#mu1-ocekivana starost osoba koje koriste pojas prilikom vožnje
#mu2-ocekivana starost osoba koje ne koriste pojas prilikom vožnje
#HO:mu1=mu2
#H1:mu1>mu2

t.test(ageOFocc[seatbelt=="belted"],ageOFocc[seatbelt=="none"],alternative = "greater")

#p-vrijednost < 2.2e-16 <0.05 pa na razini znacajnosti 0.05 odbacujemo HO i prihvacamo H1,
#tj. mozemo tvrditi da je ocekivana starost osoba koje koriste sigurnosti pojas za vezanje veæa od starosti 
#ostalih osoba koje sudjeluju u prometu



#povezanost izmedu spola i frontalnog sudara
table(frontal,sex)
prop.table(table(frontal,sex),2)
barplot(prop.table(table(frontal,sex),2),names.arg =c("zene","muskarci"),legend=c("sudar nije bio frontalni","frontalni sudar"))
#theta1-proporcija osoba koje su imale frontalni sudar meðu ženama
#theta2-proporcija osoba koje su imale frontalni sudar meðu muškarcima
#H0: theta1=theta2
#H1: theta1<theta2

prop.test(c(7493,9373),c(12248,13969),alternative="less")

#p-vrijednost < 2.2e-16 <0.05 pa na razini znacajnosti 0.05 mozemo tvrditi da je proporcija
#onih koji su imali frontalni sudar veca kod muskaraca nego kod zena



#povezanost izmedu spola i posljedica promjetne nesrece
tablica2 <-matrix(c(6632,19585,12248,13969),2,2,byrow = TRUE)
tablica2
table(injSeverity)
#Iz varijable injSeverity promatramo osobe koje nisu ozljedene kao prvi podskup i sve ostale osobe kao drugi podskup, te varijable 
prop.table(tablica2,2)
barplot(prop.table(tablica2,2),names.arg = c("žene","muškarci"),legend=c("nema ozljeda","ostale posljedice"))
#theta1-proporcija osoba koje nisu imale ozljeda meðu ženama
#theta2-proporcija osoba koje nisu imale ozljeda meðu muškarcima
#Ho: theta1=theta2
#H1: theta1<theta2

prop.test(c(2497,3982),c(12248,13969),alternative = "less")

#p-vrijednost  < 2.2e-16 <0.05 pa na razini znacajnosti 0.05 odbacujemo Ho i prihvacamo H1, tj. mozemo tvrditi da je proporcija onih osoba koje nisu imali ozljeda manja meðu ženama
#nego meðu muskarcima



#povezanost izmedu spola i prezivljelih/umlih osoba
table(dead,sex)
prop.table(table(dead,sex),2)
barplot(prop.table(table(dead,sex),2),names.arg =c("žene","muškarci"),legend=c("osoba je prezivjela","osoba je umrla"))
#theta1-proporcija preživjelih osoba meðu ženama
#theta2-proporcija preživjelih osoba meðu muškarcima
#Ho: theta1=theta2
#H1:theta1>theta2

prop.test(c(11784,13253),c(12248,13969),alternative = "greater")

#p-vrijednost=1.105e-07 <0.05 pa na razini znacajnosti 0.05 odbacujemo H0 i prihvacamo H1, mozemo
#tvrditi da je proporcija prezivjelih osoba veca meðu ženama nego meðu muškarcima



#povezanost izmedu prezivljelih/umlih i zracnih jastuka u automobilu
table(dead)
table(airbag,dead)
prop.table(table(airbag,dead),2)
barplot(prop.table(table(airbag,dead),2),names.arg = c("preživljeli","umrli"),legend=c("zraèni jastuk se aktivirao","bez znaènog jastuka"))
#theta1-proporcija osoba kod kojih se aktivirao zraèni jastuk meðu preživjelima
#theta2-proporcija osoba kod kojih se aktivirao zraèni jastuk meðu umrlima
#HO: theta1=theta2
#H1: theta1>theta2

prop.test(c(13908,511),c(25037,1180),alternative = "greater")

#p-vrijednost < 2.2e-16 <0.05 pa na razini znacajnosti 0.05 odbacujemo HO i prihvacam H1,
#mozemo tvrditi da je proporcija osoba kod kojih se aktivirao zraèni jastuk veæa meðu preživjelima nego meðu umrlima



