# VERİ GÖRSELLEŞTİRME Ödev Hafta-4

# Silindirin hacmini hesaplayan Fonksiyon
silindir_hacmi <- function(yaricap, yukseklik) {
  hacim = 3.14 * yaricap^2 * yukseklik
  print(hacim)
}
silindir_hacmi(10,5)


# Girilen sayının işaretini bulan fonksiyon
tam_sayi <- function(x) {
  sonuc = ifelse(x > 0, "Pozitif", 
                  ifelse(x < 0, "Negatif", 
                         "Sıfır veya Eşit"))
  return(sonuc)
}
print(tam_sayi(-3))

# Veri setini belirtilen URL'den yükleme
veri <- read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")

  # Veri setini belirtilen dosyadan yükleme
  #veri2 <- read.csv("C:/Users/akcam/Documents/Note_6.txt") posit cloud' da çalışıldığı için hata verir

getwd() # Çalışma dizinini gösterir 

  #setwd() posit cloud' da çalışıldığı için hata verir

View(veri) # Veri çerçevesini veya veri tabanını görsel bir tablo olarak görüntüler. 

head(veri,3) # Veri setinin ilk birkaç satırını gösterme

tail(veri) # Veri setinin son 6 satırını gösterme

dim(veri) # veri çerçevesinin satır ve sütun sayısını içeren bir vektör döndürür.

nrow(veri) # veri çerçevesinin satır sayısını içeren bir vektör döndürür.

ncol(veri) # veri çerçevesinin sütun sayısını içeren bir vektör döndürür.

colnames(veri) # Sütun adlarını kontrol etme

colnames(veri)[1:5] # ilk 5 Sütun adlarını kontrol etme

rownames(veri) # Satır adlarını kontrol etme

# VERİ SETİNDEKİ DEĞİŞKENLERİN ADINI DEĞİŞTİRMEK
install.packages("dplyr")# dplyr paketini yükle
library(dplyr)
data(mtcars)# mtcars veri setini yükle
veri_1 <- mtcars # mtcars veri setini kopyala
names(veri_1) # Kopyalanan veri setinin sütun isimlerini kontrol et
veri_2 <- veri_1 %>% rename(HP = hp) # dplyr paketini kullanarak değişken adını değiştir
names(veri_2) # Yeni veri setinin sütun isimlerini kontrol et

veri_2 =rename_all(veri_1, toupper) # Değişken adları büyük harf yapılıyor
names(veri_2)# Yeni veri setinin sütun isimlerini kontrol et

veri_2=rename(veri_2, qsec=QSEC) # değişken ismi küçük harf yapılıyor
names(veri_2)

install.packages("readr")
library(readr)

write.csv(veri_2, path = "C:/Users/akcam/Documents/veri22.csv", row.names = FALSE)# veri_2'yi CSV dosyasına yazdır
getwd()

install.packages("tidyverse")
library(tidyverse)
veri_3=data.frame(mtcars) # mtcars veri setini kullanarak bir veri çerçevesi (data frame) oluşturur ve veri_3 adında bir değişkene atar.
dim(veri_3)
View(veri_3)
veri_3_tibble=as_tibble(veri_3)# veri_3 adındaki veri çerçevesini tibble formatına dönüştürür ve veri_3_tibble adında yeni bir değişkene atar. 
head(veri_3_tibble) # ilk altı satırını ekrana yazdırır.
 
veri_3_yeni_ad=dplyr::rename(veri_3_tibble, WT=wt, CARB=carb) # ilgili değişkenler büyük harf yazılıyor
names(veri_3_yeni_ad)
 
veri_3_yeni_ad=dplyr::rename_all(veri_3_yeni_ad,toupper) # tüm değişkenler büyük harf yazılıyor
names(veri_3_yeni_ad)

veri_3_yeni_ad_vs=veri_3_yeni_ad$VS # veri setinin belirli bir sütununu(VS) seçmek için kullanılır.
print(veri_3_yeni_ad_vs)

vt<-mtcars
vt_disp=select(vt,disp)# disp kolonu seçiliyor
dim(vt_disp)
print(vt_disp) # seçimi yazdırır

vt2_disp2=pull(select(vt,disp))# disp sütunu seçilerek vektör oluşturuluyor
dim(vt2_disp2)
print(vt2_disp2)

vt3<-vt 
vt3_disp_filter=filter(vt3,vt_disp <= 100, disp >= 49) # disp değişkeni için filtreleme yapılıyor
vt3_hp=select(vt3_disp_filter,hp)# filtrelenen disp değerlerine karşılık gelen hp değerlerinin her ikiside seçiliyor
print(vt3_hp) # veri setinin son durumu ekrana yazdırılıyor

vt_piped=vt %>% filter (cyl==6 & hp<120) %>% select (mpg,qsec) # piped ile filtreleme yapılıyor
View(vt_piped)

vt4<-vt 
vt4$yeni_kolon=vt4$cyl*5 # yeni sütun eklendi
View(vt4)

vt4_mut = mutate(vt4,new_sutun=qsec*2) # yeni sütun eklendi
View(vt4_mut)

vt5_mut = mutate(vt, hp_kategori=ifelse(
                      hp<=100,"kategori 1",
                      ifelse(hp<=200,"kategori 2","kategori 3"
                    )))
View(vt5_mut)

vt5_mut_select = arrange(select(vt5_mut,hp,hp_kategori),hp) # hp değişkenine göre artan sıralama
View(vt5_mut_select)

vt5_mut_select_2 = arrange(select(vt5_mut,hp,hp_kategori),desc(hp)) # hp_kategori değişkenine göre azalan sıralama
View(vt5_mut_select_2)

vt6 = transmute(vt, newcolon=drat*2,cyl,hp) #transmute fonksiyonu ile veri setinde yeni kolon oluşturma ve seçim yapma
View(vt6)

ls() # tanımlanmış tüm nesnelerin isimlerini bir liste olarak getirir.




 