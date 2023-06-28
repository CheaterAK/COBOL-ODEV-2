       IDENTIFICATION DIVISION.
      *IDENTIFICATION DIVISION.
      *programi kimliklendirme bolumu
       PROGRAM-ID.    ODEV002
      *program id'si
       AUTHOR.        AHMET KOCABAS aka CheaterAK
      *programi yazan kisi
       ENVIRONMENT DIVISION.
      *ENVIRONMENT DIVISION.
      *programin cevre degiskenlerini tanimladigimiz bolum
      *kaynak bilgisayar, hedef bilgisayar, dosya tanimlamalari vb.

       INPUT-OUTPUT SECTION.
      *INPUT-OUTPUT SECTION.
      *programin giris cikis tanimlamalarini yaptigimiz bolum
       FILE-CONTROL.
      *FILE-CONTROL.
      *dosya tanimlamalarinin yapildigi ve niteleyicilerin tanimlandigi
      *bolum
           SELECT OUT-FILE ASSIGN TO PRTLINE
                       FILE STATUS IS STATUS-OUTFILE.
      *SELECT OUT-FILE ASSIGN TO PRTLINE FILE STATUS IS STATUS-OUTFILE.
      *select ile dosya tanimlamasi yapilir. dosya tanimlamasi yapilirken
      *programda kullanilacak dosyanin adi(en az 1 harf olmali ve 
      *program icinde benzersiz olmali), dosyanyi niteleyen dataset adi ve 
      *dosyanin durumunu tutacak degiskenin adi verilir.
           SELECT USERS-DATA   ASSIGN TO USERDATA
                       FILE STATUS IS STATUS-USERDATA.
       DATA DIVISION.
      *DATA DIVISION.
      *programin veri tanimlamalarinin yapildigi bolum
       FILE SECTION.
      *FILE SECTION.
      *dosya tanimlamalarinin yapildigi bolum
       FD  OUT-FILE RECORDING MODE F.
      *FD  OUT-FILE RECORDING MODE F.
      *(FD) yada SD dosya gostericisidir, recordind mode F, V, U veya S 
      *olabilir.
      *SD dosya gostericisi dosyalari siralamak yada birlestirmek icin
      *kullanilir.
      *F Kayit modu dosyanin sabit bir uzunlugu oldugu anlamina gelir.
      *FIXED
      *V kayit modu dosyanin uzunlugunun degisken oldugunu gosterir.
      *VARIABLE
      *U kayit modu dosyanin uzunlugunun bilinmedigini gosterir.
      *UNDEFINED(FIXED or VARIABLE)
      *S kayit modu UNDIFINED gibidir fakat data 1 blocktan buyuk olabilir. 
      *SPANNED
      *---------------------------------------------------------------
      
      
      
      *---------------------------------------------------------------
      *bknz:Entity and attribute relationship diagram (ERD).
      *01-49 66 77 88 level number olarak gecer. 01 level number en ust seviyedir.
      *01 level number'dan sonra gelen level numberlar 01 level number'in
      *alt seviyeleridir.
      *01 record description entry olarak gecer.
      *02-49 level numberlar 01 level number'in alt seviyeleridir.
      *02-49 level numberlar gruplandirilmis verileri tanimlamak icin
      * veya basit verileri tanimlamak icin kullanilir.
      *66 level number renames clause itemleri tanimlamak icin kullanilir.
      *ornegin 01 DATA. 05 DATA-PART1 PIC 9(4). 05 DATA-PART2 PIC X(1).
      *veri yapimiz olsun. altina 66 level number ile asagidaki gibi tanimlama
      *yapabiliriz.
      *66 YENI-ISIM RENAMES DATA(1:5).
      *bu sekilde DATA'in 1. karakterinden baslayarak 5 karakterlik
      *bir alani YENI-ISIM olarak tanimlamis oluruz.
      *veya 66 YENI-ISIM RENAMES DATA-PART1 THROUGH DATA-PART2.
      *bu sekilde DATA-PART1'dan DATA-PART2'ya kadar olan alani YENI-ISIM olarak
      *tanimlamis oluruz.
      *66 level number icin limitasyonlar vardir. ornegin 66 level number
      *ile tanimlanan bir alan 01 level number, 66 level number, 77 level
      *number veya 88 level number ile tanimlanan bir alanin adi olamaz.
      *77 level number diger level numberlar gibi veri tanimlamak icin
      *kullanilir. 77 level numberlar 01 level number'in alt seviyeleri
      *olamazlar ve kendilerinin alt seviyeleri olamaz.
      *88 level numberlar condition belirlemek icin kullanilir.
      *01-49 ve 77 level numberdan sonra verinin adi veya FILLER gelir. 
      * daha sonra verinin tipi(PIC), deger(VALUE) gibi bilgiler gelir.
       01  OUT-REC.
           05    ORDER-O                 PIC 9(4).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    FIRST-NAME-O            PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LAST-NAME-O             PIC X(15).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    BIRTH-DATE-O            PIC 9(8).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    CURRENT-DATE-O          PIC 9(8).
           05    FILLER                  PIC X(1) VALUE SPACE.
           05    LIVED-DAYS-O            PIC 9(5).
      *OUT-FILE dosyamizin kayit yapisi.
      *Cok aciklama oldugu icin FD yukarida kaldi...
       FD  USERS-DATA RECORDING MODE F.
       01  USER-FIELDS.
           05    ORDER-I                 PIC 9(4).
           05    FIRST-NAME-I            PIC X(15).
           05    LAST-NAME-I             PIC X(15).
           05    BIRTH-DATE-I            PIC X(8).
           05    CURRENT-DATE-I          PIC X(8).
      *USERS-FILE dosyamizin kayit yapisi
       WORKING-STORAGE SECTION.
      *WORKING-STORAGE SECTION.
      *programin calisma sirasinda kullanacagi degiskenlerin tanimlandigi bolum.
      *------------------------
       01  HEADER.
           05 FILLER                     PIC X(4)  VALUE "ORD.".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE "FIRST NAME".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(15) VALUE "LAST NAME".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "BIRTHDAY".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "TODAY".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(5)  VALUE "LIVED".
      *cikti dosyamiza yazilacak baslik icin tanimladigimiz alan.
      *------------------------
       01  ERR.
           05 FILLER                     PIC X(4)  VALUE "----".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(31)
                             VALUE "ERROR. INVALID INPUT CHK SYSOUT".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "--------".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(8)  VALUE "--------".
           05 FILLER                     PIC X(1)  VALUE SPACE.
           05 FILLER                     PIC X(5)  VALUE "-----".
      *hata mesaji icin tanimladigimiz alan.
      *------------------------
       01  WS-PARSED-YEARS.
           05  WS-CURRENT-DATE           PIC 9(8).
           05  WS-E-CURRENT-DATE.
               07 WS-CURRENT-YEAR        PIC 9(4).
               07 WS-CURRENT-MTH         PIC 9(2).
               07 WS-CURRENT-DAY         PIC 9(2).
           05  WS-BIRTH-DATE             PIC 9(8).
           05  WS-EBIRTH-DATE.
               07 WS-BIRTH-YEAR          PIC 9(4).
               07 WS-CURRENT-MTH         PIC 9(2).
               07 WS-CURRENT-DAY         PIC 9(2).
           05 WS-PARAMS.
               06 WS-PARAM-DATE          PIC X(8).
               06 WS-E-PARAM-DATE.
                   07 WS-PARAM-YEAR      PIC 9(4).
                   07 WS-PARAM-MTH       PIC 9(2).
                       88 THURTY-ONE              VALUE 1 3 5 7 8 10 12.
                       88 FEB                     VALUE 2.
                       88 THURTY                  VALUE 4 6 9 11.
                   07 WS-PARAM-DAY       PIC 9(2).
      *Tarih parametrelerini parse edip saklayacagimiz alan.
      *------------------------
       01  WS-DAYS-LIVED                 PIC 9(5).
      *yasamis gun sayisini tutacagimiz alan.
      *------------------------
       01  WS-LEAP-YEAR.
           05  WS-RULE1                  PIC 9(1).
           05  WS-RULE2                  PIC 9(2).
           05  WS-RULE3                  PIC 9(3).
       01  WS-TMP                        PIC 9(8).
      *artik yil hesaplamasi icin tanimladigimiz alan.
      *------------------------
       01  WS-FLAGS.
           05 STATUS-OUTFILE             PIC 99.
              88 OUTFILE-SUCC            VALUE 00 97.
           05 STATUS-USERDATA            PIC 99.
              88 USERDATA-SUCC           VALUE 00 97.
              88 ST-EOF                  VALUE 10.
           05 WS-VALID-ORDER             PIC 9(1) VALUE 1.
      *programin calisma sirasinda kullanacagi flaglerin tanimlandigi alan.
      *------------------------
       01  READ-CNT                      PIC 9(11) VALUE 0.
      *dosyadan okunan kayit sayisini tutacagimiz alan.
      *------------------------
       PROCEDURE DIVISION.
      *programin calisma sirasinda yapacagi islemlerin tanimlandigi bolum.
       0000-MAIN.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0110-FILE-CONTROL.
           PERFORM 0200-PROCESS-FILE.
           PERFORM 0800-CLOSE-FILES.
           PERFORM 0999-EXIT.
      *MAIN programin baslangic noktasi. sirasiyla dosyalarin acilmasi,
      *dosya kontrolu, dosyadan okuma, dosyaya yazma, dosyalarin kapatilmasi
      *ve programin sonlandirilmasi islemlerini yapiyor.
      *------------------------
       0100-OPEN-FILES.
           OPEN INPUT  USERS-DATA.
           OPEN OUTPUT OUT-FILE.
      *dosyalari acan fonksiyon.
      *------------------------
       0110-FILE-CONTROL.
           IF NOT USERDATA-SUCC
              DISPLAY "Userdata file not found. Exiting..."
              STOP RUN
           END-IF.
           IF NOT OUTFILE-SUCC
              DISPLAY "Output file not found. Exiting..."
              STOP RUN
           END-IF.
      *dosyalarin acilip acilmadigini kontrol eden fonksiyon.
      *------------------------
       0200-PROCESS-FILE.
           PERFORM 0210-HEADER-PRINT.
           PERFORM 0250-READ-RECORD
           PERFORM UNTIL ST-EOF
               IF WS-VALID-ORDER = 1
                   PERFORM 0300-CALCULATE-DAYS
               END-IF
                   PERFORM 0400-WRITE-RECORD
               PERFORM 0250-READ-RECORD
           END-PERFORM.
      *dosyadan okuma ve yazma islemlerinin yapildigi fonksiyon.
      *ilk olarak header yazdiriliyor. daha sonra dosyadan okuma yapiliyor.
      *okunan kayitlarin dogrulugu kontrol ediliyor. dogruysa yasamis gun
      *sayisi hesaplaniyor. hesaplanan deger dosyaya yazdiriliyor. dosyadan
      *okuma islemi son kayit okunana kadar devam ediyor.
      *------------------------
       0210-HEADER-PRINT.
           MOVE HEADER TO OUT-REC.
           WRITE OUT-REC.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC.
      *header yazdiran fonksiyon.
      *------------------------
       0250-READ-RECORD.
           READ USERS-DATA
           MOVE BIRTH-DATE-I TO WS-PARAM-DATE.
           ADD 1 TO READ-CNT
           PERFORM 0260-INPUT-VALIDATOR.
           IF WS-VALID-ORDER = 1
               MOVE CURRENT-DATE-I TO WS-PARAM-DATE
               PERFORM 0260-INPUT-VALIDATOR
           END-IF.
      *dosyadan okuma fonksiyonu. okunan kayitlarin dogrulugu kontrol ediliyor.
      *2 tarihin de dogrulugu kontrol ediliyor. bunun icin tarihler ws-param-date
      *alanina atanip iki kat if kontrolu yapilmasi engelleniyor.
      *------------------------
       0260-INPUT-VALIDATOR.
           IF WS-PARAM-DATE  NOT NUMERIC
               DISPLAY WS-PARAM-DATE ": Invalid date for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           ELSE
               MOVE WS-PARAM-DATE TO WS-E-PARAM-DATE
               PERFORM 0270-YEAR-VALIDATOR
               IF WS-VALID-ORDER = 1
                   PERFORM 0280-MONTH-VALIDATOR
               END-IF
               IF WS-VALID-ORDER = 1
                   PERFORM 0290-DAY-VALIDATOR
               END-IF
           END-IF.
      *tarih parametresinin dogrulugunu kontrol eden fonksiyon. once tarih
      *parametresinin numerik olup olmadigi kontrol ediliyor. numerik degilse
      *kayit hatali olarak isaretleniyor. numerikse tarih parametresi
      *ws-e-param-date alanina atanip yil, ay ve gun degerleri ayri ayri
      *kontrol ediliyor. herhangi birinde hata varsa kayit hatali olarak
      *isaretleniyor.
      *------------------------
       0270-YEAR-VALIDATOR.
           IF (WS-PARAM-YEAR < 1601)
               DISPLAY WS-PARAM-YEAR ": Invalid year for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
      *yil parametresinin dogrulugunu kontrol eden fonksiyon. yil 1601'den
      *kucukse kayit hatali olarak isaretleniyor.
      *------------------------
       0280-MONTH-VALIDATOR.
           IF (WS-PARAM-MTH < 1 OR WS-PARAM-MTH > 12)
               DISPLAY WS-PARAM-MTH ": Invalid month for order "
               ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
      *ay parametresinin dogrulugunu kontrol eden fonksiyon. ay 1-12 arasinda
      *degilse kayit hatali olarak isaretleniyor.
      *------------------------
       0290-DAY-VALIDATOR.
           IF THURTY-ONE
                IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 31)
                     DISPLAY WS-PARAM-DAY ": Invalid day for order "
                             ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                END-IF
           ELSE IF FEB
               DIVIDE 4 INTO WS-PARAM-YEAR   GIVING WS-TMP
                    REMAINDER WS-RULE1
               DIVIDE 100 INTO WS-PARAM-YEAR GIVING WS-TMP
                    REMAINDER WS-RULE2
               DIVIDE 400 INTO WS-PARAM-YEAR GIVING WS-TMP
                    REMAINDER WS-RULE3
               IF ((WS-RULE1 = 0 AND WS-RULE2 NOT = 0) OR WS-RULE3 = 0)
                   IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 29)
                       DISPLAY WS-PARAM-DAY ": Invalid day for order "
                               ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                   END-IF
               ELSE
                   IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 28)
                       DISPLAY WS-PARAM-DAY ": Invalid day for order "
                               ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
                   END-IF
               END-IF
           ELSE IF THURTY
               IF (WS-PARAM-DAY < 1 OR WS-PARAM-DAY > 30)
                   DISPLAY WS-PARAM-DAY ": Invalid day for order "
                           ORDER-I " in line " READ-CNT  " ."
                     MOVE 0 TO WS-VALID-ORDER
                     EXIT
               END-IF
           END-IF.
      *aylarin gun sayilarini kontrol eden fonksiyon. ay 31 gunlu ise gun
      *sayisi 1-31 arasinda degilse kayit hatali olarak isaretleniyor. subat
      *ayi 29 gunlu ise yilin 4'e tam bolunmesi ve 100'e tam bolunmemesi veya
      *400'e tam bolunmesi durumlarinda gun sayisi 1-29 arasinda degilse kayit
      *hatali olarak isaretleniyor. subat ayi 28 gunlu ise gun sayisi 1-28
      *arasinda degilse kayit hatali olarak isaretleniyor. ay 30 gunlu ise gun
      *sayisi 1-30 arasinda degilse kayit hatali olarak isaretleniyor.
      *------------------------
       0300-CALCULATE-DAYS.
           MOVE BIRTH-DATE-I TO WS-BIRTH-DATE.
           COMPUTE WS-BIRTH-DATE =
           FUNCTION INTEGER-OF-DATE(WS-BIRTH-DATE).
           MOVE CURRENT-DATE-I TO WS-CURRENT-DATE.
           COMPUTE WS-CURRENT-DATE  =
           FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE).
           MOVE WS-CURRENT-DATE TO WS-E-CURRENT-DATE.
           MOVE WS-BIRTH-DATE TO WS-EBIRTH-DATE.
           IF (WS-CURRENT-YEAR - WS-BIRTH-YEAR > 270)
               DISPLAY WS-CURRENT-YEAR " " WS-BIRTH-YEAR
                       ": Invalid age for order " ORDER-I
                       " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           END-IF.
           IF (WS-EBIRTH-DATE > WS-E-CURRENT-DATE)
               DISPLAY WS-DAYS-LIVED ": Invalid dates for order "
                       ORDER-I " in line " READ-CNT  " ."
               MOVE 0 TO WS-VALID-ORDER
               EXIT
           ELSE
              COMPUTE WS-DAYS-LIVED = WS-CURRENT-DATE - WS-BIRTH-DATE
           END-IF.
      *dogum tarihi ve bugunun tarihini integer olarak hesaplayan fonksiyon.
      *bugunun tarihini ws-e-current-date alanina atiyor. dogum tarihini
      *ws-e-birth-date alanina atiyor. dogum tarihi bugunden buyukse kayit
      *hatali olarak isaretleniyor. dogum tarihi bugunden kucukse dogum
      *tarihinden bugune kadar gecen gun sayisi hesaplaniyor.
      *gun sayisi olarak 5 karakterlik bir alan kullandigimizdan dolayi
      *270'den buyuk yaslar icin kayit hatali olarak isaretleniyor.
      *------------------------
       0400-WRITE-RECORD.
           INITIALIZE OUT-REC
           IF WS-VALID-ORDER = 1
               MOVE SPACES  TO OUT-REC
               MOVE ORDER-I TO ORDER-O
               MOVE FIRST-NAME-I TO FIRST-NAME-O
               MOVE LAST-NAME-I TO LAST-NAME-O
               MOVE BIRTH-DATE-I TO BIRTH-DATE-O
               MOVE CURRENT-DATE-I TO CURRENT-DATE-O
               MOVE WS-DAYS-LIVED  TO LIVED-DAYS-O
           ELSE
               MOVE ERR TO OUT-REC
               MOVE 1 TO WS-VALID-ORDER
           END-IF.
           WRITE OUT-REC.
      *kayit hatali degilse kayit yazdiriliyor. kayit hatali ise hata
      *mesaji yazdiriliyor.
      *------------------------
       0800-CLOSE-FILES.
           CLOSE USERS-DATA.
           CLOSE OUT-FILE.
      *dosyalar kapatiliyor.
      *------------------------
       0999-EXIT.
           STOP RUN.
      *program sonlaniyor.