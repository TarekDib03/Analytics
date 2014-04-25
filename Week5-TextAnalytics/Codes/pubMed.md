MITx: 15.071x The Analytics Edge - AUTOMATING REVIEWS IN MEDICINE
========================================================

## *Introduction*
The medical literature is enormous. Pubmed, a database of medical publications maintained by the U.S. National Library of Medicine, has indexed over 23 million medical publications. Further, the rate of medical publication has increased over time, and now there are nearly 1 million new publications in the field each year, or more than one per minute.

The large size and fast-changing nature of the medical literature has increased the need for reviews, which search databases like Pubmed for papers on a particular topic and then report results from the papers found. While such reviews are often performed manually, with multiple people reviewing each search result, this is tedious and time consuming. In this problem, we will see how text analytics can be used to automate the process of information retrieval.

The dataset consists of the titles (variable title) and abstracts (variable abstract) of papers retrieved in a Pubmed search. Each search result is labeled with whether the paper is a clinical trial testing a drug therapy for cancer (variable trial). These labels were obtained by two people reviewing each search result and accessing the actual paper if necessary, as part of a literature review of clinical trials testing drug therapies for advanced and metastatic breast cancer.

## *Understanding the Data*

```r
# Data
setwd("/home/tarek/Analytics/Weeks/Week5-TextAnalytics/Data")
trials <- read.csv("clinical_trial.csv", stringsAsFactors = F)
str(trials)
```

```
## 'data.frame':	1860 obs. of  3 variables:
##  $ title   : chr  "Treatment of Hodgkin's disease and other cancers with 1,3-bis(2-chloroethyl)-1-nitrosourea (BCNU; NSC-409962)." "Cell mediated immune status in malignancy--pretherapy and post-therapy assessment." "Neoadjuvant vinorelbine-capecitabine versus docetaxel-doxorubicin-cyclophosphamide in early nonresponsive breast cancer: phase "| __truncated__ "Randomized phase 3 trial of fluorouracil, epirubicin, and cyclophosphamide alone or followed by Paclitaxel for early breast can"| __truncated__ ...
##  $ abstract: chr  "" "Twenty-eight cases of malignancies of different kinds were studied to assess T-cell activity and population before and after in"| __truncated__ "BACKGROUND: Among breast cancer patients, nonresponse to initial neoadjuvant chemotherapy is associated with unfavorable outcom"| __truncated__ "BACKGROUND: Taxanes are among the most active drugs for the treatment of metastatic breast cancer, and, as a consequence, they "| __truncated__ ...
##  $ trial   : int  1 0 1 1 1 0 1 0 0 0 ...
```

```r

# How many characters are there in the longest abstract?
max(nchar(trials$abstract))
```

```
## [1] 3708
```

```r

# How many search results provided no abstract?
sum(nchar(trials$abstract) == 0)
```

```
## [1] 112
```

```r

# What is the shortest title of any article?
trials$title[which.min(nchar(trials$title))]
```

```
## [1] "A decade of letrozole: FACE."
```


## *Preparing the Corpus*

```r
# Pre process data
library(tm)
# Create Corpus
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# Convert to lower case
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

# Remove punctuation
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove Stop words
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Look at the first document
corpusTitle[[1]]
```

```
## treatment  hodgkin diseas   cancer  13bis2chloroethyl1nitrosourea bcnu nsc409962
```

```r

# Create matrix
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle
```

```
## A document-term matrix (1860 documents, 2835 terms)
## 
## Non-/sparse entries: 23415/5249685
## Sparsity           : 100%
## Maximal term length: 49 
## Weighting          : term frequency (tf)
```

```r
dtmAbstract
```

```
## A document-term matrix (1860 documents, 12291 terms)
## 
## Non-/sparse entries: 153196/22708064
## Sparsity           : 99%
## Maximal term length: 67 
## Weighting          : term frequency (tf)
```

```r

# Filter out sparse terms by keeping only terms that appear in at least 5%
# or more of the documents
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmTitle
```

```
## A document-term matrix (1860 documents, 31 terms)
## 
## Non-/sparse entries: 10683/46977
## Sparsity           : 81%
## Maximal term length: 15 
## Weighting          : term frequency (tf)
```

```r
dtmAbstract
```

```
## A document-term matrix (1860 documents, 335 terms)
## 
## Non-/sparse entries: 91977/531123
## Sparsity           : 85%
## Maximal term length: 15 
## Weighting          : term frequency (tf)
```

```r

# Convert dtmTitle and dtmAbstract to data frames
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
```


## *Building a Model*

```r
# We want to combine dtmTitle and dtmAbstract into a single data frame to
# make predictions. However, some of the variables in these data frames have
# the same names.

colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
colnames(dtmTitle)
```

```
##  [1] "Tadjuv"           "Tadvanc"          "Tbreast"         
##  [4] "Tcancer"          "Tchemotherapi"    "Tclinic"         
##  [7] "Tcombin"          "Tcompar"          "Tcyclophosphamid"
## [10] "Tdocetaxel"       "Tdoxorubicin"     "Tearli"          
## [13] "Teffect"          "Tgroup"           "Tiii"            
## [16] "Tmetastat"        "Tpatient"         "Tphase"          
## [19] "Tplus"            "Tpostmenopaus"    "Trandom"         
## [22] "Trandomis"        "Trespons"         "Tresult"         
## [25] "Tstudi"           "Ttamoxifen"       "Ttherapi"        
## [28] "Ttreatment"       "Ttrial"           "Tversus"         
## [31] "Twomen"
```

```r
colnames(dtmAbstract)
```

```
##   [1] "A0001"            "A001"             "A005"            
##   [4] "A100"             "A500"             "A5fluorouracil"  
##   [7] "Aaccord"          "Aachiev"          "Aactiv"          
##  [10] "Aaddit"           "Aadjuv"           "Aadminist"       
##  [13] "Aadministr"       "Aadvanc"          "Aadvers"         
##  [16] "Aage"             "Aagent"           "Aaim"            
##  [19] "Aalon"            "Aalso"            "Aalthough"       
##  [22] "Aamong"           "Aanalys"          "Aanalysi"        
##  [25] "Aanalyz"          "Aandor"           "Aanthracyclin"   
##  [28] "Aappear"          "Aarm"             "Aaromatas"       
##  [31] "Aassess"          "Aassign"          "Aassoci"         
##  [34] "Aavail"           "Aaxillari"        "Abackground"     
##  [37] "Abase"            "Abaselin"         "Abenefit"        
##  [40] "Abetter"          "Abone"            "Abreast"         
##  [43] "Acan"             "Acancer"          "Acarcinoma"      
##  [46] "Acase"            "Acaus"            "Acell"           
##  [49] "Achang"           "Acharacterist"    "Achemotherapi"   
##  [52] "Aclinic"          "Acmf"             "Acombin"         
##  [55] "Acommon"          "Acompar"          "Acomparison"     
##  [58] "Acomplet"         "Aconclus"         "Aconduct"        
##  [61] "Aconfid"          "Aconfirm"         "Aconsid"         
##  [64] "Aconsist"         "Acontinu"         "Acontrol"        
##  [67] "Acorrel"          "Acours"           "Acycl"           
##  [70] "Acyclophosphamid" "Adaili"           "Adata"           
##  [73] "Aday"             "Adeath"           "Adecreas"        
##  [76] "Adefin"           "Ademonstr"        "Adesign"         
##  [79] "Adetect"          "Adetermin"        "Adevelop"        
##  [82] "Adfs"             "Adiffer"          "Adiseas"         
##  [85] "Adiseasefre"      "Adistant"         "Adocetaxel"      
##  [88] "Adose"            "Adoubleblind"     "Adoxorubicin"    
##  [91] "Adrug"            "Adue"             "Adurat"          
##  [94] "Aearli"           "Aeffect"          "Aefficaci"       
##  [97] "Aeight"           "Aeither"          "Aelig"           
## [100] "Aend"             "Aendocrin"        "Aendpoint"       
## [103] "Aenrol"           "Aenter"           "Aepirubicin"     
## [106] "Aestim"           "Aestrogen"        "Aevalu"          
## [109] "Aevent"           "Aeveri"           "Aevid"           
## [112] "Aexamin"          "Aexperienc"       "Aexpress"        
## [115] "Afactor"          "Afailur"          "Afind"           
## [118] "Afirst"           "Afirstlin"        "Afive"           
## [121] "Afluorouracil"    "Afollow"          "Afollowup"       
## [124] "Afound"           "Afour"            "Afrequent"       
## [127] "Afunction"        "Ageneral"         "Agiven"          
## [130] "Agrade"           "Agreater"         "Agroup"          
## [133] "Agrowth"          "Ahazard"          "Ahematolog"      
## [136] "Aher2"            "Ahigh"            "Ahigher"         
## [139] "Ahistolog"        "Ahormon"          "Ahowev"          
## [142] "Ahuman"           "Ahundr"           "Aidentifi"       
## [145] "Aiii"             "Aimport"          "Aimprov"         
## [148] "Aincid"           "Ainclud"          "Aincreas"        
## [151] "Aindepend"        "Aindic"           "Ainform"         
## [154] "Ainfus"           "Ainhibitor"       "Ainiti"          
## [157] "Ainterv"          "Aintraven"        "Ainvestig"       
## [160] "Ainvolv"          "Alarg"            "Aleast"          
## [163] "Aless"            "Alevel"           "Alife"           
## [166] "Alimit"           "Alocal"           "Alonger"         
## [169] "Alow"             "Alower"           "Alymph"          
## [172] "Amain"            "Amajor"           "Amarker"         
## [175] "Amastectomi"      "Amay"             "Ambc"            
## [178] "Amean"            "Ameasur"          "Amedian"         
## [181] "Ametastas"        "Ametastat"        "Amethod"         
## [184] "Amethotrex"       "Amgm2"            "Amodel"          
## [187] "Amonth"           "Amulticent"       "Amultivari"      
## [190] "Anausea"          "Aneed"            "Anegat"          
## [193] "Aneoadjuv"        "Aneutropenia"     "Anew"            
## [196] "Anode"            "Anodeposit"       "Anumber"         
## [199] "Aobject"          "Aobserv"          "Aobtain"         
## [202] "Aoccur"           "Aone"             "Aoper"           
## [205] "Aoral"            "Aoutcom"          "Aoveral"         
## [208] "Apaclitaxel"      "Apartial"         "Aparticip"       
## [211] "Apatholog"        "Apatient"         "Aper"            
## [214] "Aperform"         "Aperiod"          "Aphase"          
## [217] "Aplacebo"         "Aplus"            "Apoint"          
## [220] "Apopul"           "Aposit"           "Apossibl"        
## [223] "Apostmenopaus"    "Apostop"          "Apotenti"        
## [226] "Apredict"         "Apremenopaus"     "Apresent"        
## [229] "Apretreat"        "Aprevent"         "Aprevious"       
## [232] "Aprimari"         "Aprior"           "Aprofil"         
## [235] "Aprogesteron"     "Aprognost"        "Aprogress"       
## [238] "Aprogressionfre"  "Aprolong"         "Aproport"        
## [241] "Aprospect"        "Aprovid"          "Apurpos"         
## [244] "Aqualiti"         "Aradiotherapi"    "Arandom"         
## [247] "Arandomis"        "Arang"            "Arate"           
## [250] "Aratio"           "Areceiv"          "Areceptor"       
## [253] "Areceptorposit"   "Arecurr"          "Areduc"          
## [256] "Areduct"          "Aregard"          "Aregimen"        
## [259] "Aregress"         "Arelaps"          "Arelat"          
## [262] "Aremain"          "Areport"          "Arequir"         
## [265] "Arespect"         "Arespond"         "Arespons"        
## [268] "Aresult"          "Arisk"            "Asafeti"         
## [271] "Asampl"           "Aschedul"         "Ascore"          
## [274] "Asecond"          "Asecondari"       "Aseen"           
## [277] "Aselect"          "Asequenti"        "Aserum"          
## [280] "Aset"             "Aseven"           "Asever"          
## [283] "Ashow"            "Ashown"           "Aside"           
## [286] "Asignific"        "Asimilar"         "Asingl"          
## [289] "Asite"            "Asix"             "Asize"           
## [292] "Astabl"           "Astage"           "Astandard"       
## [295] "Astart"           "Astatist"         "Astatus"         
## [298] "Astudi"           "Asubgroup"        "Asuggest"        
## [301] "Asuperior"        "Asupport"         "Asurgeri"        
## [304] "Asurviv"          "Asystem"          "Atamoxifen"      
## [307] "Aterm"            "Atest"            "Atherapi"        
## [310] "Athree"           "Atime"            "Atissu"          
## [313] "Atoler"           "Atotal"           "Atoxic"          
## [316] "Atreat"           "Atreatment"       "Atrend"          
## [319] "Atrial"           "Atumor"           "Atumour"         
## [322] "Atwo"             "Atype"            "Ause"            
## [325] "Avalu"            "Aversus"          "Avomit"          
## [328] "Aweek"            "Awell"            "Awherea"         
## [331] "Awhether"         "Awithin"          "Awithout"        
## [334] "Awomen"           "Ayear"
```

```r

# Combine the two dataframes
dtm <- cbind(dtmTitle, dtmAbstract)
# Add the Vandal variable
dtm$trial <- trials$trial

# Load CaTools
library(caTools)
set.seed(144)
spl <- sample.split(dtm, SplitRatio = 0.7)
train <- subset(trials, spl == T)
test <- subset(trials, spl == F)

# baseline model accuracy on the training set
table(train$trial)[1]/sum(table(train$trial))
```

```
##      0 
## 0.5585
```

```r

# CART Model
library(rpart)
library(rpart.plot)
trialsCART <- rpart(trial ~ ., data = train, method = "class")
prp(trialsCART)
```

```
## Warning: abbreviate used with non-ASCII chars
## Warning: input string 1 is invalid in this locale
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: conversion failure on 'title = 2epaocnbprtnetibcp,5bpviarorcs,5aefccwcea5poctoa,9dscw9bsftdobmaporthtipwbc,Aalndifanc,Actotaahpiabc,Acttcealpaiacotb,AcaotprtoasdoppccadFipwsIbc,Acsoiiapipwbmfbolceombp,Acodp(and(ittobc,AcomsrttrtaftpohmroaRTOGrpt,Acotooncrinpbc,Accsotate,Accttcthwyiotpittoabc,Aceobctarsorbt,Afcnavarct,Awbaneftcocniwwbc,Actipadtateoctanp,Amomlwbcttb1atw4acocfbc,Addfqolmiabct,AdolF,AapwoaciowwebciC4acs6,Acaetaoicipp(T1,Actwcda5(wowmafncpUa7f,Acopbcwa(le,AcfbgcwematioahfaqolippIBCSGTV,Acfep(o=ywehbcarao2p,Acfpbcamuqs,ACvCptvoaipnbcptroaECOGs,Aetpzaipwwebc5fotAbds,AiinproaSs,AlvtatcEsfpwweebcsrftB1rt,Atiebcoonpc,Atfnbcaci,Atobcwdc(,Atocvtcimofbifaoa,Atihbc,Admpsopdopibcp(IIIaIwmd,AdrvcsteteoMilteorotsia,AdvcsteteoM(ahafitmord,AsapotboMaM,Aoartaatinpbcprop,ApvopviiiabaeatRftIBCSGrtocatfebc,Ahiovfiwwbc,AotIgihbcawrtac,Aovnbga,A(hiaipbc,ApotIp(ctblapteoatipapnbcp,Amatprotnbc,AacdfBbcabohpciHbcp,Aatsoteoscariesbcacs,AaocipwpomobcRoacrs,AafscsioswalcwtitB1s,Aocarbtdaatcrfebc,Aoo [... truncated]
## Warning: input string 1 is invalid in this locale
## Warning: input string 1 is invalid in this locale
## Warning: input string 1 is invalid in this locale
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# Predict using the test set pred = predict(wikiCART, newdata=test,
# type='class') Accuracy on the test set t1 <- table(test$Vandal, pred)
# (t1[1,1] + t1[2,2])/(sum(t1)) Plot tree prp(wikiCART)
```


