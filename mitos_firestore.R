library(tidyverse)
library(lubridate)
library(dotenv)


# firebase ---------------------------------------


dotenv::load_dot_env(".env")

firebase_config <- list(
  apiKey = Sys.getenv("FIREBASE_API_KEY"),
  authDomain = Sys.getenv("FIREBASE_AUTH_DOMAIN"),
  projectId = Sys.getenv("FIREBASE_PROJECT_ID"),
  storageBucket = Sys.getenv("FIREBASE_STORAGE_BUCKET"),
  messagingSenderId = Sys.getenv("FIREBASE_MESSAGING_SENDER_ID"),
  appId = Sys.getenv("FIREBASE_APP_ID")
)

firebase_collection<-"survey-responses"


# bajar json a disco ---------------------------------------


library(httr)
library(jsonlite)
out_dir<-paste0("data/",format(Sys.time(),"%Y%m%d_%H%M%S"))
dir.create(out_dir,F,T)
n<-0;token<-NULL
repeat{
  url<-paste0("https://firestore.googleapis.com/v1/projects/",firebase_config$projectId,
              "/databases/(default)/documents/",utils::URLencode(firebase_collection,T),
              "?pageSize=300",if(!is.null(token))paste0("&pageToken=",utils::URLencode(token,T))else"",
              "&key=",firebase_config$apiKey)
  raw<-GET(url)|>content("text",encoding="UTF-8")|>fromJSON(simplifyVector=F)
  docs<-raw$documents;if(is.null(docs)||!length(docs))break
  for(doc in docs){writeLines(toJSON(doc,auto_unbox=T,null="null",pretty=T),
                              file.path(out_dir,paste0(sub(".*/","",doc$name),".json")));n<-n+1}
  token<-raw$nextPageToken;if(is.null(token))break
}
cat("Descargados:",n,"documentos en",out_dir,"\n")
rm(n,token,url,raw,docs,doc)


# leer jsons ---------------------------------------


gv<-function(v)if(is.null(v))NA_character_ else as.character(unlist(v)[1])
pull_keys<-function(f,keys)as.data.frame(setNames(lapply(keys,\(k)sapply(f,\(x)gv(x[[k]]))),keys),stringsAsFactors=FALSE)

files<-list.files(out_dir,pattern="\\.json$",full.names=TRUE)
id<-sub("\\.json$","",basename(files))
lst<-lapply(files,\(p)jsonlite::fromJSON(p,simplifyVector=FALSE))
f<-lapply(lst,\(d)d$fields)

att_keys<-paste0("A",1:5)
belief_keys<-paste0("C",1:6)
sc_keys<-paste0("E",1:14)

data<-tibble::tibble(
  id=id,
  startTime=lubridate::ymd_hms(sapply(f,\(x)gv(x$startTime)),tz="UTC"),
  completionTimestamp=lubridate::ymd_hms(sapply(f,\(x)gv(x$completionTimestamp)),tz="UTC"),
  submittedAt=lubridate::ymd_hms(sapply(f,\(x)gv(x$submittedAt)),tz="UTC"),
  duration=as.numeric(difftime(completionTimestamp,startTime,units="secs"))
  )|>
  dplyr::bind_cols(
    pull_keys(f,att_keys)|>dplyr::mutate(dplyr::across(dplyr::everything(),as.integer)),
    pull_keys(f,belief_keys)|>dplyr::mutate(dplyr::across(dplyr::everything(),as.integer)),
    tibble::tibble(
      country=sapply(f,\(x)gv(x$country)),
      uso_ia_frecuencia=sapply(f,\(x)gv(x$uso_ia_frecuencia)),
      studyArea=sapply(f,\(x)gv(x$studyArea)),
      education=sapply(f,\(x)gv(x$education)),
      workArea=sapply(f,\(x)gv(x$workArea)),
      gender=sapply(f,\(x)gv(x$gender)),
      age=as.integer(sapply(f,\(x)gv(x$age)))
    ),
    tibble::tibble(optionsCount=as.integer(sapply(f,\(x)gv(x$optionsCount))))
  )|>
  dplyr::left_join(
    dplyr::bind_rows(lapply(seq_along(f),\(i){
      ks<-intersect(sc_keys,names(f[[i]]));if(!length(ks))return(NULL)
      tibble::tibble(id=id[i],item=ks,answer=sapply(ks,\(k)gv(f[[i]][[k]])))
    }))|>dplyr::group_by(id)|>tidyr::nest(scenarios=c(item,answer)),
    by="id"
  )|>
  dplyr::distinct(id,.keep_all=TRUE)|>
  dplyr::relocate(id,.before=1)

rm(files,lst,f,att_keys,belief_keys,sc_keys,pull_keys,gv)
rm(firebase_config, firebase_collection, id, out_dir)

glimpse(data)

data %>% count(optionsCount)
data %>% count(country)
