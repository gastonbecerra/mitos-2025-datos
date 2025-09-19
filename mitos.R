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


# leer json a tidy ---------------------------------------


gv<-function(v)if(is.null(v))NA_character_ else as.character(unlist(v)[1])
`%||%`<-function(x,y)if(is.null(x))y else x

files<-list.files(out_dir,pattern="\\.json$",full.names=TRUE)
lst<-list.files(out_dir,pattern="\\.json$",full.names=TRUE)|>lapply(\(p)jsonlite::fromJSON(p,simplifyVector=FALSE))

# meta
id<-vapply(lst,\(d)sub(".*/","",d$name),"")
f<-lapply(lst,\(d)d$fields)
meta<-data.frame(
  id=sapply(lst,\(d)sub(".*/","",d$name)),
  startTime=sapply(lst,\(d)gv(d$fields$startTime)),
  completionTimestamp=sapply(lst,\(d)gv(d$fields$completionTimestamp)),
  submittedAt=sapply(lst,\(d)gv(d$fields$submittedAt)),
  stringsAsFactors=FALSE
) |>
  mutate(
    startTime=ymd_hms(startTime, tz="UTC"),
    completionTimestamp=ymd_hms(completionTimestamp, tz="UTC"),
    submittedAt=ymd_hms(submittedAt, tz="UTC"),
    duration=as.numeric(difftime(completionTimestamp,startTime,units="secs"))
  )
glimpse(meta)
rm(lst)

# attitudes
att_keys <- paste0("A",1:5)
attitudes<-dplyr::bind_cols(
  tibble::tibble(id=id),
  do.call(dplyr::bind_rows,lapply(f,\(ff){
    m<-ff$attitudes$mapValue$fields
    if(is.null(m)) return(as.data.frame(setNames(as.list(rep(NA_character_,length(att_keys))),att_keys)))
    as.data.frame(setNames(lapply(att_keys,\(nm)gv(m[[nm]])),att_keys),stringsAsFactors=FALSE)
  }))
) |> dplyr::mutate(dplyr::across(-id,~as.integer(.)))
glimpse(attitudes)

# beliefs
belief_keys<-paste0("C",1:5)
beliefs<-dplyr::bind_cols(
  tibble::tibble(id=id),
  do.call(dplyr::bind_rows,lapply(f,\(ff){
    m<-ff$beliefs$mapValue$fields
    vals<-setNames(vector("list",length(belief_keys)),belief_keys)
    for(k in belief_keys) vals[[k]]<-gv(m[[k]])
    as.data.frame(vals,stringsAsFactors=FALSE)
  }))
)|>dplyr::mutate(dplyr::across(-id,as.integer))
beliefs

sc_keys<-paste0("E",1:14)
scenarios<-do.call(dplyr::bind_rows,lapply(seq_along(f),\(i){
  m<-f[[i]]$scenarios$mapValue$fields
  if(is.null(m))return(NULL)
  ks<-intersect(sc_keys,names(m))
  if(!length(ks))return(NULL)
  tibble::tibble(
    id=id[i],
    item=ks,
    answer=sapply(ks,\(k)gv(m[[k]]$mapValue$fields$answer)),
    optionsCount=as.integer(sapply(ks,\(k)gv(m[[k]]$mapValue$fields$optionsCount)))
  )
}))
scenarios

dem_keys<-c("country","uso_ia_frecuencia","studyArea","education","workArea","gender","age")
demographics<-dplyr::bind_cols(
  tibble::tibble(id=id),
  do.call(dplyr::bind_rows,lapply(f,\(ff){
    m<-ff$demographics$mapValue$fields
    vals<-setNames(vector("list",length(dem_keys)),dem_keys)
    for(k in dem_keys) vals[[k]]<-gv(m[[k]])
    as.data.frame(vals,stringsAsFactors=FALSE)
  }))
)|>dplyr::mutate(age=as.integer(age))
demographics

rm(att_keys, sc_keys, dem_keys, belief_keys, lst)
rm(f,id,out_dir,files,gv,lst,`%||%`,firebase_collection,firebase_config)

data <- list(
  meta |> dplyr::select(id,startTime,completionTimestamp,submittedAt,duration),
  attitudes, beliefs, demographics
) |> purrr::reduce(dplyr::left_join, by="id") |>
  dplyr::left_join(
    scenarios |> dplyr::group_by(id) |> tidyr::nest(scenarios=c(item,answer,optionsCount)),
    by="id"
  )

glimpse(data)
