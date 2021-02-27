library(stringr)
library(dplyr)
library(httr)
library(WikidataQueryServiceR)

reconcile_df_to_wikidata <- function(df) {
  df[,"DOI"] <- toupper(df[,"DOI"])
  dois <- df[,"DOI"]
  api_sized_chunks <- get_chunks(dois, max=99)
  
  i = 1
  list_of_dois_wikidata <- list()
  for (dois in api_sized_chunks){
    list_of_dois_wikidata[[i]] <- get_doi_df(dois)
    i=i+1
  }
  dois_wikidata <- do.call("rbind", list_of_dois_wikidata)
  dois_wikidata <- dplyr::distinct(dois_wikidata)
  df <- dplyr::left_join(df, dois_wikidata)
  return(df)
}

get_chunks <- function(a, max=99) {
  y <- seq_along(a)
  chunks <- split(a, ceiling(y/max))
  return(chunks)
}

get_doi_df <- function(dois) {
  formatted_dois <- paste(dois, collapse = '" "')
  formatted_dois <- paste0('"', formatted_dois, '"')
  query = paste0('
    SELECT ?DOI ?item  ?itemLabel
    WHERE {
      {
        SELECT ?item ?DOI WHERE {
          VALUES ?doi {
  
          ', formatted_dois, '
  
          }
          BIND(UCASE(?doi) AS ?DOI)
          ?item wdt:P356 ?DOI.
        }
      }
      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
    }
    ')
  dois_wikidata <- query_wikidata(query)
  
  qids <- c()
  for (u in dois_wikidata[["item"]])
  {
    qid <- str_replace(u, "http://www.wikidata.org/entity/", "")
    qids <- c(qids, qid)
  }
  dois_wikidata[["item"]] <- qids
  return(dois_wikidata)
}

