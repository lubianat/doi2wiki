library(stringr)
library(dplyr)
library(httr)
library(WikidataQueryServiceR)

reconcile_df_to_wikidata <- function(df, id_type = "DOI") {
  if (id_type == "PMID") {
    df[, id_type] <- as.character(df[, id_type])
    
  }
  df[, id_type] <- toupper(df[, id_type])
  ids <- df[, id_type]
  api_sized_chunks <- get_chunks(ids, max = 99)
  
  i = 1
  list_of_ids_wikidata <- list()
  for (ids in api_sized_chunks) {
    list_of_ids_wikidata[[i]] <- get_id_df(ids,
                                           id_type=id_type)
    i = i + 1
  }
  ids_wikidata <- do.call("rbind", list_of_ids_wikidata)
  ids_wikidata <- dplyr::distinct(ids_wikidata)
  colnames(ids_wikidata) <- c(id_type, "QID", "Wikidata Label")
  
  # It was implicitly converted to tibble
  ids_wikidata<- as.data.frame(ids_wikidata)
  if (id_type == "PMID") {
    ids_wikidata[, id_type] <- as.character(ids_wikidata[, id_type])
    
  }
  df <- dplyr::left_join(df, ids_wikidata)
  return(df)
}

get_chunks <- function(a, max = 99) {
  y <- seq_along(a)
  chunks <- split(a, ceiling(y / max))
  return(chunks)
}

get_id_df <- function(ids, id_type = "DOI") {
  formatted_ids <- paste(ids, collapse = '" "')
  formatted_ids <- paste0('"', formatted_ids, '"')
  
  if (id_type == "DOI") {
    id_property <- "P356"
  } else if (id_type == "PMID") {
    id_property <- "P698"
  }
  query = paste0(
    '
    SELECT ?id ?item  ?itemLabel
    WHERE {
      {
        SELECT ?item ?id WHERE {
          VALUES ?unformatted_id {

          ',
    formatted_ids,
    '

          }
          BIND(UCASE(?unformatted_id) AS ?id)
          ?item wdt:',
    id_property,
    ' ?id
        }
      }
      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
    }
    '
  )
  ids_wikidata <- query_wikidata(query)
  
  qids <- c()
  for (u in ids_wikidata[["item"]])
  {
    qid <- str_replace(u, "http://www.wikidata.org/entity/", "")
    qids <- c(qids, qid)
  }
  ids_wikidata[["item"]] <- qids
  return(ids_wikidata)
}
