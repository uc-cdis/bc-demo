require(httr)
require(jsonlite)

credsfile <- 'credentials.json'

add_keys <- function(filename){
    json_data <- readChar(filename, file.info(filename)$size)
    keys <- fromJSON(json_data)
    variable <- jsonlite::toJSON(list(api_key = keys$api_key), auto_unbox = TRUE)
    auth <- POST('https://data.braincommons.org/user/credentials/cdis/access_token', add_headers("Content-Type" = "application/json"), body = variable)
    return(auth)
}

query_api <- function(query_txt){
    auth = add_keys(credsfile)
    query <- jsonlite::toJSON(list(query = query_txt), auto_unbox = TRUE)
    token <- paste('bearer', content(auth)$access_token, sep=" ")
    response <- POST('https://data.braincommons.org/api/v0/submission/graphql',add_headers("Authorization" = token, "Content-Type" = "application/json"), body = query)
    return(content(response))
}

download_data <- function(study_id){
          
    query_txt = paste('{expression_array_result(quick_search: "', study_id, '"){
                          submitter_id
                          file_name
                          id
                        }}',sep="")
    
    data <- query_api(query_txt)
    
    auth = add_keys(credsfile)
    token <- paste('bearer', content(auth)$access_token, sep=" ")

    uuid <- data$data$expression_array_result[[1]]$id
    filename <- data$data$expression_array_result[[1]]$file_name
    
    if(!file.exists(filename)){
        download_url <- paste('https://data.braincommons.org/user/data/download/', uuid, sep="")
        response <-GET(download_url,add_headers("Authorization" = token))
        response_file <- GET(content(response)$url,write_disk(filename,overwrite=TRUE))
    }
    
    normalized_data <- read_delim(filename, 
                    "\t", escape_double = FALSE, trim_ws = TRUE)
    
    return(normalized_data)       
}


get_phenodata <- function(project_id, study_id){

   query_txt = paste('{
                         case(first:0, project_id: "', project_id, '", with_path_to:{type: "study", submitter_id: "', study_id, '"}){
                            submitter_id
                            experimental_group
                            diagnoses{
                              caps_diagnosis
                              biospecimens{
                                samples{
                                  submitter_id
                                }
                              }
                              exposures{
                                military_service
                              }
                            }
                          }
                        }',sep="")
    
    data <- query_api(query_txt)

    # Create data frame with data
    df <- data.frame(Sample=character(),
                     Case=character(), 
                     Group=character(),
                     Diagnosis=character(),
                     PTSD=integer(),
                     Time=integer(),
                     stringsAsFactors=FALSE) 

    for (case in data$data$case){
        group <- case$experimental_group
        caseid <- case$submitter_id
        if (group == 'control'){
            ptsd = 1
        }
        else{
            ptsd = 2
        }
        for (diag in case$diagnoses){
            diagnosis <- diag$caps_diagnosis
            for (exp in diag$exposures){
                if(exp$military_service){
                    time <- 2
                }
                else{
                    time <- 1
                }
            }
            for (bio in diag$biospecimens){
                for (sm in bio$samples){
                    sample_id <- sm$submitter_id
                    t=1
                    if(time == 2){
                       t=3
                    }
                    sample <- paste(c("Sample", as.integer(unlist(strsplit(sample_id,"[-]"))[2]), "_", t), collapse="")
                }
            }
            df[nrow(df)+1,] <- c(caseid, sample, group, diagnosis, ptsd, time)
        }
    }    
    
    return(df)
    
}