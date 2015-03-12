library(dplyr)
library(RCurl)
library(XML)

get_url.f <- function(baseurl,rmv = 0,...){
  urls <- paste(baseurl,..., sep = '/')
  script <- getURL(urls)
  doc <- htmlParse(script)
  links <- getHTMLLinks(doc)
  if(rmv != 0){links <- links[-rmv]}
  links <- paste(urls, links, sep = '')
  return(links)
}
get_csv.f <- function(links, folder){
  csvurl <- links[grep('csv', links)]
  for(i in 1:length(csvurl)){
    csvname <- paste(folder,strsplit(csvurl[i], '/')[[1]][length(strsplit(csvurl[i], '/')[[1]])], sep = '/')
    download.file(url=csvurl[i],destfile=csvname)
  }
}
get_txt.f <- function(links, folder){
  txturl <- links[grep('txt', links)]
  for(i in 1:length(txturl)){
    txtfile <- try(read.table(txturl[i]), silent = TRUE)
    txtname <- paste(folder,strsplit(txturl[i], '/')[[1]][length(strsplit(txturl[i], '/')[[1]])], sep = '/')
    if(class(txtfile) == 'try-error'){
      txtfile <- readLines(txturl[i])
      writeLines(txtfile, txtname)
    }else{
      write.table(txtfile, file = txtname)
    }
  }
}

## main
get_all_data.f <- function(baseurl, folder, rmv = 0, ...){
  dir.create(file.path(folder), showWarnings = FALSE)
  
  links <- get_url.f(baseurl, rmv = 1,...)
  if(any(grepl('csv', links))) get_csv.f(links, folder)
  if(any(grepl('txt', links))) get_txt.f(links, folder)
  
  if(any(substring(links, nchar(links)) == '/')){
    lk <- links[which(substring(links, nchar(links)) == '/')]
    while(any(substring(lk, nchar(lk)) == '/')){
        lk <- lk[which(substring(lk, nchar(lk)) == '/')]
      for(i in 1:length(lk)){
        newlk <- get_url.f(lk[i], rmv = 1)
        subfolder <- paste(folder, strsplit(lk, '/')[[i]][length(strsplit(lk, '/')[[i]])], sep = '/')
        dir.create(file.path(subfolder), showWarnings = FALSE)
        if(any(grepl('csv', newlk))) get_csv.f(newlk, subfolder)
        if(any(grepl('txt', newlk))) get_txt.f(newlk, subfolder)
      }
      lk <- newlk
      folder <- subfolder
    }
  }
}

baseurl <- 'http://www.nycdatascience.com/slides/BOOTCAMP'
folder <- 'classdata'

for(i in 1:6){
  week <- paste0('week', i)
  for(j in 1:6){
    lec <- paste0('lec', j)
    try(get_all_data.f(baseurl, folder, rmv = 1, week, lec, 'data/'), silent = TRUE)
  }
}
