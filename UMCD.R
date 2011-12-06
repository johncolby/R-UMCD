library(XML)
library(RCurl)

# Login to UMCD to enable access to private data
umcdLogin <- function(email=NULL, password=NULL) {
  # Get curl handle
  curl = getCurlHandle(.opts=list(followlocation=TRUE, cookiefile='', verbose=F))  
  
  if(!(is.null(email) & is.null(password))) {
    login.url = 'http://jessebrown.webfactional.com/user/login'
    login.html = getURL(login.url, curl=curl)
    formkey = xmlAttrs(xmlRoot(htmlTreeParse(login.html))[['body']][[2]][[1]][[3]][[2]][[1]][['form']][[2]][[2]])[['value']]
    invisible(postForm(login.url,
                       email       = email,
                       password    = password,
                       '_next'     = '/',
                       '_formkey'  = formkey,
                       '_formname' = 'login',
                       curl=curl))
  }
  curl
}

# List available network names for a given study name
umcdListNetworks <- function(study_name, curl=NULL) {
  # Get curl handle
  if(is.null(curl))
    curl = umcdLogin()
  
  html = getForm('http://jessebrown.webfactional.com/study_networks', study_name=study_name, curl=curl)
  as.character(xmlSApply(xmlRoot(htmlTreeParse(html))[['body']], xmlValue))
}

# Compile UMCD analysis data for a set of requests
umcdAnalyze <- function(requests, curl=NULL) {
  # Get curl handle
  if(is.null(curl))
    curl = umcdLogin()

  pb = txtProgressBar(0, nrow(requests), 0, style=3)

  info            = NULL
  global.measures = NULL
  nodal.measures  = NULL
  for(i in 1:nrow(requests)) {  
    # Get a unique key for a new request
    form = getURL('http://jessebrown.webfactional.com/', curl=curl)
    formkey = xmlAttrs(xmlRoot(htmlTreeParse(form))[['body']][[2]][[1]][[3]][[2]][[1]][['form']][[2]][[1]])[['value']]
  
    # Submit API request and retrieve results
    results.html = postForm('http://jessebrown.webfactional.com',
                            study_name   = requests[i, 'study_name'],
                            network_name = requests[i, 'network_name'],
                            weight       = requests[i, 'weight'],
                            edge_density = requests[i, 'density'],
                            orientation  = requests[i, 'orientation'],
                            '_formkey'   = formkey,
                            '_formname'  = 'no_table/create',
                            curl=curl)
    
    # Parse out network info
    info.table = readHTMLTable(results.html)[[2]]
    info = rbind(info, data.frame(t(as.character(info.table[[2]]))))
    
    # Parse out global measures
    global.measures = rbind(global.measures, cbind(requests[i, ], readHTMLTable(results.html)[[3]], row.names=NULL))

    # Fetch nodal measures .txt file
    nodal.url = sprintf('http://jessebrown.webfactional.com/welcome/static/%s_metrics.txt', paste(requests[i, 'network_name'], requests[i, 'weight'], requests[i, 'density'], sep='_'))
    nodal.measures = rbind(nodal.measures, cbind(requests[i, ], read.table(nodal.url, header=T, sep='\t', quote='', comment.char=''), row.names=NULL))
  
    setTxtProgressBar(pb, i)
    flush.console()
  }
  
  close(pb)
  
  # Format output
  colnames(info) = info.table[[1]]
  colnames(info)[1] = 'network_name'
  info$network_name = as.character(info$network_name)
  colnames(global.measures)[6:7] = c('measure', 'value')
  global.measures$measure = gsub('(.+)  \\?', '\\1', global.measures$measure)
  global.measures$value = as.numeric(as.character(global.measures$value))
  global.measures = subset(global.measures, !measure %in% c('Chosen Density (%)', 'Small World Attributes'))
  colnames(nodal.measures)[6:11] = gsub('X\\.(.+)', '\\1', colnames(nodal.measures)[6:11])
  
  return(list(info=info, global.measures=global.measures, nodal.measures=nodal.measures))
}

# Import the entire table of available networks
umcdBrowse <- function(curl=NULL) {
  # Get curl handle
  if(is.null(curl))
    curl = umcdLogin()
  
  browse.html = getURL('http://jessebrown.webfactional.com/browse', curl=curl)
  readHTMLTable(browse.html)[[2]]
}

# Delete specific networks (by ID)
umcdDelete <- function(ids, curl=NULL) {
  # Get curl handle
  if(is.null(curl))
    curl = umcdLogin()
  
  pb = txtProgressBar(0, length(ids), 0, style=3)
  
  for(i in 1:length(ids)) {
    id = ids[i]
    update.url = paste('http://jessebrown.webfactional.com/update/', id, sep='')
    form = getURL(update.url, curl=curl)
    formkey = xmlAttrs(xmlRoot(htmlTreeParse(form))[['body']][[2]][[1]][[3]][[2]][[1]][['form']][[2]][[2]])[['value']]
    invisible(postForm(update.url,
                       delete_this_record = 'on',
                       id                 = as.character(id),
                       '_formkey'         = formkey,
                       '_formname'        = paste('upload_data/', id, sep=''),
                       curl=curl))
  
    setTxtProgressBar(pb, i)
    flush.console()
  }
  close(pb)
}
