  library(rvest)
  library(httr)
  useragent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") # spoof user agent
#Example
#  gs_df1 <- scrape_gs(term = 'intext:"genomic" AND "selection"', pages = 1:40, crawl_delay = 1.2, useragent)
# gs_df2 <- scrape_gs(term = 'intitle:"genomic" AND "selection"', pages = 1:40, crawl_delay = 1.2, useragent)
scrape_gs <- function(term, pages, crawl_delay, ...) {
#modified from Claudiu C. Papasteri's scrape_gs function
#https://claudiu.psychlab.eu/post/automated-systematic-literature-search-with-r-google-scholar-web-scraping/
#COPYRIGHT (2023) Claudiu C. Papasteri (MIT License)  
  gs_url_base <- "https://scholar.google.com/scholar"

  # set httr config outside of function and use them inside ...; e.g.:
  # useragent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") # spoof user agent
  # proxy <- httr::use_proxy(url = "proxy.com", port = 8080, username = "dave", password = "pass", auth = "basic")

  result_list <- list()
  i <- 1

  for (n_page in (pages - 1)*10) {  # gs page indexing starts with 0; there are 10 articles per page, see "?start=" param
    gs_url <- paste0(gs_url_base, "?start=", n_page, "&q=", noquote(gsub("\\s+", "+", trimws(term))))
    t0 <- Sys.time()
    session <- rvest::session(gs_url, ...)  # session
#options$useragent
    t1 <- Sys.time()
    response_delay <- as.numeric(t1-t0)  # backing off time
    wbpage <- rvest::read_html(session)

    # Avoid HTTP error 429 due to too many requests - use crawl delay & back off
    Sys.sleep(crawl_delay + 3*response_delay + runif(n = 1, min = 0.5, max = 1))
    if((i %% 10) == 0) {  # sleep every 10 iterations
      message("taking a break")
      Sys.sleep(10 + 10*response_delay + runif(n = 1, min = 0, max = 1))
    }
    i <- i + 1

    # Raw data
    titles <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rt"))
    authors_years <- rvest::html_text(rvest::html_elements(wbpage, ".gs_a"))
    part_abstracts <- rvest::html_text(rvest::html_elements(wbpage, ".gs_rs"))
    bottom_row_nodes <- rvest::html_elements(wbpage, ".gs_fl")
    bottom_row_nodes <- bottom_row_nodes[!grepl("gs_ggs gs_fl", as.character(bottom_row_nodes), fixed = TRUE)] # exclude the ones with this tag, they are download links
    bottom_row <- rvest::html_text(bottom_row_nodes)

    # Processed data
    authors <- gsub("^(.*?)\\W+-\\W+.*", "\\1", authors_years, perl = TRUE)
    years <- gsub("^.*(\\d{4}).*", "\\1", authors_years, perl = TRUE)
    citations <- strsplit(gsub("(?!^)(?=[[:upper:]])", " ", bottom_row, perl = TRUE), "  ")  # split on capital letter to get Number of citations link
    citations <- lapply(citations, "[", 3)
    n_citations <- suppressWarnings(as.numeric(sub("\\D*(\\d+).*", "\\1", citations)))

    # Store in list
    result_list <- append(
      result_list,
      list(
        list(
          page = n_page/10 + 1,
          term = term,
          title = titles,
          authors = authors,
          year = years,
          n_citations = n_citations,
          abstract = part_abstracts
        )
      )
    )
  }

  # Return as data frame
  result_df <- lapply(result_list, as.data.frame)
  result_df <<- as.data.frame(do.call(rbind, result_df))
}
