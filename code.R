library(scholar)   # for google scholar
library(curl)      # for publons API connections
library(rjson)     # to read publons data
library(ggplot2)   # to graph everything
library(ggtext)    # for a very small graphical details
                   # (text of different colours in the secondary y-axis)

## google scholar ID
id <- "mRc0hxsAAAAJ"

## get google scholar data
ct <- get_citation_history(id)
pb <- get_publications(id)
pr <- get_profile(id)

## prepare google scholar data for graph
pb <- as.data.frame( table(pb$year) )
colnames( pb ) <- c("year", "publications")
pb$year <- as.numeric( as.character( pb$year ) )


## publons data
# this is the curl call:
# curl -X GET https://publons.com//api/v2/academic/review/?academic=2654061 -H "Authorization: Token YOUR AUTHORIZATION TOKEN" -H "Content-Type: application/json"
# you can get the authorization token here: https://publons.com/api/v2/

## the rationale:
## because I can read only a page of 10 reviews per time,
## I will read the first page for the data and the total number
## of revisions (variable count).
## In this way, I know how many pages I should read ( ceiling(count / 10 ) )

## reading data from publons API:
h <- new_handle()
handle_setheaders(h,
                  "Authorization" = "Token YOUR AUTHORIZATION TOKEN",
                  "Content-Type" = "application/json"
)
req <- curl_fetch_memory("https://publons.com//api/v2/academic/review/?academic=2654061&page=1",
                         handle = h)
tmp <- jsonlite::prettify(rawToChar(req$content))
tmp1 <- fromJSON(tmp)

count <- tmp1$count

reviews <- c()

for( j in 1:ceiling( count / 10 ) ){
  
  if( i > 1 ){
    req <- curl_fetch_memory(paste0("https://publons.com//api/v2/academic/review/?academic=2654061&page=", j),
                             handle = h)
    tmp <- jsonlite::prettify(rawToChar(req$content))
    
    tmp1 <- fromJSON(tmp)
  }
  
  for(i in 1:10){
    reviews <- c( reviews, tmp1$results[i][[1]]$date_reviewed )
  }
}

## preparing publons data for graph
reviews <- as.data.frame(table(reviews))
colnames(reviews)[1] <- "year"
reviews$year <- as.numeric( as.character( reviews$year ) )

dat_all <- data.frame(
  y = c(pb$publications * 10,
        ct$cites,
        reviews$Freq * 10),
  year = c(pb$year,
           ct$year,
           reviews$year),
  type = c(
    rep( times = nrow(pb), "publications"),
    rep( times = nrow(ct), "citations"),
    rep( times = nrow(reviews), "reviews")
  )
)

ggplot( dat_all ) +
  geom_line( aes(x = year, y = y, colour = type) ) +
  geom_point( aes(x = year, y = y, colour = type), size = 2 ) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10,
                                         name = "publications and reviews"))+
  scale_x_continuous( breaks = dat_all$year)+
  xlab("")+
  labs(caption = paste("Data retrieved from Google Scholar and Publons -",
                       date() ),
       title = "Michele Scandola",
       subtitle = paste0("h-Index = ", pr$h_index,
                         "; i10-Index = ", pr$i10_index,
                         "; tot citations = ", pr$total_cites,
                         "; tot reviews =", count),
       y = "citations")+
  theme(legend.title = element_blank())
