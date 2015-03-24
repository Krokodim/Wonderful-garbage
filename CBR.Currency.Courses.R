library(XML)
library(RCurl)


currency.courses <- function (currency = "EUR", date.start = Sys.Date() - 7,
                              date.end = Sys.Date() ) {
  
  currency.codes <- c(USD="R01235", EUR="R01239")
  
  url_template <- paste0(
    "http://www.cbr.ru/currency_base/dynamics.aspx",  # page url
    "?VAL_NM_RQ=%s",                                  # currency code
    "&date_req1=%s",                                  # start date
    "&date_req2=%s",                                  # end date
    "&rt=1&mode=1"
  )
  
  currency.code <- currency.codes[which(names(currency.codes) == currency)]
  if(class(date.start) == "Date") date.start <- format(date.start, "%d.%m.%Y")
  if(class(date.end) == "Date") date.end <- format(date.end, "%d.%m.%Y")
  
  
  url <- getURL(sprintf(url_template, currency.code, date.start, date.end))
  
  html <- htmlTreeParse(url, useInternalNodes=TRUE)
  
  data.raw <- xpathSApply(html, "//table[@class='data']//td", xmlValue)
  
  data <- data.frame(
    date   = as.Date(data.raw[seq(1,length(data.raw),by=3)], format="%d.%m.%Y"),
    course = as.numeric(sub(",",".",data.raw[seq(3,length(data.raw),by=3)]))
  )
}

pp <- par(mfrow=c(1,2))
plot(currency.courses("USD","01.01.2014"), type="l", col="red", main="Euro course")
plot(currency.courses("EUR","01.01.2014"), type="l", col="blue",main="US dollar course")
par(pp)
