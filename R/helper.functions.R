################################################################################
##
## $Id: helper.functions.R 1300 2008-08-27 21:01:11Z liu $
##
## Internal helper functions
##
################################################################################


## since characters don't subtract from each other....
txsub <- function(x,y) {
  as.character(as.numeric(x) - as.numeric(y))
}

## This formats text for us. Type "p" means that its a price, adds
## commas and has two decimal digits. "s" means that its a size, adds
## commas and no decimal digits.

.prettify <- function(x, type = "p"){
    if(type == "p"){
        x <- formatC(x, format = "f", big.mark = ",", digits = 2)
        invisible(x)
    } else if(type == "s"){
        x <- formatC(x, format = "d", big.mark = ",")
        invisible(x)
    }
}


## Helper function that returns a data frame with the size aggregated
## by price level and with data above 10% on either side of the
## midpoint removed.  Takes an orderbook object as input. Returns
## orderbook object with price, size, and type. Mainly needed for
## plotting.

.combine.size <- function(object, bounds){
    x <- object@current.ob
    ## Save the midpoint
    mid <- mid.point(object)
    ## Removes rows 10% above and below the midpoint.
    x <- x[x[["price"]] < mid*(1 + bounds) &
           x[["price"]] > mid*(1 - bounds),]
    ## Aggregate by price
    x <- aggregate(x[["size"]], by = list(x[["price"]]), sum)
    names(x) <- c("price", "size")
    ## Rows with price above midpoint are ask, price below midpoint
    ## are bid.
    x$type[x[["price"]] > mid] = "ASK"
    x$type[x[["price"]] < mid] = "BID"
    return(x)
}

## Creates a new current.ob from ob.data. Takes in the object, returns
## the object with an updated current.ob.

.update <- function(ob)
{
    x <- copy(ob@ob.data)
    skip = 5
    ## Turn hash into a list. Unlist into a vector. Remove
    ## names. Vector is currently id,time,type,size,price repeated
    ## over and over
    x <- as.list(x)
    x <- unlist(x, use.names = FALSE)
    ## Get out length. Use sequence to pull out the proper values.
    len <- length(x)
    price <- as.numeric(x[seq(3, len, skip)])
    size <- as.numeric(x[seq(4, len, skip)])
    type <- as.factor(x[seq(5, len, skip)])
    time <- as.numeric(x[seq(1, len, skip)])
    id <- as.character(x[seq(2, len, skip)])

    x <- data.frame(price, size, type, time, id,
                    stringsAsFactors = FALSE)
    names(x) <- c("price", "size", "type", "time", "id")

    ob@current.ob <- x
    invisible(ob)
}


## Returns the row number of the first order after the specified time.

.get.time.row <- function(file, n, skip = 1){
    ## Open the file connection
    file <- file(file, open="r")
    ## Skip to wherever the other function told it to skip to in the
    ## data file.
    x <- scan(file, nlines = 1, sep = ",", what = "character",
              quiet = TRUE, skip = skip)
    ## Increment because now we are at the first line after skip.
    i <- skip + 1
    ## As long as there are still entries, and we haven't found a time
    ## greater than the time we are looking for, keep going.
    while(!identical(length(x), 0) && as.numeric(x[2]) <= n){
        x <- scan(file, nlines = 1, sep = ",", what = "character",
                  quiet = TRUE)
        i <- i + 1
    }
    close(file)
    return(i)
}

## Returns the time of a row number
## this doesn't seem to be called anywhere -SCL
.get.row.time <- function(file, n){
    ## Open the file connection
    file <- file(file, open="r")
    ## Skip to 1 before the row in question, then read the line
    x <- scan(file, nlines = 1, sep = ",", what = "character",
              quiet = TRUE, skip = n - 1)
    ## Close the connection
    close(file)
    ## Return the time
    return(as.numeric(x[2]))
}

## Returns the row number of the next trade after the current
## time. Pretty much the same as the above function, except we look
## for "T".
## this will need some kind of fixing -gets called in next.trade method -SCL
.get.next.trade <- function(file, n){
    file <- file(file, open="r")
    x <- scan(file, nlines = 1, sep = ",", what = "character",
              quiet = TRUE, skip = n)
    n <- n + 1
    while(!identical(length(x), 0) & !isTRUE(x[1] %in% "T")){
        x <- scan(file, nlines = 1, sep = ",", what = "character",
                  quiet = TRUE)
       	n <- n + 1
    }
    close(file)
    return(n)
}




## Takes in object and number of lines of the data file to be
## read. Returns an object with updated ob.data, current.ob,
## trade.data, my.trades, file.index, and current.time.
## keeping around for history sake.
.read.orders <- function(object, n){
    ob <- copy(object)
    ## Pull out current values
    file <- ob@file
    file.index <- ob@file.index
    ob.data <- ob@ob.data
    trade.data <- ob@trade.data
    max = 6
    ## Open file connection. Skip to the current place in the file and
    ## read in the first line after that.
    file <- file(file, open = "r")
    x <- scan(file, nlines = 1, sep = ",", what = "character", quiet =
              TRUE, skip = file.index)
    ## While there are still lines to read and less than n lines have
    ## been read.
    i <- 0

    while(!identical(length(x), 0) & i < n){
        ## If there is an add change current position, add something
        ## into ID, and increment current position.
        if (isTRUE(x[1] %in% "A")){
            ob.data[x[3]] <- x[2:max]
        }

        ## For a cancel remove the row from ob.data, remove the ID
        ## from list.
        if (isTRUE(x[1] %in% "C")){
            ob.data[x[3]] <- NULL
        }

        ## partial cancel, subtract the position
        if (isTRUE(x[1] %in% "P")){
          ttmp <- txsub(ob.data[[x[3]]][4], x[4])
          if(ttmp<=0) {
            ob.data[x[3]] <- NULL
            warning(paste("should this have been a full cancel",x[3],x[4]))
          } else {
            ob.data[[x[3]]][4] <- ttmp
          }
        }
        
        ##  partial cross, subtract the position & append the trade
        ## 
        if (isTRUE(x[1] %in% "E")){
          ttmp <- txsub(ob.data[[x[3]]][4], x[4])
          if(ttmp<=0) {
            ob.data[x[3]] <- NULL
            warning(paste("should this have been a full cross",x[3],x[4],
                          "omit!"))
          } else {
            ob.data[[x[3]]][4] <- ttmp
            xo <- ob.data[[x[3]]]
            ## got to get the trade price and size from the hash, not here
            trade.data <- append(trade.data, file.index + i + 1)
            trade.data <- append(trade.data, c(x[2:3],xo[3]))
          }
        }
        
        ## full cross
        ## 
        if (isTRUE(x[1] %in% "F")) {
          ## got to get the trade price from the hash, not here
          xo <- ob.data[[x[3]]]
          ob.data[x[3]] <- NULL
          trade.data <- append(trade.data, file.index + i + 1)
          trade.data <- append(trade.data, c(x[2:3],xo[3],xo[4]))
        }
        
        ## For a replace find the right row and replace it with the
        ## new size.
        if (isTRUE(x[1] %in% "R")){
            ob.data[[x[3]]][4] <- x[4]
        }
        ## For a trade increment the trade index and store the trade
        ## data.
        if (isTRUE(x[1] %in% "T")){
            trade.data <- append(trade.data, file.index + i + 1)
            trade.data <- append(trade.data, x[2:(max - 1)])
        }
        ## Increase i
        i <- i + 1
        ## Read in the next line.
        x <- scan(file, nlines = 1, sep = ",", what = "character",
                  quiet = TRUE)
    } ## end while
    
    close(file)
    ob@ob.data <- ob.data
    ob@file.index <- file.index + i
    ob@trade.data <- trade.data
    ob@current.time <- as.numeric(x[2])
    ## Run update to create a new current.ob from the new ob.data.
    ob = .update(ob)
    invisible(ob)
}





## Converts x to a time. x should be milliseconds since midnight
## UTC. Returns as "H:M:S".

.to.time <- function(x){
    x <- as.POSIXct(x/1000+14400, origin = Sys.Date())
    return(format(x, format = "%H:%M:%S"))
}

## Converts x to milliseconds. x should be a string, e.g. "5:01:02"
## means 5AM, 1 minute, 2 seconds.

.to.ms <- function(x){
    x <- strsplit(x, split = ":")[[1]]
    x <- ((as.numeric(x[1])) * 3600000
          + as.numeric(x[2]) * 60000
          + as.numeric(x[3]) * 1000)
    return(signif(x, 8))
}

## Midpoint Return, automatically finds the midpoint return for the
## selected message row number for a vector of time in seconds,
## e.g. c(5, 10, 60, 120) means find the midpoint return for 5s, 10s,
## 1 min, 2 min after the trade.

.midpoint.returns <- function(object, trdprice, trdrow, time){
    ## Now the orderbook is at the start order
    tmp.ob <- copy(object)
    tmp.ob <- read.orders(tmp.ob, trdrow - tmp.ob@file.index)
    ## Create a vector with the current time of the orderbook at that
    ## order number added to the times in the vector
    time <- tmp.ob@current.time + time * 1000
    rows <- sapply(time, function(x){.get.time.row(tmp.ob@file, x)})
    ## Find the first midpoint
    mid <- mid.point(tmp.ob)
    midpoints <- vector()
    for(i in 1:length(rows)){
        tmp.ob <- read.orders(tmp.ob, rows[i] - tmp.ob@file.index)
        midpoints[i] <- mid.point(tmp.ob)
    }
    if(mid > trdprice) {
      return(midpoints - trdprice)
    } else {
      return(trdprice - midpoints)
    }
}



kane.tl <- function(x) {
  ## translates the TP format to the old x[2:6] values to preserve order
  ## TP: Time,Ticker,Order,T,Shares,Price,MPID,X
  ## old: type,time,id,price,size,type 
  c(x[1],x[3],x[6],x[5],switch(x[4], B="BID",S="ASK"))
}

## Takes in object and number of lines of the data file to be
## read. Returns an object with updated ob.data, current.ob,
## trade.data, my.trades, file.index, and current.time.
.read.orders.tp <- function(object, n){
    ob <- copy(object)
    ## Pull out current values
    file <- ob@file
    file.index <- ob@file.index
    ob.data <- ob@ob.data
    trade.data <- ob@trade.data
    ## Open file connection. Skip to the current place in the file and
    ## read in the first line after that.
    file <- file(file, open = "r")
    x <- scan(file, nlines = 1, sep = ",", what = "character", quiet =
              TRUE, skip = file.index)
    ## While there are still lines to read and less than n lines have
    ## been read.
    i <- 0

    while(!identical(length(x), 0) & i < n){
        ## If there is an add change current position, add something
        ## into ID, and increment current position.
        if (isTRUE(x[4] %in% c("B","S"))){
            ob.data[x[3]] <- kane.tl(x)
        }

        ## For a cancel remove the row from ob.data, remove the ID
        ## from list.
        if (isTRUE(x[4] %in% "D")){
            ob.data[x[3]] <- NULL
        }

        ## partial cancel, subtract the position
        if (isTRUE(x[4] %in% "C")){
          ttmp <- txsub(ob.data[[x[3]]][4], x[5])
          if(ttmp<=0) {
            ob.data[x[3]] <- NULL
            warning(paste("should this have been a full cancel",x[3],x[4]))
          } else {
            ob.data[[x[3]]][4] <- ttmp
          }
        }
        
        ##  partial cross, subtract the position & append the trade
        ## 
        if (isTRUE(x[4] %in% "E")){
          ttmp <- txsub(ob.data[[x[3]]][4], x[5])
          if(ttmp<=0) {
            ob.data[x[3]] <- NULL
            warning(paste("should this have been a full cross",x[3],x[4],
                          "omit!"))
          } else {
            ob.data[[x[3]]][4] <- ttmp
            xo <- ob.data[[x[3]]]
            ## got to get the trade price and size from the hash, not here
            trade.data <-  rbind(trade.data,c((file.index+1+i),x[1],xo[3],xo[4],1))
          }
        }
        
        ## full cross
        ## 
        if (isTRUE(x[4] %in% "F")) {
          ## got to get the trade price from the hash, not here
          xo <- ob.data[[x[3]]]
          ob.data[x[3]] <- NULL
          trade.data <-  rbind(trade.data,c((file.index+1+i),x[1],xo[3],xo[4],2))
        }
        
        ## For a replace find the right row and replace it with the
        ## new size.
        if (isTRUE(x[4] %in% "R")){
            ob.data[[x[3]]][4] <- x[5]
        }
        ## For a trade increment the trade index and store the trade
        ## data. Skip "X" case, since it doesn't print size and the OrderID
        ## doesn't correspond to anything
        if (isTRUE(x[4] %in% "T")){
          trade.data <-  rbind(trade.data,c((file.index+1+i),x[1],x[6],x[5],0))
        }
        ## Increase i
        i <- i + 1
        ## Read in the next line.
        x <- scan(file, nlines = 1, sep = ",", what = "character",
                  quiet = TRUE)
    } ## end while
    
    close(file)
    ob@ob.data <- ob.data
    ob@file.index <- file.index + i
    ob@trade.data <- trade.data
    ob@current.time <- as.numeric(x[1])
    ## Run update to create a new current.ob from the new ob.data.
    ob = .update(ob)
    invisible(ob)
}
