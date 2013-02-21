ringbuffer <-
function(size=10,warn=FALSE) {
  buf = new.env()
  buf$buf = rep(NA,size)
  buf$size = size
  buf$wh = 1
  buf$rh = 1
  buf$warn = warn
  
  buf$write = function(input) {
    wh = buf$wh %% buf$size
        
    in.size = length(input)
    if (in.size > buf$size) { #truncate to fit in buffer, if necessary
      if (buf$warn) {
        warning("buffer overflow (1)") 
        warning(in.size, buf$size)
      }
      input = input[(in.size-buf$size+1):in.size]
      in.size = length(input)
    } else if (buf$writable() < in.size) {
      if (buf$warn) {
        warning("buffer overflow (2)")
      }
    }

    tail.size = buf$size - wh + 1 #how much to be written to tail
    head.size = wh - 1 #how much to be written to head

    if (wh == 0) { #corner case
      buf$buf[buf$size] = input[1]
      if (in.size > 1) {
        buf$buf[1:(in.size-1)] = input[2:in.size]
      }
    } else if (in.size <= tail.size) {
      buf$buf[wh:(wh+in.size-1)] = input[1:in.size]      
    } else {
      buf$buf[wh:buf$size] = input[1:tail.size]
 
      remainder = in.size - tail.size
      buf$buf[1:remainder] = input[(tail.size+1):in.size]
    }
    buf$wh = (buf$wh + in.size)
  }
  
  buf$read = function(size=1) {
    rh = buf$rh %% buf$size
    wh = buf$wh %% buf$size
    
    if (size > buf$size) {
      if (buf$warn) {
        warning("buffer underflow: read more than buffer holds")
      }
    } else if (size > buf$wh - buf$rh) {
      if (buf$warn) {
        warning("buffer underflow: read past write head (1)")
      }
    } else if (buf$readable() < size) {
      if (buf$warn) {
        warning("buffer underflow: read past write head (2)")      
      }
    }
    
    if (size <= buf$size - rh + 1) {
      result = buf$buf[rh:(rh+size-1)]
      #buf$buf[buf$rh:(buf$rh+size-1)] = NA
      buf$rh = buf$rh + size
      return(result)
    } else {
      result = c(buf$buf[rh:buf$size], buf$buf[1:(size - length(rh:buf$size))])
      buf$rh = buf$rh + size
      return(result)
    }
  }
  
  buf$peek = function() {
    rh = buf$rh %% buf$size
    wh = buf$wh %% buf$size
    
    if (rh == 1 && buf$wh - 1 <= buf$size) {
      return(buf$buf[1:(buf$wh-1)])
    } else {
      if ((buf$wh-1) %% buf$size == 0) {
        return(buf$buf[wh:buf$size])
      } else {
        result = c(buf$buf[wh:buf$size], buf$buf[1:((buf$wh-1) %% buf$size)])
        return(result[1:buf$size])
      }
    }
  }
  
  buf$writable = function() {
    if(buf$wh == buf$rh | (buf$wh > buf$rh & buf$wh - buf$rh < buf$size)) {
      return(buf$size - (buf$wh - buf$rh))
    } else {
      return(0)
    }
  }
  
  buf$readable = function() {
    if(buf$rh < buf$wh) {
      return(buf$wh - buf$rh)
    } else {
      return(0)
    }
  }
  return(buf)
}
