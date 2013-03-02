ringbuffer = function(size=100, warn=FALSE) {
  buf = new.env()
  buf$warn = warn
  buf$max.size = size
  buf$buffer = c()
  buf$rh = 0
  buf$wh = 0
  
  buf$read = function(n=1) {
    aa = min(n,length(buf$buffer))
    
    if (aa <= 0 & buf$warn) {
      warning("length of items to read must be greater than zero")
    } else if (aa < n & buf$warn) {
      warning("buffer underflow: attempt to read more than buffer holds")
    }
    
    bb = max(n,length(buf$buffer))
    if (bb > length(buf$buffer)) {
      bb = length(buf$buffer)
    }
    
    result = buf$buffer[1:aa]
    
    if (aa == bb) {
      buf$buffer = c()
    } else {
      buf$buffer = buf$buffer[max(1,aa+1):bb]      
    }
    
    return(result)
  }

  buf$write = function(items=c()) {
    if (length(items) <= 0 * buf$warn) {
      warning("length of list of items to write must be greater than zero")
    }
    
    
    buf$buffer = c(buf$buffer, items)
    if (length(buf$buffer) > buf$max.size) {
      if (warn) {
        warning("buffer overflow: attempt to write more than buffer holds")
      }
      bb = length(buf$buffer)
      aa = 1+(bb - buf$max.size)
      buf$buffer = buf$buffer[aa:bb]
    }
  }
  
  buf$readable = function() {
    return(length(buf$buffer))
  }
  
  buf$writable = function() {
    aa = length(buf$buffer)
    if (aa < buf$max.size) {
      return(buf$max.size - aa)
    } else {
      return(0)
    }
  }
  
  buf$peek = function() {
    return(buf$buffer)
  }
  
  return(buf)
}
