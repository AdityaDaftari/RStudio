# S4 Class definitions
setClass("LongitudinalData",
         slots = list(subjects = "list",
                      subject_factors = "numeric"))

setClass("subject",
         slots=list(id="numeric",
                    visit = "list",
                    visit_factors = "numeric"))

setClass("visit",
         slots=list(id="numeric",
                    visit="numeric",
                    room="list",
                    room_factors = "character"))

setClass("room",
         slots=list(id="numeric",
                    visit="numeric",
                    room="character",
                    values = "data.frame"))


# S4 generic functions declarations
setGeneric("make_LD", function(x){
  standardGeneric("make_LD")
})
setGeneric("subject", function(x,y){
  standardGeneric("subject")
})
setGeneric("visit", function(x,y){
  standardGeneric("visit")
})
setGeneric("room", function(x,y){
  standardGeneric("room")
})

# Function definitions
# subject(x, 44) %>% visit(0) %>% room("bedroom")
setMethod(f = "make_LD",
          signature =  c(x = "tbl_df"),
          definition =  function(x){
            subject_list <- split(x[,c("visit","room","value","timepoint")],x$id)
            new("LongitudinalData", subjects = subject_list, subject_factors = unique(x$id))
          })

setMethod(f = "subject",
          signature =  c(x="LongitudinalData",y="numeric"),
          definition =  function(x,y){
            if(!has_element(x@subject_factors,y)) return(NULL)
            factor_id <- as.character(y)
            visit_list <- split(x@subjects[[factor_id]][,c("room","value","timepoint")], x@subjects[[factor_id]]$visit)
            new("subject", id = y, visit = visit_list, visit_factors = unique(x@subjects[[factor_id]]$visit))
          })

setMethod(f = "visit",
          signature =  c(x="subject",y="numeric"),
          definition =  function(x,y){
            if(!has_element(x@visit_factors,y)) return(NULL)
            factor_visit <- as.character(y)
            room_list <- split(x@visit[[factor_visit]][,c("value","timepoint")], x@visit[[factor_visit]]$room)
            new("visit", id = x@id, visit = y, room = room_list, room_factors = unique(x@visit[[factor_visit]]$room))
          })

setMethod(f = "room",
          signature =  c(x="visit",y="character"),
          definition =  function(x,y){
            if(!has_element(x@room_factors,y)) return(NULL)
            factor_room <- as.character(y)
            new("room", id = x@id, visit = x@visit, room = y, values = x@room[[factor_room]])
          })

# generic print for LongitudinalData class
setGeneric("print")
setMethod(f = "print",
          signature =  c(x = "LongitudinalData"),
          definition =  function(x){
            paste("Longitudinal data with", length(x@subjects), "subjects")
          })

# generic print for subject class
setMethod(f = "print",
          signature =  c(x = "subject"),
          definition =  function(x){
            paste("Subject ID:", x@id)
          })

# generic print for room class
setMethod(f = "print",
          signature =  c(x = "room"),
          definition =  function(x){
            cat(paste("ID:", x@id), paste("Visit:", x@visit), paste("Room:", x@room), sep = "\n")
          })

# generic summary for subject class
setGeneric("summary")
setMethod(f = "summary",
          signature =  "subject",
          definition =  function(object, ...){
            rooms <- c()
            for(i in as.character(object@visit_factors))
            {
              rooms <- unique(c(rooms, unique(object@visit[[i]]$room)))
            }
            rooms <- sort(rooms)
            summary_df <- data.frame()
            for(i in as.character(object@visit_factors))
            {
              means <- numeric(length = length(rooms))
              j <- 1
              for (room in rooms) 
              {
                indices <- which(object@visit[[i]]$room == room)
                means[j] <- round(mean(object@visit[[i]]$value[indices]),6)
                j <- j+1
              }
              summary_df <- rbind(summary_df,c(i,means))
            }
            colnames(summary_df) <- c("visit",rooms)
            summary_df
          })

setMethod("summary",
          "room",
          function(object,...){
            summary(object@values$value)
          })