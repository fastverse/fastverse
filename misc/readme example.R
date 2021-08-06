
reprex::reprex({
  
options(crayon.enabled = TRUE)
  old_hooks <- fansi::set_knit_hooks(knitr::knit_hooks,
                                     which = c("output", "message", "error"))

# Loads and attaches the core fastverse packages
library(fastverse)

# Permanently extends the core fastverse by certain packages
fastverse_extend(xts, roll, dygraphs, permanent = TRUE)

# If the fastverse is now loaded in a new session, these packages are added 
fastverse_detach(session = TRUE)
library(fastverse)

# We can also extend only the fastverse for the session, here adding Rfast2
# and any installed suggested packages for date-time manipulation
fastverse_extend(Rfast2, topics = "DT") 

# This shows a situation report of the fastverse, including all dependencies
fastverse_sitrep(recursive = TRUE)

# Resets the fastverse to defaults, removing any permanent modifications
fastverse_reset()

})