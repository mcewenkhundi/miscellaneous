select(linelist, where(~ sum(unique(.x) %in% c("yes","no"), na.rm = TRUE)==2))
