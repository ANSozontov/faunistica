geo_check <- function(g){
# Basic check -------------------------------------------------------------
    errors <- character()
    if(stringr::str_detect(g, "[:alpha:]")) {
        errors <- c(errors, "Буквы в координатах широты")
    }
    
    g <- stringr::str_replace_all(g, "\\,", ".")
    if(stringr::str_detect(g, "[:space:]")){
        g <- stringr::str_replace_all(g, "[:space:]", "")
        # errors <- c(errors, "Пробелы в координатах")
    }
    
    if(stringr::str_detect(stringr::str_replace_all(g, "\\.|°|\'|\\\"|`", ""), "[:punct:]|[:symbol:]")) {
        errors <- c(errors, "Недопустимые символы в координатах широты")
    }
    g <- stringr::str_replace_all(g, "`", "\\'")
    g <- stringr::str_replace_all(g, "\\'\\'", "\\\"")
    if(stringr::str_count(g, "\\.") > 1) {
        errors <- c(errors, "Слишком много разделителей (точек / запятых)")
    } else if(stringr::str_count(g, ".") < 1) {
        errors <- c(errors, "Нет разделителей или недостаточная точность координат")
    }

# Order of symbols --------------------------------------------------------
    punctorder <- c(stringr::str_extract_all(g, "\\.|°|\'|\\\"", simplify = TRUE))
    
    if(length(which(punctorder == "°")) > 0 &&
       length(which(punctorder == "\\.")) > 0 &&
       which(punctorder == "°") > which(punctorder == "\\."))
    {errors <- c(errors, "Перепутан порядок градуса и минуты")}
    if(length(which(punctorder == "°")) > 0 &&
       length(which(punctorder == '\\"')) > 0 &&
       which(punctorder == "°") > which(punctorder == '\\"'))
    {errors <- c(errors, "Перепутан порядок градуса и секунды")}
    if(length(which(punctorder == "\\'")) > 0 &&
       length(which(punctorder == '\\"')) > 0 &&
       which(punctorder == "\\'") > which(punctorder == '\\"'))
    {errors <- c(errors, "Перепутан порядок минуты и секунды")}

# recognition -------------------------------------------------------------
    if(length(errors) < 1){
        g <- stringr::str_split(g, "[^a-zA-Z0-9\\.]")[[1]]
        g <- g[nchar(g)>0]
        if(length(g) == 1){
            #type 1
            coord_raw <- paste0(g, "°")
            coord <- as.numeric(g)
        } else if(length(g) == 2){
            #type 2
            coord_raw <- paste0(g[1], "°", g[2], "'")
            coord <- g |> 
                as.numeric()
            if(coord[2]>=60){
                coord <- NA
                errors <- c(errors, "Ошибка в минутах: >60")
            } else {
                coord <- coord |> 
                    purrr::map2_dbl(.y = c(1, 60), .f = `/`) |> 
                    sum()
            }
        } else if(length(g) == 3){
            #type 3
            coord_raw <- paste0(g[1], "°", g[2], "'", g[3], '"')
            coord <- g |> 
                as.numeric()
            if(coord[2]>=60){
                coord <- NA
                errors <- c(errors, "Ошибка в минутах: >60")
            } else if(coord[3]>=60){
                coord <- NA
                errors <- c(errors, "Ошибка в секундах: >60")
            } else {
                coord <- coord |> 
                    purrr::map2_dbl(.y = c(1, 60, 3600), .f = `/`) |> 
                    sum()
            }
        } else {
            # troubles
            coord_raw <- g
            coord <- NA
            errors <- c(errors, "Проблемы с разделителями")
            errors <- c(errors, "Координаты не распознаны")
        }
    } else {
        # troubles
        coord_raw <- g
        coord <- NA
        errors <- c(errors, "Координаты не распознаны")
    }
    if(!is.na(coord) && (coord > 180 | coord < -180)){
        coord <- NA
        errors <- c(errors, "Координаты выходят за пределы допустимого")
    }
    return(list(
        coord_raw = coord_raw,
        coord = coord,
        errors= errors
    ))
}
