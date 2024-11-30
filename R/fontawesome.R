




##' list fontawesome fonts (families)
##'
##'
##' @title list.fafonts
##' @return fontawesome font files
##' @export
##' @author abs
list.fafonts <- function() {
  efproto$list_fonts(type = 'fontawesome')
}


##' search fontawesome
##'
##'
##' @title search_fontawesome
##' @param str string text
##' @param approximate logical
##' @return corresponding aliases
##' @export
##' @author ygc
search_fontawesome <- function(str, approximate = FALSE) {
  efproto$search(
    str = str,
    type = 'aliases',
    approximate = approximate,
    font_data = fontawesome_data
  )
}


##' random fontawesome
##'
##'
##' @title sample_fontawesome
##' @param size a non-negative integer giving the number of items to choose
##' @param replace Should sampling be with replacement?
##' @return random fontawesome
##' @export
##' @examples
##' sample_fontawesome(3)
##' @author guangchuang yu
sample_fontawesome <- function(size, replace = FALSE) {
  sample(unlist(fontawesome_data$aliases), size, replace)
}

##' convert fontawesome aliases to text
##'
##'
##' @title fontawesome
##' @param aliases aliases
##' @return text
##' @export
##' @examples
##' fontawesome('fa-twitter')
##' @author ygc
fontawesome <- function(aliases) {
  mapper_(aliases, fontawesome_data)
}

##' load fontawesome
##'
##'
##' @title load.fontawesome
##' @param font font
##' @return NULL
##' @export
##' @author ygc
load.fontawesome <- function(font = "fontawesome-webfont.ttf") {
  fnts <- efproto$list_fonts("fontawesome")
  # purrr::walk(fnts, efproto$load_font, type='fontawesome')
  invisible(lapply(fnts, function(f)
    efproto$load_font(f, type = 'fontawesome')))
}


##' @importFrom utils read.delim
get_fontawesome_data <- function() {
  ## copy font table from:
  ## https://fortawesome.github.io/Font-Awesome/cheatsheet/
  ##
  ### This now gets done in the vignette for update_fontawesome.qmd
  y <- read.delim(pipe("pbpaste"), stringsAsFactors = F)
  fa <- gsub("(.*)fa.*", '\\1', y[, 1])
  html <- gsub(".*\\[(.*)\\].*", '\\1', y)
  
  aliases <- gsub(".*(fa.*)\\s+.*", '\\1', y)
  
  fontawesome_data <- data.frame(
    fa = fa,
    aliases = aliases,
    html = html,
    stringsAsFactors = FALSE
  )
  return(fontawesome_data)
}





## example
## library(emojifont)
## load.fontawesome()
## set.seed(123)
## d = data.frame(x=rnorm(20),
##                y=rnorm(20),
##                z=sample(fontawesome(c('fa-weibo','fa-github', 'fa-twitter', 'fa-apple')), replace=T, 10))

## library(ggplot2)
## ggplot(d, aes(x, y, color=z)) + geom_text(aes(label=z), family='fontawesome-webfont', size=8)
