split_text_into_lines <- function(text, max_length) {
  words <- unlist(strsplit(text, " "))
  lines <- c()
  current_line <- ""
  
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 <= max_length) {
      current_line <- paste0(current_line, ifelse(nchar(current_line) > 0, " ", ""), word)
    } else {
      lines <- c(lines, current_line)
      current_line <- word
    }
  }
  
  lines <- c(lines, current_line) # Add the last line
  return(paste(lines, collapse = "<br>"))
}
