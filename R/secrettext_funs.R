#' Encrypt a message
#'
#' Encrypt a character string given two numeric keys.
#' @param text A string
#' @param key1 A numeric value between 1 and 25
#' @param key2 A numeric value between 1 and 25
#' @return A string, converted to lowercase and encrypted
#' @importFrom stringr str_split_fixed str_length str_to_lower
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate bind_rows arrange lag filter mutate_all pull between
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' setcode("hello world", 5, 16)
setcode <- function(text, key1, key2)
{
  if (length(text) > 1) {
    stop("text must be length 1")
  } else {
    if (!is.character(text)) {
      stop("text must be a character string")
    } else {
      if (is.numeric(key1) &
          between(key1, 1, 25) != TRUE &
          as.integer(key1) == key1) {
        stop("key1 must be an integer between 1 and 25")
      } else {
        if (is.numeric(key2) &
            between(key2, 1, 25) != TRUE &
            as.integer(key2) == key2) {
          stop("key2 must be an integer between 1 and 25")
        } else {
          alpha <- data.frame(chars = rep(letters, 2)) %>%
            mutate(chars_use = lag(.data$chars, key1)) %>%
            drop_na() %>%
            arrange(.data$chars) %>%
            unique() %>%
            mutate_all(as.character)

          text_vec <- as.vector(str_to_lower(str_split_fixed(
            text,
            pattern = "",
            n = str_length(text)
          )))

          for (i in which(text_vec %in% letters))
          {
            text_vec[i] <- alpha %>%
              filter(.data$chars == text_vec[i]) %>%
              unique() %>%
              pull(.data$chars_use)

            alpha <- alpha %>%
              bind_rows(alpha) %>%
              mutate(chars_use = lag(.data$chars_use, key2)) %>%
              drop_na() %>%
              unique() %>%
              arrange(.data$chars_use)
          }
          return(paste(text_vec, collapse = ''))
        }
      }
    }
  }
}

#' Decrypt a Message
#'
#' Decrypt a character string generated with setcode() given two numeric keys.
#' @param text A string
#' @param key1 A numeric value between 1 and 25
#' @param key2 A numeric value between 1 and 25
#' @return A string, converted to lowercase and decrypted
#' @importFrom stringr str_split_fixed str_length str_to_lower
#' @importFrom tidyr drop_na
#' @importFrom dplyr mutate bind_rows arrange lead filter mutate_all pull between
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' # string argument as output of setcode()
#' decode(setcode("hello world", 5, 16), 5, 16)
#' # string argument as user defined character string
#' decode("cjakx preik", 5, 16)
decode <- function(text, key1, key2)
{
  if (!is.character(text)) {
    stop("text must be a character string")
  } else {
    if (length(text) > 1) {
      stop("text must be length 1")
    } else {
      if (is.numeric(key1) &
          between(key1, 1, 25) != TRUE &
          as.integer(key1) == key1) {
        stop("key1 must be an integer between 1 and 25")
      } else {
        if (is.numeric(key2) &
            between(key2, 1, 25) != TRUE &
            as.integer(key2) == key2) {
          stop("key2 must be an integer between 1 and 25")
        } else {
          alpha <- data.frame(chars = rep(letters, 2)) %>%
            mutate(chars_use = lead(.data$chars, key1)) %>%
            drop_na() %>%
            arrange(.data$chars) %>%
            unique() %>%
            mutate_all(as.character)

          text_vec <- as.vector(str_to_lower(str_split_fixed(
            text,
            pattern = "",
            n = str_length(text)
          )))

          for (i in which(text_vec %in% letters))
          {
            text_vec[i] <- alpha %>%
              filter(.data$chars == text_vec[i]) %>%
              unique() %>%
              pull(.data$chars_use)

            alpha <- alpha %>%
              bind_rows(alpha) %>%
              mutate(chars_use = lead(.data$chars_use, key2)) %>%
              drop_na() %>%
              unique() %>%
              arrange(.data$chars_use)
          }

          return(paste(text_vec, collapse = ''))
        }
      }
    }
  }
}
