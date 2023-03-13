#convert to ggml


bytes_to_unicode <- function() {
  # Create a list of utf-8 byte values
  bs <- c(seq(from = charToRaw("!"), to = charToRaw("~")), 
          seq(from = charToRaw("¡"), to = charToRaw("¬")), 
          seq(from = charToRaw("®"), to = charToRaw("ÿ")))
  
  # Create a copy of the byte values list
  cs <- bs
  
  # Add additional byte values to bs and corresponding integer values to cs
  n <- 0
  for (b in 0:255) {
    if (!(b %in% bs)) {
      bs <- c(bs, b)
      n <- n + 1
      cs <- c(cs, 256 + n)
    }
  }
  
  # Convert cs list to unicode string list
  cs <- rawToChar(as.raw(cs))
  
  # Create a dictionary mapping byte values to unicode strings
  dictionary <- setNames(cs, bs)
  
  return(dictionary)
}


#the function should take at least 4 arguments, if not up to 4, exit.


byte_to_unicode <- function(x, from = "ISO-8859-1", to = "UTF-8") {
  return(iconv(x, from = from, to = to))
}
x <- rawToChar(as.raw(c(72, 101, 108, 108, 111))) # "Hello"
unicode_string <- byte_to_unicode(x, from = "ASCII", to = "UTF-8")
cat(unicode_string) 


library(stringi)

# Define a sample text to tokenize
text <- "This is a sample text for BPE tokenization."

# Define the number of BPE operations to perform
num_operations <- 100

# Convert the text to UTF-8 byte sequence
byte_seq <- stri_encode(text, "UTF-8")

# Create a vocabulary of unique UTF-8 bytes
vocab <- unique(byte_seq)

# Convert the vocabulary to a list of Unicode strings
unicode_vocab <- lapply(vocab, function(x) stri_unescape_unicode(paste0("\\u", strtoi(charToRaw(x), base = 16))))

# Perform BPE operations on the text
for (i in 1:num_operations) {
  # Define a list of possible merges based on the current vocabulary
  merge_list <- expand.grid(vocab, vocab)
  merge_list <- merge_list[merge_list[, 1] != merge_list[, 2], ]
  
  # Calculate the scores of each merge based on their frequency in the text
  scores <- apply(merge_list, 1, function(x) sum(grepl(paste0(x[1], x[2]), byte_seq)))
  best_score <- max(scores)
  
  # Find the best merge and update the vocabulary accordingly
  if (best_score > 0) {
    best_merge <- merge_list[which(scores == best_score)[1], ]
    new_seq <- gsub(paste0(best_merge[1], best_merge[2]), paste0(best_merge[1], "_", best_merge[2]), byte_seq)
    vocab <- unique(stri_split_fixed(new_seq, "_", simplify = TRUE)[[1]])
    byte_seq <- new_seq
  } else {
    break
  }
}

# Convert the final vocabulary to a list of Unicode strings
final_unicode_vocab <- lapply(vocab, function(x) stri_unescape_unicode(paste0("\\u", strtoi(charToRaw(x), base = 16))))

# Return the byte sequence and Unicode string lists
result <- list(byte_seq = byte_seq, unicode_seq = final_unicode_vocab)
