
# Note: Important character values range from 32 through 126

# Functions:

encode_message = function(message, codeword){
  my_code = as.numeric(charToRaw(message))
  my_shift = as.numeric(charToRaw(tolower(codeword)))
  my_encoded_word = rep(0,length(my_code))
  L = length(my_shift)
  message_index = (1:length(my_code)) %% L 
  message_index[message_index == 0] = L
  for(ii in 1:L){
    my_encoded_word[message_index == ii] = (my_code[message_index == ii] + my_shift[ii] - 32) %% 95 + 32
  }
  cur_text = my_encoded_word
  for(ii in 1:length(my_encoded_word)){
    if(my_encoded_word[ii] > 122){cur_text[ii] = 32}
    if(my_encoded_word[ii] > 90 & my_encoded_word[ii] < 97){cur_text[ii] = 32}
    if(my_encoded_word[ii] == 60 | my_encoded_word[ii] == 62){cur_text[ii] = 32}
  }
  return(list(text = rawToChar(as.raw(my_encoded_word)), cur_text = rawToChar(as.raw(cur_text))))
}

decode_message = function(message, codeword){
  my_code = as.numeric(charToRaw(message))
  my_shift = as.numeric(charToRaw(tolower(codeword)))
  my_encoded_word = rep(0,length(my_code))
  L = length(my_shift)
  message_index = (1:length(my_code)) %% L 
  message_index[message_index == 0] = L
  for(ii in 1:L){
    my_encoded_word[message_index == ii] = (my_code[message_index == ii] - my_shift[ii] - 32) %% 95 +32
  }
  return(rawToChar(as.raw(my_encoded_word)))
}

# Example:

my_message  = "Drive to Mead City Hall. There you will find a miniature library which contains a book we both hated. Well that's one thing we've got."
my_codeword = "begin"

encrypted_message = encode_message(message=my_message, codeword=my_codeword)
encrypted_message$text
encrypted_message$cur_text
decode_message(message=encrypted_message$text, codeword=my_codeword)
