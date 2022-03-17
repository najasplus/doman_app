#setwd("~/R_projects/doman_cards/doman_app")
read_vocabulary <- function(input_file_path, num_words, start_word) {
    if (is.null(input_file_path)) return(1)
    word_list = c()
    line = readLines(input_file_path, n = -1, encoding = "UTF-8")
    word_list = c(word_list, toupper(line))
    #print(word_list)

    word_list = word_list[word_list != ""]
    
    word_list_len = length(word_list)
    print(paste0(num_words, ", ", start_word, ", ", word_list_len))
    if (start_word > word_list_len) return(2)
    
    if (word_list_len > num_words) {
        word_list = word_list[start_word:(start_word + num_words-1)]
    } else {
        word_list = word_list[start_word:word_list_len]
    }
        
    word_list_rand = sample(word_list)
    print(word_list_rand)
    return(word_list_rand)
}

generate_named_list <- function(input_list) {
    target_list = input_list
    list_names = lapply(seq(1:length(input_list)), toString)
    names(target_list) = list_names
    return(target_list)
}

#voc = read_vocabulary("vocabulary.txt", 10, 2)
#print(generate_named_list(voc))