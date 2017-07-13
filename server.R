#################################################
#              Text Filtering by Keywords          #
#################################################

shinyServer(function(input, output,session) {
  set.seed=2092014   

text <- reactive({
    if (is.null(input$file)) {return(NULL)}
      else {
        document = readLines(input$file$datapath)
        return(document)}
      })

wordlist <- reactive({
  if (is.null(input$keywords)) {key1 = ""} else {
    key1 = readLines(input$keywords$datapath)
  }
  if (input$keywords2 == "") {key2 = ""} else {
    key2 = unlist(strsplit(input$keywords2,","))
  }
  key = setdiff(unique(c(key1,key2)),"")
  return(key)
})  

#arbit_wl = reactive({
#  arbit_wl = wordlist() %>% data_frame()
#  colnames(arbit_wl) = "word"
#  arbit_wl  
# })
  
half_winsize <- reactive({
	return(input$num) 
})  

sentence = reactive({

  wordlist = wordlist()	
  half_winsize = half_winsize()
  text = text()
  require(stringr)  	
  text = gsub('<.*?>', "", text)   # drop html junk
	
  # first replace all ngram spaces among keywords with underscores
  wordlist1 = gsub(" ", "_", wordlist); # wordlist1
  key_ngrams = setdiff(wordlist1, wordlist); # key_ngrams 
  pattern_key = gsub("_", " ", key_ngrams); # pattern_key
	
  for (i1 in 1:length(key_ngrams)){ 
	text = gsub(pattern_key[i1], key_ngrams[i1], text, ignore.case = TRUE) }

  wordlist2 = wordlist1 %>% as.character() %>% as.data.frame() 
  colnames(wordlist2) = "word"
  # wordlist2$word = as.character(wordlist2$word) # exception handling

  # now tokenize coprus into docs_words and ID relevant chunks
  names(text) = "text"
  text_df = text %>% as.data.frame() %>% mutate(doc = seq(1:length(text))); 
  text_df$text1 =  as.character(text_df$.) 
  text_df = text_df %>% select(doc, text1)

  # prep corpus for extraction op
  text_df1 = text_df %>% unnest_tokens(word, text1, token = "words") %>% 

   		# building various indices
		mutate(word1 = 1) %>% group_by(doc) %>%  
		mutate(word_ind = seq(1:sum(word1))) %>% select(-word1) %>%
		mutate(docmin = 1) %>% mutate(docmax = as.numeric(max(word_ind))) %>%

		# build primary key on rows
		mutate(row_key = doc*1000 + 0.01*word_ind) %>% ungroup()
	
  # find extraction keywords
  target_words = text_df1 %>% inner_join(wordlist2, by = "word") %>% 
			mutate(start1 = word_ind - half_winsize) %>%
			mutate(stop1 = word_ind + half_winsize) #%>%		
  
  # find extraction cutpoints
  a1  = target_words %>% mutate(start = ifelse(start1 < 1, docmin, start1)) %>% 
	mutate(stop = ifelse(stop1 > docmax, docmax, stop1)) %>%
	mutate(row_key_start = doc*1000 + 0.01*start) %>%
	mutate(row_key_stop  = doc*1000 + 0.01*stop) %>%
	select(row_key_start, row_key, row_key_stop)

  # Highlight target keywords, using sapply()
  text_df1[text_df1$row_key %in% a1$row_key, 2] = sapply(text_df1[text_df1$row_key %in% a1$row_key, 2], 
							 function(x) {paste('**', x, '**', sep="")})

  # extract chunks and de-duplicate
  chunk_collect = vector("list", nrow(a1))
  # sentence1 = vector("list", nrow(a1))
  sentence1 = matrix(0, nrow(a1), 2)	
  colnames(sentence1) = c("ser_num", "text")	
  for (i1 in 1:nrow(a1)){

	chunk_collect[[i1]] = text_df1 %>% filter(text_df1$row_key >= a1$row_key_start[i1],
				text_df1$row_key <= a1$row_key_stop[i1]) %>% select(word) %>% as.character()

        # sentence1[[i1]] = paste(unlist(chunk_collect[[i1]]), collapse=" ")	} # i1 loop ends
	
	sentence1[i1, 1] = str_c(chunk_collect[[i1]]$word, collapse=" ")	} # i1 loop ends

  # sentence2 = sapply(sentence1, combine)
  sentence = data.frame(sentence1) # data_frame("text")
  # for (i2 in 1:length(sentence1)){sentence[i2,1] = sentence1[[i2]]}
  # sentence = as.character(sentence)	
  return(sentence)
  })


output$filter_corp = renderPrint({
cat("Total ", length(sentence())," sentences.\n")
cat(as.String(paste0(1:length(sentence())," -> ", sentence())))
})

output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )


output$downloadData2 <- downloadHandler(
  filename = function() { "Keywords.txt" },
  content = function(file) {
    writeLines(readLines("data/keywords.txt"), file)
  }
)


output$downloadData3 <- downloadHandler(
  filename = function() { "Filtered_corpus.txt" },
  content = function(file) {
    writeLines(sentence(), file)
  }
)

})
