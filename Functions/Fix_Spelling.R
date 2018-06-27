##################################################################################
#
#FUNCTION TO FIX SPELLING STRINGS
#
##################################################################################
library (tidyverse)
library(hunspell)

fix_my_text<-function(sentence){
  sentence_split<-unlist(str_split(sentence, pattern=" "))
  sentence_split_ck<-hunspell_check(sentence_split)
  if(length(which(!sentence_split_ck))>0){
    sug_list<-hunspell_suggest(sentence_split[!sentence_split_ck])
    sug_list_cut<-lapply(1:length(sug_list), function(x){
      sug_list[[x]][1]
    })
    class_sec<-0
    lapply(which(!sentence_split_ck),function(x){
      class_sec<<-class_sec+1
      sentence_split[[x]]<<-sug_list_cut[[class_sec]][1]
    })
    sentence_corrected<-str_c(str_replace_na(sentence_split,replacement = "NA"), collapse = " ")
  }else if(length(which(!sentence_split_ck))==0){
    sentence_corrected<-sentence
  }
  return(sentence_corrected)
  
}

### EXAMPLE

bad_string<-"I cant spelll rigtt noow"

fix_my_text(bad_string)
