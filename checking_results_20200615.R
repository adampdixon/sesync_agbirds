data<-"G:\\My Drive\\SESYNC_Agbirds_Processing\\outputs"

states_list<-c("Texas","Ohio","Tennessee","Wisconsin","North Dakota","Montana","Kentucky","Michigan","Minnesota","Colorado","New Mexico","Wyoming","Arkansas","Iowa","Kansas","Missouri","Nebraska","Oklahoma","South Dakota","Louisiana","Alabama","Mississippi","Illinois","Indiana")

for (i in 1:length(states_list)) {
  searchString <- ' '
  replacementString <- ''
  f<-list.files(data, pattern = gsub(searchString,replacementString,states_list[i]))
  print(states_list[i])
  print(length(f))
}

