
all.files = list.files("paper/Rcode")
files = paste0("paper/Rcode/", all.files[grep(pattern = "-", all.files)])
files
for (i in files){
  source(i)
}
