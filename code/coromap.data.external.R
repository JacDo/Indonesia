library(here)
library(ecRutils)

##file list
flist<-c(
  # file.path("/Users/eliascis/Dropbox/kairos/research_supplements/library/bibliothek.bib"),
  # file.path("/Users/eliascis/Dropbox/kairos/research_supplements/library/sources.bib"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/indonesia/kabkot/kap2015idm/kap2015idm.corrected.shx"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/indonesia/kabkot/kap2015idm/kap2015idm.corrected.shp"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/indonesia/kabkot/kap2015idm/kap2015idm.corrected.prj"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/indonesia/kabkot/kap2015idm/kap2015idm.corrected.dbf"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/world/world001.dbf"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/world/world001.prj"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/world/world001.shp"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/admin_borders/world/world001.shx"),
  file.path("/Users/eliascis/Dropbox/kairos/research_supplements/database/PODES/PODES_2014/podes2014.Rds"),
  file.path("/Users/eliascis/Dropbox/kairos/research_projects/fretpol/data/store/fretpol.data.tbase.Rds")
)
flink<-tempfile()
cat(flist,file=flink,sep="\n")
readLines(flink)

#here
system(
  paste0("rsync -avh -rp --progress ",paste0("`cat ",flink,"` "),here("data","external"))
  )

##copy twitter data from server to local 
# system(paste0("rsync -avh -rp --progress ",paste0("`cat ",flink,"` "),here("data","external")))
# 
# 
# system("rsync -chavzP --stats /path/to/copy user@host.remoted.from:/path/to/local/storage")
# 
# cmd<-paste0("rsync -a ",' "ecisneros@statoek-server3.wiso.uni-goettingen.de/home/cweisser/Twitter/Tweets_Indonesia/tweets 20191206-110752.json" ',here("data/store/tweets"))
# cmd
# writeClipboard(cmd)
# system(cmd)
# 
