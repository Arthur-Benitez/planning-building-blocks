
## Versión en R
tryCatch({
  setwd('/home/rstudio/building-blocks-app')
  backup_main_folder <- '../backup'
  backup_folder <- paste0(backup_main_folder, '/backup_', gsub(' ', 'T', gsub(':', '-', as.character(Sys.time()))))
  
  x <- lapply(c(backup_main_folder, backup_folder), function(d){
    if (!dir.exists(d)) {
      dir.create(d)
      cat(sprintf('INFO [%s] CREATING BACKUP DIRECTORY %s\n', Sys.time(), d))
    }
  })
  
  x <- lapply(list.files('.', all.files = FALSE), function(f){
    file.copy(from = f, to = backup_folder, overwrite = FALSE, recursive = TRUE)
  })
  cat(sprintf('INFO [%s] BACKUP COMPLETED %s\n', Sys.time(), d))
}, error = function(e){
  cat(sprintf('ERROR [%s] ERROR CREATING BACKUP\n', Sys.time()))
})

## Versión en shell
commands <- c(
  "home_dir=/home/rstudio/building-blocks-app",
  "echo $home_dir",
  "backup_dir=$(echo /home/rstudio/backup/backup_$(date '+%Y-%m-%d_%H-%M-%S'))",
  "echo $backup_dir",
  "mkdir $backup_dir",
  "cp -r $home_dir/* $backup_dir/",
  "echo HOME $home_dir ===========",
  "ls -l $home_dir",
  "echo BACKUP $backup_dir =======",
  "ls -l $backup_dir"
)
command <- paste(commands, collapse = ' && ')
system(command)