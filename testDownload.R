library(ssh)

## get remote list of files and file sizes
session <- ssh_connect('dfo@dfoftp.ocean.dal.ca')
out <- ssh_exec_internal(session, 'find . -path "*.kml"')
filepaths <- unlist(strsplit(rawToChar(out$stdout), '\n'))
filesizes <- unlist(
    lapply(filepaths, function(x)
        as.numeric(rawToChar(ssh_exec_internal(session, paste('wc -c <', x))$stdout))))
missionFiles <- filepaths[filesizes > 10000]
missionFilenames <- unlist(lapply(strsplit(missionFiles, '/'), function(x) x[7]))
missionSizes <- filesizes[filesizes > 10000]

## local local files and file sizes
localFiles <- dir('ftpkml', pattern='*.kml')
localSizes <- file.size(dir('ftpkml', full.names=TRUE))

## which files are not already downloaded?
to_download <- missionFiles[!(missionFilenames %in% localFiles)]
jnk <- lapply(to_download, function(x) scp_download(session, x, to='ftpkml/'))
localFiles <- dir('ftpkml', pattern='*.kml')
localSizes <- file.size(dir('ftpkml', full.names=TRUE))

## which remote files are larger than the local ones? (i.e. updated)
to_download <- missionFiles[!(missionSizes == localSizes)]
jnk <- lapply(to_download, function(x) scp_download(session, x, to='ftpkml/'))
localFiles <- dir('ftpkml', pattern='*.kml')
localSizes <- file.size(dir('ftpkml', full.names=TRUE))

ssh_disconnect(session)
