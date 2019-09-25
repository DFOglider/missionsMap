library(ssh)

## get remote list of files and file sizes
session <- ssh_connect('dfo@dfoftp.ocean.dal.ca')
cat('* looking for kml files on server\n')
out <- ssh_exec_internal(session, 'find . -path "*.kml"')
filepaths <- unlist(strsplit(rawToChar(out$stdout), '\n'))
filesizes <- unlist(
    lapply(filepaths, function(x)
        as.numeric(rawToChar(ssh_exec_internal(session, paste('wc -c <', x))$stdout))))
lastmodified <- as.POSIXct(unlist(
    lapply(filepaths, function(x)
        as.POSIXct(rawToChar(ssh_exec_internal(session, paste("stat -c '%y'", x))$stdout)))),
    origin='1970-01-01')
missionFiles <- filepaths[filesizes > 10000]
missionFilenames <- unlist(lapply(strsplit(missionFiles, '/'), function(x) x[7]))
missionSizes <- filesizes[filesizes > 10000]
missionDates <- lastmodified[filesizes > 10000]

missions <- data.frame(missionFiles, missionFilenames, missionSizes, missionDates,
                       stringsAsFactors = FALSE)
save(file='missions.rda', missions)

## does the ftpkml directory exist?
if (length(dir('ftpkml')) < 1) dir.create('ftpkml')

## local local files and file sizes
localFiles <- dir('ftpkml', pattern='*.kml')
localSizes <- file.size(dir('ftpkml', full.names=TRUE))

## which files are not already downloaded?
cat('* downloading new/changed kml files\n')
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
