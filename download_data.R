library(ssh)
library(XML)
library(stringr)

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

dir <- "./ftpkml"
files <- paste(dir, as.list(list.files(path = dir, pattern = '*.trk.kml')), sep = '/')
mlat<-mlon<- vector(mode='list',length=length(files))
missionnames<-vector(mode='logical',length=length(files))
for (i in 1:length(files)){
    d <- xmlParse(files[i])
    dx <- xmlToList(d)
    dxd <- dx$Document

    coord <- dxd[[4]]$LineString$coordinates
    coordinates <- strsplit(coord, '\n')[[1]]
    position <- strsplit(coordinates, ',')

    lon <- as.numeric(unlist(lapply(position, function(k) k[1])))
    lat <- as.numeric(unlist(lapply(position, function(k) k[2])))

    good <- !(lon == 0 & lat == 0) #remove 0,0 coordinates

    lat <- lat[good]
    lon <- lon[good]

    good2 <- !(is.na(lon) & is.na(lat))

    lat <- lat[good2]
    lon <- lon[good2]
    
    mlat[[i]]<-lat
    mlon[[i]]<-lon
    
    ## set names of missions from files
    missionnames[i]<-str_extract(string=files[i],pattern='SEA0[0-9]{2}.M[0-9]{2}')
}
    
m <- 1:length(missionnames)
names(m) <- missionnames
choices <- 1:length(missionnames)
names(choices) <- paste(names(m), format(missionDates, '%Y-%m-%d'))
o <- order(missionDates, decreasing = TRUE)
m <- m[o]
choices <- choices[o]
glider <- substr(unlist(lapply(strsplit(names(choices), ' '), function(x) x[1])), 1, 6)

missions <- list(missionFiles=missionFiles[o], missionFilenames=missionFilenames[o],
                 missionSizes=missionSizes[o], missionDates=missionDates[o],
                 choices=choices, glider=glider,
                 mlon=mlon, mlat=mlat)
saveRDS(file='missions.rds', missions)
