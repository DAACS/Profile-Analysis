library(reshape2)
library(urltools)
library(magrittr)

bad.urls <- c('/_unused_dummy_error_path_route_application/undefined',
			  '/-8643-44a0-b131-5ad5f79251d4',
			  '/-8643-44a0-b131-5ad5f79251d4',
			  '/#edit-assessment-domain-content',
			  '/403',
			  '/assessments',
			  '/assessments/',
			  '/assessments/38f31383-d4c5-4e34-a8ef-81a8f299a29f',
			  '/assessments/38f31383-d4c5-4e34-a8ef-81a8f299a29f/start',
			  '/assessments/38f31383-d4c5-4e34-a8ef-81a8f299a29f/take',
			  '/assessments/764e0455-676d-4c69-8141-575e2436f131,',
			  '/assessments/764e0455-676d-4c69-8141-575e2436f131/start',
			  '/assessments/764e0455-676d-4c69-8141-575e2436f131/take',
			  '/assessments/b39a7bea-255f-4fce-89b7-9ca6c2973588',
			  '/assessments/b39a7bea-255f-4fce-89b7-9ca6c2973588/start',
			  '/assessments/b39a7bea-255f-4fce-89b7-9ca6c2973588/take',
			  '/assessments/da2f9b89-1fc0-4aee-b369-237c0b8dac68',
			  '/assessments/da2f9b89-1fc0-4aee-b369-237c0b8dac68/start',
			  '/assessments/da2f9b89-1fc0-4aee-b369-237c0b8dac68/take',
			  '/dashboard#assessments/request-help-modal',
			  '/dashboard#assessments/request-help-modal',
			  '/assessments/764e0455-676d-4c69-8141-575e2436f131')

# Remove missing URLs (these appear to be log outs)
table(events[is.na(events$url.base),]$eventType)
events <- events[!is.na(events$url.base),]

# Remove bad URLs
as.data.frame(events[events$url.base %in% bad.urls,]$timestamp) #AL added this to see if there's a trend in dates of bad urls
events <- events[!events$url.base %in% bad.urls,]

# Fix some urls
if(length(which(events$url.base == '/dashboard/')) > 0) {
	events[which(events$url.base == '/dashboard/'),]$url.base <- '/dashboard'
}
if(length(which(events$url.base == '/#')) > 0) {
	events[which(events$url.base == '/#'),]$url.base <- '/'
}

# removing trailing /s
tmp <- grep('*/$', events$url.base)
# events$url.base[tmp]

# Remove everything after ;jsession from the URL (typically to expand feedback)
tmp <- strsplit(events$url.base, ';jsession', fixed = TRUE)
tmp <- sapply(tmp, FUN = function(x) { x[[1]] })
events$url.base <- tmp

# Recode external links
domains <- domain(events$url.base)
events[!is.na(domains),]$url.base <- domains[!is.na(domains)]
# events[which(substr(events$url.base, 1, 4) == 'http'),]$url.base <- 'EXTERNAL_LINK'

# Recode admin pages
tmp <- grep('^/admin', events$url.base)
events$url.base[tmp] <- 'ADMIN_PAGE'

# Recode item details (i.e. the student clicked the expand button on an item feedback)
tmp <- grep('^/assessments/mathematics/.*#expand-feedback*', events$url.base)
events$url.base[tmp] <- 'MATH_ITEM_DETAIL'
tmp <- grep('^/assessments/reading/.*#expand-feedback*', events$url.base)
events$url.base[tmp] <- 'READ_ITEM_DETAIL'

length(unique(events$url.base)) # Number of unique URLs capture in DAACS
unique(events$url.base) # Print the list of URLs


# Split student and advisor events
events.students <- events[which(events$role == 'ROLE_STUDENT'),]
events.advisors <- events[which(events$role == 'ROLE_ADVISOR'),]

names(events.advisors)[4] <- 'advisorId'

# TODO: secondaryId may be something else in other DAACS instances
tmp <- strsplit(events.advisors$url, 'secondaryId=')
events.advisors$studentSecondaryId <- sapply(tmp, FUN = function(x) { ifelse(length(x) > 1, x[length(x)], NA) })
events.advisors$studentSecondaryId <- as.integer(events.advisors$studentSecondaryId)
tmp <- strsplit(events.advisors$url, 'userId=')
events.advisors$studentUserId <- sapply(tmp, FUN = function(x) { ifelse(length(x) > 1, x[length(x)], NA) })
users$secondaryId <- as.integer(users$secondaryId)
events.advisors <- merge(events.advisors, users[,c('secondaryId', 'userId')],
						 by.x = 'studentSecondaryId', by.y = 'secondaryId', 
						 all.x = TRUE, incomparables = NA)
events.advisors[is.na(events.advisors$studentUserId),]$studentUserId <- events.advisors[is.na(events.advisors$studentUserId),]$userId
events.advisors <- events.advisors[,c('advisorId', 'studentUserId', 'url', 'url.base', 'timestamp', 'User-Agent')]

