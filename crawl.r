## PARSING THE PLOSONE JOURNAL TO EXTRACT INFORMATION ##

## PASS START_YEAR & END_YEAR AS ARGUMENTS

crawl<-function(start_year=2016, end_year=2016){
library(stringr)
library(rvest)         # HTML SCRAPING
library(RCurl)
library(XML)           # HTML SCRAPING

## BASE URL WHICH IS THE MAIN WEBSITE
  
baseURL = "http://journals.plos.org/plosone/browse?resultView=list&page="
res = c()
for(i in 1:2)     # 12940 PAGES IN TOTAL
{
  appendURL = paste(baseURL,i,sep="")  #APPEND PAGE NUMBER TO THE WEBSITE URL
  paperlist = html(appendURL)
  
  ## PULL OUT ALL THE HTML TAGS
  
  paperURLs = paperlist %>% html_nodes(xpath="//*[@id='search-results']/li/h2/a") %>% html_attr("href")
  
  ## ATTACH TO CREATE LINKS
  
  linklist =  paste("http://journals.plos.org", paperURLs, sep = "")
  paperDATE = html_nodes(paperlist, xpath="//*[@id='search-results']/li/p[2]") %>% html_text()
  
  ## CHECK ALL THE PUBLICATION DATE AND MATCH IT WITH USER ARGUMENTS. STORE CORRESPONDING LINKS
  
  paperYEAR = as.numeric(substr(paperDATE, 19, 22))
  res = c(res, linklist[which(paperYEAR>=start_year & paperYEAR<=end_year)])
}

## SEND THE URL TO THE GETPLOSONE FUNCTION TO EXTRACT VARIABLES

getPLOSOne<-function(url.input){

  html <- getURL(url.input, followlocation=TRUE)
  
  ## PARSE HTML
  doc = htmlParse(html, asText=TRUE)
  
  ## GET ARTICLE TITLE
  artTitle = c()
  artTitle = xpathSApply(doc, "//*[@class='title-authors']/*[@id='artTitle']", xmlValue)
  
  ## GET AUTHOR LIST
  authors = c()
  authors = xpathSApply(doc, "//*[@id='floatAuthorList']", xmlValue)
  authors = gsub("\n", " ", authors)
  
  ## GET THE AUTHOR AFFILIATIONS
  AuAffiliations = c()
  AuAffiliations = xpathSApply(doc, "//*[@data-js-tooltip='tooltip_trigger']", xmlValue)
  AuAffiliations = gsub("\n|\\* E-mail: (.*?)\n|Affiliation(s)?|ORCID(.*)[0-9]{4}", "", AuAffiliations)
  AuAffiliations = paste(AuAffiliations, collapse = "\n")
  
  ## GET THE ARTICLE PUBLISHED DATE
  PubDate = c()
  PubDate = xpathSApply(doc, "//*[@id='artPubDate']", xmlValue)
  PubDate = gsub('Published:\\s*', '', PubDate)
  
  ## GET CORRESPONDING AUTHORS
  authCorresponding = c()
  authCorresponding = xpathSApply(doc, "//ul[@class='author-list clearfix']/li[last()]/a[@class='author-name']", xmlValue)
  authCorresponding = gsub("   |\n", "", authCorresponding)
  
  ## GET CORRESPONDING AUTHORS EMAIL LIST
  authCorrespondingEmail = c()
  authCorrespondingEmail = xpathSApply(doc, "//ul[@class='author-list clearfix']/li[last()]/div/p[1]/a", xmlValue)
  authCorrespondingEmail = paste(authCorrespondingEmail, collapse = "\n")
  
  ## GET ARTICLE ABSTRACT
  Abstract = c()
  Abstract = xpathSApply(doc, "//*[@class='abstract toc-section']/*[@title='Abstract']/../p", xmlValue)
  Abstract = paste(Abstract, collapse = "\n")
  
  ## GET FULL ARTICLE TEXT
  artText = c()
  artText = xpathSApply(doc, "//div[@class='article-text']", xmlValue)
  artText = gsub("\n", "", artText)
  artText = paste(artText, collapse = "\n")
  
  ## return a list of Title, Authors, Author Affiliations, Correspondence Author, 
  ## Correspondence Author's Email, Publish Date, Abstract, Full Paper
  #output = list(Title=artTitle, Authors=authors, AuthorAffiliations=AuAffiliations, CorrespondenceAuthor=authCorresponding, CorrespondenceAuthorEmail=authCorrespondingEmail, PublishDate=PubDate, Abstract=Abstract, FullPaper=artText)
  
  output = list()
  output = list(artTitle, authors, AuAffiliations, authCorresponding, authCorrespondingEmail, PubDate, Abstract, artText)
  output
}


## GENERATE 8 EMPTY VECTORS

Title <- c()
Authors <- c()
AuthorAffiliations <- c()
CorrespondenceAuthor <- c()
CorrespondenceAuthorEmail <- c()
PublishDate <- c()
Abstract <- c()
FullPaper <- c()

## STORE INTO LISTS AND ADD "NA" IF NOT AVAILABLE

for(i in res){
  NewLine <- getPLOSOne(i)
  if(length(NewLine[[1]])>=1){
    Title <- c(Title, NewLine[[1]])
  }else{
    Title <- c(Title, NA)
  }
  if(length(NewLine[[2]])>=1){
    Authors <- c(Authors, NewLine[[2]])
  }else{
    Authors <- c(Authors, NA)
  }
  if(length(NewLine[[3]])>=1){
    AuthorAffiliations <- c(AuthorAffiliations, NewLine[[3]])
  }else{
    AuthorAffiliations <- c(AuthorAffiliations, NA)
  }
  if(length(NewLine[[4]])>=1){
    CorrespondenceAuthor <- c(CorrespondenceAuthor, NewLine[[4]])
  }else{
    CorrespondenceAuthor <- c(CorrespondenceAuthor, NA)
  }
  if(length(NewLine[[5]])>=1){
    CorrespondenceAuthorEmail <- c(CorrespondenceAuthorEmail, NewLine[[5]])
  }else{
    CorrespondenceAuthorEmail <- c(CorrespondenceAuthorEmail, NA)
  }
  if(length(NewLine[[6]])>=1){
    PublishDate <- c(PublishDate, NewLine[[6]])
  }else{
    PublishDate <- c(PublishDate, NA)
  }
  if(length(NewLine[[7]])>=1){
    Abstract <- c(Abstract, NewLine[[7]])
  }else{
    Abstract <- c(Abstract, NA)
  }
  if(length(NewLine[[8]])>=1){
    FullPaper <- c(FullPaper, NewLine[[8]])
  }else{
    FullPaper <- c(FullPaper, NA)
  }
  
}

## CONVERT INTO CHARACTER VECTOR

Title_1 = as.character(Title)
Authors_1 = as.character(Authors)
AuthorAffiliations_1 = as.character(AuthorAffiliations)
CorrespondenceAuthorEmail_1 = as.character(AuthorAffiliations)
CorrespondenceAuthor_1 = as.character(CorrespondenceAuthorEmail)
PublishDate_1 = as.character(PublishDate)
Abstract_1 = as.character(Abstract)
FullPaper_1 = as.character(FullPaper)

## STORE THE CHARACTER LISTS INTO A DATA FRAME

dat = data.frame(Titl = Title_1,
                 Author = Authors_1,
                 AuthorAffiliation = AuthorAffiliations_1,
                 CorrespondenceAutho = CorrespondenceAuthor_1,
                 CorrespondenceAuthorEmai = CorrespondenceAuthorEmail_1,
                 PublishDat = PublishDate_1,
                 Abstrac = Abstract_1,
                 FullPape = FullPaper_1
)

## WRITE THE DATA FRAME TO A TEXT FILE & A CSV FILE

write.table(dat,"output.txt",sep = "\t",row.names = FALSE)
write.csv(dat,"output2.csv")

## RETURN DATA FRAME, STORE IN A VARIABLE USING A = CRAWL(X,Y)

dat
}