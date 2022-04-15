library(data.table)
library(tm)
library(wordcloud2)

# Auxiliary functions -----------------------------------------------------
is.stopword <- function(x) {
  ## Blank some keywords
  remVec  <-
    c(
      "Institute",
      "University",
      "Department",
      "Dept.",
      "College",
      "Academy",
      "School",
      "Laboratory",
      "Laboratories",
      "Center for",
      "Research Center",
      "Computing Center",
      "Test Center",
      "Cancer Center",
      "Development Center",
      "Technische",
      "Universität",
      "Searchable",
      "Abstract",
      "Abstracts",
      "Document",
      "Conference",
      "April 12–15, 2022",
      "April 4, 2022",
      "Market Street",
      "Philadelphia",
      "Telephone",
      "UQ22",
      "@",
      "use",
      "nation",
      "work",
      "however",
      "one",
      "two",
      "due",
      "will",
      "talk",
      "present",
      "can",
      "based",
      "also",
      "many",
      "well",
      "often",
      "may",
      "thus",
      "via",
      "sandia",
      "within",
      state.name
    )
  kwd  <- sprintf("^%s$", c(tm::stopwords("english"), tolower(remVec)))
  ind  <- rowSums(sapply(kwd, grepl, x = x)) > 0
  return(ind)
}

# Read data ---------------------------------------------------------------
prgVec  <- readLines(file.path("data", "program.txt"), warn = FALSE)

# Process data ------------------------------------------------------------
prgStr  <- VCorpus(VectorSource(prgVec))
prgStr  <- tm_map(prgStr, stripWhitespace)
prgStr  <- tm_map(prgStr, content_transformer(tolower))
prgStr  <- tm_map(prgStr, content_transformer(removePunctuation))
prgStr  <- tm_map(prgStr, content_transformer(removeNumbers))
prgTdm  <- sort(rowSums(as.matrix(TermDocumentMatrix(prgStr))),
                 decreasing = TRUE)

# Visualize ---------------------------------------------------------------
cloudLDF0      <- data.table(word = names(prgTdm))
cloudLDF0$freq <- prgTdm
cloudLDF0$stem <- stemDocument(cloudLDF0$word, "english")
cloudLDF0[stem %like% "experi", stem := "experiment"]
cloudLDF0[stem == "pdes", stem := "pde"]

cloudLDF1 <- cloudLDF0[!is.stopword(word) & !is.stopword(stem)][
  order(-freq), .(word = word[1], freq = sum(freq)), by = "stem"
]

fwrite(cloudLDF1, file.path("data", "wordlist.csv"))

wordcloud2(cloudLDF1[freq > 3, c("word", "freq")])
