library("elastic")
x = connect()
a = sessions_users$news_id
a = a[!duplicated(a)]

get_docs = docs_mget(x, index = 'inca', id = a, source = c('title_rss','teaser_rss', 'text','publication_date'))
unlisted_docs = lapply(get_docs$docs, function(x) unlist(x$`_source`, FALSE))
docs_dataframe = bind_rows(unlisted_docs)
docs_dataframe$id = a
write.csv(docs_dataframe, file = "datasets/elasticsearch_info.csv", row.names = FALSE)
