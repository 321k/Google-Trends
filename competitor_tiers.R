library(dplyr)

company <- 'transferwise'

competitors <- c('moneygram',
'western union',
'fairfx',
'caxton fx',
'worldfirst',
'worldremit',
'currencyfair',
'transfergo',
'tawipay',
'xoom',
'transfast',
'remitly',
'ria money transfer',
'azimo',
'moneycorp',
'ukforex',
'hifx',
'post office money',
'transferwise')

rank_table <- data.frame(competitors=competitors, batch = ceiling(seq(1, length(competitors),1)/4), stringsAsFactors=F)
downloadDir = '/users/erik.johansson/downloads'
res = list()
for(i in 1:max(rank_table$batch)){
  r = which(rank_table$batch == i)
  keywords = c(rank_table$competitors[r], company)
  url = URL_GT(keywords, year=2016)
  GT_dir = downloadGT(url, downloadDir)
  GT_dir = paste(downloadDir, GT_dir, sep='/')
  res[[i]] = readGT(GT_dir)
}
res.normalised = list()
for(i in 1:length(res)){
  print(i)
  res.normalised[[i]] = res[[i]]
  r <- which(res[[i]]$Keyword==company)
  res.company <- res[[i]][r,]
  keywords = unique(res[[i]]$Keyword)
  
  for(j in 1:length(keywords)){
    print(paste("j", j))
    s = which(res[[i]]$Keyword == keywords[j])
    res.normalised[[i]]$SVI[s] = res[[i]]$SVI[s] / res.company$SVI
  }
}

df <- do.call("rbind", res.normalised)

df %>% ggplot(aes(Date, SVI,color=Keyword))+geom_line()

df.max <- df[which(df$Date==max(df$Date)),]

rank_table <- merge(rank_table, df.max[c(3,2)], by.x='competitors', by.y='Keyword') %>% unique
rank_table <- rank_table[order(rank_table$SVI, decreasing=T),]
deviation <- rank_table$SVI-mean(rank_table$SVI)
top_tier <- which(deviation > sd(rank_table$SVI))
bottom_tier <- which(deviation < -sqrt(sd(rank_table$SVI))/2)
rank_table$tier <- 'mid_tier'
rank_table$tier[top_tier] <- 'top_tier'
rank_table$tier[bottom_tier] <- 'bottom_tier'
