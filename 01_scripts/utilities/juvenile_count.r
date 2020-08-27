
juvenile_count <- function(by_year = TRUE){
    #Load in stock codes
    sc.base.df <- read_tsv(sc.base,guess_max = 10000)
    
    #Load in extraction sheet
    ES.base.df <- read_tsv(ES.base,guess_max = 100000)

    #Filter for juveniles
    juvenile.ES <- filter(ES.base.df,Lifestage %in% "J")
    
    if (by_year == TRUE){
        #Count juveniles by year
        juvenile.SC <- juvenile.ES %>%
                            group_by(StockCode,Year) %>%
                            count()
    } else {
        juvenile.SC <- juvenile.ES %>%
                            group_by(StockCode) %>%
                            count()
    }
    
    #Keep only important columns in stock codes file
    sc.base.df <- sc.base.df %>%
                  select(Code,collection)
    
    #Merge in collection name              
    juvenile.SC <- merge(juvenile.SC,sc.base.df,all.x=TRUE,by.x="StockCode",by.y="Code")

    #Remove any unnamed collections (weird notations in ES)
    juvenile.SC <- filter(juvenile.SC,!(collection %in% NA))
    
    #move collection to first column
    juvenile.SC <- juvenile.SC %>% 
                      select(collection,everything())

    # Assign to environment
    assign(x="juv_coll_cnt",value=juvenile.SC,pos=1)
    write_tsv(x=juvenile.SC,path=paste0(result.path,"juvenile_by_collection_",format(Sys.time(), "%Y-%m-%d"),".txt"))
}