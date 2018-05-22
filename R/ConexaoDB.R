# install.packages("D:/R/pacotes/tibble_1.3.0.zip", repos=NULL)

Sys.setenv("ORACLE_HOME" = "/data/OracleClient11/app/oracle/product/11.2.0/client_1/")
Sys.setenv("LD_LIBRARY_PATH" = "/usr/lib64/R/lib::/lib:/usr/lib/jvm/jre/lib/amd64/server:/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/java/lib/amd64:/usr/java/packages/lib/amd64:/lib:/usr/lib:/data/OracleClient11/app/oracle/product/11.2.0/client_1/lib")

wd <- getwd()
setwd("/data/OracleClient11/app/oracle/product/11.2.0/client_1/")

# Faz a conexao com o Oracle
mydb <- dbConnect(dbDriver(drvName = "Oracle"), 
                  username = "DB_DBC_BI_DW_PRD", 
                  password = "AEhHaf80YKmL3WssXe8P", 
                  dbname = "SEP_PROD_BIDW", 
                  prefetch = FALSE, 
                  bulk_read = 1000L, 
                  bulk_write = 1000L, 
                  stmt_cache = 0L,
                  external_credentials = FALSE, 
                  sysdba = FALSE)

# Retorna pasta de trabalho
setwd(wd)
