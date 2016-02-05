/* empalert.i */

{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}

empalert.table_rec_key = ip-rec_key.

RUN EMailNotify (YES).
