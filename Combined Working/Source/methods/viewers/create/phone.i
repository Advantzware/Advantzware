/* phone.i */

{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
  
{methods/run_link.i "CONTAINER-SOURCE" "setAddStatus" "(INPUT YES)"} 

phone.table_rec_key = ip-rec_key no-error.

RUN EMailNotify (YES).
