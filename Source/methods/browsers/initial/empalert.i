/* phone.i */

&IF "{&IAM}" NE "sphone" &THEN
{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
&ENDIF
