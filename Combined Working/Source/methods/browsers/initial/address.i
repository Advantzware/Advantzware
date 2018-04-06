/* address.i */

&IF "{&IAM}" NE "saddress" &THEN
{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
&ENDIF