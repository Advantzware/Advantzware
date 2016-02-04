/* custom/t-faxcli.p  ASI fax program using MS Fax client and window 2000 
*/

DEF var op-return AS INT NO-UNDO.

DEF STREAM st-temp.
{custom/globdefs.i}

DEF VAR retcode AS INT NO-UNDO.
DEF VAR ls-to-list AS cha NO-UNDO.
DEF VAR lv-faxto AS cha NO-UNDO.
DEF VAR lv-faxsubject AS cha NO-UNDO.
DEF VAR lv-faxbody AS cha NO-UNDO.
DEF VAR lv-faxattach AS cha NO-UNDO.
DEF VAR lv-rec-email AS cha NO-UNDO.

DEFINE VARIABLE hC-server AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hC-fax AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hC-Recipient AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hC-sender AS COM-HANDLE NO-UNDO.
DEF VAR v-job-id AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR lv-file-cnt AS INT NO-UNDO.  /* fax file count */

DEF VAR lv-company AS cha NO-UNDO.
DEF VAR lv-from AS cha NO-UNDO.
DEF VAR lv-from-city AS cha NO-UNDO.
DEF VAR lv-from-state AS cha NO-UNDO.
DEF VAR lv-faxnumber AS cha NO-UNDO.
DEF VAR lv-biznumber AS cha NO-UNDO.
DEF VAR lv-homenumber AS cha NO-UNDO.
DEF VAR lv-from-email AS cha NO-UNDO.
DEF VAR lv-faxname AS cha NO-UNDO.  /* fax receipient's name */
DEF BUFFER bf-cust FOR cust.
DEF VAR lv-comp-addr AS cha EXTENT 4 NO-UNDO.
DEF VAR lv-comp-city AS cha NO-UNDO.
DEF VAR lv-comp-state AS cha NO-UNDO.
DEF VAR lv-comp-zip AS cha NO-UNDO.
DEF VAR lv-server AS cha INIT "ASI2000" NO-UNDO.

FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
IF NOT AVAIL company THEN FIND FIRST company NO-LOCK NO-ERROR.
IF AVAIL company THEN ASSIGN lv-company = company.NAME
                             lv-comp-addr[1] = company.addr[1]
                             lv-comp-city = company.city
                             lv-comp-state = company.st
                             lv-comp-zip = company.zip.


ls-to-list = "12153697801".

ASSIGN lv-faxto = ls-to-list
       lv-faxsubject = "FAX TEST"
       lv-faxbody = "FAx Body"
       lv-faxattach = "c:\tmp\fax.txt".
       

lv-server = "".
MESSAGE 1 VIEW-AS ALERT-BOX.
CREATE "FaxServer.FaxServer" hC-server .
MESSAGE 2 error-status:error VIEW-AS ALERT-BOX.
hC-server:CONNECT("").
MESSAGE 3 lv-faxattach VIEW-AS ALERT-BOX.
hc-fax = hc-server:CreateDocument(lv-faxattach).
message 4 view-as alert-box.
hc-fax:FileName = "C:\tmp\fax.txt" .

hc-fax:FaxNumber = "2153697801".
message 5 hc-fax:FILENAME "," hc-fax:faxnumber view-as alert-box.
v-job-id = hc-fax:SEND().
message "Job:" v-job-id view-as alert-box.

IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    MESSAGE " ERROR"  ERROR-STATUS:NUM-MESSAGES  skip
         ERROR-STATUS:TYPE
        ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX.
    op-return = 1.
    
END.

hc-server:DISCONNECT().
