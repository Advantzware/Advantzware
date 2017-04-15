/* custom/asifax.p  ASI fax program using MS Window XP Fax Console */

DEF VAR op-return AS INT NO-UNDO.

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
DEF VAR lv-server AS cha NO-UNDO.

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
       lv-faxattach = "c:\tmp\faxtest.txt".
       

CREATE "FaxComEx.FaxServer" hC-server .
hC-server:CONNECT(lv-server).
CREATE "FaxComEx.FaxDocument" hC-fax.


ASSIGN hC-fax:Body = "c:\tmp\faxtest.tif".

/*
IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
 /*   MESSAGE "bbody ERROR"  ERROR-STATUS:NUM-MESSAGES v-job-id skip
         ERROR-STATUS:TYPE
        ERROR-STATUS:GET-MESSAGE(i)
        VIEW-AS ALERT-BOX.*/
    op-return = 1.
    RETURN .
END.
*/

hc-fax:priority = 1.
hC-fax:Recipients:ADD(lv-faxto,"ABC100"). 

hC-fax:Attachfaxtoreceipt = TRUE.
hc-fax:coverpagetype = 2.
hc-fax:coverpage = "generic".
hc-fax:note = "Fax Subject".
hc-fax:receiptAddress = lv-rec-email.
hc-fax:receipttype = 0.
  /*
hc-fax:scheduletype = 1.
hc-fax:scheduletime =
    */

       hC-fax:DocumentName = "Fax Body".
       hC-fax:Subject = "Fax Subject".


 ASSIGN hC-fax:Sender:TITLE = "Fax Subject"
                     hC-fax:sender:NAME = lv-from /*"Sender"*/
                     hC-fax:sender:city = lv-comp-city /*"Yardley"*/
                     hC-fax:sender:state = lv-comp-state /*"PA"*/
                     hc-fax:sender:zipcode = lv-comp-zip
                     hc-fax:sender:company = lv-company /*"Advanced Software"*/
                     hc-fax:sender:streetaddress = lv-comp-addr[1]
                     hc-fax:sender:Email = lv-from-email /*"ASI@advantzware.com" */
                     hc-fax:sender:faxnumber = lv-faxnumber
                     hc-fax:sender:officephone = lv-biznumber
                     hc-fax:sender:homephone = lv-homenumber                     
                     .

v-job-id = hc-fax:ConnectedSubmit(hC-server BY-POINTER) NO-ERROR.


RELEASE OBJECT hC-server.
RELEASE OBJECT hc-fax.

