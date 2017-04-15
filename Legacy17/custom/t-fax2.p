/* custom/asifax.p  ASI fax program using MS Window XP Fax Console */
DEF VAR ip-type AS cha NO-UNDO.
DEF VAR ip-file-list AS cha NO-UNDO.
DEF VAR ip-cust-no AS cha NO-UNDO.
DEF VAR ip-subject AS cha NO-UNDO.
DEF VAR ip-body AS cha NO-UNDO.

DEF var op-return AS INT NO-UNDO.

DEF STREAM st-temp.
{custom/globdefs.i}
{custom/xprint.i}
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

ASSIGN ip-type = "multi"
       ip-subject = "Subject"
       ip-body = "Body".

find first sys-ctrl where sys-ctrl.company eq g_company
                      and sys-ctrl.name    eq "FAXCODE" no-lock no-error.
if not avail sys-ctrl THEN do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = g_company
     sys-ctrl.name    = "FAXCODE"
     sys-ctrl.descrip = "Drop Outbound Prefix number 1?  For what area code?"
     sys-ctrl.log-fld = NO
        .    
end.

FIND FIRST company WHERE company.company = g_company NO-LOCK NO-ERROR.
IF NOT AVAIL company THEN FIND FIRST company NO-LOCK NO-ERROR.
IF AVAIL company THEN ASSIGN lv-company = company.NAME
                             lv-comp-addr[1] = company.addr[1]
                             lv-comp-city = company.city
                             lv-comp-state = company.st
                             lv-comp-zip = company.zip.

FIND FIRST bf-cust WHERE bf-cust.company = g_company AND
                         bf-cust.active = "X" NO-LOCK NO-ERROR.
IF AVAIL bf-cust THEN ASSIGN lv-from = bf-cust.contact
                          lv-faxnumber = bf-cust.fax
                          lv-biznumber = bf-cust.area-code + bf-cust.phone
                          lv-homenumber = bf-cust.email
                          lv-from-email = bf-cust.email
                          lv-from-city = bf-cust.city
                          lv-from-state = bf-cust.state
                          .
                          

iF ip-type = "MULTI" THEN DO:  /* faxing to Multi customer or vendor */
    
    /* ==== fax# and image file are in seperate file ==*/
    DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
    DEF VAR lv-fax-file AS cha FORM "x(60)" NO-UNDO.
    DEF VAR lv-input AS cha FORM "x(80)" NO-UNDO.
    DEF VAR lv-outfile AS cha NO-UNDO.

    INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO .
    REPEAT:
      SET lv-file-name.
      /*========= xprint image file faxing -=========*/
      IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*tif*" 
      THEN DO:    /* xprint image file */ 
        lv-fax-file = replace(lv-file-name,"tif","txt").
        ls-to-list = "".
        lv-fax-file = "c:\temp\fax\" + lv-fax-file.
        INPUT STREAM st-temp FROM VALUE (lv-fax-file) no-echo.
        REPEAT:
            IMPORT STREAM st-temp UNFORMATTED lv-input.
            IF index(lv-input,"FAX#:") > 0 THEN DO:
               ls-to-list = SUBSTRING(lv-input,index(lv-input,"FAX#:") + 5,10).
            END.
        END.
        INPUT STREAM st-temp CLOSE.

        IF ls-to-list <> "" THEN do: /* send fax */
      /*     
           FIND FIRST cust WHERE cust.company = g_company AND            /* FXaaaaaa.tif*/
                                 cust.cust-no = SUBSTRING(lv-file-name,3,INDEX(lv-file-name,".") - 3)
                            NO-LOCK NO-ERROR.
           lv-faxname = IF AVAIL cust THEN cust.contact ELSE "".
        */
           ASSIGN /*ls-to-list = /*IF length(trim(ls-to-list)) < 10 THEN ls-to-list ELSE "1" + ls-to-list */
                               /*IF AVAIL sys-ctrl AND trim(sys-ctrl.char-fld) = SUBSTRING(TRIM(ls-to-list),1,3) THEN ls-to-list ELSE "1" + ls-to-list*/
                               IF AVAIL cust AND cust.fax-prefix <> "" THEN trim(cust.fax-prefix) + ls-to-list ELSE ls-to-list*/
                  lv-faxto = "12153697801" /*ls-to-list*/
                  lv-faxsubject = ip-subject
                  lv-faxbody = ip-body                                  
                  lv-faxattach = "c:\temp\fax\" + lv-file-name
                  lv-outfile   = lv-faxattach
                  .
MESSAGE "out:" lv-outfile SKIP
       "attach:" lv-faxattach SKIP
       "body:" lv-faxbody SKIP
       "subject:" lv-faxsubject SKIP
       "to:" lv-faxto SKIP
       "faxname" lv-faxname SKIP
    lv-file-name
    VIEW-AS ALERT-BOX.

/*
ls-to-list = "12153697801".
aSSIGN lv-faxto = ls-to-list
       lv-faxsubject = "FAX TEST"
       lv-faxbody = "FAx Body"
       lv-faxattach = "c:\tmp\tax.txt".
       
lv-outfile = "".
ip-subject = "".

ip-body = "".
*/

           {custom/t-fax2.i}
        
        END.
      END.
      /*======= text file fax  ===========*/
      ELSE IF lv-file-name <> "." AND lv-file-name <> ".." 
              AND lv-file-name MATCHES "*fax*" 
      THEN DO:  /* text file */
         lv-fax-file = replace(lv-file-name,"fax","txt").
         ls-to-list = "".
         lv-fax-file = "c:\temp\fax\" + lv-fax-file.
         lv-input = "".
         INPUT STREAM st-temp FROM VALUE (lv-fax-file) no-echo.
         REPEAT:
            IMPORT STREAM st-temp UNFORMATTED lv-input.
            IF index(lv-input,"FAX#:") > 0 THEN DO:
               ls-to-list = SUBSTRING(lv-input,index(lv-input,"FAX#:") + 5,11).
            END.
         END.
         INPUT STREAM st-temp CLOSE.

         IF ls-to-list <> "" THEN do: /* send fax */
            ASSIGN /*ls-to-list = IF length(trim(ls-to-list)) < 10 THEN ls-to-list ELSE "1" + ls-to-list*/                                
                  lv-faxto = ls-to-list
                  lv-faxsubject = ip-subject
                  lv-faxbody = ip-body                                  
                  lv-faxattach = "c:\temp\fax\" + lv-file-name
                  lv-outfile   = lv-faxattach
                  .
            {custom/asifax2.i}
         END.
      END.
      /*============ end of text fiel faxing =====*/
      ASSIGN lv-file-name = ""
             ls-to-list = "" .
      
    END. /* repeat */
    INPUT CLOSE.
    OUTPUT CLOSE.

    RETURN.
END.  /* multi */

/* ==== not working with windown NT4.0 
   will send fax printer and user type fax number from there 
   ============================================================ */


IF ls-to-list = "" OR ip-type = "" THEN DO:
     RUN custom/d-fax.w (OUTPUT ls-to-list).
     IF ls-to-list = "" THEN RETURN.  /* canceled */
     lv-rec-email = "".         

END.

lv-server = "".
MESSAGE "Modem Installed in Local Computer?" VIEW-AS ALERT-BOX BUTTON YES-NO 
    UPDATE v-ans AS LOG .
IF NOT v-ans THEN RUN custom/d-fxserv.w (OUTPUT lv-server).

ASSIGN lv-faxto = ls-to-list
       lv-faxsubject = ip-subject
       lv-faxbody = ip-body
       lv-faxattach = ip-file-list.
       

CREATE "FaxComEx.FaxServer" hC-server .
hC-server:CONNECT(lv-server).
CREATE "FaxComEx.FaxDocument" hC-fax.


ASSIGN hC-fax:Body = ip-file-list.

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
hC-fax:Recipients:ADD(lv-faxto,ip-cust-no). 

hC-fax:Attachfaxtoreceipt = TRUE.
hc-fax:coverpagetype = 2.
hc-fax:coverpage = "generic".
hc-fax:note = ip-subject.
hc-fax:receiptAddress = lv-rec-email.
hc-fax:receipttype = 0.
  /*
hc-fax:scheduletype = 1.
hc-fax:scheduletime =
    */

       hC-fax:DocumentName = ip-body.
       hC-fax:Subject = ip-subject.


 ASSIGN hC-fax:Sender:TITLE = ip-subject
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
/* =====================*/


/* ==
   for NT40 fax same as print to Fax printer
   
     /*
IF INDEX("Pacific,Xprint,Southpak,Hughes",v-print-fmt) > 0 THEN is-xprint-form = YES.     
ELSE is-xprint-form = NO.
IF is-xprint-form THEN DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).
END.
ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
  */

    FILE-INFO:FILE-NAME = ip-file-list.
    RUN printfile (FILE-INFO:FILE-NAME).
===========*/
