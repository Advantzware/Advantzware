/*------------------------------------------------------------------------
    File        : misc1099.p
    Purpose     :  1099-MISC

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttMISC1099Report NO-UNDO
FIELD misc1099 AS CHAR
FIELD a AS CHAR.
DEFINE DATASET dsMISC1099Report FOR ttMISC1099Report .

    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginVendor    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndVendor      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegdt          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEnddt          AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmrddate         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmincld          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmcopy           AS INT NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMISC1099Report.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser           = ?  THEN ASSIGN prmUser           = "".
    IF prmAction         = ?  THEN ASSIGN prmAction         = "".
    IF prmOut            = ?  THEN ASSIGN prmOut            = "".
    IF prmBeginVendor    = ?  THEN ASSIGN prmBeginVendor    = "".
    IF prmEndVendor      = ?  THEN ASSIGN prmEndVendor      = "".
    IF prmBegdt          = ?  THEN ASSIGN prmBegdt          = "".
    IF prmEnddt          = ?  THEN ASSIGN prmEnddt          = "".
    IF prmrddate         = ?  THEN ASSIGN prmrddate         = "".
    IF prmincld          = ?  THEN ASSIGN prmincld          = "".
    IF prmcopy           = ?  THEN ASSIGN prmcopy           = 0.
   
                

    

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR v-VERSION AS CHAR NO-UNDO. 
{custom/xprint.i}
{custom/vxprint.i}
{sys/inc/var.i new shared}

DEF STREAM excel.

DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
 DEF VAR  tmp-path AS CHAR NO-UNDO. 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEFINE VARIABLE begin_vend      AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_vend        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE begin_date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 NO-UNDO.
DEFINE VARIABLE end_date        AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE rs-date         AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_zero-ven     AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE lv-copies       AS INTEGER FORMAT ">9":U INITIAL 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO         NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no      NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vaging.csv" NO-UNDO.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

assign
 cocode = prmComp
 locode = usercomp.loc
 tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE
 v-today = TODAY   . 


DEF VAR lv-default-comp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
                                  usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

def new shared var v-s-vend like vend.vend-no.
def new shared var v-e-vend like vend.vend-no.
def new shared var v-idate  as   log format "Invoice/Posting" init yes.
def new shared var v-sort   as   log format "Code/Name"       init yes.
def new shared var v-dtl    as   log format "Detail/Summary"  init yes.
def new shared var v-s-type AS CHAR NO-UNDO.
def new shared var v-e-type AS CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-days AS INTE NO-UNDO EXTENT 4.
DEF VAR v-refnum AS CHAR NO-UNDO.
DEF VAR v-check-date AS DATE NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS CHAR NO-UNDO.


def var v-num-per like company.num-per no-undo.

 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

{aprep\r-1099m.i NEW}


def buffer xperiod for period.




FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
  IF AVAIL company THEN v-num-per = company.num-per.

  ASSIGN
   begin_date = ?
   end_date   = ?.
  


 

IF prmAction = "misc1099" THEN DO:
    FIND FIRST sys-ctrl WHERE
       sys-ctrl.company EQ cocode AND
       sys-ctrl.name    EQ "1099MISC"
       NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN
       sys-ctrl.company  = cocode
       sys-ctrl.name     = "1099MISC"
       sys-ctrl.log-fld  = NO
       sys-ctrl.char-fld = ""
       sys-ctrl.descrip  = "1099-MISC Print Format".
  END.

  v-print-fmt = sys-ctrl.char-fld.

  CASE v-print-fmt:
      WHEN "Fibre" THEN
         ASSIGN v-program = "aprep/fibremsc.p"
                lines-per-page = 50
                is-xprint-form = YES.
  END.



   
    ASSIGN
      begin_vend   =  prmBeginVendor
      end_vend     =  prmEndVendor    
      begin_date   =  date(prmBegdt)      
      end_date     =  date(prmEnddt)      
      rs-date      =  prmrddate      
      tb_zero-ven  =  IF prmincld = "True" THEN TRUE ELSE FALSE 
      lv-copies    =  prmcopy .


       FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
           sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.
       
       IF NOT AVAIL sys-ctrl THEN
           DO:
           CREATE sys-ctrl.
           ASSIGN
               sys-ctrl.company  = cocode
               sys-ctrl.name     = "X-VERSION"
               sys-ctrl.descrip  = "Server Name"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Server 2003".
           END.
           IF AVAIL sys-ctrl  THEN
               v-VERSION = sys-ctrl.char-fld .
           RELEASE sys-ctrl.
           
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
               sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.
           
           IF NOT AVAIL sys-ctrl THEN
               DO:
               CREATE sys-ctrl.
               ASSIGN
                   sys-ctrl.company  = cocode
                   sys-ctrl.name     = "Xspool"
                   sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
                   sys-ctrl.log-fld = YES
                   sys-ctrl.char-fld = "c:\spool\".
               END.
               IF AVAIL sys-ctrl  THEN
                   tmp-path = sys-ctrl.char-fld .
                   RELEASE sys-ctrl.

                   ASSIGN  
                       init-dir    = v-webrootpath 
                       lv-pdf-file = init-dir + 'PrintMISC' 
                       lv-pdf-file = lv-pdf-file + string(prmBeginVendor) + STRING(TIME)
                       vPdfFile    = 'PrintMISC' + string(prmBeginVendor) + STRING(TIME) + '.pdf'.
    
        run run-report.
        
        IF v-VERSION = "Server 2008" THEN do:
            OS-COPY VALUE(list-name) VALUE (tmp-path).
            PAUSE 1.
        END.
        ELSE
            RUN printFile(list-name).

    
    CREATE ttMISC1099Report.
    ASSIGN ttMISC1099Report.misc1099 = vPdfFile.

END. /* IF prmAction = "Trans" THEN DO: */

    
    
PROCEDURE run-report :
DEF VAR v-vend-tot AS DEC NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-1099-m.

  {sys/form/r-top.i}
  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  IF IS-xprint-form THEN
  DO:
      /*  PUT "</PROGRESS>".*/

      PUT UNFORMATTED
                   "<PRINT=NO><SILENT=TRUE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" 
                                lv-pdf-file ".pdf><CPI10.8>".
  END.

  s-copies = lv-copies.

  

  IF rs-date EQ "Invoice" THEN
     for each vend FIELDS(company vend-no code-1099 NAME tax-id add1
         add2 city state zip) WHERE
         vend.company   eq cocode AND
         vend.vend-no   ge begin_vend AND
         vend.vend-no   le end_vend AND
         vend.code-1099 eq "y"
         NO-LOCK,
         EACH ap-inv FIELDS(company vend-no inv-date posted) WHERE
              ap-inv.company EQ cocode AND
              ap-inv.vend-no EQ vend.vend-no AND
              ap-inv.inv-date GE begin_date AND
              ap-inv.inv-date LE end_date AND
              ap-inv.posted EQ YES
              NO-LOCK,
         each ap-payl FIELDS(amt-paid amt-disc) WHERE
              ap-payl.inv-no   eq ap-inv.inv-no AND
              ap-payl.vend-no  eq ap-inv.vend-no AND
              ap-payl.posted   eq YES
              NO-LOCK
         break by vend.vend-no:

         RUN calc-total-proc(INPUT-OUTPUT v-vend-tot,
                             INPUT LAST-OF(vend.vend-no)).
     END.
  ELSE /* Check*/
     for each vend FIELDS(company vend-no code-1099 NAME tax-id add1
         add2 city state zip) WHERE
         vend.company   eq cocode AND
         vend.vend-no   ge begin_vend AND
         vend.vend-no   le end_vend AND
         vend.code-1099 eq "y"
         no-lock,
         each ap-pay FIELDS(company vend-no check-date posted) WHERE
              ap-pay.company    eq cocode AND
              ap-pay.vend-no    eq vend.vend-no AND
              ap-pay.check-date ge begin_date AND
              ap-pay.check-date le end_date AND
              ap-pay.posted     eq yes
              no-lock,
         each ap-payl FIELDS(amt-paid amt-disc) WHERE
              ap-payl.c-no eq ap-pay.c-no AND
              ap-payl.memo eq no
              no-lock
         break by vend.vend-no:
    
         RUN calc-total-proc(INPUT-OUTPUT v-vend-tot,
                             INPUT LAST-OF(vend.vend-no)).
     end.

  RUN value(v-program).
  
end procedure.

PROCEDURE calc-total-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER iop-vend-tot AS DEC NO-UNDO.
   DEFINE INPUT PARAMETER ip-last-of-vend AS LOG NO-UNDO.

  
   
      iop-vend-tot = iop-vend-tot + (ap-payl.amt-paid - ap-payl.amt-disc).
            
      if ip-last-of-vend then do:
        if iop-vend-tot ne 0 or (tb_zero-ven = YES) then
        DO:
          CREATE tt-1099-m.
          ASSIGN tt-1099-m.vend-no   = vend.vend-no
                 tt-1099-m.vend-name = vend.NAME
                 tt-1099-m.vend-tax-id = vend.tax-id
                 tt-1099-m.vend-add1 = vend.add1
                 tt-1099-m.vend-add2 = vend.add2
                 tt-1099-m.vend-city-line = vend.city + ","
                                          + " " + vend.state
                                          + " " + vend.zip
                 tt-1099-m.vend-total = iop-vend-tot.
          RELEASE tt-1099-m.
        END.
              
        iop-vend-tot  = 0.
      END.

END PROCEDURE.
