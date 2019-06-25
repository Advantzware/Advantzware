

/*------------------------------------------------------------------------
    File        : invbyvend.p
    Purpose     : AP Invoices By Vendor
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttAPInvoicesByVendor NO-UNDO
        FIELD vendinv AS CHAR
        FIELD abc  AS CHAR .
        
       

DEFINE DATASET dsAPInvoicesByVendor FOR ttAPInvoicesByVendor.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvendinv       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend       AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrun           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPInvoicesByVendor.

     IF prmUser      = ? THEN ASSIGN    prmUser      = "".   
     IF prmvendinv   = ? THEN ASSIGN    prmvendinv   = "". 
     IF prmbegvend   = ? THEN ASSIGN    prmbegvend   = "". 
     IF prmbegdt     = ? THEN ASSIGN    prmbegdt     = "". 
     IF prmendvend   = ? THEN ASSIGN    prmendvend   = "".
     IF prmenddt     = ? THEN ASSIGN    prmenddt     = "". 
     IF prmrun       = ? THEN ASSIGN    prmrun       = "". 
     IF prmOut       = ? THEN ASSIGN    prmOut       = "".
     
     
     

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_vend   AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 no-UNDO.
DEFINE VARIABLE tb_by-invdt AS LOGICAL INITIAL no NO-UNDO.



DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
def var list-name as cha no-undo.
def var list-name2 as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
DEF VAR t-setup AS LOG INITIAL NO NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEFINE VARIABLE tb_runExcel   AS LOGICAL INITIAL no NO-UNDO.

DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 g_company = prmComp
 vuser     = prmUser .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
     NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

DEF TEMP-TABLE tt-ap-inv NO-UNDO LIKE ap-inv    
    FIELD name LIKE vend.name. 

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

DEF STREAM excel.




  IF prmvendinv = "vendinv" THEN DO:
     
        ASSIGN
        v-today        = TODAY
        begin_vend     = prmbegvend
        begin_date     = DATE(prmbegdt)  
        end_vend       = prmendvend
        end_date       = DATE(prmenddt)  
        tb_by-invdt    = IF prmrun = "True" THEN TRUE ELSE FALSE    .
           
          
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'InvByVend' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  +  ".txt" .
       
        
        v-excel-file =  'InvByVend' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttAPInvoicesByVendor.

        IF prmOut = "Yes" THEN
        ASSIGN ttAPInvoicesByVendor.vendinv = v-excel-file.
        ELSE
            ASSIGN ttAPInvoicesByVendor.vendinv = lv-txt-file .


 


  END.
/*****************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

DEF VAR lv-total AS DEC EXTENT 2 NO-UNDO.
DEF VAR ll-total AS LOG EXTENT 2 NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.



ASSIGN
 str-tit2 = "AP Invoices by Vendor"
 {sys/inc/ctrtext.i str-tit2 112}. 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.
       

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vendor,Name,Inv#,Date,Amount".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.



DISPLAY "" WITH FRAME r-top.

EMPTY TEMP-TABLE tt-ap-inv.
FOR EACH ap-inv
    WHERE ap-inv.company  EQ cocode
      AND ap-inv.vend-no  GE begin_vend
      AND ap-inv.vend-no  LE end_vend
      AND ((ap-inv.inv-date GE begin_date 
      AND ap-inv.inv-date LE end_date) OR tb_by-invdt)      
      AND ap-inv.posted   EQ YES
    NO-LOCK
    BREAK BY ap-inv.vend-no
          BY ap-inv.inv-date
          BY ap-inv.inv-no : 

  FIND FIRST vend
      WHERE vend.company EQ ap-inv.company
        AND vend.vend-no EQ ap-inv.vend-no
      NO-LOCK NO-ERROR.
  
  FIND first ap-ledger
      where ap-ledger.company  eq vend.company
      and ap-ledger.vend-no  eq ap-inv.vend-no
      and ap-ledger.ref-date eq ap-inv.inv-date
      and ap-ledger.refnum   eq ("INV# " + ap-inv.inv-no)      
      and ((ap-ledger.tr-date GE begin_date AND ap-ledger.tr-date le end_date) OR NOT tb_by-invdt)
      use-index ap-ledger NO-LOCK NO-ERROR.

  IF NOT AVAIL ap-ledger THEN NEXT.    
  
  CREATE tt-ap-inv NO-ERROR.
  BUFFER-COPY ap-inv TO tt-ap-inv
  ASSIGN 
    tt-ap-inv.NAME = vend.NAME.
END.

FOR EACH tt-ap-inv NO-LOCK
    BREAK BY tt-ap-inv.vend-no
          BY tt-ap-inv.inv-date
          BY tt-ap-inv.inv-no      

  WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132:
  DISPLAY tt-ap-inv.vend-no        COLUMN-LABEL "Vendor"
                                   WHEN FIRST-OF(tt-ap-inv.vend-no)
          tt-ap-inv.name           COLUMN-LABEL "Name"
                                   WHEN FIRST-OF(tt-ap-inv.vend-no) /*AND AVAIL vend*/
          tt-ap-inv.inv-no         COLUMN-LABEL "Inv#"
          tt-ap-inv.inv-date       COLUMN-LABEL "Date"
          tt-ap-inv.net            COLUMN-LABEL "Amount"
                                   FORMAT "->>>,>>>,>>>,>>9.99".
  
  IF tb_excel THEN
     PUT STREAM excel UNFORMATTED
       '"' (IF FIRST-OF(tt-ap-inv.vend-no) THEN tt-ap-inv.vend-no
            ELSE "")                                         '",'
       '"' (IF FIRST-OF(tt-ap-inv.vend-no) THEN tt-ap-inv.NAME
            ELSE "")                                         '",'
       '"' tt-ap-inv.inv-no                                     '",'
       '"' (IF tt-ap-inv.inv-date NE ? THEN STRING(tt-ap-inv.inv-date,'99/99/9999')
            ELSE "")                                         '",'
       '"' STRING(tt-ap-inv.net,'->>>,>>>,>>>,>>9.99')          '",'
       SKIP(1).

  lv-total[1] = lv-total[1] + tt-ap-inv.net.

  IF NOT FIRST-OF(tt-ap-inv.vend-no) THEN ll-total[1] = YES.

  IF LAST-OF(tt-ap-inv.vend-no) THEN DO:
    IF ll-total[1] THEN DO:
      UNDERLINE tt-ap-inv.net.
      DISPLAY /*ap-inv.vend-no  
              vend.name       WHEN AVAIL vend*/
              "Vendor Total"  @ tt-ap-inv.inv-no
              lv-total[1]     @ tt-ap-inv.net.
      DOWN.

      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
           '"' ""                                        '",'
           '"' ""                                        '",'
           '"' ""                                        '",'
           '"' ""                                        '",'
           '"' "--------------------"                    '",'
           SKIP
           '"' ""                                        '",'
           '"' ""                                        '",'
           '"' "Vendor Total"                            '",'
           '"' ""                                        '",'
           '"' STRING(lv-total[1],'->>>,>>>,>>>,>>9.99') '",'
           SKIP(1).
      
    END.
    PUT SKIP(2).
    ASSIGN
     lv-total[2] = lv-total[2] + lv-total[1]
     lv-total[1] = 0
     ll-total[1] = NO.
    IF NOT LAST(tt-ap-inv.vend-no) THEN ll-total[2] = YES.
  END.

  IF LAST(tt-ap-inv.vend-no) AND ll-total[2] THEN DO:
    UNDERLINE tt-ap-inv.net.
    UNDERLINE tt-ap-inv.net.
    DISPLAY "Grand Total"   @ tt-ap-inv.inv-no
            lv-total[2]     @ tt-ap-inv.net.
    DOWN.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' "--------------------"                    '",'
          SKIP
          '"' ""                                        '",'
          '"' ""                                        '",'
          '"' "Grand Total"                             '",'
          '"' ""                                        '",'
          '"' STRING(lv-total[2],'->>>,>>>,>>>,>>9.99') '",'
          SKIP(1).
  END.
END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  
END.


/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

END PROCEDURE.
