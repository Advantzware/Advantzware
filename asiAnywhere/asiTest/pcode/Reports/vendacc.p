

/*------------------------------------------------------------------------
    File        : vendacc.p
    Purpose     : AP Accounts By Vendor
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttAPAccountsByVendor NO-UNDO
        FIELD vendacc AS CHAR
        FIELD abc  AS CHAR .
        
       

DEFINE DATASET dsAPAccountsByVendor FOR ttAPAccountsByVendor.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvendacc       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend       AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPAccountsByVendor.

     IF prmUser      = ? THEN ASSIGN    prmUser      = "".   
     IF prmvendacc   = ? THEN ASSIGN    prmvendacc   = "". 
     IF prmbegvend   = ? THEN ASSIGN    prmbegvend   = "". 
     IF prmbegdt     = ? THEN ASSIGN    prmbegdt     = "". 
     IF prmbegact    = ? THEN ASSIGN    prmbegact    = "".
     IF prmendvend   = ? THEN ASSIGN    prmendvend   = "". 
     IF prmenddt     = ? THEN ASSIGN    prmenddt     = "". 
     IF prmendact    = ? THEN ASSIGN    prmendact    = "".
     IF prmOut       = ? THEN ASSIGN    prmOut       = "". 
     
     

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_acct AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE end_vend   AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 no-UNDO.
DEFINE VARIABLE end_acct   AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" NO-UNDO.



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

DEF TEMP-TABLE tt-report FIELD actnum LIKE account.actnum
                         FIELD vend-no LIKE vend.vend-no
                         FIELD inv-no LIKE ap-inv.inv-no
                         FIELD jrnl LIKE gltrans.jrnl
                         FIELD tr-date LIKE ap-ledger.tr-date
                         FIELD trnum LIKE ap-ledger.trnum
                         FIELD amt AS DEC
                         FIELD dscr LIKE ap-invl.dscr
                         FIELD file-id AS INT
                         FIELD row-id AS ROWID
                         INDEX detail actnum vend-no inv-no jrnl
                         INDEX row-id row-id.

DEF STREAM excel.




  IF prmvendacc = "vendacc" THEN DO:
     
        ASSIGN
        v-today        = TODAY
        begin_vend     = prmbegvend
        begin_date     = date(prmbegdt)  
        begin_acct     = prmbegact 
        end_vend       = prmendvend
        end_date       = date(prmenddt)  
        end_acct       = prmendact  .
        
        
        
        
        
        

          
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'VendAcc' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'VendAcc' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttAPAccountsByVendor.

        IF prmOut = "Yes" THEN
        ASSIGN ttAPAccountsByVendor.vendacc = v-excel-file.
        ELSE
            ASSIGN ttAPAccountsByVendor.vendacc = lv-txt-file .


 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
{sys/form/r-topw.f}   

DEF VAR lv-jrnl LIKE gltrans.jrnl NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-bank-code LIKE bank.bank-code NO-UNDO.
DEF VAR lv-check-no AS CHAR NO-UNDO.
DEF VAR li-check-no LIKE ap-pay.check-no NO-UNDO.
DEF VAR li-line LIKE ap-payl.line NO-UNDO.
DEF VAR li-lines AS INT NO-UNDO.
DEF VAR lv-amt LIKE tt-report.amt EXTENT 3 NO-UNDO.
DEF VAR lv-excel-descr AS CHAR NO-UNDO.
DEF VAR v-void-date AS DATE NO-UNDO.
DEF VAR v-temp-date AS DATE NO-UNDO.
DEF VAR v-refnum AS CHAR.
DEF BUFFER bf-ap-ledger FOR ap-ledger.

&SCOPED-DEFINE where-ap-pay                             ~
        WHERE ap-pay.company    EQ cocode               ~
          AND ap-pay.c-no       EQ ap-payl.c-no         ~
          AND ap-pay.vend-no    EQ ap-ledger.vend-no    ~
          AND ap-pay.check-date EQ ap-ledger.ref-date   ~
          AND ap-pay.posted     EQ YES                  ~
          AND ap-pay.memo       EQ YES

FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
     account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)"
     tt-report.vend-no     COLUMN-LABEL "Vendor"      
     /*vend.name             COLUMN-LABEL "Name"        FORMAT "x(20)"*/
     tt-report.inv-no      COLUMN-LABEL "Inv#"
     tt-report.jrnl        COLUMN-LABEL "Journal"
     tt-report.trnum       COLUMN-LABEL "Run#"
     tt-report.tr-date     COLUMN-LABEL "Date"
     tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"
   
    WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
 str-tit2 = "AP Accounts by Vendor"
 {sys/inc/ctrtext.i str-tit2 112}. 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.
       

{sys/inc/outprint.i VALUE(lines-per-page)}


FOR EACH tt-report:
  DELETE tt-report.
END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","       
       "GL Acct#"
       "Description"
       "Vendor"
       "Inv#"
       "Journal"
       "Run#"
       "Date"
       "Amount"
       SKIP.
END.

DISPLAY "" WITH FRAME r-top.

FOR EACH ap-ledger
    WHERE ap-ledger.company EQ cocode
      AND ap-ledger.vend-no GE begin_vend
      AND ap-ledger.vend-no LE end_vend
      AND ap-ledger.vend-no NE ""
      AND ap-ledger.tr-date GE begin_date
      AND ap-ledger.tr-date LE end_date
    NO-LOCK:

  IF ap-ledger.refnum BEGINS "INV# " THEN DO:
    FIND FIRST ap-inv
        WHERE ap-inv.company EQ ap-ledger.company
          AND ap-inv.vend-no EQ ap-ledger.vend-no
          AND ap-inv.inv-no  EQ SUBSTR(ap-ledger.refnum,6,20)
        NO-LOCK NO-ERROR.

    lv-jrnl = "ACPAY".

    IF AVAIL ap-inv THEN DO:
      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ap-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ap-ctrl.payables
       tt-report.amt    = (ap-inv.net + ap-inv.freight) * -1.

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no = ap-inv.inv-no
       tt-report.jrnl   = lv-jrnl
       tt-report.actnum = ap-ctrl.freight
       tt-report.amt    = ap-inv.freight.

      FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no:
        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-inv.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-invl.actnum
         tt-report.amt     = ap-invl.amt
         tt-report.dscr    = ap-invl.dscr
         tt-report.file-id = 1
         tt-report.row-id  = ROWID(ap-invl).
      END.
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "MEMO#" THEN DO:
    lv-jrnl = "APMEM".
    
    FOR EACH ap-payl
        WHERE ap-payl.inv-no EQ SUBSTR(ap-ledger.refnum,6,20)
          AND NOT CAN-FIND(FIRST tt-report WHERE tt-report.row-id EQ ROWID(ap-payl))
        NO-LOCK,

        FIRST ap-pay {&where-ap-pay} NO-LOCK
        
        BREAK BY ap-payl.c-no:

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-ctrl.payables
       tt-report.amt     = ap-payl.amt-paid - ap-payl.amt-disc
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-payl.actnum
       tt-report.amt     = (ap-payl.amt-paid - ap-payl.amt-disc) * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "CHK# " AND
     LENGTH(ap-ledger.refnum) GE 11  THEN DO:
    ASSIGN
     lv-jrnl      = "CDISB"
     lv-bank-code = ""
     lv-check-no  = "".

    DO li = 1 TO LENGTH(ap-ledger.refnum):
      IF LENGTH(TRIM(lv-check-no)) GE 4                                AND
         SUBSTR(lv-check-no,LENGTH(TRIM(lv-check-no)) - 3,4) EQ " CD#" THEN
        lv-bank-code = lv-bank-code + SUBSTR(ap-ledger.refnum,li,1).
      ELSE
        lv-check-no  = lv-check-no + SUBSTR(ap-ledger.refnum,li,1).
    END.
    ASSIGN
     lv-check-no = SUBSTR(lv-check-no,6,LENGTH(TRIM(lv-check-no)) - 9)
     li-check-no = INT(lv-check-no) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company   EQ ap-ledger.company
          AND bank.bank-code EQ lv-bank-code
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
          AND ap-pay.check-act EQ bank.actnum
          AND ap-pay.check-no  EQ li-check-no
          AND ap-pay.vend-no   EQ ap-ledger.vend-no
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,
        
        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = bank.actnum
       tt-report.amt     = ap-payl.amt-paid * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-payl.actnum
       tt-report.amt     = ap-payl.amt-paid
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "AC" THEN DO:
    ASSIGN
     lv-jrnl     = "APCKR" 
     li-check-no = INT(SUBSTR(ap-ledger.refnum,3,8)) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
          AND ap-pay.check-act EQ bank.actnum
          AND ap-pay.check-no  EQ li-check-no
          AND ap-pay.vend-no   EQ ap-ledger.vend-no
          AND ap-pay.bank-code EQ bank.bank-code
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,
        
        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = bank.actnum
       tt-report.amt     = ap-payl.amt-paid * -1
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).


      IF ap-payl.amt-disc NE 0 THEN DO:
        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-ctrl.discount
         tt-report.amt     = ap-payl.amt-disc * -1
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).
      END.

      CREATE tt-report.
      BUFFER-COPY ap-ledger TO tt-report
      ASSIGN
       tt-report.inv-no  = ap-payl.inv-no
       tt-report.jrnl    = lv-jrnl
       tt-report.actnum  = ap-ctrl.payables
       tt-report.amt     = ap-payl.amt-paid + ap-payl.amt-disc
       tt-report.file-id = 2
       tt-report.row-id  = ROWID(ap-payl).
    END.
  END.

  ELSE
  IF ap-ledger.refnum BEGINS "VOIDED CHECK" THEN DO:
    ASSIGN
     lv-jrnl     = "APVOIDCK" 
     li-check-no = INT(SUBSTR(ap-ledger.refnum,13,8)) NO-ERROR.
     
    IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company    EQ ap-ledger.company
          AND ap-pay.check-act  EQ bank.actnum
          AND ap-pay.check-no   EQ li-check-no
          AND ap-pay.vend-no    EQ ap-ledger.vend-no
          AND ap-pay.bank-code  EQ bank.bank-code
          AND ap-pay.cleared    EQ YES
          AND ap-pay.reconciled EQ ?
          AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK:
      
      ASSIGN
       li-lines = 0
       li-line  = 0
       li       = 0.

      FOR EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:
        li-lines = li-lines + 1.
      END.
      li-lines = li-lines / 2.

      FOR EACH ap-payl FIELDS(LINE) WHERE
          ap-payl.c-no EQ ap-pay.c-no
          NO-LOCK
          BY ap-payl.line:
        ASSIGN
         li-line = ap-payl.line
         li      = li + 1.
        IF li GE li-lines THEN LEAVE.
      END.

      FOR EACH ap-payl
          WHERE ap-payl.c-no EQ ap-pay.c-no
            AND ap-payl.line LE li-line
          NO-LOCK:

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = bank.actnum
         tt-report.amt     = ap-payl.amt-paid
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).

        IF ap-payl.amt-disc NE 0 THEN DO:
          CREATE tt-report.
          BUFFER-COPY ap-ledger TO tt-report
          ASSIGN
           tt-report.inv-no  = ap-payl.inv-no
           tt-report.jrnl    = lv-jrnl
           tt-report.actnum  = ap-ctrl.discount
           tt-report.amt     = ap-payl.amt-disc
           tt-report.file-id = 2
           tt-report.row-id  = ROWID(ap-payl).
        END.

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
        ASSIGN
         tt-report.inv-no  = ap-payl.inv-no
         tt-report.jrnl    = lv-jrnl
         tt-report.actnum  = ap-ctrl.payables
         tt-report.amt     = (ap-payl.amt-paid + ap-payl.amt-disc) * -1
         tt-report.file-id = 2
         tt-report.row-id  = ROWID(ap-payl).
      END.
    END.
  END.
END.

FOR EACH tt-report
    WHERE tt-report.actnum GE begin_acct
      AND tt-report.actnum LE end_acct
      AND tt-report.actnum NE ""
      AND tt-report.amt    NE 0
    USE-INDEX detail
    BREAK BY tt-report.actnum
          BY tt-report.vend-no
          BY tt-report.inv-no
          BY tt-report.jrnl:

  FIND FIRST account
      WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.actnum
      NO-LOCK NO-ERROR.

  FIND FIRST vend
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ tt-report.vend-no
      NO-LOCK NO-ERROR.

  lv-amt[1] = lv-amt[1] + tt-report.amt.

  DISPLAY tt-report.actnum WHEN FIRST-OF(tt-report.actnum)
          account.dscr WHEN AVAIL account AND FIRST-OF(tt-report.actnum)
              "Not on File" WHEN NOT AVAIL account AND FIRST-OF(tt-report.actnum)
                @ account.dscr
      WITH FRAME detail.
  lv-excel-descr = account.dscr.
  IF tt-report.dscr NE "" THEN DO WITH FRAME detail:
    IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN DOWN.
    DISPLAY tt-report.dscr @ account.dscr.
    lv-excel-descr = tt-report.dscr.
  END.

  DISPLAY tt-report.vend-no WHEN FIRST-OF(tt-report.vend-no)
          /*vend.name WHEN AVAIL vend AND FIRST-OF(tt-report.vend-no)
              "Not on File" WHEN NOT AVAIL vend AND FIRST-OF(tt-report.vend-no)
                @ vend.name*/
          tt-report.inv-no
          tt-report.jrnl
          tt-report.trnum
          tt-report.tr-date
          tt-report.amt
      WITH FRAME detail.
  DOWN WITH FRAME detail.

  IF tb_excel THEN  
      EXPORT STREAM excel DELIMITER ","
      /*  gdm - 11130906
            (IF FIRST-OF(tt-report.actnum) 
                THEN tt-report.actnum
                ELSE "")

            (IF AVAIL account THEN 
                IF FIRST-OF(tt-report.actnum) 
                   THEN account.dscr
                   ELSE ""
             ELSE 
                "Not on file")         

            (IF FIRST-OF(tt-report.vend-no)
                THEN tt-report.vend-no
                ELSE "")
      */    
            tt-report.actnum
            /* account.dscr */ lv-excel-descr
            tt-report.vend-no
       /* gdm - 11130906 end */

            tt-report.inv-no
            tt-report.jrnl
            tt-report.trnum
            tt-report.tr-date
            tt-report.amt 
            SKIP.

  IF LAST-OF(tt-report.vend-no) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "  Vendor" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[1]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Vendor Totals"
              " "
              " "
              " "                           
              lv-amt[1]
              SKIP.

    ASSIGN
     lv-amt[2] = lv-amt[2] + lv-amt[1]
     lv-amt[1] = 0.

    PUT SKIP(1).
  END.

  IF LAST-OF(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Acct#" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[2]  @ tt-report.amt.
    DOWN.

    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Acct# Totals"
              " "
              " "
              " "                          
              lv-amt[2]
              SKIP.

    ASSIGN lv-amt[3] = lv-amt[3] + lv-amt[2]
           lv-amt[2] = 0.

    PUT SKIP(3).
  END.

  IF LAST(tt-report.actnum) THEN DO WITH FRAME detail:
    PUT SKIP(1).

    UNDERLINE tt-report.amt.
    DISPLAY "   Grand" @ tt-report.vend-no
            "Totals"   @ tt-report.inv-no
            lv-amt[3]  @ tt-report.amt.
    DOWN.
  
    IF tb_excel THEN  
       EXPORT STREAM excel DELIMITER ","
              " "
              " "
              " "
              "Grand Totals"
              " "
              " "
              " "                            
              lv-amt[3]
              SKIP.    
    
  END.
END.



IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   
END.


/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

END PROCEDURE.
