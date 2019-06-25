

/*------------------------------------------------------------------------
    File        : apchkrg.p
    Purpose     : AP Check Register
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttAPCheckRegister NO-UNDO
        FIELD apchk AS CHAR
        FIELD abc  AS CHAR .
        
       

DEFINE DATASET dsAPCheckRegister FOR ttAPCheckRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmapchk         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegchk        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegbnk        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend       AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendchk        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendbnk        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmglact         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrunpst        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPCheckRegister.

     IF prmUser      = ? THEN ASSIGN    prmUser      = "".   
     IF prmapchk     = ? THEN ASSIGN    prmapchk     = "". 
     IF prmbegvend   = ? THEN ASSIGN    prmbegvend   = "". 
     IF prmbegdt     = ? THEN ASSIGN    prmbegdt     = "". 
     IF prmbegchk    = ? THEN ASSIGN    prmbegchk    = 0.
     IF prmbegbnk    = ? THEN ASSIGN    prmbegbnk    = "". 
     IF prmendvend   = ? THEN ASSIGN    prmendvend   = "". 
     IF prmenddt     = ? THEN ASSIGN    prmenddt     = "".
     IF prmendchk    = ? THEN ASSIGN    prmendchk    = 0. 
     IF prmendbnk    = ? THEN ASSIGN    prmendbnk    = "". 
     IF prmglact     = ? THEN ASSIGN    prmglact     = "". 
     IF prmrunpst    = ? THEN ASSIGN    prmrunpst    = "".
     IF prmOut       = ? THEN ASSIGN    prmOut       = "". 
     
     

DEFINE VARIABLE begin_vend  AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_check AS INTEGER FORMAT ">>>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE Begin_Bank  AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE end_vend    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_date    AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 no-UNDO.
DEFINE VARIABLE end_check   AS INTEGER FORMAT ">>>>>>>>>":U INITIAL 999999999 NO-UNDO.
DEFINE VARIABLE End_Bank    AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE tb_prt-acc  AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_post-date AS LOGICAL INITIAL no NO-UNDO.


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
DEFINE VARIABLE excelHeader AS CHARACTER NO-UNDO.

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


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR ll-secure AS LOG NO-UNDO.

DEF TEMP-TABLE tt-report    LIKE report
    FIELD check-no          LIKE ap-pay.check-no
    FIELD check-date        LIKE ap-pay.check-date
    FIELD vend-no           LIKE ap-pay.vend-no
    FIELD vend-name         LIKE vend.NAME
    FIELD inv-no            LIKE ap-payl.inv-no
    FIELD due-date          LIKE ap-payl.due-date
    FIELD gross-amt         LIKE ap-payl.amt-paid
    FIELD amt-disc          LIKE ap-payl.amt-disc
    FIELD amt-paid          LIKE ap-payl.amt-paid
    FIELD LINE              AS INT
    FIELD c-no              AS INT
    FIELD row-id            AS ROWID.

DEF TEMP-TABLE tt-report2 LIKE tt-report.

DEFINE BUFFER b-tt-report FOR tt-report.

DEFINE STREAM excel.

DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
FIND FIRST ap-ctrl WHERE ap-ctrl.company = cocode NO-LOCK NO-WAIT NO-ERROR.
IF AVAIL ap-ctrl THEN ASSIGN v-frt-acct = ap-ctrl.freight.




  IF prmapchk = "apchk" THEN DO:
     
        ASSIGN
        v-today         = TODAY
        begin_vend      = prmbegvend
        begin_date      = date(prmbegdt)
        begin_check     = prmbegchk 
        Begin_Bank      = prmbegbnk 
        end_vend        = prmendvend  
        end_date        = date(prmenddt)  
        end_check       = prmendchk 
        End_Bank        = prmendbnk 
        tb_prt-acc      = IF prmglact = "True" THEN TRUE ELSE FALSE
        tb_post-date    = IF prmrunpst = "True" THEN TRUE ELSE FALSE  .
        
        

       
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'APChkReg' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt" .
       
        
        v-excel-file =  'APChkReg' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .

        
        run run-report. 

        CREATE ttAPCheckRegister.

        IF prmOut = "Yes" THEN
        ASSIGN ttAPCheckRegister.apchk = v-excel-file.
        ELSE
            ASSIGN ttAPCheckRegister.apchk = lv-txt-file .


 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
{sys/form/r-topw.f}

DEF VAR v-check-no LIKE ap-pay.check-no NO-UNDO.
DEF VAR v-amt-disc  LIKE ap-payl.amt-disc NO-UNDO.
DEF VAR v-gross-amt LIKE ap-payl.amt-paid NO-UNDO.
DEF VAR v-amt-paid LIKE ap-payl.amt-paid NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-vend-name LIKE vend.NAME NO-UNDO.

FORM tt-report.check-no    FORMAT ">>>>>>>>"  COLUMN-LABEL "Check#"
     tt-report.check-date  FORMAT "99/99/99"  COLUMN-LABEL "Chk Date"
     tt-report.inv-no                         COLUMN-LABEL "Invoice#"
     tt-report.vend-no                        COLUMN-LABEL "Vendor#"
     tt-report.vend-name                      COLUMN-LABEL "Name"
     tt-report.due-date    FORMAT "99/99/99"  COLUMN-LABEL "Due Date"
     tt-report.gross-amt                      COLUMN-LABEL "Gross Amt"
     tt-report.amt-disc                       COLUMN-LABEL "Discount"
     tt-report.amt-paid                       COLUMN-LABEL "Net Amt"
     
    WITH NO-BOX FRAME ap-chk DOWN WIDTH 180 STREAM-IO.  




ASSIGN
 str-tit2 = "AP Check Register"
 {sys/inc/ctrtext.i str-tit2 112}. 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}


DISPLAY "" WITH FRAME r-top.

FOR EACH tt-report:
  DELETE tt-report.
END.

EMPTY TEMP-TABLE tt-report2.

FOR EACH ap-pay
    WHERE ap-pay.company    EQ cocode
      AND ap-pay.vend-no    GE begin_vend
      AND ap-pay.vend-no    LE end_vend
      AND ((ap-pay.check-date GE begin_date
      AND ap-pay.check-date LE end_date) OR tb_post-date)  
      AND ap-pay.check-no   GE begin_check
      AND ap-pay.check-no   LE end_check
      AND ap-pay.bank-code  GE begin_bank
      AND ap-pay.bank-code  LE end_bank
      AND ap-pay.posted     EQ YES
      AND ap-pay.memo       EQ NO
    USE-INDEX vend-no NO-LOCK,
      
    EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK
      
    BREAK BY ap-pay.check-act
          BY ap-pay.check-no
          BY ap-payl.inv-no
          BY ap-payl.line
          BY ap-payl.amt-paid:

    FIND first ap-ledger
      where ap-ledger.company  eq ap-pay.company
      and ap-ledger.vend-no  eq ap-pay.vend-no      
      and ap-ledger.refnum   eq ("INV# " + ap-payl.inv-no)      
      and ((ap-ledger.tr-date GE begin_date AND ap-ledger.tr-date le end_date) OR NOT tb_post-date)
      use-index ap-ledger NO-LOCK NO-ERROR.

    IF NOT AVAIL ap-ledger THEN NEXT.


  ASSIGN
     v-vend-name = ""
     li = li + 1.

  IF FIRST-OF(ap-pay.check-no) THEN v-check-no = ap-pay.check-no.

  v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).

  IF FIRST-OF(ap-payl.inv-no) THEN DO:
    CREATE tt-report.

    /* rtc start */
    FIND FIRST ap-dis WHERE ap-dis.company   EQ ap-pay.company
                        AND ap-dis.check-no  EQ ap-pay.check-no
                        AND ap-dis.bank-code EQ ap-pay.bank-code
                        AND ap-dis.vend-no   EQ ap-pay.vend-no NO-LOCK NO-ERROR. 
     IF AVAILABLE(ap-dis) THEN
         v-vend-name = ap-dis.payee.
     ELSE DO: 
        FIND FIRST vend WHERE vend.company EQ cocode 
                          AND vend.vend-no EQ ap-pay.vend-no NO-LOCK NO-ERROR.
        IF AVAILABLE(vend) THEN
           v-vend-name = vend.NAME.
     END.
  
    /* rtc end */

    ASSIGN
     tt-report.rec-id     = RECID(ap-pay)
     tt-report.key-01     = ap-pay.check-act
     tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
     tt-report.key-03     = ap-payl.inv-no
     tt-report.check-no   = v-check-no
     tt-report.check-date = IF v-check-no NE 0 THEN ap-pay.check-date ELSE ?
     tt-report.inv-no     = ap-payl.inv-no
     tt-report.due-date   = ap-payl.due-date
     tt-report.gross-amt  = ap-payl.amt-paid + ap-payl.amt-disc
     tt-report.amt-disc   = ap-payl.amt-disc
     tt-report.amt-paid   = ap-payl.amt-paid
     tt-report.c-no       = ap-payl.c-no
     tt-report.LINE       = ap-payl.LINE
     tt-report.vend-no    = ap-pay.vend-no
     tt-report.vend-name  = v-vend-name
     v-amt-disc           = v-amt-disc + ap-payl.amt-disc
     v-check-no = 0
     tt-report.row-id       = ROWID(ap-pay).

     RELEASE vend.
  END.
            
  IF LAST-OF(ap-pay.check-no) THEN DO:
    IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN DO:
      CREATE tt-report.

      FIND FIRST vend WHERE
           vend.company EQ cocode AND
           vend.vend-no EQ ap-pay.vend-no
           NO-LOCK NO-ERROR.

       /* rtc start */
       FIND FIRST ap-dis WHERE ap-dis.company   EQ ap-pay.company
                           AND ap-dis.check-no  EQ ap-pay.check-no
                           AND ap-dis.bank-code EQ ap-pay.bank-code
                           AND ap-dis.vend-no   EQ ap-pay.vend-no NO-LOCK NO-ERROR. 
        IF AVAILABLE(ap-dis) THEN
            v-vend-name = ap-dis.payee.
        ELSE DO: 
           FIND FIRST vend WHERE vend.company EQ cocode 
                             AND vend.vend-no EQ ap-pay.vend-no NO-LOCK NO-ERROR.
           IF AVAILABLE(vend) THEN
              v-vend-name = vend.NAME.
        END.
       /* rtc end */

      ASSIGN
       tt-report.key-01     = ap-pay.check-act
       tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
       tt-report.key-03     = FILL("z",100) + "TOTAL"
       tt-report.check-no   = ap-pay.check-no
       tt-report.check-date = ap-pay.check-date
       tt-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
       tt-report.due-date   = ap-payl.due-date
       tt-report.gross-amt  = ap-pay.check-amt + v-amt-disc
       tt-report.amt-disc   = v-amt-disc
       tt-report.amt-paid   = ap-pay.check-amt
       tt-report.vend-no    = ap-pay.vend-no
       tt-report.vend-name  = v-vend-name.

      IF tt-report.inv-no EQ "Void" THEN
        ASSIGN
         tt-report.gross-amt = tt-report.gross-amt * -1
         tt-report.amt-disc  = tt-report.amt-disc * -1
         tt-report.amt-paid  = tt-report.amt-paid * -1.

      RELEASE vend.
    END.

    ASSIGN
     v-gross-amt = 0
     v-amt-disc  = 0. 
  END.
END.

/* gdm - */

    
IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelHeader = 'Check#,Check Date,Vendor#,Name,Invoice#,Due Date,Gross Amt,Discount,Net Amt'.
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */

RELEASE tt-report.

ASSIGN
 v-gross-amt = 0
 v-amt-disc  = 0
 v-amt-paid  = 0.

IF CAN-FIND(FIRST tt-report) THEN
FOR EACH tt-report NO-LOCK WITH FRAME ap-chk
    BREAK BY tt-report.key-01
          BY tt-report.key-02
          BY tt-report.key-03:

  IF tt-report.key-03 EQ FILL("z",100) + "TOTAL" THEN DO:
    UNDERLINE tt-report.check-no
              tt-report.check-date
              tt-report.vend-no
              tt-report.vend-name
              tt-report.gross-amt
              tt-report.amt-disc
              tt-report.amt-paid. 
    DOWN.
    
    CLEAR NO-PAUSE.
  END.

  IF tt-report.key-03 NE FILL("z",100) + "TOTAL" OR tt-report.inv-no EQ "Void" THEN
    ASSIGN
     v-gross-amt = v-gross-amt + tt-report.gross-amt
     v-amt-disc  = v-amt-disc  + tt-report.amt-disc
     v-amt-paid  = v-amt-paid  + tt-report.amt-paid.

  DISPLAY tt-report.check-no
          tt-report.check-date
          tt-report.vend-no
          tt-report.vend-name
          tt-report.inv-no
          tt-report.due-date
          tt-report.gross-amt
          tt-report.amt-disc
          tt-report.amt-paid.
  DOWN.

  IF tb_excel THEN
    PUT STREAM excel UNFORMATTED
        '"' STRING(tt-report.check-no,">>>>>>>>") '",'
        '"' (IF tt-report.check-date NE ? THEN STRING(tt-report.check-date,"99/99/99") ELSE "") '",'
        '"' tt-report.vend-no '",'
        '"' tt-report.vend-name '",'
        '"' tt-report.inv-no '",'
        '"' (IF tt-report.due-date NE ? THEN STRING(tt-report.due-date,"99/99/9999") ELSE "") '",'
        '"' tt-report.gross-amt '",'
        '"' tt-report.amt-disc '",'
        '"' tt-report.amt-paid '"'
        SKIP.

  RELEASE vend NO-ERROR.
  
  IF LAST-OF(tt-report.key-02) THEN DOWN 2.

  IF LAST(tt-report.key-01) THEN DO:
    UNDERLINE tt-report.gross-amt
              tt-report.amt-disc
              tt-report.amt-paid. 
    DOWN.

    DISPLAY v-gross-amt @ tt-report.gross-amt
            v-amt-disc  @ tt-report.amt-disc
            v-amt-paid  @ tt-report.amt-paid.
  END.
END.

IF tb_prt-acc THEN DO: 
  PAGE. 
  RUN print-gl-acct. 
END.

OUTPUT STREAM excel CLOSE.



/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

END PROCEDURE.

PROCEDURE print-gl-acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-frgt-amt LIKE ap-inv.freight NO-UNDO.
DEF VAR v-line-amt LIKE ap-invl.amt NO-UNDO.
{sys/form/r-topw.f}

ASSIGN
 str-tit2 = "AP Check Register" + " - Summary by Account"
 {sys/inc/ctrtext.i str-tit2 112}.  

FORM HEADER
 "                                                          Summary by Account " SKIP(2)
 "ACCOUNT CHECK#                      PO#   DATE   VENDOR#  INVOICE#    "
 "LINE DESCRIPTION              QTY    UNIT PRICE     AMT PAID" SKIP
 "------- ----------------------- ------- -------- ------- ---------    "
 "---- --------------------- ------  ------ ------  ----------"
 WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top45 PAGE-TOP WIDTH 132 STREAM-IO.

  DISPLAY "" WITH FRAME f-top45.

FOR EACH tt-report:

    /*exclude voided checks*/
    IF NOT CAN-FIND(FIRST b-tt-report where
       b-tt-report.key-01 = tt-report.key-01 AND
       b-tt-report.key-02 = tt-report.key-02 AND
       b-tt-report.inv-no = "Void" AND
       ROWID(b-tt-report) NE ROWID(tt-report)) THEN
       DO:
          CREATE tt-report2.
          BUFFER-COPY tt-report TO tt-report2.
          RELEASE tt-report2.
       END.
END.

FOR EACH tt-report2,
    FIRST ap-pay NO-LOCK WHERE
      ROWID(ap-pay) EQ tt-report2.row-id,
    FIRST ap-payl WHERE
          ap-payl.c-no EQ tt-report2.c-no AND
          ap-payl.LINE EQ tt-report2.LINE
          NO-LOCK,
    FIRST ap-inv WHERE
          ap-inv.company EQ ap-pay.company AND
          ap-inv.vend-no EQ ap-payl.vend-no AND
          ap-inv.inv-no  EQ ap-payl.inv-no AND
          ap-inv.freight NE 0 NO-LOCK,
    FIRST vend NO-LOCK WHERE
          vend.company EQ ap-inv.company AND
          vend.vend-no EQ ap-inv.vend-no
          USE-INDEX vend
    BREAK BY ap-inv.vend-no
          BY ap-inv.inv-no:
   
    IF FIRST(ap-inv.inv-no) THEN DO:
       FIND FIRST account WHERE
            account.company EQ ap-inv.company AND
            account.actnum  EQ v-frt-acct
            NO-LOCK NO-ERROR.
    
       PUT v-frt-acct + " - " +
          (IF AVAIL account THEN account.dscr ELSE "Not on file") FORMAT "x(40)"
          SKIP.
    END. /* FIRST(ap-inv.vend-no)*/
     
    ASSIGN v-frgt-amt = ap-payl.amt-paid * 
                        (ap-inv.freight / (ap-inv.net + ap-inv.freight)).
    
    PUT ap-payl.check-no FORMAT ">>>>>>>>" AT 6
        ap-inv.inv-date         AT 41   FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(6)
        "Freight"                       FORMAT "x(18)"
        SPACE(7)
        1.0                             FORMAT "9.9"
        SPACE(1)
        ap-inv.freight          TO 118
        v-frgt-amt              TO 131
        SKIP.
    
    ACCUM v-frgt-amt (TOTAL).

    IF LAST(ap-inv.inv-no) THEN
       PUT "** TOTAL " TO 114
           (ACCUM TOTAL v-frgt-amt) FORMAT "->>,>>>,>>9.99" TO 128
           " *" SKIP(1).
    
END. /* FOR EACH tt-report */

EMPTY TEMP-TABLE tt-report2.

FOR EACH tt-report:

    /*exclude voided checks*/
    IF NOT CAN-FIND(FIRST b-tt-report where
       b-tt-report.key-01 = tt-report.key-01 AND
       b-tt-report.key-02 = tt-report.key-02 AND
       b-tt-report.inv-no = "Void" AND
       ROWID(b-tt-report) NE ROWID(tt-report)) THEN
       DO:
          CREATE tt-report2.
          BUFFER-COPY tt-report TO tt-report2.
          RELEASE tt-report2.
       END.
END.

FOR EACH tt-report2,
    FIRST ap-pay NO-LOCK WHERE
          ROWID(ap-pay) EQ tt-report2.row-id,
    FIRST ap-payl WHERE
          ap-payl.c-no EQ tt-report2.c-no AND
          ap-payl.LINE EQ tt-report2.LINE
          NO-LOCK,
    FIRST ap-inv WHERE
          ap-inv.company EQ ap-pay.company AND
          ap-inv.vend-no EQ ap-payl.vend-no AND
          ap-inv.inv-no  EQ ap-payl.inv-no
          NO-LOCK,
    FIRST vend WHERE
          vend.company EQ ap-inv.company AND
          vend.vend-no EQ ap-inv.vend-no
          NO-LOCK,
    EACH ap-invl WHERE
         ap-invl.i-no EQ ap-inv.i-no USE-INDEX i-no
         NO-LOCK
    BREAK BY ap-invl.actnum
          BY ap-invl.inv-no
          BY ap-invl.LINE
    WITH WIDTH 132 NO-LABELS:

    IF FIRST-OF(ap-invl.actnum) THEN DO:
       FIND FIRST account WHERE
            account.company eq ap-inv.company AND
            account.actnum  eq ap-invl.actnum
            NO-LOCK NO-ERROR.
     
       PUT ap-invl.actnum + " - " +
           (IF AVAIL account THEN account.dscr ELSE "Not on file") FORMAT "x(40)" SKIP.
    END.

    ASSIGN v-line-amt = ap-payl.amt-paid * 
                        (ap-invl.amt / (ap-inv.net + ap-inv.freight)).
    
    PUT ap-payl.check-no FORMAT ">>>>>>>>" AT 6
        ap-invl.po-no         AT 34
        SPACE(1)
        ap-inv.inv-date       FORMAT "99/99/99"
        SPACE(1)
        ap-inv.vend-no
        SPACE(1)
        ap-inv.inv-no
        SPACE(1)
        {ap/invlline.i -1}    FORMAT ">>>9"
        SPACE(1)
        ap-invl.dscr          FORMAT "x(18)"
        SPACE(1)
        ap-invl.qty           FORMAT "->>,>>9.9<<"
        SPACE(1)
        ap-invl.unit-pr
        SPACE(1)
        v-line-amt
        SPACE(1)
        SKIP.
    
    ACCUM v-line-amt (TOTAL BY ap-invl.actnum).
    ACCUM v-line-amt (TOTAL).

    IF LAST-OF(ap-invl.actnum) THEN
       PUT "** TOTAL " TO 114
           (ACCUM TOTAL BY ap-invl.actnum v-line-amt) FORMAT "->>,>>>,>>9.99" TO 128
           " *" SKIP(1).
END.

END PROCEDURE.


