
/*------------------------------------------------------------------------
    File        : invbal.p
    Purpose     : Invoice Balance
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{sys/inc/VAR.i NEW SHARED}
DEFINE TEMP-TABLE ttInvoiceBalance NO-UNDO
    FIELD invno       AS CHAR
    FIELD chkno       AS CHAR
    FIELD trnsdate    AS CHAR
    FIELD dscr        AS CHAR
    FIELD credit      AS DEC
    FIELD debit       AS DEC
    FIELD balance     AS DEC
    FIELD po          AS CHAR

    FIELD invbal      AS CHAR 
    FIELD vname       AS CHAR
    FIELD temp        AS CHAR
    FIELD reckey      AS CHAR

    .

DEFINE DATASET dsInvoiceBalance FOR ttInvoiceBalance.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvend     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmInv      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmchkno       AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmtrnsdate    AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmdscr        AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmcredit      AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmdebit       AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmbalance     AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmpo          AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prminvbal      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvendname    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmbeginv      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmendinv      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmupto        AS INT   NO-UNDO.
DEFINE INPUT PARAMETER prmvopen       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmshrtby      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmout         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcon         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey      AS CHAR  NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoiceBalance .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction        = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp       = "".
IF prmUser          = ?  THEN ASSIGN prmUser       = "".
IF prmvend          = ?  THEN ASSIGN prmvend       = "".
IF prmInv           = ?  THEN ASSIGN prmInv        = "".
IF prmchkno         = ?  THEN ASSIGN prmchkno      = "". 
IF prmtrnsdate      = ?  THEN ASSIGN prmtrnsdate   = "". 
IF prmdscr          = ?  THEN ASSIGN prmdscr       = "".
IF prmcredit        = ?  THEN ASSIGN prmcredit     = 0.
IF prmdebit         = ?  THEN ASSIGN prmdebit      = 0.
IF prmbalance       = ?  THEN ASSIGN prmbalance    = 0.
IF prmpo            = ?  THEN ASSIGN prmpo         = "".
IF prminvbal        = ?  THEN ASSIGN prminvbal     = "".
IF prmvendname      = ?  THEN ASSIGN prmvendname   = "".
IF prmbeginv        = ?  THEN ASSIGN prmbeginv     = "".
IF prmendinv        = ?  THEN ASSIGN prmendinv     = "".
IF prmupto          = ?  THEN ASSIGN prmupto       = 0.
IF prmvopen         = ?  THEN ASSIGN prmvopen      = "".
IF prmshrtby        = ?  THEN ASSIGN prmshrtby     = "".
IF prmout           = ?  THEN ASSIGN prmout        = "".
IF prmcon           = ?  THEN ASSIGN prmcon        = "".
IF prmReckey        = ?  THEN ASSIGN prmReckey     = "".


DEF VAR tmp-dir AS cha NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF BUFFER b-vend FOR vend.
DEF BUFFER b-ap-inv FOR ap-inv.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var phandle as widget-handle no-undo.
  
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode 
           v-today   = TODAY .

  {sys/inc/apsecure.i}


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

DEF TEMP-TABLE tt-report NO-UNDO /*LIKE report*/
    FIELD key-01 LIKE report.key-01
    FIELD key-02 LIKE report.key-02
    FIELD check-no   AS   CHAR
    FIELD trans-date LIKE ap-inv.inv-date
    FIELD dscr       AS   CHAR
    FIELD credits    AS   DEC
    FIELD debits     AS   DEC
    FIELD balance    AS   DEC
    FIELD cr-db      AS   LOG
    FIELD add-sub    AS   LOG
    FIELD po-no      AS   CHAR
    FIELD ap-inv-rec-key AS CHAR
    FIELD vend-no AS CHAR
    FIELD inv-no AS CHAR
  INDEX key-01 IS PRIMARY key-01
  INDEX key-02 key-02
  INDEX check-no check-no
  INDEX trans-date trans-date
  INDEX dscr dscr
  INDEX credits credits
  INDEX debits debits
  INDEX balance balance
  INDEX po-no po-no.

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

{sa/sa-sls01.i}


DEFINE VARIABLE fi_days   AS INTEGER FORMAT ">,>>9":U INITIAL 9999 NO-UNDO.
DEFINE VARIABLE fi_finv   AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE fi_name   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U NO-UNDO.
DEFINE VARIABLE fi_tinv   AS CHARACTER FORMAT "x(12)" INITIAL "zzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE fi_vend   AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE VARIABLE tb_open   AS LOGICAL INITIAL YES NO-UNDO.

       ASSIGN
        fi_days   = prmupto
        fi_finv   = prmbeginv
        fi_name   = prmvendname
        fi_sortby = prmshrtby
        fi_tinv   = prmendinv
        fi_vend   = prmvend
        tb_open   = IF prmvopen = "Yes" THEN TRUE ELSE FALSE .

   
      RUN create-tempfile.


/*IF AVAIL tt-report AND tt-report.ap-inv-rec-key NE "" THEN
  DO:
    /* {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
                   "(tt-report.ap-inv-rec-key,tt-report.inv-no)"} */

     FIND FIRST b-vend WHERE
          b-vend.company EQ cocode AND
          b-vend.vend-no EQ tt-report.vend-no
          NO-LOCK NO-ERROR.
    
     IF AVAIL b-vend THEN
        RUN pushpin-image-proc(INPUT b-vend.rec_key).
  END.
  ELSE
     RUN pushpin-image-proc(INPUT ""). */

IF prmAction = "validatevend" THEN DO:

    prmvend = CAPS(prmvend).
        IF NOT CAN-FIND(FIRST vend WHERE vend.company EQ cocode
                        AND vend.vend-no EQ prmvend)
            THEN DO:
            cError = "Invalid entry, try help..." .
            RETURN.
        END.
END. /*prmAction = "validatevend"*/

IF prmAction = "Search" THEN DO:
    
     FOR EACH tt-report NO-LOCK :

        CREATE ttInvoiceBalance.
           ASSIGN 
                 ttInvoiceBalance.invno        = tt-report.key-02
                 ttInvoiceBalance.chkno        = tt-report.check-no
                 ttInvoiceBalance.trnsdate     = string(tt-report.trans-date)
                 ttInvoiceBalance.dscr         = tt-report.dscr
                 ttInvoiceBalance.credit       = tt-report.credits
                 ttInvoiceBalance.debit        = tt-report.debits 
                 ttInvoiceBalance.balance      = tt-report.balance
                 ttInvoiceBalance.po           = tt-report.po-no 
                 ttInvoiceBalance.reckey       = tt-report.ap-inv-rec-key    .
            
           FIND FIRST vend WHERE
               vend.company EQ cocode AND
               vend.vend-no EQ tt-report.vend-no
               NO-LOCK NO-ERROR.

           IF AVAIL vend THEN
               ASSIGN
               ttInvoiceBalance.temp       = string(vend.rec_key) .


    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/

FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "APINQ"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "APINQ"
     sys-ctrl.descrip  = "AP Inquiry Print Method"
     sys-ctrl.char-fld = "Triad".
    cError = "System control record NOT found. " .
            UPDATE sys-ctrl.char-fld.
  END.

IF prminvbal = "view" THEN do:

    FIND vend NO-LOCK
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ prmvend
        NO-ERROR.
    IF AVAIL vend THEN do: 

        CREATE ttInvoiceBalance.
        ASSIGN
            ttInvoiceBalance.vname= vend.name.
    END.
END.


IF prminvbal = "invbal" THEN DO:
    
    ASSIGN
        fi_days   = prmupto
        fi_finv   = prmbeginv
        fi_name   = prmvendname
        fi_sortby = prmshrtby
        fi_tinv   = prmendinv
        fi_vend   = prmvend
        tb_open   = IF prmvopen = "Yes" THEN TRUE ELSE FALSE .

        

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "Invbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "Invbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        RUN run-report.


        CREATE ttInvoiceBalance.
        ASSIGN ttInvoiceBalance.invbal = vTextFile .

END.

   
  


/*****************************procedure**********************************/

PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.

  DEF VAR lv-vend  LIKE vend.vend-no    NO-UNDO.
  DEF VAR lv-name  LIKE vend.NAME       NO-UNDO.
  DEF VAR lv-finv  LIKE ap-inv.inv-no   NO-UNDO.
  DEF VAR lv-tinv  LIKE lv-finv         NO-UNDO.
  DEF VAR li-days  AS   INT             NO-UNDO.
  DEF VAR ll-open  AS   LOG             NO-UNDO.
  DEF VAR lv-dots  AS   CHAR            NO-UNDO.
  DEF VAR lv-po-no LIKE tt-report.po-no NO-UNDO.

  FORM SKIP(1)
       lv-vend COLON 30 LABEL "Vendor#"
       lv-name          NO-LABEL
       lv-finv COLON 30 LABEL "Beginning Inv#"
       lv-tinv COLON 30 LABEL "Ending Inv#"
       li-days COLON 30 LABEL "Up to How Many Days Old?"
       ll-open COLON 30 LABEL "Open Invoices Only?"
       SKIP(1)

      WITH STREAM-IO WIDTH 80 FRAME ap-balh SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "       V E N D O R   A C C O U N T S       ".

    
  
  v-print-fmt = sys-ctrl.char-fld.
      
  FIND FIRST tt-report NO-ERROR.

  IF AVAIL tt-report THEN DO WITH FRAME ap-balh:
    

    lv-dots = FILL(".",80).
        
   /* {sys/inc/print1.i}*/
    if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.


    {sys/inc/outprint.i 56}

    DISPLAY fi_vend @ lv-vend
            fi_name @ lv-name
            fi_finv @ lv-finv
            fi_tinv @ lv-tinv
            fi_days @ li-days
            tb_open @ ll-open.

    FOR EACH tt-report NO-LOCK BREAK BY tt-report.key-01 BY tt-report.trans-date:
      DISPLAY tt-report.key-02     FORMAT "x(12)"          COLUMN-LABEL "Inv#/Check#"
                tt-report.check-no WHEN tt-report.key-02 EQ "" @ tt-report.key-02
              tt-report.trans-date FORMAT "99/99/99"       COLUMN-LABEL "Date"
              tt-report.dscr       FORMAT "x(8)"           COLUMN-LABEL "Descript"
              tt-report.credits    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Credits"
              tt-report.debits     FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Debits"
              tt-report.balance    FORMAT "->>,>>>,>>>.99" COLUMN-LABEL "Balance"
          WITH FRAME ap-bal NO-BOX STREAM-IO WIDTH 80 DOWN.
      DOWN WITH FRAME ap-bal.

      IF tt-report.po-no NE "" THEN
        lv-po-no = TRIM(lv-po-no) + " " + tt-report.po-no.

      IF LAST-OF(tt-report.key-01) THEN DO:
        IF v-print-fmt EQ "Brick" AND lv-po-no NE "" THEN
          PUT "P.O. NUMBER(S)" FORMAT "x(14)"
              SPACE(1)
              TRIM(lv-po-no)   FORMAT "x(65)"
              SKIP.

        lv-po-no = "".

        DISPLAY lv-dots          @ tt-report.key-02
                lv-dots          @ tt-report.trans-date
                lv-dots          @ tt-report.dscr
                ".............." @ tt-report.credits
                ".............." @ tt-report.debits
                ".............." @ tt-report.balance
            WITH FRAME ap-bal.
        DOWN WITH FRAME ap-bal.
      END.
    END.

    OUTPUT CLOSE.

    

   /* run scr-rpt.w (list-name,TRIM(FRAME ap-balh:TITLE),11,"P"). /* open file-name, title */ */
  END.
END PROCEDURE.



PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var balance as dec format "->>>,>>9.99".
def var t-dscr   as CHAR.
def var x-check-no   as char.
def var t-credits    as dec  format ">,>>>,>>>.99"  column-label "Credits" .
def var t-debits     as dec  format ">,>>>,>>>.99"  column-label "Debits"  .
def var t-balance    as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var work-bal     as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var bal-diff     as dec  format ">>,>>>,>>>.99" column-label "Balance" .
def var t-vendor     like  vend.vend-no.
def var f-inv        like  ap-inv.inv-no init "            ".
def var t-inv        like  ap-inv.inv-no init "ZZZZZZZZZZZZ".
def var t-o          as log format "Y/N" init "N".
def var num-day-old  as dec format ">>>9" init 9999.
def var show-bal like t-balance.
DEF VAR lv-key LIKE tt-report.key-01 NO-UNDO.
DEF VAR lv-po-no LIKE tt-report.po-no NO-UNDO.
DEF VAR v-refnum AS CHAR NO-UNDO.

ASSIGN
 t-vendor    = fi_vend
 f-inv       = fi_finv
 t-inv       = fi_tinv
 num-day-old = fi_days
 t-o         = tb_open.

FOR EACH tt-report:
  DELETE tt-report.
END.

    for each ap-inv
        where ap-inv.company  eq cocode
          and ap-inv.vend-no  eq t-vendor
          and ap-inv.inv-no   ge f-inv
          and ap-inv.posted   eq yes
          and ap-inv.inv-no   le t-inv
          and ap-inv.inv-date ge (today - num-day-old)
        no-lock
          
        by ap-inv.inv-date by ap-inv.inv-no:

      ASSIGN
         show-bal = ap-inv.net + ap-inv.freight
         lv-key = STRING(YEAR(ap-inv.inv-date),"9999") +
                  STRING(MONTH(ap-inv.inv-date),"99")  +
                  STRING(DAY(ap-inv.inv-date),"99")    +
                  ap-inv.inv-no.

      if ap-inv.due ne 0 or not t-o then do:
        CREATE tt-report.
        ASSIGN
         tt-report.key-01     = lv-key
         tt-report.key-02     = ap-inv.inv-no
         tt-report.trans-date = ap-inv.inv-date
         tt-report.dscr       = "Invoice"
         tt-report.debits     = ap-inv.net + ap-inv.freight
         tt-report.balance    = t-balance + tt-report.debits
         tt-report.cr-db      = NO
         tt-report.add-sub    = YES
         tt-report.ap-inv-rec-key = ap-inv.rec_key
         tt-report.vend-no    = ap-inv.vend-no
         tt-report.inv-no     = ap-inv.inv-no.
                         
        for each ap-payl
            where ap-payl.inv-no   eq ap-inv.inv-no
              and ap-payl.vend-no  eq ap-inv.vend-no
              and ap-payl.posted   eq YES
            no-lock,
               
            first ap-pay
            where ap-pay.company eq cocode 
              and ap-pay.c-no    eq ap-payl.c-no
            NO-LOCK:
              
          t-dscr = "Payment".
            
          if ap-payl.memo then t-dscr = "CR MEMO".
            
          if ap-payl.amt-paid            lt 0  and
             ap-payl.memo                eq no and
             ap-inv.net + ap-inv.freight gt 0  then t-dscr = "Void Chk".
               
          if ap-payl.amt-paid ne 0 then do:
            ASSIGN
            t-credits  = ap-payl.amt-paid
            t-balance  = t-balance - ap-payl.amt-paid
            x-check-no = string(ap-payl.check-no).

            CREATE tt-report.
            ASSIGN
             tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-credits
             tt-report.cr-db      = yes
             tt-report.add-sub    = NO
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

            FIND FIRST b-ap-inv WHERE
                 b-ap-inv.company EQ ap-pay.company AND
                 b-ap-inv.vend-no EQ ap-payl.vend-no AND
                 b-ap-inv.inv-no  EQ ap-payl.inv-no
                 NO-LOCK NO-ERROR.

            IF AVAIL b-ap-inv THEN
               tt-report.ap-inv-rec-key = b-ap-inv.rec_key.

            IF t-dscr NE "Void Chk" THEN
               tt-report.trans-date = ap-pay.check-date.
            ELSE
            DO:
               v-refnum = "VOIDED CHECK"
                        + string(ap-pay.check-no, "zzzzzzz9").

               FIND FIRST ap-ledger WHERE
                    ap-ledger.company EQ cocode AND
                    ap-ledger.vend-no EQ ap-pay.vend-no AND
                    ap-ledger.refnum = v-refnum
                    NO-LOCK NO-ERROR.

               IF AVAIL ap-ledger THEN
               DO:
                  tt-report.trans-date = ap-ledger.tr-date.
                  RELEASE ap-ledger.
               END.
               ELSE
                  tt-report.trans-date = ap-pay.check-date.

            END.
          end. /* if ap-payl.amt-paid ne 0 */

          if ap-payl.amt-disc ne 0 then do:
            if ap-payl.memo then t-debits = ap-payl.amt-disc.
            else t-credits = ap-payl.amt-disc.

            ASSIGN
            t-dscr = "Discount"
            x-check-no = string(ap-payl.check-no).
              
            if ap-payl.memo then do:

              ASSIGN
                 t-dscr = "DB MEMO"
                 t-balance = t-balance + ap-payl.amt-disc.
                
              create tt-report.
              assign
               tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
               tt-report.check-no   = x-check-no
               tt-report.trans-date = ap-pay.check-date
               tt-report.dscr       = t-dscr
               tt-report.credits    = t-debits
               tt-report.cr-db      = no
               tt-report.add-sub    = YES
               tt-report.vend-no    = ap-payl.vend-no
               tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
            end. /* if ap-payl.memo */

            else do:
              t-balance = t-balance - ap-payl.amt-disc.

              create tt-report.
              assign
               tt-report.key-01     = string(lv-key,"x(100)") + "a" +
                                      STRING(ap-payl.c-no,"9999999999") +
                                      STRING(ap-payl.line,"9999999999")
               tt-report.check-no   = x-check-no
               tt-report.trans-date = ap-pay.check-date
               tt-report.dscr       = t-dscr
               tt-report.credits    = t-credits
               tt-report.cr-db      = yes
               tt-report.add-sub    = NO
               tt-report.vend-no    = ap-payl.vend-no
               tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
            end. /* else do: */
          end. /* if ap-payl.amt-disc ne 0 */
        end. /* for each ap-payl */
      end. /* if (t-o and ap-inv.due ne 0) */

      for each ap-payl
          where ap-payl.inv-no  eq "0"
            and ap-payl.posted  eq yes
            and ap-payl.vend-no eq vend.vend-no
          no-lock,
            
          first ap-pay
          where ap-pay.company eq cocode 
            and ap-pay.c-no eq ap-payl.c-no
          NO-LOCK:
            
        t-dscr = "Payment".
        if ap-payl.memo then t-dscr = "CR MEMO".

        if ap-payl.amt-paid ne 0 then do:
          t-credits = ap-payl.amt-paid.
          t-balance = t-balance - ap-payl.amt-paid.
          x-check-no = string(ap-payl.check-no).
            
          create tt-report.
          assign
           tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                  STRING(ap-payl.c-no,"9999999999") +
                                  STRING(ap-payl.line,"9999999999")
           tt-report.check-no   = x-check-no
           tt-report.trans-date = ap-pay.check-date
           tt-report.dscr       = t-dscr
           tt-report.credits    = t-credits
           tt-report.cr-db      = yes
           tt-report.add-sub    = NO
           tt-report.vend-no    = ap-payl.vend-no
           tt-report.inv-no     = ap-payl.inv-no.

          FIND FIRST b-ap-inv WHERE
               b-ap-inv.company EQ ap-pay.company AND
               b-ap-inv.vend-no EQ ap-payl.vend-no AND
               b-ap-inv.inv-no  EQ ap-payl.inv-no
               NO-LOCK NO-ERROR.

          IF AVAIL b-ap-inv THEN
             tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
        end. /* if ap-payl.amt-paid ne 0 */

        if ap-payl.amt-disc ne 0 then do:
          if ap-payl.memo then t-debits  = ap-payl.amt-disc.
          else t-credits = ap-payl.amt-disc.
          t-dscr = "Discount".
          if not ap-payl.memo then t-balance = t-balance - ap-payl.amt-disc.
            x-check-no = string(ap-payl.check-no).

          if ap-payl.memo then do:

            ASSIGN
            t-dscr = "DB MEMO"
            t-balance = t-balance + ap-payl.amt-disc.
              
            create tt-report.
            assign
             tt-report.key-01     = string(lv-key,"x(100)") + "a" +
                                    STRING(ap-payl.c-no,"9999999999") +
                                    STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.trans-date = ap-pay.check-date
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-debits
             tt-report.cr-db      = no
             tt-report.add-sub    = YES
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
          end. /* if ap-payl.memo then do */
            
          else do:
            create tt-report.
            assign
             tt-report.key-01     = string(lv-key,"x(100)") + "a" + 
                                  STRING(ap-payl.c-no,"9999999999") +
                                  STRING(ap-payl.line,"9999999999")
             tt-report.check-no   = x-check-no
             tt-report.trans-date = ap-pay.check-date
             tt-report.dscr       = t-dscr
             tt-report.credits    = t-credits
             tt-report.cr-db      = yes
             tt-report.add-sub    = NO
             tt-report.vend-no    = ap-payl.vend-no
             tt-report.inv-no     = ap-payl.inv-no.

              FIND FIRST b-ap-inv WHERE
                   b-ap-inv.company EQ ap-pay.company AND
                   b-ap-inv.vend-no EQ ap-payl.vend-no AND
                   b-ap-inv.inv-no  EQ ap-payl.inv-no
                   NO-LOCK NO-ERROR.

              IF AVAIL b-ap-inv THEN
                 tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
          end. /* else do: */
        end. /* if ap-payl.amt-disc ne 0 */
      end. /* for each ap-payl */

      RELEASE tt-report.

      bal-diff = work-bal.
      if ap-inv.due ne 0 or not t-o THEN work-bal = work-bal + ap-inv.net + ap-inv.freight.

      lv-po-no = "".

      FOR EACH tt-report WHERE tt-report.key-01 BEGINS lv-key BREAK BY tt-report.key-01:

        if tt-report.add-sub then work-bal = work-bal + tt-report.credits.
        else work-bal = work-bal - tt-report.credits.

        if tt-report.dscr EQ "Invoice" then
          work-bal = work-bal - tt-report.credits.

        tt-report.balance = work-bal.
  
        if tt-report.dscr ne "Invoice" AND
           NOT tt-report.cr-db THEN
          ASSIGN
           tt-report.debits  = tt-report.credits
           tt-report.credits = 0.
          
        if FIRST(tt-report.key-01) then do:
          if ap-inv.po-no eq 0 then
          for each ap-invl
              where ap-invl.i-no  eq ap-inv.i-no
                and ap-invl.po-no ne 0
              break by ap-invl.po-no:
              
            IF LAST-OF(ap-invl.po-no) THEN
              lv-po-no = TRIM(lv-po-no) + " " +
                         TRIM(STRING(ap-invl.po-no,">>>>>>>>>>")).
          end.
            
          else lv-po-no = TRIM(STRING(ap-inv.po-no,">>>>>>>>>>")).

          tt-report.po-no = TRIM(lv-po-no).
        END.  
      end. /* for each tt-report break by tt-report.key-01 */
       
      if (show-bal ne work-bal and work-bal ne bal-diff) or
         (show-bal eq work-bal and bal-diff eq 0)        then do:
        CREATE tt-report.
        ASSIGN
         tt-report.key-01     = string(lv-key,"x(100)") + "z"
         tt-report.key-02     = "Balance Due"
         tt-report.trans-date = ap-inv.due-date
         tt-report.balance    = work-bal.                                                
      end. /* if (show-bal ne work-bal and */      
    end. /* for each ap-inv */

END PROCEDURE.

PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.
   
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR v-inv-no AS CHAR NO-UNDO.

   IF AVAIL tt-report THEN
      ASSIGN
         v-inv-no = STRING(tt-report.inv-no,"X(12)") + "APINV"
         v-att = CAN-FIND(FIRST attach WHERE
                 attach.company = cocode and
                 attach.rec_key = ip-rec_key AND
                 (attach.est-no eq v-inv-no OR ATTACH.est-no EQ "")).
   

  

   
END PROCEDURE.
