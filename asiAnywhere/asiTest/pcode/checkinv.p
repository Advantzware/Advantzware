
/*------------------------------------------------------------------------
    File        : checkinv.p
    Purpose     : Checks/Invoice
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{sys/inc/VAR.i NEW SHARED}
DEFINE TEMP-TABLE ttChecksInvoice NO-UNDO
    FIELD invno       AS CHAR
    FIELD chkno       AS INT
    FIELD chkdate     AS CHAR
    FIELD duedate     AS CHAR
    FIELD grosamt     AS DEC
    FIELD amtdisc     AS DEC
    FIELD amtpaid     AS DEC
    

    FIELD chkbal      AS CHAR 
    FIELD vname       AS CHAR
    FIELD temp        AS CHAR
    FIELD reckey      AS CHAR

    .

DEFINE DATASET dsChecksInvoice FOR ttChecksInvoice.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminvno       AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmchkno       AS INT   NO-UNDO.
DEFINE INPUT PARAMETER prmchkdate     AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmduedate     AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER prmgrosamt     AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmamtdisc     AS DEC    NO-UNDO.
DEFINE INPUT PARAMETER prmamtpaid     AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmchkbal      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvend        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvendname    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmbeginv      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmendinv      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmbegcchk     AS INT   NO-UNDO.
DEFINE INPUT PARAMETER prmendchk      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmbegdate     AS CHAR   NO-UNDO.
DEFINE INPUT PARAMETER prmenddate     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmshrtby      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmout         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcon         AS CHAR  NO-UNDO.

DEFINE INPUT PARAMETER prmReckey      AS CHAR  NO-UNDO.
          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsChecksInvoice .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


IF prmAction      = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp        = ?  THEN ASSIGN prmComp       = "".
IF prmUser        = ?  THEN ASSIGN prmUser       = "".
IF prminvno       = ?  THEN ASSIGN prminvno      = "".
IF prmchkno       = ?  THEN ASSIGN prmchkno      = 0.
IF prmchkdate     = ?  THEN ASSIGN prmchkdate    = "". 
IF prmduedate     = ?  THEN ASSIGN prmduedate    = "". 
IF prmgrosamt     = ?  THEN ASSIGN prmgrosamt    = 0.
IF prmamtdisc     = ?  THEN ASSIGN prmamtdisc    = 0.
IF prmamtpaid     = ?  THEN ASSIGN prmamtpaid    = 0.
IF prmchkbal      = ?  THEN ASSIGN prmchkbal     = "".
IF prmvend        = ?  THEN ASSIGN prmvend       = "".
IF prmvendname    = ?  THEN ASSIGN prmvendname   = "".
IF prmbeginv      = ?  THEN ASSIGN prmbeginv     = "".
IF prmendinv      = ?  THEN ASSIGN prmendinv     = "".
IF prmbegcchk     = ?  THEN ASSIGN prmbegcchk    = 0.
IF prmendchk      = ?  THEN ASSIGN prmendchk     = 0.
IF prmbegdate     = ?  THEN ASSIGN prmbegdate    = "".
IF prmenddate     = ?  THEN ASSIGN prmenddate    = "".
IF prmshrtby      = ?  THEN ASSIGN prmshrtby     = "".
IF prmout         = ?  THEN ASSIGN prmout        = "".
IF prmcon         = ?  THEN ASSIGN prmcon        = "".
IF prmReckey      = ?  THEN ASSIGN prmReckey     = "".


DEF VAR tmp-dir AS cha NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  
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


DEF TEMP-TABLE tt-report    LIKE report
    FIELD check-no          LIKE ap-pay.check-no
    FIELD check-date        LIKE ap-pay.check-date
    FIELD inv-no            LIKE ap-payl.inv-no
    FIELD due-date          LIKE ap-payl.due-date
    FIELD gross-amt         LIKE ap-payl.amt-paid
    FIELD amt-disc          LIKE ap-payl.amt-disc
    FIELD amt-paid          LIKE ap-payl.amt-paid
    FIELD ap-inv-rec-key AS CHAR
    FIELD vend-no AS CHAR.

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.
DEF BUFFER b-vend FOR vend.
DEF VAR char-hdl AS CHAR NO-UNDO.
def var phandle as widget-handle no-undo.

DEF BUFFER b-ap-inv FOR ap-inv.

DEFINE VARIABLE fi_fchk     AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE fi_fdate    AS DATE FORMAT "99/99/99" NO-UNDO.
DEFINE VARIABLE fi_finv     AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE fi_name     AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE fi_sortby   AS CHARACTER FORMAT "X(256)":U  NO-UNDO.
DEFINE VARIABLE fi_tchk     AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE fi_tdate    AS DATE FORMAT "99/99/99" NO-UNDO.
DEFINE VARIABLE fi_tinv     AS CHARACTER FORMAT "x(12)" INITIAL "zzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE fi_vend     AS CHARACTER FORMAT "X(8)":U  NO-UNDO.

ASSIGN
        fi_fchk   = prmbegcchk
        fi_fdate  = date(prmbegdate)
        fi_finv   = prmbeginv
        fi_name   = prmvendname
        fi_sortby = prmshrtby
        fi_tchk   = prmendchk
        fi_tdate  = date(prmenddate)
        fi_tinv   = prmendinv
        fi_vend   = prmvend . 




RUN create-tempfile.




FUNCTION validDate RETURNS LOGICAL
  ( INPUT pdDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  

      /* If no date range was entered, then include check. */
      IF prmbegdate = "" AND prmenddate = "" THEN
          RETURN TRUE.

      /* If start date is entered and check date is earlier, then omit. */
      IF prmbegdate <> "" AND 
          pdDate < date(prmbegdate) THEN
          RETURN FALSE.

      /* If end date is entered and check date is later, then omit. */
      IF prmenddate <> "" AND 
          pdDate > date(prmenddate) THEN
          RETURN FALSE.



  /* Otherwise include check. */
  RETURN TRUE.   /* Function return value. */

END FUNCTION.



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

        CREATE ttChecksInvoice.
           ASSIGN 
                 ttChecksInvoice.invno        = tt-report.inv-no
                 ttChecksInvoice.chkno        = tt-report.check-no
                 ttChecksInvoice.chkdate      = string(tt-report.check-date)
                 ttChecksInvoice.duedate      = string(tt-report.due-date)
                 ttChecksInvoice.grosamt      = tt-report.gross-amt
                 ttChecksInvoice.amtdisc      = tt-report.amt-disc
                 ttChecksInvoice.amtpaid      = tt-report.amt-paid .

            FIND FIRST vend WHERE
           vend.company EQ cocode AND
          vend.vend-no EQ tt-report.vend-no
                NO-LOCK NO-ERROR.

            IF AVAIL vend THEN
                ASSIGN
                ttChecksInvoice.reckey = string(vend.rec_key) /*tt-report.ap-inv-rec-key*/   .
            
    END. /*FOR EACH cust  */
END. /*IF prmAction = "Select" THEN DO:*/



IF prmchkbal = "view" THEN do:

    FIND vend NO-LOCK
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ prmvend
        NO-ERROR.
    IF AVAIL vend THEN do: 

        CREATE ttChecksInvoice.
        ASSIGN
            ttChecksInvoice.vname= vend.name.
    END.
END.


IF prmchkbal = "chkbal" THEN DO:
    
    ASSIGN
        fi_fchk   = prmbegcchk
        fi_fdate  = date(prmbegdate)
        fi_finv   = prmbeginv
        fi_name   = prmvendname
        fi_sortby = prmshrtby
        fi_tchk   = prmendchk
        fi_tdate  = date(prmenddate)
        fi_tinv   = prmendinv
        fi_vend   = prmvend . 


        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "Chkbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "Chkbal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        RUN run-report.





        CREATE ttChecksInvoice.
        ASSIGN ttChecksInvoice.chkbal = vTextFile .

END.

   
  


/*****************************procedure**********************************/

PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.

  DEF VAR lv-vend  LIKE vend.vend-no                      NO-UNDO.
  DEF VAR lv-name  LIKE vend.NAME                         NO-UNDO.
  DEF VAR lv-fchk  LIKE ap-pay.check-no FORMAT ">>>>>>>>" NO-UNDO.
  DEF VAR lv-tchk  LIKE lv-fchk                           NO-UNDO.
  DEF VAR lv-finv  LIKE ap-payl.inv-no                    NO-UNDO.
  DEF VAR lv-tinv  LIKE lv-finv                           NO-UNDO.

  FORM SKIP(1)
       lv-vend COLON 30 LABEL "Vendor#"
       lv-name          NO-LABEL
       lv-fchk COLON 30 LABEL "Beginning Check#"
       lv-tchk COLON 30 LABEL "Ending Check#"
       lv-finv COLON 30 LABEL "Beginning Inv#"
       lv-tinv COLON 30 LABEL "Ending Inv#"
       SKIP(1)

      WITH STREAM-IO WIDTH 80 FRAME ap-chkh SIDE-LABELS NO-UNDERLINE PAGE-TOP
           TITLE "                 V E N D O R     C H E C K S / I N V O I C E S                ".

  FORM tt-report.check-no    FORMAT ">>>>>>>>"  COLUMN-LABEL "Check#"
       tt-report.check-date  FORMAT "99/99/99"  COLUMN-LABEL "Chk Date"
       tt-report.inv-no                         COLUMN-LABEL "Invoice#"
       tt-report.due-date    FORMAT "99/99/99"  COLUMN-LABEL "Due Date"
       tt-report.gross-amt                      COLUMN-LABEL "Gross Amt"
       tt-report.amt-disc                       COLUMN-LABEL "Discount"
       tt-report.amt-paid                       COLUMN-LABEL "Net Amt"
     
      WITH NO-BOX FRAME ap-chk DOWN WIDTH 80 STREAM-IO.

  
  v-print-fmt = sys-ctrl.char-fld.

  FIND FIRST tt-report NO-ERROR.
 
  IF AVAIL tt-report THEN DO WITH FRAME ap-chkh:
     
        
    /*{sys/inc/print1.i}*/
      if tmp-dir = "" then tmp-dir = v-webrootpath .
      assign list-name = tmp-dir + vTextFile
          init-dir = tmp-dir.


    {sys/inc/outprint.i 56} 
 
    DISPLAY fi_vend @ lv-vend
            fi_name @ lv-name
            fi_fchk @ lv-fchk
            fi_tchk @ lv-tchk
            fi_finv @ lv-finv
            fi_tinv @ lv-tinv.
 
    FOR EACH tt-report NO-LOCK
        WITH FRAME ap-chk
        BREAK BY tt-report.key-01
              BY tt-report.key-02:

      IF tt-report.key-03 EQ "TOTAL" THEN DO:
        UNDERLINE tt-report.check-no
                  tt-report.check-date
                  tt-report.gross-amt
                  tt-report.amt-disc
                  tt-report.amt-paid. 
        DOWN.
    
        CLEAR NO-PAUSE.
      END.
  
      DISPLAY tt-report.check-no
              tt-report.check-date
              tt-report.inv-no
              tt-report.due-date
              tt-report.gross-amt
              tt-report.amt-disc
              tt-report.amt-paid.
      DOWN.

      IF LAST-OF(tt-report.key-02) THEN DOWN 2.
    END.

    OUTPUT CLOSE.

   /* run scr-rpt.w (list-name,TRIM(FRAME ap-chkh:TITLE),11,"P"). /* open file-name, title */ END.*/
  END.
END PROCEDURE.


PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR t-vendor     LIKE ap-pay.vend-no.
DEF VAR f-chk        LIKE ap-pay.check-no FORMAT ">>>>>>>>" INIT 0.
DEF VAR t-chk        LIKE f-chk INIT 99999999.
DEF VAR f-inv        LIKE ap-payl.inv-no INIT "".
DEF VAR t-inv        LIKE f-inv INIT "zzzzzzzzzzzz".
DEF VAR v-check-no   LIKE ap-pay.check-no.
DEF VAR v-disc-amt   LIKE ap-payl.amt-disc.
DEF VAR v-gross-amt  LIKE ap-payl.amt-paid.

DEF VAR li AS INT NO-UNDO.


ASSIGN
 t-vendor    = fi_vend
 f-inv       = fi_finv
 t-inv       = fi_tinv
 f-chk       = fi_fchk
 t-chk       = fi_tchk.

FOR EACH tt-report:
  DELETE tt-report.
END.

pay-block:
FOR EACH ap-pay
    WHERE ap-pay.company  EQ cocode
      AND ap-pay.vend-no  EQ t-vendor
      AND ap-pay.check-no GE f-chk
      AND ap-pay.check-no LE t-chk
      AND ap-pay.posted   EQ YES
      AND ap-pay.memo     EQ NO
    USE-INDEX vend-no NO-LOCK,
      
    EACH ap-payl
    WHERE ap-payl.c-no    EQ ap-pay.c-no
      AND ap-payl.inv-no  GE f-inv
      AND ap-payl.inv-no  LE t-inv
    NO-LOCK
      
    BREAK BY ap-pay.check-act
          BY ap-pay.check-no
          BY ap-payl.inv-no
          BY ap-payl.amt-paid:

    IF NOT validDate(ap-pay.check-date) THEN NEXT pay-block.

    li = li + 1.

    IF FIRST-OF(ap-pay.check-no) THEN v-check-no = ap-pay.check-no.
    
    v-gross-amt = v-gross-amt + (ap-payl.amt-paid + ap-payl.amt-disc).
    
    IF LAST-OF(ap-payl.inv-no) THEN DO:
        CREATE tt-report.
        ASSIGN
            tt-report.key-01     = ap-pay.check-act
            tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
            tt-report.check-no   = v-check-no
            tt-report.check-date = IF v-check-no NE 0 THEN ap-pay.check-date ELSE ?
                tt-report.inv-no     = ap-payl.inv-no
                tt-report.due-date   = ap-payl.due-date
                tt-report.gross-amt  = ap-payl.amt-paid + ap-payl.amt-disc
                tt-report.amt-disc   = ap-payl.amt-disc
                tt-report.amt-paid   = ap-payl.amt-paid
                tt-report.vend-no = ap-payl.vend-no
                v-disc-amt = v-disc-amt + ap-payl.amt-disc
                v-check-no = 0.

       FIND FIRST b-ap-inv WHERE
            b-ap-inv.company EQ ap-pay.company AND
            b-ap-inv.vend-no EQ ap-payl.vend-no AND
            b-ap-inv.inv-no  EQ ap-payl.inv-no
            NO-LOCK NO-ERROR.

       IF AVAIL b-ap-inv THEN
          tt-report.ap-inv-rec-key = b-ap-inv.rec_key.
    END. /* IF LAST-OF(ap-payl.inv-no) */
            
    IF LAST-OF(ap-pay.check-no) THEN DO:
        IF NOT FIRST-OF(ap-pay.check-no) OR v-gross-amt EQ 0 THEN DO:
            CREATE tt-report.
            ASSIGN
                tt-report.key-01     = ap-pay.check-act
                tt-report.key-02     = STRING(ap-pay.check-no,"9999999999")
                tt-report.key-03     = "TOTAL"
                tt-report.check-no   = ap-pay.check-no
                tt-report.check-date = ap-pay.check-date
                tt-report.inv-no     = IF v-gross-amt EQ 0 THEN "Void" ELSE ""
                tt-report.due-date   = ap-payl.due-date
                tt-report.gross-amt  = ap-pay.check-amt + v-disc-amt
                tt-report.amt-disc   = v-disc-amt
                tt-report.amt-paid   = ap-pay.check-amt.
        END. /* IF NOT FIRST-OF(ap-pay.check-no) */

        ASSIGN
            v-disc-amt  = 0
            v-gross-amt = 0. 
    END. /* IF LAST-OF(ap-pay.check-no) */
END. /* for each ap-pay */

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
