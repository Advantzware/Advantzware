/* --------------------------------------------------- oe/crcheck.p 2/09 GDM  */
/* CUSTOMER CREDIT CHECK ONLY                                                 */
/* THIS WILL NOT UPDATE ANY RECORD IN THE DB. VALIDATION PURPOSES ONLY.       */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-aging AS LOG   NO-UNDO. 
DEF OUTPUT PARAM op-okflg AS LOG   NO-UNDO.

{sys/inc/var.i SHARED}

   
DEF VAR v-error         AS CHAR INIT "" NO-UNDO.
DEF VAR hold_invoice_id AS RECID INIT ? NO-UNDO.

DEF VAR ld-ord-bal      LIKE cust.ord-bal   NO-UNDO.
DEF VAR lv-cust         LIKE oe-ord.cust-no NO-UNDO.
DEF VAR lv-stat         LIKE oe-ord.stat    NO-UNDO.

DO TRANSACTION:
  {sys/inc/oecredit.i}
END.

ASSIGN op-okflg = NO.

FIND oe-ord NO-LOCK 
    WHERE ROWID(oe-ord) EQ ip-rowid NO-ERROR.
IF AVAIL oe-ord
  THEN
    ASSIGN
        lv-cust = oe-ord.cust-no
        lv-stat = oe-ord.stat.

IF lv-cust EQ "" 
  THEN
    FIND inv-head NO-LOCK 
        WHERE ROWID(inv-head) EQ ip-rowid NO-ERROR.
    IF AVAIL inv-head 
      THEN
        ASSIGN
            lv-cust = inv-head.cust-no
            lv-stat = inv-head.stat.

IF lv-cust EQ "" 
  THEN
    FIND ar-inv NO-LOCK 
        WHERE ROWID(ar-inv) EQ ip-rowid NO-ERROR.
    IF AVAIL ar-inv 
      THEN
        ASSIGN
            lv-cust = ar-inv.cust-no
            lv-stat = ar-inv.stat.

IF lv-cust EQ "" 
  THEN
    FIND cust NO-LOCK 
        WHERE ROWID(cust) EQ ip-rowid NO-ERROR.


IF lv-cust NE "" AND lv-stat NE "H" AND
  (oecredit-log OR lv-stat NE "A") 
 THEN
    FIND FIRST cust NO-LOCK
        {sys/ref/custW.i}
        AND cust.cust-no EQ lv-cust USE-INDEX cust NO-ERROR.
    IF AVAIL cust THEN DO:
        FIND FIRST  terms NO-LOCK
            WHERE terms.company = cust.company
              AND terms.t-code = cust.terms NO-ERROR.

        ASSIGN
            v-error    = ""
            ld-ord-bal = cust.ord-bal.

        IF cust.cr-hold 
          THEN v-error = "is on credit hold.".
          ELSE DO:                         
              IF cust.cr-hold-invdue  GT 0 OR
                /* 04/15/10 (AH) as per Joe dont include term days in equation */
                /*(cust.cr-hold-invdays + terms.net-days) GT 0 */
                 cust.cr-hold-invdays  GT 0 
                THEN 
                  RUN oe/creditid.p (INPUT RECID(cust), OUTPUT hold_invoice_id).
              
              IF hold_invoice_id NE ? 
                THEN v-error = "invoice age".
/*                 ELSE DO:                                                     */
/*                     IF oecredit-cha EQ ""                                    */
/*                       THEN                                                   */
/*                          RUN ar/updcust1.p (BUFFER cust, OUTPUT ld-ord-bal). */
/*                                                                              */
/*                     IF ld-ord-bal + cust.acc-bal GT cust.cr-lim              */
/*                       THEN v-error = "credit".                               */
/*                       ELSE                                                   */
/*                         IF ld-ord-bal GT cust.ord-lim                        */
/*                           THEN v-error = "order".                            */
/*                 END. /* ELSE DO */                                           */


              IF v-error NE "" 
                THEN
                  v-error = "has exceeded their " + TRIM(v-error) + " limit.".
          END. /* ELSE DO */

    END. /* IF AVAIL cust */

IF V-ERROR NE "" 
  THEN
    MESSAGE 
     "Customer:" TRIM(cust.name) + " " + TRIM(v-error) SKIP
     "The release ticket status will be set to On Hold."
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF v-error NE "" THEN ASSIGN op-okflg = YES.
