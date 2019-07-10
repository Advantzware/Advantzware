/* --------------------------------------------------- oe/creditck.p 3/94 RM  */
/* O/E CUSTOMER CREDIT CHECK                                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-aging AS LOG NO-UNDO. 

{sys/inc/var.i SHARED}

DEF VAR v-error AS CHAR INIT "" NO-UNDO.
DEF VAR hold_invoice_id AS RECID INIT ? NO-UNDO.
DEF VAR ld-ord-bal LIKE cust.ord-bal NO-UNDO.
DEF VAR lv-cust LIKE oe-ord.cust-no NO-UNDO.
DEF VAR lv-stat LIKE oe-ord.stat NO-UNDO.
DEF VAR v-lock-first AS LOG INIT TRUE NO-UNDO.
DEF VAR v-final-cust-no AS CHAR NO-UNDO.

DO TRANSACTION:
  {sys/inc/oecredit.i}
END.

FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ ip-rowid NO-ERROR.
IF AVAIL oe-ord THEN
  ASSIGN
   lv-cust = oe-ord.cust-no
   lv-stat = oe-ord.stat.

IF lv-cust EQ "" THEN
FIND inv-head NO-LOCK WHERE ROWID(inv-head) EQ ip-rowid NO-ERROR.

IF AVAIL inv-head THEN
  ASSIGN
   lv-cust = inv-head.cust-no
   lv-stat = inv-head.stat.

IF lv-cust EQ "" THEN
FIND ar-inv NO-LOCK WHERE ROWID(ar-inv) EQ ip-rowid NO-ERROR.

IF AVAIL ar-inv THEN
  ASSIGN
   lv-cust = ar-inv.cust-no
   lv-stat = ar-inv.stat.

IF lv-cust EQ "" THEN
FIND cust NO-LOCK WHERE ROWID(cust) EQ ip-rowid NO-ERROR.

IF lv-cust NE "" AND lv-stat NE "H" AND
   (oecredit-log OR lv-stat NE "A") THEN
FIND FIRST cust NO-LOCK
    where (cust.company = cocode)
      AND cust.cust-no EQ lv-cust
    USE-INDEX cust NO-ERROR.

IF AVAIL cust THEN DO:
  ASSIGN
   v-error    = ""
   ld-ord-bal = cust.ord-bal
   v-final-cust-no = cust.cust-no.

  IF cust.cr-hold THEN v-error = "is on credit hold.".

  ELSE DO:
    IF ip-aging AND cust.cr-hold-invdays GT 0 THEN
      RUN oe/creditid.p (INPUT RECID(cust), OUTPUT hold_invoice_id).

    IF hold_invoice_id NE ? THEN v-error = "invoice age".
    ELSE DO:
      IF oecredit-cha EQ "" THEN
         RUN ar/updcust1.p (BUFFER cust, OUTPUT ld-ord-bal).

      IF ld-ord-bal + cust.acc-bal GT cust.cr-lim THEN v-error = "credit".
      ELSE
      IF ld-ord-bal GT cust.ord-lim THEN v-error = "order".
    END.

    IF v-error NE "" THEN
      v-error = "has exceeded their " + TRIM(v-error) + " limit.".
  END.

  DO TRANSACTION:

    /*FIND CURRENT cust NO-ERROR.*/

    IF v-error NE "" THEN DO:
        
      REPEAT:
  
         FIND FIRST cust 
              where (cust.company = cocode)
              AND cust.cust-no EQ v-final-cust-no
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF AVAIL cust THEN
            LEAVE.

         IF v-lock-first THEN
         DO:
            MESSAGE "Customer record in use, waiting for release..."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            session:set-wait-state("General").
            v-lock-first = NO.
         END.
      END.

      session:set-wait-state("").

      /* gdm - 06020913 */
      IF oecredit-int EQ 1 THEN DO:

         MESSAGE 
            "Customer has exceeded credit limit, " +
            "order limit or invoice is past due"
           VIEW-AS ALERT-BOX WARNING BUTTONS OK.

      END.
      ELSE DO:
       /* gdm - 06020913 */
       MESSAGE 
          "Customer:" TRIM(cust.name)
          TRIM(v-error)
          "The " + 
          TRIM(STRING(NOT AVAIL inv-head AND NOT AVAIL ar-inv,"Order/Invoice")) +
          " status will be set to HOLD."
        VIEW-AS ALERT-BOX.

       /* update credit hold field in cust file */
       IF NOT cust.cr-hold THEN DO:  
         
         IF AVAIL oe-ord THEN DO:
           
           FIND CURRENT oe-ord NO-ERROR.
              oe-ord.stat = "H".
           FIND CURRENT oe-ord NO-LOCK NO-ERROR.

          END.
          ELSE
          IF AVAIL inv-head THEN DO:

           FIND CURRENT inv-head NO-ERROR.
              inv-head.stat = "H".
           FIND CURRENT inv-head NO-LOCK NO-ERROR.

          END.
          ELSE
          IF AVAIL ar-inv   THEN DO:

            FIND CURRENT ar-inv NO-ERROR.
              ar-inv.stat = "H".
            FIND CURRENT ar-inv NO-LOCK NO-ERROR.

          END.

          cust.cr-hold = YES.

         END. /* IF AVAIL oe-ord */
       END. /* IF NOT cust.cr-hold */ 

       cust.ord-bal = ld-ord-bal.

       FIND CURRENT cust NO-LOCK NO-ERROR.

    END. /* IF v-error */

  END. /* DO TRANSACTION: */  

END. /* IF AVAIL cust */
