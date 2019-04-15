
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER iprOeOrdl        AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iprBolh          AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipdQty           AS DECIMAL     NO-UNDO.
DEFINE INPUT PARAMETER iplCreateBolLine AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER oprOeRell       AS ROWID NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}
{oe/relemail.i NEW}
{ce/print4.i "new shared"}   
{cec/print42.i "new shared"}    
DEF BUFFER bf-oe-rel FOR oe-rel.

{sys/inc/oereleas.i}
{oe/createRelease.i}.

FIND oe-ordl WHERE rowid(oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
FIND oe-bolh WHERE ROWID(oe-bolh) EQ iprBolh NO-LOCK NO-ERROR.


/* =-=---- for oe-comm.p ---- */
def shared var v-misc as log init no no-undo.
def shared var v-fr-tax like oe-ctrl.f-tax no-undo.
/* =   for release/bol =====*/
def new shared buffer xoe-ordl for oe-ordl.
def new shared var out-recid as recid no-undo.
def new shared var relh-recid as recid no-undo.
def new shared var v-auto as log no-undo.
DEF VAR lv-passwd AS cha NO-UNDO.
DEF VAR ld-price LIKE oe-ordl.price NO-UNDO.
DEF VAR ld-pr-uom LIKE oe-ordl.pr-uom NO-UNDO.
DEF VAR ld-ext-price LIKE oe-ordl.t-price NO-UNDO.



{fg/fullset.i NEW}

DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.

DEF TEMP-TABLE w-rel NO-UNDO
    FIELD w-rowid AS ROWID
    FIELD w-scode LIKE oe-rell.s-code
    FIELD fob AS CHAR.

DEF TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl.

DEF BUFFER b-fob FOR reftable.



/* Main Block */
main-block:
DO:

  FIND FIRST oe-ord WHERE oe-ord.company EQ oe-ordl.company
    AND oe-ord.ord-no EQ oe-ordl.ord-no NO-LOCK.

  FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) NO-LOCK NO-ERROR.

  RUN createRelease (INPUT oe-bolh.ship-id,
                     INPUT "").
  
  IF AVAIL oe-rel THEN DO:
  
    FIND CURRENT oe-rel EXCLUSIVE-LOCK.
    ASSIGN oe-rel.tot-qty = ipdQty
           oe-rel.qty = ipdQty.
    RUN createActualReleases (INPUT ROWID(oe-rel)).
  END.
  

END.


PROCEDURE createActualReleases:
DEFINE INPUT  PARAMETER iprOeRel AS ROWID       NO-UNDO.
def var choice as log no-undo.
def buffer bf-rel for oe-rel.
def var v-stat as cha no-undo.
def var v-all-items as log no-undo.
def var v-first as log no-undo.
DEF VAR lv-save-recid AS RECID NO-UNDO.
DEF VAR v-chkflg AS LOG INIT NO NO-UNDO.
DEF VAR v-all-i AS LOG NO-UNDO.
DEF VAR v-rel-type AS CHAR NO-UNDO.

/* DEF BUFFER s-code FOR reftable. */

find xoe-ord where xoe-ord.company = g_company and
                   xoe-ord.ord-no = oe-ordl.ord-no no-lock.
find first oe-ctrl where oe-ctrl.company = xoe-ord.company no-lock .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

RUN check-release NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

/* Check if the only release is an invoice only */

v-all-i = YES.
FOR EACH bf-rel
      WHERE bf-rel.company EQ xoe-ord.company
        AND bf-rel.ord-no  EQ xoe-ord.ord-no
        AND bf-rel.link-no EQ 0
      NO-LOCK:
    v-rel-type = oe-rel.s-code.
    IF v-rel-type NE "I" THEN
        v-all-i = NO.    
end.

/* gdm - 02020902 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
    
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ oe-ordl.company
          AND cust.cust-no EQ oe-ordl.cust-no NO-ERROR.
    IF AVAIL cust THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
                                         INPUT YES,
                                         OUTPUT v-chkflg).
    IF v-chkflg AND NOT v-all-i THEN DO:

        MESSAGE 
            "Can't create BOL, there are unpaid invoices."
            "Please create actual release first."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.     
    END.

END. /* avail sys-ctrl */

lv-save-recid = RECID(oe-ordl).     


FIND bf-rel WHERE ROWID(bf-rel) EQ iprOeRel NO-LOCK NO-ERROR. 
choice = avail bf-rel and index("CDZ",xoe-ord.stat) eq 0.

if choice then do:
  SESSION:SET-WAIT-STATE("general").

  find xoe-ordl of xoe-ord no-lock no-error.
  if ambig xoe-ordl then
    message "All Items on Order?" view-as alert-box question
            button yes-no update choice.

  v-all-items = choice.

  EMPTY TEMP-TABLE w-rel.

  FOR EACH bf-rel 
      WHERE ROWID(bf-rel) EQ iprOeRel
      NO-LOCK,
      FIRST xoe-ordl
      WHERE xoe-ordl.company EQ bf-rel.company
        AND xoe-ordl.ord-no  EQ bf-rel.ord-no
        AND xoe-ordl.i-no    EQ bf-rel.i-no
        AND xoe-ordl.line    EQ bf-rel.line
        AND (RECID(xoe-ordl) EQ RECID(oe-ordl) OR v-all-items)
      NO-LOCK:
     
    RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT v-stat).

    IF NOT CAN-DO("A,B",v-stat) THEN DO:

       
       CREATE w-rel.
       ASSIGN
        w-rel.w-rowid = ROWID(bf-rel)
        w-rel.w-scode = IF bf-rel.s-code <> "" AND bf-rel.s-code EQ "I" THEN
                           bf-rel.s-code
                        ELSE
                           "B".
      
       ASSIGN w-rel.fob = bf-rel.fob-code.
       RELEASE w-rel.
    END. /* if not can-do... */
  END. /* each bf-rel */

  FOR EACH w-rel,
      FIRST bf-rel WHERE ROWID(bf-rel) EQ w-rowid NO-LOCK
      BREAK BY w-rel.w-scode
            BY bf-rel.rel-date
            BY bf-rel.ship-id
            BY w-rel.fob:
                   
    IF FIRST-OF(w-rel.fob) THEN DO:
      choice = TRUE.
/*        choice = v-do-def.                                             */
/*                                                                       */
/*        MESSAGE "Create " +                                            */
/*                TRIM(STRING(w-scode EQ "I","Invoice/BOL")) +           */
/*                " for Release Date-" + TRIM(STRING(bf-rel.rel-date)) + */
/*                " and ShipID-" +  TRIM(bf-rel.ship-id) +               */
/*                " and FOB-" + TRIM(w-rel.fob) + " ?"                   */
/*           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.     */
    END.

    IF NOT choice THEN
       DELETE w-rel.
  END. /* each w-rel */

  FOR EACH w-rel,
      FIRST bf-rel WHERE ROWID(bf-rel) EQ w-rowid NO-LOCK,
      FIRST xoe-ordl
      WHERE xoe-ordl.company EQ bf-rel.company
        AND xoe-ordl.ord-no  EQ bf-rel.ord-no
        AND xoe-ordl.i-no    EQ bf-rel.i-no
        AND xoe-ordl.line    EQ bf-rel.line
      BREAK BY w-rel.w-scode
            BY bf-rel.rel-date
            BY bf-rel.ship-id
            BY w-rel.fob:

    ASSIGN
     out-recid = RECID(bf-rel)
     v-auto    = YES.

    /* Runs oe/actrelmerg.p */
    RUN oe/relbol.p (RECID(xoe-ordl)).

    FIND FIRST oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rell THEN DO:
      oprOeRell = ROWID(oe-rell).
      FIND oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL oe-relh THEN
        ASSIGN oe-relh.posted  = TRUE
               oe-relh.printed = TRUE
               oe-relh.spare-char-3 = USERID("NOSWEAT").
      RELEASE oe-relh.
    END. /* avail oe-rell */
      
    
    IF LAST-OF(w-rel.fob) AND iplCreateBolLine THEN 
      RUN oe/doBolSingle.p(INPUT iprBolh, INPUT NOT w-rel.w-scode EQ "I").

    DELETE w-rel.
  END. /* Each w-rel */


  SESSION:SET-WAIT-STATE("").
END. /* if choice */

END PROCEDURE.


PROCEDURE check-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR lHoldOK AS LOGICAL NO-UNDO.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ g_company NO-LOCK NO-ERROR.
IF AVAIL oe-ctrl THEN
    lHoldOK = oe-ctrl.p-pick.

IF lv-msg EQ "" AND oe-ord.stat eq "H" AND NOT lHoldOK THEN
  lv-msg = "orders on Hold".

IF lv-msg EQ "" AND oe-ord.priceHold AND NOT lHoldOK THEN
  lv-msg = "orders on Price Hold".

IF lv-msg EQ "" AND oe-ord.stat EQ "W" THEN
  lv-msg = "unapproved web orders".

IF lv-msg EQ "" AND NOT oe-ord.opened THEN
  lv-msg = "closed orders".

IF lv-msg EQ "" AND TRIM(oe-ordl.job-no) NE ""
                AND CAN-FIND(FIRST job
                             WHERE job.company EQ oe-ordl.company
                               AND job.job-no  EQ oe-ordl.job-no
                               AND job.job-no2 EQ oe-ordl.job-no2
                               AND job.stat    EQ "H") THEN
  lv-msg = "jobs on hold".


IF lv-msg NE "" THEN DO:
  MESSAGE "Can't release items for " +
          TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.
END PROCEDURE.

