CURRENT-WINDOW:WIDTH-CHARS = 130.
/* 09041301 - also update closed orders */
/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared }
assign cocode = g_company
       locode = g_loc.

DEF BUFFER s-code FOR reftable.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-sell-price FOR reftable.

DEF VAR char-hdl AS CHAR NO-UNDO.
def var ls-rel-stat as cha label "" form "x" no-undo.
def var lv-rel-recid as recid no-undo.
def new shared buffer xoe-ordl for oe-ordl.
def new shared buffer xoe-ord for oe-ord.
def new shared var out-recid as recid no-undo.
def new shared var relh-recid as recid no-undo.
def new shared var v-auto as log no-undo.
def new shared var nufile as log no-undo.   /* for jc-calc.p */
def new shared var lv-qty as int no-undo.
def new shared var fil_id as recid no-undo.
def var li-ship-no as int no-undo.  /* if ship-to is changed */

def var ll-unposted as log no-undo.
def var ls-po as cha no-undo.
def var ll-canceled as log no-undo.
def var lv-stat as cha no-undo.
DEF VAR ld-date as DATE NO-UNDO.
DEF VAR ll-skip AS LOG NO-UNDO.
DEF VAR lv-s-codes AS CHAR NO-UNDO.
DEF VAR lv-s-dscrs AS CHAR NO-UNDO.
DEF VAR lv-cust-x LIKE cust.cust-no NO-UNDO.
DEF VAR ll-transfer AS LOG NO-UNDO.
DEF VAR v-browse-in-update AS LOG NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.
DEF VAR v-last-shipto AS CHAR NO-UNDO.
DEF VAR l-update-reason-perms AS LOG NO-UNDO.
DEF VAR adm-cur-state AS CHAR NO-UNDO.
DEF VAR oeDateChange-log AS LOG NO-UNDO.
DEF VAR v-rtn-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.
DEF VAR v-disp-rel-qty AS DEC NO-UNDO.
DEF VAR v-scr-s-code AS CHAR NO-UNDO.
def var retact as DEC NO-UNDO.
RUN sys/ref/s-codes.p (OUTPUT lv-s-codes, OUTPUT lv-s-dscrs).

DEF TEMP-TABLE tt-report NO-UNDO
    LIKE report FIELD phantom AS LOG
    FIELD po-no LIKE oe-rel.po-no
    FIELD qty LIKE oe-rel.qty
    FIELD printed AS LOG
    FIELD s-code AS CHAR
    FIELD lot-no AS CHAR
    FIELD sell-price AS DEC
    FIELD freight-pay AS CHAR
    FIELD fob AS CHAR
    FIELD zero-sprice AS LOG
    FIELD release# LIKE oe-relh.release#.

{oe/chkordl.i NEW}
{oe/relemail.i NEW}

DO TRANSACTION:
  {sys/inc/oeship.i}
  {sys/inc/oereleas.i}
  {sys/ref/relpost.i}
  {sys/inc/addxfer.i}
  {sys/inc/reltype.i}
END.

/* Current oe-rell.r-no to process for oe-rel */
def var v-current-r-no like oe-rel.r-no no-undo.

DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "OEDateChg",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


RUN sys/ref/nk1look.p (cocode, "oeDateChange", "L", no, no, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.

/* {sys/inc/f3help.i} */
{sys/inc/oeinq.i}
{sa/sa-sls01.i}
    
lv-cust-x = "".



FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.


DISABLE TRIGGERS FOR LOAD OF oe-rel.
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

DEF BUTTON bt LABEL "GO".
DEF BUTTON btCanc LABEL "Cancel".
DEF VAR vsel AS CHAR VIEW-AS SELECTION-LIST SINGLE LIST-ITEMS "Scheduled","Actual","Both"
    INNER-CHARS 16 INNER-LINES 3.
DEF VAR vComp AS CHAR COLUMN-LABEL "Company".
DEF VAR vFrOrd  LIKE oe-ord.ord-no NO-UNDO COLUMN-LABEL "From Order#" .
DEF VAR vToOrd  LIKE oe-ord.ord-no NO-UNDO COLUMN-LABEL "To Order#".
DEF VAR vCancel AS LOG NO-UNDO.

FORM vsel LABEL "Run For" vComp vFrOrd vToOrd SKIP(1) bt space(2) btCanc WITH FRAME fprocess
    CENTERED ROW 5 WIDTH 70.

ON 'choose':U OF bt
DO:
  APPLY 'GO' TO FRAME fprocess.    
END.

ON 'choose':U OF btCanc
DO:
  vCancel = TRUE.
  APPLY 'GO' TO FRAME fprocess.    
END.

/* Main Block */
DEF BUFFER bf-oe-rel FOR oe-rel.

vCancel = FALSE.
 UPDATE vSel vComp vFrOrd vToOrd SKIP bt btCanc WITH FRAME fprocess
    CENTERED ROW 5 TITLE "Set Release Actual and Scheduled Quantities". 

IF vCancel THEN DO:
        APPLY 'close' TO THIS-PROCEDURE.
        APPLY 'window-close' TO CURRENT-WINDOW.
END.


FOR EACH company WHERE company.company EQ vComp  NO-LOCK,
    EACH bf-oe-rel WHERE bf-oe-rel.company EQ company.company
     AND bf-oe-rel.ord-no GE vFrOrd 
     AND bf-oe-rel.ord-no LE vToOrd 
    ,
    FIRST oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ bf-oe-rel.company
      AND oe-ordl.ord-no  EQ bf-oe-rel.ord-no
      AND oe-ordl.i-no    EQ bf-oe-rel.i-no
      AND oe-ordl.line    EQ bf-oe-rel.line:

  STATUS DEFAULT "Processing Order# " + STRING(bf-oe-rel.ord-no).
  FIND FIRST oe-rell 
     WHERE oe-rell.ord-no  EQ bf-oe-rel.ord-no
       AND oe-rell.i-no    EQ bf-oe-rel.i-no
       AND oe-rell.link-no EQ bf-oe-rel.r-no
       and oe-rell.rel-no  eq bf-oe-rel.rel-no
       AND oe-rell.po-no   eq bf-oe-rel.po-no
     NO-LOCK NO-ERROR.    
  if avail oe-rell then
    v-current-r-no = oe-rell.r-no.
  else
    v-current-r-no = 0.
    

  FIND oe-ord WHERE oe-ord.company = oe-ordl.company
                AND oe-ord.ord-no = oe-ordl.ord-no 
              NO-LOCK NO-ERROR.
  
  IF NOT AVAIL oe-ord THEN
      NEXT.
/*   09041301 - also update closed orders */
/*   IF oe-ord.stat = "C"  OR   */
/*       oe-ord.stat = "Z" THEN */
/*       NEXT.                  */

  RUN build-report-file.

  RUN oe/rel-stat.p (ROWID(bf-oe-rel), OUTPUT lv-stat).
  IF vsel = "Scheduled" OR vsel = "Both" THEN DO:  

      IF bf-oe-rel.tot-qty EQ 0 AND INDEX("SIL",lv-stat) GT 0 THEN DO: 
          bf-oe-rel.tot-qty = oe-ordl.qty.
      END.
      ELSE
      IF bf-oe-rel.tot-qty EQ 0 AND INDEX("ABCPZ",lv-stat) GT 0 THEN DO: 
          bf-oe-rel.tot-qty = bf-oe-rel.qty.
      END.

  END.

  IF vsel = "Actual" OR vsel = "Both" THEN DO:  

      IF bf-oe-rel.qty NE get-rel-qty() THEN DO:
          bf-oe-rel.qty = get-rel-qty().
      END.
    
      IF bf-oe-rel.tot-qty EQ 0 AND INDEX("LICZP ",lv-stat) GT 0 THEN DO:
        RUN get-act-qty (INPUT ROWID(bf-oe-rel), OUTPUT retact).
      END.
      DISP bf-oe-rel.stat bf-oe-rel.qty bf-oe-rel.tot-qty lv-stat.
     
      /* If we check that status and it's one of SIL, actual qty has to be 0 */
      IF INDEX("SIL",lv-stat) GT 0  THEN
        bf-oe-rel.qty = 0.
      ELSE
        bf-oe-rel.qty = lv-qty.
    lv-qty = 0.
  END.


  FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR.


END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.

HIDE ALL NO-PAUSE.
APPLY 'close' TO THIS-PROCEDURE.
APPLY 'window-close' TO THIS-PROCEDURE.

PROCEDURE build-report-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
/* DEF VAR lv-qty AS INT NO-UNDO. */
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-s-code LIKE oe-rell.s-code EXTENT 2 NO-UNDO.

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rell-exc FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.


  ll-transfer = CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-ordl.company
                           AND oe-ord.ord-no  EQ oe-ordl.ord-no
                           AND oe-ord.type    EQ "T").

  RUN delete-phantoms.

  FOR EACH tt-report:
    DELETE tt-report.
  END.

  /* RUN oe/cleanrel.p (ROWID(oe-ordl)). */

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        and oe-rel.po-no   eq bf-oe-rel.po-no
        and oe-rel.link-no eq v-current-r-no
      USE-INDEX ord-item
      
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

      :

    IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN DO:
        RUN create-report-record (ROWID(oe-rel), NO).        
    END.
  END.

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company  EQ oe-ordl.company
        AND oe-boll.ord-no   EQ oe-ordl.ord-no
        AND oe-boll.i-no     EQ oe-ordl.i-no
        AND oe-boll.line     EQ oe-ordl.line
        and oe-boll.po-no    eq bf-oe-rel.po-no
        and (if v-current-r-no eq 0 then true else oe-boll.r-no eq v-current-r-no)
      USE-INDEX ord-no,

      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
      
      FIRST oe-rell NO-LOCK
      WHERE oe-rell.company  EQ oe-boll.company
        AND oe-rell.ord-no   EQ oe-boll.ord-no
        AND oe-rell.line     EQ oe-boll.line
        AND oe-rell.i-no     EQ oe-boll.i-no
        AND oe-rell.r-no     EQ oe-boll.r-no
        AND oe-rell.rel-no   EQ oe-boll.rel-no
        AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        AND oe-rell.po-no    EQ oe-boll.po-no
      USE-INDEX ord-no,

      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-boll.r-no
      
      BREAK BY oe-boll.r-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no
            BY oe-boll.po-no

      :

    IF FIRST-OF(oe-boll.po-no) THEN do: 
      
/*      lv-qty = 0. */
    end.

    lv-qty = lv-qty + oe-boll.qty.

    IF LAST-OF(oe-boll.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE oe-rel.
      IF oe-rell.link-no NE 0 THEN
      FIND oe-rel NO-LOCK
          WHERE oe-rel.r-no EQ oe-rell.link-no
          USE-INDEX seq-no NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX link NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX ord-item NO-ERROR.

      IF AVAIL oe-rel THEN
      FIND CURRENT oe-rel NO-LOCK NO-ERROR .

      IF AVAIL oe-rel THEN DO:
        FIND CURRENT oe-rel.

        FOR EACH b-oe-rell NO-LOCK
            WHERE b-oe-rell.company  EQ oe-rel.company
              AND b-oe-rell.r-no     EQ oe-rel.link-no
              AND b-oe-rell.ord-no   EQ oe-rel.ord-no
              AND b-oe-rell.i-no     EQ oe-rel.i-no
              AND b-oe-rell.line     EQ oe-rel.line
              AND b-oe-rell.rel-no   EQ oe-rel.rel-no
              AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND b-oe-rell.po-no    EQ oe-rel.po-no
            USE-INDEX r-no:
          FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
              NO-LOCK NO-ERROR.
          /* IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no. */
        END.
      END.

      ELSE DO:
/*
        RUN oe/get-r-no.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
        CREATE oe-rel.
        ASSIGN
         oe-rel.company   = oe-relh.company
         oe-rel.r-no      = v-nxt-r-no
         oe-rel.link-no   = oe-rell.r-no
         oe-rel.cust-no   = oe-relh.cust-no
         oe-rel.ord-no    = oe-rell.ord-no
         oe-rel.i-no      = oe-rell.i-no
         oe-rel.line      = oe-rell.line
         oe-rel.rel-no    = oe-rell.rel-no
         oe-rel.b-ord-no  = oe-rell.b-ord-no
         oe-rel.rel-date  = oe-relh.rel-date
         oe-rel.carrier   = oe-relh.carrier
         oe-rel.ship-no   = oe-relh.ship-no
         oe-rel.ship-id   = oe-relh.ship-id
         oe-rel.ship-i[1] = oe-relh.ship-i[1]
         oe-rel.ship-i[2] = oe-relh.ship-i[2]
         oe-rel.ship-i[3] = oe-relh.ship-i[3]
         oe-rel.ship-i[4] = oe-relh.ship-i[4]
         oe-rel.po-no     = oe-boll.po-no
         oe-rel.lot-no    = oe-boll.lot-no
         oe-rel.spare-char-1 = oe-rell.loc
         oe-rel.qty       = lv-qty.
        
        RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                               INPUT ROWID(oe-boll)).
        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        if avail shipto then
          assign
           oe-rel.ship-addr[1] = shipto.ship-addr[1]
           oe-rel.ship-addr[2] = shipto.ship-addr[2]
           oe-rel.ship-city    = shipto.ship-city
           oe-rel.ship-state   = shipto.ship-state
           oe-rel.ship-zip     = shipto.ship-zip.

        /* Assign qty to itemfg-loc */
        RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

        RUN create-report-record (ROWID(oe-rel), NO).
        */
      END.
    END.
  END.

  FOR EACH oe-rell
      WHERE oe-rell.company  EQ cocode
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        and oe-rell.po-no    eq bf-oe-rel.po-no
        and (if v-current-r-no eq 0 then true else oe-rell.r-no eq v-current-r-no)        
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-no NO-LOCK,

      FIRST oe-relh NO-LOCK
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing")
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no

      :

    IF FIRST-OF(oe-rell.po-no) THEN do:
          
/*       lv-qty = 0. */
    end.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE b-oe-rell.
      IF oe-relh.posted THEN
      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
            AND b-oe-rell.r-no    EQ oe-rell.r-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            AND CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ b-oe-rell.company
                           AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                           AND oe-boll.i-no     EQ b-oe-rell.i-no
                           AND oe-boll.line     EQ b-oe-rell.line
                           AND oe-boll.r-no     EQ b-oe-rell.r-no
                           AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ b-oe-rell.po-no
                         USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:

        LEAVE.
      END.

      IF NOT AVAIL b-oe-rell THEN DO:
        RELEASE oe-rel.
        IF oe-rell.link-no NE 0 AND oe-relh.posted THEN
        FIND oe-rel NO-LOCK
            WHERE oe-rel.r-no EQ oe-rell.link-no
            USE-INDEX seq-no NO-ERROR.


        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX link NO-ERROR.


        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX ord-item NO-ERROR.
/*
        IF NOT AVAIL oe-rel THEN DO:
/*           10051225 */
/*           FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.      */
/*           v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1. */
          RUN oe/get-r-no.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
          CREATE oe-rel.
          ASSIGN
           oe-rel.company   = oe-relh.company
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.link-no   = IF oe-relh.posted THEN oe-rell.r-no ELSE 0
           oe-rel.cust-no   = oe-relh.cust-no
           oe-rel.ord-no    = oe-rell.ord-no
           oe-rel.i-no      = oe-rell.i-no
           oe-rel.line      = oe-rell.line
           oe-rel.rel-no    = oe-rell.rel-no
           oe-rel.b-ord-no  = oe-rell.b-ord-no
           oe-rel.rel-date  = oe-relh.rel-date
           oe-rel.carrier   = oe-relh.carrier
           oe-rel.ship-no   = oe-relh.ship-no
           oe-rel.ship-id   = oe-relh.ship-id
           oe-rel.ship-i[1] = oe-relh.ship-i[1]
           oe-rel.ship-i[2] = oe-relh.ship-i[2]
           oe-rel.ship-i[3] = oe-relh.ship-i[3]
           oe-rel.ship-i[4] = oe-relh.ship-i[4]
           oe-rel.po-no     = oe-rell.po-no
           oe-rel.lot-no    = oe-rell.lot-no
           oe-rel.spare-char-1 = oe-rell.loc
           oe-rel.qty       = lv-qty.
           
          RUN oe/custxship.p (oe-rel.company,
                              oe-rel.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).

          if avail shipto then
            assign
             oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-addr[2] = shipto.ship-addr[2]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip
             .


          /* Make sure lot # is updated in reftable entry */
          RUN set-lot-from-boll (INPUT ROWID(oe-rel), INPUT ROWID(oe-rell),
                                 INPUT ROWID(oe-boll)).
          RUN create-report-record (ROWID(oe-rel), NO).
        END.

        ELSE */ DO:
          FIND CURRENT oe-rel NO-LOCK NO-ERROR.

          IF AVAIL oe-rel THEN DO:
            IF oe-relh.posted THEN DO:
                
            FOR EACH b-oe-rell NO-LOCK
                  WHERE b-oe-rell.company  EQ oe-rel.company
                    AND b-oe-rell.r-no     EQ oe-rel.link-no
                    AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                    AND b-oe-rell.i-no     EQ oe-rel.i-no
                    AND b-oe-rell.line     EQ oe-rel.line
                    AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                    AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND b-oe-rell.po-no    EQ oe-rel.po-no
                  USE-INDEX r-no:
                FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
                    EXCLUSIVE NO-ERROR NO-WAIT.
          /*       IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no. */
              END.
            END.

            ELSE DO:
              /*IF oe-rel.link-no NE 0 THEN oe-rel.link-no = 0. */

              FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
              IF AVAIL tt-report THEN tt-report.qty = lv-qty.
            END.
          END.
        END.
      END.
    END.
  END.


  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:

      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

      FIND FIRST tt-report WHERE 
         tt-report.rec-id  = RECID(oe-rel) NO-ERROR.
      IF NOT AVAIL tt-report THEN DO:          
          RUN create-report-record (ROWID(oe-rel), NO).
      END.
  END.

  RELEASE oe-rel.
  RELEASE b-oe-rell.
  RELEASE oe-rell.
  RELEASE oe-boll.
  RELEASE tt-report.

END PROCEDURE.

PROCEDURE create-report-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
      

  FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-rel THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK NO-ERROR.

  IF AVAIL oe-ord THEN DO:
    FIND FIRST tt-report
        WHERE tt-report.rec-id EQ RECID(oe-rel)
        NO-ERROR.

    IF NOT AVAIL tt-report THEN CREATE tt-report.

    {oe/rel-stat.i lv-stat}

    RELEASE inv-line.
    IF lv-stat EQ "Z" AND AVAIL oe-boll THEN
    FIND FIRST inv-line
        WHERE inv-line.company EQ oe-boll.company
          AND inv-line.b-no    EQ oe-boll.b-no
          AND inv-line.ord-no  EQ oe-boll.ord-no
          AND inv-line.i-no    EQ oe-boll.i-no
          AND inv-line.po-no   NE ""
        NO-LOCK NO-ERROR.

    RUN create-report-record-1 (ip-phantom,
                                IF AVAIL oe-relh THEN oe-relh.rel-date
                                                 ELSE oe-rel.rel-date).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 B-table-Win 
PROCEDURE create-report-record-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.

  DEF VAR v-reltype AS cha NO-UNDO.


    ASSIGN
     tt-report.term-id = v-term
     tt-report.rec-id  = RECID(oe-rel)
     ld-date           = ip-date
     tt-report.key-01  = STRING(YEAR(ld-date),"9999") +
                         STRING(MONTH(ld-date),"99")  +
                         STRING(DAY(ld-date),"99")
     tt-report.key-02  = STRING(ld-date,"99999999")
     tt-report.phantom = ip-phantom
     tt-report.po-no   = /*IF AVAIL inv-line THEN inv-line.po-no
                         ELSE
                         IF AVAIL oe-boll THEN oe-boll.po-no
                         ELSE
                         IF AVAIL oe-rell THEN oe-rell.po-no
                         ELSE*/ oe-rel.po-no
     tt-report.qty     = oe-rel.qty
     tt-report.printed = (AVAIL oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",lv-stat) GT 0
     tt-report.release# = (IF AVAIL oe-relh THEN oe-relh.release# ELSE 0).

     /* task 04011103*/
     FIND FIRST sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name eq "RelType" no-lock no-error.
     IF AVAIL sys-ctrl THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                  AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
        IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                  AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
     IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
     ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
     IF v-relType <> "" THEN DO:
        FIND FIRST reftable
        WHERE reftable.reftable EQ "oe-rel.s-code"
          AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") NO-LOCK NO-ERROR.
        IF NOT AVAIL reftable THEN DO:
        END.
     END.
     
    FIND FIRST s-code
        WHERE s-code.reftable EQ "oe-rel.s-code"
          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    IF AVAIL reftable THEN
    tt-report.s-code = IF v-reltype <> "" THEN reftable.CODE
                       ELSE IF ll-transfer            THEN "T"
                       ELSE
                       IF oe-ordl.is-a-component AND
                          (NOT AVAIL s-code OR
                           s-code.code NE "T")   THEN "S"
                       ELSE
                       IF AVAIL s-code           THEN s-code.code
                       ELSE
                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                                 ELSE "B".

    FIND FIRST ref-lot-no WHERE
         ref-lot-no.reftable EQ "oe-rel.lot-no" AND
         ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-lot-no THEN
    DO:
       ASSIGN
          tt-report.lot-no      = ref-lot-no.CODE
          tt-report.freight-pay = ref-lot-no.code2
          tt-report.fob         = ref-lot-no.dscr.
       RELEASE ref-lot-no.
    END.

    FIND FIRST ref-sell-price WHERE
         ref-sell-price.reftable EQ "oe-rel.sell-price" AND
         ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-sell-price THEN
    DO:
       ASSIGN tt-report.sell-price = ref-sell-price.val[1]
              tt-report.zero-sprice = ref-sell-price.val[2] > 0.
       RELEASE ref-sell-price.
    END.

    IF oeinq THEN 
      tt-report.key-01 = STRING(9999999999 - INT(tt-report.key-01),"9999999999").

END PROCEDURE.

PROCEDURE delete-phantoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER b-tt-report FOR tt-report.

  /*
  FOR EACH b-tt-report WHERE b-tt-report.phantom:
    FIND FIRST b-oe-rel WHERE RECID(b-oe-rel) EQ b-tt-report.rec-id NO-ERROR.
    IF AVAIL b-oe-rel THEN DELETE b-oe-rel.
  END.
    */
END PROCEDURE.

FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN
  FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.

  RUN oe/rel-stat.p (IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?, OUTPUT lv-stat).

  RETURN lv-stat.
  
END FUNCTION.

FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bf1-oe-rel FOR oe-rel.
  IF AVAIL bf-oe-rel THEN
      FIND bf1-oe-rel WHERE ROWID(bf1-oe-rel) EQ ROWID(bf-oe-rel)
        NO-LOCK NO-ERROR.
  IF NOT AVAIL bf1-oe-rel THEN DO:
    FIND bf1-oe-rel WHERE RECID(bf1-oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL bf1-oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR.
  END.
   FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR.
             
  RETURN IF /*(NOT oereleas-log AND INDEX("AB",get-rel-stat()) GT 0) OR*/
            (INDEX("SIL",bf-oe-rel.stat /* get-rel-stat()*/) GT 0     
             OR bf-oe-rel.stat EQ "") THEN 0
         ELSE
         IF AVAIL tt-report        /*         AND
            INDEX("AB",get-rel-stat()) GT 0 */ THEN tt-report.qty
         ELSE
             bf-oe-rel.qty.

         /*ELSE
         IF AVAIL bf-oe-rel THEN bf-oe-rel.qty
         ELSE oe-rel.qty */.

END FUNCTION.


PROCEDURE get-act-qty:
  DEF INPUT PARAMETER ipr-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAMETER opd-qty AS DECIMAL NO-UNDO.
  DEF VAR    relpost-chr       AS CHAR    NO-UNDO.
  DEF BUFFER bf-oe-rel FOR oe-rel.
  DEF BUFFER bf1-oe-rel FOR oe-rel.
  DEF BUFFER b-oe-rell FOR oe-rell.
  DEF BUFFER b-oe-rell-exc FOR oe-rell.
  
  FIND bf1-oe-rel WHERE ROWID(bf1-oe-rel) EQ ipr-rowid NO-LOCK NO-ERROR.
  
  IF NOT AVAIL bf1-oe-rel THEN
  return.
  
  FOR EACH oe-ordl WHERE oe-ordl.company = bf1-oe-rel.company
    and oe-ordl.i-no   = bf1-oe-rel.i-no 
    AND oe-ordl.ord-no = bf1-oe-rel.ord-no NO-LOCK.

    
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN
    NEXT.
/*  09041301 - also update closed orders */
/*     IF oe-ord.stat EQ "Z" OR oe-ord.stat = "C"  THEN */
/*     NEXT.                                            */
    
    FIND FIRST bf-oe-rel 
      WHERE bf-oe-rel.company EQ oe-ordl.company
        AND bf-oe-rel.ord-no = oe-ordl.ord-no
        AND bf-oe-rel.LINE    = oe-ordl.LINE
        AND ROWID(bf-oe-rel) EQ ipr-rowid
    NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-oe-rel  THEN
    NEXT.
    
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company  EQ oe-ordl.company
          AND oe-boll.ord-no   EQ oe-ordl.ord-no
          AND oe-boll.i-no     EQ oe-ordl.i-no
          AND oe-boll.LINE     EQ oe-ordl.LINE 
        and oe-boll.po-no    eq bf-oe-rel.po-no 
        and oe-boll.rel-no eq bf-oe-rel.rel-no 
        and v-current-r-no GT 0 
        AND oe-boll.r-no eq v-current-r-no
      USE-INDEX ord-no,
      
      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
      
      first oe-rell NO-LOCK
        WHERE oe-rell.company  EQ oe-boll.company
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.LINE     EQ oe-boll.LINE
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.r-no     EQ oe-boll.r-no
/*          AND oe-rell.r-no     EQ bf-oe-rel.link-no */
/*          and oe-rell.r-no     eq v-current-r-no    */
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
          AND oe-rell.po-no    EQ oe-boll.po-no
      USE-INDEX ord-no,
      
      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-boll.r-no
      
      BREAK BY oe-boll.r-no
      BY oe-boll.rel-no
      BY oe-boll.b-ord-no
      BY oe-boll.po-no
      
      :
      
    IF FIRST-OF(oe-boll.po-no) THEN do:
       /*      lv-qty = 0. */
    end.
      lv-qty = lv-qty + oe-boll.qty.

      IF LAST-OF(oe-boll.po-no) AND lv-qty NE 0 THEN DO:
/*        RELEASE oe-rel. */
        
      END. /* last of po # and lv-qty gt 0 */
    END. /* each oe-boll of oe-ordl */
    
    FOR EACH oe-rell
      WHERE oe-rell.company  EQ bf1-oe-rel.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.LINE     EQ oe-ordl.LINE
        /* AND oe-rell.r-no     EQ bf-oe-rel.link-no */
        AND oe-rell.po-no    eq bf-oe-rel.po-no
        AND v-current-r-no GT 0 
        AND oe-rell.r-no eq v-current-r-no
        AND NOT CAN-FIND(FIRST oe-boll
      WHERE oe-boll.company  EQ oe-rell.company
        AND oe-boll.r-no     EQ oe-rell.r-no
        AND oe-boll.ord-no   EQ oe-rell.ord-no
        AND oe-boll.i-no     EQ oe-rell.i-no
        AND oe-boll.LINE     EQ oe-rell.LINE
        AND oe-boll.rel-no   EQ oe-rell.rel-no
        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        AND oe-boll.po-no    EQ oe-rell.po-no
      USE-INDEX ord-no)
      USE-INDEX ord-no  ,
      
      FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no    EQ oe-rell.r-no
          AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing")
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no
      
      :
      
    IF FIRST-OF(oe-rell.po-no) THEN do:      
      /*      lv-qty = 0. */
    end.

      lv-qty = lv-qty + oe-rell.qty.
      IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
        RELEASE b-oe-rell.
        IF oe-relh.posted THEN
        FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
               AND b-oe-rell.r-no    EQ oe-rell.r-no
               AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
               AND CAN-FIND(FIRST oe-boll
              WHERE oe-boll.company  EQ b-oe-rell.company
                 AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                 AND oe-boll.i-no     EQ b-oe-rell.i-no
                 AND oe-boll.LINE     EQ b-oe-rell.LINE
                 AND oe-boll.r-no     EQ b-oe-rell.r-no
                 AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                 AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                 AND oe-boll.po-no    EQ b-oe-rell.po-no
              USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:
          
          LEAVE.
        END.
        
        IF NOT AVAIL b-oe-rell THEN DO:
        END. /* not can find oe-rell with an oe-boll */
      END. /* last-of po-num and lv-qty gt 0 */
    END. /* each oe-rell with no oe-boll */
    
/*  RELEASE oe-rel. */
    RELEASE b-oe-rell.
    RELEASE oe-rell.
    RELEASE oe-boll.
    
    opd-qty = lv-qty.
    LEAVE.
  END.
    

END PROCEDURE.
