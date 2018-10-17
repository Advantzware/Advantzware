/* -------------------------------------------------- oe/oe-bolp3.p 06/98 JLF */
/* ORDER ENTRY MODULE - O/E RELEASE POST / CREATE BOLS                        */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM v-term AS CHAR NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF NEW SHARED BUFFER xinv-head FOR inv-head.
DEF NEW SHARED BUFFER xoe-relh  FOR oe-relh.
DEF NEW SHARED BUFFER yoe-relh  FOR oe-relh.
DEF NEW SHARED BUFFER xoe-rell  FOR oe-rell.
DEF NEW SHARED BUFFER xoe-boll  FOR oe-boll.

DEF SHARED BUFFER xoe-ord FOR oe-ord.
DEF BUFFER xfg-bin FOR fg-bin.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER xreport FOR report.
DEF BUFFER b-bolh FOR oe-bolh.
DEF BUFFER b-invl FOR inv-line.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-reftable2 FOR reftable.
DEFINE BUFFER bf-report FOR report.
DEF NEW SHARED VAR v-tax-rate AS DEC FORMAT ">,>>9.99<<<".
DEF NEW SHARED VAR v-frt-tax-rate LIKE v-tax-rate.
DEF NEW SHARED VAR v-u-inv LIKE oe-ctrl.u-inv INIT NO.
DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEF NEW SHARED VAR price-ent AS LOG NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF NEW SHARED VAR save_id AS RECID NO-UNDO.
DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.

{oe/oe-bolpi.i NEW}

DEF NEW SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEF NEW SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

DEF VAR v-relstat AS CHAR NO-UNDO.
DEF VAR v-ref-no AS INT NO-UNDO.
DEF VAR v-rcpth-no AS INT NO-UNDO.
DEF VAR v-frst AS LOG NO-UNDO.
DEF VAR v-ext-price LIKE inv-line.t-price NO-UNDO.
DEF VAR v-fg-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR v-po-no LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-uom LIKE itemfg.prod-uom NO-UNDO.

DEF VAR f AS INT NO-UNDO.
DEF VAR v-ASSIGN-comm AS LOG INIT NO NO-UNDO.
DEF VAR exist-amt AS DEC NO-UNDO.
DEF VAR exist-flag AS LOG INIT NO NO-UNDO.
DEF VAR exist-comm AS DEC EXTENT 3 INIT 0 NO-UNDO.
DEF VAR temp-tax AS DEC INIT 0 NO-UNDO.
DEF VAR v-format LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR v-rel-qty LIKE oe-rell.qty NO-UNDO.
DEF VAR v-complete AS LOG NO-UNDO.
DEF VAR bo-try AS INT NO-UNDO.
DEF VAR v-rel-no LIKE oe-rell.rel-no NO-UNDO.
DEF VAR v-b-ord-no LIKE oe-relh.b-ord-no NO-UNDO.
DEF VAR ld-sets AS DEC DECIMALS 10 NO-UNDO.
DEF VAR ll-calc-disc-FIRST AS LOG NO-UNDO.
DEF VAR v-cost AS DEC EXTENT 4 NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis INIT "" NO-UNDO.
DEF VAR fg-uom-list AS cha NO-UNDO.
DEF BUFFER b-reftable FOR reftable.
DEF BUFFER b-reftable3 FOR reftable.
DEF VAR ls AS CHAR NO-UNDO.
DEF VAR v-line-count AS INT NO-UNDO.
DEF VAR v-start-pos AS INT INIT 1 NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-new-lot-code AS CHAR NO-UNDO.
DEF VAR v-new-frt-pay AS CHAR NO-UNDO.
DEF VAR v-new-fob-code AS CHAR NO-UNDO.
DEF VAR v-fob-code AS CHAR NO-UNDO.
DEF VAR v-new-sell-price AS DEC NO-UNDO.
DEF VAR v-new-zero-price AS LOG NO-UNDO.
DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.
DEF VAR v-rtn-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.
DEF VAR invstatus-char AS CHAR NO-UNDO.
DEF VAR invstatus-log AS LOG NO-UNDO.
DEF VAR ldShip-qty AS DEC NO-UNDO.
DEF VAR ld-Inv AS DEC NO-UNDO.
DEF VAR cRelSCode AS CHAR NO-UNDO.
DEF VAR iTotalIScode AS INT NO-UNDO.
DEF VAR iTotalSScode AS INT NO-UNDO.
DEFINE VARIABLE cTermPrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE rCurrentInvHeadRow AS ROWID  NO-UNDO.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER bf-oe-boll FOR oe-boll.

RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

DEF TEMP-TABLE w-inv NO-UNDO FIELD w-rowid AS ROWID.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE upsFile AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttblUPS NO-UNDO
  FIELD company AS CHAR
  FIELD ord-no AS INT
  FIELD bol-no AS INT
  FIELD sold-to AS CHAR
  FIELD invHeadRowID AS ROWID
  FIELD cod AS LOGICAL
    INDEX ttblUPS IS PRIMARY UNIQUE company ord-no sold-to.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.
RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.
           
DEFINE VARIABLE lUseLogs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cDebugLog AS CHARACTER NO-UNDO.
DEFINE STREAM sDebug.
lUseLogs = TRUE. /* Use debug logging unless it's turned off */
if search("custfiles\logs\" + "block-oe-bolp3-logging.txt") ne ? then 
    lUseLogs = FALSE.
cDebugLog = "custfiles\logs\" + "oe-bolp3" + STRING(TODAY,"99999999") + STRING(TIME) + STRING(RANDOM(1,1000)) + ".txt".
IF lUseLogs THEN 
    OUTPUT STREAM sDebug TO VALUE(cDebugLog).
IF ERROR-STATUS:ERROR THEN 
    lUseLogs = FALSE.

/* First part of the term value */
cTermPrefix = string(year(today),"9999")      +
              string(month(today),"99")       +
              string(day(today),"99").
/* ************************  Function Implementations ***************** */
FUNCTION fLogMsg RETURNS CHARACTER 
    (INPUT ipcMessage AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    IF lUseLogs THEN 
    DO:
        OUTPUT STREAM sDebug CLOSE. 
        OUTPUT STREAM sDebug TO VALUE(cDebugLog) append.
        PUT STREAM sDebug UNFORMATTED ipcMessage SKIP.
    END.
    RETURN cResult.
END FUNCTION.

FUNCTION tabChar RETURNS CHARACTER (ipValue AS CHARACTER):
  RETURN IF ipValue NE '' THEN '~t' ELSE ''.
END FUNCTION.

STATUS DEFAULT .

/* {sys/inc/upsfile.i} */

/* upsFile = sys-ctrl.char-fld. */

DO TRANSACTION:
  {sys/inc/boltransfer.i}
END.

{fg/fullset.i NEW}
{sys/ref/relpost.i}



DO TRANSACTION:
  {sys/inc/invdate.i}
  {sys/inc/relmerge.i}
  {sys/inc/invlotline.i}
  {sys/inc/boreldate.i}
END.

/* Needed for oe/relbo.i & oe/actrelmerg.p */
DEF NEW SHARED VAR out-recid  AS RECID NO-UNDO.
DEF NEW SHARED VAR relh-recid as recid no-undo.
DEF NEW SHARED VAR v-auto     AS LOG NO-UNDO.
{oe/chkordl.i NEW}
{oe/relemail.i NEW}

/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", no, no, "", "", 
                      Output v-rtn-char, output v-rec-found).
invstatus-log = LOGICAL(v-rtn-char).
/* Invstatus to determine invoice status when created  */
RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", no, no, "", "", 
                      Output invstatus-char, output v-rec-found).
/* UPSFILE to for CSV creation in oe-bolh.trailer = "UPS"  */
RUN sys/ref/nk1look.p (cocode, "UPSFILE", "C", no, no, "", "", 
                      Output upsFile, output v-rec-found).
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "INVPRINT"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "INVPRINT"
   sys-ctrl.descrip = "Print Invoice Headers on Invoice Form".
  MESSAGE "Invoice Format:" UPDATE sys-ctrl.char-fld.
END.
ASSIGN
 v-format           = sys-ctrl.char-fld
 ll-calc-disc-FIRST = v-format EQ "Dayton".

IF v-format EQ "HARWELL" THEN invdate-chr = "Current".
                              
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
v-u-inv = oe-ctrl.u-inv.
fLogMsg("Start oe-bolp3 user: " + USERID("nosweat")).
fLogMsg("Start oe-bolp3 Pgm: " + PROGRAM-NAME(1)).
fLogMsg("Start oe-bolp3 Pgm: " + PROGRAM-NAME(2)).
fLogMsg("Start oe-bolp3 Pgm: " + PROGRAM-NAME(3)).
fLogMsg("Start oe-bolp3 Pgm: " + PROGRAM-NAME(4)).
fLogMsg("Start oe-bolp3 Pgm: " + PROGRAM-NAME(5)).
    
EMPTY TEMP-TABLE w-inv.

DO TRANSACTION:
  RUN check-posted.

  FOR EACH report WHERE report.term-id EQ v-term,
      FIRST oe-boll NO-LOCK
      WHERE RECID(oe-boll) EQ report.rec-id
        AND oe-boll.s-code EQ "T":

    CREATE tt-report.
    BUFFER-COPY report TO tt-report.
    DELETE report.    
  END.

  RUN check-posted.

  /* Check for enough components to invoice set header,
     create invoice only release and BOL lines */
  FOR EACH report NO-LOCK WHERE report.term-id EQ v-term,

      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
        
      FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no,
    
      FIRST oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-boll.company
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        AND oe-rell.r-no    EQ oe-boll.r-no
        AND oe-rell.i-no    EQ oe-boll.i-no
        AND oe-rell.line    EQ oe-boll.line,

      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-rell.r-no,
        
      FIRST oe-ordl NO-LOCK
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
        AND oe-ordl.is-a-component EQ YES
        AND oe-ordl.set-hdr-line   NE 0
      USE-INDEX ord-no

      BREAK BY oe-ordl.ord-no
            BY oe-ordl.set-hdr-line
            BY oe-ordl.line:
STATUS DEFAULT "Processing BOL Posting 1........ BOL#: " + STRING(oe-bolh.bol-no).
    IF LAST-OF(oe-ordl.set-hdr-line) THEN DO:
      /* Find the set header line */
      FIND FIRST b-oe-ordl NO-LOCK
          WHERE b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
            AND b-oe-ordl.line    EQ oe-ordl.set-hdr-line
            AND b-oe-ordl.is-a-component EQ NO
            AND ROWID(b-oe-ordl) NE ROWID(oe-ordl)
          NO-ERROR.

      IF AVAIL b-oe-ordl THEN DO:
        /* Calc all qty shipped for this set header oe-ordl */
        RUN oe/ordlsets.p (ROWID(b-oe-ordl), v-term, OUTPUT ld-sets).
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ b-oe-ordl.company
              AND itemfg.i-no    EQ b-oe-ordl.i-no
            USE-INDEX i-no NO-ERROR.
        IF AVAIL itemfg AND itemfg.isaset THEN DO:
          ld-sets = TRUNC(ld-sets, 0).
        END.
        
        /* Sets for header item GT header item t-inv-qty*/
        IF ld-sets GT b-oe-ordl.t-inv-qty THEN DO:
          FIND FIRST itemfg NO-LOCK
              WHERE itemfg.company EQ b-oe-ordl.company
                AND itemfg.i-no    EQ b-oe-ordl.i-no
              USE-INDEX i-no NO-ERROR.

          ldShip-qty = 0.

          RUN oe/ordlsqty.p (ROWID(b-oe-ordl),
                             OUTPUT ld-inv,
                             OUTPUT ldShip-Qty).

          
          ld-sets = ld-sets - /* b-oe-ordl.ship-qty */ ldShip-Qty.
          
          CREATE xoe-rell.
          BUFFER-COPY oe-rell EXCEPT rec_key TO xoe-rell
          ASSIGN
           xoe-rell.i-no     = b-oe-ordl.i-no
           xoe-rell.line     = b-oe-ordl.line
           xoe-rell.rel-no   = 0
           xoe-rell.b-ord-no = 0
           xoe-rell.s-code   = "I"
           xoe-rell.qty      = ld-sets
           xoe-rell.qty-case = 1
           xoe-rell.cases    = ld-sets
           xoe-rell.partial  = 0
           xoe-rell.loc      = ""
           xoe-rell.loc-bin  = ""
           xoe-rell.tag      = ""
           xoe-rell.cust-no  = "".

          RELEASE fg-bin.

          FOR EACH fg-bin FIELDS(qty loc loc-bin tag) NO-LOCK
              WHERE fg-bin.company EQ xoe-rell.company
                AND fg-bin.job-no  EQ xoe-rell.job-no
                AND fg-bin.job-no2 EQ xoe-rell.job-no2
                AND fg-bin.i-no    EQ xoe-rell.i-no
              USE-INDEX job
              BY fg-bin.qty DESC:
            ASSIGN
             xoe-rell.loc      = fg-bin.loc
             xoe-rell.loc-bin  = fg-bin.loc-bin
             xoe-rell.tag      = fg-bin.tag.
            LEAVE.
          END.

          IF NOT AVAIL fg-bin AND AVAIL itemfg THEN
            RUN fg/autopost.p (ROWID(itemfg),
                               xoe-rell.job-no,
                               xoe-rell.job-no2,
                               OUTPUT xoe-rell.loc,
                               OUTPUT xoe-rell.loc-bin).

          FIND CURRENT xoe-rell NO-LOCK NO-ERROR.
          IF NOT AVAIL xoe-rell THEN
            MESSAGE "Error during posting - please notify ASI (oe-bolp3-281)"
              VIEW-AS ALERT-BOX.
          CREATE xoe-boll.
          BUFFER-COPY oe-boll EXCEPT rec_key TO xoe-boll
          ASSIGN
           xoe-boll.i-no     = b-oe-ordl.i-no
           xoe-boll.line     = b-oe-ordl.line
           xoe-boll.rel-no   = 0
           xoe-boll.b-ord-no = 0
           xoe-boll.s-code   = "I"
           xoe-boll.qty      = ld-sets
           xoe-boll.qty-case = 1
           xoe-boll.cases    = ld-sets
           xoe-boll.partial  = 0
           xoe-boll.loc      = xoe-rell.loc
           xoe-boll.loc-bin  = xoe-rell.loc-bin
           xoe-boll.tag      = xoe-rell.tag
           xoe-boll.cust-no  = xoe-rell.cust-no.

          CREATE xreport.
          BUFFER-COPY report EXCEPT rec_key TO xreport
          ASSIGN
           xreport.rec-id = RECID(xoe-boll)
           xreport.key-05 = STRING(xoe-boll.rel-no,"9999999999")
           xreport.key-06 = STRING(xoe-boll.b-ord-no,"9999999999").
          RELEASE xoe-boll.
        END. /*  ld-sets GT b-oe-ordl.t-inv-qty */
      END. /* IF AVAIL b-oe-ordl */
    END. /* LAST-OF(oe-ordl.set-hdr-line) */
  END. /* Each report */
  FOR EACH report NO-LOCK WHERE report.term-id BEGINS cTermPrefix
                            AND report.term-id NE v-term,
      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id:
      /* Check that someone else isn't posting the same BOL */
      
      FOR EACH bf-oe-boll NO-LOCK WHERE bf-oe-boll.b-no EQ oe-boll.b-no:
        FIND FIRST bf-report NO-LOCK 
            WHERE bf-report.term-id BEGINS cTermPrefix
              AND bf-report.rec-id  EQ report.rec-id
              AND bf-report.rec_key NE report.rec_key
            NO-ERROR.
          /* Found overlap with another user */
          IF AVAILABLE bf-report THEN 
            fLogMsg("Collision with other user oe-bolp3.p " + " BOL# " + STRING(oe-boll.bol-no) + " Key03: " + report.key-03
               + " term-id: " + bf-report.term-id + " rpt.term-id: " + report.term-id).          
      END.
      
      /* delete all lines related to this BOL as it is being posted by someone else */
      /* To be implemented */
/*      FOR EACH bf-oe-boll NO-LOCK WHERE bf-oe-boll.b-no EQ oe-boll.b-no,  */
/*          FIRST bf-report EXCLUSIVE-LOCK WHERE bf-report.term-id NE v-term*/
/*          AND bf-report.rec-id EQ RECID(bf-oe-boll):                      */
/*          /* Found overlap with another user */                           */
/*          DELETE bf-report.                                               */
/*      END.                                                                */
      
  END. 
  
/*  FOR EACH report NO-LOCK WHERE report.term-id EQ v-term,                   */
/*      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,          */
/*      FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no:             */
/*      /* Check that there is a valid shipto for this invoice */             */
/*      FIND FIRST shipto NO-LOCK                                             */
/*          WHERE shipto.company EQ oe-bolh.company                           */
/*          AND shipto.ship-id EQ oe-bolh.ship-id                             */
/*          AND shipto.cust-no EQ oe-bolh.cust-no                             */
/*          AND shipto.ship-no NE 1                                           */
/*          USE-INDEX ship-id NO-ERROR.                                       */
/*      IF NOT AVAILABLE shipto THEN DO:                                      */
/*        FOR EACH bf-oe-boll NO-LOCK WHERE bf-oe-boll.b-no EQ oe-boll.b-no,  */
/*            FIRST bf-report EXCLUSIVE-LOCK WHERE bf-report.term-id NE v-term*/
/*            AND bf-report.rec-id EQ RECID(bf-oe-boll):                      */
/*            DELETE bf-report.                                               */
/*        END.                                                                */
/*      END.                                                                  */
/*  END.                                                                      */
  
  RUN check-posted.
  
  rCurrentInvHeadRow = ?.
  FOR EACH report WHERE report.term-id EQ v-term,

      FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id,
        
      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no,
        
      FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
      USE-INDEX ord-no,
    
      FIRST oe-rell
      WHERE oe-rell.company EQ oe-boll.company
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        AND oe-rell.r-no    EQ oe-boll.r-no
        AND oe-rell.i-no    EQ oe-boll.i-no
        AND oe-rell.line    EQ oe-boll.line,

      FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no,
      
      FIRST cust NO-LOCK
      WHERE cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no,

      FIRST oe-ord
      WHERE oe-ord.company EQ oe-boll.company
        AND oe-ord.ord-no  EQ oe-boll.ord-no,
      
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
      USE-INDEX i-no
    
      BREAK BY report.key-01
            BY report.key-02
            BY report.key-03
            BY report.key-04
            BY report.key-05
            BY report.key-06
            BY report.key-07:

    STATUS DEFAULT "Processing BOL Posting 2........ BOL#: " + STRING(oe-bolh.bol-no).
    fLogMsg("Start oe-bolp3.i from oe-bolp3.p " + " BOL# " + STRING(oe-bolh.bol-no) + " Key03: " + report.key-03 + " ino: " + oe-boll.i-no).
    /* Note: oe-bolp3 requires locks on oe-ord, oe-ordl, oe-bolh, oe-boll */
    {oe/oe-bolp3.i "report" "key-03"}
    
    CREATE tt-report.
    BUFFER-COPY report TO tt-report.
    DELETE report.
    fLogMsg("End oe-bolp3.i in oe-bolp3.p" + " BOL#: " + STRING(oe-bolh.bol-no)  + " item: " + oe-boll.i-no).    
  END.

  /* Check for and Create Backorder Releases */
  FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,
        
      FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no,
        
      FIRST oe-ordl
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
      USE-INDEX ord-no,
    
      FIRST cust NO-LOCK
      WHERE cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no,

      FIRST oe-ord NO-LOCK
      WHERE oe-ord.company EQ oe-boll.company
        AND oe-ord.ord-no  EQ oe-boll.ord-no,
      
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
      USE-INDEX i-no
      
      BREAK BY oe-boll.i-no
            BY oe-boll.r-no
            BY oe-boll.line
            BY oe-boll.ord-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no:
    STATUS DEFAULT "Processing BOL Posting 3........ BOL#: " + STRING(oe-bolh.bol-no).       
    
    {oe/oe-relbo.i oe-boll.b-ord-no}

  END.

  FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,
        
      FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no
      BREAK BY oe-boll.r-no BY oe-boll.i-no BY oe-boll.ord-no:
      STATUS DEFAULT "Processing BOL Posting 4........ BOL#: " + STRING(oe-bolh.bol-no).




      IF LAST-OF(oe-boll.ord-no) THEN DO:
        FIND FIRST oe-rell WHERE oe-rell.r-no EQ oe-boll.r-no
               and oe-rell.i-no eq oe-boll.i-no
            NO-LOCK NO-ERROR.
        IF AVAIL oe-rell THEN           
        FIND FIRST oe-rel WHERE oe-rel.link-no = oe-rell.r-no
             EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL oe-rel THEN DO:
             RUN oe/rel-stat.p (INPUT ROWID(oe-rel), OUTPUT v-relstat).
             IF v-relstat NE oe-rel.stat THEN
               oe-rel.stat = v-relstat.
             FIND itemfg-loc WHERE itemfg-loc.company EQ oe-boll.company
                AND itemfg-loc.i-no EQ oe-boll.i-no
                AND itemfg-loc.loc  EQ oe-rel.spare-char-1
             EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
             IF AVAIL itemfg-loc THEN
                 RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, 
                                    OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
             RELEASE oe-rel.
             RELEASE itemfg-loc.
        END.
        ELSE DO:        
          FOR EACH oe-rel WHERE oe-rel.company = oe-boll.company
                            AND oe-rel.ord-no  = oe-boll.ord-no
                            AND oe-rel.i-no    = oe-boll.i-no
                          EXCLUSIVE-LOCK.
             RUN oe/rel-stat.p (INPUT ROWID(oe-rel), OUTPUT v-relstat).
             IF v-relstat NE oe-rel.stat THEN
               oe-rel.stat = v-relstat.
             FIND itemfg-loc WHERE itemfg-loc.company EQ oe-boll.company
                AND itemfg-loc.i-no EQ oe-boll.i-no
                AND itemfg-loc.loc  EQ oe-rel.spare-char-1
             EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
             IF AVAIL itemfg-loc THEN
                 RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, 
                                    OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
             RELEASE oe-rel.
             RELEASE itemfg-loc.
  
          END. /* Each oe-rel */
        END. /* Oe-rel not found */
      END. /* last-of ord-no */

  END. /* each tt-report */

END. /* Do trans */

FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

  FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,

  FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no
  BREAK BY oe-boll.i-no BY oe-boll.ord-no:

      /* If the release is ship-only and we're posting the BOL,               */
      /* clean up the fg-bin records that are placeholders, total the + and - */
      IF  oe-boll.s-code EQ "S" THEN do: 
          
          FIND FIRST oe-rell WHERE oe-rell.r-no EQ oe-boll.r-no
               and oe-rell.i-no eq oe-boll.i-no
            NO-LOCK NO-ERROR.          

          IF AVAIL oe-rell THEN 
            RUN oe/cleanShipOnlyBins.p (INPUT ROWID(oe-rell)).
 
          FIND FIRST oe-ordl WHERE oe-ordl.company EQ oe-boll.company
              AND oe-ordl.ord-no EQ oe-boll.ord-no
              AND oe-ordl.LINE EQ oe-boll.LINE
              NO-LOCK NO-ERROR.
          
          IF NOT AVAIL oe-ordl THEN
            NEXT.

          iTotalISCode = 0.
          iTotalSScode = 0.
          FOR EACH oe-rel WHERE oe-rel.company EQ oe-boll.company
            AND oe-rel.ord-no EQ oe-boll.ord-no
            AND oe-rel.i-no   EQ oe-boll.i-no
            NO-LOCK:
            RUN getOeRelSCode (INPUT ROWID(oe-rel), OUTPUT cRelSCode).

            IF cRelSCode EQ "I" THEN
              iTotalISCode = iTotaliSCode + oe-rel.qty.
            ELSE
              IF cRelSCode EQ "S" THEN
                iTotalSSCode = iTotalSSCode + oe-rel.qty.

          END.
       

          
          IF iTotalISCode GT 0 AND iTotalISCode NE iTotalSSCode THEN DO:
 
            /* Missing a placeholder records, so run fg-mkbin */
            FIND FIRST itemfg WHERE itemfg.company EQ oe-boll.company
               AND itemfg.i-no EQ oe-boll.i-no
              NO-LOCK NO-ERROR.
            IF AVAIL itemfg THEN
              RUN fg/fg-mkbin.p (INPUT RECID(itemfg)).
            


         END.
         LEAVE.
      END.
END.

FOR EACH w-inv:
  /*RUN oe/invcheck.p (w-rowid).*/
  IF CAN-FIND(inv-head WHERE ROWID(inv-head) EQ w-rowid) THEN DO:      
      RUN oe/oe-invup.p (w-rowid, INPUT YES).      
  END.
    
END.

RUN upsFile.

DELETE OBJECT hNotesProcs.

STATUS DEFAULT.

RETURN.

/* end --------------------------------- copyright 1998 Advanced Software Inc.*/



/* ************************  Function Prototypes ********************** */
FUNCTION fLogMsg RETURNS CHARACTER 
    (INPUT ipcMessage AS CHARACTER  ) FORWARD.

PROCEDURE check-posted:
  FOR EACH report WHERE report.term-id EQ v-term,
      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
      FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no   EQ oe-boll.b-no:

    IF (oe-bolh.deleted OR oe-bolh.posted)
        AND oe-boll.s-code <> "T"  THEN DELETE report.
  END.
END.

PROCEDURE createUPS:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSoldTo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOrdNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBolNo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipTerms AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  IF CAN-FIND(ttblUPS WHERE ttblUPS.company EQ ipCompany
                        AND ttblUPS.ord-no EQ ipOrdNo
                        AND ttblUPS.bol-no EQ ipBolNo
                        AND ttblUPS.sold-to EQ ipSoldTo) THEN RETURN.
  FIND terms NO-LOCK WHERE terms.company EQ ipCompany
                       AND terms.t-code EQ ipTerms NO-ERROR.
  IF AVAILABLE terms THEN

  CREATE ttblUPS.
  ASSIGN
    ttblUPS.company = ipCompany
    ttblUPS.ord-no = ipOrdNO
    ttblUPS.bol-no = ipBolNo
    ttblUPS.sold-to = ipSoldTo
    ttblUPS.invHeadRowID = ipRowID                                         
    ttblUPS.cod = IF AVAILABLE terms THEN terms.cod ELSE FALSE.
END PROCEDURE.

PROCEDURE upsFile:
  DEFINE VARIABLE attnContact AS CHARACTER NO-UNDO.
  DEFINE VARIABLE emailAddr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE printHeaderLine AS LOGICAL NO-UNDO.
  DEFINE VARIABLE codAmount AS DECIMAL NO-UNDO.

  DEFINE BUFFER bCust FOR cust.
  DEFINE BUFFER bInvHead FOR inv-head.

  IF NOT CAN-FIND(FIRST ttblUPS) OR upsFile EQ '' THEN RETURN.
  printHeaderLine = SEARCH(upsFile) EQ ?.
  OUTPUT TO VALUE(upsFile) APPEND.
  IF printHeaderLine THEN
  EXPORT DELIMITER '~t'
    'Order#' 'BOL#' 'Ship ID' 'Name' 'Address 1' 'Address 2' 'City' 'State' 'Zip'
    'Contact' 'EMail' 'COD Amt'.
  FOR EACH ttblUPS NO-LOCK:
    FIND bInvHead NO-LOCK WHERE ROWID(bInvHead) EQ ttblUPS.invHeadRowID NO-ERROR.
    IF NOT AVAILABLE bInvHead THEN NEXT.
    FIND FIRST bCust NO-LOCK WHERE bCust.company EQ bInvHead.company
                               AND bCust.cust-no EQ bInvHead.cust-no NO-ERROR.
    IF NOT AVAILABLE bCust THEN NEXT.
    ASSIGN
      emailAddr = ''
      attnContact = ''.
    FOR EACH phone NO-LOCK WHERE phone.table_rec_key EQ bCust.rec_key:
      IF CAN-FIND(FIRST emaildtl
                  WHERE emaildtl.emailcod EQ 'R-BolPrt.'
                    AND emaildtl.table_rec_key EQ phone.rec_key) THEN DO:
        ASSIGN
          emailAddr = IF phone.e_mail NE '' THEN phone.e_mail ELSE bCust.email
          attnContact = phone.attention.
        LEAVE. /* use FIRST one found */
      END. /* if can-find */
    END. /* each phone */
    codAmount = IF ttblUPS.cod THEN bInvHead.t-inv-rev ELSE 0.
    EXPORT DELIMITER '~t'
      ttblUPS.ord-no
      ttblUPS.bol-no
      ttblUPS.sold-to
      bInvHead.sold-name
      bInvHead.sold-addr[1]
      bInvHead.sold-addr[2]
      bInvHead.sold-city
      bInvHead.sold-state
      bInvHead.sold-zip
      attnContact
      emailAddr
      codAmount.
  END. /* each ttblups */
  OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE getOeRelSCode:
/* This should be in a common function library */
DEFINE INPUT  PARAMETER iprOeRelRow AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER opcS-code AS CHARACTER   NO-UNDO.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF VAR v-reltype AS CHAR NO-UNDO.
DEF VAR ll-transfer AS LOG NO-UNDO.

  FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ iprOeRelRow NO-LOCK NO-ERROR.

   /* task 04011103*/
   FIND FIRST sys-ctrl where sys-ctrl.company eq cocode
                         and sys-ctrl.name eq "RelType" no-lock no-error.
   IF AVAIL sys-ctrl THEN
      FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                AND sys-ctrl-ship.ship-id = bf-oe-rel.ship-id NO-LOCK NO-ERROR.
      IF NOT AVAIL sys-ctrl-shipto THEN
      FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
   IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
   ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.

/*  FIND FIRST s-code                                               */
/*      WHERE s-code.reftable EQ "oe-rel.s-code"                    */
/*        AND s-code.company  EQ STRING(bf-oe-rel.r-no,"9999999999")*/
/*      NO-LOCK NO-ERROR.                                           */

  opcS-code = /*IF v-reltype <> "" THEN reftable.CODE
                     ELSE */ IF ll-transfer            THEN "T"
                     ELSE
                     IF oe-ordl.is-a-component AND
                        (bf-oe-rel.s-code = "" OR
                         bf-oe-rel.s-code NE "T")   THEN "S"
                     ELSE
                     IF bf-oe-rel.s-code <> ""           THEN bf-oe-rel.s-code
                     ELSE "B".
END.
