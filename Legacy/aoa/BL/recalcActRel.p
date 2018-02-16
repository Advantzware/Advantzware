&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    
/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.
IF TRUE THEN
RETURN.
DEF BUFFER s-code FOR reftable.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER ref-sell-price FOR reftable.

DEFINE VARIABLE char-hdl     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-rel-stat  AS cha       LABEL "" FORM "x" NO-UNDO.
DEFINE VARIABLE lv-rel-recid AS RECID     NO-UNDO.
DEF NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEFINE NEW SHARED VARIABLE out-recid AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE relh-recid AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto     AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nufile     AS LOGICAL NO-UNDO. /* for jc-calc.p */.
DEFINE NEW SHARED VARIABLE lv-qty     AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id     AS RECID   NO-UNDO.
DEFINE VARIABLE li-ship-no            AS INTEGER      NO-UNDO. /* if ship-to is changed */.
DEFINE VARIABLE ll-unposted           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE ls-po                 AS cha          NO-UNDO.
DEFINE VARIABLE ll-canceled           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lv-stat               AS cha          NO-UNDO.
DEFINE VARIABLE ld-date               AS DATE         NO-UNDO.
DEFINE VARIABLE ll-skip               AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lv-s-codes            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lv-s-dscrs            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lv-cust-x             LIKE cust.cust-no NO-UNDO.
DEFINE VARIABLE ll-transfer           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE v-browse-in-update    AS LOGICAL      NO-UNDO.
DEFINE VARIABLE v-cust-no             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE v-last-shipto         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE l-update-reason-perms AS LOGICAL      NO-UNDO.
DEFINE VARIABLE adm-cur-state         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE oeDateChange-log      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE v-rtn-char            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE v-rec-found           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE v-disp-rel-qty        AS DECIMAL      NO-UNDO.
DEFINE VARIABLE v-scr-s-code          AS CHARACTER    NO-UNDO.

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
  FIELD RELEASE# LIKE oe-relh.RELEASE#.

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

/* RUN methods/prgsecur.p                                                                                */
/* (  INPUT "OEDateChg",                                                                                 */
/*    INPUT "ALL", /* based on run, create, update, delete or all */                                     */
/*    INPUT NO,    /* use the directory in addition to the program */                                    */
/*    INPUT NO,    /* Show a message if not authorized */                                                */
/*    INPUT NO,    /* Group overrides user security? */                                                  */
/*    OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */                                                */
/*    OUTPUT v-access-close, /* used in template/windows.i  */                                           */
/*    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */  */
ASSIGN 
  l-update-reason-perms = TRUE.
  v-access-close = YES.
  v-access-list = "1111"
  .

RUN sys/ref/nk1look.p (cocode, "oeDateChange", "L", NO, NO, "", "",
                       OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.


{sys/inc/oeinq.i}
{sa/sa-sls01.i}

lv-cust-x = "".


FOR EACH cust NO-LOCK
  WHERE cust.company EQ cocode
  AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.


/* Main Block */
DEF BUFFER bf-oe-rel FOR oe-rel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-get-rel-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-qty Procedure 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-rel-stat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-rel-stat Procedure 
FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 12.76
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-build-report-file) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-file Procedure 
PROCEDURE build-report-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Sets lv-qty, runs create-report-record
------------------------------------------------------------------------------*/
DEFINE VARIABLE v-nxt-r-no LIKE oe-rel.r-no    NO-UNDO.
DEFINE VARIABLE lv-qty     AS INTEGER          NO-UNDO.
DEFINE VARIABLE lv-stat    AS CHARACTER        NO-UNDO.
DEFINE VARIABLE lv-s-code  LIKE oe-rell.s-code EXTENT 2 NO-UNDO.

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rell-exc FOR oe-rell.
DEF BUFFER b-oe-rel  FOR oe-rel.
  

ll-transfer = CAN-FIND(FIRST oe-ord
WHERE oe-ord.company EQ oe-ordl.company
AND oe-ord.ord-no  EQ oe-ordl.ord-no
AND oe-ord.TYPE    EQ "T").

RUN delete-phantoms.

FOR EACH tt-report:
 DELETE tt-report.
END.


FOR EACH oe-rel NO-LOCK
  WHERE oe-rel.company EQ oe-ordl.company
    AND oe-rel.ord-no  EQ oe-ordl.ord-no
    AND oe-rel.i-no    EQ oe-ordl.i-no
    AND oe-rel.LINE    EQ oe-ordl.LINE
        and oe-rel.po-no   eq bf-oe-rel.po-no
        and oe-rel.link-no eq v-current-r-no
  USE-INDEX ord-item
    
  BREAK BY oe-rel.rel-no
  BY oe-rel.b-ord-no
  BY oe-rel.po-no:
    
    IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN DO:
      RUN create-report-record (ROWID(oe-rel), NO).
    END.

END. /* each oe-rel */

FOR EACH oe-boll NO-LOCK
 WHERE oe-boll.company  EQ oe-ordl.company
 AND oe-boll.ord-no   EQ oe-ordl.ord-no
 AND oe-boll.i-no     EQ oe-ordl.i-no
 AND oe-boll.LINE     EQ oe-ordl.LINE
 AND oe-boll.po-no    eq bf-oe-rel.po-no
 AND (if v-current-r-no eq 0 then true else oe-boll.r-no eq v-current-r-no)
 USE-INDEX ord-no,
 
 FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
 
 FIRST oe-rell NO-LOCK
   WHERE oe-rell.company  EQ oe-boll.company
     AND oe-rell.ord-no   EQ oe-boll.ord-no
     AND oe-rell.LINE     EQ oe-boll.LINE
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
 BY oe-boll.po-no:
 
    IF FIRST-OF(oe-boll.po-no) THEN do: 
/* calculation should be everything for this oe-rel */      
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
       AND oe-rel.LINE     EQ oe-rell.LINE
       AND oe-rel.rel-no   EQ oe-rell.rel-no
       AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
       AND oe-rel.po-no    EQ oe-rell.po-no
   USE-INDEX link NO-ERROR.
   IF NOT AVAIL oe-rel THEN
   FIND FIRST oe-rel NO-LOCK
     WHERE oe-rel.company  EQ oe-rell.company
       AND oe-rel.ord-no   EQ oe-rell.ord-no
       AND oe-rel.i-no     EQ oe-rell.i-no
       AND oe-rel.LINE     EQ oe-rell.LINE
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
         AND b-oe-rell.LINE     EQ oe-rel.LINE
         AND b-oe-rell.rel-no   EQ oe-rel.rel-no
         AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
         AND b-oe-rell.po-no    EQ oe-rel.po-no
       USE-INDEX r-no:
       FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
       NO-LOCK NO-ERROR.
      
     END.

   END. /* if avail oe-rel */
   
   ELSE DO:
      /* Would have created a new one here */
   END.
 END. /* last of po-no */
END. /* each oe-boll */

FOR EACH oe-rell
 WHERE oe-rell.company  EQ cocode
   AND oe-rell.ord-no   EQ oe-ordl.ord-no
   AND oe-rell.i-no     EQ oe-ordl.i-no
   AND oe-rell.LINE     EQ oe-ordl.LINE
        and oe-rell.po-no    eq bf-oe-rel.po-no
        and (if v-current-r-no eq 0 then true else oe-rell.r-no eq v-current-r-no)        
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
 USE-INDEX ord-no NO-LOCK,
 
 FIRST oe-relh NO-LOCK
 WHERE oe-relh.r-no    EQ oe-rell.r-no
 AND (oe-relh.posted EQ NO OR relpost-chr EQ "Nothing")
 
 BREAK BY oe-rell.r-no
 BY oe-rell.rel-no
 BY oe-rell.b-ord-no
 BY oe-rell.po-no:
 
      

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
     AND oe-rel.LINE     EQ oe-rell.LINE
     AND oe-rel.rel-no   EQ oe-rell.rel-no
     AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
     AND oe-rel.po-no    EQ oe-rell.po-no
     USE-INDEX link NO-ERROR.
     
     
     IF NOT AVAIL oe-rel THEN
     FIND FIRST oe-rel NO-LOCK
        WHERE oe-rel.company  EQ oe-rell.company
           AND oe-rel.ord-no   EQ oe-rell.ord-no
           AND oe-rel.i-no     EQ oe-rell.i-no
           AND oe-rel.LINE     EQ oe-rell.LINE
           AND oe-rel.rel-no   EQ oe-rell.rel-no
           AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
           AND oe-rel.po-no    EQ oe-rell.po-no
        USE-INDEX ord-item NO-ERROR.
     DO:
       FIND CURRENT oe-rel NO-LOCK NO-ERROR.
       
       IF AVAIL oe-rel THEN DO:
         IF oe-relh.posted THEN DO:
           
           FOR EACH b-oe-rell NO-LOCK
             WHERE b-oe-rell.company  EQ oe-rel.company
                AND b-oe-rell.r-no     EQ oe-rel.link-no
                AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                AND b-oe-rell.i-no     EQ oe-rel.i-no
                AND b-oe-rell.LINE     EQ oe-rel.LINE
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
         END. /* else do */
       END. /* avail oe-rel */
     END. /* Do... with current rel */
   END. /* not avail b-oerell */
 END. /* last-of po-no */
END. /* Each oe-rell */


FOR EACH oe-rel NO-LOCK
 WHERE oe-rel.company EQ oe-ordl.company
 AND oe-rel.ord-no  EQ oe-ordl.ord-no
 AND oe-rel.i-no    EQ oe-ordl.i-no
 AND oe-rel.LINE    EQ oe-ordl.LINE
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-report-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record Procedure 
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
 
 RUN create-report-record-1 
         (ip-phantom,
          IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-report-record-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report-record-1 Procedure 
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
      tt-report.po-no   = oe-rel.po-no
      tt-report.qty     = oe-rel.qty
      tt-report.printed = (AVAIL oe-relh AND oe-relh.printed) OR
      INDEX("PCZ",lv-stat) GT 0
      tt-report.RELEASE# = (IF AVAIL oe-relh THEN oe-relh.RELEASE# ELSE 0).
  
  /* task 04011103*/
  FIND FIRST sys-ctrl 
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME EQ "RelType" 
    NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl 
      WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
        AND sys-ctrl-ship.ship-id = oe-rel.ship-id 
     NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl-shipto THEN
    FIND FIRST sys-ctrl-shipto OF sys-ctrl 
      WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
        AND sys-ctrl-ship.ship-id = "" 
      NO-LOCK NO-ERROR.

  IF AVAIL sys-ctrl-shipto 
      AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.

  ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.


  ASSIGN tt-report.s-code = IF v-reltype <> "" THEN oe-rel.s-code
       ELSE IF ll-transfer            THEN "T"
       ELSE
        IF oe-ordl.is-a-component AND
         (NOT AVAIL s-code OR s-code.CODE NE "T")   THEN "S"
        ELSE
          IF AVAIL s-code           THEN s-code.CODE
            ELSE
          IF AVAIL oe-rell          THEN oe-rell.s-code
            ELSE "B".

    ASSIGN
        tt-report.lot-no      = oe-rel.lot-no
        tt-report.freight-pay = oe-rel.frt-pay
        tt-report.fob         = oe-rel.fob-code.
  
/*  FIND FIRST ref-sell-price                                          */
/*    WHERE ref-sell-price.reftable EQ "oe-rel.sell-price"             */
/*      AND ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")*/
/*    NO-LOCK NO-ERROR.                                                */
/*                                                                     */
/*  IF AVAIL ref-sell-price THEN                                       */
/*  DO:                                                                */
/*    ASSIGN tt-report.sell-price = ref-sell-price.val[1]              */
/*           tt-report.zero-sprice = ref-sell-price.val[2] > 0.        */
/*    RELEASE ref-sell-price.                                          */
/*  END.                                                               */
    ASSIGN
        tt-report.sell-price = oe-rel.sell-price              
        tt-report.zero-sprice = oe-rel.zeroPrice GT 0
        . 
  IF oeinq THEN
  tt-report.key-01 = STRING(9999999999 - INT(tt-report.key-01),"9999999999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-delete-phantoms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-phantoms Procedure 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-act-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-act-qty Procedure 
PROCEDURE get-act-qty :
DEF INPUT PARAMETER ipr-rowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAMETER opd-qty AS DECIMAL NO-UNDO.

  DEFINE VARIABLE relpost-chr AS CHARACTER NO-UNDO.
  
  DEF BUFFER bf-oe-rel FOR oe-rel.
  DEF BUFFER bf1-oe-rel FOR oe-rel.
  DEF BUFFER b-oe-rell FOR oe-rell.
  DEF BUFFER b-oe-rell-exc FOR oe-rell.

  FIND bf1-oe-rel WHERE ROWID(bf1-oe-rel) EQ ipr-rowid NO-LOCK NO-ERROR.
  lv-qty = 0.
  IF NOT AVAIL bf1-oe-rel THEN
  return.
  v-current-r-no = 0.
  IF v-current-r-no EQ 0 THEN DO:
     /* This is so that the procedure can be called externally */
     FIND FIRST oe-rell 
          WHERE oe-rell.company EQ bf1-oe-rel.company 
              AND oe-rell.ord-no EQ bf1-oe-rel.ord-no
              AND oe-rell.i-no   EQ bf1-oe-rel.i-no
              AND oe-rell.link-no EQ bf1-oe-rel.r-no
              and oe-rell.rel-no  eq bf1-oe-rel.rel-no
              AND oe-rell.po-no eq bf1-oe-rel.po-no
          NO-LOCK NO-ERROR.
            
          if AVAIL oe-rell THEN
              v-current-r-no = oe-rell.r-no.

            
  END.

  FOR EACH oe-ordl WHERE oe-ordl.company = bf1-oe-rel.company
    and oe-ordl.i-no   = bf1-oe-rel.i-no 
    AND oe-ordl.ord-no = bf1-oe-rel.ord-no NO-LOCK.

    
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF NOT AVAIL oe-ord THEN
    NEXT.
    
    IF oe-ord.stat EQ "Z" OR oe-ord.stat = "C"  THEN
    NEXT.
    
    FIND FIRST bf-oe-rel 
      WHERE bf-oe-rel.company EQ oe-ordl.company
        AND bf-oe-rel.ord-no = oe-ordl.ord-no
        AND bf-oe-rel.LINE    = oe-ordl.LINE
        AND ROWID(bf-oe-rel) EQ ipr-rowid
    NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-oe-rel  THEN
    NEXT.
    
/*     IF v-current-r-no EQ 0 THEN          */
/*         v-current-r-no = bf-oe-rel.r-no. */
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company  EQ oe-ordl.company
          AND oe-boll.ord-no   EQ oe-ordl.ord-no
          AND oe-boll.i-no     EQ oe-ordl.i-no
          AND oe-boll.LINE     EQ oe-ordl.LINE 
        and oe-boll.po-no    eq bf-oe-rel.po-no 
        and oe-boll.rel-no eq bf-oe-rel.rel-no 
        AND (IF v-current-r-no GT 0 THEN oe-boll.r-no eq v-current-r-no ELSE TRUE)
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
    
    FOR EACH oe-rell NO-LOCK
      WHERE oe-rell.company  EQ bf1-oe-rel.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.LINE     EQ oe-ordl.LINE
        /* AND oe-rell.r-no     EQ bf-oe-rel.link-no */
        AND oe-rell.po-no    eq bf-oe-rel.po-no
        AND oe-rell.rel-no   EQ bf-oe-rel.rel-no
        AND (IF v-current-r-no GT 0 THEN oe-rell.r-no eq v-current-r-no ELSE TRUE)
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recalc-act-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recalc-act-qty Procedure 
PROCEDURE recalc-act-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipRelRow AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opActRelQty AS DEC NO-UNDO.
DEF VAR ldREturnQty AS DEC.

FOR EACH bf-oe-rel 
    WHERE ROWID(bf-oe-rel) EQ ipRelRow
  EXCLUSIVE-LOCK,
  FIRST oe-ordl NO-LOCK
   WHERE oe-ordl.company EQ bf-oe-rel.company
      AND oe-ordl.ord-no  EQ bf-oe-rel.ord-no
      AND oe-ordl.i-no    EQ bf-oe-rel.i-no
      AND oe-ordl.LINE    EQ bf-oe-rel.LINE:

     FIND company WHERE company.company EQ bf-oe-rel.company NO-LOCK.
     
     FIND oe-ord 
        WHERE oe-ord.company = oe-ordl.company
          AND oe-ord.ord-no = oe-ordl.ord-no
        NO-LOCK NO-ERROR.

     /* Consider whether to continue with this order */
     IF NOT AVAIL oe-ord THEN
        NEXT.
     
     IF oe-ord.stat = "C"  OR oe-ord.stat = "Z" THEN
     NEXT.


     /* Get temp-table record built */
     /* RUN build-report-file. 5/1 testing */
     
     RUN oe/rel-stat.p (ROWID(bf-oe-rel), OUTPUT lv-stat).
     
     /* do actual assignment */
     /* WFK - was not recalculating if status had been set incorrectly */
     IF  TRUE /* INDEX("LICZP ",lv-stat) GT 0 */ THEN DO:
         FIND FIRST oe-rell 
            WHERE oe-rell.company EQ bf-oe-rel.company 
              AND oe-rell.ord-no EQ bf-oe-rel.ord-no
              AND oe-rell.i-no   EQ bf-oe-rel.i-no
              AND oe-rell.link-no EQ bf-oe-rel.r-no
              and oe-rell.rel-no  eq bf-oe-rel.rel-no
              AND oe-rell.po-no eq bf-oe-rel.po-no
            NO-LOCK NO-ERROR.
            
          if AVAIL oe-rell THEN
            v-current-r-no = oe-rell.r-no.
          ELSE
            v-current-r-no = 0.
        lv-qty = 0.  
        /* Why does this need a report record to calculate? */
        RUN get-act-qty (INPUT ROWID(bf-oe-rel), OUTPUT ldREturnQty).
        IF ldReturnQty NE bf-oe-rel.qty THEN DO:

          bf-oe-rel.qty = ldReturnQty.
        END.
     END.
      
     opActRelQty = ldReturnQty.
    /* FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR. */
  
END.
FIND FIRST bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ipRelRow
  NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-get-rel-qty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-qty Procedure 
FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF BUFFER bf1-oe-rel FOR oe-rel.
  
  IF AVAIL bf-oe-rel THEN
    FIND bf1-oe-rel 
      WHERE ROWID(bf1-oe-rel) EQ ROWID(bf-oe-rel)
    NO-LOCK NO-ERROR.
  IF NOT AVAIL bf1-oe-rel THEN DO:
    FIND bf1-oe-rel WHERE RECID(bf1-oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
/*     IF AVAIL bf1-oe-rel THEN                                                    */
/*       FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR. */
  END.
/*   FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(bf-oe-rel) NO-ERROR. */
  
  RETURN IF 
    (INDEX("SIL",bf-oe-rel.stat ) GT 0
     OR bf-oe-rel.stat EQ "") THEN 0
  ELSE
/* tt-report.qty is just bf-oe-rel.qty anyway */
/*     IF AVAIL tt-report  THEN tt-report.qty */
/*   ELSE                                     */
    bf-oe-rel.qty.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-rel-stat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-rel-stat Procedure 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

