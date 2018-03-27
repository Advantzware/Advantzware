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

{custom/globdefs.i}
{oe/ordholdstat.i} 
DEF VAR h_callproc AS HANDLE.
/* for oecomm.i */
DEF NEW SHARED VAR v-upd-comm AS LOG INITIAL YES NO-UNDO.
DEF NEW SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.
{sys/inc/var.i "new shared" }
DEF BUFFER bfx-ord FOR oe-ord.
DEF VAR li-lead-days AS INT NO-UNDO.
DEF VAR ll-f-bill AS LOG NO-UNDO.
DEF VAR li-sold-no AS INT NO-UNDO.
DEF VAR ls-ship-i AS cha EXTENT 4 NO-UNDO.
DEF VAR v-slow-ord AS LOG NO-UNDO.
DEF VAR v-beeler AS LOG NO-UNDO.
DEF VAR v-ilwalker AS LOG NO-UNDO.
DEF NEW SHARED VAR v-create-job AS LOG NO-UNDO.
DEF VAR v-custype LIKE cust.type NO-UNDO.
DEF VAR v-ord-limit LIKE cust.ord-lim NO-UNDO.
DEF VAR v-crd-limit LIKE cust.cr-lim NO-UNDO.
DEF VAR v-valdcode AS cha INIT "ON,BY,MH" NO-UNDO.
DEF VAR v-valtype AS cha INIT "O,R,C" NO-UNDO.
DEF VAR v-duelist AS cha INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO.
DEF VAR v-oecount LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-full-cost AS LOG NO-UNDO.
DEF VAR oeprompt LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-quo-price LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-inactive AS LOG NO-UNDO.
DEF VAR save_id AS RECID NO-UNDO.
DEF NEW SHARED VAR fil_id AS RECID NO-UNDO.
DEF VAR v-job-no LIKE oe-ord.job-no NO-UNDO.
DEF VAR v-job-no2 LIKE oe-ord.job-no2 NO-UNDO.
DEF VAR v-exp-limit AS INT INIT 10 NO-UNDO.
DEF VAR v-n-ord LIKE oe-ctrl.n-ord NO-UNDO.
DEF VAR v-estord-id AS RECID EXTENT 10 NO-UNDO.
DEF VAR v-multord AS LOG NO-UNDO.
DEF VAR v-ship-id LIKE oe-rel.ship-id NO-UNDO.

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
DEF NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
DEF NEW SHARED VAR nufile AS LOG NO-UNDO.
DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.  /* for oe-ordl.qty and oe-ordm calc */
DEF NEW SHARED VAR v-d-rel AS INT NO-UNDO.
DEF VAR lv-new-row-id AS ROWID NO-UNDO.  /* first creation error */
DEF NEW SHARED VAR v-qty-mod AS LOG NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR ll-order-from-est AS LOG NO-UNDO.  /* is order created from estimate */
DEF VAR ll-cust-displayed AS LOG NO-UNDO.
DEF VAR ll-est-no-mod AS LOG NO-UNDO.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
DEF VAR ll-is-new-rec AS LOG NO-UNDO.
DEF VAR ll-from-tandem AS LOG NO-UNDO.
DEF VAR lv-old-cust-no LIKE oe-ord.cust-no NO-UNDO.
DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.
DEF VAR v-margin AS DEC NO-UNDO.

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xeb FOR eb.
DEF NEW SHARED BUFFER xef FOR ef.


&Scoped-define sman-fields oe-ord.sman oe-ord.s-pct oe-ord.s-comm

DEF NEW SHARED TEMP-TABLE w-ord NO-UNDO FIELD w-ord-no LIKE oe-ord.ord-no.

DEF TEMP-TABLE old-oe-ord NO-UNDO LIKE oe-ord.

DEF NEW SHARED TEMP-TABLE tt-oe-ordl NO-UNDO LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 

ASSIGN
 cocode = g_company
 locode = g_loc.

{oe/tt-item-qty-price.i}

{oe/oe-sysct1.i NEW}
        
  DO TRANSACTION:
    {sys/inc/oedate.i}
    {sys/inc/oecomb.i}
    {sys/inc/job#.i}
    {sys/inc/graphic.i}
    {sys/inc/oeestcom.i}
    {sys/inc/OEPrepTaxCode.i}
    {sys/inc/oeship.i}
  END.

DEF VAR lcReturn AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.
DEF VAR llOeShipFromLog AS LOG NO-UNDO.
DEF VAR OEJobHold-log AS LOG NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "OESHIPFROM", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).
IF llRecFound THEN
    llOeShipFromLog = LOGICAL(lcReturn) NO-ERROR.    
    

RUN sys/ref/nk1look.p (cocode, "OEJobHold", "L", NO, NO, "", "", 
                          OUTPUT lcReturn, OUTPUT llRecFound).
IF llRecFound THEN
   OEJobHold-log = LOGICAL(lcReturn) NO-ERROR.  
   
/* transaction */
{sys/inc/f16to32.i}

/* transaction */
 {sys/inc/ceprepprice.i} 

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-get-handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-handle Procedure 
FUNCTION get-handle RETURNS HANDLE
( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-sv Procedure 
FUNCTION get-sv RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-val) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-val Procedure 
FUNCTION get-val RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-entry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD set-entry Procedure 
FUNCTION set-entry RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD set-sv Procedure 
FUNCTION set-sv RETURNS CHARACTER
  ( ipv-item AS CHAR, ipv-value AS CHAR /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-add-order) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-order Procedure 
PROCEDURE add-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-add-tandem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-tandem Procedure 
PROCEDURE add-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ll-from-tandem = YES.

  RUN dispatch ("add-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-tandem-button) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-tandem-button Procedure 
PROCEDURE check-tandem-button :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-enabled AS LOG NO-UNDO.


  RUN custom/frame-en.p (get-handle("{&FRAME-NAME}"), "{&ENABLED-FIELDS}", OUTPUT op-enabled).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-use-1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use-1 Procedure 
PROCEDURE check-use-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO:
    IF get-sv("oe-ord.est-no") NE "" THEN DO:
      FIND FIRST est
           WHERE est.company EQ cocode
             AND est.est-no  EQ get-sv("oe-ord.est-no") 
           NO-LOCK NO-ERROR.

       {est/checkuse.i "no cancel"} 
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-use-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-use-2 Procedure 
PROCEDURE check-use-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO:
    &Scoped-define SECOND-EXTERNAL-TABLE itemfg
    &Scoped-define THIRD-EXTERNAL-TABLE job
    &Scoped-define FOURTH-EXTERNAL-TABLE job-hdr
    &Scoped-define FIFTH-EXTERNAL-TABLE 

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
          NO-LOCK:

      FIND FIRST itemfg
          WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no 
            NO-LOCK NO-ERROR.

      IF TRIM(oe-ordl.job-no) NE "" THEN
      FIND FIRST job
          WHERE job.company EQ oe-ordl.company
            AND job.job-no  EQ oe-ordl.job-no
            AND job.job-no2 EQ oe-ordl.job-no2
          NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-LOCK NO-ERROR.

      DO WHILE TRUE:
        {custom/checkuse.i "no cancel"}

        RELEASE itemfg.

        IF AVAIL job-hdr THEN
        FIND NEXT job-hdr
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
              AND job-hdr.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL job-hdr THEN LEAVE.
      END.
    END.

    &Scoped-define SECOND-EXTERNAL-TABLE
    &Scoped-define THIRD-EXTERNAL-TABLE
    &Scoped-define FOURTH-EXTERNAL-TABLE
    &Scoped-define FIFTH-EXTERNAL-TABLE
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkOrdNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkOrdNo Procedure 
PROCEDURE checkOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAM ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT-OUTPUT PARAM io-ord-no AS INT NO-UNDO.

  DEFINE BUFFER b-oe-ord FOR oe-ord.

  DO WHILE CAN-FIND(FIRST b-oe-ord
                    WHERE b-oe-ord.company EQ g_company
                      AND b-oe-ord.ord-no  EQ io-ord-no
                      AND ROWID(b-oe-ord)  NE ipRowID):
    io-ord-no = io-ord-no + 1.
    RUN updateOrdNo (INPUT-OUTPUT io-ord-no).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-job) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job Procedure 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-recid AS RECID NO-UNDO.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  DEF VAR v-job-job LIKE job.job NO-UNDO.
  DEF VAR v-job-no LIKE job.job-no NO-UNDO.
  DEF VAR v-job-no2 LIKE job.job-no2 NO-UNDO.
  DEF VAR li-j-no AS INT NO-UNDO.
    
  /* === from oe/oe-ord1.p  ============= */
         
  FIND LAST job WHERE job.company EQ cocode NO-LOCK NO-ERROR.
  v-job-job = IF AVAIL job THEN job.job + 1 ELSE 1.
  ASSIGN
   v-job-no  = oe-ord.job-no
   v-job-no2 = oe-ord.job-no2.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  CREATE job.
  ASSIGN job.job        = v-job-job
         job.company    = cocode
         job.loc        = locode
         job.est-no     = oe-ord.est-no
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid = RECID(job).

  FOR EACH oe-ordl WHERE oe-ordl.company EQ oe-ord.company
                     AND oe-ordl.ord-no  EQ oe-ord.ord-no exclusive:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ oe-ord.job-no
            AND job-hdr.job-no2 EQ oe-ord.job-no2
            AND job-hdr.ord-no  EQ oe-ord.ord-no
            AND job-hdr.i-no    EQ oe-ordl.i-no
          NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
         FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
                             AND itemfg.i-no    EQ oe-ordl.i-no
                             NO-LOCK NO-ERROR.   
         
         CREATE job-hdr.
         ASSIGN job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.est-no       = oe-ord.est-no
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         IF AVAIL itemfg THEN
              ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         ASSIGN job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      END.

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END.

      ASSIGN job-hdr.est-no  = oe-ord.est-no
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
             oe-ordl.est-no  = job-hdr.est-no
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no = job-hdr.j-no.

      FIND CURRENT job-hdr NO-LOCK.
  END.
  
  IF oe-ord.stat EQ "H" THEN
    RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
    
  FIND CURRENT job NO-LOCK.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-misc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-misc Procedure 
PROCEDURE create-misc :
/*------------------------------------------------------------------------------
  Purpose:    from  oe/ordlmisc.p 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF VAR li-line AS INT NO-UNDO.
  DEF VAR v-tax-rate AS DEC FORM ">,>>9.99<<<" NO-UNDO.
  DEF VAR v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
  DEF BUFFER bf-eb FOR eb .
  
  FIND bf-eb WHERE RECID(bf-eb) = ip-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-eb THEN RETURN.


  FIND FIRST cust NO-LOCK
      WHERE cust.company = g_company 
        AND cust.cust-no = oe-ord.cust-no
      NO-ERROR.
  
  FOR EACH est-prep WHERE est-prep.company = g_company
                      AND est-prep.est-no = bf-eb.est-no
                      AND est-prep.simon = "S"     NO-LOCK .
      FIND FIRST oe-ordm WHERE oe-ordm.company = g_company
                           AND oe-ordm.ord-no = oe-ord.ord-no
                           AND oe-ordm.charge = est-prep.code
                           NO-LOCK NO-ERROR.
      IF NOT AVAIL oe-ordm THEN DO:
         FIND LAST oe-ordm OF oe-ord NO-LOCK NO-ERROR.
         li-line = IF AVAIL oe-ordm THEN oe-ordm.line + 1 ELSE 1.
         FIND cust WHERE cust.company = g_company 
                     AND cust.cust-no = oe-ord.cust-no NO-ERROR.
         FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
         FIND FIRST prep WHERE prep.company = g_company 
                           AND prep.code = est-prep.code NO-LOCK NO-ERROR.
         CREATE oe-ordm.
         ASSIGN oe-ordm.company = g_company
                oe-ordm.ord-no = oe-ord.ord-no
                oe-ordm.line = li-line
                oe-ordm.charge = est-prep.code
                oe-ordm.dscr = IF est-prep.dscr <> "" THEN est-prep.dscr ELSE prep.dscr
                oe-ordm.actnum = IF AVAIL prep AND prep.actnum <> "" THEN prep.actnum ELSE ar-ctrl.sales
                oe-ordm.amt =  IF ceprepprice-chr EQ "Profit" THEN
                                  (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) * 
                                  (est-prep.amtz / 100)
                               ELSE
                                  (est-prep.cost * est-prep.qty) * (1 + (est-prep.mkup / 100)) * 
                                  (est-prep.amtz / 100)
                oe-ordm.est-no = est-prep.est-no
                oe-ordm.tax = cust.sort = "Y" AND oe-ord.tax-gr <> ""
                oe-ordm.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100))
                oe-ordm.bill  = "Y".
            
         IF PrepTax-log THEN 
            ASSIGN oe-ordm.tax = TRUE
                   oe-ordm.spare-char-1 = IF cust.spare-char-1 <> "" THEN cust.spare-char-1 ELSE oe-ord.tax-gr.
                   .  
         RUN ar/cctaxrt.p (INPUT g_company, oe-ord.tax-gr,
                            OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

         IF AVAIL cust THEN DO:
           FIND CURRENT cust.
           cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                          (IF oe-ordm.tax THEN (oe-ordm.amt * v-tax-rate / 100) ELSE 0).            
           FIND CURRENT cust NO-LOCK.
         END.
      END.

      FIND CURRENT oe-ordm NO-LOCK.
  END.
  FOR EACH ef OF bf-eb /*where ef.company = g_company and
                    ef.est-no = oe-ord.est-no */
                    NO-LOCK:
      DO i = 1 TO 5:
         IF ef.mis-simon[i] = "S" THEN DO:
            FIND LAST oe-ordm OF oe-ord NO-LOCK NO-ERROR.
            li-line = IF AVAIL oe-ordm THEN oe-ordm.line + 1 ELSE 1.
            FIND cust WHERE cust.company = g_company 
                        AND cust.cust-no = oe-ord.cust-no NO-ERROR.
            FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
            FIND FIRST prep WHERE prep.company = g_company 
                           AND prep.code = ef.mis-cost[i] NO-LOCK NO-ERROR.

            CREATE oe-ordm.
            ASSIGN oe-ordm.company = g_company
                   oe-ordm.ord-no = oe-ord.ord-no
                   oe-ordm.line = li-line
                   oe-ordm.charge = ef.mis-cost[i]
                   oe-ordm.bill  = "Y"
                   oe-ordm.tax = cust.sort = "Y" AND oe-ord.tax-gr <> ""
                   oe-ordm.amt = IF ceprepprice-chr EQ "Profit" THEN
                                    (ef.mis-labf[i] + ef.mis-matf[i] +
                                    ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) /
                                    (1 - (ef.mis-mkup[i] / 100))
                                 ELSE
                                    (ef.mis-labf[i] + ef.mis-matf[i] +
                                    ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))) *
                                    (1 + (ef.mis-mkup[i] / 100))
                   oe-ordm.est-no = oe-ord.est-no
                   oe-ordm.dscr = IF AVAIL prep THEN prep.dscr ELSE ""
                   oe-ordm.actnum = IF AVAIL prep AND prep.actnum <> "" THEN prep.actnum ELSE ar-ctrl.sales
                   oe-ordm.cost = (ef.mis-labf[i] + ef.mis-matf[i] +
                                  ((ef.mis-labm[i] + ef.mis-matm[i]) * (lv-qty / 1000))).

            RUN ar/cctaxrt.p (INPUT g_company, oe-ord.tax-gr,
                              OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

            IF AVAIL cust THEN DO:
              FIND CURRENT cust.
              cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                             (IF oe-ordm.tax THEN (oe-ordm.amt * v-tax-rate / 100) ELSE 0).
              FIND CURRENT cust NO-LOCK.
            END.

            FIND CURRENT oe-ordm NO-LOCK.            
         END.  /* simon = "S" */
      END.  /* do */              
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-release) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-release Procedure 
PROCEDURE create-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-qty-sum AS INT NO-UNDO.
  DEF VAR v-nxt-r-no AS INT INIT 1 NO-UNDO.
  DEF VAR v-lst-rel AS DATE NO-UNDO.
  DEF VAR v-pct-chg AS DEC NO-UNDO.

  DEF VAR v-num-shipto AS INT NO-UNDO.
  DEF VAR v-ship-from AS CHAR NO-UNDO.
  DEF BUFFER xoe-rel FOR oe-rel.

  
  FIND xoe-ord WHERE RECID(xoe-ord) = recid(oe-ord) NO-LOCK.  
  FIND FIRST oe-ordl WHERE oe-ordl.company = oe-ord.company AND
                           oe-ordl.ord-no = oe-ord.ord-no
                           NO-LOCK NO-ERROR.
  ASSIGN v-qty-sum  = 0.
  {oe/oe-rel.a &fil="oe-ordl"}
  
  FIND FIRST xoe-rel WHERE xoe-rel.company EQ cocode
                       AND xoe-rel.ord-no  EQ oe-ordl.ord-no
                       AND recid(xoe-rel)  NE recid(oe-rel)
                       AND xoe-rel.link-no EQ 0
                       NO-LOCK NO-ERROR.                             
  IF NOT AVAIL xoe-rel THEN DO:
     FOR EACH shipto WHERE shipto.company EQ cocode
                       AND shipto.cust-no EQ oe-ordl.cust-no:
               ASSIGN v-num-shipto = v-num-shipto + 1.
     END.
     
     IF v-num-shipto GT 1 OR llOeShipFromLog THEN
     DO:
         /* If oeshipto then use prior value when prompting */
         IF NOT (oeship-cha = "OESHIPTO" AND v-ship-id GT "") THEN DO:
             IF oe-ordl.est-no NE "" THEN
             DO:
                FIND FIRST eb WHERE eb.company = oe-ordl.company AND
                                    eb.est-no EQ oe-ordl.est-no
                                AND eb.form-no  NE 0
                                   NO-LOCK NO-ERROR.
                IF AVAIL eb THEN ASSIGN v-ship-id = eb.ship-id.
             END.
             ELSE DO:
                FIND FIRST shipto WHERE shipto.company EQ cocode
                                    AND shipto.cust-no EQ oe-ordl.cust-no
                                    AND shipto.ship-id = oe-ordl.cust-no
                                    NO-LOCK NO-ERROR.
                IF AVAIL shipto THEN ASSIGN v-ship-id = shipto.ship-id.
                ELSE DO:
                     FIND FIRST shipto WHERE shipto.company EQ cocode
                                         AND shipto.cust-no EQ oe-ordl.cust-no
                                         NO-LOCK NO-ERROR.
                     IF AVAIL shipto THEN ASSIGN v-ship-id = shipto.ship-id.   
                END.
             END.
         END.
         RUN oe/d-shipid.w (INPUT oe-ord.cust-no, INPUT-OUTPUT v-ship-id, INPUT-OUTPUT v-ship-from)  .
         ASSIGN oe-rel.ship-id = TRIM(v-ship-id).
         FIND FIRST shipto WHERE shipto.company = cocode AND
                                  shipto.cust-no = oe-ord.cust-no  AND
                                  shipto.ship-id = v-ship-id
                                  USE-INDEX ship-id NO-LOCK NO-ERROR.
         IF AVAILABLE shipto THEN DO:
            ASSIGN v-ship-id           = shipto.ship-id
                   oe-rel.ship-no      = shipto.ship-no
                                oe-rel.ship-id      = shipto.ship-id
                                oe-rel.ship-addr[1] = shipto.ship-addr[1]
                                oe-rel.ship-addr[2] = shipto.ship-addr[2]
                                oe-rel.ship-city    = shipto.ship-city
                                oe-rel.ship-state   = shipto.ship-state
                                oe-rel.ship-zip     = shipto.ship-zip
                                oe-rel.ship-i[1] = shipto.notes[1]
                                oe-rel.ship-i[2] = shipto.notes[2]
                                oe-rel.ship-i[3] = shipto.notes[3]
                               oe-rel.ship-i[4] = shipto.notes[4].
            IF v-ship-from GT "" THEN
                oe-rel.spare-char-1 = v-ship-from.
             /* if add mode then use default carrier */
          /*   if sel = 3 /* and NOT oe-rel.carrier ENTERED */ then do: */
            FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                  AND sys-ctrl.name    EQ "OECARIER"
                             NO-LOCK NO-ERROR.
            IF NOT AVAIL sys-ctrl THEN DO:
                               CREATE sys-ctrl.
                               ASSIGN
                                 sys-ctrl.company  = cocode
                                 sys-ctrl.name     = "OECARIER"
                                 sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                                 sys-ctrl.char-fld = "ShipTo".
       
                               DO WHILE TRUE:
                                   /* wfk - replace
                                 message "Default Shipping Carrier from Header or Shipto?" 
                                   update sys-ctrl.char-fld. */
                                 IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE. 
                               END.
            END.
            oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto.carrier
                               ELSE oe-ord.carrier.
         END.
         /* Run Freight calculation  */
         RUN oe/oe-frtcl.p.
      END.  /* multi ship to */
      ELSE DO:
           FIND FIRST shipto WHERE shipto.company EQ cocode AND
                                        shipto.cust-no EQ oe-ord.cust-no AND
                                        shipto.ship-id EQ v-ship-id
                                  NO-LOCK NO-ERROR.
            IF NOT AVAIL shipto THEN
                 FIND FIRST shipto WHERE shipto.company EQ cocode AND
                                          shipto.cust-no EQ oe-ord.cust-no
                                    NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN DO:
               ASSIGN oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                         oe-rel.ship-addr[1] = shipto.ship-addr[1]
                         oe-rel.ship-addr[2] = shipto.ship-addr[2]
                         oe-rel.ship-city    = shipto.ship-city
                         oe-rel.ship-state   = shipto.ship-state
                         oe-rel.ship-zip     = shipto.ship-zip
                         oe-rel.ship-i[1] = shipto.notes[1]
                         oe-rel.ship-i[2] = shipto.notes[2]
                         oe-rel.ship-i[3] = shipto.notes[3]
                         oe-rel.ship-i[4] = shipto.notes[4].
               /* if add mode then use default carrier */
               IF TRUE /* wfk - replace adm-new-record */ THEN DO:
                  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                        AND sys-ctrl.name    EQ "OECARIER"
                       NO-LOCK NO-ERROR.
                  IF NOT AVAIL sys-ctrl THEN DO:
                     CREATE sys-ctrl.
                     ASSIGN sys-ctrl.company  = cocode
                            sys-ctrl.name     = "OECARIER"
                            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                            sys-ctrl.char-fld = "ShipTo".
       
                     DO WHILE TRUE:
                         /* wfk - replace
                        message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.
                        */
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE. 
                     END.
                  END.
                  oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                     else oe-ord.carrier.
               END.
            END. /* avail shipto */
      END. /* not multi */
  END. /* if no oe-rel */
  ELSE DO:
       FIND FIRST shipto WHERE shipto.company = cocode AND
                               shipto.cust-no = oe-ord.cust-no  AND
                               shipto.ship-id = xoe-rel.ship-id
                               USE-INDEX ship-id NO-LOCK NO-ERROR.
       IF AVAILABLE shipto THEN DO:
           ASSIGN oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                       oe-rel.ship-addr[1] = shipto.ship-addr[1]
                       oe-rel.ship-addr[2] = shipto.ship-addr[2]
                       oe-rel.ship-city    = shipto.ship-city
                       oe-rel.ship-state   = shipto.ship-state
                       oe-rel.ship-zip     = shipto.ship-zip
                       oe-rel.ship-i[1] = shipto.notes[1]
                       oe-rel.ship-i[2] = shipto.notes[2]
                       oe-rel.ship-i[3] = shipto.notes[3]
                       oe-rel.ship-i[4] = shipto.notes[4].
               /* if add mode then use default carrier */
           IF TRUE /* wfk - replace logical(get-val("adm-new-record")) */ THEN DO:                
                 FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                                       AND sys-ctrl.name    EQ "OECARIER"
                 NO-LOCK NO-ERROR.
                 IF NOT AVAIL sys-ctrl THEN DO:
                    CREATE sys-ctrl.
                    ASSIGN sys-ctrl.company  = cocode
                           sys-ctrl.name     = "OECARIER"
                           sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                           sys-ctrl.char-fld = "ShipTo".
       
                    DO WHILE TRUE:
                        /* wfk - repl 
                        message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.
                        if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "Sh~ipTo" then leave. 
                        */
                    END.
                 END.
                 oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                    else oe-ord.carrier.
           END.           
       END.           
  END.
         
  fil_id = RECID(oe-ordl).
  IF AVAIL oe-ordl AND (oe-ordl.est-no NE "" AND oe-ordl.job-no EQ "") THEN DO:
      /* wfk -replace
       message " Since job number is blank, a job will not be created "
                  view-as alert-box.
                  */
  END.  
  ELSE RUN oe/ordlup.p.         /* Update Inventory and Job Costing */
           
 IF /*oe-ord.est-no eq "" and*/ oe-ordl.est-no NE "" THEN DO:
      fil_id = RECID(oe-ordl).
      RUN oe/estupl.p.
      v-qty-mod = NO.
      fil_id = RECID(oe-ordl).
      IF AVAIL oe-ordl AND (oe-ordl.est-no NE "" AND oe-ordl.job-no EQ "") THEN DO:
         MESSAGE " Since job number is blank, a purchase order will not be create~d "
                 VIEW-AS ALERT-BOX .
      END.  
      ELSE DO:
         RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
      END.   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-crt-itemfg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg Procedure 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
/* Add FG thru estimating                                                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-item LIKE itemfg.i-no.

DEF VAR v-alloc LIKE itemfg.alloc INIT YES.
DEF VAR tmpstore AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.

 {ce/msfcalc.i}
 {oe/fgfreight.i} 
 
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SETPRINT"
    NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN v-alloc = sys-ctrl.log-fld.
       
FIND FIRST cust  WHERE cust.company EQ cocode
                   AND cust.cust-no EQ xeb.cust-no
    NO-LOCK NO-ERROR.

CREATE itemfg.
ASSIGN
 itemfg.company    = cocode
 itemfg.loc        = locode
 itemfg.i-no       = v-item
 itemfg.i-code     = "C"
 itemfg.i-name     = oe-ordl.i-name
 itemfg.part-dscr1 = oe-ordl.part-dscr1
 itemfg.part-dscr2 = oe-ordl.part-dscr2
 itemfg.sell-price = oe-ordl.price
 itemfg.sell-uom   = oe-ordl.pr-uom
 itemfg.part-no    = oe-ordl.part-no
 itemfg.cust-no    = oe-ord.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.prod-uom   = IF xeb.pur-man THEN "EA" ELSE "M"
 itemfg.alloc      = v-alloc
 itemfg.stocked    = YES
 itemfg.setupDate  = TODAY.
  /* Create an itemfg-loc for the default warehouse */
  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").   
IF v-graphic-char NE "" THEN 
DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
END.

 IF AVAIL xeb THEN DO:
    ASSIGN itemfg.die-no     = xeb.die-no
           itemfg.plate-no   = xeb.plate-no
           itemfg.style      = xeb.style
           itemfg.procat     = xeb.procat
           itemfg.cad-no     = xeb.cad-no
           itemfg.upc-no     = xeb.upc-no
           itemfg.spc-no     = xeb.spc-no
           itemfg.isaset     = xeb.form-no EQ 0
           itemfg.pur-man    = xeb.pur-man
           itemfg.alloc      = xeb.set-is-assembled.

    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.
    {oe/fgfreighta.i xeb}  
    {fg/set-inks1.i itemfg xeb}
    {sys/inc/fgcascnt.i itemfg xeb}
    {sys/inc/updfgdim.i "xeb"} 

 END.
 ELSE DO:
    IF itemfg.def-loc = "" AND itemfg.def-loc-bin = "" THEN DO:
       FIND FIRST cust WHERE cust.company = cocode AND
                          cust.active  = "X"    NO-LOCK NO-ERROR.
       IF AVAIL cust THEN DO:
          FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN DO:
             ASSIGN itemfg.def-loc        = shipto.loc        
                    itemfg.def-loc-bin    = shipto.loc-bin.
          END.
       END.
    END.
 END.  


FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
itemfg.i-code = IF oe-ordl.est-no NE "" THEN "C"
                ELSE IF AVAIL oe-ctrl THEN
                        IF oe-ctrl.i-code THEN "S"
                        ELSE "C"
                ELSE "S".
/* ==== not yet 
if itemfg.i-code eq "S" then do:
  fil_id = recid(itemfg).
  run oe/fg-item.p.
  fil_id = recid(oe-ordl).
  {oe/ordlfg.i}
  display oe-ordl.i-name oe-ordl.i-no oe-ordl.price
          oe-ordl.pr-uom oe-ordl.cas-cnt oe-ordl.part-dscr2 oe-ordl.cost
          oe-ordl.part-no oe-ordl.part-dscr1 with frame oe-ordlf.
end.
*/

FIND CURRENT itemfg NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-display-cust-detail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-cust-detail Procedure 
PROCEDURE display-cust-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.
  DEF INPUT PARAMETER ip-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-autotrans AS LOG NO-UNDO.

  FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-row EXCLUSIVE-LOCK NO-ERROR.

  DEF VAR v-lead-days LIKE oe-ord.lead-days NO-UNDO.
  DEF VAR v-last-date LIKE oe-ord.last-date NO-UNDO.
  DEF VAR v-due-date  LIKE oe-ord.due-date  NO-UNDO.
  DEF VAR v-ord-date  LIKE oe-ord.ord-date  NO-UNDO.


  DO :
    {sys/inc/lastship.i} 
  END.


  FIND cust WHERE RECID(cust) = ip-recid NO-LOCK NO-ERROR.
  IF AVAIL cust THEN DO :
       ASSIGN li-lead-days = cust.ship-days   ll-f-bill = oe-ord.frt-pay = "B".

            set-sv("oe-ord.cust-no"   , cust.cust-no  ).
            set-sv("oe-ord.cust-name" , cust.name     ).
            set-sv("oe-ord.addr[1]"   , cust.addr[1]  ).
            set-sv("oe-ord.addr[2]"   , cust.addr[2]  ).
            set-sv("oe-ord.city"      , cust.city     ).
            set-sv("oe-ord.state"     , cust.state    ).
            set-sv("oe-ord.zip"       , cust.zip      ).
            set-sv("oe-ord.contact"   , cust.contact  ).
            
            set-sv("oe-ord.last-date" , STRING(DATE(get-sv("oe-ord.ord-date"))
                                               + cust.ship-days)
                                               ).
            IF NOT ip-autotrans THEN
            set-sv("oe-ord.due-date"  , get-sv("oe-ord.last-date")   ).
            set-sv("oe-ord.terms"     , cust.terms                   ).
            set-sv("oe-ord.over-pct"  , STRING(cust.over-pct)        ).
            set-sv("oe-ord.under-pct" , STRING(cust.under-pct)       ).
            set-sv("oe-ord.fob-code"  , cust.fob-code                ).
            set-sv("oe-ord.frt-pay"   , IF oe-ord.frt-pay EQ "" THEN
                                            cust.frt-pay ELSE oe-ord.frt-pay).
            set-sv("oe-ord.tax-gr"    , cust.tax-gr                           ).           
           set-sv("oe-ord.sman[1]"   , cust.sman                              ).
            set-sv("oe-ord.s-pct[1]" , "100.00"                               ).

         ASSIGN
            v-custype         = cust.type
            v-ord-limit       = cust.ord-lim
            v-crd-limit       = cust.cr-lim - (cust.acc-bal + cust.ord-bal).

      FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-row EXCLUSIVE-LOCK NO-ERROR.
      IF  lastship-cha = "Stock/Custom" AND NOT ip-autotrans THEN DO:
          /* If order has no estimate. */
          IF get-sv("oe-ord.est-no") = ""  THEN
              set-sv("oe-ord.due-date",STRING(DATE(get-sv("oe-ord.ord-date")) + lastship-int)).
          ELSE
              set-sv("oe-ord.due-date", STRING(DATE(get-sv("oe-ord.ord-date")) + INT(lastship-dec))).
      END.

    ASSIGN
     lv-old-cust-no    = get-sv("oe-ord.cust-no")
     ll-cust-displayed = YES.

    ASSIGN
     v-lead-days = li-lead-days
     v-last-date = DATE(get-sv("oe-ord.last-date"))
     v-due-date  = DATE(get-sv("oe-ord.due-date"))
     v-ord-date  = DATE(get-sv("oe-ord.ord-date")).

    {oe/lastship.i "v-" 1}
    
    ASSIGN
     li-lead-days                  = v-lead-days.
     set-sv("oe-ord.last-date", STRING(v-last-date)).
    IF NOT ip-autotrans THEN
     set-sv("oe-ord.due-date", STRING(v-due-date)).

     FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "SALESREP" NO-ERROR.
     IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
       FIND FIRST sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company EQ cocode
          AND sys-ctrl-shipto.name    EQ "SALESREP" 
          AND sys-ctrl-shipto.cust-vend-no = get-sv("oe-ord.cust-no")
        NO-ERROR.
       IF AVAIL sys-ctrl-shipto AND get-sv("oe-ord.sman[1]") NE sys-ctrl-shipto.char-fld THEN DO:
                set-sv("oe-ord.sman[2]", sys-ctrl-shipto.char-fld).
                set-sv("oe-ord.s-comm[2]", STRING(sys-ctrl-shipto.dec-fld)).
                set-sv("oe-ord.s-pct[2]", "100.00").
         FIND sman WHERE sman.company = oe-ord.company
            AND sman.sman = get-sv("oe-ord.sman[2]")
            NO-LOCK NO-ERROR.
         IF AVAIL sman THEN set-sv("oe-ord.sname[2]", sman.sname).
       END.

     END.

    FIND sman WHERE sman.company = oe-ord.company
                AND sman.sman = cust.sman
                NO-LOCK NO-ERROR.
    IF AVAIL sman THEN DO:  set-sv("oe-ord.sname[1", sman.sname)  .
                              set-sv("oe-ord.s-comm[1]", STRING(sman.scomm))  .
                              v-margin = 0.
    END.
    IF oe-ord.carrier EQ "" THEN 
        set-sv("oe-ord.carrier", cust.carrier).

    FIND FIRST terms WHERE terms.company EQ cocode
                        AND terms.t-code  EQ cust.terms
               NO-LOCK NO-ERROR.
    IF AVAIL terms THEN  
        set-sv("oe-ord.terms-d", terms.dscr) .
    ELSE 
        set-sv("oe-ord.terms-d", "").
         
    FIND FIRST soldto WHERE soldto.company EQ cocode
                        AND soldto.cust-no EQ cust.cust-no
                        AND soldto.sold-id BEGINS trim(cust.cust-no)
        USE-INDEX sold-id NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN /*oe-ord.sold-noget-sv("")      = soldto.sold-no*/             
             li-sold-no = soldto.sold-no.

             set-sv("oe-ord.sold-id"      , soldto.sold-id).
             set-sv("oe-ord.sold-name"    , soldto.sold-name).
             set-sv("oe-ord.sold-addr[1]" , soldto.sold-addr[1]).
             set-sv("oe-ord.sold-addr[2]" , soldto.sold-addr[2]).
             set-sv("oe-ord.sold-city"    , soldto.sold-city).
             set-sv("oe-ord.sold-state"   , soldto.sold-state).
             set-sv("oe-ord.sold-zip"     , soldto.sold-zip).

    FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ cust.cust-no
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
       ASSIGN 
              ls-ship-i[1] = shipto.notes[1]
              ls-ship-i[2] = shipto.notes[2]
              ls-ship-i[3] = shipto.notes[3]
              ls-ship-i[4] = shipto.notes[4].
       
    IF cust.active EQ "X" THEN set-sv("fi_type", "T").
    
    IF INDEX("HA",get-sv("oe-ord.stat")) = 0 THEN DO:
      RUN oe/creditck.p (ROWID(cust), NO).
      FIND CURRENT cust NO-LOCK NO-ERROR.
      IF AVAIL cust AND cust.cr-hold THEN 
          set-sv("oe-ord.stat","H").      
    END.   
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-display-sold-id) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-sold-id Procedure 
PROCEDURE display-sold-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li-sold-no LIKE soldto.sold-no.

    FIND FIRST soldto WHERE soldto.company = g_company AND
                            soldto.cust-no = get-sv("oe-ord.cust-no")
                        AND trim(soldto.sold-id) = trim(get-sv("oe-ord.sold-id"))
                        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN  DO:

      ASSIGN li-sold-no = soldto.sold-no.
      set-sv("oe-ord.sold-id"      , soldto.sold-id).
      set-sv("oe-ord.sold-name"    , soldto.sold-name).
      set-sv("oe-ord.sold-addr[1]" , soldto.sold-addr[1]).
      set-sv("oe-ord.sold-addr[2]" , soldto.sold-addr[2]).
      set-sv("oe-ord.sold-city"    , soldto.sold-city).
      set-sv("oe-ord.sold-state"   , soldto.sold-state).
      set-sv("oe-ord.sold-zip"     , soldto.sold-zip).
    END.
    ELSE IF get-sv("oe-ord.sold-id") <> "" THEN DO:
         /* message "Invalid Sold To. Try help. " view-as alert-box error. */
         RETURN "Invalid Sold To".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-est-from-tandem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE est-from-tandem Procedure 
PROCEDURE est-from-tandem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-new-tandem AS LOG NO-UNDO.

  DEF BUFFER est FOR est.
  DEF BUFFER eb FOR eb.

  /*
  RUN ce/new-est.p (4, OUTPUT lv-rowid). ce-ctrl locking moved to button save in est/d-select.w

  FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.

  IF AVAIL eb THEN */
  DO:
    RUN est/d-selest.w (?, NO, get-sv("oe-ord.cust-no"),
                        OUTPUT ll-new-tandem, OUTPUT lv-rowid).

    FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.

    IF ll-new-tandem THEN DO:
      set-sv("oe-ord.est-no",  eb.est-no) .
      /* wfk - replace
      APPLY "value-changed" TO oe-ord.est-no.
      */
      FIND FIRST xest OF eb NO-LOCK NO-ERROR.

      FOR EACH eb OF xest EXCLUSIVE:
        eb.cust-no = get-sv("oe-ord.cust-no").

        FOR EACH shipto
            WHERE shipto.company EQ eb.company
              AND shipto.cust-no EQ eb.cust-no
            NO-LOCK
            BREAK BY shipto.ship-no DESC:
          IF shipto.ship-id EQ eb.ship-id THEN LEAVE.
          IF LAST(shipto.ship-no) THEN eb.ship-id = shipto.ship-id.
        END.
      END.

      RELEASE xeb.

      RUN est/oeselest.p.
      
      SESSION:SET-WAIT-STATE ("general").
      RUN get-from-est.
      SESSION:SET-WAIT-STATE ("").
    END.

    ELSE DO:
      FIND FIRST est OF eb EXCLUSIVE NO-ERROR.
      IF AVAIL est THEN DELETE est.
    END.
  END.

  RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-from-est) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-from-est Procedure 
PROCEDURE get-from-est :
/*------------------------------------------------------------------------------
  Purpose:     get all info from estimate    
              /* same purpose as oe/orduest.p & oe/estup.p */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-est-no LIKE est.est-no NO-UNDO.
DEF VAR v-est-type LIKE est.est-type NO-UNDO.
DEF VAR v-factor AS DEC NO-UNDO.
DEF VAR v-run-list AS CHAR INIT
                "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p".
DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR x AS INT NO-UNDO.
DEF VAR nufile AS LOG NO-UNDO.
DEF VAR v-blk-qty AS INT NO-UNDO.
DEF VAR v-tax-rate AS DEC FORM "->>>.99" NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC FORM "->>>,99" NO-UNDO.
DEF VAR v-quo-price LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR li-line-no AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.
DEF VAR hld-id AS RECID NO-UNDO.
DEF VAR hld-stat LIKE job.stat NO-UNDO.
DEF VAR hld-nufile AS LOG NO-UNDO.
DEF VAR lv-pr-uom AS cha NO-UNDO.
DEF VAR lv-date AS DATE NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.
DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

DO:
v-est-no = get-sv("oe-ord.est-no") .
RUN util/rjust.p (INPUT-OUTPUT v-est-no,8).
set-sv("oe-ord.est-no", v-est-no).
 
FIND FIRST xest
    WHERE xest.company EQ cocode
      AND xest.est-no  EQ v-est-no 
    NO-LOCK NO-ERROR.

ASSIGN
 v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0
 ll-do-job  = job#-int EQ 0 OR
              CAN-FIND(FIRST eb WHERE eb.company EQ xest.company
                                  AND eb.est-no  EQ xest.est-no
                                  AND eb.pur-man EQ NO
                                  AND eb.form-no NE 0).

IF AVAIL xest THEN DO:
  /** CHECK for INACTIVE CUSTOMERS **/
  FOR EACH eb WHERE eb.company = cocode AND
                    eb.est-no   EQ xest.est-no
                AND eb.form-no NE 0
                AND TRIM(eb.cust-no) NE ""
           NO-LOCK BREAK BY eb.est-no BY eb.cust-no:
      
    IF FIRST-OF(eb.cust-no) THEN DO:
      /** finding the customer is done this way because the index is not
      setup efficently to find the customer regardles of active stat **/
      FIND FIRST cust {sys/ref/custW.i}
                      AND cust.cust-no EQ eb.cust-no
           USE-INDEX cust NO-LOCK NO-ERROR.
      IF NOT AVAIL cust OR cust.active EQ "I" THEN DO:
         MESSAGE              "   Inactive Customer:" cust.name SKIP
            "   Orders May not Be Processed for Inactive Customers.  "
            VIEW-AS ALERT-BOX WARNING.
         ASSIGN v-inactive = YES.
         RETURN.
      END. /* do */
    END. /* first-of(eb.cust-no) */
    IF v-est-fg1 = "HOLD" AND eb.stock-no = "" THEN DO:
        /* wfk - replace
       MESSAGE "Sorry, FG item does not exist. Order has not been approved."
           VIEW-AS ALERT-BOX ERROR.
       set-entry("OE-ORD.EST-NO").
       RETURN "NO FGITEM".
       */
    END.
    IF eb.stock-no <> "" THEN DO:
        RUN fg/GetItemfgActInact.p (INPUT g_company,
                                    INPUT eb.stock-no,
                                    OUTPUT lActive).

        IF NOT lActive THEN DO:
          MESSAGE eb.stock-no "has InActive Status. Order cannot be placed for the Inactive Item."
                  VIEW-AS ALERT-BOX ERROR. 
          RETURN "Inactive FGItem".
       END.
          
    END.
  END. /* each eb */

  ASSIGN
   j         = 1
   fil_id    = save_id        /* reset fil_id, scrambled in calc...*/
   ll-order-from-est = YES.              
   
  FIND FIRST xeb WHERE xeb.company = xest.company 
                   AND xeb.est-no EQ xest.est-no
                   AND xeb.form-no  EQ 0
       NO-LOCK NO-ERROR.     
  FOR EACH eb WHERE eb.company = xest.company 
                AND eb.est-no  EQ xest.est-no
                AND eb.form-no  NE 0
                AND eb.blank-no NE 0
                AND TRIM(eb.cust-no) NE ""
      NO-LOCK,
      FIRST cust NO-LOCK
      {sys/ref/custW.i}
        AND cust.cust-no EQ eb.cust-no
      USE-INDEX cust
      BREAK BY eb.est-no BY eb.cust-no BY eb.form-no BY eb.blank-no:
      
    IF FIRST-OF(eb.cust-no) THEN DO:
        /* if order is not created from above - only one cust-no */
          v-prod-cat = eb.procat.
      FIND xoe-ord WHERE RECID(xoe-ord) = recid(oe-ord) NO-LOCK.
      
      IF ll-do-job THEN DO:
        v-job-no = FILL(" ",6 - length(TRIM(get-sv("oe-ord.ord-no")))) + get-sv("oe-ord.ord-no").
        RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                         INPUT-OUTPUT v-job-no2,
                         INPUT v-prod-cat,
                         INPUT FILL(" ",6 - length(TRIM(get-sv("oe-ord.est-no")))) + trim(get-sv("oe-ord.est-no"))).
         
        IF v-job-no EQ "" THEN
          v-job-no = FILL(" ",6 - length(TRIM(get-sv("oe-ord.est-no")))) + trim(get-sv("oe-ord.est-no")).
      
      END.
      ELSE
        ASSIGN
         v-job-no  = ""
         v-job-no2 = 0.

      RUN display-cust-detail (RECID(cust)).

      ASSIGN  v-job-no2 = 0 v-custype         = cust.type.

          set-sv("oe-ord.sold-id",    eb.cust-no). /** DEFAULT to first SOLD to **/
             set-sv("oe-ord.sman[1]",    eb.sman).
             set-sv("oe-ord.cust-no",    eb.cust-no).
             set-sv("oe-ord.carrier",    eb.carrier).
             set-sv("oe-ord.frt-pay",    eb.chg-method).
            set-sv("oe-ord.s-pct[1]",   "100").
             set-sv("oe-ord.due-code",   "ON").
            
             set-sv("oe-ord.job-no",     v-job-no).
             set-sv("oe-ord.job-no2",    STRING(v-job-no2)).
             set-sv("oe-ord.cust-no",    cust.cust-no).
             set-sv("oe-ord.cust-name",  cust.NAME).
             set-sv("oe-ord.addr[1]",    cust.addr[1]).
             set-sv("oe-ord.addr[2]",    cust.addr[2]).
             set-sv("oe-ord.city",       cust.city).
             set-sv("oe-ord.state",      cust.state).
             set-sv("oe-ord.zip",        cust.zip).
             set-sv("oe-ord.contact",    cust.contact).
             set-sv("oe-ord.last-date",  STRING(oe-ord.ord-date + cust.ship-days)).
             set-sv("oe-ord.due-date",   get-sv("oe-ord.last-date")).
             set-sv("oe-ord.terms",      cust.terms).
             set-sv("oe-ord.over-pct",   STRING(cust.over-pct)).
             set-sv("oe-ord.under-pct",  STRING(cust.under-pct)).
             set-sv("oe-ord.fob-code",   cust.fob-code).
             set-sv("oe-ord.tax-gr",     cust.tax-gr).
            .


      IF /* wfk - replace lastship-cha = "Stock/Custom" */ TRUE THEN DO:
          /* If order has no estimate. */
          /* wfk - replace
          IF get-sv("oe-ord.est-no") = "" THEN
             set-sv("oe-ord.due-date",STRING(DATE(get-sv("oe-ord.ord-date")) + lastship-int)).
          ELSE
             set-sv("oe-ord.due-date", STRING(DATE(get-sv("oe-ord.ord-date")) + INT(lastship-dec))).
             */
      END.

      FIND FIRST sman WHERE
           sman.company EQ eb.company AND
           sman.sman EQ eb.sman
           NO-LOCK NO-ERROR.
        
      IF oeestcom-log = NO OR
         NOT(AVAIL sman AND sman.commbasis EQ "M") THEN DO:
      
         ASSIGN v-margin = 0.
         set-sv("oe-ord.s-comm[1]",STRING(eb.comm)).
       END.    
      ELSE
      DO:
         lv-qty = 0.
         FIND FIRST est-qty WHERE
              est-qty.company = xest.company AND
              est-qty.est-no = xest.est-no
              NO-LOCK NO-ERROR.

         /*best guess before actually picking qty
           order header will be updated later with current commission*/
         IF AVAIL est-qty THEN
            lv-qty = est-qty.qty[1].

         v-qty = IF NOT(v-est-type EQ 3 OR v-est-type EQ 4) THEN
                    lv-qty
                 ELSE
                    eb.bl-qty.

         IF NOT(eb.est-type EQ 4 OR eb.est-type EQ 8) THEN
            FOR EACH probe WHERE
                probe.company = eb.company AND
                probe.est-no = eb.est-no AND
                probe.probe-date NE ? AND
                probe.est-qty EQ v-qty
                NO-LOCK
                BY probe.probe-date DESC
                BY probe.probe-time DESC:
                
                LEAVE.
            END.
         ELSE
            FOR EACH probe WHERE
                probe.company = eb.company AND
                probe.est-no = eb.est-no AND
                probe.probe-date NE ?
                NO-LOCK
                BY probe.probe-date DESC
                BY probe.probe-time DESC:
                
                LEAVE.
            END.

         IF AVAIL probe THEN DO:
         
            ASSIGN v-margin = probe.market-price.
               set-sv("oe-ord.s-comm[1]", STRING(probe.comm)).
         END.      
         ELSE DO:
         
            ASSIGN v-margin = 0.
               set-sv("oe-ord.s-comm[1]", STRING(eb.comm)).
         END.      
      END.

      IF xest.ord-no NE 0 THEN set-sv("fi_prev_order", STRING(xest.ord-no)).
      
      IF FIRST(eb.cust-no) THEN 
         fil_id = RECID(xoe-ord).
          
      

      FIND FIRST terms WHERE terms.company EQ cocode
                        AND terms.t-code  EQ cust.terms
               NO-LOCK NO-ERROR.
      IF AVAIL terms THEN  set-sv("oe-ord.terms-d", terms.dscr).
      ELSE set-sv("oe-ord.terms-d", "").

      IF cust.active EQ "X" THEN set-sv("fi_type", "T").
 
      v-factor = IF xest.est-type GE 1 AND xest.est-type LE 4 THEN  /* wfk - replace lastship-dec */ 0
                 ELSE 1. 
      IF /* wfk - replace lastship-cha eq "Fibre" */ TRUE   THEN DO:
         set-sv("oe-ord.last-date", STRING(TODAY + (cust.ship-days * v-factor))).
         set-sv("oe-ord.due-date", STRING(TODAY + (/* wfk - replace lastship-int */ 1 * v-factor))).
      END.
      


      IF get-sv("oe-ord.carrier") EQ "" THEN set-sv("oe-ord.carrier", cust.carrier).
    END. /* first-of(eb.cust-no) */

    LEAVE. /** 2pc box & Set headers **/
  END. /* each eb */
END. /* avail xest */
END. /*  */

ASSIGN
 ll-est-no-mod     = NO
 lv-old-cust-no    = get-sv("oe-ord.cust-no")
 ll-cust-displayed = YES.

RUN release-shared-buffers.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hold-approve) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-approve Procedure 
PROCEDURE hold-approve :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-uom LIKE itemfg.prod-uom NO-UNDO.
  DEF VAR lv-job-recid AS RECID NO-UNDO.
  DEF VAR choice AS LOG NO-UNDO.
  DEF VAR hld-id AS RECID NO-UNDO.
  DEF VAR hld-stat LIKE job.stat NO-UNDO.
  DEF VAR hld-nufile AS LOG NO-UNDO.
  DEF VAR v-run-schedule AS LOG NO-UNDO.

  DEF BUFFER b-oe-ord FOR oe-ord.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-cust FOR cust.

  RELEASE cust.

  IF AVAIL oe-ord THEN
  FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ROWID(oe-ord)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF AVAIL b-oe-ord /*AND CAN-DO("A,H,W",oe-ord.stat)*/ THEN
  FIND FIRST cust
      WHERE cust.company EQ cocode
        AND cust.cust-no EQ oe-ord.cust-no
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF AVAIL cust THEN DO:
    IF oe-ord.stat EQ "W" THEN DO:

       SESSION:SET-WAIT-STATE("General").

       ASSIGN
          oe-ord.user-id   = USERID("nosweat")
          oe-ord.t-freight = 0.
      
       IF oe-ord.type EQ "" THEN oe-ord.type = "O".
      
       IF oe-ord.sman[1] EQ "" THEN
          ASSIGN
             oe-ord.sman    = ""
             oe-ord.sman[1] = cust.sman.
      
       IF oe-ord.sman[1] NE "" AND oe-ord.s-pct[1] EQ 0 THEN
          oe-ord.s-pct[1] = 100.00.
      
       DO i = 1 TO EXTENT(oe-ord.sman):
          IF oe-ord.s-comm[i] GE 100 THEN
             ASSIGN
                oe-ord.s-comm = 0
                v-margin = 0.
      
          FIND FIRST sman
              WHERE sman.company EQ oe-ord.company
                AND sman.sman    EQ oe-ord.sman[i]
              NO-LOCK NO-ERROR.
          IF AVAIL sman THEN DO:
             oe-ord.sname[1] = sman.sname.
         
             IF oe-ord.s-comm[i] LE 0 THEN
             DO:
                oe-ord.s-comm[i] = sman.scomm.
                IF i = 1 THEN
                   v-margin = 0.
             END.
          END.
       END.
      
       FOR EACH b-oe-ordl NO-LOCK
           WHERE b-oe-ordl.company EQ oe-ord.company
             AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
             AND b-oe-ordl.job-no  EQ ""
             AND b-oe-ordl.i-no    NE "",
           FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ b-oe-ordl.company
             AND itemfg.i-no    EQ b-oe-ordl.i-no:
      
         FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl)
             EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF NOT AVAIL oe-ordl THEN NEXT.
      
         ASSIGN
          oe-ordl.req-code  = oe-ord.due-code
          oe-ordl.req-date  = oe-ord.due-date
          oe-ordl.prom-code = oe-ord.due-code
          oe-ordl.prom-date = oe-ord.due-date.
      
         DO i = 1 TO MIN(EXTENT(oe-ordl.s-man),EXTENT(oe-ord.sman)):
           ASSIGN
            oe-ordl.s-man[i]  = oe-ord.sman[i]
            oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
            oe-ordl.s-comm[i] = oe-ord.s-comm[i].

           IF i = 1 THEN
              ASSIGN
                 oe-ordl.q-qty = oe-ord.t-fuel
                 v-margin = oe-ord.t-fuel.
         END.
      
         FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company   EQ oe-ordl.company
               AND po-ordl.i-no      EQ oe-ordl.i-no
               AND po-ordl.po-no     EQ oe-ordl.po-no-po
               AND po-ordl.item-type EQ NO
             USE-INDEX item-ordno NO-ERROR.
         IF AVAIL po-ordl THEN
           ASSIGN
            lv-uom       = po-ordl.cons-uom
            oe-ordl.cost = po-ordl.cons-cost.
         ELSE
           ASSIGN
            lv-uom       = itemfg.prod-uom
            oe-ordl.cost = itemfg.total-std-cost.
       
         IF lv-uom NE "M" THEN
           RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                                  oe-ordl.cost, OUTPUT oe-ordl.cost).
      
         RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
         oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight.
       END.
      
       RUN oe/ordfrate.p (ROWID(oe-ord)).
      
       FIND xoe-ord WHERE ROWID(xoe-ord) EQ ROWID(oe-ord) NO-LOCK NO-ERROR.
       IF AVAIL xoe-ord THEN RUN oe/oe-comm.p.
      
       RUN oe/calcordt.p (ROWID(oe-ord)).

       RUN oe/creditck.p (ROWID(cust), YES).

       FIND FIRST b-cust WHERE
            b-cust.company EQ cust.company AND
            b-cust.cust-no EQ cust.cust-no
            NO-LOCK.

       IF b-cust.cr-hold THEN DO:
       
          ASSIGN oe-ord.stat = "H".
          set-sv("oe-ord.stat", "H").
          IF OEJobHold-log THEN 
          RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
          
          
       END.     
       ELSE DO:       
          ASSIGN oe-ord.stat = "A".
          set-sv("oe-ord.stat", "A").
          IF OEJobHold-log THEN 
            RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Release").
          
       END.      
       oe-ord.approved-date = TODAY. 
       /*create job*/
       IF v-create-job AND oe-ord.job-no NE "" THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ oe-ord.company
                AND job.job-no  EQ oe-ord.job-no
                AND job.job-no2 EQ oe-ord.job-no2
              NO-ERROR.
         
          IF AVAIL job AND TRIM(job.est-no) NE TRIM(oe-ord.est-no) THEN
            IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          AND job-hdr.ord-no  NE oe-ord.ord-no) OR
               CAN-FIND(FIRST b-oe-ord
                        WHERE b-oe-ord.company EQ job.company
                          AND b-oe-ord.job-no  EQ job.job-no
                          AND b-oe-ord.job-no2 EQ job.job-no2
                          AND b-oe-ord.est-no  EQ job.est-no)   OR
               CAN-FIND(FIRST b-oe-ordl
                        WHERE b-oe-ordl.company EQ job.company
                          AND b-oe-ordl.job-no  EQ job.job-no
                          AND b-oe-ordl.job-no2 EQ job.job-no2
                          AND b-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.
            ELSE
            DO:
              FIND CURRENT job NO-ERROR.
              IF AVAIL job THEN DELETE job.
            END.
         
          IF NOT AVAIL job THEN DO:
            RUN create-job (OUTPUT lv-job-recid).
            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
          END.                 
         
          v-qty-mod = YES.

          IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
            /*IF NOT v-qty-mod THEN
               RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).*/
         
            IF v-qty-mod OR job.stat EQ "P" THEN DO:
              RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
              IF NOT choice THEN DO:
                ASSIGN hld-id     = fil_id
                       hld-nufile = nufile 
                       hld-stat   = job.stat
                       nufile     = YES.
         
                RUN jc/jc-calc.p(RECID(job), NO).
                ASSIGN fil_id   = hld-id
                       nufile   = hld-nufile.
               
                IF hld-stat NE "P" THEN DO:
                  FIND CURRENT job EXCLUSIVE.
                  job.stat = hld-stat.
                  FIND CURRENT job NO-LOCK.
                END.
              END.
            END.
          END.
                
          FIND FIRST sys-ctrl WHERE
               sys-ctrl.company EQ cocode AND
               sys-ctrl.name    EQ "SCHEDULE"
               NO-LOCK NO-ERROR.

          v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                           ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                           ELSE NO.

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ oe-ord.ord-no
                AND oe-ordl.is-a-component EQ NO
         
              BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:
         
            IF LAST-OF(oe-ordl.job-no2) THEN DO:
              ASSIGN
               hld-id     = fil_id
               hld-nufile = nufile
               fil_id     = RECID(oe-ordl).
             
              RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.
              /* check oe-ordl.due-date and calc promised date and job's start-date */

              IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.
              
              ASSIGN
               fil_id = hld-id
               nufile = hld-nufile.
            END.
          END.
       END.  /* v-create-job */

       SESSION:SET-WAIT-STATE("").
    END.

    ELSE DO: /*not web order*/
       oe-ord.stat = IF oe-ord.stat NE "H" THEN "H" ELSE "A".
       IF OEJobHold-log THEN 
         RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT (IF oe-ord.stat EQ "H" THEN "Hold" ELSE "release")).       
    END.

    IF oe-ord.stat EQ "A" THEN oe-ord.posted = NO.

    FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    FIND CURRENT b-oe-ord NO-LOCK NO-ERROR.
/* 05060903 */      
/*     /* update credit hold field in cust file                        */
/*      gdm - TAKEN OUT FOR THE ABOVE CREDIT CHECK DOES THIS ALREADY*/ */
    cust.cr-hold = oe-ord.stat EQ "H" OR
                   CAN-FIND(FIRST b-oe-ord
                            WHERE b-oe-ord.company EQ oe-ord.company
                              AND b-oe-ord.cust-no EQ oe-ord.cust-no
                              AND b-oe-ord.stat    EQ "H").
    
    FIND CURRENT cust NO-LOCK NO-ERROR.
    

    /* wfk - replace  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (ROWID(oe-ord)).
      */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lib-create-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lib-create-record Procedure 
PROCEDURE lib-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-company AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-loc     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-type    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-orig-ord AS ROWID NO-UNDO.
  DEF OUTPUT PARAMETER op-ord-row AS ROWID NO-UNDO.

  cocode = ip-company.
  g_company = ip-company.
  locode = ip-loc.
  g_loc = ip-loc.
  DEF BUFFER bf-orig-ord FOR oe-ord.
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  
  DEF VAR li-next-ordno AS INT NO-UNDO.
  FIND bf-orig-ord WHERE ROWID(bf-orig-ord) EQ ip-orig-ord
                   NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-orig-ord THEN
      RETURN ERROR.
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN nextOrdNo (OUTPUT li-next-ordno).
  IF li-next-ordno EQ ? THEN RETURN ERROR.
                                 
  /* Dispatch standard ADM method.                             */
 /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) . */
  CREATE oe-ord.
  op-ord-row = ROWID(oe-ord).
  RUN set-order IN h_callproc (INPUT ROWID(oe-ord)).
  /* Code placed here will execute AFTER standard behavior.    */
  lv-new-row-id = ROWID(oe-ord).

  RUN checkOrdNo (ROWID(oe-ord),INPUT-OUTPUT li-next-ordno).

  ASSIGN oe-ord.company = g_company
         oe-ord.loc = g_loc
         oe-ord.ord-date = TODAY
         oe-ord.ord-no = li-next-ordno
         oe-ord.user-id = USERID("nosweat")
         oe-ord.type = ip-type
         oe-ord.cust-no = bf-orig-ord.cust-no
         oe-ord.due-date = bf-orig-ord.due-date
         oe-ord.due-code = "ON".



        set-sv("fi_type", oe-ord.TYPE).
        set-sv("fi_prev_order", IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                                     ELSE STRING(oe-ord.pord-no)).


  FOR EACH b-oe-ordl
      WHERE b-oe-ordl.company EQ oe-ord.company
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no:
    DELETE b-oe-ordl.
  END.

  CREATE b-oe-ordl.
  ASSIGN
   b-oe-ordl.company = oe-ord.company
   b-oe-ordl.ord-no  = oe-ord.ord-no
   b-oe-ordl.line    = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lib-post-assign-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lib-post-assign-record Procedure 
PROCEDURE lib-post-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-autotrans AS LOG NO-UNDO.
  FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-row EXCLUSIVE-LOCK NO-ERROR.

  DEF VAR lv-date LIKE oe-ord.due-date NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR calcDueDate AS DATE NO-UNDO.
  DEF VAR calcStartDate AS DATE NO-UNDO.
  DEF VAR li-tries AS INT NO-UNDO.

  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER due-job-hdr FOR job-hdr.
  
  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST terms WHERE terms.t-code = oe-ord.terms NO-LOCK NO-ERROR.
  IF AVAIL terms THEN oe-ord.terms-d = terms.dscr.
/*   06211305 - Took out update of ord-no from db trigger, so this is not needed */
/*   IF oe-ord.ord-no NE lv-ord-no THEN DO:                                           */
/*     FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode EXCLUSIVE NO-WAIT NO-ERROR. */
/*     IF AVAIL oe-ctrl                  AND                                          */
/*        lv-ord-no + 1 EQ oe-ctrl.n-ord THEN oe-ctrl.n-ord = oe-ctrl.n-ord - 1.      */
/*     FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.                                         */
/*                                                                                    */
/*     FOR EACH oe-ordl                                                               */
/*         WHERE oe-ordl.company EQ oe-ord.company                                    */
/*           AND oe-ordl.ord-no  EQ lv-ord-no:                                        */
/*       oe-ordl.ord-no = oe-ord.ord-no.                                              */
/*     END.                                                                           */
/*   END.                                                                             */

  IF oe-ord.job-no NE '' THEN DO:
    FIND FIRST job EXCLUSIVE-LOCK WHERE job.company EQ oe-ord.company
                                    AND job.job-no EQ oe-ord.job-no
                                    AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.
    IF AVAILABLE job THEN DO:
      IF dueDateChanged THEN DO:
        job.due-date = oe-ord.due-date.
        FOR EACH due-job-hdr EXCLUSIVE-LOCK
            WHERE due-job-hdr.company EQ job.company
              AND due-job-hdr.job     EQ job.job
              AND due-job-hdr.job-no  EQ job.job-no
              AND due-job-hdr.job-no2 EQ job.job-no2:
          due-job-hdr.due-date = oe-ord.due-date.
        END. /* each due-job-hdr */
      END. /* if duedatechanged */
      FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ oe-ord.company
                                    AND sys-ctrl.name EQ 'SCHEDULE' NO-ERROR.
      IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
        IF prodDateChanged THEN
        job.start-date = IF sys-ctrl.char-fld NE 'NoDate' THEN oe-ord.prod-date
                         ELSE ?.
        IF dueDateChanged AND sys-ctrl.char-fld EQ 'PlanDate' AND NOT ip-autotrans THEN DO:
          IF NOT VALID-HANDLE(scheduleHndl) THEN
          RUN custom/schedule.p PERSISTENT SET scheduleHndl.
          RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).
          IF calcDueDate NE oe-ord.due-date THEN
          MESSAGE 'Machine Capacity calulated Scheduled Completion date of'
            calcDueDate SKIP 'Update Due Date and Promise Date on Order?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE updateDueDate AS LOGICAL.
          IF updateDueDate THEN
          ASSIGN
            job.due-date = calcDueDate
            oe-ord.due-date = calcDueDate.
        END. /* if duedatechanged */
        job.start-date = calcStartDate.
        FIND CURRENT job NO-LOCK.
      END. /* avail sys-ctrl */
    END. /* avail job */
  END. /* if job-no ne '' */
  /* wfk - replace
  ASSIGN
    prodDateChanged = NO
    dueDateChanged = NO
    oe-ord.type = fi_type
    oe-ord.po-no2 = fi_prev_order
    oe-ord.t-fuel = v-margin.
                  */
  IF oe-ord.pord-no EQ 0 THEN
    oe-ord.pord-no = INT(get-val("fi_prev_order")) NO-ERROR.


  FOR EACH oe-ordl OF oe-ord:
    IF ll-new-po THEN DO:
      oe-ordl.po-no = oe-ord.po-no.
      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

        li-tries = 0.
        IF INDEX("SLIA",lv-stat) GT 0 THEN DO WHILE TRUE:
          li-tries = li-tries + 1.
          IF li-tries GE 1000 THEN LEAVE.

          FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel)
              EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL b-oe-rel THEN DO:
            b-oe-rel.po-no = oe-ordl.po-no.
            LEAVE.
          END.
        END.
      END.
    END.

    IF oe-ordl.est-no NE "" THEN DO:
      FIND eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND ((eb.est-type NE 2 AND eb.est-type NE 6) OR
                 ((eb.est-type EQ 2 OR eb.est-type EQ 6) AND
                  eb.form-no EQ 0))
          EXCLUSIVE NO-ERROR.
      IF AVAIL eb THEN eb.stock-no = oe-ordl.i-no.
    END.
  END.

  IF oe-ord.due-date NE lv-date THEN
  FOR EACH oe-ordl OF oe-ord BREAK BY oe-ordl.line:
    IF NOT ll-new-due THEN
      ll-new-due = FIRST(oe-ordl.line) AND LAST(oe-ordl.line).

    IF NOT ll-new-due THEN
      MESSAGE "Update all line items with this Due Date?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-new-due.

    IF ll-new-due THEN DO:
      oe-ordl.req-date = oe-ord.due-date.

      IF oe-ordl.req-date GT oe-ordl.prom-date THEN
        oe-ordl.prom-date = oe-ordl.req-date.
    END.
    ELSE LEAVE.
  END. /* each oe-ordl */
  FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
  
  RELEASE eb.
  RELEASE oe-rel.
  RELEASE bfx-ord.
  RELEASE oe-ordl.

  IF oe-ord.frt-pay = "B" THEN oe-ord.f-bill = YES.
  ELSE oe-ord.f-bill = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lib-pre-assign-record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lib-pre-assign-record Procedure 
PROCEDURE lib-pre-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-ord-row AS ROWID NO-UNDO.
  FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-row EXCLUSIVE-LOCK NO-ERROR.

  DEF VAR lv-date LIKE oe-ord.due-date NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR calcDueDate AS DATE NO-UNDO.
  DEF VAR calcStartDate AS DATE NO-UNDO.
  DEF VAR li-tries AS INT NO-UNDO.

  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER due-job-hdr FOR job-hdr.


  SESSION:SET-WAIT-STATE ("general").

  /* Code placed here will execute PRIOR to standard behavior. */
  /* wfk - replace this message in main program
  DO WITH FRAME {&FRAME-NAME}:
    ll-new-po = NO.
    
    IF NOT logical(get-val("adm-new-record")) AND oe-ord.po-no NE get-sv("oe-ord.po-no") THEN
      MESSAGE "Update all order lines/releases with this "  +
              TRIM(oe-ord.po-no:LABEL) + "?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-new-po.
  END.
  */
  ASSIGN
   lv-date   = oe-ord.due-date
   lv-ord-no = oe-ord.ord-no.

  /* Dispatch standard ADM method.                             */
  /* (in .w) RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) . */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-cust-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no Procedure 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAMETER ip-autotrans AS LOG NO-UNDO.
  FIND oe-ord WHERE ROWID(oe-ord) EQ ip-ord-row EXCLUSIVE-LOCK NO-ERROR.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR cs AS CHAR.
  cs = get-sv("oe-ord.cust-no").



    FIND FIRST cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ get-sv("oe-ord.cust-no")
          AND INDEX("AXSE",cust.active) GT 0
        NO-LOCK NO-ERROR.

    IF AVAIL cust THEN RUN display-cust-detail (RECID(cust), ip-ord-row, ip-autotrans).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-sman) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-sman Procedure 
PROCEDURE new-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
  DEF INPUT PARAM ip-ord-row AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-sman LIKE sman.sman NO-UNDO.

  DO:
    lv-sman = IF ip-int EQ 3 THEN 
                 get-sv("oe-ord.sman[3]")
              ELSE
              IF ip-int EQ 2 THEN get-sv("oe-ord.sman[2]")
                             ELSE get-sv("oe-ord.sman[1]").

    IF lv-sman NE "" THEN DO:
      FIND FIRST sman
          WHERE sman.company EQ cocode
            AND sman.sman    EQ lv-sman
          NO-LOCK NO-ERROR.
      IF AVAIL sman THEN DO:
        IF ip-int EQ 3 THEN DO:
          set-sv("oe-ord.sname[3]", sman.sname).
          IF DEC(get-sv("oe-ord.s-pct[3]")) EQ 0 THEN 
              set-sv("oe-ord.s-pct[3]", "100").
          IF DEC(get-sv("oe-ord.s-comm[3]")) EQ 0 THEN 
             set-sv("oe-ord.s-comm[3]", STRING(sman.scomm)).
        END.
        ELSE
        IF ip-int EQ 2 THEN DO:
          set-sv("oe-ord.sname[2]", sman.sname).
          IF DEC(get-sv("oe-ord.s-pct[2]")) EQ 0 THEN set-sv("oe-ord.s-pct[2]", "100").
          IF DEC(get-sv("oe-ord.s-comm[2]")) EQ 0 THEN set-sv("oe-ord.s-comm[2]", STRING(sman.scomm)).
        END.
        ELSE DO:
          set-sv("oe-ord.sname[1]", sman.sname).
          IF DEC(get-sv("oe-ord.s-pct[1]")) EQ 0 THEN set-sv("oe-ord.s-pct[1]", "100").
          IF DEC(get-sv("oe-ord.s-comm[1]")) EQ 0 THEN DO:
          
             ASSIGN v-margin = 0.
             set-sv("oe-ord.s-comm[1]", STRING(sman.scomm)).
          END.     
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new-type) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type Procedure 
PROCEDURE new-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF INPUT PARAMETER ip-ord-row AS ROWID NO-UNDO.

    set-sv("fi_type", CAPS(get-sv("fi_type"))).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-nextOrdNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextOrdNo Procedure 
PROCEDURE nextOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-ord-no AS INTEGER NO-UNDO.
  DEF VAR iNextOrd AS INT NO-UNDO.


  op-ord-no = ?.
  RUN updateOrdNo (INPUT-OUTPUT op-ord-no).

  IF op-ord-no EQ ? THEN
    MESSAGE "Unable to obtain next avail order number..."
        VIEW-AS ALERT-BOX ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-order-from-est-lib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE order-from-est-lib Procedure 
PROCEDURE order-from-est-lib :
/*------------------------------------------------------------------------------
  Purpose:     /* same purpose as oe/orduest.p & oe/estup.p */
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-new-ord AS LOG NO-UNDO.
DEF VAR op-apply-entry-to-est AS LOG NO-UNDO.
DEF VAR v-ord-rec AS ROWID NO-UNDO.
DEF VAR cReturnVal AS CHAR NO-UNDO.
IF AVAIL oe-ord THEN
    v-ord-rec = ROWID(oe-ord).
 /* {oe/ordfrest.i} */
nufile = YES.
/* wfk - replace
RUN oe/ordfrest.p 
(INPUT THIS-PROCEDURE,
 INPUT        v-ord-rec,
 INPUT-OUTPUT ip-new-ord,
 INPUT-OUTPUT v-qty-mod,
 INPUT-OUTPUT v-inactive,
 INPUT-OUTPUT v-est-fg1,
 INPUT-OUTPUT lv-qty,
 INPUT-OUTPUT v-d-rel,
 INPUT-OUTPUT v-quo-price-log,
 INPUT-OUTPUT save_id,
 INPUT-OUTPUT v-job-no,
 INPUT-OUTPUT v-job-no2,
 INPUT-OUTPUT fil_id,
 INPUT-OUTPUT v-exp-limit,
 INPUT-OUTPUT v-n-ord,
 INPUT-OUTPUT nufile,
 INPUT-OUTPUT v-estord-id  /* EXTENT 10 */,
 INPUT-OUTPUT v-multord,
 INPUT-OUTPUT v-margin,
 INPUT-OUTPUT v-custype,
 INPUT-OUTPUT ld-lastship-dec,
 INPUT-OUTPUT "" /* wfk - replacelastship-cha */,
 INPUT-OUTPUT 0 /*  wfk - replace lastship-int */,
 INPUT-OUTPUT v-foamdate-log,
 INPUT-OUTPUT v-foamdate-int,
 INPUT-OUTPUT v-est-fg,
 INPUT-OUTPUT v-full-cost,
 INPUT-OUTPUT oeestcom-log,
 INPUT-OUTPUT v-oecomb-int,
 INPUT-OUTPUT ll-from-tandem,
 INPUT-OUTPUT logical(get-val("adm-new-record")),
 INPUT-OUTPUT ll-is-new-rec,
 INPUT-OUTPUT v-create-job,
 OUTPUT  op-apply-entry-to-est).
*/
 cReturnVal = RETURN-VALUE. 
 IF cReturnVal GT "" THEN DO:
     CASE cReturnVal:
         WHEN "Inactive Cust" THEN
          MESSAGE              "   Inactive Customer" SKIP
             "   Orders May not Be Processed for Inactive Customers.  "
             VIEW-AS ALERT-BOX WARNING.
         WHEN "NO FGITEM" THEN DO:
              MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                VIEW-AS ALERT-BOX ERROR.
            set-entry("oe-ord.est-no") .
         END.
         WHEN "No Next Order" THEN
                   MESSAGE " Unable to Obtain next available Order Number. " 
                     VIEW-AS ALERT-BOX ERROR.
         WHEN  "No Control" THEN
                    MESSAGE "Company " cocode " has no Order-Control record." 
                            VIEW-AS ALERT-BOX ERROR.
         WHEN "CANCEL" THEN
                   RETURN NO-APPLY.
     END CASE.
 END.

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  RUN release-shared-buffers.

  RUN dispatch ('open-query').
  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-status) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-status Procedure 
PROCEDURE process-status :
/*------------------------------------------------------------------------------
  Purpose:     Set order status and enable status type.
  Parameters:  status
  Notes:       Status button will send 'set' parameter.
               local-enable-fields will send blank parameter.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcStatus AS CHAR NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-release-shared-buffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-shared-buffers Procedure 
PROCEDURE release-shared-buffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RELEASE xoe-ord.
  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-hcaller) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-hcaller Procedure 
PROCEDURE set-hcaller :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER hCaller AS HANDLE.
h_callproc = hCaller.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-ord-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-ord-no Procedure 
PROCEDURE update-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO :
    READKEY PAUSE 0.
    IF LOGICAL(get-val("adm-new-record")) /* wfk - replace AND
       oe-ord.ord-no:SENSITIVE EQ NO */ THEN DO:
      /* wfk - replace oe-ord.ord-no:SENSITIVE = YES.*/
      set-entry("oe-ord.ord-no"). 
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-update-start-date) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-start-date Procedure 
PROCEDURE update-start-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-update-job-stdate AS LOG  NO-UNDO.
 DEF VAR lv-prom-date AS DATE NO-UNDO.
 DEFINE VARIABLE v-run-schedule AS LOGICAL NO-UNDO.

 IF oe-ordl.job-no = "" THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

  FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
  END.

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT NO-UNDO.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          ASSIGN
             lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                           ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
             lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                             TRUNCATE(bf-mch.run-hr,0) * 3600 +
                           ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
             lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
  END.
  
  ASSIGN
     lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
                 ELSE TRUNCATE(lv-job-time / 3600,0)
     lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day. /*- 1. */

  /*  get from mach-calendar 
  lv-chk-date = lv-start-date.
  li-num-of-wkend = 0.
  DO i = 1 TO lv-first-due-date - lv-start-date:
     IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 THEN li-num-of-wkend = li-num-of-wkend + 1.
     lv-chk-date = lv-chk-date + 1.
  END.
  lv-start-date = lv-start-date - li-num-of-wkend.
  */
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY  /* ip-type = "Update-2" is from v-ord.w*/
  THEN DO:
     lv-update-job-stdate = NO.
     /*MESSAGE "JOB CANNOT BE COMPLETED BEFORE REQUESTED DUE DATE DUE TO TOTAL MACHINE HOURS."
         SKIP
         "PROMISED DATE WILL BE   " lv-prom-date SKIP
         "UPDATE JOB's START DATE & DUE DATE?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
    */
     MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
     /*IF lv-update-job-stdate THEN .
     ELSE DO:
         bx-ordl.prom-date = lv-prom-date.           
         return.
     END. */
     lv-start-date = TODAY.
  END.
  
  v-run-schedule = NOT CAN-FIND(FIRST sys-ctrl
                                WHERE sys-ctrl.company EQ oe-ord.company
                                  AND sys-ctrl.name EQ 'SCHEDULE'
                                  AND sys-ctrl.char-fld EQ 'NoDate'
                                  AND sys-ctrl.log-fld EQ YES).
  IF v-run-schedule THEN DO: /* run if above does not exist */
  
  /* === reset start-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0.
  
  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no
                    AND bf-hdr.job-no2 = oe-ordl.job-no2,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                    AND bf-mch.job-no = bf-hdr.job-no
                    AND bf-mch.job-no2 = bf-hdr.job-no2
                    AND NOT bf-mch.anchored
               BREAK BY bf-mch.frm BY bf-mch.blank-no BY bf-mch.pass BY bf-mch.m-code:

          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                            AND mach-calendar.m-code = bf-mch.m-code
                            AND mach-calendar.m-date = lv-start-date
                            NO-LOCK NO-ERROR.
          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                      ELSE 28800. /* 8 HRs*/
          IF lv-m-time LT 0 THEN lv-m-time = 28800.
          lv-maccum-time = lv-maccum-time + lv-m-time.
          IF FIRST(bf-mch.frm) THEN DO:
             FIND FIRST bf-job OF bf-hdr.
             ASSIGN
                bf-job.start-date = lv-start-date
                lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
          END.
          IF FIRST-OF(bf-mch.frm) THEN
                bf-hdr.start-date = job.start-date.
      
          ASSIGN
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      TRUNCATE(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
          bf-mch.seq-no = 0                 
          bf-mch.start-time-su = lv-wrk-st-time
          bf-mch.start-time = lv-wrk-st-time + lv-mr-time
          bf-mch.start-date-su = lv-start-date
          lv-start-date-fr = lv-start-date
          lv-job-time = lv-job-time + lv-mr-time
          lv-start-date = lv-start-date + 
                          IF lv-mr-time > lv-m-time AND
                             lv-mr-time MOD lv-m-time > 0 THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                          ELSE IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.
          IF lv-m-time <> lv-maccum-time THEN DO:
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
          END.
          ASSIGN
          lv-start-date-fr = lv-start-date
          bf-mch.end-date-su = lv-start-date
          bf-mch.start-date = lv-start-date
          lv-job-time = lv-job-time + lv-run-time
          lv-start-date = lv-start-date + 
                          IF lv-run-time > lv-m-time AND
                             lv-run-time MOD lv-m-time > 0 THEN TRUNCATE(lv-run-time / lv-m-time,0) 
                          ELSE IF lv-run-time > lv-m-time THEN TRUNCATE(lv-run-time / lv-m-time,0) - 1
                          ELSE 0
          lv-start-date-fr = lv-start-date.

          IF lv-m-time <> lv-maccum-time THEN
             lv-start-date = lv-start-date + 
                          IF lv-job-time > lv-maccum-time AND
                             lv-job-time MOD lv-maccum-time > 0 THEN TRUNCATE(lv-job-time / lv-maccum-time,0) 
                          ELSE IF lv-job-time > lv-maccum-time THEN TRUNCATE(lv-job-time / lv-maccum-time,0) - 1
                          ELSE 0.
          
          ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time
                 bf-mch.end-time-su = bf-mch.start-time-su + lv-mr-time
                 bf-mch.end-date = lv-start-date           
                 lv-wrk-st-time = lv-wrk-st-time + lv-mr-time + lv-run-time.
  END.
  END. /* if v-run-schedule*/
  
  ASSIGN
     bx-ordl.prom-date = lv-prom-date
     bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateOrdNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateOrdNo Procedure 
PROCEDURE updateOrdNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR iNextOrd AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

/*  06211305 - Replace with sequence */
/*   RELEASE oe-ctrl.                                                                         */
/*                                                                                            */
/*   DO WHILE NOT AVAIL oe-ctrl AND i LT 1000:                                                */
/*     li = li + 1.                                                                           */
/*                                                                                            */
/*     FIND FIRST oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company EQ g_company NO-WAIT NO-ERROR. */
/*                                                                                            */
/*     IF AVAIL oe-ctrl THEN DO:                                                              */
/*       oe-ctrl.n-ord = io-ord-no + 1.                                                       */
/*       FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.                                               */
/*     END.                                                                                   */
/*                                                                                            */
/*     ELSE                                                                                   */
/*     IF li GE 1000 THEN io-ord-no = ?.                                                      */
/*   END.                                                                                     */
/*                                                                                            */
/*   RELEASE oe-ctrl.                                                                         */
  RUN sys/ref/asiseq.p (INPUT g_company, INPUT "order_seq", OUTPUT iNextOrd) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
   io-ord-no = iNextOrd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-est-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no Procedure 
PROCEDURE valid-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
 IF LOGICAL(get-val("adm-new-record")) THEN
 DO :
    IF get-sv("oe-ord.est-no") NE "" THEN DO:
      FIND FIRST est
          WHERE est.company EQ g_company
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(get-sv("oe-ord.est-no")))) +
                               TRIM(get-sv("oe-ord.est-no"))
          NO-LOCK NO-ERROR.
      IF NOT AVAIL est THEN DO:
          /* wfk - replace
        MESSAGE "Invalid Estimate#, try help..." VIEW-AS ALERT-BOX ERROR.
        set-entry("oe-ord.est-no").
        RETURN ERROR.
        */
      END.

      IF v-quo-price-log AND v-quo-price-dec EQ 1 THEN DO:
        FOR EACH quotehd
            WHERE quotehd.company EQ est.company
              AND quotehd.loc     EQ est.loc
              AND quotehd.est-no  EQ est.est-no
             NO-LOCK,
    
             EACH quoteitm OF quotehd NO-LOCK,

             EACH quoteqty OF quoteitm NO-LOCK:
          LEAVE.
        END.
              
        IF NOT AVAIL quoteqty THEN DO:
            /* wfk - replace
          MESSAGE "No quotes exists for this estimate..."
              VIEW-AS ALERT-BOX ERROR.
          set-entry("oe-ord.est-no").
          RETURN ERROR.
          */
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-job-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no Procedure 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    set-sv("oe-ord.job-no", FILL(" ",6 - LENGTH(TRIM(get-sv("oe-ord.job-no")))) +
                                 TRIM(get-sv("oe-ord.job-no"))). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-ord-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no Procedure 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ord FOR oe-ord.

       
  DO :
    IF INT(get-sv("oe-ord.ord-no")) EQ 0 OR
       CAN-FIND(FIRST b-oe-ord
                WHERE b-oe-ord.company EQ cocode
                  AND b-oe-ord.ord-no  EQ INT(get-sv("oe-ord.ord-no"))
                  AND ROWID(b-oe-ord)  NE ROWID(oe-ord))
    THEN DO:
        /* wfk - replace
      MESSAGE TRIM(oe-ord.ord-no:LABEL) +
              " is zero or invalid, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      set-entry("oe-ord.ord-no").
      RETURN ERROR.
      */
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-po-no) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no Procedure 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  
  DO:
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-ord.company
          AND cust.cust-no EQ get-sv("oe-ord.cust-no")
          AND cust.po-mandatory
        NO-ERROR.
    
    IF AVAIL cust AND TRIM(get-sv("oe-ord.po-no")) EQ "" THEN DO:
        /* wfk - replace
      MESSAGE "PO# is mandatory for this Customer..."
          VIEW-AS ALERT-BOX ERROR.
      set-entry("oe-ord.po-no").
      RETURN ERROR.
      */
    END.

    IF NOT ll-valid-po-no AND oeprompt AND get-sv("oe-ord.po-no") NE "" THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ oe-ord.company
          AND b-oe-ordl.po-no   EQ get-sv("oe-ord.po-no")
          AND b-oe-ordl.cust-no EQ get-sv("oe-ord.cust-no")
          AND b-oe-ordl.ord-no  NE INT(get-sv("oe-ord.ord-no"))
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN DO:
        /*  wfk - replace
      MESSAGE "Customer PO already exists for Order/Item - " + 
              TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
              TRIM(b-oe-ordl.i-no) " ." SKIP
              "Do you want to continue?"
          VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
      IF NOT ll-ans THEN DO:
        set-entry("oe-ord.po-no").
        RETURN ERROR.
      END.
      ELSE ll-valid-po-no = YES.
      */
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valid-sman) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman Procedure 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-sman LIKE sman.sman NO-UNDO.


  li = ip-int.

  IF li EQ 0 THEN
    ASSIGN
     ip-int = 1
     li     = 3.

  DO ip-int = ip-int TO li:
    lv-sman = IF ip-int EQ 3 THEN get-sv("oe-ord.sman[3]")
              ELSE
              IF ip-int EQ 2 THEN get-sv("oe-ord.sman[2]")
                             ELSE get-sv("oe-ord.sman[1]").
    
    IF lv-sman NE "" THEN DO:
        FIND FIRST sman NO-LOCK WHERE sman.company EQ cocode
                                  AND sman.sman    EQ lv-sman NO-ERROR.
        IF NOT AVAILABLE sman THEN DO:
            /* wfk - replace
          MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR. */

          IF ip-int EQ 3 THEN set-entry("oe-ord.sman[3]").
          ELSE
          IF ip-int EQ 2 THEN set-entry("oe-ord.sman[2]").
                         ELSE set-entry("oe-ord.sman[1]").
          RETURN ERROR.
        END.
        ELSE DO:
          IF ip-int EQ 3 THEN get-sv("oe-ord.sname[3]") = sman.sname.
          ELSE
          IF ip-int EQ 2 THEN get-sv("oe-ord.sname[2]") = sman.sname.
                         ELSE get-sv("oe-ord.sname[1]") = sman.sname.
        END.
    END.
    ELSE DO:
      IF ip-int EQ 3 THEN DO:
         set-sv("oe-ord.s-pct[3]", "0").
         set-sv("oe-ord.s-comm[3]", "0").
      END.
      ELSE
      IF ip-int EQ 2 THEN DO:     
        set-sv("oe-ord.s-pct[2]", "0").
        set-sv("oe-ord.s-comm[2]","0"). 
      END.
      ELSE DO:
         v-margin = 0.
         set-sv("oe-ord.s-pct[1]", "0").
         set-sv("oe-ord.s-comm[1]","0").
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-get-handle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-handle Procedure 
FUNCTION get-handle RETURNS HANDLE
( ipv-item AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ?.   /* Function return */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-sv Procedure 
FUNCTION get-sv RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
DEF VAR cValue AS CHAR NO-UNDO.
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF VALID-HANDLE(h_callproc) THEN
    RUN get-sv IN h_callproc (INPUT ipv-item, OUTPUT cValue).
RETURN cValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-val) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-val Procedure 
FUNCTION get-val RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-entry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION set-entry Procedure 
FUNCTION set-entry RETURNS CHARACTER
  ( ipv-item AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-sv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION set-sv Procedure 
FUNCTION set-sv RETURNS CHARACTER
  ( ipv-item AS CHAR, ipv-value AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(h_callproc) THEN
      RUN set-sv IN h_callproc (INPUT ipv-item, INPUT ipv-value).
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

