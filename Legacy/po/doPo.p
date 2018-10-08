&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : po/do-po.p 
    Purpose     :
                 Add PO from Order Entry Program - P/O Module                             
                 Similar logic in do-po-best.p                                              
    Syntax      :

    Description :

    Author(s)   : 04/99 FWK
    Created     :
    Notes       : Uncomment line in create-report-record!!!
                  Uncomment update oe-ordl in setPoValues!!!
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iplPromptRM AS LOGICAL     NO-UNDO.
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE gvlDebug AS LOG NO-UNDO.
gvldebug = NO.
DEFINE STREAM sDebug.
{sys/inc/var.i shared}

DEFINE NEW SHARED VARIABLE v-basis-w     LIKE ITEM.basis-w NO-UNDO.
DEFINE NEW SHARED VARIABLE v-len         LIKE ITEM.s-len NO-UNDO.
DEFINE NEW SHARED VARIABLE v-wid         LIKE ITEM.s-wid NO-UNDO.
DEFINE NEW SHARED VARIABLE v-dep         LIKE ITEM.s-dep NO-UNDO.
DEFINE NEW SHARED VARIABLE v-gl-desc     AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-msf     AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-adder       AS DECIMAL   EXTENT 2.
DEFINE NEW SHARED VARIABLE head          AS ch        FORMAT "x(80)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-unline      AS CHARACTER FORMAT "x(78)".
DEFINE NEW SHARED VARIABLE v-part-dscr1  AS CHARACTER FORMAT "x(30)".
DEFINE NEW SHARED VARIABLE v-part-dscr2  AS CHARACTER FORMAT "x(30)".
DEFINE NEW SHARED VARIABLE v-report-cost AS DECIMAL   INIT 0 FORMAT ">>,>>9.9999" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-cont-upd    AS LOG       INIT YES NO-UNDO.


DEFINE BUFFER b-po-ordl FOR po-ordl.
DEFINE BUFFER bf-itemfg FOR itemfg.

/* Program is based on this variable */
DEFINE SHARED     VARIABLE fil_id                 AS RECID     NO-UNDO.

DEFINE            VARIABLE cFilIdSource           AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-recid               AS RECID     NO-UNDO.
DEFINE            VARIABLE nufile                 AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE sel                    AS INTEGER   NO-UNDO.
DEFINE            VARIABLE call_id                AS RECID     NO-UNDO.
DEFINE            VARIABLE gvlChoice              AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-tot-cost             AS DECIMAL   DECIMALS 4 NO-UNDO.
DEFINE            VARIABLE v-access-close         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-access-list          AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-old-i-no             LIKE po-ordl.i-no NO-UNDO.
DEFINE            VARIABLE save_id                AS RECID.
DEFINE            VARIABLE v-op-type              AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-hld-cost             AS DECIMAL   FORMAT "->>>>>>>9.99<<".
DEFINE            VARIABLE v-hld-line-qty         AS DECIMAL   FORMAT "->>>>>>>>9.99<<".
DEFINE            VARIABLE v-uom                  AS CHARACTER FORMAT "x(4)".
DEFINE            VARIABLE v-ord-qty              LIKE po-ordl.ord-qty.
DEFINE            VARIABLE v-uom-help             AS CHARACTER.
DEFINE            VARIABLE count-mat              AS INTEGER   INIT 0 NO-UNDO.
DEFINE            VARIABLE v-frm                  LIKE job-mat.frm NO-UNDO.
DEFINE            VARIABLE v-blk                  LIKE job-mat.blank-no NO-UNDO.
DEFINE            VARIABLE v-lastkey              AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v_item_lu              AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-qty                  AS DECIMAL.
DEFINE            VARIABLE v-cost                 AS DECIMAL.
DEFINE            VARIABLE num-job-mats           AS INTEGER   INIT 0 NO-UNDO.
DEFINE            VARIABLE v-job-no               LIKE po-ordl.job-no NO-UNDO.
DEFINE            VARIABLE v-job-no2              LIKE po-ordl.job-no2 NO-UNDO.
DEFINE            VARIABLE v-new-board            AS LOGICAL   INIT NO.
DEFINE            VARIABLE item-recid             AS RECID     NO-UNDO.
DEFINE            VARIABLE jm-recid               AS RECID     NO-UNDO.
DEFINE            VARIABLE v-exist-item           AS LOGICAL   INIT YES NO-UNDO.
DEFINE            VARIABLE v-uom-list             AS CHARACTER NO-UNDO INIT "C,CS,EA,L,LB,LF,LOT,M,MSF,SHT,TON,BF".
DEFINE            VARIABLE pr-uom-list            AS CHARACTER NO-UNDO INIT "EA,LB,M,MSF,TON,BF".
DEFINE            VARIABLE cons-uom-list          AS CHARACTER NO-UNDO INIT "M,LF,EA,LB,TON".
DEFINE            VARIABLE fg-uom-list            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-item-cost            AS DECIMAL   FORMAT ">>,>>9.9999" INIT 0 NO-UNDO.
DEFINE            VARIABLE v-setup-cost           AS DECIMAL   FORMAT ">>,>>9.9999" INIT 0 NO-UNDO.
DEFINE            VARIABLE gvcVendNo              LIKE vend.vend-no INIT "" NO-UNDO.
DEFINE            VARIABLE v-vend-item            LIKE ITEM.vend-item NO-UNDO.
DEFINE            VARIABLE v-setup                LIKE e-item-vend.setup NO-UNDO.
DEFINE            VARIABLE v-recid                AS RECID.
DEFINE            VARIABLE v-autopo-sec           AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-autofg-sec           AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-autoprep-sec         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE nk1-oeautopo-int       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-ord-no               LIKE oe-ordl.ord-no NO-UNDO.
DEFINE            VARIABLE li                     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ll                     AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-hold                 AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE v-vendor-chosen-report AS RECID     NO-UNDO.
DEFINE            VARIABLE v-sel-uom              AS CHARACTER NO-UNDO.
DEFINE            VARIABLE rOrderPoRow            AS ROWID     NO-UNDO.
DEFINE            VARIABLE po-found               AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-new-i-no             LIKE ITEM.i-no INIT "" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-new-wid              LIKE ITEM.s-wid INIT 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-new-len              LIKE ITEM.s-len INIT 0 NO-UNDO.

{ce/msfcalc.i}
{sa/sa-sls01.i}

DEFINE NEW SHARED VARIABLE vmatch            AS ch.
DEFINE NEW SHARED VARIABLE v-open            AS LOGICAL   INIT YES.
DEFINE NEW SHARED VARIABLE v-findlines       AS LOGICAL   INIT YES.
DEFINE NEW SHARED VARIABLE v-tot-ord         AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE NEW SHARED VARIABLE v-neword          AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-abortord        AS LOGICAL   FORMAT "Yes/No".
DEFINE NEW SHARED VARIABLE save_hdr          AS RECID     NO-UNDO.
DEFINE NEW SHARED VARIABLE v-sname           LIKE shipto.ship-name.
DEFINE NEW SHARED VARIABLE v-saddr           LIKE shipto.ship-addr.
DEFINE NEW SHARED VARIABLE v-sadd1           AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-sadd2           AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-scity           LIKE shipto.ship-city.
DEFINE NEW SHARED VARIABLE v-sstate          LIKE shipto.ship-state.
DEFINE NEW SHARED VARIABLE v-szip            LIKE shipto.ship-zip.
DEFINE NEW SHARED VARIABLE errormsg          AS CHARACTER FORMAT "x(80)".
DEFINE NEW SHARED VARIABLE v-pocost1         AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-hold-op1        AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-all-msf         AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-po-qty          AS LOGICAL   INIT TRUE NO-UNDO.

DEFINE            VARIABLE v-exp-limit       AS INTEGER   NO-UNDO INIT 10.
DEFINE            VARIABLE v-hld-sel         AS INTEGER.
DEFINE            VARIABLE tmp-recid         AS RECID     NO-UNDO.
DEFINE            VARIABLE nk1-oeautopo-log  LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEFINE            VARIABLE v-uom-comp        LIKE po-ordl.pr-qty-uom NO-UNDO.
DEFINE            VARIABLE v-qty-comp        LIKE job-mat.qty NO-UNDO.
DEFINE            VARIABLE v-qty-comp1       LIKE job-mat.qty NO-UNDO.
DEFINE            VARIABLE gvdPoDate         AS DATE      NO-UNDO.
DEFINE            VARIABLE gvdDueDate        AS DATE      NO-UNDO.
DEFINE            VARIABLE nk1-oeautopo-char AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE            VARIABLE v-new-avail       AS LOGICAL   INIT NO NO-UNDO.
DEFINE            VARIABLE v-job             AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-job-mat-qty     LIKE job-mat.qty NO-UNDO.
DEFINE            VARIABLE v-job-mat-uom     LIKE job-mat.qty-uom NO-UNDO.
DEFINE            VARIABLE ld-line-qty       LIKE po-ordl.ord-qty NO-UNDO.
DEFINE            VARIABLE ld-part-qty       AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE ll-drop           AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE gvcDropCustNo     AS cha       NO-UNDO.
DEFINE            VARIABLE gvcShipChoice     AS cha       NO-UNDO.
DEFINE            VARIABLE ll-canceled       AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE ld-dim-charge     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE oeautoprep-log    LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE            VARIABLE gvrJobRecid       AS RECID     NO-UNDO.
DEFINE            VARIABLE v-actnum          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-charge          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-index           AS INTEGER   NO-UNDO.

DEFINE BUFFER tmp-po-ord FOR po-ord.
DEFINE BUFFER xjob-mat   FOR job-mat.
DEFINE BUFFER bpo-ordl   FOR po-ordl.
DEFINE BUFFER b-item     FOR ITEM.
DEFINE BUFFER b-oe-ordl  FOR oe-ordl.
DEFINE BUFFER bf-ordl    FOR oe-ordl.
DEFINE BUFFER bf-ord     FOR oe-ord.
DEFINE BUFFER b-orderpo  FOR reftable.
DEFINE BUFFER b-jc-calc  FOR reftable.
DEFINE BUFFER b-ref1     FOR reftable.
DEFINE BUFFER b-ref2     FOR reftable.
DEFINE BUFFER b-job-mat  FOR job-mat.
DEFINE BUFFER b-job-hdr  FOR job-hdr.
DEFINE BUFFER xest       FOR est.
DEFINE BUFFER xeb        FOR eb.

DEFINE TEMP-TABLE tt-ref1 NO-UNDO LIKE reftable.
DEFINE BUFFER tt-ref2 FOR tt-ref1.
DEFINE VARIABLE v-po-best       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-from-po-entry AS LOGICAL   NO-UNDO.

/* New for this program */
DEFINE VARIABLE lPoExists       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gvrPoOrdl       AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrItem         AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrVend         AS ROWID     NO-UNDO.

DEFINE VARIABLE gvrOeOrdl       AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrOeOrd        AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrJob          AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrPoOrd        AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrTT-eiv       AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrTT-ei        AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrB-orderPo    AS ROWID     NO-UNDO.
DEFINE VARIABLE gvrItemfg       AS ROWID     NO-UNDO.
DEFINE VARIABLE gvcFilIdSource  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gvrWJobMat      AS ROWID     NO-UNDO.


DEFINE VARIABLE llFirstJobFrm   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llFirstOfJobFrm AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gvrJcCalc       AS ROWID     NO-UNDO.
DEFINE VARIABLE lNextOuters     AS LOGICAL   NO-UNDO.


DEFINE NEW SHARED WORKFILE work-vend NO-UNDO
    FIELD cost AS DECIMAL FORMAT ">>,>>9.9999"
    FIELD v-cost-num AS INTEGER 
    FIELD v-recid AS RECID.

DEFINE TEMP-TABLE w-job-mat NO-UNDO like job-mat
    FIELD w-rowid      AS ROWID
    FIELD w-recid      AS RECID
    FIELD this-is-a-rm AS LOG
    FIELD isaset       AS LOG
    FIELD isacomponent AS LOG
    FIELD fg-i-no      LIKE job-hdr.i-no
    FIELD est-no       LIKE eb.est-no
    FIELD eqty         LIKE eb.eqty
    FIELD prep         AS LOG
    field estPrepEQty  AS DEC
    field estPrepLine  as int
    field miscType     as int
    field miscInd      as char.

DEFINE TEMP-TABLE tt-itemfg NO-UNDO 
    FIELD isaset       LIKE itemfg.isaset
    FIELD isacomponent AS LOG 
    FIELD pur-man      LIKE itemfg.pur-man
    FIELD form-no      LIKE eb.form-no
    FIELD blank-no     LIKE eb.blank-no
    FIELD qty          LIKE oe-ordl.qty                                 
    FIELD pur-uom      LIKE itemfg.pur-uom
    FIELD row-id       AS ROWID.

DEFINE BUFFER b-tt-itemfg FOR tt-itemfg.

DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD company AS CHARACTER
    FIELD i-no    AS CHARACTER
    FIELD std-uom AS CHARACTER
    INDEX i-no company i-no.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD company   AS CHARACTER
    FIELD vend-no   AS CHARACTER
    FIELD i-no      AS CHARACTER
    FIELD vend-i-no AS CHARACTER
    FIELD run-qty   AS DECIMAL   DECIMALS 3 EXTENT 20
    FIELD run-cost  AS DECIMAL   DECIMALS 4 EXTENT 20
    FIELD setups    AS DECIMAL   DECIMALS 2 EXTENT 20
    FIELD roll-w    AS DECIMAL   DECIMALS 4 EXTENT 30
    FIELD est-no    AS CHARACTER
    FIELD form-no   AS INTEGER
    FIELD blank-no  AS INTEGER
    FIELD item-type AS LOG
    FIELD rec_key   AS CHARACTER
    FIELD rec-id    AS RECID
    FIELD std-uom   AS CHARACTER
    INDEX i-no    company item-type i-no    vend-no
    INDEX vend-no company i-no      vend-no.

IF INDEX(PROGRAM-NAME(2),"add-po-best") GT 0 THEN
    v-po-best = YES.

IF INDEX(PROGRAM-NAME(2),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(3),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(4),"w-purord") GT 0 THEN
    v-from-po-entry = TRUE.

{fg/fullset.i NEW}

{sys/ref/pocost.i}

ASSIGN
    v-pocost1  = v-pocost
    v-hold-op1 = v-hold-op
    lv-recid   = fil_id.

DO TRANSACTION:

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "POQTY"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:

        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "POQTY"
            sys-ctrl.descrip  = "Auto PO QTY to Use Job QTY or Net Sheets?"
            sys-ctrl.char-fld = "JobQty"
            sys-ctrl.log-fld  = NO.
   
        RUN po/d-poqty.w (OUTPUT sys-ctrl.char-fld).       
    END.
    v-po-qty = IF sys-ctrl.char-fld EQ "Net Shts" THEN FALSE ELSE TRUE.
END.

DO TRANSACTION:

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "OEAUTOPO"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = cocode
            sys-ctrl.name     = "OEAUTOPO"
            sys-ctrl.descrip  = "Auto/Manual PO Creation from Order Entry? Multiple Jobs per PO?"
            sys-ctrl.char-fld = "Manual"
            sys-ctrl.log-fld  = NO.
        RUN po/d-oepo.w (OUTPUT sys-ctrl.char-fld).
        MESSAGE "Create PO with Multiple Jobs?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
    END.
    ASSIGN
        nk1-oeautopo-char = sys-ctrl.char-fld
        nk1-oeautopo-log  = sys-ctrl.log-fld
        nk1-oeautopo-int  = sys-ctrl.int-fld.
END.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "OEAutoPO",
    INPUT "ALL",
    INPUT NO,
    INPUT NO,
    INPUT NO,
    OUTPUT v-autopo-sec,
    OUTPUT v-access-close,
    OUTPUT v-access-list).
/* Security check only for order entry */
IF v-from-po-entry THEN
    v-autopo-sec = TRUE.

DO TRANSACTION:

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OEAUTOPREP"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "OEAUTOPREP"
            sys-ctrl.log-fld = NO
            sys-ctrl.descrip = "Auto Prep PO Creation from Order Entry?".
  
    END.

    oeautoprep-log = sys-ctrl.log-fld.
END. /* Transaction */
/* Check if authorized to create PO's */
IF oeautoprep-log THEN
    RUN methods/prgsecur.p
        (INPUT "OEAutoPrep", /* program */
        INPUT "ALL",        /*Basis */
        INPUT NO,
        INPUT NO,
        INPUT NO,
        OUTPUT v-autoprep-sec,
        OUTPUT v-access-close,
        OUTPUT v-access-list).

/* Security only for entry from order entry */
IF v-from-po-entry THEN
    v-autoprep-sec = TRUE.
{sys/inc/ap-gl#.i}

DO TRANSACTION:
    {sys/inc/oeautofg.i}
    {sys/inc/pouom.i}
    {sys/inc/aptax.i}
END.

/* Check if authorized to create PO's */
IF oeautofg-log THEN
    RUN methods/prgsecur.p
        (INPUT "OEAutoFG", /* Program master program name */
        INPUT "ALL",      /* Security based on */
        INPUT NO,
        INPUT NO,
        INPUT NO,
        OUTPUT v-autofg-sec,
        OUTPUT v-access-close,
        OUTPUT v-access-list).

/* only check security from order entry */
IF v-from-po-entry THEN 
    v-autofg-sec = TRUE.

FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
/* fil_id = oe-ordl or job to process */
/* oeautofg-log v-autofg-sec, whether to create and security */

IF gvlDebug THEN
    OUTPUT STREAM sDebug TO c:\tmp\doPoDebug.txt.

IF gvlDebug THEN
    PUT STREAM sDebug UNFORMATTED "Start Program " SKIP.

/*     oe-ordl buffer           */
/*     sets v-ord-no            */
/*     creates tt-fg-set        */
/*     create tt-itemfg records */
/* finds a oe-ordl or a job record matching fil_id */
RUN findOrderFromRecid (INPUT fil_id, 
    OUTPUT gvrOeOrdl, 
    OUTPUT gvrJob,
    OUTPUT gvcFilIdSource).

/*     creates tt-itemfg records */
/*     create tt-fg-set          */
/* create tt-itemfg based on a job */
FIND job WHERE ROWID(job) EQ gvrJob NO-LOCK NO-ERROR.
cFilIdSource = gvcFilIdSource.

IF gvcFilIdSource EQ "JOB" THEN
    RUN ttItemfgFromJob (INPUT gvrJob, INPUT gvrOeOrdl, INPUT gvcFilIdSource).

FIND oe-ordl WHERE ROWID(oe-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.
FIND oe-ord WHERE ROWID(oe-ord) EQ gvrOeOrd NO-LOCK NO-ERROR.
FIND job WHERE ROWID(job) EQ gvrJob NO-LOCK NO-ERROR.

/* run several procedures to build w-job-mat */
RUN buildJobmat.

/* For each w-job-mat, process it */
RUN processJobMat.


IF gvlDebug THEN
    OUTPUT STREAM sDebug CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addHeaderTot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addHeaderTot Procedure 
PROCEDURE addHeaderTot :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.    
    ASSIGN
        v-old-i-no         = bf-po-ordl.i-no
        v-tot-ord          = v-tot-ord + bf-po-ordl.t-cost
        bf-po-ordl.dscr[1] = v-part-dscr1
        bf-po-ordl.dscr[2] = v-part-dscr2.
    RELEASE bf-po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-askDropShip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE askDropShip Procedure 
PROCEDURE askDropShip :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs
          bf-ordl
          w-job-mat
          po-ord (modified)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.

    DEFINE VARIABLE ll          AS LOG       NO-UNDO.
    DEFINE VARIABLE look-recid  AS RECID     NO-UNDO.
    DEFINE VARIABLE char-val    AS cha       NO-UNDO.
    DEFINE VARIABLE rec-val     AS RECID     NO-UNDO.
    DEFINE VARIABLE ship-choice AS CHARACTER LABEL "  Ship To"
        VIEW-AS RADIO-SET HORIZONTAL
        RADIO-BUTTONS "Vendor", "Vendor",
        "Customer", "Customer"
        SIZE 28 BY 1 NO-UNDO.

    DEFINE BUTTON Btn_OK AUTO-GO 
        LABEL "OK" 
        SIZE 15 BY 1
        BGCOLOR 8.

    DEFINE FRAME f-drop
        ship-choice SKIP
        btn_ok AT ROW 2 COL 11
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER TITLE "Drop Ship PO"
        SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE.

    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-ordl      FOR oe-ordl.

    FIND bf-po-ord   WHERE ROWID(bf-po-ord)     EQ iprPoOrd EXCLUSIVE-LOCK NO-ERROR.
    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-ordl      WHERE ROWID(bf-ordl)      EQ iprOeOrdl NO-LOCK NO-ERROR.  
  
    ON "value-changed" OF ship-choice
        DO:
            gvcShipChoice = SUBSTR(ship-choice:SCREEN-VALUE,1,1).
        END.

    ON 'choose':U OF btn_ok
        DO:
            APPLY "go" TO FRAME f-drop.
        END.

    bf-po-ord.type = "D".
    RELEASE itemfg.
    IF AVAILABLE bf-ordl AND NOT bf-w-job-mat.this-is-a-rm THEN
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ bf-ordl.company
            AND itemfg.i-no    EQ bf-ordl.i-no
            NO-ERROR.

    CREATE WIDGET-POOL "w-drop".

    IF gvcDropCustNo EQ "" THEN gvcShipChoice = "V".
    ELSE 
        IF NOT AVAILABLE itemfg  /* task# 09160518*/ THEN 
        DO:
            ENABLE ship-choice btn_ok WITH FRAME f-drop.
            APPLY "value-changed" TO ship-choice.
            APPLY "entry" TO ship-choice.
            WAIT-FOR GO OF FRAME f-drop.
        END.

    IF gvcShipChoice EQ "C" THEN 
    DO WHILE TRUE:
        IF AVAILABLE itemfg THEN 
        DO:  /* task# 09160518*/
            FIND FIRST oe-rel OF bf-ordl NO-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ bf-ordl.company
                    AND shipto.cust-no EQ bf-ordl.cust-no
                    AND shipto.ship-id EQ oe-rel.ship-id
                    USE-INDEX ship-id NO-ERROR.
            IF AVAILABLE shipto THEN rec-val = RECID(shipto).
        END. /* if avail itemfg */

        IF NOT AVAILABLE itemfg OR rec-val EQ ? THEN  /* task# 09160518*/
            RUN windows/l-shipt2.w (cocode, locode, gvcDropCustNo, bf-po-ord.ship-id, OUTPUT char-val, OUTPUT rec-val).

        FIND shipto WHERE RECID(shipto) EQ rec-val NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN 
        DO:
            ASSIGN
                bf-po-ord.cust-no      = gvcDropCustNo
                bf-po-ord.ship-id      = shipto.ship-id
                bf-po-ord.ship-name    = shipto.ship-name
                bf-po-ord.ship-addr[1] = shipto.ship-addr[1]
                bf-po-ord.ship-addr[2] = shipto.ship-addr[2]
                bf-po-ord.ship-city    = shipto.ship-city
                bf-po-ord.ship-state   = shipto.ship-state
                bf-po-ord.ship-zip     = shipto.ship-zip.

            IF bf-po-ord.frt-pay NE "P" THEN bf-po-ord.carrier = shipto.carrier.
            LEAVE.
        END. /* if avail shipto */
        ELSE 
        DO:
            MESSAGE "Cust not found. Retrying..." 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.
    END. /* if gvcShipChoice eq "C" */

    ELSE
    DO WHILE TRUE:

        RUN windows/l-vendno.w (cocode, "A", bf-po-ord.ship-id, OUTPUT char-val).
        IF char-val NE "" THEN 
        DO:
            bf-po-ord.ship-id = ENTRY(1,char-val).
            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ cocode
                AND vend.vend-no EQ bf-po-ord.ship-id
                NO-ERROR.
            IF AVAILABLE vend THEN 
            DO:
                ASSIGN
                    bf-po-ord.cust-no      = ""
                    bf-po-ord.ship-name    = vend.name
                    bf-po-ord.ship-addr[1] = vend.add1
                    bf-po-ord.ship-addr[2] = vend.add2
                    bf-po-ord.ship-city    = vend.city
                    bf-po-ord.ship-state   = vend.state
                    bf-po-ord.ship-zip     = vend.zip
                    bf-po-ord.carrier      = vend.carrier.
                LEAVE.
            END. /* avail vend */
            ELSE 
            DO:
                MESSAGE "Error: Vendor not found retrying: " bf-po-ord.ship-id
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                NEXT.
            END.
        END. /* char-val ne "" */
    END. /* NOT gvcShipChoice eq "C" */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-autoRm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE autoRm Procedure 
PROCEDURE autoRm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl
          job-mat
          w-job-mat (updated)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    DEFINE BUFFER xjob-mat   FOR job-mat.

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.    
 
    IF nk1-oeautopo-char EQ "AutoRM" AND v-autopo-sec THEN 
    DO:

        FIND FIRST b-item
            WHERE b-item.company EQ cocode
            AND b-item.i-no    EQ v-new-i-no
            AND b-item.i-no    NE ""
            NO-LOCK NO-ERROR.
        IF AVAILABLE b-item THEN bf-po-ordl.i-no = v-new-i-no.
        FIND FIRST xjob-mat WHERE RECID(xjob-mat) EQ RECID(job-mat) NO-ERROR.
        IF AVAILABLE xjob-mat THEN w-job-mat.rm-i-no = v-new-i-no.

    END. /* nk1-oeautopo-char eq "AutoRM" ... */ 
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-brdLenCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brdLenCheck Procedure 
PROCEDURE brdLenCheck :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-LOCK NO-ERROR.

    IF v-len               EQ 0                       AND
        bf-po-ordl.cons-uom    EQ "EA"                    AND
        (bf-po-ordl.item-type OR
        LOOKUP(bf-po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
        LOOKUP(bf-po-ordl.pr-uom,fg-uom-list)     EQ 0)   AND
        (bf-po-ordl.pr-qty-uom NE bf-po-ordl.cons-uom OR
        bf-po-ordl.pr-uom     NE bf-po-ordl.cons-uom)        THEN 
    DO:
        MESSAGE "Length must be entered"  VIEW-AS ALERT-BOX ERROR.
    END. /* if v-len eq 0 ... */
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildJobMat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildJobMat Procedure 
PROCEDURE buildJobMat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-job FOR job.
    IF gvlDebug THEN
        PUT STREAM sDebug UNFORMATTED "Start buildJobMat" SKIP.
  
    FIND oe-ord WHERE ROWID(oe-ord) EQ gvrOeOrd NO-LOCK NO-ERROR.
    FIND oe-ordl WHERE ROWID(oe-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.
    FIND bf-job WHERE ROWID(bf-job) EQ gvrJob NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.
  
    IF AVAIL(oe-ordl) AND NOT AVAIL(oe-ord) THEN
        FIND oe-ord WHERE oe-ord.company EQ oe-ordl.company
            AND oe-ord.ord-no EQ oe-ordl.ord-no
            NO-LOCK NO-ERROR.
  
    /* Create w-job-mat from itemfg */
    IF oeautofg-log AND v-autofg-sec AND AVAILABLE oe-ord AND AVAILABLE oe-ordl THEN
        RUN wJobFromttItemfg
            (INPUT  oeautofg-chr,
            INPUT  gvrOeOrd,
            INPUT gvrOeOrdl).
 
    /* Create w-job-mat from job-mat */
    IF AVAILABLE bf-job AND ((nk1-oeautopo-char NE "Manual" AND v-autopo-sec AND v-po-best EQ NO) OR v-po-best) THEN
        RUN wJobFromJobMat (INPUT v-po-best, INPUT ROWID(bf-job), INPUT gvrOeOrdl).
  
    IF AVAILABLE bf-job 
        AND oeautoprep-log AND v-autoprep-sec 
        AND AVAILABLE bf-ordl AND v-po-best EQ NO THEN 
    DO:

        /* create w-job-mat from job-prep */
        RUN wJobFromJobPrep (INPUT ROWID(bf-job), INPUT oe-ordl.i-no).
        /* create w-job-mat from b-job-mat */
        RUN wJobFromBJobMat (INPUT ROWID(bf-job)).

    END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildRptRecs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildRptRecs Procedure 
PROCEDURE buildRptRecs :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      inputs:
        uses w-job-mat
         cocode
        uses bf-ordl
         v-wid (global)
         v-len (global)
         v-term (global)
         v-qty-comp (global)
         ld-dim-charge (global)
         fil_id (global)
         v-setup (global)
         v-qty-comp1 (global)
         v-vendor-chosen-report (global) not currently used
         
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstFrm   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprTT-ei      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItemfg     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprVend       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-ordl      FOR oe-ordl.

    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.


    /*****************************************/
    /* Create report records                 */
    /*****************************************/
    FIND tt-ei WHERE ROWID(tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.
    IF AVAILABLE tt-ei THEN 
    DO:
        FOR EACH tt-eiv
            WHERE tt-eiv.company    EQ cocode
            AND tt-eiv.i-no       EQ tt-ei.i-no
            AND tt-eiv.item-type  EQ bf-w-job-mat.this-is-a-rm
            AND tt-eiv.vend-no    NE ""
          
            AND (tt-eiv.item-type EQ NO OR
            (v-wid                GE tt-eiv.roll-w[27] AND
            v-wid                LE tt-eiv.roll-w[28] AND
            v-len                GE tt-eiv.roll-w[29] AND
            v-len                LE tt-eiv.roll-w[30]))
            NO-LOCK,

            FIRST vend
            WHERE vend.company EQ cocode
            AND vend.vend-no EQ tt-eiv.vend-no
            AND vend.active  EQ "A"
            NO-LOCK:

            DO i = 1 TO EXTENT(tt-eiv.run-qty):
                IF v-qty-comp LE tt-eiv.run-qty[i] THEN LEAVE.
            END.
            IF i > 20 THEN
                i = 20.

            v-setup = tt-eiv.setups[i].
          
            ld-dim-charge = 0.

            RUN est/dim-charge.p (tt-eiv.rec_key,
                v-wid,
                v-len,
                INPUT-OUTPUT ld-dim-charge).     
            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = STRING(tt-eiv.run-cost[i] + ld-dim-charge,"9999999999.9999")
                report.key-02  = STRING(v-qty-comp) /* msf or tt-ei.std-uom */
                report.key-03  = tt-eiv.vend-no
                report.key-04  = STRING(v-qty-comp1) /* tons */
                report.key-05  = STRING((v-setup / v-qty-comp),"9999999999.9999")
                report.key-06  = STRING(v-setup,"9999999999.9999")
                report.key-07  = tt-eiv.vend-i-no
                report.key-08  = tt-eiv.i-no
                report.rec-id  = tt-eiv.rec-id.

        END. /* for each tt-eiv */
    
        RELEASE report.
       
        IF gvlChoice THEN 
        DO:
      
            IF gvlDebug THEN
                PUT STREAM sDebug UNFORMATTED "buildRptRec - choose vendor " + bf-w-job-mat.i-no SKIP.
            RUN po/d-vndcst.w (v-term, bf-w-job-mat.w-recid,
                bf-w-job-mat.this-is-a-rm, bf-w-job-mat.i-no,
                INPUT v-qty-comp, INPUT v-job-mat-uom).
      
            IF fil_id EQ ? THEN ll-canceled = YES.
            ELSE FIND report WHERE RECID(report) EQ fil_id NO-LOCK NO-ERROR.
      
            IF AVAILABLE report THEN 
            DO:
                v-vendor-chosen-report = report.REC-ID.
                /* create tt-eiv for a specific itemfg (from e-itemfg-vend records) */
                FIND itemfg WHERE ROWID(itemfg) EQ iprItemfg NO-LOCK NO-ERROR.
          
                RUN createTtEiv (INPUT iprItemfg, INPUT ROWID(bf-w-job-mat)) NO-ERROR.

            END.
      
        END. /* If gvlChoice = true */

        ELSE 
        DO:
            FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ gvcVendNo NO-ERROR.
            IF AVAILABLE tt-eiv THEN
                FIND FIRST report
                    WHERE report.term-id EQ v-term
                    AND report.rec-id  EQ tt-eiv.rec-id
                    NO-LOCK NO-ERROR.
        END. /* If not gvlChoice = true */

   
        IF AVAILABLE report THEN 
        DO:
        
            ASSIGN
                fil_id       = report.rec-id
                v-item-cost  = DEC(report.key-01)
                v-setup-cost = DEC(report.key-05)
                v-setup      = DEC(report.key-06)
                v-vend-item  = report.key-07.
       
            FIND FIRST vend
                WHERE vend.company EQ cocode
                AND vend.vend-no EQ report.key-03 
                NO-LOCK NO-ERROR.
            IF AVAILABLE vend THEN 
            DO:   
                oprVend = ROWID(vend).
                gvcVendNo = vend.vend-no.          
                IF AVAILABLE bf-ordl AND iplFirstFrm THEN
                    FOR EACH b-oe-ordl
                        WHERE b-oe-ordl.company EQ bf-ordl.company
                        AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
                        AND b-oe-ordl.job-no  EQ bf-ordl.job-no
                        AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
                        EXCLUSIVE-LOCK:
                        /* for testing, uncomment this when live */
                        b-oe-ordl.vend-no = vend.vend-no. 
                    END. /* each b-oe-ordl */
            END. /* if avail vend */
        END. /* If avail report */
    

        FOR EACH report
            {sys/look/reportW.i}
           AND CAN-FIND(FIRST tt-eiv WHERE tt-eiv.rec-id EQ report.rec-id):
        DELETE report.
    END.

END. /* avail tt-ei */
RELEASE bf-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcCost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcCost Procedure 
PROCEDURE calcCost :
    /*------------------------------------------------------------------------------
      Purpose:     Calculate of po-ordl.cost
      Parameters:  <none>
      Notes:       
      Inputs:
        po-ordl (modified)
        tt-ei
        globals: v-pocost1, v-basis-w, v-wid, v-len, v-dep
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprtt-ei    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER bf-tt-ei   FOR tt-ei.
     
    FIND bf-tt-ei WHERE ROWID(bf-tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.item-type         AND
        v-pocost1 EQ "Vendor/MSH" AND
        AVAILABLE bf-tt-ei               AND
        bf-tt-ei.std-uom EQ "TON"    AND
        v-basis-w NE 0            AND
        v-wid NE 0                THEN 
    DO:
        RUN sys/ref/convcuom.p (bf-tt-ei.std-uom, "MSH",
            v-basis-w, v-len, v-wid, v-dep,
            bf-po-ordl.cost, OUTPUT bf-po-ordl.cost).
        bf-po-ordl.pr-uom = "MSH".
    END. /* if bf-po-ordl.item-type ... */
    RELEASE bf-po-ordl.
    RELEASE bf-tt-ei.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcCostSetup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcCostSetup Procedure 
PROCEDURE calcCostSetup :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          tt-ei
          w-job-mat
          gvcVendNo
          v-vend-item
          po-ordl (updates)
    
    ------------------------------------------------------------------------------*/
  
    DEFINE INPUT  PARAMETER iprTT-ei      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendNo     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendItem   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-po-ordl   FOR po-ordl.  
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-tt-ei     FOR tt-ei.  
  
    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-tt-ei WHERE ROWID(bf-tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAILABLE bf-tt-ei THEN
        FIND FIRST bf-tt-ei
            WHERE bf-tt-ei.company EQ cocode
            AND bf-tt-ei.i-no    EQ bf-w-job-mat.rm-i-no
            NO-LOCK NO-ERROR.

    IF NOT AVAILABLE tt-eiv THEN
        FIND FIRST tt-eiv
            WHERE tt-eiv.company   EQ bf-w-job-mat.company
            AND tt-eiv.i-no      EQ bf-w-job-mat.i-no
            AND tt-eiv.vend-no   EQ gvcVendNo
            NO-LOCK NO-ERROR.
  
    IF AVAILABLE bf-tt-ei  THEN 
    DO:
  
        ASSIGN
            bf-po-ordl.cost      = v-item-cost
            bf-po-ordl.setup     = v-setup     
            bf-po-ordl.vend-i-no = v-vend-item.
        /* Uncomment below to implement vendor UOM per matrix */
        /* IF v-vendor-chosen-report EQ ? THEN */
        bf-po-ordl.pr-uom = bf-tt-ei.std-uom.
    END. /* Avail bf-tt-ei */
    ELSE
        ASSIGN
            bf-po-ordl.cost   = bf-w-job-mat.std-cost
            bf-po-ordl.setup  = 0
            bf-po-ordl.pr-uom = bf-w-job-mat.sc-uom.
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcDueDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcDueDate Procedure 
PROCEDURE calcDueDate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat EXCLUSIVE-LOCK NO-ERROR.

    RUN po/d-podate.w ("PO",INPUT-OUTPUT gvdPoDate, v-job, bf-w-job-mat.frm, bf-w-job-mat.rm-i-no).
    IF gvdDueDate LE gvdPoDate THEN gvdDueDate = gvdPoDate + 1.
    RUN po/d-podate.w ("Due",INPUT-OUTPUT gvdDueDate, v-job, bf-w-job-mat.frm, bf-w-job-mat.rm-i-no).
    RELEASE bf-w-job-mat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcEstValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcEstValues Procedure 
PROCEDURE calcEstValues :
    /*------------------------------------------------------------------------------
      Purpose:     If an RM, get values from estimate
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl (exclusive)
          v-po-qty
          w-job-mat
          bf-ordl
          job-hdr
          job
          item
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem    AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl    FOR po-ordl.  
    DEFINE BUFFER bf-w-job-mat  FOR w-job-mat.
    DEFINE BUFFER bf-ordl       FOR oe-ordl.  
    DEFINE BUFFER bf-item       FOR ITEM.
    DEFINE BUFFER bf2-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-job-hdr    FOR job-hdr.
    DEFINE BUFFER bf2-w-job-mat FOR w-job-mat.

    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND bf-item WHERE ROWID(bf-item) EQ iprItem NO-LOCK NO-ERROR.

    IF gvrJobRecid NE ? THEN
        FIND job-hdr 
            WHERE RECID(job-hdr) EQ gvrJobRecid
            NO-LOCK.
    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    IF bf-po-ordl.item-type THEN 
    DO:
        /* S-8-POQTY JOBQTY or NETSHTS */

        IF v-po-qty OR bf-w-job-mat.n-up EQ 0 OR
      INDEX("BP",bf-item.mat-type) LE 0 THEN  
        DO:       
            ld-line-qty = bf-w-job-mat.qty.  /* Job Qty */
        END.
        ELSE 
        DO:
            ASSIGN
                ld-line-qty = IF AVAILABLE bf-ordl THEN
                        bf-ordl.qty
                     ELSE
                     IF AVAILABLE job-hdr THEN
                        job-hdr.qty
                     ELSE
                        bf-w-job-mat.qty
                ld-part-qty = 0.                  

            IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN 
            DO:
     
                FOR EACH eb FIELDS(quantityPerSet)
                    WHERE eb.company EQ job.company
                    AND eb.est-no  EQ job.est-no
                    AND eb.form-no EQ bf-w-job-mat.frm
                    NO-LOCK:
                    ld-part-qty = ld-part-qty +
                        (ld-line-qty * IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet)
                        ELSE eb.quantityPerSet).
                END. /* Each eb */
         
            END.
            ELSE  IF AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN 
                DO:
                    ld-line-qty = 0.
        
                    IF AVAILABLE bf-ordl THEN 
                    DO:
                        FOR EACH bf-job-hdr WHERE bf-job-hdr.company EQ job-hdr.company
                            AND bf-job-hdr.job-no EQ job-hdr.job-no 
                            AND bf-job-hdr.job-no2 EQ job-hdr.job-no2
                            NO-LOCK,
                            FIRST bf2-ordl WHERE bf2-ordl.company EQ bf-ordl.company
                            AND bf2-ordl.ord-no EQ bf-ordl.ord-no
                            AND bf2-ordl.i-no EQ bf-job-hdr.i-no
                            NO-LOCK:
                            IF AVAILABLE bf2-ordl THEN
                                ld-line-qty = ld-line-qty + bf2-ordl.qty.

                        END. /* each bf-job-hdr */         
          
                    END. /* oe-ordl was available */
                    ELSE IF AVAILABLE job-hdr THEN 
                        DO:
                            FOR EACH bf-job-hdr WHERE bf-job-hdr.company EQ job-hdr.company
                                NO-LOCK:
                                ld-line-qty = ld-line-qty + bf-job-hdr.qty.
                            END. /* each bf-job-hdr */          
                        END. /* job-hdr was available */
                        ELSE IF AVAILABLE  bf-w-job-mat THEN 
                            DO:
                                FOR EACH bf2-w-job-mat WHERE bf2-w-job-mat.i-no EQ bf-w-job-mat.i-no:
                                    ld-line-qty = ld-line-qty + (bf2-w-job-mat.qty * bf2-w-job-mat.n-up).
                                END.          
                            END.
          
                    ld-line-qty = ld-line-qty / bf-w-job-mat.n-up.

                END.
                ELSE 
                    ld-part-qty = ld-line-qty.

            IF NOT(AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8)) THEN
                ld-line-qty = ld-part-qty / bf-w-job-mat.n-up.

            IF bf-po-ordl.pr-qty-uom EQ "EA" THEN 
            DO:
                {sys/inc/roundup.i ld-line-qty}             
            END.


        END. /* NOT v-po-qty OR bf-w-job-mat.n-up EQ 0 OR ... */

        IF bf-po-ordl.pr-qty-uom NE "EA" THEN
            RUN sys/ref/convquom.p (bf-w-job-mat.qty-uom,bf-po-ordl.pr-qty-uom,
                                bf-w-job-mat.basis-w, bf-w-job-mat.len, bf-w-job-mat.wid, bf-w-job-mat.dep,
                                ld-line-qty, OUTPUT ld-line-qty).

        bf-po-ordl.ord-qty = ld-line-qty.
    END. /* If po-ordl.item-type */
    RELEASE bf-po-ordl.
    RELEASE job.
    RELEASE job-hdr.
    RELEASE bf-w-job-mat.
    RELEASE bf-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcExtCost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcExtCost Procedure 
PROCEDURE calcExtCost :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.


    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    
    /**************************************************************/
    /**  Calculate Extended cost, order quantity is based on UOM **/
    /**************************************************************/
    IF LOOKUP(bf-po-ordl.pr-uom,"L,LOT") GT 0 THEN
        bf-po-ordl.t-cost = (bf-po-ordl.cost + bf-po-ordl.setup) *
            IF bf-po-ordl.ord-qty LT 0 THEN -1 ELSE 1.

    ELSE 
    DO:
        v-ord-qty = bf-po-ordl.ord-qty.

        IF bf-po-ordl.pr-qty-uom NE bf-po-ordl.pr-uom            AND
            (bf-po-ordl.item-type                           OR
            LOOKUP(bf-po-ordl.pr-qty-uom,fg-uom-list) EQ 0 OR
            LOOKUP(bf-po-ordl.pr-uom,fg-uom-list)     EQ 0)   THEN

            RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, bf-po-ordl.pr-uom,
                v-basis-w, v-len, v-wid, v-dep,
                v-ord-qty, OUTPUT v-ord-qty).
 
        bf-po-ordl.t-cost = (v-ord-qty * bf-po-ordl.cost) + bf-po-ordl.setup.
    END. /* NOT LOOKUP(bf-po-ordl.pr-uom,"L,LOT") GT 0 */

    bf-po-ordl.cons-cost = bf-po-ordl.t-cost / bf-po-ordl.cons-qty.

    IF bf-po-ordl.disc NE 0 THEN bf-po-ordl.t-cost = bf-po-ordl.t-cost * (1 - (bf-po-ordl.disc / 100)).
    RELEASE bf-po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcLenWid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcLenWid Procedure 
PROCEDURE calcLenWid :
    /*------------------------------------------------------------------------------
      Purpose:     Calculate len & width values 
      Parameters:  <none>
      Notes:       
        INputs:
          po-ordl
          b-item
          po-ord
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER b-item     FOR ITEM.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND b-item WHERE ROWID(b-item) EQ iprItem NO-LOCK NO-ERROR.

    ASSIGN
        v-len = 0
        v-wid = 0
        v-dep = 0.

    IF AVAILABLE b-item THEN 
    DO:

        FIND FIRST e-item-vend WHERE
            e-item-vend.company EQ cocode AND
            e-item-vend.i-no EQ bf-po-ordl.i-no AND
            e-item-vend.vend-no EQ bf-po-ord.vend-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE e-item-vend AND e-item-vend.vend-item NE "" THEN
            bf-po-ordl.vend-i-no = e-item-vend.vend-item.
        ELSE
            IF b-item.vend-no EQ bf-po-ord.vend-no THEN
                bf-po-ordl.vend-i-no = b-item.vend-item.
            ELSE
                IF b-item.vend2-no EQ bf-po-ord.vend-no THEN
                    bf-po-ordl.vend-i-no = b-item.vend2-item.

        IF INDEX("1234BPR",b-item.mat-type) GT 0 THEN 
        DO:
            ASSIGN
                v-basis-w = b-item.basis-w
                v-len     = b-item.s-len
                v-wid     = b-item.s-wid
                v-dep     = b-item.s-dep.
     
            IF v-wid EQ 0 THEN v-wid = b-item.r-wid.
        END. /* if index(... */
    END. /* Avail b-item */
  
    /* Cust-no from order or job */
    bf-po-ordl.cust-no = IF AVAILABLE bf-ord THEN bf-ord.cust-no
    ELSE
        IF AVAILABLE job-hdr THEN job-hdr.cust-no
        ELSE "".
    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
    RELEASE bf-po-ordl.
    RELEASE bf-po-ord.
    RELEASE b-item.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcMSF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcMSF Procedure 
PROCEDURE calcMSF :
    /*------------------------------------------------------------------------------
      Purpose:     Total MSF Calculation
      Parameters:  <none>
      Notes:       
        inputs:
          po-ordl (modified)
          globals: v-tot-msf, v-len, v-wid
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.pr-qty-uom EQ "EA" THEN
        v-tot-msf = IF v-corr THEN ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) * .007) *
            bf-po-ordl.ord-qty) / 1000)
            ELSE ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) / 144) *
            bf-po-ordl.ord-qty) / 1000).
    ELSE
        v-tot-msf = 0.


    /** Appears to be here so that it is assigned after the Total MSF Calculation */
    ASSIGN
        bf-po-ordl.s-len = v-len
        bf-po-ordl.s-wid = v-wid.
    IF v-dep GT 0 THEN DO:        
        bf-po-ordl.s-dep = v-dep.
    END.
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calcOrdQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcOrdQty Procedure 
PROCEDURE calcOrdQty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        w-job-mat
        po-ordl (updates)
        globals vars: v-basis-w, v-len, v-wid, v-dep    
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl   FOR po-ordl.  
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.

  
    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.  
    IF NOT AVAILABLE bf-w-job-mat OR NOT AVAILABLE bf-po-ordl THEN 
    DO:
        FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
        RETURN ERROR.
    END.

    ASSIGN
        v-len = bf-w-job-mat.len
        v-wid = bf-w-job-mat.wid
        v-dep = bf-w-job-mat.dep.

    IF bf-po-ordl.s-num EQ 0 AND bf-po-ordl.b-num EQ 0 THEN
        ASSIGN bf-po-ordl.s-num = bf-w-job-mat.frm
            bf-po-ordl.b-num = bf-w-job-mat.blank-no.

    IF bf-po-ordl.pr-qty-uom EQ "BF" THEN 
    DO:
        RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, "EA",
            v-basis-w, v-len, v-wid, v-dep,
            bf-po-ordl.ord-qty, OUTPUT bf-po-ordl.ord-qty).

        {sys/inc/roundup.i bf-po-ordl.ord-qty}
        bf-po-ordl.pr-qty-uom = "EA".
    END. /* bf-po-ordl.pr-qty-uom EQ "BF" */
    RELEASE bf-po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cancelMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelMessage Procedure 
PROCEDURE cancelMessage :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /*****************************************************/
    /* Process Error on no vendor matrix                 */
    /*****************************************************/

    MESSAGE "Cannot create a PO for " +
        (IF AVAILABLE job THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
        TRIM(STRING(w-job-mat.frm,"99")))
        ELSE ("Order/FG#: " +
        TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
        "/" + TRIM(w-job-mat.rm-i-no) +
        ", Vendor Matrix does not exist..."
        VIEW-AS ALERT-BOX ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkZeroQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkZeroQty Procedure 
PROCEDURE checkZeroQty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.


    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER bf-po-ord  FOR po-ord.  
    DEFINE BUFFER bf-e-itemfg-vend  FOR e-itemfg-vend.  
    

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-po-ordl THEN
        FIND FIRST bf-po-ord 
            WHERE bf-po-ord.company EQ bf-po-ordl.company
            AND bf-po-ord.po-no EQ bf-po-ordl.po-no
            NO-LOCK NO-ERROR.
  
    /***********************************************/
    /* Handle a zero line quantity or cost         */
    /***********************************************/
    /** Purchase UOM and Consuption UOM are equal **/
    IF NOT bf-po-ord.received AND (v-hld-line-qty EQ 0 OR v-hld-cost EQ 0) THEN 
    DO:



        IF bf-po-ordl.pr-qty-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                       AND
            LOOKUP(bf-po-ordl.pr-qty-uom,fg-uom-list) GT 0 AND
            LOOKUP(bf-po-ordl.cons-uom,fg-uom-list)   GT 0)     THEN
            bf-po-ordl.cons-qty = bf-po-ordl.ord-qty.
        ELSE
            RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, bf-po-ordl.cons-uom,
                v-basis-w, v-len, v-wid, v-dep,
                bf-po-ordl.ord-qty, OUTPUT bf-po-ordl.cons-qty).
      
        IF bf-po-ordl.pr-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                     AND
            LOOKUP(bf-po-ordl.pr-uom,fg-uom-list)   GT 0 AND
            LOOKUP(bf-po-ordl.cons-uom,fg-uom-list) GT 0)   THEN
            bf-po-ordl.cons-cost = bf-po-ordl.cost.
        ELSE
            RUN sys/ref/convcuom.p(bf-po-ordl.pr-uom, bf-po-ordl.cons-uom,
                v-basis-w, v-len, v-wid, v-dep,
                bf-po-ordl.cost, OUTPUT bf-po-ordl.cons-cost).





        /*FG*/
        /**************************************/
        /* Calculate oe-ordl.cost             */
        /*************************************/
        IF NOT bf-po-ordl.item-type THEN
        DO:
            FIND oe-ordl WHERE RECID(oe-ordl) EQ lv-recid EXCLUSIVE-LOCK NO-ERROR.
      
            IF AVAILABLE oe-ordl THEN
            DO:
                IF bf-po-ordl.cons-uom EQ "M" THEN
                    oe-ordl.cost = bf-po-ordl.cons-cost.
                ELSE
                    RUN sys/ref/convcuom.p (bf-po-ordl.cons-uom, "M", 0, 0, 0, 0,
                        bf-po-ordl.cons-cost, OUTPUT oe-ordl.cost).
            END. /* avail oe-ordl */
      
            FIND FIRST bf-e-itemfg-vend WHERE
                   bf-e-itemfg-vend.company EQ bf-po-ordl.company AND
                   bf-e-itemfg-vend.i-no EQ bf-po-ordl.i-no AND
                   bf-e-itemfg-vend.vend-no EQ bf-po-ord.vend-no AND
                   bf-e-itemfg-vend.est-no EQ ""
                   NO-LOCK NO-ERROR.

              IF AVAIL bf-e-itemfg-vend THEN
            DO:
                oe-ordl.cost = oe-ordl.cost * (1 + (bf-e-itemfg-vend.markup / 100.0 )).
                END. /* avail reftable */

            RELEASE oe-ordl.
        END. /* not bf-po-ordl.item-type */



    END. /* not po-ord.received  ... */
    RELEASE bf-po-ordl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createPoOrd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createPoOrd Procedure 
PROCEDURE createPoOrd :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Requires company record
        Creates PO-ord
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oplNextOuters AS LOGICAL     NO-UNDO.

    DEFINE BUFFER bf-ord FOR oe-ord.
    FIND bf-ord WHERE ROWID(bf-ord) EQ iprOeOrd NO-LOCK NO-ERROR.
    DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.

    oplNextOuters = NO.
    /* outers label is needed for po-ord.a 
       if a 'next' was done in po/po-ord.a then
       iCnt will become = 2 and routine will return */
    outers:
    REPEAT iCnt = 1 TO 2:
        IF iCnt NE 1 THEN 
        DO:
            ASSIGN 
                oplNextOuters = TRUE.
            LEAVE.
        END.
        /* Requires cocode, sets fil_id to new po-ord */
        {po/po-ord.a}
        LEAVE.
    END.
    IF oplNextOuters THEN
        RETURN.

    ASSIGN
        po-ord.po-date        = gvdPoDate
        po-ord.due-date       = gvdDueDate
        po-ord.last-ship-date = po-ord.due-date
        po-ord.vend-no        = gvcVendNo.
  
    IF AVAILABLE bf-ord THEN
        ASSIGN
            gvcDropCustNo = bf-ord.cust-no
            gvcShipChoice = "C". /* task# 09160518*/

    gvrPoOrd = ROWID(po-ord).
  
    /* Prompt for drop ship shipto and assign po-ord ship fields */
    IF ll-drop THEN 
    DO: 
        RUN askDropShip (INPUT gvrPoOrd,
            INPUT gvrWJobMat,
            INPUT gvrOeOrdl) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            MESSAGE "ASI Error message: Not able to run askDropShip"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.

    END.
    ELSE
        IF AVAILABLE company THEN
            ASSIGN
                po-ord.ship-id      = company.company
                po-ord.ship-name    = company.NAME
                po-ord.ship-addr[1] = company.addr[1]
                po-ord.ship-addr[2] = company.addr[2]
                po-ord.ship-city    = company.city
                po-ord.ship-state   = company.state
                po-ord.ship-zip     = company.zip.

    oprPoOrd = ROWID(po-ord).
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
    RELEASE po-ord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createPoOrdl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createPoOrdl Procedure 
PROCEDURE createPoOrdl :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Requires:
        item
        bf-ordl
        w-job-mat
        cocode
        v-po-best
        po-ord (for po-ordl.a)
        vend (found in po-ordl.a)
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItemfg AS ROWID NO-UNDO.

    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.

    FIND ITEM WHERE ROWID(ITEM) EQ iprItem NO-LOCK NO-ERROR.
    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND po-ord WHERE ROWID(po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
        FIND b-item WHERE RECID(b-item) EQ RECID(item) NO-LOCK.

    v-new-avail = NO.
    /* If bf-itemfg found, then this is an FG item and don't join on frm */
    /* 05281404 - added join to rm-i-no */
    FIND FIRST bf-itemfg
        WHERE bf-itemfg.company EQ bf-ordl.company
        AND bf-itemfg.i-no    EQ /* wfk - 05281404 - bf-ordl.i-no */ bf-w-job-mat.rm-i-no
        NO-LOCK NO-ERROR.

    IF gvlChoice THEN
        FIND FIRST po-ordl
            WHERE po-ordl.company    EQ cocode
            AND po-ordl.job-no     EQ bf-w-job-mat.job-no
            AND po-ordl.job-no2    EQ bf-w-job-mat.job-no2
            AND (po-ordl.s-num      EQ bf-w-job-mat.frm OR AVAIL(bf-itemfg))
            AND po-ordl.i-no       EQ bf-w-job-mat.rm-i-no
            AND po-ordl.item-type  EQ bf-w-job-mat.this-is-a-rm
            AND (bf-w-job-mat.job-no NE "" OR
            po-ordl.ord-no    EQ v-ord-no)
            NO-ERROR.
        
    IF NOT AVAILABLE po-ordl AND nk1-oeautopo-char EQ "AutoRM" AND v-autopo-sec AND bf-w-job-mat.this-is-a-rm THEN 
    DO:
        FIND FIRST po-ordl EXCLUSIVE-LOCK 
            WHERE po-ordl.company EQ cocode
            AND po-ordl.job-no  EQ bf-w-job-mat.job-no
            AND po-ordl.job-no2 EQ bf-w-job-mat.job-no2
            AND po-ordl.s-num   EQ bf-w-job-mat.frm
            AND po-ordl.i-no    EQ
            IF LENGTH(bf-w-job-mat.i-no) LE 10 THEN bf-w-job-mat.i-no
            ELSE substr(bf-w-job-mat.i-no,LENGTH(bf-w-job-mat.i-no) - 9,10)
            NO-ERROR.
        
        IF AVAILABLE po-ordl THEN 
        DO:
            FIND FIRST b-item
                WHERE b-item.company EQ cocode
                AND b-item.i-no    EQ
                IF LENGTH(bf-w-job-mat.i-no) LE 10 THEN bf-w-job-mat.i-no
                ELSE substr(bf-w-job-mat.i-no,LENGTH(bf-w-job-mat.i-no) - 9,10)
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE b-item THEN v-new-avail = YES.
            ELSE
            DO:
                IF bf-w-job-mat.prep EQ NO THEN
                DO:
                    IF v-po-best EQ NO THEN
                        FIND FIRST b-item
                            WHERE b-item.company  EQ cocode 
                            AND b-item.i-no     EQ bf-w-job-mat.rm-i-no 
                            AND index("1234BPR",b-item.mat-type) GT 0 
                            NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST b-item
                            WHERE b-item.company  EQ cocode 
                            AND b-item.i-no     EQ bf-w-job-mat.rm-i-no 
                            AND b-item.mat-type EQ "B" 
                            NO-LOCK NO-ERROR.
                END. /* if bf-w-job-mat.prep eq no */
                ELSE
                    FIND FIRST b-item
                        WHERE b-item.company  EQ cocode 
                        AND b-item.i-no     EQ bf-w-job-mat.rm-i-no
                        NO-LOCK NO-ERROR.
            END. /* not avail b-item */
        END. /* if avail po-ordl */
    END. /* Not avail po-ordl and this is RM */
  
    IF NOT AVAILABLE po-ordl THEN 
    DO:
        {po/po-ordl.a}
        ASSIGN
            po-ordl.tax       = po-ord.tax-gr NE "" AND
                         (aptax-chr EQ "Vendor" OR 
                          (aptax-chr EQ "Item" AND
                           (AVAILABLE b-item AND b-item.tax-rcpt) OR
                           (AVAILABLE itemfg AND itemfg.taxable)))
            po-ordl.item-type = bf-w-job-mat.this-is-a-rm.
    END. /* Not avail po-ordl then add it */

    IF AVAILABLE bf-itemfg THEN
        oprItemfg = ROWID(bf-itemfg).

    IF AVAILABLE po-ordl THEN
        gvrPoOrdl = ROWID(po-ordl).
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    RELEASE po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTtEiv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtEiv Procedure 
PROCEDURE createTtEiv :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprItemfg AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat
        NO-ERROR.
    FIND itemfg WHERE ROWID(itemfg) EQ iprItemfg NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN
        RETURN ERROR.

    FOR EACH e-itemfg NO-LOCK
        WHERE e-itemfg.company EQ itemfg.company
        AND e-itemfg.i-no    EQ itemfg.i-no
        :
        CREATE tt-ei.
        ASSIGN
            tt-ei.company = e-itemfg.company
            tt-ei.i-no    = e-itemfg.i-no
            tt-ei.std-uom = e-itemfg.std-uom.
        

        IF bf-w-job-mat.est-no NE "" THEN 
        DO:
            FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company  EQ itemfg.company
                AND e-itemfg-vend.est-no   EQ bf-w-job-mat.est-no
                AND e-itemfg-vend.form-no  EQ bf-w-job-mat.frm
                AND e-itemfg-vend.blank-no EQ bf-w-job-mat.blank-no
                BREAK BY e-itemfg-vend.eqty:
                   
                IF LAST(e-itemfg-vend.eqty)            OR
                    e-itemfg-vend.eqty GE bf-w-job-mat.qty THEN 
                DO:
                    FIND CURRENT bf-w-job-mat EXCLUSIVE-LOCK.
                    bf-w-job-mat.eqty = e-itemfg-vend.eqty.
                    FIND CURRENT bf-w-job-mat NO-LOCK.
                    LEAVE.
                END.
            END.
           
           
            FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company  EQ itemfg.company
                AND e-itemfg-vend.est-no   EQ bf-w-job-mat.est-no
                AND e-itemfg-vend.eqty     EQ bf-w-job-mat.eqty
                AND e-itemfg-vend.form-no  EQ bf-w-job-mat.frm
                AND e-itemfg-vend.blank-no EQ bf-w-job-mat.blank-no:
                IF NOT CAN-FIND(FIRST tt-eiv
                    WHERE tt-eiv.company   EQ e-itemfg-vend.company
                    AND tt-eiv.i-no      EQ bf-w-job-mat.i-no
                    AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                DO:
                    CREATE tt-eiv.
                    
                    ASSIGN
                        tt-eiv.rec-id    = RECID(e-itemfg-vend)
                        tt-eiv.est-no    = ""
                        tt-eiv.i-no      = bf-w-job-mat.i-no
                        tt-eiv.form-no   = 0
                        tt-eiv.blank-no  = 0
                        tt-eiv.company   = e-itemfg-vend.company
                        tt-eiv.vend-no   = e-itemfg-vend.vend-no
                        tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                        tt-eiv.item-type = e-itemfg-vend.item-type
                        tt-eiv.rec_key   = e-itemfg-vend.rec_key.
                    gvrTT-eiv = ROWID(tt-eiv).
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index]
                            tt-eiv.roll-w[v-index]   = e-itemfg-vend.roll-w[v-index].
                    END. /* do v-index ... */
                   
                    DO v-index = 11 TO 30:
                        tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                    END. /* do v-index ... */
                END. /* can-find(first tt-eiv ... */
            END. /* for each itemfg-vend */
        END. /* w-job-mat.est-no NE "" */
        
        IF NOT CAN-FIND(FIRST tt-eiv) THEN
            FOR EACH e-itemfg-vend OF e-itemfg NO-LOCK:
                IF NOT CAN-FIND(FIRST tt-eiv
                    WHERE tt-eiv.company   EQ e-itemfg-vend.company
                    AND tt-eiv.i-no      EQ e-itemfg-vend.i-no
                    AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                DO:
                    CREATE tt-eiv.
                    ASSIGN
                        tt-eiv.rec-id    = RECID(e-itemfg-vend)
                        tt-eiv.est-no    = e-itemfg-vend.est-no
                        tt-eiv.i-no      = e-itemfg-vend.i-no
                        tt-eiv.form-no   = e-itemfg-vend.form-no
                        tt-eiv.blank-no  = e-itemfg-vend.blank-no
                        tt-eiv.company   = e-itemfg-vend.company
                        tt-eiv.vend-no   = e-itemfg-vend.vend-no
                        tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                        tt-eiv.item-type = e-itemfg-vend.item-type
                        tt-eiv.rec_key   = e-itemfg-vend.rec_key.
                    gvrTT-eiv = ROWID(tt-eiv).
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index]
                            tt-eiv.roll-w[v-index]   = e-itemfg-vend.roll-w[v-index].
                    END.
          
                    DO v-index = 11 TO 30:
                        tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                    END.
                END. /* not can-find .. */
            END. /* each e-itemfg-vend */
    END. /* each e-itemfg */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTtEivItemfg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtEivItemfg Procedure 
PROCEDURE createTtEivItemfg :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        cocode
        w-job-mat
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.

    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat
        NO-ERROR.


    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCocode
        AND itemfg.i-no    EQ bf-w-job-mat.rm-i-no
        NO-ERROR.

    IF AVAILABLE itemfg THEN
        FOR EACH e-itemfg
            WHERE e-itemfg.company EQ itemfg.company
            AND e-itemfg.i-no    EQ itemfg.i-no
            NO-LOCK:
            CREATE tt-ei.
            ASSIGN
                tt-ei.company = e-itemfg.company
                tt-ei.i-no    = e-itemfg.i-no
                tt-ei.std-uom = e-itemfg.std-uom.


            IF bf-w-job-mat.est-no NE "" THEN 
            DO:
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company  EQ itemfg.company
                    AND e-itemfg-vend.est-no   EQ bf-w-job-mat.est-no
                    AND e-itemfg-vend.form-no  EQ bf-w-job-mat.frm
                    AND e-itemfg-vend.blank-no EQ bf-w-job-mat.blank-no
                    BREAK BY e-itemfg-vend.eqty:
                    IF LAST(e-itemfg-vend.eqty)            OR
                        e-itemfg-vend.eqty GE bf-w-job-mat.qty THEN 
                    DO:
                        FIND CURRENT bf-w-job-mat EXCLUSIVE-LOCK.
                        bf-w-job-mat.eqty = e-itemfg-vend.eqty.
                        FIND CURRENT bf-w-job-mat NO-LOCK.
                        LEAVE.
                    END.
                END.
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company  EQ itemfg.company
                    AND e-itemfg-vend.est-no   EQ bf-w-job-mat.est-no
                    AND e-itemfg-vend.eqty     EQ bf-w-job-mat.eqty
                    AND e-itemfg-vend.form-no  EQ bf-w-job-mat.frm
                    AND e-itemfg-vend.blank-no EQ bf-w-job-mat.blank-no:
                    IF NOT CAN-FIND(FIRST tt-eiv
                        WHERE tt-eiv.company   EQ e-itemfg-vend.company
                        AND tt-eiv.i-no      EQ bf-w-job-mat.i-no
                        AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                    DO:
                        CREATE tt-eiv.

                        ASSIGN
                            tt-eiv.rec-id    = RECID(e-itemfg-vend)
                            tt-eiv.est-no    = ""
                            tt-eiv.i-no      = bf-w-job-mat.i-no
                            tt-eiv.form-no   = 0
                            tt-eiv.blank-no  = 0
                            tt-eiv.company   = e-itemfg-vend.company
                            tt-eiv.vend-no   = e-itemfg-vend.vend-no
                            tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                            tt-eiv.item-type = e-itemfg-vend.item-type
                            tt-eiv.rec_key   = e-itemfg-vend.rec_key.

                        DO v-index = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                                tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                                tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index]
                                tt-eiv.roll-w[v-index]   = e-itemfg-vend.roll-w[v-index].
                        END. /* do v-index ... */

                        DO v-index = 11 TO 30:
                            tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                        END. /* do v-index ... */
                    END. /* can-find(first tt-eiv ... */
                END. /* for each itemfg-vend */
            END. /* bf-w-job-mat.est-no NE "" */

            IF NOT CAN-FIND(FIRST tt-eiv) THEN
                FOR EACH e-itemfg-vend OF e-itemfg NO-LOCK:
                    IF NOT CAN-FIND(FIRST tt-eiv
                        WHERE tt-eiv.company   EQ e-itemfg-vend.company
                        AND tt-eiv.i-no      EQ e-itemfg-vend.i-no
                        AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                    DO:
                        CREATE tt-eiv.
                        ASSIGN
                            tt-eiv.rec-id    = RECID(e-itemfg-vend)
                            tt-eiv.est-no    = e-itemfg-vend.est-no
                            tt-eiv.i-no      = e-itemfg-vend.i-no
                            tt-eiv.form-no   = e-itemfg-vend.form-no
                            tt-eiv.blank-no  = e-itemfg-vend.blank-no
                            tt-eiv.company   = e-itemfg-vend.company
                            tt-eiv.vend-no   = e-itemfg-vend.vend-no
                            tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                            tt-eiv.item-type = e-itemfg-vend.item-type
                            tt-eiv.rec_key   = e-itemfg-vend.rec_key.

                        DO v-index = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[v-index]  = e-itemfg-vend.run-qty[v-index]
                                tt-eiv.run-cost[v-index] = e-itemfg-vend.run-cost[v-index]
                                tt-eiv.setups[v-index]   = e-itemfg-vend.setups[v-index]
                                tt-eiv.roll-w[v-index]   = e-itemfg-vend.roll-w[v-index].
                        END.

                        DO v-index = 11 TO 30:
                            tt-eiv.roll-w[v-index] = e-itemfg-vend.roll-w[v-index].
                        END.
                    END. /* not can-find .. */
                END. /* each e-itemfg-vend */
        END. /* each e-itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTtEivVend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtEivVend Procedure 
PROCEDURE createTtEivVend :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      input w-job-mat
            v-po-best
            cocode
            
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplPoBest AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem AS ROWID         NO-UNDO.
  
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprJobMat
        NO-ERROR.
    IF gvlDebug THEN             
        PUT STREAM sDebug UNFORMATTED "createTtEiv-from-item-vend " bf-w-job-mat.i-no  SKIP.
  
    RELEASE ITEM.

    IF bf-w-job-mat.prep EQ NO THEN
    DO:
        IF iplPoBest EQ NO THEN
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipcCocode
                AND item.i-no     EQ bf-w-job-mat.rm-i-no
                AND index("1234BPR",item.mat-type) GT 0
                NO-ERROR.
        ELSE
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipcCocode
                AND item.i-no     EQ bf-w-job-mat.rm-i-no
                AND item.mat-type EQ "B"
                NO-ERROR.
    END. /* If bf-w-job-mat.prep EQ NO */
    ELSE
        FIND FIRST ITEM NO-LOCK
            WHERE item.company  EQ ipcCocode
            AND item.i-no     EQ bf-w-job-mat.rm-i-no
            NO-ERROR.

    IF AVAILABLE item THEN
        FOR EACH e-item NO-LOCK OF item :
            CREATE tt-ei.
            ASSIGN
                tt-ei.company = e-item.company
                tt-ei.i-no    = e-item.i-no
                tt-ei.std-uom = e-item.std-uom.
     
            FOR EACH e-item-vend OF e-item NO-LOCK:
                IF NOT CAN-FIND(FIRST tt-eiv
                    WHERE tt-eiv.company   EQ e-item-vend.company
                    AND tt-eiv.i-no      EQ e-item-vend.i-no
                    AND tt-eiv.vend-no   EQ e-item-vend.vend-no) THEN 
                DO:
                    CREATE tt-eiv.
                    ASSIGN
                        tt-eiv.rec-id    = RECID(e-item-vend)
                        tt-eiv.company   = e-item-vend.company
                        tt-eiv.vend-no   = e-item-vend.vend-no
                        tt-eiv.i-no      = e-item-vend.i-no
                        tt-eiv.est-no    = e-item-vend.est-no
                        tt-eiv.form-no   = e-item-vend.form-no
                        tt-eiv.blank-no  = e-item-vend.blank-no
                        tt-eiv.item-type = e-item-vend.item-type
                        tt-eiv.vend-i-no = e-item-vend.vend-item
                        tt-eiv.rec_key   = e-item-vend.rec_key.
            
                    DO v-index = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[v-index]  = e-item-vend.run-qty[v-index]
                            tt-eiv.run-cost[v-index] = e-item-vend.run-cost[v-index]
                            tt-eiv.setups[v-index]   = e-item-vend.setups[v-index]
                            tt-eiv.roll-w[v-index]   = e-item-vend.roll-w[v-index].
                    END.
            
                    DO v-index = 11 TO 30:
                        tt-eiv.roll-w[v-index] = e-item-vend.roll-w[v-index].
                    END.
            

            
                    IF AVAILABLE e-item-vend THEN
                    DO:

             
                        DO v-index = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[v-index + 10]  = e-item-vend.runQtyXtra[v-index]
                                tt-eiv.run-cost[v-index + 10] = e-item-vend.runCostXtra[v-index]
                                tt-eiv.setups[v-index + 10]   = e-item-vend.setupsXtra[v-index].
                        END. /* v-index = 1 to 10 */
                    END. /* if avail b-qty */
                END. /* if tt-eiv doesn't already exist */
            END. /* for each e-item-vend */
        END. /* for each e-item */
    IF AVAILABLE ITEM THEN
        oprItem = ROWID(ITEM).

END PROCEDURE. /* end createTtEivVend */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-findExistingPo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findExistingPo Procedure 
PROCEDURE findExistingPo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          b-orderpo
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  iprBOrderPo AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailPO AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd AS ROWID       NO-UNDO.

    FIND b-orderpo WHERE ROWID(b-orderpo) EQ iprBOrderPo NO-LOCK NO-ERROR.

    FIND LAST po-ord NO-LOCK 
        WHERE po-ord.company   EQ cocode
        AND po-ord.po-date   EQ gvdPoDate
        AND po-ord.due-date  EQ gvdDueDate
        AND po-ord.po-no     EQ INT(b-orderpo.val[1])
        AND b-orderpo.val[1] NE 0
        AND po-ord.vend-no   EQ gvcVendNo
        AND po-ord.opened    EQ YES
        AND (po-ord.type     EQ "D" OR NOT ll-drop)
        NO-ERROR.
     
    IF NOT AVAILABLE po-ord THEN 
    DO:
        FIND po-ord NO-LOCK
            WHERE po-ord.company  EQ cocode
            AND po-ord.due-date EQ gvdDueDate
            AND po-ord.vend-no  EQ gvcVendNo
            AND po-ord.opened   EQ YES
            AND (po-ord.type    EQ "D" OR NOT ll-drop)
            NO-ERROR.

        gvlChoice = AMBIGUOUS po-ord.

        IF gvlChoice THEN
            RUN windows/l-povndt.w (cocode, gvcVendNo, gvdDueDate, BUFFER po-ord).
        oplAvailPo = NO.
    END.
    ELSE oplAvailPo = YES.
    IF AVAILABLE po-ord THEN
        oprPoOrd = ROWID(po-ord).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-findOrderFromRecid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findOrderFromRecid Procedure 
PROCEDURE findOrderFromRecid :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        fil_id
        po-found - this is not set anywhere
      Outputs:
        oe-ordl buffer    
        sets v-ord-no
        creates tt-fg-set
        create tt-itemfg records
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprFilId  AS RECID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeOrdl AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprJob AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilIdSource AS CHARACTER   NO-UNDO.

    FIND bf-ordl WHERE RECID(bf-ordl) EQ iprFilId NO-LOCK NO-ERROR.

    /* wfk - Build tt-itemfg based on job */
    IF AVAILABLE bf-ordl THEN 
    DO:
        /* bf-itemfg holds buffer for item of bf-ordl since itemfg is used for set components below */
        opcFilIdSource = "oe-ordl".
        FIND FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ bf-ordl.company
            AND bf-itemfg.i-no    EQ bf-ordl.i-no
            NO-ERROR.
        v-ord-no = bf-ordl.ord-no.

        FIND FIRST bf-ord NO-LOCK 
            WHERE bf-ord.company EQ bf-ordl.company
            AND bf-ord.ord-no  EQ bf-ordl.ord-no
            AND bf-ord.opened  EQ YES
            AND bf-ord.stat    NE "H"
            NO-ERROR.

        IF AVAILABLE bf-ord THEN 
        DO:
            IF TRIM(bf-ordl.job-no) NE "" THEN
                FIND FIRST job NO-LOCK 
                    WHERE job.company EQ cocode
                    AND job.job-no  EQ bf-ordl.job-no
                    AND job.job-no2 EQ bf-ordl.job-no2
                    NO-ERROR.
            gvrOeOrd = ROWID(bf-ord).
            /* wfk - po-found was leftover in error from task 05281404       */
            /* and should probably be removed. This needs more investigation */
            IF NOT AVAILABLE job OR NOT po-found THEN 
            DO:
         
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ bf-ordl.company
                    AND itemfg.i-no    EQ bf-ordl.i-no
                    NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    IF itemfg.isaset THEN 
                    DO:
                        RUN fg/fullset.p (ROWID(itemfg)).
       
                        RELEASE itemfg.
       
                        FOR EACH tt-fg-set,
                            FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ bf-ordl.company
                            AND itemfg.i-no    EQ tt-fg-set.part-no:
                            IF gvlDebug THEN
                                PUT STREAM sDebug UNFORMATTED "Create tt-itemfg for set part " tt-fg-set.part-no SKIP.
                            CREATE tt-itemfg.
                            BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                                ASSIGN
                                tt-itemfg.isacomponent = TRUE
                                tt-itemfg.form-no  = 0
                                tt-itemfg.blank-no = 0
                                tt-itemfg.qty      = bf-ordl.qty * tt-fg-set.part-qty-dec
                                tt-itemfg.pur-uom  = "EA"
                                tt-itemfg.row-id   = ROWID(itemfg).
              
                            /* WFK - 06051407 */
                            FIND FIRST eb NO-LOCK WHERE
                                eb.company EQ bf-ordl.company AND
                                eb.est-no EQ bf-ordl.est-no AND
                                eb.stock-no EQ tt-fg-set.part-no AND
                                eb.blank-no EQ tt-itemfg.blank-no AND
                                eb.form-no EQ tt-itemfg.form-no
                                NO-ERROR.

                            IF AVAILABLE eb THEN
                                tt-itemfg.pur-man = eb.pur-man.
                        END. /* Each tt-fg-set */
                    END. /* If itemfg.isaset */
       
                    ELSE 
                    DO:
                        IF gvlDebug THEN
                            PUT STREAM sDebug UNFORMATTED "Create tt-itemfg for FG " itemfg.i-no SKIP.

                        CREATE tt-itemfg.
                        BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                            ASSIGN
                            tt-itemfg.form-no  = bf-ordl.form-no
                            tt-itemfg.blank-no = bf-ordl.blank-no
                            tt-itemfg.qty      = bf-ordl.qty
                            tt-itemfg.pur-uom  = "EA"
                            tt-itemfg.row-id   = ROWID(itemfg).
                        FIND FIRST eb NO-LOCK WHERE
                            eb.company EQ bf-ordl.company AND
                            eb.est-no EQ bf-ordl.est-no AND
                            eb.stock-no EQ bf-ordl.i-no AND
                            eb.blank-no EQ bf-ordl.blank-no AND
                            eb.form-no EQ bf-ordl.form-no
                            NO-ERROR.

                        IF AVAILABLE eb THEN
                            tt-itemfg.pur-man = eb.pur-man.
                    END. /* If Not Itemfg.isaset */
                END. /* If avail itemfg */
            END. /* If not avail job ... */
        END. /* If avail bf-ord */
    END. /* If avail bf-ordl */

    ELSE 
    DO:
        /* Oe-ordl not available */
        FIND job WHERE RECID(job) EQ iprFilId NO-LOCK NO-ERROR.
        opcFilIdSource = "JOB".
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.ord-no  NE 0:
            FIND FIRST bf-ord NO-LOCK
                WHERE bf-ord.company EQ cocode
                AND bf-ord.ord-no  EQ job-hdr.ord-no
                AND bf-ord.opened  EQ YES
                AND bf-ord.stat    NE "H"
                NO-ERROR.
            IF NOT AVAILABLE bf-ord THEN 
            DO:
                RELEASE job.
                LEAVE.
            END. /* not avail bf-ord */
    
            gvrOeOrd = ROWID(bf-ord).

            FIND FIRST bf-ordl NO-LOCK 
                WHERE bf-ordl.company EQ job-hdr.company
                AND bf-ordl.ord-no  EQ job-hdr.ord-no
                AND bf-ordl.i-no    EQ job-hdr.i-no
                NO-ERROR.
            IF AVAILABLE bf-ordl THEN LEAVE.
        END. /* each job-hdr */

    END. /* bf-ordl not avail */

    IF AVAILABLE bf-ordl THEN 
    DO:

        oprOeOrdl = ROWID(bf-ordl).
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ bf-ordl.company
            AND oe-ord.ord-no EQ bf-ordl.ord-no
            NO-ERROR.
        IF AVAILABLE oe-ord THEN
            gvrOeOrd = ROWID(oe-ord).
  
    END. /* avail bf-ordl */

    IF AVAILABLE job THEN
        oprJob = ROWID(job).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getItemfgGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getItemfgGL Procedure 
PROCEDURE getItemfgGL :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/


    DEFINE INPUT PARAMETER ip-comp LIKE job-hdr.company.
    DEFINE INPUT PARAMETER ip-i-no LIKE itemfg.i-no.
    DEFINE OUTPUT PARAMETER out-actnum LIKE po-ordl.actnum.

    DEFINE VARIABLE v-charge AS CHARACTER NO-UNDO.
     
    /* populate GL# from reftable if it exists using itemfg AH 02-23-10 */

    FIND itemfg NO-LOCK WHERE itemfg.company = ip-comp
        AND itemfg.i-no = ip-i-no NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:

        ASSIGN 
            v-charge = "".
        FIND FIRST surcharge NO-LOCK WHERE surcharge.company = ip-comp
            AND surcharge.charge <> "" NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN v-charge = surcharge.charge.
        FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ itemfg.company
            AND reftable.loc      EQ itemfg.procat
            AND reftable.code     EQ v-charge
            /* AND reftable.code2 = "" */
            NO-ERROR.

        IF AVAILABLE reftable AND reftable.code2 <> "" THEN 
            ASSIGN out-actnum = reftable.code2.

        RELEASE reftable.
    END. /* avail itemfg */
    RELEASE itemfg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initJobVals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initJobVals Procedure 
PROCEDURE initJobVals :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      inputs:
        job
        w-job-mat
        assigns fil_id (global)
        cocode
        assigns v-ord-no (global)
        bf-ordl
        finds po-ord
        gvcVendNo
        v-job
        assigns gvrJobRecid (used in assign po-ordl values)
        assigns v-ord-no
        releases po-ord, po-ordl, b-orderpo
        finds po-ordl
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilId AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJob AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendNo AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iPono AS INTEGER NO-UNDO .
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bff-job-mat FOR job-mat .

    FIND bf-w-job-mat WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    ASSIGN 
        gvrJobRecid = ?.

    IF AVAILABLE job THEN
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            BREAK BY job-hdr.frm      DESCENDING
            BY job-hdr.blank-no DESCENDING:
            IF (job-hdr.frm EQ bf-w-job-mat.frm AND
                (job-hdr.blank-no EQ bf-w-job-mat.blank-no OR
                bf-w-job-mat.blank-no EQ 0)) OR
                LAST(job-hdr.blank-no) THEN 
            DO:
          
                ASSIGN 
                    v-ord-no    = job-hdr.ord-no
                    gvrJobRecid = RECID(job-hdr).
                LEAVE.
            END.
        END. /* each job-hdr */

    /*once out of loop above, not pointing to correct job-hdr*/
    IF gvrJobRecid NE ? THEN
        FIND job-hdr NO-LOCK
            WHERE RECID(job-hdr) EQ gvrJobRecid
            .

    ASSIGN
        sel        = 1
        fil_id     = ?
        v-sname    = ""
        v-saddr[1] = ""
        v-saddr[2] = ""
        v-scity    = ""
        v-sstate   = ""
        v-szip     = ""
        call_id    = fil_id
        nufile     = YES
        v-neword   = YES
        gvcVendNo  = ""
        .
  
    RELEASE po-ord.
    RELEASE po-ordl.
    RELEASE b-orderpo.

    rOrderPoRow = ?.
    /* 05281404 - added first po-ordl to check item # */
    
       FOR EACH po-ord NO-LOCK WHERE po-ord.company EQ cocode
        AND po-ord.po-no EQ INTEGER(bf-w-job-mat.po-no)
        AND po-ord.stat NE "C"
        ,
        FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no EQ po-ord.po-no
        AND po-ordl.i-no  EQ bf-w-job-mat.rm-i-no
        AND po-ordl.s-num EQ bf-w-job-mat.frm
      
        :
        iPono = bf-w-job-mat.po-no .
    
    END. /* Each b-orderpo */

    IF iPono EQ 0 THEN 
    DO:
        IF AVAILABLE bf-ordl AND bf-ordl.vend-no NE "" AND bf-ordl.po-no-po NE 0 THEN 
        DO:
            FIND FIRST po-ordl NO-LOCK 
                WHERE po-ordl.company EQ cocode
                AND po-ordl.po-no   EQ bf-ordl.po-no-po
                AND po-ordl.job-no  EQ bf-w-job-mat.job-no
                AND po-ordl.job-no2 EQ bf-w-job-mat.job-no2
                AND po-ordl.s-num   EQ bf-w-job-mat.frm
                AND po-ordl.i-no    EQ bf-w-job-mat.rm-i-no
                NO-ERROR.
            IF AVAILABLE po-ordl THEN 
            DO:         
                iPono = po-ordl.po-no.   
                /* update job-mat table with pono*/
                FIND FIRST bff-job-mat EXCLUSIVE-LOCK
                    WHERE bff-job-mat.company EQ cocode
                    AND ROWID(bff-job-mat) EQ bf-w-job-mat.w-rowid NO-ERROR .
                IF AVAIL bff-job-mat THEN
                    bff-job-mat.po-no = po-ordl.po-no.
            END.
        END. /* If avail bf-ordl */
    END. /* If not avail b-orderpo */

    FIND FIRST po-ord NO-LOCK 
        WHERE po-ord.company   EQ cocode
        AND po-ord.po-no     EQ int(iPono)
        AND b-orderpo.val[1] NE 0
        NO-ERROR.
    IF AVAILABLE po-ord THEN gvcVendNo = po-ord.vend-no.  

    IF AVAILABLE job THEN 
        ASSIGN
            v-job = job.job-no
            v-job = FILL(" ",6 - length(TRIM(v-job))) + trim(v-job) +
              "-" + string(job.job-no2,"99").

    opcJob    = v-job.
    opcVendNo = gvcVendNo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initRptRecs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initRptRecs Procedure 
PROCEDURE initRptRecs :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      inputs:
        cocode
        writes to w-job-mat
        updates w-job-mat.qty
        GLOBALS:
        outputs v-job-mat-qty
        outputs v-job-mat-uom
        outputs v-uom-comp
        outputs v-qty-comp
        outputs v-qty-comp1
        outputs v-len
        outputs v-wid
        outputs v-dep
        outputs v-basis-w
        outputs ll-canceled
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprTT-ei AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.


    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.

    ASSIGN
        v-len       = bf-w-job-mat.len
        v-wid       = bf-w-job-mat.wid
        v-dep       = bf-w-job-mat.dep
        v-basis-w   = bf-w-job-mat.basis-w
        ll-canceled = NO.

    FIND FIRST tt-ei
        WHERE tt-ei.company EQ cocode
        AND tt-ei.i-no    EQ bf-w-job-mat.rm-i-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE tt-ei THEN 
    DO:
        oprTT-ei = ROWID(tt-ei).
        IF bf-w-job-mat.qty-uom EQ "BF" THEN 
        DO:
            RUN sys/ref/convquom.p(bf-w-job-mat.qty-uom, "EA",
                v-basis-w, v-len, v-wid, v-dep,
                bf-w-job-mat.qty, OUTPUT v-job-mat-qty).

            {sys/inc/roundup.i v-job-mat-qty}
            v-job-mat-uom = "EA".
        END. /* bf-w-job-mat.qty-uom eq "Bf */

        ELSE
            ASSIGN
                v-job-mat-uom = bf-w-job-mat.qty-uom
                v-job-mat-qty = bf-w-job-mat.qty.

        v-uom-comp = IF bf-w-job-mat.this-is-a-rm THEN
            IF CAN-DO("1,2,3,4",item.mat-type) THEN "BF" ELSE "MSF"
            ELSE tt-ei.std-uom.

        IF v-job-mat-uom EQ v-uom-comp                 OR
            (NOT bf-w-job-mat.this-is-a-rm             AND
            LOOKUP(v-job-mat-uom,fg-uom-list) GT 0 AND
            LOOKUP(v-uom-comp,fg-uom-list)    GT 0)    THEN
            v-qty-comp = v-job-mat-qty.
        ELSE
            RUN sys/ref/convquom.p(v-job-mat-uom, v-uom-comp,
                v-basis-w, v-len, v-wid, v-dep,
                v-job-mat-qty, OUTPUT v-qty-comp).

        IF bf-w-job-mat.job-no NE "" THEN
            RUN po/groupcst.p (bf-w-job-mat.job-no,
                bf-w-job-mat.job-no2,
                bf-w-job-mat.rm-i-no,
                bf-w-job-mat.frm,
                bf-w-job-mat.blank-no,
                INPUT-OUTPUT v-qty-comp).

        v-uom-comp = "TON".

        IF v-job-mat-uom EQ v-uom-comp                 OR
            (NOT bf-w-job-mat.this-is-a-rm             AND
            LOOKUP(v-job-mat-uom,fg-uom-list) GT 0 AND
            LOOKUP(v-uom-comp,fg-uom-list)    GT 0)    THEN
            v-qty-comp1 = v-job-mat-qty.
        ELSE
            RUN sys/ref/convquom.p(v-job-mat-uom, v-uom-comp,
                v-basis-w, v-len, v-wid, v-dep,
                v-job-mat-qty, OUTPUT v-qty-comp1).
    END. /* Avail tt-ei */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-poOrdlAddVals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE poOrdlAddVals Procedure 
PROCEDURE poOrdlAddVals :
    /*------------------------------------------------------------------------------
      Purpose:      Assign more values to bf-po-ordl if a RM
      Parameters:  <none>
      Notes:       
        Inputs:
          bf-po-ordl
          tt-ei
          bf-ordl
          w-job-mat
          b-item
          pouom-int
          nk1-oeautopo-char, v-autopo-sec, v-new-avail
        Outputs:
          sets fil_id
          runs rm/;itemcopy.p
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER iprPoOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprTT-ei AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER b-item       FOR ITEM .
    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-tt-ei     FOR tt-ei.

    FIND bf-po-ordl   EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl)   EQ iprPoOrdl NO-ERROR.
    FIND b-item       NO-LOCK WHERE ROWID(b-item)       EQ iprItem NO-ERROR.
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl      NO-LOCK WHERE ROWID(bf-ordl)      EQ iprOeOrdl NO-ERROR.  
    FIND bf-tt-ei     NO-LOCK WHERE ROWID(bf-tt-ei)     EQ iprTT-ei NO-ERROR.

    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        MESSAGE "Error: PO line not available in poOrdlAddVals"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN ERROR.
    END.
    IF NOT AVAILABLE b-item THEN 
    DO:
        MESSAGE "Error: item record not available in poOrdlAddVals"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN ERROR.
    END.

    IF bf-po-ordl.item-type THEN 
    DO:
        IF AVAILABLE bf-tt-ei THEN 
        DO:
            IF (bf-tt-ei.std-uom EQ "MSF" OR bf-tt-ei.std-uom EQ "EA" OR
                bf-tt-ei.std-uom EQ "M" OR bf-tt-ei.std-uom EQ "MSH"  OR
                bf-tt-ei.std-uom EQ "BF") THEN
                bf-po-ordl.cons-uom = "EA".
            ELSE
                bf-po-ordl.cons-uom = bf-tt-ei.std-uom.
      
            bf-po-ordl.pr-uom = bf-tt-ei.std-uom.

            IF AVAILABLE bf-ordl AND bf-po-ordl.item-type THEN 
            DO:
                /* Adding code to create new RM from Estimated RM */
                ASSIGN
                    v-new-i-no = IF LENGTH(bf-ordl.i-no) LE 10 THEN bf-ordl.i-no
                      ELSE SUBSTRING(bf-ordl.i-no,(LENGTH(bf-ordl.i-no) - 9),10)
                    v-new-len  = bf-w-job-mat.len
                    v-new-wid  = bf-w-job-mat.wid.
                       
                IF nk1-oeautopo-char EQ "AutoRM" AND v-autopo-sec AND NOT v-new-avail THEN 
                DO:         
                    fil_id = RECID(b-item).
                    RUN rm/itemcopy.p.   /* not done */
                END. /* format eq "AutoRM" ... */
            END. /* avail bf-ordl ... */
        END. /* AVail bf-tt-ei */

        IF pouom-int EQ 1 AND b-item.mat-type EQ "P" THEN bf-po-ordl.cons-uom = "TON".
    END. /* bf-po-ordl.item-type = yes */

    RELEASE bf-po-ordl.
    RELEASE b-item.
    RELEASE bf-w-job-mat.
    RELEASE bf-ordl.
    RELEASE bf-tt-ei.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PoOrdlFinal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PoOrdlFinal Procedure 
PROCEDURE PoOrdlFinal :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Requires:
          po-ordl
        Needs the UI to be removed
    
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.


    DEFINE BUFFER bf-po-ordl FOR po-ordl.  

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.

    EMPTY TEMP-TABLE tt-ref1.

    {po/po-ordls.i bf-}

    IF NOT bf-po-ordl.item-type THEN
        RUN jc/writeFarmFromPO.p (ROWID(bf-po-ordl), bf-po-ordl.job-no, STRING(bf-po-ordl.job-no2),
            STRING(bf-po-ordl.ord-no), bf-po-ordl.i-no, STRING(bf-po-ordl.s-num),
            STRING(bf-po-ordl.b-num)).


    /********************************************/
    /* Process Scoring Changes ******************/
    /* find on b-ref1 is not in the program     */
    /********************************************/
    IF AVAILABLE b-ref1 THEN 
    DO:
        CREATE tt-ref1.
        BUFFER-COPY b-ref1 TO tt-ref1.
        DELETE b-ref1.
    END.

    IF AVAILABLE b-ref2 THEN 
    DO:
        CREATE tt-ref2.
        BUFFER-COPY b-ref2 TO tt-ref2.
        DELETE b-ref2.
    END.

    RUN po/po-ordls.p (RECID(bf-po-ordl)).

    IF AVAILABLE tt-ref1 OR AVAILABLE tt-ref2 THEN 
    DO:
        {po/po-ordls.i}

        ll = NO.
        IF AVAILABLE b-ref1 AND AVAILABLE tt-ref1 THEN 
        DO li = 1 TO EXTENT(b-ref1.val):
            IF b-ref1.val[li] NE tt-ref1.val[li] THEN ll = YES.
            IF ll THEN LEAVE.
        END. /* avail b-ref1 ... */
        IF NOT ll                         AND
            AVAILABLE b-ref2 AND AVAILABLE tt-ref2 THEN 
        DO li = 1 TO EXTENT(b-ref2.val):
            IF b-ref2.val[li] NE tt-ref2.val[li] THEN ll = YES.
            IF ll THEN LEAVE.
        END. /* Not ll */

        IF ll THEN 
        DO:
            ll = NO.
            MESSAGE "Scoring allowances have changed for PO/RM#: " +
                TRIM(STRING(bf-po-ordl.po-no,">>>>>>>>"))         +
                "/" + bf-po-ordl.i-no                             +
                ", would you like to apply the new scores?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll.
        END. /* If ll */

        IF NOT ll THEN 
        DO:
            IF AVAILABLE b-ref1 AND AVAILABLE tt-ref1 THEN BUFFER-COPY tt-ref1 TO b-ref1.
            IF AVAILABLE b-ref2 AND AVAILABLE tt-ref2 THEN BUFFER-COPY tt-ref2 TO b-ref2.
        END. /* not ll */

    END. /* AVail tt-ref1 */
    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processAdders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processAdders Procedure 
PROCEDURE processAdders :
    /*------------------------------------------------------------------------------
      Purpose:     processAdders   
      Parameters:  <none>
      Notes:       
        Inputs:
          w-job-mat
          b-item
          po-ordl (updated)
          globals: v-pocost1, v-op-type nufile, (many in po-vendc.i )
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl   FOR po-ordl.  
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
     
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.

    FIND job-mat NO-LOCK WHERE RECID(job-mat) EQ bf-w-job-mat.w-recid NO-ERROR.

    IF AVAILABLE job-mat                        AND 
        AVAILABLE b-item                         AND
     index("1234BPR",b-item.mat-type) GT 0 AND
        b-item.i-code   EQ "E"               THEN 
        RUN po/po-adder.p (RECID(bf-po-ordl), RECID(job-mat)).
    
    /* needed for po-vendc.i */
    FIND po-ordl EXCLUSIVE-LOCK WHERE ROWID(po-ordl) EQ ROWID(bf-po-ordl) .
    FIND po-ord EXCLUSIVE-LOCK WHERE ROWID(po-ord) EQ ROWID(bf-po-ord) .
    /* updates v-cost, v-qty v-setup, po-ordl.cost, po-ordl.cons-cost, v-adder[] */
    /* Needs po-ord, po-ordl, cocode, tt-eiv, tt-ei */
    IF (v-pocost1 BEGINS "Vendor" OR po-ordl.job-no EQ "") AND
        v-op-type                                          AND
        nufile                                             THEN
        {po/po-vendc.i}
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    FIND CURRENT po-ord NO-LOCK NO-ERROR.
    RELEASE po-ordl.
    RELEASE po-ord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessExisting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessExisting Procedure 
PROCEDURE ProcessExisting :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstOfFrm AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.

    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ iprPoOrd NO-ERROR.

    IF NOT gvlChoice THEN 
    DO:
        /****************************************/
        /** Chose not to update the existing PO */
        /****************************************/
        gvlChoice = NO.
  
        IF gvcVendNo EQ "" THEN 
        DO:
            IF AVAILABLE tt-eiv THEN
                FIND FIRST po-ord NO-LOCK
                    WHERE po-ord.company  EQ ipcCocode
                    AND po-ord.po-date  EQ gvdPoDate
                    AND po-ord.due-date EQ gvdDueDate
                    AND po-ord.vend-no  EQ tt-eiv.vend-no
                    AND (po-ord.type     EQ "D" OR NOT ll-drop)
                    NO-ERROR.
  
            ELSE 
            DO: /* not avail tt-eiv */
  
                FIND FIRST vend  WHERE vend.company EQ ipcCocode
                    AND vend.vend-no EQ gvcVendNo
                    NO-LOCK NO-ERROR.
                IF AVAILABLE vend THEN 
                DO:
                    gvcVendNo = vend.vend-no.
  
                    IF AVAILABLE bf-ordl AND iplFirstOfFrm THEN
                        FOR EACH b-oe-ordl
                            WHERE b-oe-ordl.company EQ bf-ordl.company
                            AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
                            AND b-oe-ordl.job-no  EQ bf-ordl.job-no
                            AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
                            AND b-oe-ordl.i-no    EQ bf-w-job-mat.fg-i-no:
                            ASSIGN
                                b-oe-ordl.po-no-po = po-ord.po-no
                                b-oe-ordl.vend-no  = gvcVendNo.
                        END. /* each b-oe-ordl */
                END. /* avail vend */
            END. /* else do */
  
            FIND FIRST po-ord NO-LOCK
                WHERE po-ord.company  EQ ipcCocode
                AND po-ord.po-date  EQ gvdPoDate
                AND po-ord.due-date EQ gvdDueDate
                AND po-ord.vend-no  EQ gvcVendNo
                AND (po-ord.type     EQ "D" OR NOT ll-drop)
                NO-ERROR.
        END. /* gvcVendNo eq "" */
    END.  /* not gvlChoice (chose not to update the PO */ 

    ELSE 
    DO:
        /***********************************/
        /* Chose to update the existing PO */
        /***********************************/
        ASSIGN
            gvdPoDate  = po-ord.po-date
            gvdDueDate = po-ord.due-date.
    END. /* if gvlChoice */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processJobMat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processJobMat Procedure 
PROCEDURE processJobMat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    outers:
    FOR EACH w-job-mat    
        WHERE (IF iplPromptRM THEN TRUE ELSE w-job-mat.this-is-a-rm EQ FALSE)
        BREAK BY w-job-mat.this-is-a-rm
        BY w-job-mat.frm
        BY w-job-mat.blank-no
        BY w-job-mat.i-no:

        ASSIGN 
            gvcVendNo      = ""
            gvrPoOrdl      = ?
            gvrItem        = ?
            gvrVend        = ?
            gvrPoOrd       = ?
            gvrTT-eiv      = ?
            gvrTT-ei       = ?
            gvrItemfg      = ?
            gvcFilIdSource = ? 
            gvrWJobMat     = ?.

        IF NOT w-job-mat.this-is-a-rm THEN 
        DO:
            FIND itemfg NO-LOCK WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ w-job-mat.i-no 
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                gvrItemfg = ROWID(itemfg).
        END.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF gvlDebug THEN
            PUT STREAM sDebug UNFORMATTED "Process Item " + w-job-mat.i-no SKIP.
        lNextOuters = NO.

        EMPTY TEMP-TABLE tt-ei.
        EMPTY TEMP-TABLE tt-eiv.

        v-vendor-chosen-report = ?.
        gvrWJobMat = ROWID(w-job-mat).

        llFirstOfJobFrm = FIRST-OF(w-job-mat.frm).
        llFirstJobFrm = FIRST(w-job-mat.frm).
  
        IF w-job-mat.this-is-a-rm THEN 
        DO:
  
            /* Create tt-ei and tt-eiv for e-itemvend of an item */
            RUN createTtEivVend (INPUT cocode,
                INPUT ROWID(w-job-mat),
                INPUT v-po-best,
                OUTPUT gvrItem).
        END.
        ELSE 
        DO:
  
            /* Create tt-eiv for a w-job-mat and itemfg */
            RUN createTtEivItemfg (INPUT  cocode,                                         
                INPUT  ROWID(w-job-mat)).
        END.
  
        /* Just a prompt to create a line */
        RUN promptCreatePoLine.

        /* User choose not to create, so don't continue with this item */
        IF NOT gvlChoice THEN
            NEXT outers.

        /* Sets gvrB-orderpo, initialize global variables and create a b-orderpo */
        RUN initJobVals (INPUT cocode,
            INPUT fil_id,
            INPUT gvrJob,
            INPUT ROWID(w-job-mat),
            INPUT gvrOeOrdl,
            OUTPUT v-job,
            OUTPUT gvcVendNo).

        /* Get v-len, v-wid, v-dep, v-job-mat-qty, v-qty-comp, v-uom-comp */         
        RUN initRptRecs (INPUT cocode,
            INPUT ROWID(w-job-mat),
            OUTPUT gvrTT-ei) .
  
        /* Creates a report record for each tt-eiv, sets fil_id */
        RUN buildRptRecs (INPUT cocode, 
            INPUT llFirstJobFrm,
            INPUT ROWID(w-job-mat),
            INPUT gvrOeOrdl,
            INPUT gvrTT-ei,
            INPUT gvrItemfg,
            OUTPUT gvrVend).

        /* Warning message that vendor matrix does not exist */
        IF gvcVendNo EQ "" AND gvlChoice AND NOT ll-canceled THEN
            RUN cancelMessage.

        IF gvcVendNo EQ "" OR ll-canceled THEN 
        DO:
            IF gvlDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Skip Item for canceled or gvcVendNo " w-job-mat.i-no " gvcVendNo " gvcVendNo SKIP.
            NEXT.
        END.
    
        /* Set po dates from oe-ord or job */
        RUN setPoDates (INPUT gvrVend, INPUT gvrOeOrd, INPUT gvrJob).

        /* Set GV ll-drop */
        RUN promptDropShip.

        /* prompt for updating PO for given vendor and date */
        RUN promptUpdPoNum (INPUT cocode, 
            INPUT gvrB-orderPo,
            OUTPUT gvrPoOrd,
            OUTPUT gvrTT-eiv,
            OUTPUT lNextOuters). /* set choice */

        IF lNextOuters THEN
            NEXT outers.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF NOT gvlChoice THEN 
        DO:
            IF NOT AVAILABLE po-ord THEN 
            DO:
                /* SEts globals gvdDueDate and gvdPoDate */
                RUN calcDueDate (INPUT ROWID(w-job-mat)).
            END.
        END.

        /* If they chose not to update existing then return */
        IF NOT gvlChoice AND AVAIL(po-ord) THEN 
        DO:
            IF gvlDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Return since choose not to update existing " w-job-mat.i-no  SKIP.
            /* RETURN. WFK - taken out so that prompts for remaining RMs */
            NEXT outers.
        END.
    
        /* Check gvlChoice and update oe-ordl.po-no-po and vend-no */
        RUN ProcessExisting (INPUT cocode,
            INPUT gvrOeOrdl,
            INPUT llFirstOfJobFrm,
            INPUT ROWID(w-job-mat),
            INPUT gvrPoOrd).
  
        /* Find existing PO for a due date and vendor. */
        RUN findExistingPo (INPUT gvrB-orderpo, OUTPUT lPoExists, OUTPUT gvrPoOrd).

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF NOT lPoExists THEN 
        DO:
            IF AVAILABLE po-ord THEN 
            DO:
                /* Po exists for given vendor and date, locks record if say yes */
                RUN PromptExistingPo. /* release current po-ord buffer if they say no */      
            END.
            IF NOT AVAILABLE po-ord THEN 
            DO:
                RUN createPoOrd (INPUT gvrOeOrd, OUTPUT gvrPoOrd, OUTPUT lNextOuters).
                IF lNextOuters THEN 
                DO:
                    IF gvlDebug THEN             
                        PUT STREAM sDebug UNFORMATTED "Skip do to createPoOrd " w-job-mat.i-no  SKIP.
                    NEXT outers.
                END.
                FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
   
            END. /* Not avail po-ord then add it */
        END.
        ELSE
            gvlChoice = YES.

        /* Assign po values to oe-ordl and assign vend values to po-ord */
        RUN setPoValues (INPUT llFirstOfJobFrm, 
            INPUT gvrPoOrd, 
            INPUT gvrOeOrdl,
            INPUT ROWID(w-job-mat),
            INPUT gvrB-orderpo).
                  
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.

        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after assign po values)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND b-item NO-LOCK WHERE ROWID(b-item) EQ gvrItem NO-ERROR.
        FIND vend NO-LOCK WHERE ROWID(vend) EQ gvrVend NO-ERROR.

        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create PO Line " w-job-mat.i-no  SKIP.
        FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ gvrOeOrdl NO-ERROR.

        /* creates po-ordl based on gvlChoice */
        RUN createPoOrdl (INPUT gvrPoOrd,
            INPUT gvrOeOrdl,
            INPUT ROWID(w-job-mat),
            INPUT gvrItem,
            OUTPUT gvrItemfg).

        FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ gvrItemfg NO-ERROR.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after create po-ordl)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE "PO Line not available, skipping it"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT outers.
        END.

        /* set values from item and w-job-mat */
        IF po-ordl.item-type THEN
            RUN setPoOrdRm (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItem,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrJob).
        ELSE
            /* Set values from itemfg */
            RUN setPoOrdlFg (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItemfg,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrJob).


        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after assign po-ordl)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-ERROR.

        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE 'Error: No Po Line available (main block)' 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.


        IF po-ordl.item-type THEN 
        DO:
  
            IF NOT AVAILABLE tt-ei THEN
                FIND FIRST tt-ei
                    WHERE tt-ei.company EQ cocode
                    AND tt-ei.i-no    EQ w-job-mat.rm-i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE tt-ei THEN
                gvrTT-ei = ROWID(tt-ei).
            ELSE 
                gvrTT-ei = ?.

            /* get v-new-i-no, v-new-len, v-new-wid, set po-ordl.cons-uom and pr-uom */
            RUN poOrdlAddVals (INPUT gvrPoOrdl,
                INPUT gvrItem,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrTT-ei).
        END. /* run poOrdlAddVals */   

        /* Get len, wid, depth from item. Set po-ordl.cust-no */
        RUN calcLenWid (INPUT gvrPoOrd,
            INPUT gvrPoOrdl,
            INPUT gvrItem).

        /* get po-ordl.ord-qty from job-hdr, w-job-mat or oe-ordl */
        IF po-ordl.item-type THEN 
            RUN calcEstValues (INPUT gvrPoOrdl,
                INPUT gvrJob,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrItem).
        /* Set po-ordl.s-num and b-num. UOM Conversion on po-ordl.ord-qty */
        RUN calcOrdQty    (INPUT gvrPoOrdl, INPUT ROWID(w-job-mat)).

        /* get po-ordl.cost from w-job-mat or v-item-cost */
        RUN calcCostSetup (INPUT gvrTt-ei, INPUT ROWID(w-job-mat),
            INPUT gvrPoOrdl, INPUT gvcVendNo,
            INPUT v-vend-item).

        /*  Calculate v-tot-msf. Set po-ordl.s-len and s-wid */
        RUN calcMSF       (INPUT gvrPoOrdl).

        /* runs po/po-adder.p and po/po-vendc.i */
        RUN processAdders (INPUT ROWID(w-job-mat), INPUT gvrPoOrdl, INPUT gvrPoOrd).

        /* UOM conversion for po-ordl.cost */
        RUN calcCost      (INPUT gvrTT-ei, INPUT gvrPoOrdl).

        RELEASE b-item.
  
        /* Warns user of zero length or width in Job */
        RUN zeroLenWarning (INPUT gvrPoOrdl, OUTPUT gvrItem).

        FIND b-item WHERE ROWID(b-item) EQ gvrItem NO-LOCK NO-ERROR.
        FIND po-ordl WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-LOCK NO-ERROR.

        IF (AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0) OR
            NOT po-ordl.item-type                                   THEN 
        DO:
    
            /* Validate than Length was entered */
            RUN brdLenCheck (INPUT gvrPoOrdl).

            /* UOM conversions to handle zere line qty or cost. Calculate oe-ordl.cost  */
            RUN checkZeroQty (INPUT gvrPoOrdl).

            /* Calculate po-ordl.t-cost and cons-cost */
            RUN calcExtCost (INPUT gvrPoOrdl).

            /* Find po-ordl and set v-old-i-no, v-tot-ord and po-ordl.dscr */
            RUN addHeaderTot (INPUT gvrPoOrdl).

            /* If v-new-i-no is a valid item, set w-job-mat.rm-i-no to it */
            RUN autoRm (INPUT gvrPoOrdl,
                INPUT ROWID(w-job-mat),
                OUTPUT gvrItem) /* needs additional buffers */.

        END. /* if avail b-item ... */

        /* Update Farm recs, deal with changes to scoring allowances */
        RUN PoOrdlFinal (INPUT gvrPoOrdl).
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Internal Error - PO not found." 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        /* Calculate PO Header Totals */
        RUN po/po-total.p (RECID(po-ord)).

        /* CHECK for exceeding vendor's max PO Cost and worn for it (set v-hold) */
        RUN validMaxCost (gvrPoOrd).

        FIND po-ordl WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-LOCK NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE "Error - Po Line not found (22)" 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        /* Update item inventory totals */
        RUN po/poordlup.p (RECID(po-ordl), 1, v-cont-upd).

    END. /* each w-job-mat */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-promptCreatePoLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptCreatePoLine Procedure 
PROCEDURE promptCreatePoLine :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    IF gvlDebug THEN             
        PUT STREAM sDebug UNFORMATTED "Prompt create PO line " w-job-mat.i-no  SKIP.
  
    gvlChoice = NO.
    IF gvcVendNo EQ "" 
        AND ((v-autopo-sec AND w-job-mat.this-is-a-rm) OR (v-autofg-sec AND NOT w-job-mat.this-is-a-rm)) 
        AND NOT w-job-mat.isaset THEN 
    DO ON ENDKEY UNDO, LEAVE:

        MESSAGE "Do you wish to create a PO line for " +
            (IF w-job-mat.this-is-a-rm
            THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
            TRIM(STRING(w-job-mat.frm,"99")))
            ELSE ("Order/FG#: " +
            TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
            "/" + TRIM(w-job-mat.rm-i-no) + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE gvlChoice.
    END. /* Prompt to create po line */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-promptDropShip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptDropShip Procedure 
PROCEDURE promptDropShip :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        SEt GV ll-drop
    ------------------------------------------------------------------------------*/

    ll-drop = NO.
    IF nk1-oeautopo-int EQ 1 THEN
        MESSAGE "Is this a Drop Shipment?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-drop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-promptExistingPo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptExistingPo Procedure 
PROCEDURE promptExistingPo :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Finds po-ord with a share-lock
        requires GV gvlChoice and 
        sets gvlChoice
        May release po-ord
    ------------------------------------------------------------------------------*/
      
    IF NOT gvlChoice AND nk1-oeautopo-log THEN
        MESSAGE "PO exists for given Vendor and Date." SKIP
            "Do you want to update existing PO? " 
            VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE gvlChoice.

    IF  nk1-oeautopo-log = NO THEN
        gvlChoice = NO.

    IF gvlChoice THEN FIND CURRENT po-ord NO-ERROR.
    ELSE RELEASE po-ord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-promptUpdPoNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promptUpdPoNum Procedure 
PROCEDURE promptUpdPoNum :
    /*------------------------------------------------------------------------------
      Purpose:     If PO Already Exists, ask to Update it 
      Parameters:  <none>
      Notes:       
        Inputs:
          Needs b-orderpo
          Finds po-ord
          cocode
          gvcVendNo
          ll-drop
          Sets gvlChoice
          finds tt-eiv
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprB-orderpo AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprTT-eiv AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oplNextOuters AS LOGICAL     NO-UNDO.

    FIND b-orderpo WHERE ROWID(b-orderpo) EQ iprB-orderpo NO-LOCK NO-ERROR.
    gvlChoice = NO.
    oplNextOuters = FALSE.
    FIND LAST po-ord NO-LOCK
        WHERE po-ord.company   EQ cocode
        AND po-ord.po-no     EQ INT(b-orderpo.val[1])
        AND b-orderpo.val[1] NE 0
        AND po-ord.vend-no   EQ gvcVendNo
        AND (po-ord.type     EQ "D" OR NOT ll-drop)
        NO-ERROR.

    IF AVAILABLE po-ord AND NOT po-ord.opened THEN 
    DO: 
        oplNextOuters = TRUE.
    END. /* If PO was found but was not opened */
    ELSE 
    DO:

  
        gvlChoice = AVAILABLE po-ord.
  
        IF gvlChoice AND po-ord.opened AND v-autopo-sec THEN 
        DO:
            MESSAGE "Do you wish to update PO #" +
                TRIM(STRING(po-ord.po-no,">>>>>>>>")) + " for " +
                (IF AVAILABLE job THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +
                TRIM(STRING(w-job-mat.frm,"99")))
                ELSE ("Order/FG#: " +
                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +
                "/" + TRIM(w-job-mat.rm-i-no) + "?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE gvlChoice.

            FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ po-ord.vend-no NO-ERROR.
        END.

        ELSE FIND FIRST tt-eiv WHERE tt-eiv.rec-id EQ fil_id NO-LOCK NO-ERROR.

        IF AVAILABLE tt-eiv THEN
            oprTT-eiv = ROWID(tt-eiv).   
        IF AVAILABLE po-ord THEN 
        DO:
            oprPoOrd = ROWID(po-ord).      
        END.
      
    END. /* NOT If PO was found but was not opened */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPoDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPoDates Procedure 
PROCEDURE setPoDates :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs
          vend
          job
          bf-ord
        Outputs
          gvdPoDate (global)
          gvdDueDate (global)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprVend  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob   AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-job FOR job.

    FIND vend NO-LOCK WHERE ROWID(vend) EQ iprVend NO-ERROR.
    FIND bf-ord NO-LOCK WHERE ROWID(bf-ord) EQ iprOeOrd NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.

    /* vend.disc-days eq lead time */
    IF AVAILABLE vend THEN gvdPoDate = TODAY.
    ELSE gvdPoDate = IF AVAILABLE bf-ord THEN bf-ord.ord-date ELSE bf-job.start-date.

    IF AVAILABLE vend THEN gvdDueDate = TODAY + vend.disc-days.
    ELSE gvdDueDate = IF AVAILABLE bf-ord THEN bf-ord.ord-date ELSE bf-job.start-date.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPoOrdlFg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPoOrdlFg Procedure 
PROCEDURE setPoOrdlFg :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        requires:
          itemfg
          job
          bf-ordl (updates)
          w-job-mat
          po-ordl (updates)
        Global: 
          lv-recid
        Outputs: 
           makes tt-ei available
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItemFg AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-job       FOR job.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord   NO-LOCK WHERE ROWID(bf-po-ord)  EQ iprPoOrd NO-ERROR.
    FIND bf-itemfg  NO-LOCK WHERE ROWID(bf-itemfg)  EQ iprItemfg NO-ERROR.
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob  NO-ERROR.

    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        MESSAGE "Error: Po Line not available (setPoOrdlFg)"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN ERROR.
    END.

    IF AVAILABLE bf-itemfg THEN 
    DO:
        FIND FIRST tt-ei NO-LOCK 
            WHERE tt-ei.company EQ cocode
            AND tt-ei.i-no    EQ bf-itemfg.i-no
            NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST xest NO-LOCK
                WHERE xest.company EQ bf-po-ordl.company
                AND xest.est-no  EQ bf-job.est-no
                NO-ERROR.
        IF AVAIL(xest) THEN
            FIND FIRST xeb NO-LOCK WHERE xeb.company = bf-po-ordl.company
                AND xeb.est-no = xest.est-no
                AND xeb.stock-no EQ bf-itemfg.i-no
                NO-ERROR.
  
  
        ASSIGN
            bf-po-ordl.i-no       = bf-itemfg.i-no
            bf-po-ordl.i-name     = bf-ordl.i-name
            bf-po-ordl.pr-qty-uom = IF pouom-chr EQ "Purchase" THEN bf-itemfg.pur-uom
                                                       ELSE bf-itemfg.prod-uom
            bf-po-ordl.cons-uom   = bf-itemfg.prod-uom
            bf-po-ordl.pr-uom     = bf-itemfg.pur-uom
            bf-po-ordl.cons-cost  = bf-itemfg.last-cost
            v-part-dscr1          = bf-ordl.part-dscr1
            v-part-dscr2          = bf-ordl.part-dscr2
            bf-po-ordl.ord-qty    = bf-w-job-mat.qty
            v-op-type             = NO.
  
        IF AVAILABLE xeb THEN
            ASSIGN
                bf-po-ordl.s-num = xeb.form-no
                bf-po-ordl.b-num = xeb.blank-no.
            
        /* Uncomment below to implement a vendor matrix UOM per vendor */
        /*      IF v-vendor-chosen-report NE ? THEN DO:                                           */
        /*          FIND FIRST e-itemfg-vend WHERE recid(e-itemfg-vend) EQ v-vendor-chosen-report */
        /*              NO-LOCK.                                                                  */
        /*          IF AVAIL e-itemfg-vend THEN                                                   */
        /*          po-ordl.pr-uom = e-itemfg-vend.std-uom.                                       */
        /*      END.                                                          
                              */
        IF NOT AVAILABLE tt-ei THEN
            FIND FIRST tt-ei
                WHERE tt-ei.company EQ cocode
                AND tt-ei.i-no    EQ bf-w-job-mat.rm-i-no
                NO-LOCK NO-ERROR.
  
  
        IF AVAILABLE tt-ei THEN
            bf-po-ordl.pr-uom = tt-ei.std-uom.
           
        bf-po-ordl.ord-no = IF AVAILABLE bf-ordl THEN bf-ordl.ord-no ELSE 0.
        FIND oe-ordl EXCLUSIVE-LOCK WHERE RECID(oe-ordl) EQ lv-recid NO-ERROR.
  
        IF AVAILABLE oe-ordl THEN
        DO:
            IF bf-po-ordl.cons-uom EQ "M" THEN
                oe-ordl.cost = bf-po-ordl.cons-cost.
            ELSE
                RUN sys/ref/convcuom.p (bf-po-ordl.cons-uom, "M", 0, 0, 0, 0,
                    bf-po-ordl.cons-cost, OUTPUT oe-ordl.cost).
         
            RELEASE oe-ordl. 
        END. /* oe-ordl */
  
        IF bf-ordl.i-no NE bf-itemfg.i-no THEN
            ASSIGN
                bf-po-ordl.i-name = bf-itemfg.i-name
                v-part-dscr1      = bf-itemfg.part-dscr1
                v-part-dscr2      = bf-itemfg.part-dscr2.
  
        IF bf-po-ordl.pr-qty-uom NE "EA"                    AND
            (bf-po-ordl.item-type OR
            LOOKUP(bf-po-ordl.pr-qty-uom,fg-uom-list) EQ 0) THEN
            RUN sys/ref/convquom.p("EA", bf-po-ordl.pr-qty-uom,
                bf-w-job-mat.basis-w, bf-w-job-mat.len, bf-w-job-mat.wid, bf-w-job-mat.dep,
                bf-po-ordl.ord-qty, OUTPUT bf-po-ordl.ord-qty).
  
        FOR EACH prodl FIELDS(prolin)
            WHERE prodl.company EQ cocode
            AND prodl.procat  EQ bf-itemfg.procat
            NO-LOCK,
            FIRST prod FIELDS(fg-mat)
            WHERE prod.company EQ cocode
            AND prod.prolin  EQ prodl.prolin
            NO-LOCK:
  
            bf-po-ordl.actnum = prod.fg-mat.
            LEAVE.
        END. /* each prodl */
  
        IF bf-itemfg.vend-no EQ bf-po-ord.vend-no THEN
            bf-po-ordl.vend-i-no = bf-itemfg.vend-item.
        ELSE IF bf-itemfg.vend2-no EQ bf-po-ord.vend-no THEN
                bf-po-ordl.vend-i-no = bf-itemfg.vend2-item.
  
        /* populate GL# from reftable if it exists using bf-itemfg AH 02-23-10*/
        ASSIGN 
            v-charge = "".
        FIND FIRST surcharge WHERE surcharge.company = cocode
            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN v-charge = surcharge.charge.
        FIND FIRST reftable NO-LOCK WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ bf-itemfg.company
            AND reftable.loc      EQ bf-itemfg.procat
            AND reftable.code     EQ v-charge
            /* AND reftable.code2 = "" */
            NO-ERROR.
        IF AVAILABLE reftable AND reftable.dscr <> "" THEN 
            ASSIGN bf-po-ordl.actnum = reftable.dscr.
    END. /* avail bf-itemfg */

    FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
    RELEASE bf-po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPoOrdRm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPoOrdRm Procedure 
PROCEDURE setPoOrdRm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Requires:
          po-ord
          po-ordl
          b-item
        Uses Globals:
          v-actnum
          cocode
          gvrJobRecid
          v-actnum
          v-op-type
        Outputs:
          make tt-ei available
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER b-item       FOR ITEM .
    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-job       FOR job.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND b-item NO-LOCK WHERE ROWID(b-item) EQ iprItem NO-ERROR.
    FIND bf-w-job-mat NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.

    ASSIGN 
        v-actnum = "".
    FIND FIRST costtype NO-LOCK
        WHERE costtype.company   EQ bf-po-ordl.company
        AND costtype.loc       EQ bf-po-ord.loc
        AND costtype.cost-type EQ b-item.cost-type
        NO-ERROR.
    IF AVAILABLE costtype AND ap-gl#-log THEN
        bf-po-ordl.actnum = IF ap-gl#-cha EQ "Asset"   THEN costtype.inv-asset
        ELSE
            IF ap-gl#-cha BEGINS "Exp"  AND
            (ap-gl#-cha EQ "Expense" OR costtype.cons-exp NE "")
            THEN costtype.cons-exp
            ELSE                            bf-po-ordl.actnum.


    /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10*/
    IF gvrJobRecid <> ? THEN 
    DO:
        FIND b-job-hdr NO-LOCK WHERE RECID(b-job-hdr) = gvrJobRecid NO-ERROR.

        /* Get gl actnum for a itemfg */
        IF AVAILABLE b-job-hdr THEN 
            RUN getItemfgGL (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT v-actnum).

        IF v-actnum <> "" THEN 
            ASSIGN bf-po-ordl.actnum = v-actnum.
    END.
    RELEASE b-job-hdr.

    FIND FIRST tt-ei
        WHERE tt-ei.company EQ cocode
        AND tt-ei.i-no    EQ b-item.i-no
        NO-LOCK NO-ERROR.

    ASSIGN
        bf-po-ordl.s-num      = bf-w-job-mat.frm
        bf-po-ordl.b-num      = bf-w-job-mat.blank-no
        bf-po-ordl.job-no     = bf-job.job-no
        bf-po-ordl.job-no2    = bf-job.job-no2
        bf-po-ordl.i-no       = b-item.i-no
        bf-po-ordl.i-name     = b-item.i-name
        bf-po-ordl.pr-qty-uom = bf-w-job-mat.qty-uom
        bf-po-ordl.cons-uom   = b-item.cons-uom
        bf-po-ordl.pr-uom     = b-item.pur-uom
        bf-po-ordl.cons-cost  = b-item.last-cost
        v-part-dscr1          = b-item.i-dscr
        v-part-dscr2          = b-item.est-dscr
        v-op-type             = YES.
   
        ASSIGN bf-po-ordl.pr-qty-uom = IF pouom-chr EQ "Purchase" THEN b-item.pur-uom
                                                                      ELSE b-item.cons-uom .
   
    bf-po-ordl.ord-no = IF AVAILABLE bf-ordl THEN bf-ordl.ord-no ELSE 0.
    FIND CURRENT bf-po-ordl NO-LOCK.
    RELEASE bf-po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPoValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPoValues Procedure 
PROCEDURE setPoValues :
    /*------------------------------------------------------------------------------
      Purpose:     procedure Assign PO ord values from vend and sales order 
      Parameters:  <none>
      Notes:       
      
      Global Buffers Used:
      po-ordl     modified - used in next step
      b-orderpo   must be available Modified
      bf-ord      must be available
      bf-ordl     must be available
      b-oe-ordl   local?
      vend        found in procedure
      po-ord      modified
      oe-rel      found in procedure
      item        must be available
      b-item      modified - used in createPoOrdl
      w-job-mat   must be available
      
      
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplFirstOfFrm AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprB-orderpo AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-w-job-mat FOR w-job-mat.

    FIND bf-po-ord EXCLUSIVE-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND bf-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-w-job-mat  NO-LOCK WHERE ROWID(bf-w-job-mat) EQ iprWJobMat NO-ERROR.
    FIND b-orderpo EXCLUSIVE-LOCK WHERE ROWID(b-orderpo) EQ iprB-orderpo NO-ERROR.

    FIND CURRENT b-orderpo EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE b-orderpo THEN 
    DO:
        MESSAGE "Error: b-orderpo not found, skipping..."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN ERROR.
    END.
    b-orderpo.val[1] = bf-po-ord.po-no.
    FIND CURRENT b-orderpo NO-LOCK NO-ERROR.
   
    IF AVAILABLE bf-ordl AND iplFirstOfFrm THEN
        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company EQ bf-ordl.company
            AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
            AND b-oe-ordl.job-no  EQ bf-ordl.job-no
            AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
            AND b-oe-ordl.i-no    EQ bf-w-job-mat.fg-i-no
            EXCLUSIVE-LOCK:
            /* for testing put this back!!! */

            ASSIGN
                b-oe-ordl.po-no-po = bf-po-ord.po-no
                b-oe-ordl.vend-no  = gvcVendNo.

        END.

    FIND CURRENT bf-po-ord EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST vend
        WHERE vend.company EQ cocode 
        AND vend.vend-no EQ bf-po-ord.vend-no 
        USE-INDEX vend NO-LOCK NO-ERROR.
    IF AVAILABLE vend THEN 
    DO:
        ASSIGN
            bf-po-ord.last-ship-date = bf-po-ord.due-date
            bf-po-ord.over-pct       = vend.over-pct
            bf-po-ord.under-pct      = vend.under-pct
            bf-po-ord.carrier        = vend.carrier
            bf-po-ord.contact        = vend.contact
            bf-po-ord.terms          = vend.terms
            bf-po-ord.fob-code       = vend.fob-code
            bf-po-ord.frt-pay        = vend.frt-pay
            bf-po-ord.tax-gr         = vend.tax-gr.
     
        IF bf-po-ord.fob-code EQ "" THEN bf-po-ord.fob = "DEST".
        IF bf-po-ord.frt-pay  EQ "" THEN bf-po-ord.frt-pay = "B".
    
        IF bf-po-ord.due-date LT bf-po-ord.po-date THEN
            ASSIGN
                bf-po-ord.due-date       = bf-po-ord.po-date
                bf-po-ord.last-ship-date = bf-po-ord.due-date.
    END. /* If avail vend */

    IF AVAILABLE bf-ordl        AND bf-po-ord.type EQ "D"    AND
        bf-po-ord.cust-no NE "" AND bf-po-ord.frt-pay NE "P" THEN
    DO:
        FIND FIRST oe-rel OF bf-ordl NO-LOCK NO-ERROR.
        IF AVAILABLE oe-rel THEN bf-po-ord.carrier = oe-rel.carrier.
    END.

    IF AVAILABLE bf-ord THEN
        ASSIGN bf-po-ord.frt-pay  = bf-ord.frt-pay
            bf-po-ord.fob-code = bf-ord.fob-code.
            
    FIND CURRENT bf-po-ord NO-LOCK NO-ERROR.
    RELEASE bf-po-ord.
    RELEASE bf-ordl.
    RELEASE bf-w-job-mat.
    RELEASE b-orderpo.
/* end procedure Assign PO ord values from vend and sales order */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ttItemfgFromJob) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttItemfgFromJob Procedure 
PROCEDURE ttItemfgFromJob :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Input:
        job buffer
        b-tt-itemfg?
        bf-ordl
      Output:
        creates tt-itemfg records
        create tt-fg-set
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilIdSource AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bf-ordl FOR oe-ordl.
    /* Check that cFilIDSource is JOB since job record may be available here    */
    /* even though the fil_id is related to oe-ordl and could cause a duplicate */
    /* tt-itemfg                                                                */
    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR. 
    
    IF gvlDebug THEN
        PUT STREAM sDebug UNFORMATTED "In ttItemfgFromJob " SKIP.
  
    IF AVAILABLE job AND ipcFilIdSource = "JOB" THEN 
    DO:
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2,
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no:
       
            FIND FIRST eb NO-LOCK WHERE 
                eb.company EQ job-hdr.company AND
                eb.est-no EQ job-hdr.est-no AND
                eb.stock-no EQ job-hdr.i-no AND
                eb.blank-no EQ job-hdr.blank-no AND
                eb.form-no EQ job-hdr.frm
                NO-ERROR.
            IF gvlDebug THEN
                PUT STREAM sDebug UNFORMATTED "Create tt-itemfg from job-hdr " itemfg.i-no " frm " job-hdr.frm " blank " job-hdr.blank-no SKIP.
  
            CREATE tt-itemfg.
            BUFFER-COPY itemfg EXCEPT rec_key pur-man TO tt-itemfg
                ASSIGN
                tt-itemfg.form-no  = job-hdr.frm
                tt-itemfg.blank-no = job-hdr.blank-no
                tt-itemfg.qty      = IF AVAILABLE bf-ordl THEN bf-ordl.qty ELSE job-hdr.qty
                tt-itemfg.row-id   = ROWID(itemfg). 
            tt-itemfg.pur-man = IF AVAILABLE eb THEN eb.pur-man ELSE itemfg.pur-man.
        END.
    
        /* WFK - needs more explanation, why would this be found? */
        /* was in original versions of program                    */
        FIND b-tt-itemfg EXCLUSIVE-LOCK NO-ERROR.
    
        IF AVAILABLE b-tt-itemfg THEN 
        DO:
            IF b-tt-itemfg.isaset THEN 
            DO:
                b-tt-itemfg.pur-man = NO.
          
                FOR EACH b-jc-calc NO-LOCK
                    WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
                    AND b-jc-calc.company  EQ job.company
                    AND b-jc-calc.loc      EQ ""
                    AND b-jc-calc.code     EQ STRING(job.job,"999999999"),
                    FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ b-jc-calc.company
                    AND itemfg.i-no    EQ b-jc-calc.code2
                    AND itemfg.pur-man:
                    IF gvlDebug THEN
                        PUT STREAM sDebug UNFORMATTED "Create tt-itemfg from tt-fg-set (job) " itemfg.i-no 
                            " frm " b-jc-calc.val[12] " blank " b-jc-calc.val[13] SKIP.
                    CREATE tt-itemfg.
                    BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                        ASSIGN
                        tt-itemfg.form-no  = b-jc-calc.val[12]
                        tt-itemfg.blank-no = b-jc-calc.val[13]
                        tt-itemfg.row-id   = ROWID(itemfg). 
              
                    IF AVAILABLE bf-ordl THEN
                        FIND FIRST bf-itemfg NO-LOCK
                            WHERE bf-itemfg.company EQ bf-ordl.company
                            AND bf-itemfg.i-no    EQ bf-ordl.i-no
                            NO-ERROR.
                    IF AVAILABLE bf-itemfg THEN
                        RUN fg/fullset.p (ROWID(bf-itemfg)).
          
                    FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ b-jc-calc.code2:
                        tt-itemfg.qty = tt-itemfg.qty + (b-tt-itemfg.qty * tt-fg-set.part-qty-dec).
                    END. /* each tt-fg-set */
                END. /* each b-jc-calc */
            END. /* if b-tt-itemfg.isaset */
       
            IF b-tt-itemfg.pur-man EQ NO THEN DELETE b-tt-itemfg.
        END. /* if avail bb-tt-itemfg */
    END. /* if avail job... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-validMaxCost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validMaxCost Procedure 
PROCEDURE validMaxCost :
    /*------------------------------------------------------------------------------
      Purpose:     Validate Maximum P.O. Cost 
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ord FOR po-ord.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-po-ord THEN 
    DO:
        MESSAGE "Internal error - No PO found."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    IF NOT v-hold AND bf-po-ord.stat NE "H" AND
        AVAILABLE vend AND vend.rebate-% NE 0 THEN
    DO:
        v-tot-cost = 0.
        FOR EACH b-po-ordl FIELDS(t-cost) NO-LOCK WHERE
            b-po-ordl.company EQ bf-po-ord.company AND
            b-po-ordl.po-no EQ bf-po-ord.po-no
            :

            v-tot-cost = v-tot-cost + b-po-ordl.t-cost.
        END.

        IF v-tot-cost GT vend.rebate-% THEN
        DO:
            MESSAGE "Purchase Order Cost Has Exceeded Vendor's Max P.O. Cost." SKIP
                "Purchase Order Will Be Placed On Hold."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            ASSIGN 
                v-hold         = YES
                bf-po-ord.stat = "H".
        END. /* v-tot-cost gt vend.rebate-% */
    END. /* not v-hold ...*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-wJobFromBJobMat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wJobFromBJobMat Procedure 
PROCEDURE wJobFromBJobMat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          job
          updates w-job-mat
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprJob AS ROWID NO-UNDO.
    DEFINE BUFFER bf-job FOR job.

    FIND bf-job WHERE ROWID(bf-job) EQ iprJob NO-LOCK NO-ERROR.


    /* Create w-job-mat from item */
    FOR EACH b-job-mat NO-LOCK WHERE
        b-job-mat.company EQ bf-job.company AND
        b-job-mat.job     EQ bf-job.job AND
        b-job-mat.job-no  EQ bf-job.job-no AND
        b-job-mat.job-no2 EQ bf-job.job-no2 AND
        NOT CAN-FIND(FIRST tt-itemfg WHERE
        tt-itemfg.form-no EQ b-job-mat.frm AND
        tt-itemfg.pur-man EQ YES)
        ,
        FIRST prep FIELDS(i-no number-up) NO-LOCK WHERE
        prep.company EQ b-job-mat.company AND
        prep.i-no EQ b-job-mat.i-no
        ,
        FIRST ITEM  NO-LOCK WHERE
        item.company EQ b-job-mat.company AND
        item.i-no    EQ prep.i-no AND
        CAN-DO("7,8,M,X,Y",item.mat-type)
        :

        FIND FIRST b-jc-calc NO-LOCK WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ bf-job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(bf-job.job,"999999999") AND
            b-jc-calc.val[12]  EQ b-job-mat.frm AND
            (b-jc-calc.val[13] EQ b-job-mat.blank-no OR b-job-mat.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-ERROR.

        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from b-job-mat " b-job-mat.i-no  SKIP.

        CREATE w-job-mat.
        BUFFER-COPY b-job-mat TO w-job-mat
            ASSIGN
            w-job-mat.w-rowid      = ROWID(b-job-mat)
            w-job-mat.w-recid      = RECID(b-job-mat)
            w-job-mat.this-is-a-rm = YES
            w-job-mat.dep          = item.s-dep
            w-job-mat.basis-w      = item.basis-w
            w-job-mat.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                                    ELSE bf-ordl.i-no
            w-job-mat.prep         = YES
            w-job-mat.rm-i-no      = b-job-mat.i-no
            w-job-mat.i-no         = b-job-mat.i-no
            w-job-mat.qty-uom      = "EA"
            w-job-mat.n-up         = b-job-mat.n-up.
            
            IF w-job-mat.dep EQ 0 THEN w-job-mat.dep = b-job-mat.dep.

    END. /* for each b-job-mat */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-wJobFromJobMat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wJobFromJobMat Procedure 
PROCEDURE wJobFromJobMat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        job
        v-po-best
        bf-ordl   must exist
        b-jc-calc may exists
      Outputs:
        creates w-job-mat
    ------------------------------------------------------------------------------*/
    /* Create w-job-mat from job-mat */
    DEFINE INPUT  PARAMETER iplPoBest AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ordl FOR oe-ordl.
    DEFINE BUFFER bf-job  FOR job.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.


    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ bf-job.company
        AND job-mat.job     EQ bf-job.job
        AND job-mat.job-no  EQ bf-job.job-no
        AND job-mat.job-no2 EQ bf-job.job-no2
        AND NOT CAN-FIND(FIRST tt-itemfg
        WHERE tt-itemfg.form-no EQ job-mat.frm
        AND tt-itemfg.pur-man EQ YES
        AND tt-itemfg.isaset EQ NO)
        ,

        FIRST ITEM NO-LOCK
        WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
        :
        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no " iplpobest "
                iplPoBest " type " item.mat-type SKIP.
    
        IF iplPoBest = NO AND NOT CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN NEXT.
        ELSE IF iplPoBest AND ITEM.mat-type NE "B" THEN NEXT.

        FIND FIRST b-jc-calc NO-LOCK
            WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
            AND b-jc-calc.company  EQ bf-job.company
            AND b-jc-calc.loc      EQ ""
            AND b-jc-calc.code     EQ STRING(bf-job.job,"999999999")
            AND b-jc-calc.val[12]  EQ job-mat.frm
            AND (b-jc-calc.val[13] EQ job-mat.blank-no OR job-mat.blank-no EQ 0)
            AND b-jc-calc.code2    NE bf-ordl.i-no
            NO-ERROR.
        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no SKIP.
        CREATE w-job-mat.
        BUFFER-COPY job-mat TO w-job-mat
            ASSIGN
            w-job-mat.w-rowid      = ROWID(job-mat)
            w-job-mat.w-recid      = RECID(job-mat)
            w-job-mat.this-is-a-rm = YES
            w-job-mat.dep          = item.s-dep
            w-job-mat.basis-w      = item.basis-w
            w-job-mat.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                               ELSE bf-ordl.i-no.
        IF w-job-mat.dep EQ 0 THEN w-job-mat.dep = job-mat.dep.
    END. /* each job-mat */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-wJobFromJobPrep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wJobFromJobPrep Procedure 
PROCEDURE wJobFromJobPrep :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        job
        bf-ordl.i-no
      Outputs: 
        creates w-job-mat
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIno AS CHARACTER   NO-UNDO.

    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.

    IF NOT AVAILABLE job THEN
        RETURN.

    /* Create w-job-mat  from job-prep */
    FOR EACH job-prep NO-LOCK WHERE
        job-prep.company EQ job.company AND
        job-prep.job     EQ job.job AND
        job-prep.job-no  EQ job.job-no AND
        job-prep.job-no2 EQ job.job-no2 AND
        NOT CAN-FIND(FIRST tt-itemfg WHERE
        tt-itemfg.form-no EQ job-prep.frm AND
        tt-itemfg.pur-man EQ YES AND
        tt-itemfg.isaset EQ NO)
        ,
        FIRST prep FIELDS(i-no number-up)  NO-LOCK WHERE
        prep.company EQ job-prep.company AND
        prep.CODE EQ job-prep.CODE AND
        prep.i-no NE ""
        ,
        FIRST ITEM NO-LOCK WHERE
        item.company EQ job-prep.company AND
        item.i-no    EQ prep.i-no
        :
   
        FIND FIRST b-jc-calc WHERE
            b-jc-calc.reftable EQ "jc/jc-calc.p" AND
            b-jc-calc.company  EQ job.company AND
            b-jc-calc.loc      EQ "" AND
            b-jc-calc.code     EQ STRING(job.job,"999999999") AND
            b-jc-calc.val[12]  EQ job-prep.frm AND
            (b-jc-calc.val[13] EQ job-prep.blank-no OR job-prep.blank-no EQ 0) AND
            b-jc-calc.code2    NE bf-ordl.i-no
            NO-LOCK NO-ERROR.
        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from job-prep " prep.i-no  SKIP.      
        CREATE w-job-mat.
        BUFFER-COPY job-prep TO w-job-mat
            ASSIGN
            w-job-mat.w-rowid      = ROWID(job-prep)
            w-job-mat.w-recid      = RECID(job-prep)
            w-job-mat.this-is-a-rm = YES
            w-job-mat.dep          = item.s-dep
            w-job-mat.basis-w      = item.basis-w
            w-job-mat.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                                    ELSE ipcIno
            w-job-mat.prep         = YES
            w-job-mat.rm-i-no      = prep.i-no
            w-job-mat.i-no         = prep.i-no
            w-job-mat.qty-uom      = "EA"
            w-job-mat.n-up         = prep.number-up.
    END.
    RELEASE job.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-wJobFromOrdm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wJobFromOrdm Procedure 
PROCEDURE wJobFromOrdm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs:
        job
        bf-ordl.i-no
      Outputs: 
        creates w-job-mat
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIno AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bfOeOrd FOR oe-ord.


    FIND bfOeOrd NO-LOCK WHERE ROWID(bfOeOrd) EQ iprOrd NO-ERROR.

    IF NOT AVAILABLE bfOeOrd THEN
        RETURN.


    FOR EACH oe-Ordm NO-LOCK
        WHERE oe-Ordm.company EQ bfOeOrd.company 
        AND oe-ordm.ord-no EQ bfOeOrd.ord-no
        AND oe-ordm.bill EQ "Y" ,
        FIRST est-prep NO-LOCK 
        WHERE est-prep.company EQ oe-ordm.company
        AND est-prep.est-no EQ oe-ordm.est-no
        AND est-prep.CODE EQ oe-ordm.charge
        AND est-prep.s-num EQ oe-ordm.FORM-no
        ,
        FIRST prep FIELDS(i-no number-up) NO-LOCK 
        WHERE prep.company EQ oe-ordm.company 
        AND prep.CODE EQ oe-ordm.charge 
        AND prep.i-no NE ""
        ,
        FIRST ITEM NO-LOCK
        WHERE item.company EQ est-prep.company 
        AND item.i-no    EQ prep.i-no
        :  
 
        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from est-prep " prep.i-no  SKIP.      
        CREATE w-job-mat.
        BUFFER-COPY oe-ordm except po-no TO w-job-mat 
            ASSIGN
            w-job-mat.w-rowid      = ROWID(est-prep)
            w-job-mat.w-recid      = RECID(est-prep)
            w-job-mat.this-is-a-rm = YES
            w-job-mat.dep          = item.s-dep
            w-job-mat.basis-w      = item.basis-w
            w-job-mat.fg-i-no      = ipcIno
            w-job-mat.prep         = YES
            w-job-mat.rm-i-no      = prep.i-no
            w-job-mat.i-no         = prep.i-no
            w-job-mat.qty          = 1
            w-job-mat.qty-uom      = "EA"
            w-job-mat.n-up         = prep.number-up
            w-job-mat.eqty         = est-prep.eqty
            w-job-mat.est-no       = est-prep.est-no
            w-job-mat.frm          = est-prep.s-num
            w-job-mat.blank-no     = est-prep.b-num
            w-job-mat.std-cost     = oe-ordm.cost
            w-job-mat.sc-uom       = "EA"
            w-job-mat.po-no        = integer(oe-ordm.po-no).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-wJobFromttItemfg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wJobFromttItemfg Procedure 
PROCEDURE wJobFromttItemfg :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      Inputs
        oeautofg-log v-autofg-sec bf-ord bf-ordl oeautofg-chr 
    ------------------------------------------------------------------------------*/
    /* wfk - Create w-job-mat based on tt-itemfg created above */
    /*       w-job-mat handling both fg and rm                 */

    DEFINE INPUT  PARAMETER ipcOeAutoFg AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrd     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl    AS ROWID       NO-UNDO.
  
    DEFINE BUFFER bf-ord  FOR oe-ord.
    DEFINE BUFFER bf-ordl FOR oe-ordl.

    IF gvlDebug THEN
        PUT STREAM sDebug UNFORMATTED "Start create-w-job-from-tt-itemfg"  SKIP.


    FIND bf-ord NO-LOCK WHERE ROWID(bf-ord) EQ iprOeOrd NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.

    FOR EACH tt-itemfg WHERE tt-itemfg.pur-man,
        FIRST itemfg NO-LOCK WHERE ROWID(itemfg) EQ tt-itemfg.row-id :

        IF gvlDebug THEN
            PUT STREAM sDebug UNFORMATTED "Create w-job-mat from tt-itemfg consider criteria " itemfg.i-no 
                " oeautofg-chr " oeautofg-chr " ipcOeAutofg " ipcOeAutofg SKIP.

        IF ((oeautofg-chr EQ "NonStock" OR ipcOeAutoFg EQ "Any") AND
            NOT itemfg.stocked)                                         OR
            ((ipcOeAutoFg EQ "LotCntrl" OR ipcOeAutoFg EQ "Any") AND
            NOT itemfg.ord-policy)                                      OR
            ((ipcOeAutoFg EQ "Avail<0"  OR ipcOeAutoFg EQ "Any") AND
            itemfg.q-avail LT 0)                                        THEN 
        DO:
  
            IF gvlDebug THEN
                PUT STREAM sDebug UNFORMATTED "Create w-job-mat from purchased tt-itemfg " itemfg.i-no  SKIP.
            CREATE w-job-mat.
            ASSIGN
                w-job-mat.w-recid      = ?
                w-job-mat.rm-i-no      = itemfg.i-no
                w-job-mat.i-no         = itemfg.i-no
                w-job-mat.fg-i-no      = itemfg.i-no
                w-job-mat.est-no       = bf-ordl.est-no
                w-job-mat.frm          = tt-itemfg.form-no
                w-job-mat.blank-no     = tt-itemfg.blank-no
                w-job-mat.isaset       = itemfg.isaset
                w-job-mat.isacomponent = tt-itemfg.isacomponent
                w-job-mat.this-is-a-rm = NO
                w-job-mat.basis-w      = itemfg.t-wid * itemfg.t-len * 100
                w-job-mat.basis-w      = itemfg.weight-100 /
                                (IF v-corr THEN (w-job-mat.basis-w * .007)
                                           ELSE (w-job-mat.basis-w / 144) /
                                 1000)
                w-job-mat.qty-uom      = "EA"
                w-job-mat.sc-uom       = itemfg.pur-uom
                w-job-mat.qty          = tt-itemfg.qty.
                RUN po/GetFGDimsForPO.p (ROWID(itemfg), OUTPUT w-job-mat.len, OUTPUT w-job-mat.wid, OUTPUT w-job-mat.dep).
  
        END. /* if q-avail is OK */
    END. /* each tt-itemfg */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-zeroLenWarning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zeroLenWarning Procedure 
PROCEDURE zeroLenWarning :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        INputs:
          po-ordl
          Global Vars:
          v-len
          v-wid
          cocode
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.

    IF bf-po-ordl.item-type THEN /*DO*/
        FIND FIRST b-item NO-LOCK 
            WHERE b-item.company EQ cocode
            AND b-item.i-no    EQ bf-po-ordl.i-no
            USE-INDEX i-no NO-ERROR.
    IF AVAILABLE b-item THEN
        oprItem = ROWID(b-item).
    IF AVAILABLE b-item AND b-item.i-code EQ "E"
        AND index("1234BPRWF",b-item.mat-type) GT 0 THEN 
    DO:
        IF (v-len EQ 0 OR v-wid EQ 0) THEN 
        DO:
            MESSAGE "Invalid Length or Width. Enter Valid Job or Non-Zero Length and Width"
                VIEW-AS ALERT-BOX ERROR.
        /* return ??*/          
        END. /* v-len eq 0 ... */
    END. /* avail b-item ... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

