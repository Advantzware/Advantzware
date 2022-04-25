&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : po/dopo.p 
    Purpose     :
                 Add PO from Order Entry Program - P/O Module                             
                 Similar logic in dopo-best.p                                              
    Syntax      :

    Description :

    Author(s)   : 04/18/2022
    Created     :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
USING system.SharedConfig.

DEFINE INPUT  PARAMETER iplPromptRM     AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipchCompany     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER reORDRecId      AS RECID NO-UNDO.
DEFINE INPUT  PARAMETER locode          AS CHARACTER NO-UNDO.
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE loDebug AS LOG NO-UNDO.
loDebug = NO.
DEFINE STREAM sDebug.

DEFINE VARIABLE debasis-w      LIKE ITEM.basis-w NO-UNDO.
DEFINE VARIABLE deS-Len        LIKE ITEM.s-len NO-UNDO.
DEFINE VARIABLE deS-wid        LIKE ITEM.s-wid NO-UNDO.
DEFINE VARIABLE deS-dep        LIKE ITEM.s-dep NO-UNDO.
DEFINE VARIABLE deAdder        AS DECIMAL   NO-UNDO EXTENT 2.
DEFINE VARIABLE chPartDescr1   AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE chPartDescr2   AS CHARACTER NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE loCountUpdate  AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE cocode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE loNewfile      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loChoice       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAccessClose  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chAccessList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE deqty          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE decost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE deItemCost     AS DECIMAL   NO-UNDO FORMAT ">>,>>9.9999" INIT 0.
DEFINE VARIABLE chVendNo       LIKE vend.vend-no INITIAL "" NO-UNDO.
DEFINE VARIABLE chVendItem     LIKE ITEM.vend-item NO-UNDO.
DEFINE VARIABLE deSetup        LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE loAutoPoSec    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAutoFgSec    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loAutoPrepSec  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE inOeAutoPONK1  AS INTEGER   NO-UNDO.
DEFINE VARIABLE looeAutopoNK1  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE choeAutoPoNK1  AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE looeAutoPrep   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chOrderNo      LIKE oe-ordl.ord-no NO-UNDO.
DEFINE VARIABLE chNewItemNo    LIKE ITEM.i-no INITIAL "" NO-UNDO.
DEFINE VARIABLE chpoCost1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE loHoldop1      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loPoQty        AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE inExpLimit     AS INTEGER   NO-UNDO INITIAL 10.
DEFINE VARIABLE deQtyComp      LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE deQtyComp1     LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE daPoDate       AS DATE      NO-UNDO.
DEFINE VARIABLE daDueDate      AS DATE      NO-UNDO.
DEFINE VARIABLE loNewAvail     AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE chJobNumber    AS CHARACTER NO-UNDO.
DEFINE VARIABLE deJobMatQty    LIKE job-mat.qty NO-UNDO.
DEFINE VARIABLE chJobMatQtyUOM LIKE job-mat.qty-uom NO-UNDO.
DEFINE VARIABLE loDrop         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chDropCustNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chShipChoice   AS CHARACTER NO-UNDO.
DEFINE VARIABLE loCanceled     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE deDimCharge    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE reJobRecid     AS RECID     NO-UNDO.
DEFINE VARIABLE chCharge       AS CHARACTER NO-UNDO.
DEFINE VARIABLE inIndex        AS INTEGER   NO-UNDO.
DEFINE VARIABLE loPOBest       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE loFromPoEntry  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE roPoOrdl       AS ROWID     NO-UNDO.
DEFINE VARIABLE roPoOrd        AS ROWID     NO-UNDO.
DEFINE VARIABLE roOeOrdl       AS ROWID     NO-UNDO.
DEFINE VARIABLE roOeOrd        AS ROWID     NO-UNDO.
DEFINE VARIABLE roJob          AS ROWID     NO-UNDO.
DEFINE VARIABLE roTT-eiv       AS ROWID     NO-UNDO.
DEFINE VARIABLE roTT-ei        AS ROWID     NO-UNDO.
DEFINE VARIABLE roItemFG       AS ROWID     NO-UNDO.
DEFINE VARIABLE chFilIdSource  AS CHARACTER NO-UNDO.
DEFINE VARIABLE roWJobMat      AS ROWID     NO-UNDO.
DEFINE VARIABLE loNextOuters   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE deOeAutoFg     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE haPOProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE scInstance     AS CLASS     system.SharedConfig NO-UNDO.

DEFINE VARIABLE loAPGL              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chAPGL              AS CHARACTER NO-UNDO.
DEFINE VARIABLE loOEAutoFg          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chOEAutoFG          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chAPTax             AS CHARACTER NO-UNDO.        
DEFINE VARIABLE chPOStatus          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chPOUom             AS CHARACTER NO-UNDO.
DEFINE VARIABLE inPOUom             AS INTEGER   NO-UNDO.
DEFINE VARIABLE loMSFcalc           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lNewVendorItemCost  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIncludeBlankVendor AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
    
DEFINE BUFFER b-item     FOR ITEM.
DEFINE BUFFER b-oe-ordl  FOR oe-ordl.
DEFINE BUFFER bf-ordl    FOR oe-ordl.
DEFINE BUFFER bf-ord     FOR oe-ord.
DEFINE BUFFER b-jc-calc  FOR reftable.
DEFINE BUFFER b-job-mat  FOR job-mat.
DEFINE BUFFER b-job-hdr  FOR job-hdr.
DEFINE BUFFER xest       FOR est.
DEFINE BUFFER xeb        FOR eb.

{sa/sa-sls01.i}
{system/VendorCostProcs.i}

DEFINE TEMP-TABLE tt-fg-set LIKE fg-set
    FIELD isaset LIKE itemfg.isaset
    FIELD alloc  LIKE itemfg.alloc
    FIELD part-qty-dec AS DECIMAL.
    
DEFINE TEMP-TABLE tt-fg-set2 LIKE tt-fg-set.

DEFINE TEMP-TABLE ttJobMaterial NO-UNDO LIKE job-mat
    FIELD w-rowid      AS ROWID
    FIELD w-recid      AS RECID
    FIELD this-is-a-rm AS LOGICAL
    FIELD isaset       AS LOGICAL
    FIELD isacomponent AS LOGICAL
    FIELD fg-i-no      LIKE job-hdr.i-no
    FIELD est-no       LIKE eb.est-no
    FIELD eqty         LIKE eb.eqty
    FIELD prep         AS LOGICAL
    FIELD estPrepEQty  AS DECIMAL
    FIELD estPrepLine  AS INTEGER
    FIELD miscType     AS INTEGER
    FIELD miscInd      AS CHARACTER
    FIELD fg-part-no   AS CHARACTER.

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

DEFINE TEMP-TABLE ttEstError
    FIELD estHeaderID AS INT64
    FIELD iFormNo     AS INTEGER
    FIELD iBlankNo    AS INTEGER
    FIELD cErrorType  AS CHARACTER
    FIELD cError      AS CHARACTER.

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
    FIELD item-type AS LOGICAL
    FIELD rec_key   AS CHARACTER
    FIELD rec-id    AS RECID
    FIELD std-uom   AS CHARACTER
    INDEX i-no    company item-type i-no    vend-no
    INDEX vend-no company i-no      vend-no.
   
ASSIGN cocode = ipchCompany.   
    
IF INDEX(PROGRAM-NAME(2),"add-po-best") GT 0 THEN
    loPOBest = YES.

IF INDEX(PROGRAM-NAME(2),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(3),"w-purord") GT 0
    OR INDEX(PROGRAM-NAME(4),"w-purord") GT 0 THEN
    ASSIGN loFromPoEntry     = TRUE.
        
IF INDEX(PROGRAM-NAME(2),"b-po-inq.w") GT 0
    OR INDEX(PROGRAM-NAME(3),"b-po-inq.w") GT 0
    OR INDEX(PROGRAM-NAME(4),"b-po-inq.w") GT 0
    OR INDEX(PROGRAM-NAME(2),"ordfrest.p") GT 0
    OR INDEX(PROGRAM-NAME(3),"ordfrest.p") GT 0
    OR INDEX(PROGRAM-NAME(4),"ordfrest.p") GT 0
     THEN
     ASSIGN lIncludeBlankVendor = FALSE. //Blank Vendor is not a valid vendor for PO

RUN ipSetGlobalSettings.
 
FIND FIRST company NO-LOCK WHERE company.company EQ ipchCompany NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-getVendCostQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVendCostQty Procedure
FUNCTION fGetVendCostQty RETURNS DECIMAL 
  (ipdQty AS DEC, ipcFromUom AS CHAR, ipcToUom AS CHAR  ) FORWARD.

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
/* fil_id = oe-ordl or job to process */
/* loOEAutoFg loAutoFgSec, whether to create and security */

IF loDebug THEN
    OUTPUT STREAM sDebug TO c:\tmp\doPoDebug.txt.

IF loDebug THEN
    PUT STREAM sDebug UNFORMATTED "Start Program " SKIP.

/*     oe-ordl buffer           */
/*     sets chOrderNo            */
/*     creates tt-fg-set        */
/*     create tt-itemfg records */
/* finds a oe-ordl or a job record matching fil_id */
RUN findOrderFromRecid (INPUT reORDRecId, 
    OUTPUT roOeOrdl, 
    OUTPUT roJob,
    OUTPUT chFilIdSource).

/*     creates tt-itemfg records */
/*     create tt-fg-set          */
/* create tt-itemfg based on a job */

IF chFilIdSource EQ "JOB" THEN
    RUN ttItemfgFromJob (INPUT roJob, INPUT roOeOrdl, INPUT chFilIdSource).

FIND oe-ordl WHERE ROWID(oe-ordl) EQ roOeOrdl NO-LOCK NO-ERROR.
FIND oe-ord WHERE ROWID(oe-ord) EQ roOeOrd NO-LOCK NO-ERROR.
FIND job WHERE ROWID(job) EQ roJob NO-LOCK NO-ERROR.

/* run several procedures to build ttJobMaterial */
RUN buildJobmat.

/* For each ttJobMaterial, process it */
RUN processJobMat.

IF loDebug THEN
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
        bf-po-ordl.dscr[1] = chPartDescr1
        bf-po-ordl.dscr[2] = chPartDescr2.
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
          ttJobMaterial
          po-ord (modified)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-ordl      FOR oe-ordl.
    
    DEFINE VARIABLE chValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE reShipTo     AS RECID     NO-UNDO.
    DEFINE VARIABLE chShipChoice AS CHARACTER LABEL "  Ship To"
        VIEW-AS RADIO-SET HORIZONTAL
        RADIO-BUTTONS "Vendor", "Vendor",
        "Customer", "Customer"
        SIZE 28 BY 1 NO-UNDO.

    DEFINE BUTTON Btn_OK AUTO-GO 
        LABEL "OK" 
        SIZE 15 BY 1
        BGCOLOR 8.

    DEFINE FRAME f-drop
        chShipChoice SKIP
        btn_ok AT ROW 2 COL 11
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER TITLE "Drop Ship PO"
        SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE.

    FIND bf-po-ord        WHERE ROWID(bf-po-ord)     EQ iprPoOrd EXCLUSIVE-LOCK NO-ERROR.
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-ordl          WHERE ROWID(bf-ordl)      EQ iprOeOrdl NO-LOCK NO-ERROR.  
  
    ON "value-changed" OF chShipChoice
        DO:
            chShipChoice = SUBSTR(chShipChoice:SCREEN-VALUE,1,1).
        END.

    ON 'choose':U OF btn_ok
        DO:
            APPLY "go" TO FRAME f-drop.
        END.

    bf-po-ord.type = "D".
    RELEASE itemfg.
    IF AVAILABLE bf-ordl AND NOT bf-ttJobMaterial.this-is-a-rm THEN
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ bf-ordl.company
            AND itemfg.i-no    EQ bf-ordl.i-no
            NO-ERROR.

    CREATE WIDGET-POOL "w-drop".

    IF chDropCustNo EQ "" THEN chShipChoice = "V".
    ELSE 
        IF NOT AVAILABLE itemfg  /* task# 09160518*/ THEN 
        DO:
            ENABLE chShipChoice btn_ok WITH FRAME f-drop.
            APPLY "value-changed" TO chShipChoice.
            APPLY "entry" TO chShipChoice.
            WAIT-FOR GO OF FRAME f-drop.
        END.

    IF chShipChoice EQ "C" THEN 
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
            IF AVAILABLE shipto THEN reShipTo = RECID(shipto).
        END. /* if avail itemfg */

        IF NOT AVAILABLE itemfg OR reShipTo EQ ? THEN  /* task# 09160518*/
            RUN windows/l-shipt2.w (ipchCompany, locode, chDropCustNo, bf-po-ord.ship-id, OUTPUT chValue, OUTPUT reShipTo).

        FIND shipto WHERE RECID(shipto) EQ reShipTo NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN 
        DO:
            ASSIGN
                bf-po-ord.cust-no      = chDropCustNo
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
            RUN pAddError(INPUT "Cust not found. Retrying...", INPUT "3", INPUT 0, INPUT 0, INPUT 0).
            
            NEXT.
        END.
    END. /* if chShipChoice eq "C" */

    ELSE
    DO WHILE TRUE:

        RUN windows/l-vendno.w (ipchCompany, "A", bf-po-ord.ship-id, OUTPUT chValue).
        IF chValue NE "" THEN 
        DO:
            bf-po-ord.ship-id = ENTRY(1,chValue).
            FIND FIRST vend NO-LOCK
                WHERE vend.company EQ ipchCompany
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
                RUN pAddError(INPUT "Error: Vendor not found retrying: " + bf-po-ord.ship-id, INPUT "3", INPUT 0, INPUT 0, INPUT 0).                
                NEXT.
            END.
        END. /* chValue ne "" */
    END. /* NOT chShipChoice eq "C" */

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
          ttJobMaterial (updated)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat    AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    DEFINE BUFFER xjob-mat   FOR job-mat.

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.    
 
    IF choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec THEN 
    DO:
        FIND FIRST b-item
            WHERE b-item.company EQ ipchCompany
            AND b-item.i-no    EQ chNewItemNo
            AND b-item.i-no    NE ""
            NO-LOCK NO-ERROR.
            
        IF AVAILABLE b-item THEN bf-po-ordl.i-no = chNewItemNo.
        FIND FIRST xjob-mat WHERE RECID(xjob-mat) EQ RECID(job-mat) NO-ERROR.
        IF AVAILABLE xjob-mat THEN ttJobMaterial.rm-i-no = chNewItemNo.

    END. /* choeAutoPoNK1 eq "AutoRM" ... */ 
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

    IF deS-Len               EQ 0                       AND
        bf-po-ordl.cons-uom    EQ "EA"                    AND
        (bf-po-ordl.item-type OR
        NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) OR
        NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-uom))   AND
        (bf-po-ordl.pr-qty-uom NE bf-po-ordl.cons-uom OR
        bf-po-ordl.pr-uom     NE bf-po-ordl.cons-uom)        THEN 
    DO:
        RUN pAddError(INPUT "Length must be entered", INPUT "3", INPUT 0, INPUT 0, INPUT 0). 
    END. /* if deS-Len eq 0 ... */
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
    
    IF loDebug THEN
        PUT STREAM sDebug UNFORMATTED "Start buildJobMat" SKIP.
  
    FIND oe-ord WHERE ROWID(oe-ord) EQ roOeOrd NO-LOCK NO-ERROR.
    FIND oe-ordl WHERE ROWID(oe-ordl) EQ roOeOrdl NO-LOCK NO-ERROR.
    FIND bf-job WHERE ROWID(bf-job) EQ roJob NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ roOeOrdl NO-LOCK NO-ERROR.
  
    IF AVAIL(oe-ordl) AND NOT AVAIL(oe-ord) THEN
        FIND oe-ord WHERE oe-ord.company EQ oe-ordl.company
            AND oe-ord.ord-no EQ oe-ordl.ord-no
            NO-LOCK NO-ERROR.
  
    /* Create ttJobMaterial from itemfg */
    IF loOEAutoFg AND loAutoFgSec AND AVAILABLE oe-ord AND AVAILABLE oe-ordl THEN
        RUN wJobFromttItemfg
            (INPUT chOEAutoFG,
             INPUT roOeOrd,
             INPUT roOeOrdl).
 
    /* Create ttJobMaterial from job-mat */
    IF AVAILABLE bf-job AND ((choeAutoPoNK1 NE "Manual" AND loAutoPoSec AND loPOBest EQ NO) OR loPOBest) THEN
        RUN wJobFromJobMat (INPUT loPOBest, INPUT ROWID(bf-job), INPUT roOeOrdl).
  
    IF AVAILABLE bf-job 
        AND looeAutoPrep AND loAutoPrepSec 
        AND AVAILABLE bf-ordl AND loPOBest EQ NO THEN 
    DO:
        /* create ttJobMaterial from job-prep */
        RUN wJobFromJobPrep (INPUT ROWID(bf-job), INPUT oe-ordl.i-no).
        /* create ttJobMaterial from b-job-mat */
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
        uses ttJobMaterial
         ipchCompany
        uses bf-ordl
         deS-wid (global)
         deS-Len (global)
         v-term (global)
         deQtyComp (global)
         deDimCharge (global)
         fil_id (global)
         deSetup (global)
         deQtyComp1 (global)
         v-vendor-chosen-report (global) not currently used
    `        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstFrm   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprTT-ei      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItemfg     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprVend       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-ordl          FOR oe-ordl.

    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCount2            AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lNew               AS LOGICAL   NO-UNDO. //launch new VendorItemCost Selector
    DEFINE VARIABLE gcScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE gcScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    
    DEFINE VARIABLE reVendorCostId     AS RECID     NO-UNDO.
    
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    /*****************************************/
    /* Create report records                 */
    /*****************************************/        
    FIND tt-ei WHERE ROWID(tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.            
    IF AVAILABLE tt-ei THEN 
    DO:        
        FOR EACH tt-eiv
            WHERE tt-eiv.company    EQ ipchCompany
            AND tt-eiv.i-no       EQ tt-ei.i-no
            AND tt-eiv.item-type  EQ bf-ttJobMaterial.this-is-a-rm
            AND tt-eiv.vend-no    NE "" 
          
            AND (tt-eiv.item-type EQ NO OR
            (deS-wid                GE tt-eiv.roll-w[27] AND
            (deS-wid                LE tt-eiv.roll-w[28] OR tt-eiv.roll-w[28] EQ 0) AND
            deS-Len                GE tt-eiv.roll-w[29] AND
            (deS-Len                LE tt-eiv.roll-w[30] OR tt-eiv.roll-w[30] EQ 0)))
            NO-LOCK,

            FIRST vend
            WHERE vend.company EQ ipchCompany
            AND vend.vend-no EQ tt-eiv.vend-no
            AND vend.active  EQ "A"
            NO-LOCK:

            DO iCount2 = 1 TO EXTENT(tt-eiv.run-qty):
                IF deQtyComp LE tt-eiv.run-qty[iCount2] THEN LEAVE.
            END.
            IF iCount2 > 20 THEN
                iCount2 = 20.
                
            deSetup = tt-eiv.setups[iCount2].
          
            deDimCharge = 0.

            RUN est/dim-charge.p (tt-eiv.rec_key,
                deS-wid,
                deS-Len,
                INPUT-OUTPUT deDimCharge).  
                   
            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = STRING(tt-eiv.run-cost[iCount2] + deDimCharge,"9999999999.9999")
                report.key-02  = STRING(deQtyComp) /* msf or tt-ei.std-uom */
                report.key-03  = tt-eiv.vend-no
                report.key-04  = STRING(deQtyComp1) /* tons */
                report.key-05  = STRING((deSetup / deQtyComp),"9999999999.9999")
                report.key-06  = STRING(deSetup,"9999999999.9999")
                report.key-07  = tt-eiv.vend-i-no
                report.key-08  = tt-eiv.i-no
                report.rec-id  = tt-eiv.rec-id.         
        END. /* for each tt-eiv */
        
        RELEASE report.
                
        IF loChoice THEN 
        DO:                 
            IF loDebug THEN
                PUT STREAM sDebug UNFORMATTED "buildRptRec - choose vendor " + bf-ttJobMaterial.i-no SKIP.
            
            IF deOeAutoFg EQ 1 THEN             
                RUN GetFirstVendCostFromReport (
                    INPUT  ipchCompany,
                    INPUT  v-term, 
                    INPUT  bf-ttJobMaterial.w-recid, 
                    INPUT  bf-ttJobMaterial.this-is-a-rm, 
                    INPUT  bf-ttJobMaterial.i-no, 
                    INPUT  deQtyComp, 
                    INPUT  chJobMatQtyUOM,
                    INPUT  lNewVendorItemCost,   /* Send true to use new VendItemCost tables, false to use old tables */
                    OUTPUT reVendorCostId,
                    OUTPUT lSuccess,
                    OUTPUT cMessage
                    ) NO-ERROR.
            ELSE 
                DO:                    
                    IF lNew THEN   //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          
                        RUN system/vendorcostSelector.w(
                            INPUT  bf-ttJobMaterial.company, //ipcCompany ,
                            INPUT  bf-ttJobMaterial.i-no , //ipcItemID
                            INPUT  IF bf-ttJobMaterial.this-is-a-rm THEN "RM" ELSE "FG", //ipcItemType ,
                            INPUT  IF bf-ttJobMaterial.this-is-a-rm THEN gcScopeRMOverride ELSE  gcScopeFGEstimated, //ipcScope ,
                            INPUT  lIncludeBlankVendor,
                            INPUT  job-hdr.e-num,  //ipcEstimateNo,
                            INPUT  bf-ttJobMaterial.frm, //ipiFormNo,
                            INPUT  bf-ttJobMaterial.blank-no, //ipiBlankNo,
                            INPUT  bf-ttJobMaterial.qty-all , //ipdQuantity ,
                            INPUT  bf-ttJobMaterial.qty-uom, //ipcQuantityUOM ,
                            INPUT  bf-ttJobMaterial.len, //ipdDimLength ,
                            INPUT  bf-ttJobMaterial.wid, //ipdDimWidth ,
                            INPUT  bf-ttJobMaterial.dep, //ipdDimDepth ,
                            INPUT  "",   //ipcDimUOM ,
                            INPUT  bf-ttJobMaterial.basis-w, //ipdBasisWeight ,
                            INPUT  "", //ipcBasisWeightUOM ,
                            OUTPUT  TABLE ttVendItemCost,
                            OUTPUT  lError ,
                            OUTPUT  cMessage 
                            ).  
                    ELSE   //---------------------------------------------------------------------------
                        DO:           
                            IF lNewVendorItemCost THEN 
                                /*Checking the new logic to call vendor selector*/
                                RUN VendorSelector(INPUT v-term, 
                                    INPUT bf-ttJobMaterial.w-recid, 
                                    INPUT bf-ttJobMaterial.this-is-a-rm, 
                                    INPUT bf-ttJobMaterial.i-no, 
                                    INPUT deQtyComp, 
                                    INPUT chJobMatQtyUOM).               
                            ELSE 
                                RUN po/d-vndcst.w (
                                    INPUT v-term, 
                                    INPUT bf-ttJobMaterial.w-recid, 
                                    INPUT bf-ttJobMaterial.this-is-a-rm, 
                                    INPUT bf-ttJobMaterial.i-no, 
                                    INPUT deQtyComp, 
                                    INPUT chJobMatQtyUOM
                                    ).
                        END.
                END.
                
            IF lNew OR lNewVendorItemCost THEN
            DO: 
                FIND FIRST ttVendItemCost WHERE ttVendItemCost.isSelected NO-ERROR.
                IF AVAILABLE ttVendItemCost THEN
                DO:                          
                    RUN RevCreateTtEiv (INPUT iprItemfg, INPUT ROWID(bf-ttJobMaterial)) NO-ERROR.
                    
                    ASSIGN
                        reVendorCostId = RECID(ttVendItemCost)
                        deItemCost    = ttVendItemCost.costPerVendorUOM
                        deSetup        = ttVendItemCost.costTotal
                        chVendItem    = ttVendItemCost.itemID.   
                        
                    FIND FIRST vend
                        WHERE vend.company EQ ipchCompany
                        AND vend.vend-no EQ ttVendItemCost.vendorID 
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE vend THEN 
                    DO:   
                        oprVend  = ROWID(vend).
                        chVendNo = vend.vend-no.  
                                
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
                END.                       
                ELSE
                    loCanceled = YES.
            END.
            ELSE
            DO: //------------------------------------------------------------------------   
                IF reVendorCostId EQ ? THEN loCanceled = YES.
                ELSE FIND report WHERE RECID(report) EQ reVendorCostId NO-LOCK NO-ERROR.
            END.
            
            IF AVAILABLE report THEN 
            DO:
                    //v-vendor-chosen-report = report.REC-ID.
                /* create tt-eiv for a specific itemfg (from e-itemfg-vend records) */
                FIND itemfg WHERE ROWID(itemfg) EQ iprItemfg NO-LOCK NO-ERROR.
          
                IF lNewVendorItemCost THEN RUN RevCreateTtEiv (INPUT iprItemfg, INPUT ROWID(bf-ttJobMaterial)) NO-ERROR.
                ELSE RUN createTtEiv (INPUT iprItemfg, INPUT ROWID(bf-ttJobMaterial)) NO-ERROR.

            END.      
      
        END. /* If loChoice = true */

        ELSE 
        DO:
            FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ chVendNo NO-ERROR.
            IF AVAILABLE tt-eiv THEN
                FIND FIRST report
                    WHERE report.term-id EQ v-term
                    AND report.rec-id  EQ tt-eiv.rec-id
                    NO-LOCK NO-ERROR.
        END. /* If not loChoice = true */

        IF AVAILABLE report THEN 
        DO:
            ASSIGN
                reVendorCostId = report.rec-id
                deItemCost     = DEC(report.key-01)                
                deSetup        = DEC(report.key-06)
                chVendItem     = report.key-07.
            
            FIND FIRST vend
                WHERE vend.company EQ ipchCompany
                AND vend.vend-no EQ report.key-03 
                NO-LOCK NO-ERROR.
            IF AVAILABLE vend THEN 
            DO:   
                oprVend = ROWID(vend).
                chVendNo = vend.vend-no.          
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


&IF DEFINED(EXCLUDE-ipPoordlUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPoordlUp Procedure
PROCEDURE ipPoordlUp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER v-recid  AS   RECID     NO-UNDO.
    DEFINE INPUT PARAMETER v-factor AS   INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER v-reopen AS   LOGical   NO-UNDO.

    FIND po-ordl WHERE RECID(po-ordl) EQ v-recid NO-ERROR.

    IF AVAILABLE po-ordl THEN 
    DO:
        {po/poordlup.i}
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-ipSetGlobalSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetGlobalSettings Procedure
PROCEDURE ipSetGlobalSettings:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE loFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE chReturn AS CHARACTER NO-UNDO.    
            
    /* POCOST */
    RUN sys/ref/nk1look.p (ipchCompany, "POCOST", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chpoCost1 = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "POCOST", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loHoldop1 = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    /* OEAUTOPO */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    choeAutoPoNK1 = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    looeAutopoNK1 = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPO", "I" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    inOeAutoPONK1 = IF loFound THEN INTEGER(chReturn) ELSE 0.
    
    /* OEAUTOPREP */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOPREP", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    looeAutoPrep = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    /* POSTATUS */
    RUN sys/ref/nk1look.p (ipchCompany, "POSTATUS", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chPOStatus = IF loFound THEN chReturn ELSE "".
    
    /* POQTY */
    RUN sys/ref/nk1look.p (ipchCompany, "POQTY", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loPoQty = NOT(loFound AND chReturn EQ "Net Shts").
        
    /* "AP GL#" */
    RUN sys/ref/nk1look.p (ipchCompany, "AP GL#", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chAPGL = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "AP GL#", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loAPGL = IF loFound AND chReturn = "Yes" THEN YES ELSE NO.
    
    /* OEAUTOFG */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "C" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    chOEAutoFG = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "L" , NO, NO, "","", OUTPUT chReturn, OUTPUT loFound).
    loOEAutoFg = IF loFound AND chReturn = "YES" THEN YES ELSE NO.
    
    /* OEAUTOFG */
    RUN sys/ref/nk1look.p (ipchCompany, "OEAUTOFG", "D", NO, YES, "", "", OUTPUT chReturn, OUTPUT loFound).
    deOeAutoFg = IF loFound THEN DECIMAL(chReturn) ELSE 0. 
        
    /* POUOM */
    RUN sys/ref/nk1look.p (ipchCompany, "POUOM", "C" , NO, No, "","", OUTPUT chReturn, OUTPUT loFound).
    chPOUom = IF loFound THEN chReturn ELSE "".
    
    RUN sys/ref/nk1look.p (ipchCompany, "POUOM", "I" , NO, No, "","", OUTPUT chReturn, OUTPUT loFound).
    inPOUom = IF loFound THEN  INTEGER(chReturn) ELSE 0. 
    
    /* APTAX */
    RUN sys/ref/nk1look.p (ipchCompany, "APTAX", "C" , NO, No, "","", OUTPUT chReturn, OUTPUT loFound).
    chAPTax = IF loFound THEN chReturn ELSE "".
    
    /* MSFCALC */
    RUN sys/ref/nk1look.p (ipchCompany, "MSFCALC", "C" , NO, No, "","", OUTPUT chReturn, OUTPUT loFound).
    loMSFcalc = loFound AND chReturn EQ "Corrware".
    
    /* VendItemCost */
    RUN sys/ref/nk1look.p (ipchCompany, "VendItemCost", "L", NO, NO, "", "", OUTPUT chReturn, OUTPUT loFound).
    lNewVendorItemCost = IF loFound AND chReturn = "YES" THEN YES ELSE NO.

    IF loFromPoEntry THEN /* Security check only for order entry */
        ASSIGN 
            loAutoPoSec   = TRUE
            loAutoPrepSec = TRUE
            loAutoFgSec   = TRUE.
    ELSE 
    DO:
        /* Check if authorized to create PO's */
        RUN methods/prgsecur.p
            (INPUT "OEAutoPO",
            INPUT "ALL",
            INPUT NO,
            INPUT NO,
            INPUT NO,
            OUTPUT loAutoPoSec,
            OUTPUT loAccessClose,
            OUTPUT chAccessList).
            
        /* Check if authorized to create PO's */
        IF looeAutoPrep THEN
            RUN methods/prgsecur.p
                (INPUT "OEAutoPrep", /* program */
                INPUT "ALL",        /*Basis */
                INPUT NO,
                INPUT NO,
                INPUT NO,
                OUTPUT loAutoPrepSec,
                OUTPUT loAccessClose,
                OUTPUT chAccessList).
            
        /* Check if authorized to create PO's */
        IF loOEAutoFg THEN
            RUN methods/prgsecur.p
                (INPUT "OEAutoFG", /* Program master program name */
                INPUT "ALL",      /* Security based on */
                INPUT NO,
                INPUT NO,
                INPUT NO,
                OUTPUT loAutoFgSec,
                OUTPUT loAccessClose,
                OUTPUT chAccessList).        
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-test) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test Procedure
PROCEDURE ipFullSet:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
     Source: Copy of fg/fullset.p (ROWID(itemfg)).     
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.    

    DEFINE VARIABLE v-part-qty-dec LIKE tt-fg-set.part-qty-dec NO-UNDO.
    DEFINE VARIABLE v-i-no         LIKE itemfg.i-no NO-UNDO.

    FOR EACH tt-fg-set:
        DELETE tt-fg-set.
    END.

    FIND itemfg NO-LOCK 
        WHERE ROWID(itemfg) EQ ip-rowid NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST fg-set NO-LOCK 
            WHERE fg-set.company EQ ipchCompany 
              AND fg-set.set-no  EQ itemfg.i-no 
              AND fg-set.part-no EQ itemfg.i-no NO-ERROR.

        IF AVAILABLE fg-set THEN 
        DO:
            {sys/inc/part-qty.i v-part-qty-dec fg-set}
        END.

        CREATE tt-fg-set.
        ASSIGN
            tt-fg-set.part-no      = itemfg.i-no
            tt-fg-set.qtyPerSet    = IF AVAILABLE fg-set THEN fg-set.qtyPerSet ELSE 1
            tt-fg-set.part-qty-dec = IF AVAILABLE fg-set THEN v-part-qty-dec ELSE 1
            tt-fg-set.isaset       = NOT AVAILABLE fg-set
            tt-fg-set.alloc        = NOT AVAILABLE fg-set.
    END.

    DO WHILE CAN-FIND(FIRST tt-fg-set WHERE 
        tt-fg-set.set-no NE tt-fg-set.part-no AND 
        tt-fg-set.isaset AND tt-fg-set.alloc):
        FOR EACH tt-fg-set WHERE 
            tt-fg-set.isaset AND tt-fg-set.alloc:
            FOR EACH fg-set NO-LOCK WHERE 
                fg-set.company EQ ipchCompany AND 
                fg-set.set-no  EQ tt-fg-set.part-no AND 
                fg-set.set-no  NE fg-set.part-no,
                FIRST itemfg NO-LOCK WHERE 
                itemfg.company EQ ipchCompany AND 
                itemfg.i-no    EQ fg-set.part-no:

                {sys/inc/part-qty.i v-part-qty-dec fg-set}

                CREATE tt-fg-set2.
                BUFFER-COPY fg-set TO tt-fg-set2
                    ASSIGN
                    tt-fg-set2.qtyPerSet     = tt-fg-set.qtyPerSet * fg-set.qtyPerSet
                    tt-fg-set2.part-qty-dec = tt-fg-set.part-qty-dec * v-part-qty-dec
                    tt-fg-set2.isaset       = itemfg.isaset
                    tt-fg-set2.alloc        = itemfg.alloc.
            END.
            DELETE tt-fg-set.
        END.

        FOR EACH tt-fg-set2:
            CREATE tt-fg-set.
            BUFFER-COPY tt-fg-set2 TO tt-fg-set.
            DELETE tt-fg-set2.
        END.
    END.

    FOR EACH tt-fg-set 
        BREAK BY tt-fg-set.part-no:
        IF FIRST-OF(tt-fg-set.part-no) THEN ASSIGN
                v-part-qty-dec = 0.
        ASSIGN
            v-part-qty-dec = v-part-qty-dec + tt-fg-set.part-qty-dec.
        IF LAST-OF(tt-fg-set.part-no) THEN ASSIGN
                tt-fg-set.part-qty-dec = v-part-qty-dec.
        ELSE 
            DELETE tt-fg-set.
    END.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pAddError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddError Procedure
PROCEDURE pAddError:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ipcError         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstHeaderID   AS INT64     NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo       AS INTEGER   NO-UNDO.

    CREATE ttEstError.
    ASSIGN
        ttEstError.cError      = ipcError
        ttEstError.cErrorType  = ipcErrorType
        ttEstError.estHeaderID = ipiEstHeaderID
        ttEstError.iFormNo     = ipiFormNo
        ttEstError.iBlankNo    = ipiBlankNo.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


PROCEDURE vendorSelector:
    DEFINE INPUT PARAMETER ipcterm        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipRiJobMat     AS RECID     NO-UNDO.
    DEFINE INPUT PARAMETER iplRM          AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipci-no        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty-comp    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcJob-mat-uom AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cEstimateNo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cScopeRMOverride  AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - RM Override".
    DEFINE VARIABLE cScopeFGEstimated AS CHARACTER NO-UNDO INITIAL "Effective and Not Expired - FG Estimated".
    DEFINE VARIABLE iCount            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAdderList        AS CHARACTER NO-UNDO EXTENT 6.
    
    DEFINE BUFFER bf-ef FOR ef.
    
    /*run vendorCostselector.w*/
    FIND FIRST job-mat NO-LOCK 
        WHERE RECID(job-mat) = ipRiJobMat NO-ERROR.
        
    IF AVAILABLE job-mat THEN
    DO:
        FOR FIRST job NO-LOCK 
            WHERE job.company = job-mat.company
            AND job.job = job-mat.job:
            ASSIGN 
                cEstimateNo = job.est-no.
        END.
        
        IF cEstimateNo <> "" THEN
            FOR EACH bf-ef NO-LOCK
                WHERE bf-ef.company EQ job-mat.company
                AND bf-ef.est-no  EQ cEstimateNo:
                DO iCount = 1 TO 6:
                    IF bf-ef.adder[iCount] <> "" THEN 
                        cAdderList[iCount] = bf-ef.adder[iCount].
                END.
            END.                    
     
        RUN system/vendorcostSelector.w(
            INPUT  job-mat.company, //ipcCompany ,
            INPUT  job-mat.i-no ,
            INPUT  IF iplRM THEN "RM" ELSE "FG", //ipcItemType ,
            INPUT  IF iplRM THEN cScopeRMOverride ELSE cScopeFGEstimated, //ipcScope ,
            INPUT  lIncludeBlankVendor,
            INPUT  cEstimateNo, //ipcEstimateNo,
            INPUT  job-mat.frm, //ipiFormNo,
            INPUT  job-mat.blank-no, //ipiBlankNo,
            INPUT  job-mat.qty , //ipdQuantity ,
            INPUT  Job-mat.qty-UOM, //ipcQuantityUOM ,
            INPUT  job-mat.Len, //ipdDimLength ,
            INPUT  job-mat.Wid, //ipdDimWidth ,
            INPUT  job-mat.Dep, //ipdDimDepth ,
            INPUT  job-mat.sc-UOM, //ipcDimUOM ,
            INPUT  Job-mat.basis-W, //ipdBasisWeight ,
            INPUT  "", //ipcBasisWeightUOM ,
            INPUT cAdderList, //adders
            OUTPUT  TABLE ttVendItemCost,
            OUTPUT  lError ,
            OUTPUT  cMessage).
    END. 
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
        globals: chpoCost1, debasis-w, deS-wid, deS-Len, deS-dep
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprtt-ei    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER bf-tt-ei   FOR tt-ei.
     
    FIND bf-tt-ei WHERE ROWID(bf-tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.item-type AND chpoCost1 EQ "Vendor/MSH" AND AVAILABLE bf-tt-ei AND
       bf-tt-ei.std-uom EQ "TON" AND debasis-w NE 0 AND deS-wid NE 0 THEN 
    DO:
        RUN sys/ref/convcuom.p (bf-tt-ei.std-uom, "MSH",
            debasis-w, deS-Len, deS-wid, deS-dep,
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
          ttJobMaterial
          chVendNo
          chVendItem
          po-ordl (updates)
    
    ------------------------------------------------------------------------------*/
  
    DEFINE INPUT  PARAMETER iprTT-ei      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendNo     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendItem   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-tt-ei         FOR tt-ei.  
  
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-tt-ei WHERE ROWID(bf-tt-ei) EQ iprTT-ei NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAILABLE bf-tt-ei THEN
        FIND FIRST bf-tt-ei
            WHERE bf-tt-ei.company EQ ipchCompany
            AND bf-tt-ei.i-no    EQ bf-ttJobMaterial.rm-i-no
            NO-LOCK NO-ERROR.

    IF NOT AVAILABLE tt-eiv THEN
        FIND FIRST tt-eiv
            WHERE tt-eiv.company   EQ bf-ttJobMaterial.company
            AND tt-eiv.i-no      EQ bf-ttJobMaterial.i-no
            AND tt-eiv.vend-no   EQ chVendNo
            NO-LOCK NO-ERROR.
  
    IF AVAILABLE bf-tt-ei  THEN 
    DO:  
        ASSIGN
            bf-po-ordl.cost      = deItemCost
            bf-po-ordl.setup     = deSetup     
            bf-po-ordl.vend-i-no = chVendItem
            bf-po-ordl.pr-uom = bf-tt-ei.std-uom.
    END. /* Avail bf-tt-ei */
    ELSE
        ASSIGN
            bf-po-ordl.cost   = bf-ttJobMaterial.std-cost
            bf-po-ordl.setup  = 0
            bf-po-ordl.pr-uom = bf-ttJobMaterial.sc-uom.
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
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat EXCLUSIVE-LOCK NO-ERROR.

    IF deOeAutoFg EQ 1 THEN
        daPoDate = TODAY.
    ELSE
        RUN po/d-podate.w (
            INPUT        "PO",
            INPUT-OUTPUT daPoDate,
            INPUT        chJobNumber,
            INPUT        bf-ttJobMaterial.frm,
            INPUT        bf-ttJobMaterial.rm-i-no
            ).
        
    IF daDueDate LE daPoDate THEN 
        daDueDate = daPoDate + 1.
    
    IF deOeAutoFg EQ 1 THEN
        daDueDate = TODAY + 1.  
    ELSE    
        RUN po/d-podate.w (
            INPUT        "Due",
            INPUT-OUTPUT daDueDate, 
            INPUT        chJobNumber, 
            INPUT        bf-ttJobMaterial.frm, 
            INPUT        bf-ttJobMaterial.rm-i-no
            ).
            
    RELEASE bf-ttJobMaterial.

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
          loPoQty
          ttJobMaterial
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

    DEFINE BUFFER bf-po-ordl        FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial  FOR ttJobMaterial.
    DEFINE BUFFER bf-ordl           FOR oe-ordl.  
    DEFINE BUFFER bf-item           FOR ITEM.
    DEFINE BUFFER bf2-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job-hdr        FOR job-hdr.
    DEFINE BUFFER bf2-ttJobMaterial FOR ttJobMaterial.
    
    DEFINE VARIABLE deLineQty LIKE po-ordl.ord-qty NO-UNDO.
    DEFINE VARIABLE dePartQty AS DECIMAL NO-UNDO.

    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND bf-item WHERE ROWID(bf-item) EQ iprItem NO-LOCK NO-ERROR.

    IF reJobRecid NE ? THEN
        FIND job-hdr 
            WHERE RECID(job-hdr) EQ reJobRecid
            NO-LOCK.
    FIND FIRST est
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
        NO-LOCK NO-ERROR.
    IF bf-po-ordl.item-type THEN 
    DO:
        /* S-8-POQTY JOBQTY or NETSHTS */

        IF loPoQty OR bf-ttJobMaterial.n-up EQ 0 OR INDEX("BP",bf-item.mat-type) LE 0 THEN  
        DO:       
            deLineQty = bf-ttJobMaterial.qty.  /* Job Qty */
        END.
        ELSE 
        DO:
            ASSIGN
                deLineQty = IF AVAILABLE bf-ordl THEN
                        bf-ordl.qty
                     ELSE
                     IF AVAILABLE job-hdr THEN
                        job-hdr.qty
                     ELSE
                        bf-ttJobMaterial.qty
                dePartQty = 0.                  

            IF AVAILABLE est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN 
            DO:
     
                FOR EACH eb FIELDS(quantityPerSet)
                    WHERE eb.company EQ job.company
                    AND eb.est-no  EQ job.est-no
                    AND eb.form-no EQ bf-ttJobMaterial.frm
                    NO-LOCK:
                    dePartQty = dePartQty +
                        (deLineQty * IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet)
                        ELSE eb.quantityPerSet).
                END. /* Each eb */
         
            END.
            ELSE  IF AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN 
                DO:
                    deLineQty = 0.
        
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
                                deLineQty = deLineQty + bf2-ordl.qty.

                        END. /* each bf-job-hdr */         
          
                    END. /* oe-ordl was available */
                    ELSE IF AVAILABLE job-hdr THEN 
                        DO:
                            FOR EACH bf-job-hdr WHERE bf-job-hdr.company EQ job-hdr.company
                                NO-LOCK:
                                deLineQty = deLineQty + bf-job-hdr.qty.
                            END. /* each bf-job-hdr */          
                        END. /* job-hdr was available */
                        ELSE IF AVAILABLE  bf-ttJobMaterial THEN 
                            DO:
                                FOR EACH bf2-ttJobMaterial WHERE bf2-ttJobMaterial.i-no EQ bf-ttJobMaterial.i-no:
                                    deLineQty = deLineQty + (bf2-ttJobMaterial.qty * bf2-ttJobMaterial.n-up).
                                END.          
                            END.
          
                    deLineQty = deLineQty / bf-ttJobMaterial.n-up.

                END.
                ELSE 
                    dePartQty = deLineQty.

            IF NOT(AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8)) THEN
                deLineQty = dePartQty / bf-ttJobMaterial.n-up.

            IF bf-po-ordl.pr-qty-uom EQ "EA" THEN 
            DO:
            {sys/inc/roundup.i deLineQty}             
            END.
        END. /* NOT loPoQty OR bf-ttJobMaterial.n-up EQ 0 OR ... */

        IF bf-po-ordl.pr-qty-uom NE "EA" THEN
            RUN sys/ref/convquom.p (bf-ttJobMaterial.qty-uom,bf-po-ordl.pr-qty-uom,
                bf-ttJobMaterial.basis-w, bf-ttJobMaterial.len, bf-ttJobMaterial.wid, bf-ttJobMaterial.dep,
                deLineQty, OUTPUT deLineQty).

        bf-po-ordl.ord-qty = deLineQty.
    END. /* If po-ordl.item-type */
    RELEASE bf-po-ordl.
    RELEASE job.
    RELEASE job-hdr.
    RELEASE bf-ttJobMaterial.
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

    DEFINE VARIABLE deOrderQty              LIKE po-ordl.ord-qty.
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
        deOrderQty = bf-po-ordl.ord-qty.

        IF bf-po-ordl.pr-qty-uom NE bf-po-ordl.pr-uom            AND
            (bf-po-ordl.item-type                           OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-uom))   THEN

            RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, bf-po-ordl.pr-uom,
                debasis-w, deS-Len, deS-wid, deS-dep,
                deOrderQty, OUTPUT deOrderQty).
 
        bf-po-ordl.t-cost = (deOrderQty * bf-po-ordl.cost) + bf-po-ordl.setup.
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
PROCEDURE calcLenWid:
    /*------------------------------------------------------------------------------
          Purpose:     Calculate len & width values 
          Parameters:  <none>
          Notes:       
            INputs:
              po-ordl
              b-item
              po-ord
              
        ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem   AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER b-item     FOR ITEM.
    
    DEFINE VARIABLE chVendItemNo AS CHARACTER NO-UNDO.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.
    FIND b-item WHERE ROWID(b-item) EQ iprItem NO-LOCK NO-ERROR.

    ASSIGN
        deS-Len = 0
        deS-wid = 0
        deS-dep = 0.

    IF AVAILABLE b-item THEN 
    DO:
        RUN Vendor_GetVendorItemNumber(INPUT ipchCompany, 
                                       INPUT bf-po-ordl.i-no,
                                       INPUT bf-po-ord.vend-no, 
                                       INPUT lNewVendorItemCost,
                                       OUTPUT chVendItemNo).
                                       
        IF chVendItemNo NE "" THEN
            bf-po-ordl.vend-i-no = chVendItemNo.
        ELSE
            IF b-item.vend-no EQ bf-po-ord.vend-no THEN
                bf-po-ordl.vend-i-no = b-item.vend-item.
            ELSE
                IF b-item.vend2-no EQ bf-po-ord.vend-no THEN
                    bf-po-ordl.vend-i-no = b-item.vend2-item.

        IF INDEX("1234BPR",b-item.mat-type) GT 0 THEN 
        DO:
            ASSIGN
                debasis-w = b-item.basis-w
                deS-Len     = b-item.s-len
                deS-wid     = b-item.s-wid
                deS-dep     = b-item.s-dep.
     
            IF deS-wid EQ 0 THEN deS-wid = b-item.r-wid.
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
          globals: v-tot-msf, deS-Len, deS-wid
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.

    DEFINE VARIABLE v-tot-msf    AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
    DEFINE BUFFER bf-po-ordl    FOR po-ordl.  

    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.

    IF bf-po-ordl.pr-qty-uom EQ "EA" THEN
        v-tot-msf = IF loMSFcalc THEN ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) * .007) *
                                          bf-po-ordl.ord-qty) / 1000)
                    ELSE ((((bf-po-ordl.s-len * bf-po-ordl.s-wid) / 144) *
                             bf-po-ordl.ord-qty) / 1000).
    ELSE
        v-tot-msf = 0.  // Varun - No use of this variable


    /** Appears to be here so that it is assigned after the Total MSF Calculation */
    ASSIGN
        bf-po-ordl.s-len = deS-Len
        bf-po-ordl.s-wid = deS-wid.
        
    IF deS-dep GT 0 THEN DO:        
        bf-po-ordl.s-dep = deS-dep.
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
        ttJobMaterial
        po-ordl (updates)
        globals vars: debasis-w, deS-Len, deS-wid, deS-dep    
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrdl  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
  
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND bf-po-ordl WHERE ROWID(bf-po-ordl) EQ iprPoOrdl EXCLUSIVE-LOCK NO-ERROR.  
    IF NOT AVAILABLE bf-ttJobMaterial OR NOT AVAILABLE bf-po-ordl THEN 
    DO:
        FIND CURRENT bf-po-ordl NO-LOCK NO-ERROR.
        RETURN ERROR.
    END.

    ASSIGN
        deS-Len = bf-ttJobMaterial.len
        deS-wid = bf-ttJobMaterial.wid
        deS-dep = bf-ttJobMaterial.dep.

    IF bf-po-ordl.s-num EQ 0 AND bf-po-ordl.b-num EQ 0 THEN
        ASSIGN bf-po-ordl.s-num = bf-ttJobMaterial.frm
            bf-po-ordl.b-num = bf-ttJobMaterial.blank-no.

    IF bf-po-ordl.pr-qty-uom EQ "BF" THEN 
    DO:
        RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, "EA",
            debasis-w, deS-Len, deS-wid, deS-dep,
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
    
    DEFINE VARIABLE chMessageText AS CHARACTER NO-UNDO.
    
    ASSIGN chMessageText = "Cannot create a PO for " +
                            (IF AVAILABLE job THEN ("Job/Form/RM#: " + TRIM(chJobNumber) + "/" +
                            TRIM(STRING(ttJobMaterial.frm,"99")))
                            ELSE ("Order/FG#: " + TRIM(STRING(chOrderNo,">>>>>>>>>>")))) +
                            "/" + TRIM(ttJobMaterial.rm-i-no) + ", Vendor Matrix does not exist...".
                     
    RUN pAddError(INPUT chMessageText, INPUT "3", INPUT 0, INPUT 0, INPUT 0).
        
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
    DEFINE INPUT PARAMETER iprPoOrdl AS ROWID       NO-UNDO.

    DEFINE VARIABLE deHoldCost   AS DECIMAL FORMAT "->>>>>>>9.99<<".  //Varun - We can remove
    DEFINE VARIABLE dehdeLineQty AS DECIMAL FORMAT "->>>>>>>>9.99<<". //Varun - We can remove
    
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-po-ord        FOR po-ord.  
    DEFINE BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.  

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
    IF NOT bf-po-ord.received AND (dehdeLineQty EQ 0 OR deHoldCost EQ 0) THEN 
    DO:
        IF bf-po-ordl.pr-qty-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                       AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.cons-uom))     THEN
            bf-po-ordl.cons-qty = bf-po-ordl.ord-qty.
        ELSE
            RUN sys/ref/convquom.p(bf-po-ordl.pr-qty-uom, bf-po-ordl.cons-uom,
                debasis-w, deS-Len, deS-wid, deS-dep,
                bf-po-ordl.ord-qty, OUTPUT bf-po-ordl.cons-qty).
      
        IF bf-po-ordl.pr-uom EQ bf-po-ordl.cons-uom           OR
            (NOT bf-po-ordl.item-type                     AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-uom) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.cons-uom))   THEN
            bf-po-ordl.cons-cost = bf-po-ordl.cost.
        ELSE
            RUN sys/ref/convcuom.p(bf-po-ordl.pr-uom, bf-po-ordl.cons-uom,
                debasis-w, deS-Len, deS-wid, deS-dep,
                bf-po-ordl.cost, OUTPUT bf-po-ordl.cons-cost).

        /*FG*/
        /**************************************/
        /* Calculate oe-ordl.cost             */
        /*************************************/
        IF NOT bf-po-ordl.item-type THEN
        DO:
            FIND oe-ordl WHERE RECID(oe-ordl) EQ reORDRecId EXCLUSIVE-LOCK NO-ERROR.
      
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
    DEFINE INPUT  PARAMETER iprOeOrd       AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd       AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oploNextOuters AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE iCount3 AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCnt    AS INTEGER NO-UNDO.
    DEFINE VARIABLE fil_id  AS RECID   NO-UNDO.
    
    DEFINE BUFFER bf-ord FOR oe-ord.
    
    FIND bf-ord WHERE ROWID(bf-ord) EQ iprOeOrd NO-LOCK NO-ERROR.
    
    oploNextOuters = NO.
    
    /* outers label is needed for po-ord.a 
       if a 'next' was done in po/po-ord.a then
       iCnt will become = 2 and routine will return */
    outers:
    REPEAT iCnt = 1 TO 2:
        IF iCnt NE 1 THEN 
        DO:
            ASSIGN 
                oploNextOuters = TRUE.
            LEAVE.
        END.
        /* Requires ipchCompany, sets fil_id to new po-ord */
        {po/po-ord.a}
        LEAVE.
    END.
    
    IF oploNextOuters THEN
        RETURN.

    ASSIGN
        po-ord.po-date        = daPoDate
        po-ord.due-date       = daDueDate
        po-ord.last-ship-date = po-ord.due-date
        po-ord.vend-no        = chVendNo.
  
    IF AVAILABLE bf-ord THEN
        ASSIGN
            chDropCustNo = bf-ord.cust-no
            chShipChoice = "C". /* task# 09160518*/

    roPoOrd = ROWID(po-ord).
  
    /* Prompt for drop ship shipto and assign po-ord ship fields */
    IF loDrop THEN 
    DO: 
        RUN askDropShip (INPUT roPoOrd,
            INPUT roWJobMat,
            INPUT roOeOrdl) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            RUN pAddError(INPUT "ASI Error message: Not able to run askDropShip", INPUT "3", INPUT 0, INPUT 0, INPUT 0).            
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
        ttJobMaterial
        ipchCompany
        loPOBest
        po-ord (for po-ordl.a)
        vend (found in po-ordl.a)
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItemfg   AS ROWID       NO-UNDO.

    DEFINE VARIABLE iCount4        AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-itemfg        FOR Itemfg.

    FIND ITEM WHERE ROWID(ITEM) EQ iprItem NO-LOCK NO-ERROR.
    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND po-ord WHERE ROWID(po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.
    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
        FIND b-item WHERE RECID(b-item) EQ RECID(item) NO-LOCK.

    loNewAvail = NO.
    /* If bf-itemfg found, then this is an FG item and don't join on frm */
    /* 05281404 - added join to rm-i-no */
    FIND FIRST bf-itemfg
        WHERE bf-itemfg.company EQ bf-ordl.company
        AND bf-itemfg.i-no      EQ /* wfk - 05281404 - bf-ordl.i-no */ bf-ttJobMaterial.rm-i-no
        NO-LOCK NO-ERROR.
              
    IF NOT AVAILABLE po-ordl AND choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec AND bf-ttJobMaterial.this-is-a-rm THEN 
    DO: 
        FIND FIRST po-ordl EXCLUSIVE-LOCK 
            WHERE po-ordl.company EQ ipchCompany
            AND po-ordl.job-no  EQ bf-ttJobMaterial.job-no
            AND po-ordl.job-no2 EQ bf-ttJobMaterial.job-no2
            AND po-ordl.s-num   EQ bf-ttJobMaterial.frm
            AND po-ordl.i-no    EQ
            IF LENGTH(bf-ttJobMaterial.i-no) LE 10 THEN bf-ttJobMaterial.i-no
            ELSE substr(bf-ttJobMaterial.i-no,LENGTH(bf-ttJobMaterial.i-no) - 9,10)
            NO-ERROR.
        
        IF AVAILABLE po-ordl THEN 
        DO:
            FIND FIRST b-item
                WHERE b-item.company EQ ipchCompany
                AND b-item.i-no    EQ
                IF LENGTH(bf-ttJobMaterial.i-no) LE 10 THEN bf-ttJobMaterial.i-no
                ELSE substr(bf-ttJobMaterial.i-no,LENGTH(bf-ttJobMaterial.i-no) - 9,10)
                NO-LOCK NO-ERROR.
            
            IF AVAILABLE b-item THEN loNewAvail = YES.
            ELSE
            DO:
                IF bf-ttJobMaterial.prep EQ NO THEN
                DO:
                    IF loPOBest EQ NO THEN
                        FIND FIRST b-item
                            WHERE b-item.company  EQ ipchCompany 
                            AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no 
                            AND index("1234BPR",b-item.mat-type) GT 0 
                            NO-LOCK NO-ERROR.
                    ELSE
                        FIND FIRST b-item
                            WHERE b-item.company  EQ ipchCompany 
                            AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no 
                            AND b-item.mat-type EQ "B" 
                            NO-LOCK NO-ERROR.
                END. /* if bf-ttJobMaterial.prep eq no */
                ELSE
                    FIND FIRST b-item
                        WHERE b-item.company  EQ ipchCompany 
                        AND b-item.i-no     EQ bf-ttJobMaterial.rm-i-no
                        NO-LOCK NO-ERROR.
            END. /* not avail b-item */
        END. /* if avail po-ordl */
    END. /* Not avail po-ordl and this is RM */
  
    IF NOT AVAILABLE po-ordl THEN 
    DO:
    {po/po-ordl.a}
        ASSIGN
            po-ordl.tax = po-ord.tax-gr NE "" AND
                         (chAPTax EQ "Vendor" OR 
                          (chAPTax EQ "Item" AND
                           (AVAILABLE b-item AND b-item.tax-rcpt) OR
                           (AVAILABLE itemfg AND itemfg.taxable)))
            po-ordl.item-type = bf-ttJobMaterial.this-is-a-rm.
    END. /* Not avail po-ordl then add it */

    IF AVAILABLE bf-itemfg THEN
        oprItemfg = ROWID(bf-itemfg).

    IF AVAILABLE po-ordl THEN
        roPoOrdl = ROWID(po-ordl).
    FIND CURRENT po-ordl NO-LOCK NO-ERROR.
    RELEASE po-ordl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createTtEiv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtEiv Procedure 
PROCEDURE createTtEiv:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprItemfg  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat
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

        IF bf-ttJobMaterial.est-no NE "" THEN 
        DO:
            FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company  EQ itemfg.company
                AND e-itemfg-vend.est-no   EQ bf-ttJobMaterial.est-no
                AND e-itemfg-vend.form-no  EQ bf-ttJobMaterial.frm
                AND e-itemfg-vend.blank-no EQ bf-ttJobMaterial.blank-no
                BREAK BY e-itemfg-vend.eqty:
                   
                IF LAST(e-itemfg-vend.eqty)            OR
                    e-itemfg-vend.eqty GE bf-ttJobMaterial.qty THEN 
                DO:
                    FIND CURRENT bf-ttJobMaterial EXCLUSIVE-LOCK.
                    bf-ttJobMaterial.eqty = e-itemfg-vend.eqty.
                    FIND CURRENT bf-ttJobMaterial NO-LOCK.
                    LEAVE.
                END.
            END.
           
           
            FOR EACH e-itemfg-vend NO-LOCK
                WHERE e-itemfg-vend.company  EQ itemfg.company
                AND e-itemfg-vend.est-no   EQ bf-ttJobMaterial.est-no
                AND e-itemfg-vend.eqty     EQ bf-ttJobMaterial.eqty
                AND e-itemfg-vend.form-no  EQ bf-ttJobMaterial.frm
                AND e-itemfg-vend.blank-no EQ bf-ttJobMaterial.blank-no:
                IF NOT CAN-FIND(FIRST tt-eiv
                    WHERE tt-eiv.company   EQ e-itemfg-vend.company
                    AND tt-eiv.i-no      EQ bf-ttJobMaterial.i-no
                    AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                DO:
                    CREATE tt-eiv.
                    
                    ASSIGN
                        tt-eiv.rec-id    = RECID(e-itemfg-vend)
                        tt-eiv.est-no    = ""
                        tt-eiv.i-no      = bf-ttJobMaterial.i-no
                        tt-eiv.form-no   = 0
                        tt-eiv.blank-no  = 0
                        tt-eiv.company   = e-itemfg-vend.company
                        tt-eiv.vend-no   = e-itemfg-vend.vend-no
                        tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                        tt-eiv.item-type = e-itemfg-vend.item-type
                        tt-eiv.rec_key   = e-itemfg-vend.rec_key.
                    roTT-eiv = ROWID(tt-eiv).
                    DO inIndex = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[inIndex]  = e-itemfg-vend.run-qty[inIndex]
                            tt-eiv.run-cost[inIndex] = e-itemfg-vend.run-cost[inIndex]
                            tt-eiv.setups[inIndex]   = e-itemfg-vend.setups[inIndex]
                            tt-eiv.roll-w[inIndex]   = e-itemfg-vend.roll-w[inIndex].
                    END. /* do inIndex ... */
                   
                    DO inIndex = 11 TO 30:
                        tt-eiv.roll-w[inIndex] = e-itemfg-vend.roll-w[inIndex].
                    END. /* do inIndex ... */
                END. /* can-find(first tt-eiv ... */
            END. /* for each itemfg-vend */
        END. /* ttJobMaterial.est-no NE "" */
        
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
                    roTT-eiv = ROWID(tt-eiv).
                    DO inIndex = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[inIndex]  = e-itemfg-vend.run-qty[inIndex]
                            tt-eiv.run-cost[inIndex] = e-itemfg-vend.run-cost[inIndex]
                            tt-eiv.setups[inIndex]   = e-itemfg-vend.setups[inIndex]
                            tt-eiv.roll-w[inIndex]   = e-itemfg-vend.roll-w[inIndex].
                    END.
          
                    DO inIndex = 11 TO 30:
                        tt-eiv.roll-w[inIndex] = e-itemfg-vend.roll-w[inIndex].
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
        ipchCompany
        ttJobMaterial
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat
        NO-ERROR.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipchCompany
        AND itemfg.i-no    EQ bf-ttJobMaterial.rm-i-no
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


            IF bf-ttJobMaterial.est-no NE "" THEN 
            DO:
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company  EQ itemfg.company
                    AND e-itemfg-vend.est-no   EQ bf-ttJobMaterial.est-no
                    AND e-itemfg-vend.form-no  EQ bf-ttJobMaterial.frm
                    AND e-itemfg-vend.blank-no EQ bf-ttJobMaterial.blank-no
                    BREAK BY e-itemfg-vend.eqty:
                    IF LAST(e-itemfg-vend.eqty)            OR
                        e-itemfg-vend.eqty GE bf-ttJobMaterial.qty THEN 
                    DO:
                        FIND CURRENT bf-ttJobMaterial EXCLUSIVE-LOCK.
                        bf-ttJobMaterial.eqty = e-itemfg-vend.eqty.
                        FIND CURRENT bf-ttJobMaterial NO-LOCK.
                        LEAVE.
                    END.
                END.
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company  EQ itemfg.company
                    AND e-itemfg-vend.est-no   EQ bf-ttJobMaterial.est-no
                    AND e-itemfg-vend.eqty     EQ bf-ttJobMaterial.eqty
                    AND e-itemfg-vend.form-no  EQ bf-ttJobMaterial.frm
                    AND e-itemfg-vend.blank-no EQ bf-ttJobMaterial.blank-no:
                    IF NOT CAN-FIND(FIRST tt-eiv
                        WHERE tt-eiv.company   EQ e-itemfg-vend.company
                        AND tt-eiv.i-no      EQ bf-ttJobMaterial.i-no
                        AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
                    DO:
                        CREATE tt-eiv.

                        ASSIGN
                            tt-eiv.rec-id    = RECID(e-itemfg-vend)
                            tt-eiv.est-no    = ""
                            tt-eiv.i-no      = bf-ttJobMaterial.i-no
                            tt-eiv.form-no   = 0
                            tt-eiv.blank-no  = 0
                            tt-eiv.company   = e-itemfg-vend.company
                            tt-eiv.vend-no   = e-itemfg-vend.vend-no
                            tt-eiv.vend-i-no = e-itemfg-vend.vend-item
                            tt-eiv.item-type = e-itemfg-vend.item-type
                            tt-eiv.rec_key   = e-itemfg-vend.rec_key.

                        DO inIndex = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[inIndex]  = e-itemfg-vend.run-qty[inIndex]
                                tt-eiv.run-cost[inIndex] = e-itemfg-vend.run-cost[inIndex]
                                tt-eiv.setups[inIndex]   = e-itemfg-vend.setups[inIndex]
                                tt-eiv.roll-w[inIndex]   = e-itemfg-vend.roll-w[inIndex].
                        END. /* do inIndex ... */

                        DO inIndex = 11 TO 30:
                            tt-eiv.roll-w[inIndex] = e-itemfg-vend.roll-w[inIndex].
                        END. /* do inIndex ... */
                    END. /* can-find(first tt-eiv ... */
                END. /* for each itemfg-vend */
            END. /* bf-ttJobMaterial.est-no NE "" */

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

                        DO inIndex = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[inIndex]  = e-itemfg-vend.run-qty[inIndex]
                                tt-eiv.run-cost[inIndex] = e-itemfg-vend.run-cost[inIndex]
                                tt-eiv.setups[inIndex]   = e-itemfg-vend.setups[inIndex]
                                tt-eiv.roll-w[inIndex]   = e-itemfg-vend.roll-w[inIndex].
                        END.

                        DO inIndex = 11 TO 30:
                            tt-eiv.roll-w[inIndex] = e-itemfg-vend.roll-w[inIndex].
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
      input ttJobMaterial
            loPOBest
            ipchCompany
            
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplPoBest   AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem     AS ROWID       NO-UNDO.
  
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprJobMat
        NO-ERROR.
    IF loDebug THEN             
        PUT STREAM sDebug UNFORMATTED "createTtEiv-from-item-vend " bf-ttJobMaterial.i-no  SKIP.
  
    RELEASE ITEM.

    IF bf-ttJobMaterial.prep EQ NO THEN
    DO:
        IF iplPoBest EQ NO THEN
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipchCompany
                AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
                AND index("1234BPR",item.mat-type) GT 0
                NO-ERROR.
        ELSE
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipchCompany
                AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
                AND item.mat-type EQ "B"
                NO-ERROR.
    END. /* If bf-ttJobMaterial.prep EQ NO */
    ELSE
        FIND FIRST ITEM NO-LOCK
            WHERE item.company  EQ ipchCompany
            AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
            NO-ERROR.

    IF AVAILABLE item THEN
        FOR EACH e-item NO-LOCK OF item:
            
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
            
                    DO inIndex = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[inIndex]  = e-item-vend.run-qty[inIndex]
                            tt-eiv.run-cost[inIndex] = e-item-vend.run-cost[inIndex]
                            tt-eiv.setups[inIndex]   = e-item-vend.setups[inIndex]
                            tt-eiv.roll-w[inIndex]   = e-item-vend.roll-w[inIndex].
                    END.
            
                    DO inIndex = 11 TO 30:
                        tt-eiv.roll-w[inIndex] = e-item-vend.roll-w[inIndex].
                    END.
            

            
                    IF AVAILABLE e-item-vend THEN
                    DO:

             
                        DO inIndex = 1 TO 10:
                            ASSIGN
                                tt-eiv.run-qty[inIndex + 10]  = e-item-vend.runQtyXtra[inIndex]
                                tt-eiv.run-cost[inIndex + 10] = e-item-vend.runCostXtra[inIndex]
                                tt-eiv.setups[inIndex + 10]   = e-item-vend.setupsXtra[inIndex].
                        END. /* inIndex = 1 to 10 */
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
PROCEDURE findExistingPo:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
        Inputs:
          b-orderpo
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipiPoNo     AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailPO  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd    AS ROWID NO-UNDO.


    FIND LAST po-ord NO-LOCK 
        WHERE po-ord.company   EQ ipchCompany
        AND po-ord.po-date   EQ daPoDate
        AND po-ord.due-date  EQ daDueDate
        AND po-ord.po-no     EQ ipiPoNo
        AND po-ord.vend-no   EQ chVendNo
        AND po-ord.opened    EQ YES
        AND (po-ord.type     EQ "D" OR NOT loDrop)
        NO-ERROR.
     
    IF NOT AVAILABLE po-ord THEN 
    DO:
        FIND po-ord NO-LOCK
            WHERE po-ord.company  EQ ipchCompany
            AND po-ord.due-date EQ daDueDate
            AND po-ord.vend-no  EQ chVendNo
            AND po-ord.opened   EQ YES
            AND (po-ord.type    EQ "D" OR NOT loDrop)
            NO-ERROR.

        loChoice = AMBIGUOUS po-ord.

        IF loChoice THEN
            RUN windows/l-povndt.w (ipchCompany, chVendNo, daDueDate, BUFFER po-ord).
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
        sets chOrderNo
        creates tt-fg-set
        create tt-itemfg records
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprFilId        AS RECID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeOrdl       AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprJob          AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilIdSource  AS CHARACTER   NO-UNDO.

    FIND bf-ordl WHERE RECID(bf-ordl) EQ iprFilId NO-LOCK NO-ERROR.

    /* wfk - Build tt-itemfg based on job */
    IF AVAILABLE bf-ordl THEN 
    DO:
        /* bf-itemfg holds buffer for item of bf-ordl since itemfg is used for set components below */
        ASSIGN 
            opcFilIdSource = "oe-ordl"
            chOrderNo       = bf-ordl.ord-no.

        FIND FIRST bf-ord NO-LOCK 
            WHERE bf-ord.company EQ bf-ordl.company
            AND bf-ord.ord-no  EQ bf-ordl.ord-no
            AND bf-ord.opened  EQ YES
            AND bf-ord.stat    NE "H"
            NO-ERROR.

        IF AVAILABLE bf-ord THEN 
        DO:
            roOeOrd = ROWID(bf-ord).
            
            IF TRIM(bf-ordl.job-no) NE "" THEN
                FIND FIRST job NO-LOCK 
                    WHERE job.company EQ ipchCompany
                    AND job.job-no  EQ bf-ordl.job-no
                    AND job.job-no2 EQ bf-ordl.job-no2
                    NO-ERROR.
            
            IF NOT AVAILABLE job THEN
            DO:
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ bf-ordl.company
                    AND itemfg.i-no    EQ bf-ordl.i-no
                    NO-ERROR.
                    
                IF AVAILABLE itemfg THEN 
                DO:
                    IF itemfg.isaset THEN 
                    DO:
                        RUN ipFullSet(ROWID(itemfg)).
       
                        RELEASE itemfg.
       
                        FOR EACH tt-fg-set,
                            FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ bf-ordl.company
                            AND itemfg.i-no    EQ tt-fg-set.part-no:
                                
                            IF loDebug THEN
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
              
                            IF CAN-FIND(FIRST eb NO-LOCK WHERE
                                eb.company EQ bf-ordl.company AND
                                eb.est-no EQ bf-ordl.est-no AND
                                eb.stock-no EQ tt-fg-set.part-no AND
                                eb.blank-no EQ tt-itemfg.blank-no AND
                                eb.form-no EQ tt-itemfg.form-no AND
                                eb.pur-man EQ TRUE) THEN 
                                ASSIGN tt-itemfg.pur-man = TRUE.
                        END. /* Each tt-fg-set */
                    END. /* If itemfg.isaset */       
                    ELSE 
                    DO:
                        IF loDebug THEN
                            PUT STREAM sDebug UNFORMATTED "Create tt-itemfg for FG " itemfg.i-no SKIP.

                        CREATE tt-itemfg.
                        BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg
                            ASSIGN
                            tt-itemfg.form-no  = bf-ordl.form-no
                            tt-itemfg.blank-no = bf-ordl.blank-no
                            tt-itemfg.qty      = bf-ordl.qty
                            tt-itemfg.pur-uom  = "EA"
                            tt-itemfg.row-id   = ROWID(itemfg).
                            
                        IF CAN-FIND (FIRST eb NO-LOCK WHERE
                            eb.company EQ bf-ordl.company AND
                            eb.est-no EQ bf-ordl.est-no AND
                            eb.stock-no EQ bf-ordl.i-no AND
                            eb.blank-no EQ bf-ordl.blank-no AND
                            eb.form-no EQ bf-ordl.form-no AND 
                            eb.pur-man EQ TRUE) THEN 
                            ASSIGN tt-itemfg.pur-man = TRUE.
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
                WHERE bf-ord.company EQ ipchCompany
                AND bf-ord.ord-no  EQ job-hdr.ord-no
                AND bf-ord.opened  EQ YES
                AND bf-ord.stat    NE "H"
                NO-ERROR.
            IF NOT AVAILABLE bf-ord THEN 
            DO:
                RELEASE job.
                LEAVE.
            END. /* not avail bf-ord */
    
            roOeOrd = ROWID(bf-ord).

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
            roOeOrd = ROWID(oe-ord).
  
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
    
    DEFINE INPUT  PARAMETER ip-comp      LIKE job-hdr.company.
    DEFINE INPUT  PARAMETER ip-i-no      LIKE itemfg.i-no.
    DEFINE OUTPUT PARAMETER out-actnum   LIKE po-ordl.actnum.

    DEFINE VARIABLE chCharge AS CHARACTER NO-UNDO.
     
    /* populate GL# from reftable if it exists using itemfg AH 02-23-10 */

    FIND itemfg NO-LOCK WHERE itemfg.company = ip-comp AND itemfg.i-no = ip-i-no NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        ASSIGN 
            chCharge = "".
        FIND FIRST surcharge NO-LOCK WHERE surcharge.company = ip-comp
            AND surcharge.charge <> "" NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN chCharge = surcharge.charge.
        FIND FIRST fgcat NO-LOCK WHERE fgcat.company  EQ itemfg.company
            AND fgcat.procat      EQ itemfg.procat
            NO-ERROR.

        IF AVAILABLE fgcat 
            AND fgcat.miscCharge  EQ chCharge 
            AND fgcat.brdExpAcct <> "" THEN 
            ASSIGN out-actnum = fgcat.brdExpAcct.


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
        ttJobMaterial
        assigns fil_id (global)
        ipchCompany
        assigns chOrderNo (global)
        bf-ordl
        finds po-ord
        chVendNo
        chJobNumber
        assigns reJobRecid (used in assign po-ordl values)
        assigns chOrderNo
        releases po-ord, po-ordl
        finds po-ordl
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilId    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJob      AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendNo   AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-oe-ordl       FOR oe-ordl.

    FIND bf-ttJobMaterial WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-LOCK NO-ERROR.
    FIND job WHERE ROWID(job) EQ iprJob NO-LOCK NO-ERROR.
    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.
    
    ASSIGN 
        reJobRecid = ?.

    IF AVAILABLE job THEN
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            BREAK BY job-hdr.frm      DESCENDING
            BY job-hdr.blank-no DESCENDING:
            IF (job-hdr.frm EQ bf-ttJobMaterial.frm AND
                (job-hdr.blank-no EQ bf-ttJobMaterial.blank-no OR
                bf-ttJobMaterial.blank-no EQ 0)) OR
                LAST(job-hdr.blank-no) THEN 
            DO:
          
                ASSIGN 
                    chOrderNo  = job-hdr.ord-no
                    reJobRecid = RECID(job-hdr).
                LEAVE.
            END.
        END. /* each job-hdr */

    /*once out of loop above, not pointing to correct job-hdr*/
    IF reJobRecid NE ? THEN
        FIND job-hdr NO-LOCK
            WHERE RECID(job-hdr) EQ reJobRecid
            .

    ASSIGN        
        loNewfile = YES        
        chVendNo  = "".
  
    RELEASE po-ord.
    RELEASE po-ordl.

    FIND FIRST po-ord NO-LOCK 
        WHERE po-ord.company   EQ ipchCompany
        AND po-ord.po-no     EQ INT(bf-ttJobMaterial.po-no)
        NO-ERROR.
    IF AVAILABLE po-ord THEN chVendNo = po-ord.vend-no.  

    IF AVAILABLE job THEN 
        ASSIGN
            chJobNumber = job.job-no
            chJobNumber = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', chJobNumber, job.job-no2)) .

    opcJob    = chJobNumber.
    opcVendNo = chVendNo.

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
        ipchCompany
        writes to ttJobMaterial
        updates ttJobMaterial.qty
        GLOBALS:
        outputs deJobMatQty
        outputs chJobMatQtyUOM
        outputs v-uom-comp
        outputs deQtyComp
        outputs deQtyComp1
        outputs deS-Len
        outputs deS-wid
        outputs deS-dep
        outputs debasis-w
        outputs loCanceled
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprTT-ei    AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    DEFINE VARIABLE v-uom-comp        LIKE po-ordl.pr-qty-uom NO-UNDO.

    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.

    ASSIGN
        deS-Len    = bf-ttJobMaterial.len
        deS-wid    = bf-ttJobMaterial.wid
        deS-dep    = bf-ttJobMaterial.dep
        debasis-w  = bf-ttJobMaterial.basis-w
        loCanceled = NO.

    FIND FIRST tt-ei
        WHERE tt-ei.company EQ ipchCompany
        AND tt-ei.i-no    EQ bf-ttJobMaterial.rm-i-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE tt-ei THEN 
    DO:
        oprTT-ei = ROWID(tt-ei).
        IF bf-ttJobMaterial.qty-uom EQ "BF" THEN 
        DO:
            RUN sys/ref/convquom.p(bf-ttJobMaterial.qty-uom, "EA",
                debasis-w, deS-Len, deS-wid, deS-dep,
                bf-ttJobMaterial.qty, OUTPUT deJobMatQty).

            {sys/inc/roundup.i deJobMatQty}
            chJobMatQtyUOM = "EA".
        END. /* bf-ttJobMaterial.qty-uom eq "Bf */

        ELSE
            ASSIGN
                chJobMatQtyUOM = bf-ttJobMaterial.qty-uom
                deJobMatQty = bf-ttJobMaterial.qty.

        v-uom-comp = IF bf-ttJobMaterial.this-is-a-rm THEN
            IF CAN-DO("1,2,3,4",item.mat-type) THEN "BF" ELSE "MSF"
            ELSE tt-ei.std-uom.

        IF chJobMatQtyUOM EQ v-uom-comp                 OR
            (NOT bf-ttJobMaterial.this-is-a-rm             AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, chJobMatQtyUOM) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, v-uom-comp))    THEN
            deQtyComp = deJobMatQty.
        ELSE
            RUN sys/ref/convquom.p(chJobMatQtyUOM, v-uom-comp,
                debasis-w, deS-Len, deS-wid, deS-dep,
                deJobMatQty, OUTPUT deQtyComp).

        IF bf-ttJobMaterial.job-no NE "" THEN
            RUN po/groupcst.p (bf-ttJobMaterial.job-no,
                bf-ttJobMaterial.job-no2,
                bf-ttJobMaterial.rm-i-no,
                bf-ttJobMaterial.frm,
                bf-ttJobMaterial.blank-no,
                INPUT-OUTPUT deQtyComp).

        v-uom-comp = "TON".

        IF chJobMatQtyUOM EQ v-uom-comp                 OR
            (NOT bf-ttJobMaterial.this-is-a-rm             AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, chJobMatQtyUOM) AND
            DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-ttJobMaterial.i-no, v-uom-comp))    THEN
            deQtyComp1 = deJobMatQty.
        ELSE
            RUN sys/ref/convquom.p(chJobMatQtyUOM, v-uom-comp,
                debasis-w, deS-Len, deS-wid, deS-dep,
                deJobMatQty, OUTPUT deQtyComp1).
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
          ttJobMaterial
          b-item
          inPOUom
          choeAutoPoNK1, loAutoPoSec, loNewAvail
        Outputs:
          sets fil_id
          runs rm/;itemcopy.p
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iprPoOrdl    AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprItem      AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprWJobMat   AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprOeOrdl    AS ROWID       NO-UNDO.
    DEFINE INPUT PARAMETER iprTT-ei     AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER b-item           FOR ITEM .
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-tt-ei         FOR tt-ei.

    FIND bf-po-ordl   EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl)   EQ iprPoOrdl NO-ERROR.
    FIND b-item       NO-LOCK WHERE ROWID(b-item)       EQ iprItem NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl      NO-LOCK WHERE ROWID(bf-ordl)      EQ iprOeOrdl NO-ERROR.  
    FIND bf-tt-ei     NO-LOCK WHERE ROWID(bf-tt-ei)     EQ iprTT-ei NO-ERROR.

    DEFINE VARIABLE reItem AS RECID  NO-UNDO.
    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        RUN pAddError(INPUT "Error: PO line not available in poOrdlAddVals", INPUT "1", INPUT 0, INPUT 0, INPUT 0).
        RETURN ERROR.
    END.
    IF NOT AVAILABLE b-item THEN 
    DO:
        RUN pAddError(INPUT "Error: item record not available in poOrdlAddVals", INPUT "1", INPUT 0, INPUT 0, INPUT 0).
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
                    chNewItemNo = IF LENGTH(bf-ordl.i-no) LE 10 THEN bf-ordl.i-no
                                  ELSE SUBSTRING(bf-ordl.i-no,(LENGTH(bf-ordl.i-no) - 9),10).
                       
                IF choeAutoPoNK1 EQ "AutoRM" AND loAutoPoSec AND NOT loNewAvail THEN 
                DO:         
                    reItem = RECID(b-item).
                    RUN rm/itemcopy.p.   /* not done */
                END. /* format eq "AutoRM" ... */
            END. /* avail bf-ordl ... */
        END. /* AVail bf-tt-ei */

        IF inPOUom EQ 1 AND b-item.mat-type EQ "P" THEN bf-po-ordl.cons-uom = "TON".
    END. /* bf-po-ordl.item-type = yes */

    RELEASE bf-po-ordl.
    RELEASE b-item.
    RELEASE bf-ttJobMaterial.
    RELEASE bf-ordl.
    RELEASE bf-tt-ei.

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
          ttJobMaterial
          b-item
          po-ordl (updated)
          globals: chpoCost1, loNewfile, (many in po-vendc.i )
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl       FOR po-ordl.  
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    
    DEFINE VARIABLE iCount5 AS INTEGER NO-UNDO.
     
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.

    FIND job-mat NO-LOCK WHERE RECID(job-mat) EQ bf-ttJobMaterial.w-recid NO-ERROR.

    IF AVAILABLE job-mat AND AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0 
                         AND b-item.i-code EQ "E" THEN                          
        RUN po/po-adder.p (RECID(bf-po-ordl), RECID(job-mat),ipchCompany).
    
    /* needed for po-vendc.i */
    FIND po-ordl EXCLUSIVE-LOCK WHERE ROWID(po-ordl) EQ ROWID(bf-po-ordl) .
    FIND po-ord EXCLUSIVE-LOCK WHERE ROWID(po-ord) EQ ROWID(bf-po-ord) .
    /* updates decost, v-qty deSetup, po-ordl.cost, po-ordl.cons-cost, deAdder[] */
    /* Needs po-ord, po-ordl, ipchCompany, tt-eiv, tt-ei */
    IF (chpoCost1 BEGINS "Vendor" OR po-ordl.job-no EQ "") AND po-ordl.item-type AND loNewfile THEN
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
    DEFINE INPUT  PARAMETER ipchCompany     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl       AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplFirstOfFrm   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd        AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ iprPoOrd NO-ERROR.

    IF NOT loChoice THEN 
    DO:
        /****************************************/
        /** Chose not to update the existing PO */
        /****************************************/
        loChoice = NO.
  
        IF chVendNo EQ "" THEN 
        DO:
            IF AVAILABLE tt-eiv THEN
                FIND FIRST po-ord NO-LOCK
                    WHERE po-ord.company EQ ipchCompany
                    AND po-ord.po-date   EQ daPoDate
                    AND po-ord.due-date  EQ daDueDate
                    AND po-ord.vend-no   EQ tt-eiv.vend-no
                    AND (po-ord.type     EQ "D" OR NOT loDrop)
                    NO-ERROR.
  
            ELSE 
            DO: /* not avail tt-eiv */
  
                FIND FIRST vend  WHERE vend.company EQ ipchCompany
                    AND vend.vend-no EQ chVendNo
                    NO-LOCK NO-ERROR.
                IF AVAILABLE vend THEN 
                DO:
                    chVendNo = vend.vend-no.
  
                    IF AVAILABLE bf-ordl AND iplFirstOfFrm THEN
                        FOR EACH b-oe-ordl
                            WHERE b-oe-ordl.company EQ bf-ordl.company
                            AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
                            AND b-oe-ordl.job-no  EQ bf-ordl.job-no
                            AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
                            AND b-oe-ordl.i-no    EQ bf-ttJobMaterial.fg-i-no:
                            ASSIGN
                                b-oe-ordl.po-no-po = po-ord.po-no
                                b-oe-ordl.vend-no  = chVendNo.
                        END. /* each b-oe-ordl */
                END. /* avail vend */
            END. /* else do */
  
            FIND FIRST po-ord NO-LOCK
                WHERE po-ord.company  EQ ipchCompany
                AND po-ord.po-date  EQ daPoDate
                AND po-ord.due-date EQ daDueDate
                AND po-ord.vend-no  EQ chVendNo
                AND (po-ord.type     EQ "D" OR NOT loDrop)
                NO-ERROR.
        END. /* chVendNo eq "" */
    END.  /* not loChoice (chose not to update the PO */ 

    ELSE 
    DO:
        /***********************************/
        /* Chose to update the existing PO */
        /***********************************/
        ASSIGN
            daPoDate  = po-ord.po-date
            daDueDate = po-ord.due-date.
    END. /* if loChoice */
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
    DEFINE VARIABLE lPoExists       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE roItem          AS ROWID   NO-UNDO.
    DEFINE VARIABLE roVend          AS ROWID   NO-UNDO.
    DEFINE VARIABLE loFirstJobFrm   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE loFirstOfJobFrm AS LOGICAL   NO-UNDO.
   
    outers:
    FOR EACH ttJobMaterial    
        WHERE (IF iplPromptRM THEN TRUE ELSE ttJobMaterial.this-is-a-rm EQ FALSE)
        BREAK BY ttJobMaterial.this-is-a-rm
        BY ttJobMaterial.frm
        BY ttJobMaterial.blank-no
        BY ttJobMaterial.i-no:

        ASSIGN 
            chVendNo      = ""
            roPoOrdl      = ?
            roItem        = ?
            roVend        = ?
            roPoOrd       = ?
            roTT-eiv      = ?
            roTT-ei       = ?
            roItemFG      = ?
            chFilIdSource = ? 
            roWJobMat     = ?.

        IF NOT ttJobMaterial.this-is-a-rm THEN 
        DO:
            FIND itemfg NO-LOCK WHERE itemfg.company EQ ipchCompany
                AND itemfg.i-no EQ ttJobMaterial.i-no 
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                roItemFG = ROWID(itemfg).
        END.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.
  
        IF loDebug THEN
            PUT STREAM sDebug UNFORMATTED "Process Item " + ttJobMaterial.i-no SKIP.
        loNextOuters = NO.

        EMPTY TEMP-TABLE tt-ei.
        EMPTY TEMP-TABLE tt-eiv.

        //v-vendor-chosen-report = ?.
        roWJobMat = ROWID(ttJobMaterial).

        loFirstOfJobFrm = FIRST-OF(ttJobMaterial.frm).
        loFirstJobFrm = FIRST(ttJobMaterial.frm).        

        IF ttJobMaterial.this-is-a-rm THEN 
        DO:  
            /* Create tt-ei and tt-eiv for e-itemvend of an item */
            IF lNewVendorItemCost THEN RUN RevCreateTtEivVend (INPUT ipchCompany, INPUT ROWID(ttJobMaterial), INPUT loPOBest, OUTPUT roItem).
            ELSE RUN createTtEivVend (INPUT ipchCompany, INPUT ROWID(ttJobMaterial), INPUT loPOBest, OUTPUT roItem).
        END.
        ELSE 
        DO:
            FIND itemfg NO-LOCK WHERE itemfg.company = ipchCompany
                                  AND itemfg.i-no = ttJobMaterial.rm-i-no NO-ERROR.
            IF NOT AVAIL itemfg THEN RETURN.
            /* Create tt-eiv for a ttJobMaterial and itemfg */
            IF lNewVendorItemCost THEN RUN RevCreateTtEiv (INPUT ROWID(itemfg), INPUT  ROWID(ttJobMaterial)).
            ELSE RUN createTtEivItemfg (INPUT  ipchCompany, INPUT  ROWID(ttJobMaterial)).
        END.

        /* Just a prompt to create a line */
        RUN promptCreatePoLine.        

        /* User choose not to create, so don't continue with this item */
        IF NOT loChoice THEN
            NEXT outers.

        /* Sets gvrB-orderpo, initialize global variables and create a b-orderpo */
        RUN initJobVals (INPUT ipchCompany,
            INPUT "",
            INPUT roJob,
            INPUT ROWID(ttJobMaterial),
            INPUT roOeOrdl,
            OUTPUT chJobNumber,
            OUTPUT chVendNo).

        /* Get deS-Len, deS-wid, deS-dep, deJobMatQty, deQtyComp, v-uom-comp */         
        RUN initRptRecs (INPUT ipchCompany,
            INPUT ROWID(ttJobMaterial),
            OUTPUT roTT-ei).  
  
        /* Creates a report record for each tt-eiv, sets fil_id */
   
        RUN buildRptRecs (INPUT ipchCompany, 
            INPUT loFirstJobFrm,
            INPUT ROWID(ttJobMaterial),
            INPUT roOeOrdl,
            INPUT roTT-ei,
            INPUT roItemFG,
            OUTPUT roVend).

        /* Warning message that vendor matrix does not exist */
        IF chVendNo EQ "" AND loChoice AND NOT loCanceled THEN 
            RUN cancelMessage.   
         
        IF chVendNo EQ "" OR loCanceled THEN 
        DO:
            IF loDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Skip Item for canceled or chVendNo " ttJobMaterial.i-no " chVendNo " chVendNo SKIP.
            NEXT.
        END.    
        
        /* Set po dates from oe-ord or job */
        RUN setPoDates (INPUT roVend, INPUT roOeOrd, INPUT roJob).

        /* Set GV loDrop */
        RUN promptDropShip.

        /* prompt for updating PO for given vendor and date */
        RUN promptUpdPoNum (INPUT ipchCompany, 
            INPUT ttJobMaterial.po-no,
            OUTPUT roPoOrd,
            OUTPUT roTT-eiv,
            OUTPUT loNextOuters). /* set choice */

        IF loNextOuters THEN
            NEXT outers.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.
  
        IF NOT loChoice THEN 
        DO:
            IF NOT AVAILABLE po-ord THEN 
            DO:
                /* SEts globals daDueDate and daPoDate */
                RUN calcDueDate (INPUT ROWID(ttJobMaterial)).
            END.
        END.

        /* If they chose not to update existing then return */
        IF NOT loChoice AND AVAIL(po-ord) THEN 
        DO:
            IF loDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Return since choose not to update existing " ttJobMaterial.i-no  SKIP.
            /* RETURN. WFK - taken out so that prompts for remaining RMs */
            NEXT outers.
        END.
    
        /* Check loChoice and update oe-ordl.po-no-po and vend-no */
        RUN ProcessExisting (INPUT ipchCompany,
            INPUT roOeOrdl,
            INPUT loFirstOfJobFrm,
            INPUT ROWID(ttJobMaterial),
            INPUT roPoOrd).
  
        /* Find existing PO for a due date and vendor. */
        RUN findExistingPo (INPUT ttJobMaterial.po-no, OUTPUT lPoExists, OUTPUT roPoOrd).

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ roOeOrd NO-ERROR.
          
        IF NOT lPoExists THEN 
        DO:
            IF AVAILABLE po-ord THEN 
            DO:
                /* Po exists for given vendor and date, locks record if say yes */
                RUN PromptExistingPo. /* release current po-ord buffer if they say no */      
            END.
            IF NOT AVAILABLE po-ord THEN 
            DO:                
                RUN createPoOrd (INPUT roOeOrd, OUTPUT roPoOrd, OUTPUT loNextOuters).
                IF loNextOuters THEN 
                DO:                   
                    IF loDebug THEN             
                        PUT STREAM sDebug UNFORMATTED "Skip do to createPoOrd " ttJobMaterial.i-no  SKIP.
                    NEXT outers.
                END.
                FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
   
            END. /* Not avail po-ord then add it */
        END.
        ELSE
            loChoice = YES.

        /* Assign po values to oe-ordl and assign vend values to po-ord */
        RUN setPoValues (INPUT loFirstOfJobFrm, 
            INPUT roPoOrd, 
            INPUT roOeOrdl,
            INPUT ROWID(ttJobMaterial)).
                  
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.

        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after assign po values)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            NEXT.
        END.

        FIND b-item NO-LOCK WHERE ROWID(b-item) EQ roItem NO-ERROR.
        FIND vend NO-LOCK WHERE ROWID(vend) EQ roVend NO-ERROR.

        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create PO Line " ttJobMaterial.i-no  SKIP.
        FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ roOeOrdl NO-ERROR.

        /* creates po-ordl based on loChoice */
        RUN createPoOrdl (INPUT roPoOrd,
            INPUT roOeOrdl,
            INPUT ROWID(ttJobMaterial),
            INPUT roItem,
            OUTPUT roItemFG).

        FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ roItemFG NO-ERROR.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after create po-ordl)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ roPoOrdl NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT "PO Line not available, skipping it", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT outers.
        END.

        /* set values from item and ttJobMaterial */
        IF po-ordl.item-type THEN
            RUN setPoOrdRm (INPUT roPoOrd,
                INPUT roPoOrdl,
                INPUT roItem,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roJob).
        ELSE
            /* Set values from itemfg */
            RUN setPoOrdlFg (INPUT roPoOrd,
                INPUT roPoOrdl,
                INPUT roItemFG,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roJob).         


        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Error: No Po found (after assign po-ordl)", INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ roPoOrdl NO-ERROR.

        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT 'Error: No Po Line available (main block)', INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            
            NEXT.
        END.


        IF po-ordl.item-type THEN 
        DO:
  
            IF NOT AVAILABLE tt-ei THEN
                FIND FIRST tt-ei
                    WHERE tt-ei.company EQ ipchCompany
                    AND tt-ei.i-no    EQ ttJobMaterial.rm-i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE tt-ei THEN
                roTT-ei = ROWID(tt-ei).
            ELSE 
                roTT-ei = ?.

            /* get chNewItemNo, v-new-len, v-new-wid, set po-ordl.cons-uom and pr-uom */
            RUN poOrdlAddVals(INPUT roPoOrdl,
                INPUT roItem,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roTT-ei).
        END. /* run poOrdlAddVals */ 
        
        /* Get len, wid, depth from item. Set po-ordl.cust-no */
        RUN calcLenWid (INPUT roPoOrd,
            INPUT roPoOrdl,
            INPUT roItem).

        /* get po-ordl.ord-qty from job-hdr, ttJobMaterial or oe-ordl */
        IF po-ordl.item-type THEN 
            RUN calcEstValues (INPUT roPoOrdl,
                INPUT roJob,
                INPUT ROWID(ttJobMaterial),
                INPUT roOeOrdl,
                INPUT roItem).
        /* Set po-ordl.s-num and b-num. UOM Conversion on po-ordl.ord-qty */
        RUN calcOrdQty    (INPUT roPoOrdl, INPUT ROWID(ttJobMaterial)).

        /* get po-ordl.cost from ttJobMaterial or deItemCost */
        RUN calcCostSetup (INPUT roTT-ei, INPUT ROWID(ttJobMaterial),
            INPUT roPoOrdl, INPUT chVendNo,
            INPUT chVendItem).

        /*  Calculate v-tot-msf. Set po-ordl.s-len and s-wid */
        RUN calcMSF       (INPUT roPoOrdl).

        /* runs po/po-adder.p and po/po-vendc.i */
        RUN processAdders (INPUT ROWID(ttJobMaterial), INPUT roPoOrdl, INPUT roPoOrd).

        /* UOM conversion for po-ordl.cost */
        RUN calcCost      (INPUT roTT-ei, INPUT roPoOrdl).

        RELEASE b-item.
  
        /* Warns user of zero length or width in Job */
        RUN zeroLenWarning (INPUT roPoOrdl, OUTPUT roItem).

        FIND b-item WHERE ROWID(b-item) EQ roItem NO-LOCK NO-ERROR.
        FIND po-ordl WHERE ROWID(po-ordl) EQ roPoOrdl NO-LOCK NO-ERROR.

        IF (AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0) OR
            NOT po-ordl.item-type                                   THEN 
        DO:    
            /* Validate than Length was entered */
            RUN brdLenCheck (INPUT roPoOrdl).

            /* UOM conversions to handle zere line qty or cost. Calculate oe-ordl.cost  */
            RUN checkZeroQty (INPUT roPoOrdl).

            /* Calculate po-ordl.t-cost and cons-cost */
            RUN calcExtCost (INPUT roPoOrdl).

            /* Find po-ordl and set v-old-i-no, v-tot-ord and po-ordl.dscr */
            RUN addHeaderTot (INPUT roPoOrdl).

            /* If chNewItemNo is a valid item, set ttJobMaterial.rm-i-no to it */
            RUN autoRm (INPUT roPoOrdl,
                INPUT ROWID(ttJobMaterial),
                OUTPUT roItem) /* needs additional buffers */.

        END. /* if avail b-item ... */
                
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ roPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            RUN pAddError(INPUT "Internal Error - PO not found.", INPUT "3", INPUT 0, INPUT 0, INPUT 0).            
            NEXT.
        END.
        
        /* Check user po limit */
        RUN pCheckUserLimit(BUFFER po-ord).
        
        /* Calculate PO Header Totals */
        RUN po/po-total.p (RECID(po-ord)).

        /* CHECK for exceeding vendor's max PO Cost and worn for it (set lohold) */
        RUN validMaxCost (roPoOrd).

        FIND po-ordl WHERE ROWID(po-ordl) EQ roPoOrdl NO-LOCK NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            RUN pAddError(INPUT "Error - Po Line not found (22)", INPUT "3", INPUT 0, INPUT 0, INPUT 0).            
            NEXT.
        END.

        /* Update item inventory totals */
        RUN ipPoordlUp (RECID(po-ordl), 1, loCountUpdate).

    END. /* each ttJobMaterial */
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
  DEFINE VARIABLE lCheckFgItemPoStatus AS LOGICAL NO-UNDO .  
  
    IF loDebug THEN             
        PUT STREAM sDebug UNFORMATTED "Prompt create PO line " ttJobMaterial.i-no  SKIP.

    IF NOT ttJobMaterial.this-is-a-rm THEN
        RUN fg/GetItemfgPoStatus.p (INPUT ipchCompany,
                                    INPUT ttJobMaterial.rm-i-no,"",NO,
                                    OUTPUT lCheckFgItemPoStatus).
                                    
    IF NOT ttJobMaterial.this-is-a-rm THEN
        RUN pCheckFGItemCustHold(ipchCompany,ttJobMaterial.rm-i-no,INPUT-OUTPUT lCheckFgItemPoStatus) .

    loChoice = NO.
    IF chVendNo EQ "" 
        AND ((loAutoPoSec AND ttJobMaterial.this-is-a-rm) OR (loAutoFgSec AND NOT ttJobMaterial.this-is-a-rm AND lCheckFgItemPoStatus )) 
        AND NOT ttJobMaterial.isaset THEN 
    DO ON ENDKEY UNDO, LEAVE:
        IF deOeAutoFg EQ 1 THEN
            loChoice = TRUE.
        ELSE
            MESSAGE "Do you wish to create a PO line for " +
                (IF ttJobMaterial.this-is-a-rm
                THEN ("Job/Form/RM#: " + TRIM(chJobNumber) + "/" +
                TRIM(STRING(ttJobMaterial.frm,"99")))
                ELSE ("Order/FG#: " +
                TRIM(STRING(chOrderNo,">>>>>>>>>>")))) +
                "/" + TRIM(ttJobMaterial.rm-i-no) + "?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE loChoice.
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
        SEt GV loDrop
    ------------------------------------------------------------------------------*/

    loDrop = NO.
    IF inOeAutoPONK1 EQ 1 THEN 
    DO:
        IF deOeAutoFg EQ 1 THEN
            loDrop = TRUE.
        ELSE
            MESSAGE "Is this a Drop Shipment?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE loDrop.
    END.
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
        requires GV loChoice and 
        sets loChoice
        May release po-ord
    ------------------------------------------------------------------------------*/
      
    IF NOT loChoice AND looeAutopoNK1 THEN DO:
        IF deOeAutoFg EQ 1 THEN
            loChoice = TRUE.
        ELSE    
            MESSAGE "PO exists for given Vendor and Date." SKIP
                "Do you want to update existing PO? " 
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE loChoice.
    END.
    
    IF looeAutopoNK1 = NO THEN
        loChoice = NO.

    IF loChoice THEN FIND CURRENT po-ord NO-ERROR.
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
          ipchCompany
          chVendNo
          loDrop
          Sets loChoice
          finds tt-eiv
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprPoOrd        AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprTT-eiv       AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oploNextOuters  AS LOGICAL     NO-UNDO.

    DEFINE VARIABLE fil_id AS RECID NO-UNDO.
    
    loChoice = NO.
    oploNextOuters = FALSE.
    
    FIND LAST po-ord NO-LOCK
        WHERE po-ord.company EQ ipchCompany
        AND po-ord.po-no     EQ ipiPoNo
        AND po-ord.vend-no   EQ chVendNo
        AND (po-ord.type     EQ "D" OR NOT loDrop)
        NO-ERROR.

    IF AVAILABLE po-ord AND NOT po-ord.opened THEN 
    DO: 
        oploNextOuters = TRUE.
    END. /* If PO was found but was not opened */
    ELSE 
    DO:  
        IF AVAILABLE po-ord AND po-ord.opened AND loAutoPoSec THEN 
        DO:
            MESSAGE "Do you wish to update PO #" +
                TRIM(STRING(po-ord.po-no,">>>>>>>>")) + " for " +
                (IF AVAILABLE job THEN ("Job/Form/RM#: " + TRIM(chJobNumber) + "/" +
                TRIM(STRING(ttJobMaterial.frm,"99")))
                ELSE ("Order/FG#: " +
                TRIM(STRING(chOrderNo,">>>>>>>>>>")))) +
                "/" + TRIM(ttJobMaterial.rm-i-no) + "?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE loChoice.

            FIND FIRST tt-eiv WHERE tt-eiv.vend-no EQ po-ord.vend-no NO-ERROR.
        END.

        ELSE 
            FIND FIRST tt-eiv WHERE tt-eiv.rec-id EQ fil_id NO-LOCK NO-ERROR.

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

&IF DEFINED(EXCLUDE-RevCreateTtEiv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RevCreateTtEiv Procedure
PROCEDURE RevCreateTtEiv:
    /*------------------------------------------------------------------------------
     Purpose: Revisized CreateTtEiv procedure to use vendItemCost table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprItemfg  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat
        NO-ERROR.
    FIND itemfg WHERE ROWID(itemfg) EQ iprItemfg NO-LOCK NO-ERROR.
    IF NOT AVAILABLE itemfg THEN
        RETURN ERROR.

    FIND FIRST vendItemCost NO-LOCK    
        WHERE vendItemCost.company EQ itemfg.company
        AND vendItemCost.ItemID    EQ itemfg.i-no
        AND vendItemCost.ItemType  EQ "FG"
        AND vendItemCost.effectiveDate LE TODAY
        AND (venditemcost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?)
        NO-ERROR.        
    IF AVAIL vendItemCost THEN DO:    
       CREATE tt-ei.
       ASSIGN tt-ei.company = itemfg.company
              tt-ei.i-no    = itemfg.i-no
              tt-ei.std-uom = vendItemCost.VendorUOM
              .        
    END.
    IF bf-ttJobMaterial.est-no NE "" THEN 
    DO:
      inIndex = 0.      
      FOR EACH vendItemCost NO-LOCK  WHERE vendItemCost.company EQ itemfg.company
                                     AND vendItemCost.estimateNo EQ bf-ttJobMaterial.est-no
                                     AND vendItemCost.formNo EQ bf-ttJobMaterial.frm
                                     AND vendItemCost.blankNo EQ bf-ttJobMaterial.blank-no
                                     AND vendItemCost.ItemID    EQ itemfg.i-no
                                     AND vendItemCost.ItemType EQ "FG" 
                                     AND vendItemCost.effectiveDate LE TODAY
                                     AND (venditemcost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?),
          
          EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
            /* AND venditemcostlevel.quantityfrom <= fGetVendCostQty(bf-ttJobMaterial.qty, bf-ttJobMaterial.qty-uom, venditemcost.vendorUom)
             AND venditemcostlevel.quantityto >= fGetVendCostQty(bf-ttJobMaterial.qty, bf-ttJobMaterial.qty-uom, venditemcost.vendorUom)             
             */
             BY vendItemCostLevel.vendItemCostLevelID:
         
          /*IF NOT CAN-FIND(FIRST tt-eiv
              WHERE tt-eiv.company   EQ e-itemfg-vend.company
              AND tt-eiv.i-no      EQ bf-ttJobMaterial.i-no
              AND tt-eiv.vend-no   EQ e-itemfg-vend.vend-no) THEN 
          */                   
               
            IF  venditemcostlevel.quantityfrom <= fGetVendCostQty(bf-ttJobMaterial.qty, bf-ttJobMaterial.qty-uom, venditemcost.vendorUom)
                AND venditemcostlevel.quantityto >= fGetVendCostQty(bf-ttJobMaterial.qty, bf-ttJobMaterial.qty-uom, venditemcost.vendorUom)
            THEN .
            ELSE NEXT.                          
             
            inIndex = inIndex + 1.    
            FIND FIRST tt-eiv WHERE tt-eiv.rec_key = vendItemCostLevel.rec_key NO-ERROR.
            IF NOT AVAIL tt-eiv THEN 
            DO:       
                CREATE tt-eiv.
                ASSIGN tt-eiv.rec_key   = vendItemCostLevel.rec_key.
                       tt-eiv.rec-id    = RECID(vendItemCostLevel).
                       tt-eiv.est-no    = "".
                       tt-eiv.i-no      = vendItemCost.itemID.
                       tt-eiv.form-no   = 0.
                       tt-eiv.blank-no  = 0.
                       tt-eiv.company   = vendItemCost.company.
                       tt-eiv.vend-no   = vendItemCost.vendorID.
                       tt-eiv.vend-i-no = vendItemCost.vendorItemID.
                       tt-eiv.item-type = IF vendItemCost.itemType = "RM" THEN YES ELSE NO.
            END.                                
            ASSIGN tt-eiv.roll-w[27] = venditemCost.dimWidthMinimum
                   tt-eiv.roll-w[28] = venditemCost.dimWidthMaximum
                   tt-eiv.roll-w[29] = venditemCost.dimlengthMinimum
                   tt-eiv.roll-w[30] = venditemCost.dimlengthMaximum.
                   
            IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 20 */
               inIndex GT 0 AND inIndex LE 20 THEN 
                ASSIGN /*inIndex                  = vendItemCostLevel.vendItemCostLevelID*/
                       tt-eiv.run-qty[inIndex]  = vendItemCostLevel.quantityBase  /* e-item-vend.run-qty[inIndex]*/
                       tt-eiv.run-cost[inIndex] = vendItemCostLevel.costPerUOM  /* e-item-vend.run-cost[inIndex] */
                       tt-eiv.setups[inIndex]   = vendItemCostLevel.costSetup   /* e-itemfg-vend.setups[inIndex] */
                       .
            IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 30*/
                inIndex GT 0 AND inIndex LE 26 THEN           
                ASSIGN tt-eiv.roll-w[inIndex]   = vendItemCost.validWidth[inIndex] /* e-itemfg-vend.roll-w[inIndex] */   
                       .
                            
      END. /* each vendcostitem */
    END. /* if est-no <> "" */ 
    inIndex = 0.
    IF NOT CAN-FIND(FIRST tt-eiv) THEN
    FOR EACH vendItemCost NO-LOCK  WHERE vendItemCost.company EQ itemfg.company
        AND vendItemCost.estimateNo EQ ""
/*        AND vendItemCost.formNo EQ bf-ttJobMaterial.frm       */
/*        AND vendItemCost.blankNo EQ bf-ttJobMaterial.blank-no */
          AND vendItemCost.ItemID    EQ itemfg.i-no
          AND vendItemCost.ItemType EQ "FG"
          AND vendItemCost.effectiveDate LE TODAY
          AND (venditemcost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?),
                                                     
        EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
        BY vendItemCostLevel.vendItemCostLevelID:    
    
        inIndex = inIndex + 1.
            IF NOT CAN-FIND(FIRST tt-eiv
                WHERE tt-eiv.company   EQ vendItemCost.company
                AND tt-eiv.i-no      EQ vendItemCost.ItemId
                AND tt-eiv.vend-no   EQ vendItemCost.vendorID) THEN 
            DO:
                CREATE tt-eiv.
                ASSIGN tt-eiv.rec_key = vendItemCostLevel.rec_key.
                       tt-eiv.rec-id    = RECID(vendItemCostLevel).
                       tt-eiv.est-no    = "".
                       tt-eiv.i-no      = vendItemCost.itemID.
                       tt-eiv.form-no   = 0.
                       tt-eiv.blank-no  = 0.
                       tt-eiv.company   = vendItemCost.company.
                       tt-eiv.vend-no   = vendItemCost.vendorID.
                       tt-eiv.vend-i-no = vendItemCost.vendorItemID.
                       tt-eiv.item-type = IF vendItemCost.itemType = "RM" THEN YES ELSE NO.
                                       
                roTT-eiv = ROWID(tt-eiv).
             END.
             ASSIGN tt-eiv.roll-w[27] = venditemCost.dimWidthMinimum
                    tt-eiv.roll-w[28] = venditemCost.dimWidthMaximum
                    tt-eiv.roll-w[29] = venditemCost.dimlengthMinimum
                    tt-eiv.roll-w[30] = venditemCost.dimlengthMaximum                                
                    .   
             IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 20*/
                  inIndex GT 0 AND inIndex LE 20 THEN 
                    ASSIGN /*inIndex                  = vendItemCostLevel.vendItemCostLevelID*/
                        tt-eiv.run-qty[inIndex]  = vendItemCostLevel.quantityTo  /* e-item-vend.run-qty[inIndex]*/
                        tt-eiv.run-cost[inIndex] = vendItemCostLevel.costPerUOM  /* e-item-vend.run-cost[inIndex] */
                        tt-eiv.setups[inIndex]   = vendItemCostLevel.costSetup   /* e-itemfg-vend.setups[inIndex] */
                        .
                IF /*vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 30 */
                   inIndex GT 0 AND inIndex LE 26 THEN           
                    ASSIGN tt-eiv.roll-w[inIndex] = vendItemCost.validWidth[inIndex] /* e-itemfg-vend.roll-w[inIndex] */   
                        .                        
                            
      END.
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-RevCreateTtEivVend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RevCreateTtEivVend Procedure
PROCEDURE RevCreateTtEivVend:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompany AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprJobMat   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iplPoBest   AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem     AS ROWID       NO-UNDO.
 
    DEFINE VARIABLE dQtyInVendorUOM AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprJobMat
        NO-ERROR.
    IF loDebug THEN             
        PUT STREAM sDebug UNFORMATTED "createTtEiv-from-item-vend " bf-ttJobMaterial.i-no  SKIP.
  
    RELEASE ITEM.

    IF bf-ttJobMaterial.prep EQ NO THEN
    DO:
        IF iplPoBest EQ NO THEN
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipchCompany
                AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
                AND index("1234BPR",item.mat-type) GT 0
                NO-ERROR.
        ELSE
            FIND FIRST ITEM NO-LOCK
                WHERE item.company  EQ ipchCompany
                AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
                AND item.mat-type EQ "B"
                NO-ERROR.
    END. /* If bf-ttJobMaterial.prep EQ NO */
    ELSE
        FIND FIRST ITEM NO-LOCK
            WHERE item.company  EQ ipchCompany
            AND item.i-no     EQ bf-ttJobMaterial.rm-i-no
            NO-ERROR.

    IF NOT AVAILABLE item THEN RETURN.
    
    /*===    
    cScope = "Effective and Not Expired" /* DYNAMIC-FUNCTION("GetValidScopes")*/ .
    lIncludeBlankVendor = NO.
    
    RUN BuildVendItemCosts(ipchCompany, ITEM.i-no, "RM", cScope, lIncludeBlankVendor,
        10000, "EA", 
        deS-Len, deS-wid, 0, "IN",
        item.basis-w, "LBS/MSF", 
        OUTPUT TABLE ttVendItemCost,
        OUTPUT lError, OUTPUT cMessage).
        
    FOR EACH ttVendItemCost NO-LOCK    BY ttVendItemCost.costTotal: 
        DISPLAY ttVendItemCost.quantityTargetInVendorUOM ttVendItemCost.isValid ttVendItemCost.vendorID ttVendItemCost.costTotal ttVendItemCost.costPerVendorUOM ttVendItemCost.vendorUOM.                
    END.    
    
   ===*/
    
    FIND FIRST vendItemCost NO-LOCK    
        WHERE vendItemCost.company EQ item.company
        AND vendItemCost.ItemID    EQ item.i-no
        AND vendItemCost.ItemType EQ "RM"
        AND vendItemCost.effectiveDate LE TODAY
        AND (venditemcost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?)
        NO-ERROR.
    IF AVAIL vendItemCost THEN 
    DO:    
        CREATE tt-ei.
        ASSIGN 
            tt-ei.company = item.company
            tt-ei.i-no    = item.i-no
            tt-ei.std-uom = vendItemCost.VendorUOM
            .        
    END.
    inIndex = 0. 
 
    FOR EACH vendItemCost NO-LOCK  WHERE vendItemCost.company EQ ITEM.company
                    AND vendItemCost.ItemID    EQ item.i-no
                    AND vendItemCost.ItemType EQ "RM" 
                    AND bf-ttJobMaterial.wid GE venditemCost.dimWidthMinimum AND (bf-ttJobMaterial.wid LE venditemCost.dimWidthMaximum OR venditemCost.dimWidthMaximum EQ 0)
                    AND bf-ttJobMaterial.len GE venditemCost.dimlengthMinimum AND (bf-ttJobMaterial.len LE venditemCost.dimlengthMaximum OR venditemCost.dimlengthMaximum EQ 0)
                    AND vendItemCost.effectiveDate LE TODAY
                    AND (venditemcost.expirationDate GE TODAY OR vendItemCost.expirationDate = ?),
                                                     
        EACH vendItemCostLevel NO-LOCK WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostId
        BY vendItemCostLevel.vendItemCostLevelID:  
        dQtyInVendorUOM = fGetVendCostQty(bf-ttJobMaterial.qty, bf-ttJobMaterial.qty-uom, venditemcost.vendorUom).
        IF  dQtyInVendorUOM LT venditemcostlevel.quantityfrom
            OR dQtyInVendorUOM GT venditemcostlevel.quantityto 
            THEN NEXT.  
                        
        inIndex = inIndex + 1.         
        FIND FIRST tt-eiv WHERE tt-eiv.rec_key = vendItemCostLevel.rec_key NO-ERROR.
        IF NOT AVAIL tt-eiv THEN 
        DO:                   
            CREATE tt-eiv.
            ASSIGN 
                tt-eiv.rec_key = vendItemCostLevel.rec_key.
                       tt-eiv.rec-id    = RECID(vendItemCostLevel).
                       tt-eiv.est-no    = vendItemCost.estimateNo .
                       tt-eiv.i-no      = vendItemCost.itemID.
                       tt-eiv.form-no   = vendItemCost.formNo.
                       tt-eiv.blank-no  = vendItemCost.blankNo.
                       tt-eiv.company   = vendItemCost.company.
                       tt-eiv.vend-no   = vendItemCost.vendorID.
                       tt-eiv.vend-i-no = vendItemCost.vendorItemID.
                       tt-eiv.item-type = IF vendItemCost.itemType = "RM" THEN YES ELSE NO
                       .
            ASSIGN     tt-eiv.roll-w[27] = venditemCost.dimWidthMinimum
                       tt-eiv.roll-w[28] = venditemCost.dimWidthMaximum
                       tt-eiv.roll-w[29] = venditemCost.dimlengthMinimum
                       tt-eiv.roll-w[30] = venditemCost.dimlengthMaximum
                       .        
        END.  
        IF /* vendItemCostLevel.vendItemCostLevelID GT 0 AND vendItemCostLevel.vendItemCostLevelID LE 20 */
               inIndex GT 0 AND inIndex LE 20 THEN 
                ASSIGN /*inIndex                  = (vendItemCostLevel.vendItemCostLevelID*/
                       tt-eiv.run-qty[inIndex]  = vendItemCostLevel.quantityTo  /* e-item-vend.run-qty[inIndex]*/
                       tt-eiv.run-cost[inIndex] = vendItemCostLevel.costPerUOM  /* e-item-vend.run-cost[inIndex] */
                       tt-eiv.setups[inIndex]   = vendItemCostLevel.costSetup   /* e-itemfg-vend.setups[inIndex] */
                       .
        IF inIndex GT 0 AND inIndex LE 26 THEN           
                ASSIGN tt-eiv.roll-w[inIndex]   = vendItemCost.validWidth[inIndex] /* e-itemfg-vend.roll-w[inIndex] */   
                       .
    END.
    
    oprItem = ROWID(ITEM).

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
          daPoDate (global)
          daDueDate (global)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprVend  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrd AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob   AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-job FOR job.

    FIND vend   NO-LOCK WHERE ROWID(vend)   EQ iprVend NO-ERROR.
    FIND bf-ord NO-LOCK WHERE ROWID(bf-ord) EQ iprOeOrd NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.

    /* vend.disc-days eq lead time */
    IF AVAILABLE vend THEN daPoDate = TODAY.
    ELSE daPoDate = IF AVAILABLE bf-ord THEN bf-ord.ord-date ELSE bf-job.start-date.

    IF AVAILABLE vend THEN daDueDate = TODAY + vend.disc-days.
    ELSE daDueDate = IF AVAILABLE bf-ord THEN bf-ord.ord-date ELSE bf-job.start-date.

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
          ttJobMaterial
          po-ordl (updates)
        Global: 
          lv-recid
        Outputs: 
           makes tt-ei available
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItemFg   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-itemfg        FOR itemfg.
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job           FOR job.

    FIND bf-po-ord   NO-LOCK WHERE ROWID(bf-po-ord)  EQ iprPoOrd NO-ERROR.
    FIND bf-itemfg  NO-LOCK WHERE ROWID(bf-itemfg)  EQ iprItemfg NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob  NO-ERROR.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    
    IF NOT AVAILABLE bf-po-ordl THEN 
    DO:
        RUN pAddError(INPUT "Error: Po Line not available (setPoOrdlFg)", INPUT "1", INPUT 0, INPUT 0, INPUT 0).        
        RETURN ERROR.
    END.

    IF AVAILABLE bf-itemfg THEN 
    DO:
        FIND FIRST tt-ei NO-LOCK 
            WHERE tt-ei.company EQ ipchCompany
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
            bf-po-ordl.pr-qty-uom = IF chPOUom EQ "Purchase" THEN bf-itemfg.pur-uom
                                                       ELSE bf-itemfg.prod-uom
            bf-po-ordl.cons-uom   = bf-itemfg.prod-uom
            bf-po-ordl.pr-uom     = bf-itemfg.pur-uom
            bf-po-ordl.cons-cost  = bf-itemfg.last-cost
            chPartDescr1          = bf-ordl.part-dscr1
            chPartDescr2          = bf-ordl.part-dscr2
            bf-po-ordl.ord-qty    = bf-ttJobMaterial.qty.
  
        IF AVAILABLE xeb THEN
            ASSIGN
                bf-po-ordl.s-num = xeb.form-no
                bf-po-ordl.b-num = xeb.blank-no.
            
        IF NOT AVAILABLE tt-ei THEN
            FIND FIRST tt-ei
                WHERE tt-ei.company EQ ipchCompany
                AND tt-ei.i-no    EQ bf-ttJobMaterial.rm-i-no
                NO-LOCK NO-ERROR.
  
        IF AVAILABLE tt-ei THEN
            bf-po-ordl.pr-uom = tt-ei.std-uom.
           
        bf-po-ordl.ord-no = IF AVAILABLE bf-ordl THEN bf-ordl.ord-no ELSE 0.
        FIND oe-ordl EXCLUSIVE-LOCK WHERE RECID(oe-ordl) EQ reORDRecId NO-ERROR.
  
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
                chPartDescr1      = bf-itemfg.part-dscr1
                chPartDescr2      = bf-itemfg.part-dscr2.
  
        IF bf-po-ordl.pr-qty-uom NE "EA"                    AND
            (bf-po-ordl.item-type OR
            NOT DYNAMIC-FUNCTION("Conv_IsEAUOM", ipchCompany, bf-po-ordl.i-no, bf-po-ordl.pr-qty-uom)) THEN
            RUN sys/ref/convquom.p("EA", bf-po-ordl.pr-qty-uom,
                bf-ttJobMaterial.basis-w, bf-ttJobMaterial.len, bf-ttJobMaterial.wid, bf-ttJobMaterial.dep,
                bf-po-ordl.ord-qty, OUTPUT bf-po-ordl.ord-qty).
  
        FOR EACH prodl FIELDS(prolin)
            WHERE prodl.company EQ ipchCompany
            AND prodl.procat  EQ bf-itemfg.procat
            NO-LOCK,
            FIRST prod FIELDS(fg-mat)
            WHERE prod.company EQ ipchCompany
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
            chCharge = "".
        FIND FIRST surcharge WHERE surcharge.company = ipchCompany
            AND surcharge.charge <> "" NO-LOCK NO-ERROR.
        IF AVAILABLE surcharge THEN
            ASSIGN chCharge = surcharge.charge.
        FIND FIRST fgcat NO-LOCK WHERE fgcat.company  EQ bf-itemfg.company
            AND fgcat.procat      EQ bf-itemfg.procat
            NO-ERROR.

        IF AVAILABLE fgcat 
        AND fgcat.miscCharge  EQ chCharge 
        AND fgcat.brdExpAcct <> "" THEN 
            ASSIGN bf-po-ordl.actnum = fgcat.brdExpAcct.


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
          chActnum
          ipchCompany
          reJobRecid
          chActnum
          
        Outputs:
          make tt-ei available
          
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprPoOrd    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprItem     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat  AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl   AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob      AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.
    DEFINE BUFFER bf-po-ordl       FOR po-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER b-item           FOR ITEM .
    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-job           FOR job.
    
    DEFINE VARIABLE chActnum          AS CHARACTER NO-UNDO.

    FIND bf-po-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.
    FIND bf-po-ord NO-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND b-item NO-LOCK WHERE ROWID(b-item) EQ iprItem NO-ERROR.
    FIND bf-ttJobMaterial NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.

    ASSIGN 
        chActnum = "".
    FIND FIRST costtype NO-LOCK
        WHERE costtype.company   EQ bf-po-ordl.company
        AND costtype.loc       EQ bf-po-ord.loc
        AND costtype.cost-type EQ b-item.cost-type
        NO-ERROR.
    IF AVAILABLE costtype AND loAPGL THEN
        bf-po-ordl.actnum = IF chAPGL EQ "Asset"   THEN costtype.inv-asset
        ELSE
            IF chAPGL BEGINS "Exp"  AND
            (chAPGL EQ "Expense" OR costtype.cons-exp NE "")
            THEN costtype.cons-exp
            ELSE                            bf-po-ordl.actnum.


    /* populate GL# from job-hdr.i-no + itemfg tables, then reftable AH 02-24-10*/
    IF reJobRecid <> ? THEN 
    DO:
        FIND b-job-hdr NO-LOCK WHERE RECID(b-job-hdr) = reJobRecid NO-ERROR.

        /* Get gl actnum for a itemfg */
        IF AVAILABLE b-job-hdr THEN 
            RUN getItemfgGL (INPUT b-job-hdr.company, b-job-hdr.i-no, OUTPUT chActnum).

        IF chActnum <> "" THEN 
            ASSIGN bf-po-ordl.actnum = chActnum.
    END.
    RELEASE b-job-hdr.

    FIND FIRST tt-ei
        WHERE tt-ei.company EQ ipchCompany
        AND tt-ei.i-no    EQ b-item.i-no
        NO-LOCK NO-ERROR.

    ASSIGN
        bf-po-ordl.s-num      = bf-ttJobMaterial.frm
        bf-po-ordl.b-num      = bf-ttJobMaterial.blank-no
        bf-po-ordl.job-no     = bf-job.job-no
        bf-po-ordl.job-no2    = bf-job.job-no2
        bf-po-ordl.i-no       = b-item.i-no
        bf-po-ordl.i-name     = b-item.i-name
        bf-po-ordl.pr-qty-uom = bf-ttJobMaterial.qty-uom
        bf-po-ordl.cons-uom   = b-item.cons-uom
        bf-po-ordl.pr-uom     = b-item.pur-uom
        bf-po-ordl.cons-cost  = b-item.last-cost
        chPartDescr1          = b-item.i-dscr
        chPartDescr2          = b-item.est-dscr.
   
        ASSIGN bf-po-ordl.pr-qty-uom = IF chPOUom EQ "Purchase" THEN b-item.pur-uom
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
      ttJobMaterial   must be available
      
      
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplFirstOfFrm AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprPoOrd      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprWJobMat    AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-ordl          FOR oe-ordl.
    DEFINE BUFFER bf-po-ord        FOR po-ord.
    DEFINE BUFFER bf-ttJobMaterial FOR ttJobMaterial.

    FIND bf-po-ord EXCLUSIVE-LOCK WHERE ROWID(bf-po-ord) EQ iprPoOrd NO-ERROR.
    FIND bf-ordl EXCLUSIVE-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.
    FIND bf-ttJobMaterial  NO-LOCK WHERE ROWID(bf-ttJobMaterial) EQ iprWJobMat NO-ERROR.

    IF AVAILABLE bf-ordl AND iplFirstOfFrm THEN
        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company EQ bf-ordl.company
            AND b-oe-ordl.ord-no  EQ bf-ordl.ord-no
            AND b-oe-ordl.job-no  EQ bf-ordl.job-no
            AND b-oe-ordl.job-no2 EQ bf-ordl.job-no2
            AND b-oe-ordl.i-no    EQ bf-ttJobMaterial.fg-i-no
            EXCLUSIVE-LOCK:
            /* for testing put this back!!! */

            ASSIGN
                b-oe-ordl.po-no-po = bf-po-ord.po-no
                b-oe-ordl.vend-no  = chVendNo.
        END.

    FIND CURRENT bf-po-ord EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST vend
        WHERE vend.company EQ ipchCompany 
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
    RELEASE bf-ttJobMaterial.
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
    DEFINE INPUT  PARAMETER iprJob         AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFilIdSource AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER bf-ordl FOR oe-ordl.
    /* Check that cFilIDSource is JOB since job record may be available here    */
    /* even though the fil_id is related to oe-ordl and could cause a duplicate */
    /* tt-itemfg                                                                */
    
    DEFINE BUFFER bf-itemfg FOR Itemfg.
    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR. 
    
    IF loDebug THEN
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
            IF loDebug THEN
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
                    IF loDebug THEN
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
                        RUN ipFullSet (ROWID(bf-itemfg)).
          
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
    
    DEFINE VARIABLE deTotalCost  AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE lohold       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-po-ord FOR po-ord.
    DEFINE BUFFER b-po-ordl FOR po-ordl.

    FIND bf-po-ord WHERE ROWID(bf-po-ord) EQ iprPoOrd EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-po-ord THEN 
    DO:
        RUN pAddError(INPUT "Internal error - No PO found.", INPUT "1", INPUT 0, INPUT 0, INPUT 0).        
        RETURN. 
    END.

    IF NOT lohold AND bf-po-ord.stat NE "H" AND
        AVAILABLE vend AND vend.rebate-% NE 0 THEN
    DO:
        deTotalCost = 0.
        FOR EACH b-po-ordl FIELDS(t-cost) NO-LOCK WHERE
            b-po-ordl.company EQ bf-po-ord.company AND
            b-po-ordl.po-no EQ bf-po-ord.po-no
            :

            deTotalCost = deTotalCost + b-po-ordl.t-cost.
        END.

        IF deTotalCost GT vend.rebate-% THEN
        DO:
            RUN pAddError(INPUT "Purchase Order Cost Has Exceeded Vendor's Max P.O. Cost." + CHR(10) + "Purchase Order Will Be Placed On Hold.", 
                INPUT "4", INPUT 0, INPUT 0, INPUT 0).
            ASSIGN 
                lohold         = YES
                bf-po-ord.stat = "H".
        END. /* deTotalCost gt vend.rebate-% */
    END. /* not lohold ...*/
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
          updates ttJobMaterial
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprJob AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-job FOR job.

    FIND bf-job WHERE ROWID(bf-job) EQ iprJob NO-LOCK NO-ERROR.

    /* Create ttJobMaterial from item */
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

        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from b-job-mat " b-job-mat.i-no  SKIP.

        CREATE ttJobMaterial.
        BUFFER-COPY b-job-mat TO ttJobMaterial
            ASSIGN
            ttJobMaterial.w-rowid      = ROWID(b-job-mat)
            ttJobMaterial.w-recid      = RECID(b-job-mat)
            ttJobMaterial.this-is-a-rm = YES
            ttJobMaterial.dep          = item.s-dep
            ttJobMaterial.basis-w      = item.basis-w
            ttJobMaterial.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                                    ELSE bf-ordl.i-no
            ttJobMaterial.prep         = YES
            ttJobMaterial.rm-i-no      = b-job-mat.i-no
            ttJobMaterial.i-no         = b-job-mat.i-no
            ttJobMaterial.qty-uom      = "EA"
            ttJobMaterial.n-up         = b-job-mat.n-up.
            
            IF ttJobMaterial.dep EQ 0 THEN ttJobMaterial.dep = b-job-mat.dep.

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
        loPOBest
        bf-ordl   must exist
        b-jc-calc may exists
      Outputs:
        creates ttJobMaterial
    ------------------------------------------------------------------------------*/
    /* Create ttJobMaterial from job-mat */
    DEFINE INPUT  PARAMETER iplPoBest AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER iprJob    AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-job  FOR job.
    
    DEFINE VARIABLE cFGItemID AS CHARACTER NO-UNDO.
    
    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.
    FIND bf-oe-ordl NO-LOCK WHERE ROWID(bf-oe-ordl) EQ iprOeOrdl NO-ERROR.
    IF AVAILABLE bf-ordl THEN 
        cFGItemID = bf-oe-ordl.i-no.

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
        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no " iplpobest "
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
            AND b-jc-calc.code2    NE cFGItemID
            NO-ERROR.
        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no SKIP.
        CREATE ttJobMaterial.
        BUFFER-COPY job-mat TO ttJobMaterial
            ASSIGN
            ttJobMaterial.w-rowid      = ROWID(job-mat)
            ttJobMaterial.w-recid      = RECID(job-mat)
            ttJobMaterial.this-is-a-rm = YES
            ttJobMaterial.dep          = item.s-dep
            ttJobMaterial.basis-w      = item.basis-w
            ttJobMaterial.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                               ELSE cFGItemID.
        IF ttJobMaterial.dep EQ 0 THEN ttJobMaterial.dep = job-mat.dep.
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
        creates ttJobMaterial
        
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIno AS CHARACTER   NO-UNDO.

    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.

    IF NOT AVAILABLE job THEN
        RETURN.

    /* Create ttJobMaterial  from job-prep */
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
        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from job-prep " prep.i-no  SKIP.      
        CREATE ttJobMaterial.
        BUFFER-COPY job-prep TO ttJobMaterial
            ASSIGN
            ttJobMaterial.w-rowid      = ROWID(job-prep)
            ttJobMaterial.w-recid      = RECID(job-prep)
            ttJobMaterial.this-is-a-rm = YES
            ttJobMaterial.dep          = item.s-dep
            ttJobMaterial.basis-w      = item.basis-w
            ttJobMaterial.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2
                                                    ELSE ipcIno
            ttJobMaterial.prep         = YES
            ttJobMaterial.rm-i-no      = prep.i-no
            ttJobMaterial.i-no         = prep.i-no
            ttJobMaterial.qty-uom      = "EA"
            ttJobMaterial.n-up         = prep.number-up.
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
        creates ttJobMaterial
        
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
 
        IF loDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from est-prep " prep.i-no  SKIP.      
        CREATE ttJobMaterial.
        BUFFER-COPY oe-ordm EXCEPT po-no TO ttJobMaterial 
            ASSIGN
            ttJobMaterial.w-rowid      = ROWID(est-prep)
            ttJobMaterial.w-recid      = RECID(est-prep)
            ttJobMaterial.this-is-a-rm = YES
            ttJobMaterial.dep          = item.s-dep
            ttJobMaterial.basis-w      = item.basis-w
            ttJobMaterial.fg-i-no      = ipcIno
            ttJobMaterial.prep         = YES
            ttJobMaterial.rm-i-no      = prep.i-no
            ttJobMaterial.i-no         = prep.i-no
            ttJobMaterial.qty          = 1
            ttJobMaterial.qty-uom      = "EA"
            ttJobMaterial.n-up         = prep.number-up
            ttJobMaterial.eqty         = est-prep.eqty
            ttJobMaterial.est-no       = est-prep.est-no
            ttJobMaterial.frm          = est-prep.s-num
            ttJobMaterial.blank-no     = est-prep.b-num
            ttJobMaterial.std-cost     = oe-ordm.cost
            ttJobMaterial.sc-uom       = "EA"
            ttJobMaterial.po-no        = INTEGER(oe-ordm.po-no).
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
        loOEAutoFg loAutoFgSec bf-ord bf-ordl chOEAutoFG 
    ------------------------------------------------------------------------------*/
    /* wfk - Create ttJobMaterial based on tt-itemfg created above */
    /*       ttJobMaterial handling both fg and rm                 */

    DEFINE INPUT  PARAMETER ipcOeAutoFg AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrd     AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER iprOeOrdl    AS ROWID       NO-UNDO.
  
    DEFINE BUFFER bf-ord  FOR oe-ord.
    DEFINE BUFFER bf-ordl FOR oe-ordl.

    IF loDebug THEN
        PUT STREAM sDebug UNFORMATTED "Start create-w-job-from-tt-itemfg"  SKIP.

    FIND bf-ord NO-LOCK WHERE ROWID(bf-ord) EQ iprOeOrd NO-ERROR.
    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.

    FOR EACH tt-itemfg WHERE tt-itemfg.pur-man,
        FIRST itemfg NO-LOCK WHERE ROWID(itemfg) EQ tt-itemfg.row-id :

        IF loDebug THEN
            PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from tt-itemfg consider criteria " itemfg.i-no 
                " chOEAutoFG " chOEAutoFG " ipcOeAutofg " ipcOeAutofg SKIP.

        IF ((chOEAutoFG EQ "NonStock" OR ipcOeAutoFg EQ "Any") AND
            NOT itemfg.stocked)                                         OR
            ((ipcOeAutoFg EQ "LotCntrl" OR ipcOeAutoFg EQ "Any") AND
            NOT itemfg.ord-policy)                                      OR
            ((ipcOeAutoFg EQ "Avail<0"  OR ipcOeAutoFg EQ "Any") AND
            itemfg.q-avail LT 0)                                        THEN 
        DO:
  
            IF loDebug THEN
                PUT STREAM sDebug UNFORMATTED "Create ttJobMaterial from purchased tt-itemfg " itemfg.i-no  SKIP.
            CREATE ttJobMaterial.
            ASSIGN
                ttJobMaterial.w-recid      = ?
                ttJobMaterial.rm-i-no      = itemfg.i-no
                ttJobMaterial.i-no         = itemfg.i-no
                ttJobMaterial.fg-i-no      = itemfg.i-no
                ttJobMaterial.fg-part-no   = bf-ordl.part-no
                ttJobMaterial.est-no       = bf-ordl.est-no
                ttJobMaterial.frm          = tt-itemfg.form-no
                ttJobMaterial.blank-no     = tt-itemfg.blank-no
                ttJobMaterial.isaset       = itemfg.isaset
                ttJobMaterial.isacomponent = tt-itemfg.isacomponent
                ttJobMaterial.this-is-a-rm = NO
                ttJobMaterial.basis-w      = itemfg.t-wid * itemfg.t-len * 100
                ttJobMaterial.basis-w      = itemfg.weight-100 /
                                (IF loMSFcalc THEN (ttJobMaterial.basis-w * .007)
                                           ELSE (ttJobMaterial.basis-w / 144) /
                                 1000)
                ttJobMaterial.qty-uom      = "EA"
                ttJobMaterial.sc-uom       = itemfg.pur-uom
                ttJobMaterial.qty          = tt-itemfg.qty.
                RUN po/GetFGDimsForPO.p (ROWID(itemfg), OUTPUT ttJobMaterial.len, OUTPUT ttJobMaterial.wid, OUTPUT ttJobMaterial.dep).
  
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
          deS-Len
          deS-wid
          ipchCompany
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER iprPoOrdl     AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER oprItem       AS ROWID       NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.  
    DEFINE BUFFER b-item     FOR ITEM.
    
    FIND bf-po-ordl NO-LOCK WHERE ROWID(bf-po-ordl) EQ iprPoOrdl NO-ERROR.

    IF bf-po-ordl.item-type THEN /*DO*/
        FIND FIRST b-item NO-LOCK 
            WHERE b-item.company EQ ipchCompany
            AND b-item.i-no    EQ bf-po-ordl.i-no
            USE-INDEX i-no NO-ERROR.
            
    IF AVAILABLE b-item THEN
        oprItem = ROWID(b-item).
    IF AVAILABLE b-item AND b-item.i-code EQ "E"
        AND index("1234BPRWF",b-item.mat-type) GT 0 THEN 
    DO:
        IF (deS-Len EQ 0 OR deS-wid EQ 0) THEN 
        DO:
            RUN pAddError(INPUT "Invalid Length or Width. Enter Valid Job or Non-Zero Length and Width", 
                INPUT "4", INPUT 0, INPUT 0, INPUT 0).            
        /* return ??*/          
        END. /* deS-Len eq 0 ... */
    END. /* avail b-item ... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCheckFGItemCustHold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckFGItemCustHold Procedure 
PROCEDURE pCheckFGItemCustHold :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER       NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItem               AS CHARACTER       NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER oplFgItemCustHold AS LOGICAL         NO-UNDO.

    DEFINE VARIABLE cCurrentTitle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCurrentMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuppressMessage AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bff-itemfg FOR itemfg.  
    
    FIND FIRST bff-itemfg NO-LOCK
        WHERE bff-itemfg.company EQ ipcCompany
        AND bff-itemfg.i-no    EQ ipcFGItem NO-ERROR.
        
    IF AVAILABLE bff-itemfg AND bff-itemfg.cust-no NE ""  THEN 
    DO:
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipchCompany 
            AND cust.cust-no EQ bff-itemfg.cust-no NO-ERROR .
        IF AVAILABLE cust AND cust.cr-hold THEN 
            RUN displayMessageQuestionLOG ("12", OUTPUT oplFgItemCustHold).
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
 
 
&IF DEFINED(EXCLUDE-pCheckFGItemCustHold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckUserLimit Procedure 
PROCEDURE pCheckUserLimit :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord FOR po-ord.
    
    DEFINE VARIABLE lHoldPoStatus  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dPurchaseLimit AS DECIMAL NO-UNDO.

    IF TRIM(chPOStatus) = "User Limit" THEN
    DO: 
        RUN Po/POProcs.p PERSISTENT SET haPOProcs. 
        RUN PO_CheckPurchaseLimit IN haPOProcs(BUFFER ipbf-po-ord, OUTPUT lHoldPoStatus, OUTPUT dPurchaseLimit) .
        IF lHoldPoStatus THEN 
        DO:
            FIND CURRENT ipbf-po-ord EXCLUSIVE-LOCK NO-ERROR.
            
            ipbf-po-ord.stat    = "H". 
            scInstance = SharedConfig:instance.
            scInstance:SetValue("PurchaseLimit",TRIM(STRING(dPurchaseLimit))).
            
            RUN displayMessage ( INPUT 57).  
            FIND CURRENT ipbf-po-ord NO-LOCK NO-ERROR.
        END.
        DELETE OBJECT haPOProcs.
    END.       
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF 


/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-getVendCostQty) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVendCostQty Procedure
FUNCTION fGetVendCostQty RETURNS DECIMAL(ipdQty AS DEC, ipcFromUom AS CHAR, ipcToUom AS CHAR  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ReturnQty AS DECIMAL NO-UNDO.
       
    IF ipcToUom = ipcFromUom THEN ReturnQty = ipdQty.
    ELSE 
    DO:                                  
        RUN sys/ref/convquom.p(ipcFromUom, ipcToUom, ttJobMaterial.basis-w, ttJobMaterial.len, 
                               ttJobMaterial.wid, ttJobMaterial.dep, ipdQty, OUTPUT ReturnQty) .    
    END. 
       
    RETURN ReturnQty.

END FUNCTION.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

