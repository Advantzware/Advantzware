&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/actrel.i
    Purpose     : order entry - Create actual releases from planned 
                  release line  

    Syntax      :

    Description :

    Author(s)   : 01/98 JLF 
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-rowid         AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcAction         AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iocPrompt AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oprOeRelhRow  AS ROWID NO-UNDO.

/* ***************************  Definitions  ************************** */

{sys/inc/VAR.i SHARED}

DEFINE SHARED VARIABLE out-recid       AS RECID     NO-UNDO.
DEFINE SHARED VARIABLE relh-recid      AS RECID     NO-UNDO.
DEFINE SHARED VARIABLE v-auto          AS LOG       NO-UNDO.

DEFINE VARIABLE lPerformMerge        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lPromptForMerge      AS LOGICAL   INIT ? NO-UNDO.
DEFINE VARIABLE lEmail               AS LOGICAL   INIT YES NO-UNDO.
DEFINE VARIABLE iNextRno             AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE iMergeSelection      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCustNo              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAlreadyPosted       AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lRellFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMergeByPO           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iOeRellRno           LIKE oe-rell.r-no NO-UNDO.
DEFINE VARIABLE rOeRelhRow           AS ROWID     NO-UNDO.
DEFINE VARIABLE cShiptoLevelRelmerge AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcRelMergeCalc       AS CHARACTER NO-UNDO.
DEFINE VARIABLE rOeRelRow            AS ROWID     NO-UNDO.
DEFINE VARIABLE rOeRellRow2          AS ROWID     NO-UNDO.
DEFINE VARIABLE rOeRelhRow2          AS ROWID     NO-UNDO.
DEFINE VARIABLE cSCode               LIKE oe-rel.s-code NO-UNDO.
DEFINE VARIABLE cRefDscr             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMergeResponse       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMergeMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessageBody         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessageRelh         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMergeable           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFobDscr2            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPromptByItem        AS LOG       NO-UNDO.
DEFINE VARIABLE hdOutboundProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lReleaseCreated      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lActiveScope         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLocation       AS LOGICAL   NO-UNDO.

/* Procedure to prepare and execute API calls */
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

DEFINE BUFFER bf-notes        FOR notes.
DEFINE BUFFER bf-oe-ordl      FOR oe-ordl.
DEFINE BUFFER bf-oe-relh      FOR oe-relh .
DEFINE BUFFER b-reft-findrelh FOR reftable.
DEFINE BUFFER b-reft-fob      FOR reftable.
DEFINE BUFFER b2-oe-rell      FOR oe-rell.
DEFINE BUFFER b2-reftable     FOR reftable.
DEFINE BUFFER b-oe-rel        FOR oe-rel.
DEFINE BUFFER s-code          FOR reftable.
DEFINE BUFFER bf-rel          FOR oe-rel.

DEFINE STREAM s1.
{oe/chkordl.i}

{oe/relemail.i}

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.
          
&SCOPED-DEFINE for-each1 ~
    FOR EACH b-oe-rel NO-LOCK ~
         WHERE b-oe-rel.company  EQ oe-rel.company  ~
           AND b-oe-rel.po-no    EQ oe-rel.po-no    ~
           AND b-oe-rel.r-no     NE oe-rel.r-no     ~
           AND b-oe-rel.rel-date EQ oe-rel.rel-date ~
           AND b-oe-rel.cust-no  EQ oe-rel.cust-no
           
&SCOPED-DEFINE for-each2 ~
    EACH b2-oe-rell NO-LOCK ~
        WHERE b2-oe-rell.company EQ b-oe-rel.company ~
          AND b2-oe-rell.ord-no  EQ b-oe-rel.ord-no  ~
          AND b2-oe-rell.i-no    EQ b-oe-rel.i-no     

&SCOPED-DEFINE for-each3 ~
    EACH oe-relh NO-LOCK ~
        WHERE oe-relh.company  EQ cocode ~
          AND oe-relh.rel-date EQ oe-rel.rel-date ~
          AND oe-relh.cust-no  EQ cCustNo ~
          AND oe-relh.ship-id  EQ oe-rel.ship-id  ~
          AND oe-relh.posted   EQ NO ~
          AND oe-relh.deleted  EQ NO ~
          AND (oe-relh.printed EQ NO OR relmerge-log) ~
          AND oe-relh.r-no     EQ b2-oe-rell.r-no ~
          USE-INDEX r-no  ~
          BY oe-relh.upd-date DESCENDING ~
          BY oe-relh.upd-time DESCENDING 
                         
&SCOPED-DEFINE for-each4 ~
    FIND FIRST bf-oe-ordl NO-LOCK ~
        WHERE bf-oe-ordl.company EQ b-oe-rel.company ~
          AND bf-oe-ordl.ord-no  EQ b-oe-rel.ord-no ~
          AND bf-oe-ordl.line    EQ b-oe-rel.line ~
        NO-ERROR.
        
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
&IF DEFINED(EXCLUDE-fGetMergePrompt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetMergePrompt Procedure 
FUNCTION fGetMergePrompt RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-fGetShipToLevelRelmerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetShipToLevelRelmerge Procedure 
FUNCTION fGetShipToLevelRelmerge RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetRelmergeCharValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetRelmergeCharValue Procedure 
FUNCTION fGetRelmergeCharValue RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fIsRunFromBol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsRunFromBol Procedure 
FUNCTION fIsRunFromBol RETURNS LOGICAL
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
         HEIGHT             = 18.48
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DO TRANSACTION:
    {sys\inc\addxfer.i}
END.

FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

oprOeRelhRow = ?. 

IF ipcAction EQ "CREATE" THEN 
DO:
  
    RUN pAskToProceed (OUTPUT lPerformMerge, OUTPUT lPromptByItem).
    /* Setting these character values with the idea that we may need 'First One' */
    IF iocPrompt = "" THEN 
    DO:      
        iocPrompt = (IF lPromptByItem THEN "ALWAYS" ELSE "NEVER").
    END.
    IF lPerformMerge THEN
        RUN pCheckReadyMerge (OUTPUT lMergeable).
        
END.
ELSE
    ASSIGN 
        lPerformMerge = YES 
        lMergeable    = YES
        .

IF lPerformMerge AND lMergeable THEN 
DO:
    IF addxfer-log THEN
        RUN pGetSCode.

    RELEASE oe-relh.
    
    ASSIGN 
        lPromptForMerge      = ?
        cCustNo              = oe-rel.cust-no
        cSCode               = oe-rel.s-code
        cRefDscr             = oe-rel.fob-code
        lMergeByPO           = NO
        cShiptoLevelRelmerge = fGetShipToLevelRelmerge()        
        vcRelMergeCalc       = DYNAMIC-FUNCTION('fGetRelmergeCharValue':U)
        .
    
    IF cShiptoLevelRelmerge BEGINS "SamePo#Only" THEN
        lMergeByPO = YES.
    
    /** Determine if matchine oe-rell found for given oe-rel  */ 
    /** with same PO number, rel-date, cust-no                */ 
    /** Set iOeRellRno and lRellFound if it is found by       */
    /** ord-no as well as rel-date, ship-id, cust-no          */
    lRellFound = NO.
    
    /* To make sure not available initially */
    RELEASE oe-relh.
    
    RUN testers/actrelmergtester.p(
        INPUT ip-rowid
        ).
        
    RUN pFindMatchingOerell (OUTPUT lRellFound, OUTPUT iOeRellRno, 
        OUTPUT rOeRelRow, OUTPUT rOeRellRow2,
        OUTPUT rOeRelhRow2).

    
    /*** Search for oe-relh to match to oe-rel by rel-date, cust-no, ship-to    ***/
    /*** with exceptions for when relmerge value is sameOrderONly or samePoOnly ***/
    /*** In those cases, oe-rell must have been matched also. If it was, leave  ***/
    /*** the loop with oe-relh available                                        ***/
    rOeRelhRow = ?.

    /* Try matching first with the oe-relh that was just created */
    IF relh-recid NE ? THEN
        RUN pFindMatchingRelh (INPUT YES, OUTPUT rOeRelhRow).      
    IF rOeRelhRow EQ ? THEN
        RUN pFindMatchingRelh (INPUT NO, OUTPUT rOeRelhRow).    
    IF rOeRelhRow NE ?  THEN
        FIND oe-relh WHERE ROWID(oe-relh) EQ rOeRelhRow NO-ERROR.
  
    IF AVAILABLE oe-relh THEN 
    DO:
        /****  If an oe-relh was found matching the given oe-rel           ****/
        /****  then see if we have to prompt to merge into this existing   ****/
    
        /**** Determines if merge value is set per customer for samePO#Only ***/  
        IF ((NOT cShiptoLevelRelmerge EQ "SamePo#Only")
            OR (lRellFound AND oe-relh.r-no = iOeRellRno)) THEN            
            lPromptForMerge = fGetMergePrompt().
    END. /* If Avail Oe-relh based on include file */
  
    IF AVAILABLE oe-relh THEN 
    DO:
        RUN pSetMergeMsg (INPUT vcRelMergeCalc, OUTPUT cMergeMessage, 
            OUTPUT cMessageBody, OUTPUT cMessageRelh).
  
        IF vcRelMergeCalc NE "SamePo#Only" AND vcRelMergeCalc BEGINS "SamePo#Only" THEN
            lPromptForMerge = fGetMergePrompt().
        ELSE
            lPromptForMerge = IF iocPrompt = "ALWAYS" THEN TRUE ELSE FALSE.
 
        out-recid = RECID(oe-relh).
        IF fIsRunFromBol() THEN
            lPromptForMerge = NO.

        /* lPromptForMerge is initially ? */
        IF (v-auto AND lPromptForMerge NE YES) 
            OR (lPromptForMerge EQ NO) THEN  
            iMergeSelection = 1.
        ELSE 
        DO:     
            lMergeResponse = ?.
            IF lPromptForMerge THEN
                RUN pAskToMerge (OUTPUT lMergeResponse).
            CASE lMergeResponse:
                WHEN YES THEN 
                    iMergeSelection = 1.
                WHEN NO  THEN 
                    iMergeSelection = 2.
                OTHERWISE 
                iMergeSelection = 3.
            END CASE.
        END. /* Not v-auto, ask user whether to merge */
    
        IF iocPrompt = "FIRST" THEN 
        DO:
            ASSIGN 
                iMergeSelection = 2 /* don't merge on first one */
                iocPrompt       = "NEVER".
        END.
    
    END. /* If avail oe-relh */  
  
    IF ipcAction = "FINDRELH" THEN 
    DO:
        IF AVAILABLE oe-relh THEN
            oprOeRelhRow = ROWID(oe-relh).
        RETURN.
    END.

    /* Answered 'Cancel' */
    IF iMergeSelection EQ 3 THEN RETURN.
  
    /* Answered 'NO', so a separate release is created */
    IF iMergeSelection EQ 2 OR NOT AVAILABLE oe-relh THEN 
    DO: 
        RUN oe/cre-relh.p (RECID(oe-rel)).
        FIND FIRST bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ oe-rel.company
            AND bf-oe-ordl.ord-no  EQ oe-rel.ord-no
            AND bf-oe-ordl.line    EQ oe-rel.line
            NO-ERROR.
        FIND bf-oe-relh WHERE RECID(bf-oe-relh) EQ relh-recid NO-LOCK.
         /* Check to trigger createrelease when release header creates */
        IF ipcAction EQ "CREATE" AND AVAILABLE bf-oe-relh THEN
            lReleaseCreated = YES.

        IF AVAILABLE bf-oe-ordl THEN 
            FOR EACH notes NO-LOCK WHERE notes.rec_key = bf-oe-ordl.rec_key 
                AND notes.note_type <> "S" AND notes.note_type <> "o"  :
                IF AVAILABLE bf-oe-relh THEN 
                DO:
                    CREATE bf-notes .
                    BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                    ASSIGN 
                        bf-notes.rec_key = bf-oe-relh.rec_key .
                END.
            END.
    END.
    ELSE
        /* If merging, set the recid of the one that was found since it */
        /* may not be the last one                                      */
        IF AVAILABLE oe-relh THEN
            relh-recid = RECID(oe-relh).
  
    /****** If Answered 'YES', using the existing oe-relh implicitly ******/
    IF lEmail THEN  
        RUN pCreateTTEmail.
  
    /*******  Create Actual Release   ******/
    RUN oe/cre-rell.p (RECID(oe-rel)).
    IF iocPrompt = "FIRST" THEN 
    DO:
        ASSIGN 
            iMergeSelection = 2 /* don't merge on first one */
            iocPrompt       = "NEVER"
            .
    END.
    
    /* calls sendrelease by triggering createrelease trigger */
    IF lReleaseCreated THEN DO:
        RUN Outbound_IsApiScopeActive IN hdOutboundProcs(
            INPUT oe-rel.company,
            INPUT oe-rel.spare-char-1,
            INPUT "SendRelease",
            INPUT oe-rel.cust-no,
            INPUT "Customer",
            INPUT "CreateRelease",
            OUTPUT lActiveScope
            ).
        IF lActiveScope THEN DO:
            RUN Outbound_ValidateLocation IN hdOutboundProcs(
                  INPUT oe-rel.company,
                  INPUT oe-rel.spare-char-1,
                  INPUT "SendRelease",
                  OUTPUT lValidLocation,
                  OUTPUT cMessage 
                  ).
            IF NOT lValidLocation THEN DO:
                SESSION:SET-WAIT-STATE (''). 
                RETURN. 
            END.
            DO TRANSACTION:    
                FIND CURRENT bf-oe-relh EXCLUSIVE-LOCK NO-ERROR.
                bf-oe-relh.printed = YES.
                FIND CURRENT bf-oe-relh NO-LOCK NO-ERROR.
            END.                                
        END. 
                    
        RUN pRunAPIOutboundTrigger (
            BUFFER bf-oe-relh,
            INPUT  "CreateRelease"
            ).
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pAskToMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAskToMerge Procedure 
PROCEDURE pAskToMerge :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplCombineItems AS LOGICAL NO-UNDO.
    /* *** Ask the user - Based on lPromptForMerge value ******  */
    MESSAGE cMergeMessage SKIP(1)
        "     " +  cMessageRelh
        SKIP(1)
        "Matching the current release line: "
        SKIP(1)
        "     " + cMessageBody
        SKIP(1)
        "Choose YES to print multiple items on one Release/Pick ticket.,Choose NO to create a separate Release/Pick ticket for each item."
        VIEW-AS ALERT-BOX BUTTON YES-NO-CANCEL UPDATE lMergeResponse AS LOG.
    oplCombineItems = lMergeResponse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAskToProceed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAskToProceed Procedure 
PROCEDURE pAskToProceed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER lPerformMerge AS LOG NO-UNDO.    
DEFINE OUTPUT PARAMETER opPromptItem AS LOG NO-UNDO.
DEFINE VARIABLE cInputParms     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputValues   AS CHARACTER NO-UNDO.
DEFINE VARIABLE llFromAutoRun   AS LOG NO-UNDO.
DEFINE VARIABLE lcMergeMessage  AS CHARACTER NO-UNDO.

/* This prompt is supressed if from autorun.p */
IF INDEX(PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE ""), "autorel") GT 0 THEN
  llFromAutoRun = TRUE.
ELSE
  llFromAutoRun = FALSE.

ASSIGN
    lPerformMerge = YES
    opPromptItem  = relmerge-log
    .
    
IF NOT v-auto AND NOT llFromAutoRun THEN DO:

    lcMergeMessage = "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) + 
                     " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?" .
         
    cInputParms = 
          "type=literal,name=fi4,row=3,col=18,enable=false,width=58,scrval=" + lcMergeMessage + ",FORMAT=X(58)"
          + "|type=image,image=webspeed\images\question.gif,name=im1,row=2,col=5,enable=false" 
          + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=8".

   RUN custom/d-prompt.w (INPUT "yes-no", cInputParms, "", OUTPUT cOutputValues).
   
    /* Default */    
    opPromptItem = relmerge-log.
    DO i = 1 TO NUM-ENTRIES(cOutputValues) BY 2.
        IF ENTRY(i, cOutputValues) EQ "default" THEN
          lPerformMerge = LOGICAL(ENTRY(i + 1, cOutputValues)) NO-ERROR.
        IF ENTRY(i, cOutputValues) EQ "tg1" THEN
          opPromptItem = LOGICAL(ENTRY(i + 1, cOutputValues)) NO-ERROR.          

    END.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCheckAlreadyPosted) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckAlreadyPosted Procedure 
PROCEDURE pCheckAlreadyPosted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplAlreadyPosted AS LOG NO-UNDO.

oplAlreadyPosted = NO.

rel-block:
  /** Find the most recent actual release with the same #order, shipto,
  release date, not deleted, and not printed. **/
  REPEAT PRESELECT
    EACH oe-relh NO-LOCK
      WHERE oe-relh.company  EQ oe-rel.company
        AND oe-relh.rel-date EQ oe-rel.rel-date
        AND oe-relh.cust-no  EQ oe-rel.cust-no
        AND oe-relh.ship-id  EQ oe-rel.ship-id
        AND oe-relh.posted   EQ NO
        AND oe-relh.deleted  EQ NO
    USE-INDEX rel-date,
    
    EACH oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
        AND oe-rell.ord-no  EQ oe-rel.ord-no
        AND oe-rell.i-no    EQ oe-rel.i-no
        AND oe-rell.rel-no  EQ oe-rel.rel-no 
    :
    
    FIND NEXT oe-rell NO-ERROR.

    IF NOT AVAILABLE oe-rell THEN LEAVE rel-block.

    IF AVAILABLE oe-rell THEN DO:
      IF NOT v-auto  THEN DO:
        oplAlreadyPosted = YES.
        LEAVE rel-block.
      END. /* Not V-auto */
    END. /* Avail Oe-rell */
  END. /* repeat preselect oe-relh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCheckReadyMerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckReadyMerge Procedure 
PROCEDURE pCheckReadyMerge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReady AS LOG NO-UNDO.

  oplReady = YES.
  RUN pCheckAlreadyPosted (OUTPUT lAlreadyPosted).

  IF lAlreadyPosted AND NOT v-auto THEN DO:
    MESSAGE "This has been already been released. Can not release."
    VIEW-AS ALERT-BOX ERROR.
    oplReady = NO.
  END. /* Not V-auto */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCreateTTEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTTEmail Procedure 
PROCEDURE pCreateTTEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE tt-email.
    ASSIGN
      tt-email.cust-no = oe-rel.cust-no
      tt-email.ord-no  = oe-rel.ord-no
      tt-email.i-no    = oe-rel.i-no
      tt-email.rel-qty = oe-rel.qty
      tt-email.rel-date = IF AVAILABLE oe-relh THEN oe-relh.rel-date
                          ELSE oe-rel.rel-date
      tt-email.po-no    = oe-rel.po-no
      .
    
    RELEASE tt-email.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFindMatchingOerell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFindMatchingOerell Procedure 
PROCEDURE pFindMatchingOerell :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes: wfk - seems to indicate a match on PO# and returns matching 
             oe-relh.r-no      
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOeRellFound   AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOeRellRno     AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow     AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRellRow2    AS ROWID   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprOeRelhRow2    AS ROWID   NO-UNDO.
    
    DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE lRellFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOeRellRno AS INTEGER NO-UNDO.

    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ oe-rel.company
          AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
          AND bf-oe-ordl.line EQ oe-rel.line
        NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.    

    lRellFound = NO.
     IF vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO" THEN DO:
        REL-MATCH:
            {&for-each1}
                AND b-oe-rel.ord-no EQ oe-rel.ord-no,
            {&for-each2}
                AND b2-oe-rell.loc EQ oe-rel.spare-char-1,
            {&for-each3}:
            {&for-each4}
                   
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.
    END.
    ELSE IF vcRelMergeCalc NE "SameOrder&SameShipFrom&SamePO" AND 
         INDEX(vcRelMergeCalc, "SameOrder") GT 0 THEN DO:  
              
        REL-MATCH1:
            {&for-each1}
                AND b-oe-rel.ord-no EQ oe-rel.ord-no,
            {&for-each2},
            {&for-each3}:
            {&for-each4}
                           
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.    
    END.
    ELSE DO:
       REL-MATCH2:
           {&for-each1},
           {&for-each2},        
           {&for-each3}:
            {&for-each4}
         
            IF vcRelMergeCalc EQ "AllOrders&NotRunShip" AND AVAIL(bf-oe-ordl) THEN 
            DO:
                /* Criteria for AllOrders&ShipFromWhse, Run & ship can't be merged with any other */
                IF  bf-oe-ordl.whsed = TRUE OR lRunShip EQ TRUE THEN 
                    NEXT REL-MATCH2.                  
            END.
                   
            ASSIGN 
                lRellFound    = TRUE
                iOeRellRno    = b2-oe-rell.r-no
                oprOeRellRow  = ROWID(b-oe-rel)
                oprOeRellRow2 = ROWID(b-oe-rel)
                oprOeRelhRow2 = ROWID(oe-relh)
                .
    
            LEAVE.
        END.        
    END.    
    ASSIGN
        oplOeRellFound = lRellFound
        opiOeRellRno = iOeRellRno
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFindMatchingRelh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFindMatchingRelh Procedure 
PROCEDURE pFindMatchingRelh :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:  
            Search for oe-relh to match to oe-rel by rel-date, cust-no, ship-to 
            with exceptions for when relmerge value is sameOrderONly or samePoOnly
            In those cases, oe-rell must have been matched also. If it was, leave
            the loop with oe-relh available. If iplUsingNew, tries to match only
            with a newly created oe-relh via relh-recid set in cre-rell
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplUsingNew  AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oprRelhRowid AS ROWID NO-UNDO.

    DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ oe-rel.company
          AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
          AND bf-oe-ordl.line EQ oe-rel.line
        NO-ERROR.
     IF AVAILABLE bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.
 

    rOeRelhRow = ?.
    oe-relh-loop:
    FOR EACH oe-relh NO-LOCK
        WHERE  oe-relh.company EQ oe-rel.company
          AND oe-relh.rel-date EQ oe-rel.rel-date
          AND oe-relh.cust-no  EQ cCustNo
          AND oe-relh.ship-id  EQ oe-rel.ship-id        
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND (oe-relh.printed EQ NO OR relmerge-log) 
          AND (IF iplUsingNew THEN RECID(oe-relh) EQ relh-recid ELSE TRUE)

          /* **** Either the reftable matching relh is not found or the s-code matches *** */
          /* **** the one found                                     * */
          AND (NOT cSCode GT "" OR
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.s-code EQ cSCode))

          /* ****** Either it's not 'SameOrderOnly', or the order number does match ****/
          AND ((vcRelMergeCalc NE "SameOrderOnly" OR lMergeByPO) OR
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.ord-no EQ oe-rel.ord-no))

          /* ****** Either it's not '..ShipFromWhse', or the ShipFromWhse  does match ****/
          AND ((NOT vcRelMergeCalc EQ "AllOrders&ShipFromWhse") OR
               /* Criteria for AllOrders&ShipFromWhse */
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))

          /* ****** Either it's not 'SamePO#Only' for this customer or lRellFound * ***/
          AND ((NOT vcRelMergeCalc  BEGINS "SamePO#Only")                          
                OR  
                /* Criteria for SamePO#Only */
                (lRellFound AND oe-relh.r-no = iOeRellRno))      

          /* ****** Either it's not '..SamePO#&ShipFromWhse', or the PO# & ShipFromWhse  does match ****/
          AND ((NOT vcRelMergeCalc EQ "SamePO#&ShipFromWhse") OR
               /* Criteria for Same ship-from */
               (CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
               AND /* Criteria for Same PO# */ 
                 (lRellFound AND oe-relh.r-no = iOeRellRno))
     
          /* ****** Either it's not '..SameOrder&SameShipFrom', or the Order # & ShipFromWhse does match ****/
           AND ((NOT vcRelMergeCalc EQ "SameOrder&SameShipFrom") OR
                   /* Criteria for Same ship-from */
                   (CAN-FIND(FIRST oe-rell
                            WHERE oe-rell.r-no   EQ oe-relh.r-no
                              AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
                   AND /* Criteria for same order */
                         CAN-FIND(FIRST oe-rell
                            WHERE oe-rell.r-no   EQ oe-relh.r-no
                              AND oe-rell.ord-no EQ oe-rel.ord-no))
        
          /* ****** Either it's not '..SameOrder&SameShipFrom&SamePO', or the Order #, PO#, ShipFromWhse does match ****/
           AND ((NOT vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO") OR
                   /* Criteria for Same ship-from */
                   (CAN-FIND(FIRST oe-rell
                            WHERE oe-rell.r-no   EQ oe-relh.r-no
                              AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
                   AND /* Criteria for same order */
                         CAN-FIND(FIRST oe-rell
                            WHERE oe-rell.r-no   EQ oe-relh.r-no
                              AND oe-rell.ord-no EQ oe-rel.ord-no)
                   AND /* Criteria for SamePO#Only */
                    (lRellFound AND oe-relh.r-no = iOeRellRno))

        USE-INDEX rel-date 
        BY oe-relh.printed
        BY oe-relh.r-no:


        /* ****** Either it's not '..AllOrders&NoRunShip', or the oeitem is not run & ship ****/
         IF vcRelMergeCalc EQ "AllOrders&NotRunShip" THEN DO:
            
            /* Criteria for AllOrders&ShipFromWhse */
            FIND FIRST  bf-oe-rell
                 WHERE bf-oe-rell.r-no   EQ oe-relh.r-no
                 NO-LOCK NO-ERROR.
          

            IF (AVAILABLE bf-oe-rell AND CAN-FIND(FIRST oe-ordl
            WHERE oe-ordl.company   EQ bf-oe-rell.company
              AND oe-ordl.ord-no EQ bf-oe-rell.ord-no
              AND oe-ordl.line EQ bf-oe-rell.line
              AND oe-ordl.whsed EQ YES)) OR lRunShip THEN 
                NEXT.
              
        END.  
        
        
        /* Check that the fob desc for this oe-relh matches that of oe-rel */
/*        IF cRefDscr GT "" THEN                                  */
/*        DO:                                                     */
/*           FOR EACH b2-oe-rell NO-LOCK                          */
/*                WHERE b2-oe-rell.r-no EQ oe-relh.r-no           */
/*                :                                               */
/*               cFobDscr2 = b2-oe-rell.fob-code.                 */
/*               /* Find of fob description for this oe-rell */   */
/*               IF (cFobDscr2 GT "" AND cFobDscr2 NE cRefDscr) OR*/
/*                  (cFobDscr2 EQ "" AND cRefDscr NE "") THEN     */
/*                  NEXT oe-relh-loop.                            */
/*           END.                                                 */
/*        END.                                                    */
        rOeRelhRow = ROWID(oe-relh).
        
        LEAVE oe-relh-loop.
    END.
    
    oprRelhRowid = rOeRelhRow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetCustX) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCustX Procedure 
PROCEDURE pGetCustX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST cust NO-LOCK
    WHERE cust.company EQ cocode 
      AND cust.active EQ 'X'
    NO-ERROR.

  IF AVAILABLE cust THEN
  DO:
    IF CAN-FIND(FIRST shipto NO-LOCK
          WHERE shipto.company EQ cocode 
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-no EQ oe-rel.ship-no 
            AND shipto.ship-id EQ oe-rel.ship-id) THEN
    cCustNo = cust.cust-no.

    RELEASE cust.
  END. /* Avail Cust */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetSCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSCode Procedure 
PROCEDURE pGetSCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF oe-rel.s-code EQ 'T' THEN
          RUN pGetCustX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetMergeMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetMergeMsg Procedure 
PROCEDURE pSetMergeMsg :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       cMessageRelh is same as cMessageCore except for inclusion of i-no
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcMergeType   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessageCore AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessageRelh AS CHARACTER NO-UNDO.

CASE ipcMergeType:
    WHEN "" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date:"
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                  "/" + oe-rel.i-no
                                  .
    WHEN "AllOrders" OR WHEN "AllOrders&NotRunShip" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                  "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        .
    WHEN "SameOrderOnly" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/Order:"
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                   "/" + oe-rel.i-no 
                                   .
    WHEN "SamePo#Only" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no  +
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no
                           .
    WHEN "SamePo#OnlyWithPrompt" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no +
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no
                           .
    WHEN "SamePo#OnlyWithoutPrompt" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no +
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no
                           .
    WHEN "AllOrders&ShipFromWhse" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/ShipFrom:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +
                                   oe-rel.spare-char-1 +
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   oe-rel.spare-char-1 
                                   .
    WHEN "SamePO#&ShipFromWhse" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/PO/ShipFrom:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 +
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 
                                   .
    WHEN "SameOrder&SameShipFrom" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/Order/ShipFrom:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +                                   
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.spare-char-1 + 
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.spare-char-1 
                                   .
    WHEN "SameOrder&SameShipFrom&SamePO" THEN ASSIGN 
        opcMessage = "A previous release exists for Customer/Ship-To/Date/Order/PO/ShipFrom:"
        opcMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +                                   
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 + 
                                   "/" + oe-rel.i-no
        opcMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 
                                   .    

  END CASE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE pRunAPIOutboundTrigger:
/*------------------------------------------------------------------------------
 Purpose:  Fires Outbound APIs for given release header
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-relh FOR oe-relh.
    DEFINE INPUT PARAMETER ipcTriggerID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
       
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    
    IF AVAILABLE ipbf-oe-relh THEN DO:
        FOR EACH bf-oe-rell NO-LOCK 
            WHERE bf-oe-rell.company EQ ipbf-oe-relh.company
            AND bf-oe-rell.r-no EQ ipbf-oe-relh.r-no,
            FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ bf-oe-rell.company
            AND bf-itemfg.i-no EQ bf-oe-rell.i-no
            BREAK BY bf-oe-rell.r-no  /*In order to get .loc from first oe-rell as "shipFrom"*/
            BY bf-oe-rell.i-no:
            
            IF FIRST-OF(bf-oe-rell.r-no) THEN DO:
                ASSIGN 
                    cAPIID       = "SendRelease"
                    cPrimaryID   = STRING(ipbf-oe-relh.release#)
                    cDescription = cAPIID + " triggered by " + ipcTriggerID 
                                 + " from actrelmerg.p for Release: " + cPrimaryID
                    . 

                RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
                    INPUT  ipbf-oe-relh.company,        /* Company Code (Mandatory) */
                    INPUT  bf-oe-rell.loc,              /* Location Code (Mandatory) */
                    INPUT  cAPIID,                      /* API ID (Mandatory) */
                    INPUT  ipbf-oe-relh.cust-no,        /* Scope ID*/
		            INPUT  "Customer",                  /* Scope Type */
                    INPUT  ipcTriggerID,                /* Trigger ID (Mandatory) */
                    INPUT  "oe-relh",                   /* Comma separated list of table names for which data being sent (Mandatory) */
                    INPUT  STRING(ROWID(ipbf-oe-relh)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                    INPUT  cPrimaryID,              	/* Primary ID for which API is called for (Mandatory) */   
                    INPUT  cDescription,       			/* Event's description (Optional) */
                    OUTPUT lSuccess,                	/* Success/Failure flag */
                    OUTPUT cMessage                 	/* Status message */
                    ) NO-ERROR.
            END.
        END.               
        /* Reset context at the end of API calls to clear temp-table 
           data inside OutboundProcs */
        RUN Outbound_ResetContext IN hdOutboundProcs.
    END.
END PROCEDURE.
&ENDIF

/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-fGetMergePrompt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetMergePrompt Procedure 
FUNCTION fGetMergePrompt RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Merge-prompt is initially ?, this uses the NK1 to determine if 
            will prompt the user to merge
------------------------------------------------------------------------------*/
        
  FIND FIRST sys-ctrl-shipto
    WHERE sys-ctrl-shipto.company        = oe-relh.company
      AND sys-ctrl-shipto.NAME           = "RelMerge"
      AND sys-ctrl-shipto.cust-vend      = YES
      AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
      AND sys-ctrl-shipto.cust-vend-no   = oe-relh.cust-no
  NO-LOCK NO-ERROR.
  IF AVAILABLE sys-ctrl-shipto THEN DO:
  
    /* Set the lPromptForMerge value if sys-ctrl-shipto was found */
    IF INDEX(sys-ctrl-shipto.char-fld, "WithPrompt") > 0 THEN
        lPromptForMerge = YES.
    ELSE IF INDEX(sys-ctrl-shipto.char-fld, "WithoutPrompt") > 0 THEN
      lPromptForMerge = NO.
    ELSE
      lPromptForMerge = ?.
    RETURN lPromptForMerge.
  END.
  ELSE
    RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-fGetShipToLevelRelmerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetShipToLevelRelmerge Procedure 
FUNCTION fGetShipToLevelRelmerge RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST sys-ctrl-shipto
    WHERE sys-ctrl-shipto.company = oe-rel.company
      AND sys-ctrl-shipto.NAME = "RelMerge"
      AND sys-ctrl-shipto.cust-vend = YES
      AND sys-ctrl-shipto.cust-vend-no = cCustNo
    NO-LOCK NO-ERROR.

  IF AVAILABLE sys-ctrl-shipto THEN
      RETURN sys-ctrl-shipto.char-fld.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetRelmergeCharValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetRelmergeCharValue Procedure 
FUNCTION fGetRelmergeCharValue RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cShiptoLevelRelmerge AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcRelMergeCalc        AS CHARACTER NO-UNDO.

  cShiptoLevelRelmerge = fGetShipToLevelRelmerge().
  vcRelMergeCalc = relmerge-chr.
  IF cShiptoLevelRelmerge GT "" THEN
      vcRelMergeCalc = cShiptoLevelRelmerge.
  RETURN vcRelMergeCalc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fIsRunFromBol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIsRunFromBol Procedure 
FUNCTION fIsRunFromBol RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  If run from relbol (create bol button) then don't prompt per Joe
------------------------------------------------------------------------------*/
DEFINE VARIABLE lvStr AS CHARACTER NO-UNDO.
lvStr =  (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
        + (IF PROGRAM-NAME(8) NE ? THEN "," + PROGRAM-NAME(8) ELSE "")
        + (IF PROGRAM-NAME(9) NE ? THEN "," + PROGRAM-NAME(9) ELSE "")
        + (IF PROGRAM-NAME(10) NE ? THEN "," + PROGRAM-NAME(10) ELSE "")
        + (IF PROGRAM-NAME(11) NE ? THEN "," + PROGRAM-NAME(11) ELSE "")
        + (IF PROGRAM-NAME(12) NE ? THEN "," + PROGRAM-NAME(12) ELSE "").

IF INDEX(lvStr, "bol") GT 0 THEN
    RETURN TRUE.
ELSE
    RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

