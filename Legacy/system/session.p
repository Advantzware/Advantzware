&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : session.p
    Purpose     : Super Procedure for Session

    Syntax      : RUN system\session.p PERSISTENT SET hProc.
                  SESSION:ADD-SUPER-PROCEDURE (hProc).

    Description : session procedures

    Author(s)   : Ron Stark
    Created     : 7.6.2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE hMainMenuHandle     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hSysCtrlUsageHandle AS HANDLE  NO-UNDO.
/* cue card variables */
DEFINE VARIABLE iCueOrder           AS INTEGER NO-UNDO.
DEFINE VARIABLE lNext               AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCueCardActive      AS LOGICAL NO-UNDO.

{system/ttSysCtrlUsage.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fCueCardActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCueCardActive Procedure
FUNCTION fCueCardActive RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfClearTtSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfClearTtSysCtrlUsage Procedure 
FUNCTION sfClearTtSysCtrlUsage RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetMainMenuHandle Procedure 
FUNCTION sfGetMainMenuHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetNextRecKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetNextRecKey Procedure 
FUNCTION sfGetNextRecKey RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetSysCtrlUsageHandle Procedure 
FUNCTION sfGetSysCtrlUsageHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetTtSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfGetTtSysCtrlUsageHandle Procedure 
FUNCTION sfGetTtSysCtrlUsageHandle RETURNS HANDLE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfSetMainMenuHandle Procedure 
FUNCTION sfSetMainMenuHandle RETURNS LOGICAL
  (iphMainMenuHandle AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfSetSysCtrlUsageHandle Procedure 
FUNCTION sfSetSysCtrlUsageHandle RETURNS LOGICAL
  (iphSysCtrlUsageHandle AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfUserSecurityLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sfUserSecurityLevel Procedure 
FUNCTION sfUserSecurityLevel RETURNS INTEGER
  (  ) FORWARD.

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

FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID("ASI")
     NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-spCreateSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCreateSysCtrlUsage Procedure 
PROCEDURE spCreateSysCtrlUsage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcModule        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcName          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCharFld       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDateFld      AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipdDecFld        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiIntFld        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iplLogFld        AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescrip       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtUsageNow     AS DATETIME  NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCustVend      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustVendNo    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSeqNo         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipId        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubCategory   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSysCtrlID     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcTypeCode      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStackTrace    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSecurityLevel AS INTEGER   NO-UNDO.

    IF sfUserSecurityLevel() LT ipiSecurityLevel THEN 
    ASSIGN 
        ipcCharFld    = ?
        ipdtDateFld   = ?
        ipdDecFld     = ?
        ipiIntFld     = ?
        iplLogFld     = ?
        ipcDescrip    = "Administratively Blocked from View"
        iplCustVend   = ?
        ipcCustVendNo = ?
        ipcShipID     = ?
        .
    FIND FIRST ttSysCtrlUsage
         WHERE ttSysCtrlUsage.company  EQ ipcCompany
           AND ttSysCtrlUsage.module   EQ ipcModule
           AND ttSysCtrlUsage.name     EQ ipcName
         NO-ERROR.
    IF NOT AVAILABLE ttSysCtrlUsage THEN
    CREATE ttSysCtrlUsage.
    ASSIGN 
        ttSysCtrlUsage.company      = ipcCompany
        ttSysCtrlUsage.module       = ipcModule
        ttSysCtrlUsage.name         = ipcName
        ttSysCtrlUsage.char-fld     = ipcCharFld
        ttSysCtrlUsage.date-fld     = ipdtDateFld
        ttSysCtrlUsage.dec-fld      = ipdDecFld
        ttSysCtrlUsage.int-fld      = ipiIntFld
        ttSysCtrlUsage.log-fld      = iplLogFld
        ttSysCtrlUsage.descrip      = ipcDescrip
        ttSysCtrlUsage.usageNow     = ipdtUsageNow
        ttSysCtrlUsage.category     = ipcCategory
        ttSysCtrlUsage.cust-vend    = iplCustVend
        ttSysCtrlUsage.cust-vend-no = ipcCustVendNo
        ttSysCtrlUsage.seqNo        = ipiSeqNo
        ttSysCtrlUsage.ship-id      = ipcShipID
        ttSysCtrlUsage.subCategory  = ipcSubCategory
        ttSysCtrlUsage.sysCtrlID    = ipiSysCtrlID
        ttSysCtrlUsage.typeCode     = ipcTypeCode
        ttSysCtrlUsage.stackTrace   = ipcStackTrace
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spCueCardClose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCueCardClose Procedure
PROCEDURE spCueCardClose:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    iCueOrder = 99999.
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spCueCardFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spCueCardFrame Procedure
PROCEDURE spCueCardFrame:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    iphWidget:MOVE-TO-TOP ().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spSetDontShowAgain) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetDontShowAgain Procedure
PROCEDURE spSetDontShowAgain:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    CREATE xCueCard.
    ASSIGN
        xCueCard.user_id   = USERID("ASI")
        xCueCard.cueType   = cueCard.cueType
        xCueCard.cueTextID = cueCardText.cueTextID
        .
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spSetDismiss) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spSetDismiss Procedure
PROCEDURE spSetDismiss:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bCueCard     FOR cueCard.
    DEFINE BUFFER bCueCardText FOR cueCardText.
    
    FOR EACH bCueCardText NO-LOCK
        WHERE bCueCardText.cueID EQ cueCardText.cueID,
        FIRST bCueCard NO-LOCK
        WHERE bCueCard.cueType   EQ cueCard.cueType
          AND bCueCard.cueID     EQ cueCardText.cueID
        :
        IF CAN-FIND(FIRST xCueCard
                    WHERE xCueCard.user_id   EQ USERID("ASI")
                      AND xCueCard.cueType   EQ bCueCard.cueType
                      AND xCueCard.cueTextID EQ bCueCardText.cueTextID) THEN
        NEXT.
        CREATE xCueCard.
        ASSIGN
            xCueCard.user_id   = USERID("ASI")
            xCueCard.cueType   = bCueCard.cueType
            xCueCard.cueTextID = bCueCardText.cueTextID
            .
    END. /* each bcuecardtext */
    iCueOrder = 99999.
    RUN spNextCue (iphWidget).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spNextCue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spNextCue Procedure 
PROCEDURE spNextCue :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        iCueOrder = iCueOrder + 1
        lNext     = YES 
        .
    
    APPLY "U1":U TO iphWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spPrevCue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spPrevCue Procedure 
PROCEDURE spPrevCue :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        iCueOrder = iCueOrder - 1
        lNext     = NO
        .
    IF iCueOrder LT 1 THEN
    iCueOrder = ?.
    
    APPLY "U1":U TO iphWidget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spRunCueCard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spRunCueCard Procedure 
PROCEDURE spRunCueCard :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcPrgmName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphContainer AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iplActive    AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cCueCardPool        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCueCardFrame       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardRectangle   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hClose              AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardArrow       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardPrev        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardNext        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hCueCardText        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hDismiss            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hDontShowAgain      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dCol                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRow                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTitle              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iPosition           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCueType            AS CHARACTER NO-UNDO INITIAL
        "System,Note,Message".
    DEFINE VARIABLE cOrientation        AS CHARACTER NO-UNDO INITIAL ~
"default_LeftUp,default_Up,default_RightUp,default_Right,default_RightDown,default_Down,~
default_LeftDown,default_Left,information,default_SidebarCollapse,default_SidebarExpand".
    
    IF iphFrame:SENSITIVE EQ NO THEN RETURN.
    
    cCueCardPool = "CueCardPool" + STRING(TIME,"99999").
    DELETE WIDGET-POOL cCueCardPool NO-ERROR.
    CREATE WIDGET-POOL cCueCardPool NO-ERROR.

    /* process various types of cue cards */
    DO idx = 1 TO NUM-ENTRIES(cCueType):
        /* check for active cue card */
        IF CAN-FIND(FIRST cueCard
                    WHERE cueCard.cuePrgmName EQ ipcPrgmName
                      AND cueCard.cueType     EQ ENTRY(idx,cCueType)
                      AND cueCard.isActive    EQ YES) THEN 
        FOR EACH cueCard NO-LOCK
            WHERE cueCard.cuePrgmName EQ ipcPrgmName
              AND cueCard.cueType     EQ ENTRY(idx,cCueType)
              AND cueCard.isActive    EQ YES
            :
            ASSIGN
                iCueOrder = 1
                dTitle    = IF iphFrame:TITLE EQ ? THEN 0 ELSE 1
                lNext     = YES
                .
            /* check to be sure there are active cue card texts */
            IF CAN-FIND(FIRST cueCardText
                        WHERE cueCardText.cueID    EQ cueCard.cueID
                          AND cueCardText.cueType  EQ cueCard.cueType
                          AND cueCardText.isActive EQ YES) THEN
            DO WHILE TRUE:
                /* done, no more cue card texts */
                IF iCueOrder EQ ? THEN LEAVE.
                FIND FIRST cueCardText NO-LOCK
                     WHERE cueCardText.cueID    EQ cueCard.cueID
                       AND cueCardText.cueType  EQ cueCard.cueType
                       AND cueCardText.cueOrder EQ iCueOrder
                     NO-ERROR.
                /* if can't find, done, no more cue card texts */
                IF NOT AVAILABLE cueCardText THEN LEAVE.
                /* cue card not active, get next card in sequence */
                /* or check if user has blocked this cue card text */
                IF NOT cueCardText.isActive OR
                  (iplActive EQ YES AND 
                   CAN-FIND(FIRST xCueCard
                            WHERE xCueCard.user_id   EQ USERID("ASI")
                              AND xCueCard.cueType   EQ cueCardText.cueType
                              AND xCueCard.cueTextID EQ cueCardText.cueTextID)) THEN DO:
                    IF lNext THEN
                    RUN spNextCue (hMainMenuHandle).
                    ELSE
                    RUN spPrevCue (hMainMenuHandle).
                    NEXT.
                END.
                /* check security level of user vs cue card security */
                IF sfUserSecurityLevel() LT cueCard.securityLevel THEN
                DO TRANSACTION:
                    CREATE xCueCard.
                    ASSIGN
                        xCueCard.user_id   = USERID("ASI")
                        xCueCard.cueType   = cueCard.cueType
                        xCueCard.cueTextID = cueCardText.cueTextID
                        .
                    NEXT.
                END. /* if securitylevel */
                /* calculate the cue card screen position */
                CASE cueCardText.cuePosition:
                    /* arrows: 1=absolute */
                    WHEN 1 THEN
                    ASSIGN 
                        dCol = cueCardText.frameCol
                        dRow = cueCardText.frameRow - dTitle
                        .
                    /* arrows: 2=width */
                    WHEN 2 THEN 
                    ASSIGN 
                        dCol = iphFrame:WIDTH - cueCardText.frameCol
                        dRow = cueCardText.frameRow - dTitle
                        .
                    /* arrows: 3=height & width */
                    WHEN 3 THEN 
                    ASSIGN 
                        dCol = iphFrame:WIDTH  - cueCardText.frameCol
                        dRow = iphFrame:HEIGHT - cueCardText.frameRow - dTitle
                        .
                    /* arrows: 4=height */
                    WHEN 4 THEN  
                    ASSIGN 
                        dCol = cueCardText.frameCol
                        dRow = iphFrame:HEIGHT - cueCardText.frameRow - dTitle
                        .
                END CASE.
                IF dRow LT 1 THEN dRow = 1.
                /* create cue card objects */
                /* FRAME */
                CREATE FRAME hCueCardFrame IN WIDGET-POOL cCueCardPool
                    ASSIGN 
                        PARENT = iphContainer
                        FRAME = iphFrame
                        NAME = "CueCardFrame"
                        COL = dCol
                        ROW = dRow
                        HEIGHT = cueCardText.frameHeight
                        WIDTH = cueCardText.frameWidth
                        BOX = NO 
                        BGCOLOR = cueCardText.frameBGColor
                        FGCOLOR = cueCardText.frameFGColor
                        HIDDEN = NO 
                        SENSITIVE = YES
                  TRIGGERS:
                    ON ENTRY
                      PERSISTENT RUN spCueCardFrame IN THIS-PROCEDURE (hCueCardFrame).
                  END TRIGGERS.
                hCueCardFrame:MOVE-TO-TOP().
                /* RECTANGLE */
                CREATE RECTANGLE hCueCardRectangle IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "CueCardRect"
                        COL = 1
                        ROW = 1
                        HEIGHT = hCueCardFrame:HEIGHT - .1
                        WIDTH = hCueCardFrame:WIDTH - .3
                        ROUNDED = YES
                        GRAPHIC-EDGE = YES
                        EDGE-PIXELS = 1
                        SENSITIVE = NO
                        HIDDEN = NO 
                        .
                hCueCardRectangle:MOVE-TO-TOP().
                /* CLOSE BUTTON */
                CREATE BUTTON hClose IN WIDGET-POOL cCueCardPool
                    ASSIGN 
                        FRAME = hCueCardFrame
                        NAME = "btnClose"
                        COL = hCueCardFrame:WIDTH - 4.4 
                        ROW = 1.24
                        TOOLTIP = "Close Cue Card"
                        FLAT-BUTTON = YES
                        HEIGHT = 1
                        WIDTH = 4.2
                        HIDDEN = NO 
                        SENSITIVE = YES
                  TRIGGERS:
                    ON CHOOSE
                      PERSISTENT RUN spCueCardClose IN THIS-PROCEDURE (hMainMenuHandle).
                  END TRIGGERS.
                hClose:LOAD-IMAGE("Graphics\16x16\delete.gif").
                hClose:MOVE-TO-TOP().
                /* ARROW IMAGE */
                CREATE IMAGE hCueCardArrow IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "ArrowImage"
                        COL = cueCardText.arrowCol
                        ROW = cueCardText.arrowRow
                        SENSITIVE = NO
                        HIDDEN = NO 
                        WIDTH = 7
                        HEIGHT = 1.67
                        TRANSPARENT = YES
                        .
                hCueCardArrow:LOAD-IMAGE("Graphics\24x24\" + ENTRY(cueCardText.cueOrientation,cOrientation) + ".gif").
                hCueCardArrow:MOVE-TO-TOP().
                /* PREVIOUS BUTTON */
                CREATE BUTTON hCueCardPrev IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "btnPrev"
                        COL = cueCardText.prevCol
                        ROW = cueCardText.prevRow
                        TOOLTIP = "Show Previous Cue Card"
                        FLAT-BUTTON = YES
                        SENSITIVE = YES 
                        HIDDEN = NO 
                        WIDTH = 5
                        HEIGHT = 1.19
                  TRIGGERS:
                    ON CHOOSE
                      PERSISTENT RUN spPrevCue IN THIS-PROCEDURE (hMainMenuHandle).
                  END TRIGGERS.
                hCueCardPrev:LOAD-IMAGE("Graphics\24x24\" + ENTRY(10,cOrientation) + ".gif").
                hCueCardPrev:MOVE-TO-TOP().
                /* NEXT BUTTON */
                CREATE BUTTON hCueCardNext IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "btnNext"
                        COL = cueCardText.nextCol
                        ROW = cueCardText.nextRow
                        TOOLTIP = "Show Next Cue Card"
                        FLAT-BUTTON = YES
                        SENSITIVE = YES 
                        HIDDEN = NO 
                        WIDTH = 5
                        HEIGHT = 1.19
                  TRIGGERS:
                    ON CHOOSE
                      PERSISTENT RUN spNextCue IN THIS-PROCEDURE (hMainMenuHandle).
                  END TRIGGERS.
                hCueCardNext:LOAD-IMAGE("Graphics\24x24\" + ENTRY(11,cOrientation) + ".gif").
                hCueCardNext:MOVE-TO-TOP().
                /* EDITOR */
                CREATE EDITOR hCueCardText IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "CueCardText"
                        FONT = cueCardText.textFont
                        COL = cueCardText.textCol
                        ROW = cueCardText.textRow
                        WIDTH = cueCardText.textWidth
                        HEIGHT = cueCardText.textHeight
                        BGCOLOR = cueCardText.frameBGColor
                        FGCOLOR = cueCardText.frameFGColor
                        SCROLLBAR-HORIZONTAL = NO
                        SCROLLBAR-VERTICAL = NO
                        WORD-WRAP = YES
                        READ-ONLY = YES
                        BOX = NO
                        SCREEN-VALUE = cueCardText.cueText
                        SENSITIVE = YES
                        HIDDEN = NO
                        .
                hCueCardText:MOVE-TO-TOP().
                /* TOGGLE BOX DON'T SHOW AGAIN */
                CREATE TOGGLE-BOX hDontShowAgain IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "DontShowAgain"
                        LABEL = "Don't Show Again"
                        FONT = cueCardText.dontShowAgainFont
                        COL = cueCardText.dontShowAgainCol
                        ROW = cueCardText.dontShowAgainRow
                        SENSITIVE = YES
                        HIDDEN = NO 
                        WIDTH = 24
                        HEIGHT = .81
                        SCREEN-VALUE = "NO"
                  TRIGGERS:
                    ON VALUE-CHANGED
                      PERSISTENT RUN spSetDontShowAgain IN THIS-PROCEDURE (hMainMenuHandle).
                  END TRIGGERS.
                hDontShowAgain:MOVE-TO-TOP().
                ASSIGN
                    hDontShowAgain:SENSITIVE = iplActive
                    hDontShowAgain:HIDDEN    = NOT cueCard.enableDontShowAgain
                    .
                /* TOGGLE BOX DISMISS */
                CREATE TOGGLE-BOX hDismiss IN WIDGET-POOL cCueCardPool
                    ASSIGN
                        FRAME = hCueCardFrame
                        NAME = "Dismiss"
                        LABEL = "Dismiss This Cue Card Set"
                        FONT = cueCardText.dismissFont
                        COL = cueCardText.dismissCol
                        ROW = cueCardText.dismissRow
                        SENSITIVE = YES
                        HIDDEN = NO 
                        WIDTH = 34
                        HEIGHT = .81
                        SCREEN-VALUE = "NO"
                  TRIGGERS:
                    ON VALUE-CHANGED
                      PERSISTENT RUN spSetDismiss IN THIS-PROCEDURE (hMainMenuHandle).
                  END TRIGGERS.
                hDismiss:MOVE-TO-TOP().
                ASSIGN 
                    hDismiss:SENSITIVE = iplActive
                    hDismiss:HIDDEN    = NOT cueCard.enableDismiss
                    lCueCardActive     = YES
                    .            
                WAIT-FOR "U1":U OF hMainMenuHandle.
                lCueCardActive = NO.            
                DELETE OBJECT hCueCardFrame.
            END. /* do while */
        END. /* each cuecard */
    END. /* do idx */
    DELETE WIDGET-POOL cCueCardPool NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCueCardActive) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCueCardActive Procedure
FUNCTION fCueCardActive RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN lCueCardActive.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-sfClearTtSysCtrlUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfClearTtSysCtrlUsage Procedure 
FUNCTION sfClearTtSysCtrlUsage RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        EMPTY TEMP-TABLE ttSysCtrlUsage.

        RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetMainMenuHandle Procedure 
FUNCTION sfGetMainMenuHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN hMainMenuHandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetNextRecKey) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetNextRecKey Procedure 
FUNCTION sfGetNextRecKey RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN STRING(YEAR(TODAY),"9999")
         + STRING(MONTH(TODAY),"99")
         + STRING(DAY(TODAY),"99")
         + STRING(TIME,"99999")
         + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
         .
         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetSysCtrlUsageHandle Procedure 
FUNCTION sfGetSysCtrlUsageHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        RETURN hSysCtrlUsageHandle.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfGetTtSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfGetTtSysCtrlUsageHandle Procedure 
FUNCTION sfGetTtSysCtrlUsageHandle RETURNS HANDLE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        RETURN TEMP-TABLE ttSysCtrlUsage:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetMainMenuHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfSetMainMenuHandle Procedure 
FUNCTION sfSetMainMenuHandle RETURNS LOGICAL
  (iphMainMenuHandle AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    hMainMenuHandle = iphMainMenuHandle.
    
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfSetSysCtrlUsageHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfSetSysCtrlUsageHandle Procedure 
FUNCTION sfSetSysCtrlUsageHandle RETURNS LOGICAL
  (iphSysCtrlUsageHandle AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    hSysCtrlUsageHandle = iphSysCtrlUsageHandle.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sfUserSecurityLevel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sfUserSecurityLevel Procedure 
FUNCTION sfUserSecurityLevel RETURNS INTEGER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN IF AVAILABLE users THEN users.securityLevel ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

