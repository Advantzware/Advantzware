&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
File: mxptrace.w
Description: RUN/INCLUDE tracer drill-down utility
Input Parameters: <none>
Output Parameters: <none>
History: Original (beta) version - Apr 6, 2011 - MYT
  
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 2011 Foresight Software LLC.  All Rights               *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR iLastLine AS INT.
DEF VAR iIndent AS INT.
DEF VAR iLevel AS INT.
DEF VAR lDupes AS LOG.
DEF VAR iLimit AS INT.
DEF VAR lAbort AS LOG.
DEF STREAM rpt1.
OUTPUT STREAM rpt1 CLOSE.

DEFINE TEMP-TABLE eLine
    FIELD lineno AS INT
    FIELD parentpgm AS CHAR
    FIELD pgmname AS CHAR
    FIELD pgmline AS INT
    FIELD pgmtype AS CHAR
    FIELD tabpos AS INT.

DEFINE TEMP-TABLE donepgms
    FIELD pgmname AS CHAR.

DEFINE TEMP-TABLE perprocs
    FIELD chandle AS CHAR
    FIELD cpgmname AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tHandles fiProgram rsLevel fiOffset tbRun ~
tbIncludes tDupes bStart bAbort bPrint bExit eResult 
&Scoped-Define DISPLAYED-OBJECTS tHandles fiProgram rsLevel fiOffset tbRun ~
tbIncludes tDupes eResult fiWhat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bAbort 
     LABEL "Abort" 
     SIZE 15 BY 1.08.

DEFINE BUTTON bExit AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.08.

DEFINE BUTTON bPrint 
     LABEL "Print" 
     SIZE 15 BY 1.08.

DEFINE BUTTON bStart 
     LABEL "Start" 
     SIZE 15 BY 1.08.

DEFINE VARIABLE eResult AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 141 BY 22.75
     FONT 2 NO-UNDO.

DEFINE VARIABLE fiLevels AS INTEGER FORMAT "99":U INITIAL 9 
     LABEL "Levels" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fiOffset AS INTEGER FORMAT "9":U INITIAL 3 
     LABEL "Offset" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE fiProgram AS CHARACTER FORMAT "X(256)":U 
     LABEL "Starting Program" 
     VIEW-AS FILL-IN 
     SIZE 86 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 142 BY 1 NO-UNDO.

DEFINE VARIABLE rsLevel AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Single Level", "Single",
"Multi-Level", "Multi"
     SIZE 33 BY 1.58 NO-UNDO.

DEFINE VARIABLE tbIncludes AS LOGICAL INITIAL yes 
     LABEL "Includes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .92 NO-UNDO.

DEFINE VARIABLE tbRun AS LOGICAL INITIAL yes 
     LABEL "RUN stmts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .92 NO-UNDO.

DEFINE VARIABLE tDupes AS LOGICAL INITIAL no 
     LABEL "Dupes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .92 NO-UNDO.

DEFINE VARIABLE tHandles AS LOGICAL INITIAL yes 
     LABEL "Handles?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     tHandles AT ROW 3.67 COL 112
     fiProgram AT ROW 1.71 COL 19 COLON-ALIGNED
     rsLevel AT ROW 3.38 COL 4 NO-LABEL
     fiLevels AT ROW 3.67 COL 43 COLON-ALIGNED
     fiOffset AT ROW 3.67 COL 58 COLON-ALIGNED
     tbRun AT ROW 3.67 COL 67
     tbIncludes AT ROW 3.67 COL 84
     tDupes AT ROW 3.67 COL 99
     bStart AT ROW 1.5 COL 129
     bAbort AT ROW 2.88 COL 129
     bPrint AT ROW 4.13 COL 129
     bExit AT ROW 5.5 COL 129
     eResult AT ROW 6.96 COL 4 NO-LABEL
     fiWhat AT ROW 29.96 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     " Line     Parent               CallType     Line No      Called Proc" VIEW-AS TEXT
          SIZE 120 BY .67 AT ROW 6 COL 4
          FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 147.6 BY 29.95
         CANCEL-BUTTON bExit.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Run Statement Tracer"
         HEIGHT             = 29.96
         WIDTH              = 147.57
         MAX-HEIGHT         = 29.96
         MAX-WIDTH          = 152.57
         VIRTUAL-HEIGHT     = 29.96
         VIRTUAL-WIDTH      = 152.57
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   Custom                                                               */
/* SETTINGS FOR FILL-IN fiLevels IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiLevels:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN fiWhat IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Run Statement Tracer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Run Statement Tracer */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bAbort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bAbort wWin
ON CHOOSE OF bAbort IN FRAME fMain /* Abort */
DO:
    ASSIGN 
        lAbort = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExit wWin
ON CHOOSE OF bExit IN FRAME fMain /* Exit */
DO:
    APPLY 'window-close' TO wWin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bPrint wWin
ON CHOOSE OF bPrint IN FRAME fMain /* Print */
DO:
    DEF VAR ipt AS INT.
    DEF VAR lContinue AS LOG.

    SYSTEM-DIALOG PRINTER-SETUP
        UPDATE lContinue.
    IF NOT lContinue THEN RETURN.
    
    OUTPUT STREAM rpt1 TO PRINTER PAGE-SIZE 78.
    DO ipt = 1 TO LENGTH(eResult:SCREEN-VALUE):
        IF SUBSTRING(eResult:SCREEN-VALUE,ipt,1) = chr(10) THEN do:
            IF SUBSTRING(eResult:SCREEN-VALUE,ipt + 1,1) = chr(10) THEN DO:
                    PUT STREAM rpt1 SKIP(1).
                    NEXT.
            END.
            ELSE PUT STREAM rpt1 SKIP.
        END.
        ELSE PUT STREAM rpt1 UNFORMATTED SUBSTRING(eResult:SCREEN-VALUE,ipt,1).
    END.
    OUTPUT STREAM rpt1 CLOSE.
    /*
    DO ipt = 1 TO LENGTH(eResult:SCREEN-VALUE):
        MESSAGE 
            SUBSTRING(eResult:SCREEN-VALUE,ipt,1) SKIP
            ASC(SUBSTRING(eResult:SCREEN-VALUE,ipt,1))
            VIEW-AS ALERT-BOX.
    END.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bStart wWin
ON CHOOSE OF bStart IN FRAME fMain /* Start */
DO:
    EMPTY TEMP-TABLE eLine.
    EMPTY TEMP-TABLE donepgms.
    EMPTY TEMP-TABLE perprocs.

    ASSIGN
        eResult:screen-value IN FRAME fMain = ""
        iLastLine = 0
        iIndent = 0.
        lAbort = FALSE.

    RUN ipPSD IN THIS-PROCEDURE (INPUT fiProgram:SCREEN-VALUE).

    ASSIGN
        fiWhat:SCREEN-VALUE = "Converting to text...".
    RUN ipDisplayResult IN THIS-PROCEDURE.
    ASSIGN
        fiWhat:SCREEN-VALUE = "Done.".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevels
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevels wWin
ON LEAVE OF fiLevels IN FRAME fMain /* Levels */
DO:
    ASSIGN
        iLimit = INTEGER(SELF:SCREEN-VALUE).
    IF iLimit > 9 THEN MESSAGE
        "A drill down of more than 9 levels may incur" SKIP
        "sigificant processing time and could possibly" SKIP
        "exceed your system resources.  Please use with" SKIP
        "extreme caution."
        VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsLevel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsLevel wWin
ON VALUE-CHANGED OF rsLevel IN FRAME fMain
DO:
    IF SELF:SCREEN-VALUE = "Single" THEN ASSIGN
        fiLevels:VISIBLE = FALSE
        fiLevels:SENSITIVE = FALSE
        fiLevels:SCREEN-VALUE = STRING(iLimit).
    ELSE ASSIGN
        fiLevels:VISIBLE = TRUE
        fiLevels:SENSITIVE = TRUE
        fiLevels:SCREEN-VALUE = STRING(iLimit).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tDupes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tDupes wWin
ON VALUE-CHANGED OF tDupes IN FRAME fMain /* Dupes? */
DO:
    ASSIGN lDupes = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY tHandles fiProgram rsLevel fiOffset tbRun tbIncludes tDupes eResult 
          fiWhat 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE tHandles fiProgram rsLevel fiOffset tbRun tbIncludes tDupes bStart 
         bAbort bPrint bExit eResult 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR ii AS INT.
  /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN 
        iLimit = 9
        fiLevels = iLimit.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    /*
    ASSIGN ii = 1.
    FOR EACH z_funct WHERE z_funct.funct-name <> "":
        cbFunction:ADD-LAST(z_funct.funct-name) IN FRAME fMain.
        IF ii = 1 THEN ASSIGN
            cbFunction:SCREEN-VALUE = z_funct.funct-name.
        ASSIGN 
            ii = ii + 1.
    END.
    APPLY 'value-changed' TO cbFunction.
    */
    APPLY 'entry' TO fiProgram IN FRAME fMain.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDisplayResult wWin 
PROCEDURE ipDisplayResult :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cDisplay AS CHAR FORMAT "x(200)".
    DEF VAR cParent AS CHAR FORMAT "x(20)".

    IF tHandles:CHECKED IN FRAME fMain THEN DO:
        ASSIGN cDisplay = 
            "Handle Variable                         Run Program" + CHR(10).
        eResult:INSERT-STRING(cDisplay) IN FRAME fMain.
        ASSIGN cDisplay = 
            FILL("-",80) + CHR(10).
        eResult:INSERT-STRING(cDisplay) IN FRAME fMain.

        FOR EACH perprocs:
            ASSIGN
                cDisplay = FILL(" ",80)
                SUBSTRING(cDisplay,1,LENGTH(perprocs.chandle)) = perprocs.chandle
                SUBSTRING(cDisplay,41,LENGTH(perprocs.cpgmname)) = perprocs.cpgmname
                cDisplay = cDisplay + CHR(10).
            eResult:INSERT-STRING(cDisplay) IN FRAME fMain.
        END.

        eResult:INSERT-STRING(CHR(10)).
        eResult:INSERT-STRING(CHR(10)).
    END.
    
    eResult:INSERT-STRING("Line     Parent                CallType     Line No      Called Proc" + CHR(10)).
    ASSIGN cDisplay = 
        FILL("-",80) + CHR(10).
    eResult:INSERT-STRING(cDisplay) IN FRAME fMain.
    
    FOR EACH eLine:
        IF eLine.pgmtype = "FUNCTION" 
        OR (eLine.parentpgm = "" AND eline.pgmtype <> "RUNPGM__") THEN 
            eResult:INSERT-STRING(CHR(10)).

        ASSIGN
            cParent = eLine.parentpgm + "                    "
            cParent = SUBSTRING(cParent,1,20)
            cDisplay = 
                STRING(eLine.lineno,"999999") + "   " +
                cParent + "  " + 
                STRING(eLine.pgmtype,"x(10)") +
                FILL(" ",eLine.tabpos) +
                STRING(eLine.pgmline,"99999") + "     " +
                eLine.pgmname +
                CHR(10).

        eResult:INSERT-STRING(cDisplay) IN FRAME fMain.
    END.

    eResult:CURSOR-OFFSET = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipPSD wWin 
PROCEDURE ipPSD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcPgmName AS CHAR.
    DEF VAR chLine AS CHAR NO-UNDO.
    DEF VAR iPgmLine AS INT.
    DEF VAR chPgm AS CHAR NO-UNDO.
    DEF VAR chFull AS CHAR NO-UNDO.
    DEF VAR iSeek AS INT NO-UNDO.
    DEF VAR lConstruct AS LOG.
    DEF VAR cInWhat AS CHAR.
    DEF VAR lDontDrill AS log.

    PROCESS EVENTS.
    IF lAbort THEN do:
        RETURN.
    END.

    IF SEARCH(ipcPgmName) = ? THEN DO: 
        MESSAGE
            "ERROR in ipPSD: Program not found: " + ipcPgmName
            VIEW-AS ALERT-BOX.
        RETURN.
    END.

    ASSIGN chFull = ipcPgmName.

    INPUT FROM value(SEARCH(chFull)).
    REPEAT:
        IMPORT UNFORMATTED chLine.
        ASSIGN
            lDontDrill = FALSE
            iPgmLine = iPgmLine + 1.
            fiWhat:SCREEN-VALUE IN FRAME fMain = "Processing program: " + chFull.

        /* Handle RUN statements */
        IF tbRun:CHECKED
        AND INDEX(chLine,"RUN ") <> 0 
        AND SUBSTRING(chLine,1,28) <> "       RUN constructObject (" THEN DO: 
            IF index(chLine,"run ") > 1
            AND substring(chLine,INDEX(chLine,"RUN ") - 1, 1) <> " " THEN next.
            ASSIGN
                chPgm = SUBSTRING(chLine,INDEX(chLine,"RUN ") + 4).
            IF INDEX(chPgm," ") <> 0 THEN ASSIGN
                chPgm = SUBSTRING(chPgm,1,INDEX(chPgm," ")).
            ASSIGN
                chPgm = TRIM(chPgm)
                chPgm = TRIM(chPgm,".")
                chPgm = REPLACE(chPgm,'VALUE("',"")
                chPgm = REPLACE(chPgm,'":U)',"")
                chPgm = REPLACE(chPgm,'")',"")
                chPgm = REPLACE(chPgm,'"',"")
                chPgm = TRIM(chPgm).
            IF INDEX(chPgm,"(") <> 0 THEN ASSIGN
                chPgm = SUBSTRING(chPgm,1,INDEX(chPgm,"(") - 1).
                                  
            IF CAN-DO('persistent,persistent.:U,super,pt/mess.p,MessageOnly,constructobject,different,of,gui,pt/',chPgm) THEN.
            ELSE DO:
                IF INDEX(chPgm,",") <> 0 THEN NEXT.
                CREATE eLine.
                ASSIGN
                    iLastLine = iLastLine + 1
                    eLine.lineno = iLastLine
                    eLine.parentpgm = chFull
                    eLine.pgmname = chPgm
                    eLine.pgmline = iPgmLine
                    eLine.pgmtype = IF INDEX(chPgm,"/") <> 0 THEN "RUNSTMT" ELSE "PROCRUN"
                    eLine.tabpos = iIndent + INTEGER(fiOffset:SCREEN-VALUE IN FRAME fMain).
                FIND FIRST donepgms WHERE
                    donepgms.pgmname = chPgm
                    NO-LOCK NO-ERROR.
                IF AVAIL donepgms THEN ASSIGN
                    SUBSTRING(eLine.pgmtype,8,1) = "*". 

                IF SUBSTRING(eLine.pgmtype,1,4) = "PROC" 
                AND INDEX(chLine," IN ") <> 0 THEN DO:
                    ASSIGN 
                        cInWhat = SUBSTRING(chLine,INDEX(chLine," IN ")).
                    IF INDEX(cInWhat,"(") <> 0 THEN ASSIGN
                        cInWhat = SUBSTRING(cInWhat,1,INDEX(cInWhat,"("))
                        cInWhat = TRIM(cInWhat,"(")
                        cInWhat = TRIM(cInWhat," ")
                        cInWhat = TRIM(cInWhat,"(")
                        cInWhat = TRIM(cInWhat," ")
                        cInWhat = TRIM(cInWhat,".").
                    ASSIGN
                        eLine.pgmname = chPgm + " " + cInWhat
                        eLine.pgmname = REPLACE(eline.pgmname,"  "," ").

                    FIND FIRST perprocs WHERE
                        perprocs.chandle = SUBSTRING(cInWhat,4)
                        NO-LOCK NO-ERROR.
                    IF AVAIL perprocs THEN DO:
                    END.
                        
                END.

                IF INDEX(chLine," persistent set ") <> 0 THEN DO:
                    ASSIGN
                        /* lDontDrill = TRUE */
                        cInWhat = SUBSTRING(chLine,INDEX(chLine," SET ") + 5)
                        cInWhat = TRIM(cInWhat,".")
                        cInWhat = TRIM(cInWhat).
                    FIND FIRST perprocs WHERE
                        perprocs.chandle = cInWhat
                        NO-LOCK NO-ERROR.
                    IF NOT AVAIL perprocs THEN CREATE perprocs.
                    ASSIGN
                        perprocs.chandle = cInWhat
                        perprocs.cpgmname = chPgm.
                    ASSIGN
                        cInWhat = "(SET " + cInWhat + ")"
                        eLine.pgmname = chPgm + " " + cInWhat
                        eLine.pgmname = REPLACE(eline.pgmname,"  "," ").
                END.

                IF SEARCH(chPgm) <> ? 
                AND NOT lDontDrill THEN DO:
                    ASSIGN
                        iLevel = iLevel + 1.
                    /* If single level stop here */
                    IF rsLevel:SCREEN-VALUE = "Single"
                    AND iLevel > 0 THEN.
                    /* Respect the multi-level limit */
                    ELSE IF rsLevel:SCREEN-VALUE = "Multi" 
                    AND iLevel + 1 > iLimit THEN.
                    /* If already processed and no dupes, stop here */
                    ELSE IF CAN-FIND(FIRST donepgms WHERE
                                 donepgms.pgmname = chPgm)
                    AND NOT lDupes THEN.
                    /* If lower than 8 levels, stop anyway */
                    ELSE IF CAN-FIND(FIRST donepgms WHERE
                                 donepgms.pgmname = chPgm)
                    AND iLevel > 9 THEN.
                    ELSE IF INDEX(chPgm,"/adm") <> 0 THEN.
                    ELSE DO:
                        ASSIGN
                            iSeek = SEEK(INPUT)
                            iIndent = iIndent + INTEGER(fiOffset:SCREEN-VALUE).
                
                        CREATE donepgms.
                        ASSIGN
                            donepgms.pgmname = chPgm.
                
                        INPUT CLOSE.
                        RUN ipPSD IN THIS-PROCEDURE (INPUT chPgm).
                        ASSIGN
                            iIndent = iIndent - INTEGER(fiOffset:SCREEN-VALUE).
                        INPUT FROM value(SEARCH(chFull)).
                        SEEK INPUT TO iSeek.
                    END.
                    ASSIGN
                        iLevel = iLevel - 1.
                END.
            END.
        END.  /* RUN statements */

        /* Handle ADM constructors, part 1 */
        ELSE IF tbRun:CHECKED 
        AND SUBSTRING(chLine,1,28) = "       RUN constructObject (" THEN DO: 
            ASSIGN 
                lConstruct = true.
            NEXT.
        END.

        /* Handle includes */
        ELSE IF tbIncludes:CHECKED 
        AND INDEX(chLine,"~{") <> 0
        AND INDEX(chLine,"~}") <> 0 THEN DO:
            ASSIGN
                chPgm = SUBSTRING(chLine,INDEX(chLine,"~{"))
                chPgm = SUBSTRING(chPgm,1,INDEX(chPgm,"~}"))
                chPgm = REPLACE(chPgm,'"',"")
                chPgm = TRIM(chPgm,"~{")
                chPgm = TRIM(chPgm,"~}").
            IF INDEX(chPgm," ") <> 0 THEN ASSIGN
                cInWhat = SUBSTRING(chPgm,INDEX(chPgm," "))
                chPgm = SUBSTRING(chPgm,1,INDEX(chPgm," "))
                chPgm = TRIM(chPgm)
                cInWhat = TRIM(cInWhat,"~}")
                cInWhat = TRIM(cInWhat).
            IF INDEX(chPgm,"&") <> 0 THEN NEXT.
            IF INDEX(chPgm,"/") = 0
            AND INDEX(chPgm,"\") = 0 THEN NEXT.

            CREATE eLine.
            ASSIGN
                iLastLine = iLastLine + 1
                eLine.lineno = iLastLine
                eLine.parentpgm = chFull
                eLine.pgmname = chPgm + IF cInWhat <> "" THEN " (" + cInWhat + ")" ELSE ""
                eLine.pgmline = iPgmLine
                eLine.pgmtype = "INCLUDE"
                eLine.tabpos = iIndent + INTEGER(fiOffset:SCREEN-VALUE IN FRAME fMain).
            FIND FIRST donepgms WHERE
                donepgms.pgmname = chPgm
                NO-LOCK NO-ERROR.
            IF AVAIL donepgms THEN ASSIGN
                SUBSTRING(eLine.pgmtype,8,1) = "*". 
            
            IF SEARCH(chPgm) <> ? THEN DO:
                ASSIGN
                    iLevel = iLevel + 1.
                /* If single level stop here */
                IF rsLevel:SCREEN-VALUE = "Single"
                AND iLevel > 0 THEN.
                /* Respect the multi-level limit */
                ELSE IF rsLevel:SCREEN-VALUE = "Multi" 
                AND iLevel + 1 > iLimit THEN.
                /* If already processed and no dupes, stop here */
                ELSE IF CAN-FIND(FIRST donepgms WHERE
                             donepgms.pgmname = chPgm)
                AND NOT lDupes THEN.
                /* If lower than 8 levels, stop anyway */
                ELSE IF CAN-FIND(FIRST donepgms WHERE
                             donepgms.pgmname = chPgm)
                AND iLevel > 9 THEN.
                ELSE IF INDEX(chPgm,"/adm") <> 0 THEN.
                ELSE DO:
                    ASSIGN
                        iSeek = SEEK(INPUT)
                        iIndent = iIndent + INTEGER(fiOffset:SCREEN-VALUE).

                    CREATE donepgms.
                    ASSIGN
                        donepgms.pgmname = chPgm.

                    INPUT CLOSE.
                    RUN ipPSD IN THIS-PROCEDURE (INPUT chPgm).
                    ASSIGN
                        iIndent = iIndent - INTEGER(fiOffset:SCREEN-VALUE).
                    INPUT FROM value(SEARCH(chFull)).
                    SEEK INPUT TO iSeek.
                END.
                ASSIGN
                    iLevel = iLevel - 1.
            END.
        END. /* Includes */
        
        /* Handle ADM constructors, part 2 */
        IF lConstruct THEN DO:
            ASSIGN
                lConstruct = FALSE
                chPgm = TRIM(chLine)
                chPgm = SUBSTRING(chPgm,9)
                chPgm = SUBSTRING(chPgm,1,INDEX(chPgm,".") + 1)
                chPgm = REPLACE(chPgm,"':U ,","").
            IF chPgm <> "pt/mess.p" 
            AND chPgm <> "MessageOnly" 
            AND chPgm <> "SUPER" THEN DO:
                CREATE eLine.
                ASSIGN
                    iLastLine = iLastLine + 1
                    eLine.lineno = iLastLine
                    eLine.pgmname = chPgm
                    eLine.parentpgm = chFull
                    eLine.pgmline = iPgmLine
                    eLine.pgmtype = IF INDEX(chPgm,"/") <> 0 THEN "RUNSTMT" ELSE "PROCRUN"
                    eLine.tabpos = iIndent + INTEGER(fiOffset:SCREEN-VALUE IN FRAME fMain).
                FIND FIRST donepgms WHERE
                    donepgms.pgmname = chPgm
                    NO-LOCK NO-ERROR.
                IF AVAIL donepgms THEN ASSIGN
                    SUBSTRING(eLine.pgmtype,8,1) = "*". 
                
                IMPORT UNFORMATTED chLine.
                IMPORT UNFORMATTED chLine.
                IMPORT UNFORMATTED chLine.
                
                IF INDEX(chLine,"OUTPUT") <> 0 THEN DO:
                    ASSIGN
                        cInWhat = ""
                        cInWhat = SUBSTRING(chLine,INDEX(chLine,"OUTPUT") + 7)
                        cInWhat = SUBSTRING(cInWhat,1,LENGTH(cInWhat) - 3).
                    FIND FIRST perprocs WHERE
                        perprocs.chandle = cInWhat
                        NO-LOCK NO-ERROR.
                    IF NOT AVAIL perprocs THEN CREATE perprocs.
                    ASSIGN
                        perprocs.chandle = cInWhat
                        perprocs.cpgmname = chPgm.
                    ASSIGN
                        cInWhat = "(SET " + cInWhat + ")"
                        eLine.pgmname = chPgm + " " + cInWhat
                        eLine.pgmname = REPLACE(eline.pgmname,"  "," ").
                END.

                IF SEARCH(chPgm) <> ? THEN DO:
                    ASSIGN
                        iLevel = iLevel + 1.
                    /* If single level stop here */
                    IF rsLevel:SCREEN-VALUE = "Single"
                    AND iLevel > 0 THEN.
                    /* Respect the multi-level limit */
                    ELSE IF rsLevel:SCREEN-VALUE = "Multi" 
                    AND iLevel + 1 > iLimit THEN.
                    /* If already processed and no dupes, stop here */
                    ELSE IF CAN-FIND(FIRST donepgms WHERE
                                 donepgms.pgmname = chPgm)
                    AND NOT lDupes THEN.
                    /* If lower than 8 levels, stop anyway */
                    ELSE IF CAN-FIND(FIRST donepgms WHERE
                                 donepgms.pgmname = chPgm)
                    AND iLevel > 9 THEN.
                    ELSE DO:
                        ASSIGN
                            iSeek = SEEK(INPUT)
                            iIndent = iIndent + INTEGER(fiOffset:SCREEN-VALUE).
                
                        CREATE donepgms.
                        ASSIGN
                            donepgms.pgmname = chPgm.
                
                        INPUT CLOSE.
                        RUN ipPSD IN THIS-PROCEDURE (INPUT chPgm).
                        ASSIGN
                            iIndent = iIndent - INTEGER(fiOffset:SCREEN-VALUE).
                        INPUT FROM value(SEARCH(chFull)).
                        SEEK INPUT TO iSeek.
                    END.
                    ASSIGN
                        iLevel = iLevel - 1.
                END.
            END.
        END.

    END.  /* REPEAT */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

