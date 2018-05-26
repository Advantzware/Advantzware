&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: schemaCheck.w

  Description: Verify that schema changes have been performed on a 
               customer's database while upgrading to a new release

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Wade Kaldawi

  Created: 12.5.2016

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

DEFINE TEMP-TABLE ttDeltaList
    FIELD allFileName  AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Table"
    FIELD awVersion    AS CHARACTER FORMAT "x(12)" COLUMN-LABEL "Version"
    FIELD dbTable      AS CHARACTER COLUMN-LABEL "Lock Type"
    FIELD dbField      AS CHARACTER COLUMN-LABEL "Flags"
    FIELD objectType   AS CHARACTER COLUMN-LABEL "Type (e.g. field or sequence)"
    FIELD shouldExist  AS LOG 
    FIELD databaseName AS CHARACTER 
    FIELD passesTest   AS LOGICAL 
  .   
    
DEFINE TEMP-TABLE ttfiles 
    FIELD ttfullname AS CHARACTER  
    FIELD ttfilename AS CHARACTER   .  
  
ASSIGN
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE labelLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE dataLine  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsTrans  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lIsLocks  AS LOGICAL   NO-UNDO.
DEFINE STREAM monitorStrm.

IF INDEX(PROPATH,".\custom") EQ 0 THEN
    PROPATH = ".\custom," + PROPATH.

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
   {methods/lockWindowUpdate.i}
&ENDIF

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 fiVersion edLocks btnCompareSchema ~
btnClose 
&Scoped-Define DISPLAYED-OBJECTS fiVersion edLocks 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
    LABEL "&Close" 
    SIZE 15 BY 1.43.

DEFINE BUTTON btnCompareSchema 
    LABEL "&Compare Schema" 
    SIZE 23 BY 1.43.

DEFINE VARIABLE edLocks   AS CHARACTER 
    VIEW-AS EDITOR SCROLLBAR-VERTICAL
    SIZE 62 BY 9.91 NO-UNDO.

DEFINE VARIABLE fiVersion AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 65 BY 11.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    fiVersion AT ROW 1.71 COL 37 COLON-ALIGNED NO-LABELS WIDGET-ID 28
    edLocks AT ROW 6.33 COL 6 NO-LABELS WIDGET-ID 18
    btnCompareSchema AT ROW 17.67 COL 7 WIDGET-ID 6
    btnClose AT ROW 17.67 COL 49.8 HELP
    "Close TCP/IP Server Process"
    "Deltas To Apply" VIEW-AS TEXT
    SIZE 17.8 BY .62 AT ROW 5.29 COL 6.2 WIDGET-ID 22
    FGCOLOR 9 
    "Upgrade to Version (E.g. 16.2.0) :" VIEW-AS TEXT
    SIZE 33 BY .62 AT ROW 1.95 COL 4 WIDGET-ID 30
    RECT-2 AT ROW 5.62 COL 5 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 74.4 BY 19.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Transaction Monitor"
        HEIGHT             = 19.29
        WIDTH              = 74.4
        MAX-HEIGHT         = 320
        MAX-WIDTH          = 320
        VIRTUAL-HEIGHT     = 320
        VIRTUAL-WIDTH      = 320
        MAX-BUTTON         = NO
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transaction Monitor */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transaction Monitor */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
    DO:
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCompareSchema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCompareSchema C-Win
ON CHOOSE OF btnCompareSchema IN FRAME DEFAULT-FRAME /* Compare Schema */
    DO:
 
        RUN pShowNeededDeltas.
        RETURN NO-APPLY.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    DO:
        RUN disable_UI.
    END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        
    RUN enable_UI.

    RUN pCreateObjectReferences.
    RUN readDeltaFiles.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY fiVersion edLocks 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    ENABLE RECT-2 fiVersion edLocks btnCompareSchema btnClose 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateObjectReferences C-Win
PROCEDURE pCreateObjectReferences:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.3.4.df"
        ttDeltaList.awVersion    = "16.3.4"
        ttDeltaList.dbTable      = "EDPOTran"
        ttDeltaList.dbField      = "Ack-Date"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .  

    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.3.0.df"
        ttDeltaList.awVersion    = "16.3.0"
        ttDeltaList.dbTable      = "cust-markup"
        ttDeltaList.dbField      = "markup_reduction"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .  

    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.2.0.df"
        ttDeltaList.awVersion    = "16.2.0"
        ttDeltaList.dbTable      = "vend"
        ttDeltaList.dbField      = "payment-type"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .

    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.1.0.df"
        ttDeltaList.awVersion    = "16.1.0"
        ttDeltaList.dbTable      = "userEula"
        ttDeltaList.dbField      = "eula_code"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .
         
    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.0.6.df"
        ttDeltaList.awVersion    = "16.0.6"
        ttDeltaList.dbTable      = "emailcod"
        ttDeltaList.dbField      = "emailTo"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .
         
    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "asi_delta_16.0.0.df"
        ttDeltaList.awVersion    = "16.0.0"
        ttDeltaList.dbTable      = "oe-ord"
        ttDeltaList.dbField      = "ack-prnt-date"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "ASI" 
        .
        
    CREATE ttDeltaList.
    ASSIGN 
        ttDeltaList.allFileName  = "addon\nosweat_delta_16.3.0.df"
        ttDeltaList.awVersion    = "16.3.0"
        ttDeltaList.dbTable      = "mfgroup"
        ttDeltaList.dbField      = "mfgroup_data"
        ttDeltaList.objectType   = "field"
        ttDeltaList.shouldExist  = YES
        ttDeltaList.databaseName = "NOSWEAT" 
        .
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowNeededDeltas C-Win
PROCEDURE pShowNeededDeltas:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cDeltaDisplay AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPassesTest   AS LOG       NO-UNDO.
  
    /*  RUN pCreateObjectReferences. */
  

  
    DO WITH FRAME {&frame-name}:
        FIND FIRST ttDeltaList WHERE ttDeltaList.allFileName BEGINS "addon"
            NO-ERROR.
        IF AVAILABLE ttDeltaList THEN
            MESSAGE "Please run this utility from advantzware and addons also"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        edLocks:SCREEN-VALUE = "".
        ASSIGN fiVersion.
        
        FOR EACH ttDeltaList WHERE ttDeltaList.awVersion LE fiVersion:
            
            lPassesTest = FALSE.
            CASE ttDeltaList.databaseName:
                WHEN "ASI" THEN 
                    DO:                                                
                        FIND FIRST asi._file NO-LOCK  WHERE asi._file._file-name EQ ttDeltaList.dbTable     
                           NO-ERROR.
                        IF AVAILABLE asi._file THEN 
                            FIND FIRST asi._field NO-LOCK OF asi._file 
                                WHERE asi._field._field-name EQ   ttDeltaList.dbField
                                NO-ERROR.
                        lPassesTest = avail(asi._field) EQ ttDeltaList.shouldExist.                     

                    END.
                WHEN "NOSWEAT" THEN 
                    DO:
                        
                        FIND FIRST nosweat._file NO-LOCK WHERE nosweat._file._file-name EQ ttDeltaList.dbTable     
                            NO-ERROR.
                        IF AVAILABLE nosweat._file THEN 
                            FIND FIRST nosweat._field NO-LOCK OF nosweat._file 
                                WHERE nosweat._field._field-name EQ   ttDeltaList.dbField
                                NO-ERROR.
                        lPassesTest = avail(nosweat._field) EQ ttDeltaList.shouldExist.               
                    END.
            END CASE.
            
            ttDeltaList.passesTest = lPassesTest.

    
        END.  
        FOR EACH ttDeltaList WHERE ttDeltaList.awVersion LE fiVersion 
            BREAK BY ttDeltaList.allFileName
                  BY ttDeltaList.passesTest:  
          IF FIRST-OF(ttDeltaList.passesTest) THEN DO:
            lPassesTest = ttDeltaList.passesTest.

            cDeltaDisplay = 
                ttDeltaList.allFileName 
                + (IF lPassesTest THEN " Has Been Applied." ELSE " Needs to be applied")
                + chr(13)
                . 
                
            edLocks:SCREEN-VALUE = edLocks:SCREEN-VALUE + cDeltaDisplay.

          END.

        END.
    END. 
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readDeltaFiles C-Win
PROCEDURE readDeltaFiles:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName   AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE cShortName  AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cFolderName AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cFullName   AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE ctype       AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cInputLine  AS CHARACTER FORMAT "x(50)". 
    DEFINE VARIABLE cTable      AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.   
    
    DEFINE VARIABLE iLeftPos    AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE iRightPos   AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE iSlashPos AS INTEGER.  
     
    cFileName = SEARCH("stdMenu\deltas\asi_delta_15.9.0.df"). 
    /* cFileName = "C:\Advantzware\v16\Resources\stdMenu\deltas\asi_delta_15.9.0.df". */ 
    
    /* Examine folder for file list */    
    iSlashPos = R-INDEX(cFileName, "\"). 
    cShortName = SUBSTRING(cFileName,  iSlashPos + 1).  
    cFolderName = SUBSTRING(cFileName,  1, iSlashPos - 1).  

    INPUT FROM OS-DIR (cFolderName) .  
    REPEAT:  
        /* Input from os-Dir returns space delimited  File name and full name */
        IMPORT cFullName cFileName cType. 
        DISPLAY cFileName FORMAT "x(20)" cFullName FORMAT "x(65)"  cType.   
        IF cType BEGINS "F" THEN 
        DO:   
            CREATE ttFiles.  
            ASSIGN 
                ttFiles.ttFullName = cFullName  
                ttFiles.ttFileName = cFileName 
                .             
        END.   
    END.    
    INPUT CLOSE.  


    /* With each file in the list, read the contents */
    FOR EACH ttFiles:  
        cFileName = ttFiles.ttFileName.
        INPUT from value(cFileName).

        REPEAT: 
            cInputLine = "". 
            IMPORT UNFORMATTED cInputLine. 
            
            cInputLine = TRIM(cInputLine).
            IF NOT cInputLine BEGINS "add field" THEN NEXT.  
      
            iLeftPos = INDEX(cInputLine, '"').   
            iRightPos = R-INDEX(cInputLine, '"'). 
            
            IF iLeftPos EQ 0 OR iRightPos EQ 0 THEN NEXT. 
            
            cInputLine = SUBSTRING(cInputLine, iLeftPos, iRightPos - iLeftPos).   
            cInputLine = TRIM(REPLACE(cINputLine, '"', '')). 
            cInputLine = TRIM(REPLACE(cINputLine, '  ', ' ')).  
            cInputLine = TRIM(REPLACE(cINputLine, ' OF ', ',')).  
            
            cTable = ENTRY(2, cInputLine).  
            cField = ENTRY(1, cInputLine).    
            
            
            IF cShortName BEGINS "asi" THEN 
            DO:
                FIND FIRST asi._file NO-LOCK WHERE asi._file._file-name EQ cTable 
                     NO-ERROR. 
                IF AVAILABLE asi._file THEN  
                    FIND FIRST asi._field NO-LOCK OF asi._file WHERE  
                        asi._field._field-name EQ cField  
                        NO-ERROR. 
                          
                    CREATE ttDeltaList.
                    ASSIGN 
                        ttDeltaList.allFileName  = TRIM(ttFiles.ttfullname)
                        ttDeltaList.awVersion    = entry(3, ttFiles.ttFileName, "_")
                        ttDeltaList.dbTable      = cTable
                        ttDeltaList.dbField      = cField
                        ttDeltaList.objectType   = "field"
                        ttDeltaList.shouldExist  = YES
                        ttDeltaList.databaseName = "ASI" 
                        .         
            END.   
            ELSE IF cShortName BEGINS "nosweat" THEN 
                DO: 
                    FIND FIRST nosweat._file  NO-LOCK WHERE nosweat._file._file-name EQ cTable 
                        NO-ERROR. 
                    IF AVAILABLE nosweat._file THEN  
                        FIND FIRST nosweat._field NO-LOCK OF nosweat._file WHERE  
                            nosweat._field._field-name EQ cField  
                             NO-ERROR.   
                    CREATE ttDeltaList.
                    ASSIGN 
                        ttDeltaList.allFileName  = TRIM(ttFiles.ttfullname)
                        ttDeltaList.awVersion    = entry(3, ttFiles.ttFileName, "_")
                        ttDeltaList.dbTable      = cTable
                        ttDeltaList.dbField      = cField
                        ttDeltaList.objectType   = "field"
                        ttDeltaList.shouldExist  = YES
                        ttDeltaList.databaseName = "NOSWEAT" 
                        .                             

                END.   
                ELSE IF cShortName BEGINS "asihlp" THEN 
                    DO: 
                        FIND FIRST asihlp._file NO-LOCK WHERE asihlp._file._file-name EQ cTable 
                             NO-ERROR. 
                        IF AVAILABLE asihlp._file THEN  
                            FIND FIRST asihlp._field NO-LOCK OF asihlp._file WHERE  
                                asihlp._field._field-name EQ cField  
                                 NO-ERROR.   
                       CREATE ttDeltaList.
                       ASSIGN 
                            ttDeltaList.allFileName  = TRIM(ttFiles.ttfullname)
                            ttDeltaList.awVersion    = entry(3, ttFiles.ttFileName, "_")
                            ttDeltaList.dbTable      = cTable
                            ttDeltaList.dbField      = cField
                            ttDeltaList.objectType   = "field"
                            ttDeltaList.shouldExist  = YES
                            ttDeltaList.databaseName = "asihlp" 
                            . 
                    END.        
        END.  /* Repeat each line of file */
        INPUT close.    
    END. /* each ttFiles */
  


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize C-Win 
PROCEDURE winReSize :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
    ASSIGN
        {&WINDOW-NAME}:WINDOW-STATE          = 1
        {&WINDOW-NAME}:HEIGHT-PIXELS         = {&WINDOW-NAME}:HEIGHT-PIXELS - 30
        {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS
        {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        FRAME {&FRAME-NAME}:WIDTH-PIXELS     = {&WINDOW-NAME}:WIDTH-PIXELS
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS    = {&WINDOW-NAME}:HEIGHT-PIXELS
        .
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
    RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
    ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

