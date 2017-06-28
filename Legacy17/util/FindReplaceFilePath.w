&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/autoclosejobs.w

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

DEFINE STREAM log-out.
DEFINE STREAM attach-bak.
DEFINE STREAM boxdes-bak.
DEFINE STREAM pattern-bak.
DEFINE STREAM fgitem-bak.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 fi_find fi_replace tb_box tb_pattern ~
tb_fgitem tb_drive-only btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 fi_find fi_replace tb_box ~
tb_pattern tb_fgitem tb_drive-only 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "This utility will search all attachments in the system for what is specified in the Find box and replace it with what is in the Replace With box.  If you simply want to change the drive letter, check the ~"Change Drive Letter Only~" option." 
     VIEW-AS EDITOR
     SIZE 79 BY 2.38 NO-UNDO.

DEFINE VARIABLE fi_find AS CHARACTER FORMAT "X(40)":U 
     LABEL "Find" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE fi_replace AS CHARACTER FORMAT "X(40)":U 
     LABEL "Replace with" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.29.

DEFINE VARIABLE tb_box AS LOGICAL INITIAL no 
     LABEL "Include Box Images" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_drive-only AS LOGICAL INITIAL yes 
     LABEL "Change Drive Letter Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fgitem AS LOGICAL INITIAL no 
     LABEL "Include FGItem Images" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_pattern AS LOGICAL INITIAL no 
     LABEL "Include Pattern Images" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     EDITOR-1 AT ROW 5.29 COL 5 NO-LABEL WIDGET-ID 6
     fi_find AT ROW 7.81 COL 20 COLON-ALIGNED HELP
          "Enter string that you want to replace"
     fi_replace AT ROW 8.91 COL 20 COLON-ALIGNED HELP
          "Enter string to replace what is in the Find box"
     tb_box AT ROW 10.05 COL 23 WIDGET-ID 2
     tb_pattern AT ROW 11.05 COL 23 WIDGET-ID 10
     tb_fgitem AT ROW 12 COL 23 WIDGET-ID 12
     tb_drive-only AT ROW 12.95 COL 23 WIDGET-ID 8
     btn-process AT ROW 14.67 COL 22
     btn-cancel AT ROW 14.67 COL 52
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.2 BY 15.52.

DEFINE FRAME FRAME-B
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Find and Replace Drive and File Path in Attachments"
         HEIGHT             = 15.52
         WIDTH              = 90.2
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Find and Replace Drive and File Path in Attachments */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Find and Replace Drive and File Path in Attachments */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
    DEFINE VARIABLE lProcess AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
        IF fi_find EQ "" THEN DO:
            MESSAGE "Find field cannot be blank"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "Entry" TO fi_find.
            lProcess = NO.
        END.
        ELSE DO:
            cMessage = "Are you sure you want to find '" + fi_find + "' and ".
            IF fi_replace EQ "" 
                THEN cMessage = cMessage + " remove it from".
                ELSE cMessage = cMessage + " replace it with '" + fi_replace + "' in".
            IF tb_drive-only THEN
                cMessage = cMessage + " the drive letter of all attachments".
            ELSE
                cMessage = cMessage + " the file path of all attachments".
            IF tb_box THEN cMessage = cMessage + " and box images".
            cMessage = cMessage + "?".
            MESSAGE cMessage
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lProcess.
        END.
    END.
    IF lProcess THEN RUN RunProcess.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_find.
  END.
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
  DISPLAY EDITOR-1 fi_find fi_replace tb_box tb_pattern tb_fgitem tb_drive-only 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 fi_find fi_replace tb_box tb_pattern tb_fgitem tb_drive-only 
         btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunProcess C-Win 
PROCEDURE RunProcess :
DEFINE VARIABLE cFilePath AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNewFilePath AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-attach FOR ATTACH.
DEFINE BUFFER bf-box-design-hdr FOR box-design-hdr.
DEFINE BUFFER bf-strap for reftable.
DEFINE BUFFER bf-pattern for reftable.

DISABLE TRIGGERS FOR LOAD OF ATTACH.     
DISABLE TRIGGERS FOR LOAD OF box-design-hdr.    
DISABLE TRIGGERS FOR LOAD OF reftable.
DISABLE TRIGGERS FOR LOAD OF itemfg.

SESSION:SET-WAIT-STATE("General").

OUTPUT STREAM log-out TO VALUE("C:\tmp\log-find-replace-attach.txt").
OUTPUT STREAM attach-bak TO VALUE("C:\tmp\attach" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.
OUTPUT STREAM boxdes-bak TO VALUE("C:\tmp\box-design-hdr" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.
OUTPUT STREAM pattern-bak TO VALUE("C:\tmp\pattern" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.
OUTPUT STREAM fgitem-bak TO VALUE("C:\tmp\fgitem" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + ".d") APPEND.

IF tb_drive-only THEN DO:
    IF LENGTH(fi_find) NE 1 THEN DO:
        MESSAGE "If you are only changing drive letter, you can only put one letter in the Find: field"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO fi_find IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.
    IF LENGTH(fi_replace) NE 1 THEN DO:
        MESSAGE "If you are only changing drive letter, you can only put one letter in the Replace: field"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "entry" TO fi_find IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
    END.
END.

/*Process attachments*/
FOR EACH bf-attach EXCLUSIVE-LOCK:
    EXPORT STREAM attach-bak bf-attach.
    cFilePath = bf-attach.attach-file.
    IF tb_drive-only THEN 
        IF SUBSTRING(cFilePath,1,1) EQ fi_find THEN
            cNewFilePath = fi_replace + SUBSTRING(cFilePath,2,LENGTH(cFilePath)).
        ELSE cNewFilePath = cFilePath.
    ELSE
        cNewFilePath = REPLACE(cFilePath, fi_find, fi_replace).
    IF cFilePath NE cNewFilePath THEN DO:
        PUT STREAM log-out "ATTACHMENT: " cFilePath FORMAT "x(100)" " to " cNewFilePath FORMAT "x(100)" SKIP.        
        bf-attach.attach-file = cNewFilePath.
    END.
END.
IF tb_box THEN DO:  /*Include Box Images*/
    FOR EACH bf-box-design-hdr EXCLUSIVE-LOCK:
        EXPORT STREAM boxdes-bak bf-box-design-hdr.
        cFilePath = bf-box-design-hdr.box-image.
        IF cFilePath <> "" THEN DO: 
            IF tb_drive-only THEN 
                IF SUBSTRING(cFilePath,1,1) EQ fi_find THEN
                    cNewFilePath = fi_replace + SUBSTRING(cFilePath,2,LENGTH(cFilePath)).
                ELSE cNewFilePath = cFilePath.
            ELSE
                cNewFilePath = REPLACE(cFilePath, fi_find, fi_replace).
            IF cFilePath NE cNewFilePath THEN DO:
                PUT STREAM log-out "BOX DESIGN: " cFilePath FORMAT "x(100)" " to " cNewFilePath FORMAT "x(100)" SKIP.        
                bf-box-design-hdr.box-image = cNewFilePath.
            END.
        END.
        cFilePath = bf-box-design-hdr.box-3d-image.
        IF cFilePath <> "" THEN DO: 
            IF tb_drive-only THEN 
                IF SUBSTRING(cFilePath,1,1) EQ fi_find THEN
                    cNewFilePath = fi_replace + SUBSTRING(cFilePath,2,LENGTH(cFilePath)).
                ELSE cNewFilePath = cFilePath.
            ELSE
                cNewFilePath = REPLACE(cFilePath, fi_find, fi_replace).
            IF cFilePath NE cNewFilePath THEN DO:
                PUT STREAM log-out "BOX DESIGN(3D): " cFilePath FORMAT "x(100)" " to " cNewFilePath FORMAT "x(100)" SKIP.        
                bf-box-design-hdr.box-3d-image = cNewFilePath.
            END.
        END.
    END. /*each box-design-hdr*/
END. /* if tb_box - "Include Box Images checked*/

IF tb_pattern THEN DO:
 FOR EACH reftable NO-LOCK
    WHERE reftable.reftable = "STACK"
      AND ASI.reftable.company = "" 
      AND reftable.loc = "" , 
    FIRST bf-strap  WHERE bf-strap.reftable = "STACKSTRAP"
      AND bf-strap.company = ""
      AND bf-strap.loc = ""
      AND bf-strap.code = reftable.code NO-LOCK, 
    FIRST bf-pattern WHERE bf-pattern.reftable = "STACKPAT"
                               AND bf-pattern.company = "" 
                               AND bf-pattern.loc = ""    
                               AND bf-pattern.dscr <> "" 
                               AND bf-pattern.code = reftable.code EXCLUSIVE-LOCK :

    {custom/statusMsg.i "'Processing Pattern # ' + string(bf-strap.code)"}

    EXPORT STREAM pattern-bak bf-pattern .

    cFilePath = bf-pattern.dscr .
        IF cFilePath <> "" THEN DO: 
            IF tb_drive-only THEN 
                IF SUBSTRING(cFilePath,1,1) EQ fi_find THEN
                    cNewFilePath = fi_replace + SUBSTRING(cFilePath,2,LENGTH(cFilePath)).
                ELSE cNewFilePath = cFilePath.
            ELSE
                cNewFilePath = REPLACE(cFilePath, fi_find, fi_replace).
            IF cFilePath NE cNewFilePath THEN DO:
                PUT STREAM log-out "Pattern DESIGN: " cFilePath FORMAT "x(100)" " to " cNewFilePath FORMAT "x(100)" SKIP.        
                bf-pattern.dscr = cNewFilePath.
            END.
        END.
 END. /* for each reftable*/
END. /* tb_pattern*/

IF tb_fgitem THEN DO:
 FOR EACH itemfg EXCLUSIVE-LOCK
    WHERE  itemfg.box-image <> "" :

    {custom/statusMsg.i "'Processing FG Item # ' + string(itemfg.i-no)"}

    EXPORT STREAM fgitem-bak itemfg .
    cFilePath = itemfg.box-image  .
    IF cFilePath <> "" THEN DO: 
        IF tb_drive-only THEN 
            IF SUBSTRING(cFilePath,1,1) EQ fi_find THEN
                cNewFilePath = fi_replace + SUBSTRING(cFilePath,2,LENGTH(cFilePath)).
            ELSE cNewFilePath = cFilePath.
        ELSE
            cNewFilePath = REPLACE(cFilePath, fi_find, fi_replace).

            IF cFilePath NE cNewFilePath THEN DO:
                PUT STREAM log-out "FG Item Image: " cFilePath FORMAT "x(100)" " to " cNewFilePath FORMAT "x(100)" SKIP.        
                itemfg.box-image = cNewFilePath.
            END.
    END.
 END. /* for each itemfg*/
END. /* tb_pattern*/



OUTPUT STREAM log-out CLOSE.
OUTPUT STREAM attach-bak CLOSE.
OUTPUT STREAM boxdes-bak CLOSE.
OUTPUT STREAM pattern-bak CLOSE.
OUTPUT STREAM fgitem-bak CLOSE.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
APPLY "close" TO THIS-PROCEDURE.

RETURN NO-APPLY.

/* end ---------------------------------- copr. 2006  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

