&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: EDIRelBL.w

  Description: browser of customer values for EDI

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.15.2010
           1.04.2013  - task 12031201 rstark - Rheem format change

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE relh-recid AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto AS LOGICAL NO-UNDO.

DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.

{oe/chkordl.i NEW}
{oe/EDIRelBL.i}
/*&IF DEFINED(useLotNo) NE 0 &THEN*/
{oe/relemail.i NEW}
/*&ENDIF*/

DEFINE VARIABLE custFTP AS CHARACTER NO-UNDO.
DEFINE VARIABLE ediReleaseCharFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE ediBOLPostCharFld AS CHARACTER NO-UNDO.
DEFINE VARIABLE endTime AS INTEGER NO-UNDO.
DEFINE VARIABLE sysCarrier AS CHARACTER NO-UNDO.
DEF VAR iocPrompt AS CHAR NO-UNDO. /* passes back and forth through relmerge */
ASSIGN
  cocode = IF g_company NE '' THEN g_company ELSE 'MAIN'
  locode = IF g_loc NE '' THEN g_loc ELSE '001'.

{sys/inc/oereordr.i}
{sys/inc/addrelse.i}

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ 'OECARIER' NO-ERROR.
sysCarrier = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE 'ShipTo'.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ 'EDIRelease' NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN
ediReleaseCharFld = sys-ctrl.char-fld.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ 'EDIBOLPost' NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN
ediBOLPostCharFld = sys-ctrl.char-fld.

DEFINE STREAM ediIN.
DEFINE STREAM ediOUT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME EDIBOLPost

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sys-ctrl-shipto cust

/* Definitions for BROWSE EDIBOLPost                                    */
&Scoped-define FIELDS-IN-QUERY-EDIBOLPost sys-ctrl-shipto.cust-vend-no ~
cust.name sys-ctrl-shipto.char-fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-EDIBOLPost 
&Scoped-define QUERY-STRING-EDIBOLPost FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ cocode AND ~
sys-ctrl-shipto.name EQ 'EDIBOLPost' AND ~
sys-ctrl-shipto.cust-vend EQ YES AND ~
sys-ctrl-shipto.log-fld EQ YES NO-LOCK, ~
      FIRST cust WHERE cust.cust-no EQ sys-ctrl-shipto.cust-vend-no NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-EDIBOLPost OPEN QUERY EDIBOLPost FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ cocode AND ~
sys-ctrl-shipto.name EQ 'EDIBOLPost' AND ~
sys-ctrl-shipto.cust-vend EQ YES AND ~
sys-ctrl-shipto.log-fld EQ YES NO-LOCK, ~
      FIRST cust WHERE cust.cust-no EQ sys-ctrl-shipto.cust-vend-no NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-EDIBOLPost sys-ctrl-shipto cust
&Scoped-define FIRST-TABLE-IN-QUERY-EDIBOLPost sys-ctrl-shipto
&Scoped-define SECOND-TABLE-IN-QUERY-EDIBOLPost cust


/* Definitions for BROWSE EDIRelease                                    */
&Scoped-define FIELDS-IN-QUERY-EDIRelease sys-ctrl-shipto.cust-vend-no ~
cust.name sys-ctrl-shipto.char-fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-EDIRelease 
&Scoped-define QUERY-STRING-EDIRelease FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ cocode AND ~
sys-ctrl-shipto.name EQ 'EDIRelease' AND ~
sys-ctrl-shipto.cust-vend EQ YES AND ~
sys-ctrl-shipto.log-fld EQ YES NO-LOCK, ~
      FIRST cust WHERE cust.cust-no EQ sys-ctrl-shipto.cust-vend-no NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-EDIRelease OPEN QUERY EDIRelease FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ cocode AND ~
sys-ctrl-shipto.name EQ 'EDIRelease' AND ~
sys-ctrl-shipto.cust-vend EQ YES AND ~
sys-ctrl-shipto.log-fld EQ YES NO-LOCK, ~
      FIRST cust WHERE cust.cust-no EQ sys-ctrl-shipto.cust-vend-no NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-EDIRelease sys-ctrl-shipto cust
&Scoped-define FIRST-TABLE-IN-QUERY-EDIRelease sys-ctrl-shipto
&Scoped-define SECOND-TABLE-IN-QUERY-EDIRelease cust


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-EDIBOLPost}~
    ~{&OPEN-QUERY-EDIRelease}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDIRelease EDIBOLPost ftpData btnSave ~
btnEDIRelease btnEDIBOLPost btnRunAutoLoad btnClose 
&Scoped-Define DISPLAYED-OBJECTS ftpFormat ftpData 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 EDIRelease EDIBOLPost btnUndo btnSave btnEDIRelease ~
btnEDIBOLPost btnRunAutoLoad btnClose 
&Scoped-define List-2 intervalValue btnStartStop 
&Scoped-define List-3 ftpData btnUndo btnSave 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     LABEL "&Close" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnEDIBOLPost 
     LABEL "EDI &BOL Post" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnEDIRelease 
     LABEL "EDI &Release" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnRunAutoLoad 
     LABEL "&Run Auto Load Monitoring" 
     SIZE 30 BY 1.14.

DEFINE BUTTON btnSave 
     LABEL "&Save" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnStartStop 
     LABEL "&Start" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnUndo 
     LABEL "&Undo Change" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE ftpData AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 45 BY 5.24
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ftpFormat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP
     SIZE 45 BY 7.14 NO-UNDO.

DEFINE VARIABLE intervalValue AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "&Interval in Minutes" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.14 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 13.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY EDIBOLPost FOR 
      sys-ctrl-shipto, 
      cust SCROLLING.

DEFINE QUERY EDIRelease FOR 
      sys-ctrl-shipto, 
      cust SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE EDIBOLPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS EDIBOLPost C-Win _STRUCTURED
  QUERY EDIBOLPost NO-LOCK DISPLAY
      sys-ctrl-shipto.cust-vend-no COLUMN-LABEL "BOLPost ID" FORMAT "x(8)":U
      cust.name FORMAT "x(30)":U
      sys-ctrl-shipto.char-fld COLUMN-LABEL "Form" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 14.76.

DEFINE BROWSE EDIRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS EDIRelease C-Win _STRUCTURED
  QUERY EDIRelease NO-LOCK DISPLAY
      sys-ctrl-shipto.cust-vend-no COLUMN-LABEL "Release ID" FORMAT "x(8)":U
            WIDTH 11.2
      cust.name FORMAT "x(30)":U
      sys-ctrl-shipto.char-fld COLUMN-LABEL "Form" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 14.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EDIRelease AT ROW 1.24 COL 2 HELP
          "Select Customer"
     EDIBOLPost AT ROW 1.24 COL 63 HELP
          "Select Customer"
     ftpFormat AT ROW 1.95 COL 125 NO-LABEL
     ftpData AT ROW 9.33 COL 125 NO-LABEL
     btnUndo AT ROW 15.05 COL 138
     btnSave AT ROW 15.05 COL 155
     btnEDIRelease AT ROW 16.24 COL 2 HELP
          "Release Auto Load & Create"
     btnEDIBOLPost AT ROW 16.24 COL 63 HELP
          "BOL Post FTP"
     intervalValue AT ROW 16.24 COL 98 COLON-ALIGNED HELP
          "Enter Interval in Minutes to Monitor EDI Releases"
     btnStartStop AT ROW 16.24 COL 107 HELP
          "Start/Stop Auto Load Monitoring"
     btnRunAutoLoad AT ROW 16.24 COL 124 HELP
          "Run this Process in Background Auto Monitor Mode"
     btnClose AT ROW 16.24 COL 155 HELP
          "Close this Module"
     " BOL Post FTP Information: Format" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 1.24 COL 125
     RECT-1 AT ROW 1.48 COL 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 171 BY 16.5.


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
         TITLE              = "Release Create & BOL Load (v1.01)"
         HEIGHT             = 16.52
         WIDTH              = 171
         MAX-HEIGHT         = 16.52
         MAX-WIDTH          = 171
         VIRTUAL-HEIGHT     = 16.52
         VIRTUAL-WIDTH      = 171
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB EDIRelease RECT-1 DEFAULT-FRAME */
/* BROWSE-TAB EDIBOLPost EDIRelease DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnClose IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnEDIBOLPost IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnEDIRelease IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnRunAutoLoad IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   1 3                                                                  */
/* SETTINGS FOR BUTTON btnStartStop IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
ASSIGN 
       btnStartStop:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnUndo IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BROWSE EDIBOLPost IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BROWSE EDIRelease IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR EDITOR ftpData IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR EDITOR ftpFormat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN intervalValue IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE 2                                               */
ASSIGN 
       intervalValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE EDIBOLPost
/* Query rebuild information for BROWSE EDIBOLPost
     _TblList          = "asi.sys-ctrl-shipto,asi.cust WHERE asi.sys-ctrl-shipto ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Where[1]         = "sys-ctrl-shipto.company EQ cocode AND
sys-ctrl-shipto.name EQ 'EDIBOLPost' AND
sys-ctrl-shipto.cust-vend EQ YES AND
sys-ctrl-shipto.log-fld EQ YES"
     _JoinCode[2]      = "cust.cust-no EQ sys-ctrl-shipto.cust-vend-no"
     _FldNameList[1]   > asi.sys-ctrl-shipto.cust-vend-no
"sys-ctrl-shipto.cust-vend-no" "BOLPost ID" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.cust.name
     _FldNameList[3]   > asi.sys-ctrl-shipto.char-fld
"sys-ctrl-shipto.char-fld" "Form" "x(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE EDIBOLPost */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE EDIRelease
/* Query rebuild information for BROWSE EDIRelease
     _TblList          = "asi.sys-ctrl-shipto,asi.cust WHERE asi.sys-ctrl-shipto ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Where[1]         = "sys-ctrl-shipto.company EQ cocode AND
sys-ctrl-shipto.name EQ 'EDIRelease' AND
sys-ctrl-shipto.cust-vend EQ YES AND
sys-ctrl-shipto.log-fld EQ YES"
     _JoinCode[2]      = "cust.cust-no EQ sys-ctrl-shipto.cust-vend-no"
     _FldNameList[1]   > asi.sys-ctrl-shipto.cust-vend-no
"sys-ctrl-shipto.cust-vend-no" "Release ID" ? "character" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = asi.cust.name
     _FldNameList[3]   > asi.sys-ctrl-shipto.char-fld
"sys-ctrl-shipto.char-fld" "Form" "x(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE EDIRelease */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = 4.76
       WIDTH           = 20
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-BEFORE(EDIRelease:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Release Create  BOL Load (v1.01) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Release Create  BOL Load (v1.01) */
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
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEDIBOLPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEDIBOLPost C-Win
ON CHOOSE OF btnEDIBOLPost IN FRAME DEFAULT-FRAME /* EDI BOL Post */
DO:
  IF EDIBOLPostCharFld NE '' THEN RUN ftpFiles.
  MESSAGE 'BOL Post FTP Complete' VIEW-AS ALERT-BOX.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEDIRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEDIRelease C-Win
ON CHOOSE OF btnEDIRelease IN FRAME DEFAULT-FRAME /* EDI Release */
DO:
  IF EDIReleaseCharFld NE '' THEN RUN loadFiles.
  MESSAGE 'Release Auto Load & Create Complete' VIEW-AS ALERT-BOX.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunAutoLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunAutoLoad C-Win
ON CHOOSE OF btnRunAutoLoad IN FRAME DEFAULT-FRAME /* Run Auto Load Monitoring */
DO:
  DISABLE {&List-1} WITH FRAME {&FRAME-NAME}.
  ASSIGN
    btnStartStop:HIDDEN = NO
    intervalValue:HIDDEN = NO.
  ENABLE {&List-2} WITH FRAME {&FRAME-NAME}.
  APPLY 'ENTRY':U TO intervalValue.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
  ftpData:SAVE-FILE(custFTP).
  DISABLE btnUndo WITH FRAME {&FRAME-NAME}.
  MESSAGE 'FTP Data Information SAVED' VIEW-AS ALERT-BOX.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStartStop C-Win
ON CHOOSE OF btnStartStop IN FRAME DEFAULT-FRAME /* Start */
DO:
  IF intervalValue NE 0 THEN
  CASE SELF:LABEL:
    WHEN '~&Start' THEN DO:
      DISABLE intervalValue WITH FRAME {&FRAME-NAME}.
      ASSIGN
        SELF:LABEL = '~&Stop'
        chCtrlFrame:PSTimer:ENABLED = TRUE
        chCtrlFrame:PSTimer:Interval = 1000.
        endTime = 0.
      APPLY 'ENTRY':U TO SELF.
    END.
    WHEN '~&Stop' THEN DO:
      ASSIGN
        SELF:LABEL = '~&Start'
        chCtrlFrame:PSTimer:Interval = 0
        chCtrlFrame:PSTimer:ENABLED = FALSE
        intervalValue = 0
        intervalValue:HIDDEN = TRUE
        SELF:HIDDEN = TRUE.
      ENABLE {&List-1} WITH FRAME {&FRAME-NAME}.
      DISABLE btnUndo WITH FRAME {&FRAME-NAME}.
      APPLY 'ENTRY':U TO {&BROWSE-NAME}.
    END.
  END CASE.
  ELSE DO:
    MESSAGE 'Please Enter Interval Time in Minutes' VIEW-AS ALERT-BOX WARNING.
    APPLY 'ENTRY':U TO intervalValue.
  END.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUndo C-Win
ON CHOOSE OF btnUndo IN FRAME DEFAULT-FRAME /* Undo Change */
DO:
  ftpData:READ-FILE(custFTP).
  DISABLE btnUndo WITH FRAME {&FRAME-NAME}.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  IF btnStartStop:LABEL IN FRAME {&FRAME-NAME} EQ '~&Stop' THEN DO:
    IF TIME GE endTime THEN DO:
      IF EDIReleaseCharFld NE '' THEN
      FOR EACH sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company EQ cocode
            AND sys-ctrl-shipto.name EQ 'EDIRelease'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.log-fld EQ YES:
        RUN loadFiles.
      END. /* each sys-ctrl-shipto */
      IF EDIBOLPostCharFld NE '' THEN
      FOR EACH sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company EQ cocode
            AND sys-ctrl-shipto.name EQ 'EDIBOLPost'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.log-fld EQ YES:
        RUN ftpFiles.
      END. /* each sys-ctrl-shipto */
      endTime = TIME + intervalValue * 60.
    END. /* if time */
  END. /* if label */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME EDIBOLPost
&Scoped-define SELF-NAME EDIBOLPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDIBOLPost C-Win
ON DEFAULT-ACTION OF EDIBOLPost IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CHOOSE':U TO btnEDIBOLPost.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDIBOLPost C-Win
ON ENTRY OF EDIBOLPost IN FRAME DEFAULT-FRAME
DO:
  ENABLE btnEDIBOLPost WITH FRAME {&FRAME-NAME}.
  DISABLE btnEDIRelease WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDIBOLPost C-Win
ON VALUE-CHANGED OF EDIBOLPost IN FRAME DEFAULT-FRAME
DO:
  IF NOT AVAILABLE sys-ctrl-shipto THEN DO:
    DISABLE {&List-3} WITH FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.

  custFTP = ediBOLPostCharFld + '/' + sys-ctrl-shipto.cust-vend-no + '.dat'.
  IF SEARCH(custFTP) EQ ? THEN DO:
    OUTPUT TO VALUE(custFTP).
    PUT UNFORMATTED 'FTP Data Information Missing' SKIP.
    OUTPUT CLOSE.
  END.
  ftpData:READ-FILE(custFTP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME EDIRelease
&Scoped-define SELF-NAME EDIRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDIRelease C-Win
ON DEFAULT-ACTION OF EDIRelease IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CHOOSE':U TO btnEDIRelease.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDIRelease C-Win
ON ENTRY OF EDIRelease IN FRAME DEFAULT-FRAME
DO:
  ENABLE btnEDIRelease WITH FRAME {&FRAME-NAME}.
  DISABLE btnEDIBOLPost WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ftpData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ftpData C-Win
ON ENTRY OF ftpData IN FRAME DEFAULT-FRAME
DO:
  ENABLE btnUndo WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME intervalValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intervalValue C-Win
ON LEAVE OF intervalValue IN FRAME DEFAULT-FRAME /* Interval in Minutes */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME EDIBOLPost
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  SESSION:SET-WAIT-STATE('').
  RUN enable_UI.
  ftpFormat:SCREEN-VALUE = 'Line 1: FTP IP Address' + CHR(10) +
    'Line 2: FTP User Login' + CHR(10) +
    'Line 3: FTP User Password' + CHR(10) +
    'Line 4: FTP Remote Directory' + CHR(10) + CHR(10) +
    'Example:' + CHR(10) + CHR(10) +
    '256.256.256.256' + CHR(10) +
    'user-login-id' + CHR(10) +
    'user-password' + CHR(10) +
    'htdocs/bol-post-files/directory'.
  APPLY 'VALUE-CHANGED':U TO EDIBOLPost.
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "EDIRelBL.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "EDIRelBL.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.
  DISPLAY ftpFormat ftpData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE EDIRelease EDIBOLPost ftpData btnSave btnEDIRelease btnEDIBOLPost 
         btnRunAutoLoad btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ftpFiles C-Win 
PROCEDURE ftpFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bolDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bolFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DEFINE VARIABLE ipAddr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ftpUser AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ftpPswd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ftpDir AS CHARACTER NO-UNDO.

  IF NOT AVAILABLE sys-ctrl-shipto THEN DO:
    MESSAGE 'Please Select a BOLPost ID!' VIEW-AS ALERT-BOX.
    RETURN.
  END.

  bolDir = ediBOLPostCharFld + '/' + sys-ctrl-shipto.cust-vend-no.
  OS-CREATE-DIR VALUE(bolDir).
  OS-CREATE-DIR VALUE(bolDir + '/archive').

  RUN getFiles (bolDir,OUTPUT bolFile).
  IF bolFile EQ '' THEN RETURN.

  SESSION:SET-WAIT-STATE('General').

  INPUT FROM VALUE(custFTP) NO-ECHO.
  IMPORT ipAddr.
  IMPORT ftpUser.
  IMPORT ftpPswd.
  IMPORT ftpDir.
  INPUT CLOSE.

  OUTPUT TO VALUE(bolDir + '/ftpBOL.bat').
  PUT UNFORMATTED 'ftp -v -i -s:' bolDir '/ftpBOL.cmd ' ipAddr SKIP.
  OUTPUT TO VALUE(bolDir + '/ftpBOL.cmd').
  PUT UNFORMATTED
    ftpUser SKIP
    ftpPswd SKIP
    'cd ' ftpDir SKIP
    'lcd ' bolDir SKIP.
  DO i = NUM-ENTRIES(bolFile) TO 1 BY -1:
    PUT UNFORMATTED 'put ' SEARCH(bolDir + '/' + ENTRY(i,bolFile)) SKIP.
  END. /* do i */
  PUT UNFORMATTED 'quit' SKIP.
  OUTPUT CLOSE.

  OS-COMMAND SILENT VALUE(SEARCH(bolDir + '/ftpBOL.bat')).

  OS-DELETE VALUE(bolDir + '/ftpBOL.bat').
  OS-DELETE VALUE(bolDir + '/ftpBOL.cmd').

  DO i = 1 TO NUM-ENTRIES(bolFile):
    OS-COPY VALUE(bolDir + '/' + ENTRY(i,bolFile))
            VALUE(bolDir + '/archive/' + ENTRY(i,bolFile)).
    IF SEARCH(bolDir + '/archive/' + ENTRY(i,bolFile)) NE ? THEN
    OS-DELETE VALUE(bolDir + '/' + ENTRY(i,bolFile)).
  END. /* do i */

  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFiles C-Win 
PROCEDURE getFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipDir AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE osFileName AS CHARACTER FORMAT 'X(30)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.

  INPUT FROM OS-DIR(ipDir) NO-ECHO.
  REPEAT:
    SET osFileName ^ attrList.
    IF attrList NE 'f' OR
       osFileName BEGINS '.' OR
       osFileName BEGINS 'EDI' OR
       INDEX(osFileName,'.log') NE 0 THEN NEXT.
    opFile = opFile + (IF opFile NE '' THEN ',' ELSE '') + osFileName.
  END. /* repeat */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFiles C-Win 
PROCEDURE loadFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ediDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE qtyAlloc AS DECIMAL NO-UNDO.
  DEFINE VARIABLE qtyAvail AS DECIMAL NO-UNDO.

  DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fil_id AS RECID NO-UNDO.
  DEFINE VARIABLE v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.

  DEFINE VARIABLE ediPONo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediLineNo AS INTEGER NO-UNDO.
  DEFINE VARIABLE ediPartNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediVendPartNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediQty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ediDate AS DATE NO-UNDO.

  /* task 12031201 rstark begin */ /*
  DEFINE VARIABLE ediLotNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediNotes AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ediShipID AS CHARACTER NO-UNDO. */
  DEFINE VARIABLE ediDateTime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  /* task 12031201 rstark end */

  IF NOT AVAILABLE sys-ctrl-shipto THEN DO:
    MESSAGE 'Please Select a Release ID!' VIEW-AS ALERT-BOX.
    RETURN.
  END.

  ediDir = ediReleaseCharFld + '/' + sys-ctrl-shipto.cust-vend-no.
  OS-CREATE-DIR VALUE(ediDir).
  OS-CREATE-DIR VALUE(ediDir + '/archive').

  RUN getFiles (ediDir,OUTPUT ediFile).
  IF ediFile EQ '' THEN RETURN.

  SESSION:SET-WAIT-STATE('General').

  DO i = NUM-ENTRIES(ediFile) TO 1 BY -1:
    OUTPUT STREAM ediOUT TO VALUE(ediDir + '/archive/' + ENTRY(i,edifile) + '.log').
    INPUT STREAM ediIN FROM VALUE(ediDir + '/' + ENTRY(i,ediFile)) NO-ECHO.
    /* task 12031201 rstark begin */
    IF sys-ctrl-shipto.char-fld EQ 'RHEEM' THEN DO:
      IMPORT STREAM ediIN ^. /* column headers */
      IMPORT STREAM ediIN DELIMITER ',' ^ ^ ^ ^ ^ ^ ^ ediDateTime. /* order date column */
      ediDate = DATE(ENTRY(1,ediDateTime,' ')).
      DO j = 1 TO 5:
        IMPORT STREAM ediIN ^.
      END. /* do j */
      IMPORT STREAM ediIN DELIMITER ',' ediPONo. /* row #8 */
      IMPORT STREAM ediIN ^.
    END. /* if rheem */
    /* task 12031201 rstark end */
    REPEAT:
      CASE sys-ctrl-shipto.char-fld:
        WHEN 'RHEEM' THEN DO:
          /* task 12031201 rstark begin */ /*
          IMPORT STREAM ediIN DELIMITER ',' ediPONo ediLineNo ediPartNo ediVendPartNo ediQty ediLotNo ediDate ediNotes. */
          /* there are now 2 row for each line */
          IMPORT STREAM ediIN DELIMITER ','
            ediLineNo /* line column */
            ^ /* type column */
            ediPartNo /* item/job column */
            ^ /* 2nd item/job column */
            ediVendPartNo /* supplier item */
            ^ /* description column */
            ^ /* uom column */
            ^ /* qty column */
            ^ /* price column */
            ^ /* status column */
            ^ /* reason column */
            ^ /* shipment column */
            .
          IMPORT STREAM ediIN DELIMITER ','
            ^ /* line column */
            ^ /* type column */
            ^ /* item/job column */
            ^ /* 2nd item/job column */
            ^ /* supplier item */
            ^ /* description column */
            ^ /* uom column */
            ediQty /* qty column */
            ^ /* price column */
            ^ /* status column */
            ^ /* reason column */
            ^ /* shipment column */
            .
          /* task 12031201 rstark end */
          ASSIGN
            ediPONo = TRIM(ediPONo)
            ediPartNo = TRIM(ediPartNo)
            ediVendPartNo = TRIM(ediVendPartNo)
         /* ediLotNo = TRIM(ediLotNo) task 12031201 rstark
            ediNotes = TRIM(ediNotes)
            ediShipID = STRING(INTEGER(SUBSTR(ediLotNo,3,2))) */
            .

          FOR EACH oe-ordl EXCLUSIVE-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.part-no EQ ediVendPartNo
                AND oe-ordl.po-no EQ '',
              FIRST itemfg OF oe-ordl NO-LOCK:
            qtyAvail = 0.
            FOR EACH fg-bin NO-LOCK
                WHERE fg-bin.company EQ itemfg.company
                  AND fg-bin.job-no EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no EQ itemfg.i-no:
              qtyAvail = qtyAvail + fg-bin.qty.
            END. /* each fg-bin */
            IF qtyAvail LE 0 THEN NEXT.
            FIND FIRST xoe-ord NO-LOCK
                 WHERE xoe-ord.company EQ cocode
                   AND xoe-ord.ord-no EQ oe-ordl.ord-no.
            &SCOPED-DEFINE programVersion EDIRelBOLPost
            {oe/check-release.i}
            IF lv-msg NE '' THEN DO:
              PUT STREAM ediOUT UNFORMATTED
               'Release Check: (' NOW ') ' lv-msg ' '
                xoe-ord.ord-no ' '
                ediPONo ' '
                ediLineNo ' '
                ediPartNo ' '
                ediVendPartNo ' '
                ediQty ' '
             /* ediLotNo ' ' task 12031201 rstark */
                ediDate ' '
             /* ediNotes task 12031201 rstark */
                SKIP
                .
              lv-msg = ''.
              NEXT.
            END. /* if lv-msg */

            PUT STREAM ediOUT UNFORMATTED
             'Create Release for Order: (' NOW ') ' oe-ordl.ord-no ' '
              ediPONo ' '
              ediLineNo ' '
              ediPartNo ' '
              ediVendPartNo ' '
              ediQty ' '
           /* ediLotNo ' ' task 12031201 rstark */
              ediDate ' '
           /* ediNotes ' ' task 12031201 rstark */
              qtyAvail
              SKIP
              .

            FIND FIRST oe-rel NO-LOCK USE-INDEX seq-no NO-ERROR.
            v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.
            CREATE oe-rel.
            ASSIGN
              oe-ordl.e-num = ediLineNo
              oe-rel.carrier = IF sysCarrier EQ 'ShipTo' AND AVAIL shipto THEN shipto.carrier
                               ELSE xoe-ord.carrier
              oe-rel.company = oe-ordl.company
              oe-rel.cust-no = oe-ordl.cust-no
              oe-rel.i-no = oe-ordl.i-no
              oe-rel.line = oe-ordl.line
              oe-rel.loc = g_loc
              /* task 12031201 rstark
              &IF DEFINED(useLotNo) NE 0 &THEN
              oe-rel.lot-no = ediLotNo
              &ENDIF */
              oe-rel.qty = IF qtyAvail GE ediQty THEN ediQty ELSE qtyAvail
              oe-rel.ord-no = oe-ordl.ord-no
              oe-rel.po-no = ediPONo
              oe-rel.r-no = v-nxt-r-no
              oe-rel.rel-date = ediDate
              oe-rel.sold-no = xoe-ord.sold-no
              oe-rel.tot-qty = oe-rel.qty
              ediQty = ediQty - oe-rel.qty
              .
            /* task 12031201 rstark
            FIND FIRST shipto NO-LOCK
                 WHERE shipto.company EQ oe-ordl.company
                   AND shipto.ship-id EQ ediShipID
                   AND shipto.cust-no EQ oe-ordl.cust-no
                 NO-ERROR.
            IF NOT AVAILABLE shipto THEN */
            FIND FIRST shipto NO-LOCK
                 WHERE shipto.company EQ oe-ordl.company
                   AND shipto.cust-no EQ oe-ordl.cust-no
                   AND shipto.ship-id EQ oe-ordl.ship-id
                 NO-ERROR.
            IF NOT AVAILABLE shipto THEN
            FIND FIRST shipto NO-LOCK
                 WHERE shipto.company EQ xoe-ord.company
                   AND shipto.cust-no EQ xoe-ord.cust-no
                   AND shipto.ship-id EQ xoe-ord.cust-no
                 NO-ERROR.
            IF AVAILABLE shipto THEN
            ASSIGN
              oe-rel.ship-id = shipto.ship-id
              oe-rel.ship-addr = shipto.ship-addr[1]
              oe-rel.ship-addr = shipto.ship-addr[2]
              oe-rel.ship-city = shipto.ship-city
              oe-rel.ship-state = shipto.ship-state
              oe-rel.ship-zip = shipto.ship-zip
              .
            ELSE
            ASSIGN
              oe-rel.ship-id = oe-ordl.ship-id
              oe-rel.ship-addr = xoe-ord.addr[1]
              oe-rel.ship-addr = xoe-ord.addr[2]
              oe-rel.ship-city = xoe-ord.city
              oe-rel.ship-state = xoe-ord.state
              oe-rel.ship-zip = xoe-ord.zip
              .

            /* task 12031201 rstark
            &IF DEFINED(useLotNo) EQ 0 &THEN
            FIND FIRST reftable EXCLUSIVE-LOCK
                 WHERE reftable.reftable EQ 'oe-rel.lot-no'
                   AND reftable.company EQ STRING(oe-rel.r-no,'9999999999') NO-ERROR.
            IF NOT AVAIL reftable THEN DO:
               CREATE reftable.
               ASSIGN
                 reftable.reftable = 'oe-rel.lot-no'
                 reftable.company = STRING(oe-rel.r-no,'9999999999').
            END. /* if not avail */
            reftable.code = ediLotNo.
            &ENDIF */

            FOR EACH w-ordl: DELETE w-ordl. END. /* each w-ordl */

            v-auto = YES.
            RUN oe/actrel.p (RECID(oe-rel),INPUT-OUTPUT iocPrompt).

            IF ediQty LE 0 THEN LEAVE.

          END. /* each oe-ordl */

          RELEASE oe-ordl.

          ASSIGN
         /* ediPONo = '' task 12031201 rstark */
            ediLineNo = 0
            ediPartNo = ''
            ediVendPartNo = ''
            ediQty = 0
         /* ediLotNo = '' task 12031201 rstark
            ediDate = ?
            ediNotes = '' */
            .
        END. /* rheem format */
        OTHERWISE NEXT.
      END CASE.

    END. /* repeat */
    INPUT STREAM ediIN CLOSE.
    OUTPUT STREAM ediOUT CLOSE.
    OS-COPY VALUE(ediDir + '/' + ENTRY(i,edifile))
            VALUE(ediDir + '/archive/' + ENTRY(i,edifile)).
  END. /* do i */

  DO i = 1 TO NUM-ENTRIES(ediFile):
    IF SEARCH(ediDir + '/archive/' + ENTRY(i,edifile)) NE ? THEN
    OS-DELETE VALUE(ediDir + '/' + ENTRY(i,edifile)).
  END. /* do i */

  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

