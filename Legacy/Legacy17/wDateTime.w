&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
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
&Scoped-Define ENABLED-OBJECTS BUTTON-8 BUTTON-7 FILL-IN-4 BUTTON-9 ~
BUTTON-10 FILL-IN-5 FILL-IN-6 FILL-IN-7 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-4 FILL-IN-5 FILL-IN-6 FILL-IN-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-10 
     LABEL "Set Time" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-7 
     LABEL "Set Date" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-8 
     LABEL "Read Date" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-9 
     LABEL "Read Time" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE FILL-IN-4 AS DATE FORMAT "99/99/99":U INITIAL 01/01/01 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-8 AT ROW 1 COL 1
     BUTTON-7 AT ROW 2.43 COL 1
     FILL-IN-4 AT ROW 2.43 COL 13 COLON-ALIGNED NO-LABEL
     BUTTON-9 AT ROW 3.86 COL 1
     BUTTON-10 AT ROW 5.29 COL 1
     FILL-IN-5 AT ROW 5.29 COL 13 COLON-ALIGNED NO-LABEL
     FILL-IN-6 AT ROW 5.29 COL 20 COLON-ALIGNED NO-LABEL
     FILL-IN-7 AT ROW 5.29 COL 27 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 41 BY 5.52.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 5.52
         WIDTH              = 41
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 1
       COLUMN          = 15
       HEIGHT          = 1.05
       WIDTH           = 27
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 3.86
       COLUMN          = 15
       HEIGHT          = .95
       WIDTH           = 26
       HIDDEN          = no
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {20DD1B9E-87C4-11D1-8BE3-0000F8754DA1} type: DTPicker */
      CtrlFrame:MOVE-AFTER(BUTTON-8:HANDLE IN FRAME fMain).
      CtrlFrame-2:MOVE-AFTER(BUTTON-9:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 wWin
ON CHOOSE OF BUTTON-10 IN FRAME fMain /* Set Time */
DO:
 
 
   chCtrlFrame-2:DTPicker:hour() = INT(FILL-IN-5:SCREEN-VALUE).
   chCtrlFrame-2:DTPicker:minute() = INT(FILL-IN-6:SCREEN-VALUE).
   chCtrlFrame-2:DTPicker:second() = INT(FILL-IN-7:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME fMain /* Set Date */
DO:
  chCtrlframe:dtpicker:FORMAT() = 1.
   chCtrlframe:dtpicker:VALUE() = DATE(FILL-IN-4:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON CHOOSE OF BUTTON-8 IN FRAME fMain /* Read Date */
DO:
  
MESSAGE chCtrlframe:dtpicker:VALUE()
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME fMain /* Read Time */
DO:
  MESSAGE "HOUR " + STRING(chCtrlFrame-2:DTPicker:hour()) SKIP
          "MINUTE " + STRING(chCtrlFrame-2:DTPicker:minute()) SKIP
          "SECOND " + STRING(chCtrlFrame-2:DTPicker:second())
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Click
PROCEDURE CtrlFrame.DTPicker.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

IF chctrlframe:dtpicker:VALUE = ? THEN
DO:
  chCtrlframe:dtpicker:FORMAT() = 3.
   chCtrlframe:dtpicker:CustomFORMAT() = "  /  /".
END.
ELSE
DO:
  chCtrlframe:dtpicker:FORMAT() = 1.
   chCtrlframe:dtpicker:VALUE() = DATE(FILL-IN-4:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.CloseUp
PROCEDURE CtrlFrame.DTPicker.CloseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  chCtrlframe:dtpicker:FORMAT() = 1.

END PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "wDateTime.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wDateTime.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY FILL-IN-4 FILL-IN-5 FILL-IN-6 FILL-IN-7 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-8 BUTTON-7 FILL-IN-4 BUTTON-9 BUTTON-10 FILL-IN-5 FILL-IN-6 
         FILL-IN-7 
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  FILL-IN-4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

