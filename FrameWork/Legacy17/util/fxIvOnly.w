&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

{src/adm2/widgetprto.i}

  {custom/globdefs.i}
{custom/gcompany.i}
{sys/inc/var.i NEW SHARED}
  RUN custom/getcomp.p.
{sys/inc/varasgn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiOrder btCorrect btExit 
&Scoped-Define DISPLAYED-OBJECTS fiOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCorrect 
     LABEL "Correct Release" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btExit 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiOrder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fiOrder AT ROW 4.33 COL 15 COLON-ALIGNED WIDGET-ID 2
     btCorrect AT ROW 4.33 COL 40 WIDGET-ID 6
     btExit AT ROW 8.14 COL 31 WIDGET-ID 8
     "Enter the order number associated with the missing invoice-only release." VIEW-AS TEXT
          SIZE 103 BY .95 AT ROW 2.43 COL 8 WIDGET-ID 4
          FONT 0
     RECT-1 AT ROW 1.71 COL 4 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.4 BY 9.81 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Correct Invoice Only Release"
         HEIGHT             = 9.81
         WIDTH              = 119.4
         MAX-HEIGHT         = 49.1
         MAX-WIDTH          = 324.4
         VIRTUAL-HEIGHT     = 49.1
         VIRTUAL-WIDTH      = 324.4
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartWindowCues" wWin _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartWindow,ab,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Correct Invoice Only Release */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Correct Invoice Only Release */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCorrect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCorrect wWin
ON CHOOSE OF btCorrect IN FRAME fMain /* Correct Release */
DO:


 DEFINE VARIABLE iOrder AS INTEGER     NO-UNDO.
 DEFINE BUFFER bf-oe-boll FOR oe-boll.
 DEFINE BUFFER bf-oe-relh FOR oe-relh.

 DEF VAR save-row AS ROWID.
 DEF VAR save-release LIKE oe-relh.release#.


 iOrder = integer(fiOrder:SCREEN-VALUE).
 FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
   AND oe-ord.ord-no EQ iOrder NO-LOCK NO-ERROR.
 IF NOT AVAIL oe-ord  THEN DO:
   MESSAGE "Order not found" VIEW-AS ALERT-BOX .
   RETURN NO-APPLY.
 END.

 save-row = ?.
 FOR EACH oe-boll WHERE oe-boll.company EQ cocode
   AND oe-boll.ord-no EQ iOrder 
   AND oe-boll.posted = NO
   NO-LOCK , FIRST oe-bolh WHERE oe-bolh.company EQ cocode
   AND oe-bolh.bol-no EQ oe-boll.bol-no
   AND oe-bolh.posted = YES NO-LOCK.
   save-row = ROWID(oe-boll).
   LEAVE.
 END.

 FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ save-row NO-LOCK NO-ERROR.
 
 IF AVAIL oe-boll THEN DO:

    FIND FIRST oe-rell WHERE oe-rell.company EQ cocode
      AND oe-rell.ord-no EQ oe-boll.ord-no
      AND oe-rell.i-no EQ oe-boll.i-no
      AND oe-rell.s-code EQ "I"
    NO-LOCK NO-ERROR.
   
 END.

 IF AVAIL oe-boll AND AVAIL oe-rell THEN DO:

   FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ ROWID(oe-boll) 
     EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL bf-oe-boll THEN
     DELETE bf-oe-boll.

   FIND FIRST bf-oe-relh OF oe-rell EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL bf-oe-relh THEN DO:
     save-release = bf-oe-relh.release#.
     bf-oe-relh.posted = NO.
     MESSAGE "Release # " + STRING(save-release) + " is now ready to post."
       VIEW-AS ALERT-BOX.
   END.

 END.
 ELSE DO:

   MESSAGE "Nothing found to be corrected."
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN NO-APPLY.

 END.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wWin
ON CHOOSE OF btExit IN FRAME fMain /* Exit */
DO:
 RUN exitObject.
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
  DISPLAY fiOrder 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 fiOrder btCorrect btExit 
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

