&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg/ordrupdt.w

  Description: update selected order fields from fg item

  Input Parameters: itemfg rowid, original part no

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.12.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipPartNo AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipRowID AS ROWID NO-UNDO.
DEFINE VARIABLE ipPartNo AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i new shared}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS iName partNo partDscr1 partDscr2 partDscr3 ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS iName partNo partDscr1 partDscr2 partDscr3 

/* Custom List Definitions                                              */
/* orderFields,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define orderFields iName partNo partDscr1 partDscr2 partDscr3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 5.24.

DEFINE VARIABLE iName AS LOGICAL INITIAL no 
     LABEL "Item Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE partDscr1 AS LOGICAL INITIAL no 
     LABEL "Part Description 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE partDscr2 AS LOGICAL INITIAL no 
     LABEL "Part Description 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE partDscr3 AS LOGICAL INITIAL no 
     LABEL "Part Description 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE partNo AS LOGICAL INITIAL no 
     LABEL "Part Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     iName AT ROW 2.19 COL 11 HELP
          "Select to Update Order's Item Name"
     partNo AT ROW 3.14 COL 11 HELP
          "Select to Update Order's Part Number"
     partDscr1 AT ROW 4.1 COL 11 HELP
          "Select to Update Order's Part Description 1"
     partDscr2 AT ROW 5.05 COL 11 HELP
          "Select to Update Order's Part Description 2"
     partDscr3 AT ROW 6 COL 11 HELP
          "Select to Update Order's Part Description 3" WIDGET-ID 2
     Btn_OK AT ROW 7.43 COL 15
     "Select Order Information to Update:" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 1.24 COL 3
     RECT-11 AT ROW 1.95 COL 2
     SPACE(0.00) SKIP(1.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Order"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX iName IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partDscr1 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partDscr2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partDscr3 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Order */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF BUFFER b-oe-ordl FOR oe-ordl.


  ASSIGN {&orderFields}.

  FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ ipRowID NO-ERROR.

  IF AVAILABLE itemfg THEN DO:
    IF ipPartNo NE itemfg.part-no AND partNo THEN
    FOR EACH oe-ordl WHERE oe-ordl.company EQ itemfg.company
                       AND oe-ordl.i-no EQ itemfg.i-no
                       AND oe-ordl.part-no EQ ipPartNo EXCLUSIVE-LOCK:
      oe-ordl.part-no = itemfg.part-no.
    END. /* each oe-ordl */
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ itemfg.company
          AND oe-ordl.i-no    EQ itemfg.i-no:
      FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL b-oe-ordl THEN DO:
        IF iName THEN b-oe-ordl.i-name = itemfg.i-name.
        IF partNo THEN b-oe-ordl.part-no = itemfg.part-no.
        IF partDscr1 THEN b-oe-ordl.part-dscr1 = itemfg.part-dscr1.
        IF partDscr2 THEN b-oe-ordl.part-dscr2 = itemfg.part-dscr2.
        IF partDscr3 THEN b-oe-ordl.part-dscr3 = itemfg.part-dscr3.
      END.
    END. /* each oe-ordl */
  END. /* if avail */
  RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY iName partNo partDscr1 partDscr2 partDscr3 
      WITH FRAME Dialog-Frame.
  ENABLE iName partNo partDscr1 partDscr2 partDscr3 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

