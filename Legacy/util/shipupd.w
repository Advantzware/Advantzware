&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters: 

  Output Parameters: <none>

  Author: Ron Stark

  Created: 02.06.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE OUTPUT PARAMETER oplRelease AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplBol AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplOrder AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplInv AS LOGICAL NO-UNDO.
&ELSE
DEFINE VARIABLE oplRelease AS LOGICAL NO-UNDO.
DEFINE VARIABLE oplBol     AS LOGICAL NO-UNDO.
DEFINE VARIABLE oplOrder   AS LOGICAL NO-UNDO.
DEFINE VARIABLE oplInv     AS LOGICAL NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}

DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

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
&Scoped-Define ENABLED-OBJECTS tdPostRel tdPostBol tdCloseOrder tdPostedInv ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS tdPostRel tdPostBol tdCloseOrder ~
tdPostedInv 

/* Custom List Definitions                                              */
/* orderFields,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define orderFields tdPostRel tdPostBol tdCloseOrder tdPostedInv 

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

DEFINE VARIABLE tdCloseOrder AS LOGICAL INITIAL NO 
    LABEL "Include Close Order" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tdPostBol    AS LOGICAL INITIAL NO 
    LABEL "Include Posted BOL" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tdPostedInv  AS LOGICAL INITIAL NO 
    LABEL "Include Posted Invoice" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tdPostRel    AS LOGICAL INITIAL NO 
    LABEL "Include Posted Releases" 
    VIEW-AS TOGGLE-BOX
    SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    tdPostRel AT ROW 2.19 COL 11 HELP
    "Select to Update Posted Release"
    tdPostBol AT ROW 3.14 COL 11 HELP
    "Select to Update Posted Bol"
    tdCloseOrder AT ROW 4.1 COL 11 HELP
    "Select to Update close Order"
    tdPostedInv AT ROW 5.05 COL 11 HELP
    "Select to Update Posted Invoice"
    Btn_OK AT ROW 7.43 COL 15
    "Select Ship To Information to Update:" VIEW-AS TEXT
    SIZE 35 BY .62 AT ROW 1.24 COL 3
    RECT-11 AT ROW 1.95 COL 2
    SPACE(0.00) SKIP(1.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Update ShipTo"
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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tdCloseOrder IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tdPostBol IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tdPostedInv IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tdPostRel IN FRAME Dialog-Frame
   1                                                                    */
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
        ASSIGN {&orderFields}.
        ASSIGN
            oplRelease = tdPostRel
            oplBol     = tdPostBol
            oplOrder   = tdCloseOrder
            oplInv     = tdPostedInv .

        RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).

  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


ASSIGN
    cocode = g_company
    locode = g_loc.

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
    DISPLAY tdPostRel tdPostBol tdCloseOrder tdPostedInv 
        WITH FRAME Dialog-Frame.
    ENABLE tdPostRel tdPostBol tdCloseOrder tdPostedInv Btn_OK 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

