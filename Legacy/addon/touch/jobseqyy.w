&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: touch/jobseq.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

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

{touch/touchdef.i}

&Scoped-define BUTTON-INCLUDE JOBSEQ

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Button-2 Btn_Button-6 Btn_First Btn_Last ~
Btn_Close Btn_Button-3 Btn_Button-7 Btn_ticket Btn_boximg Btn_boximg-2 ~
Btn_Button-4 Btn_Button-8 Btn_Button-10 Btn_Button-5 Btn_Button-9 ~
Btn_Page_Up Btn_Page_Down Btn_Button-1 Btn_Cancel RECT-1 RECT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Button-2 Btn_Button-6 Btn_Button-3 Btn_Button-7 ~
Btn_Button-4 Btn_Button-8 Btn_Button-10 Btn_Button-5 Btn_Button-9 ~
Btn_Button-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_boximg 
     LABEL "Box Image" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_boximg-2 
     LABEL "Item Image" 
     SIZE 13 BY 2.38.

DEFINE BUTTON Btn_Button-1 
     LABEL "BUTTON 1" 
     SIZE 81 BY 2.38.

DEFINE BUTTON Btn_Button-10 
     LABEL "BUTTON 10" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-2 
     LABEL "BUTTON 2" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-3 
     LABEL "BUTTON 3" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-4 
     LABEL "BUTTON 4" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-5 
     LABEL "BUTTON 5" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-6 
     LABEL "BUTTON 6" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-7 
     LABEL "BUTTON 7" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-8 
     LABEL "BUTTON 8" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Button-9 
     LABEL "BUTTON 9" 
     SIZE 40 BY 2.38
     FONT 4.

DEFINE BUTTON Btn_Cancel 
     LABEL "CANCEL" 
     SIZE 40 BY 2.38.

DEFINE BUTTON Btn_Close 
     IMAGE-UP FILE "images\exit-au":U
     LABEL "CLOSE" 
     SIZE 10 BY 2.38.

DEFINE BUTTON Btn_First 
     LABEL "FIRST CODE" 
     SIZE 16 BY 2.38.

DEFINE BUTTON Btn_Last 
     LABEL "LAST CODE" 
     SIZE 14 BY 2.38.

DEFINE BUTTON Btn_Page_Down 
     IMAGE-UP FILE "images\pagedown":U
     LABEL "Page Down" 
     SIZE 20 BY 2.38.

DEFINE BUTTON Btn_Page_Up 
     IMAGE-UP FILE "images\pageup":U
     LABEL "Page Up" 
     SIZE 20 BY 2.38.

DEFINE BUTTON Btn_ticket 
     LABEL "Job Card" 
     SIZE 14 BY 2.38.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 12.95.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 2.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Button-2 AT ROW 1.24 COL 2
     Btn_Button-6 AT ROW 1.24 COL 43
     Btn_First AT ROW 1.24 COL 84
     Btn_Last AT ROW 1.24 COL 100
     Btn_Close AT ROW 1.24 COL 114
     Btn_Button-3 AT ROW 3.62 COL 2
     Btn_Button-7 AT ROW 3.62 COL 43
     Btn_ticket AT ROW 3.62 COL 84
     Btn_boximg AT ROW 3.62 COL 98
     Btn_boximg-2 AT ROW 3.62 COL 111
     Btn_Button-4 AT ROW 6 COL 2
     Btn_Button-8 AT ROW 6 COL 43
     Btn_Button-10 AT ROW 6 COL 84
     Btn_Button-5 AT ROW 8.38 COL 2
     Btn_Button-9 AT ROW 8.38 COL 43
     Btn_Page_Up AT ROW 8.38 COL 84
     Btn_Page_Down AT ROW 8.38 COL 104
     Btn_Button-1 AT ROW 11.24 COL 2
     Btn_Cancel AT ROW 11.24 COL 84
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 11 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Button-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-10 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-3 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-4 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-5 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-6 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-7 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-8 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Button-9 IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_boximg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_boximg s-object
ON CHOOSE OF Btn_boximg IN FRAME F-Main /* Box Image */
DO:
  button_item = 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_boximg-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_boximg-2 s-object
ON CHOOSE OF Btn_boximg-2 IN FRAME F-Main /* Item Image */
DO:
  button_item = 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-1 s-object
ON CHOOSE OF Btn_Button-1 IN FRAME F-Main /* BUTTON 1 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-10 s-object
ON CHOOSE OF Btn_Button-10 IN FRAME F-Main /* BUTTON 10 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-2 s-object
ON CHOOSE OF Btn_Button-2 IN FRAME F-Main /* BUTTON 2 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-3 s-object
ON CHOOSE OF Btn_Button-3 IN FRAME F-Main /* BUTTON 3 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-4 s-object
ON CHOOSE OF Btn_Button-4 IN FRAME F-Main /* BUTTON 4 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-5 s-object
ON CHOOSE OF Btn_Button-5 IN FRAME F-Main /* BUTTON 5 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-6 s-object
ON CHOOSE OF Btn_Button-6 IN FRAME F-Main /* BUTTON 6 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-7 s-object
ON CHOOSE OF Btn_Button-7 IN FRAME F-Main /* BUTTON 7 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-8 s-object
ON CHOOSE OF Btn_Button-8 IN FRAME F-Main /* BUTTON 8 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Button-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Button-9 s-object
ON CHOOSE OF Btn_Button-9 IN FRAME F-Main /* BUTTON 9 */
DO:
  {touch/buttons.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel s-object
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* CANCEL */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(9)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* CLOSE */
DO:
  {methods/run_link.i "CONTAINER" "Change_Page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_First s-object
ON CHOOSE OF Btn_First IN FRAME F-Main /* FIRST CODE */
DO:
  button_item = 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Last s-object
ON CHOOSE OF Btn_Last IN FRAME F-Main /* LAST CODE */
DO:
  button_item = NUM-ENTRIES(itemlist,'@') + 1.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Down s-object
ON CHOOSE OF Btn_Page_Down IN FRAME F-Main /* Page Down */
DO:
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Page_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Page_Up s-object
ON CHOOSE OF Btn_Page_Up IN FRAME F-Main /* Page Up */
DO:
  button_item = button_item - 20.
  RUN Button_Labels (INPUT-OUTPUT button_item).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ticket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ticket s-object
ON CHOOSE OF Btn_ticket IN FRAME F-Main /* Job Card */
DO:
    RUN jcrep/r-ticket.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Button_Labels s-object 
PROCEDURE Button_Labels :
/*------------------------------------------------------------------------------
  Purpose:     place values on button labels
  Parameters:  Input current button item
  Notes:       
------------------------------------------------------------------------------*/
  {touch/btnlabel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Job_Sequence s-object 
PROCEDURE Get_Job_Sequence :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with next job sequences
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define SEQUENCE-TYPE JOB

  {touch/jobseq.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Machine_Sequence s-object 
PROCEDURE Get_Machine_Sequence :
/*------------------------------------------------------------------------------
  Purpose:     populate selection list with next machine sequences
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &Scoped-define SEQUENCE-TYPE MACHINE

  {touch/jobseq.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

