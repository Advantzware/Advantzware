&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: listobjs/rm-ibtag_.w

  Description: from SMART.W - Template for basic SmartObject

  Author: 
  Created: 

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
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

{methods/defines/hndlset.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartListObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-1 RECT-3 list-order begin_date ~
end_date begin_i-no end_i-no Begin_ven End_ven T1 T4 T7 T19 T2 T8 T6 T9 T3 ~
T12 T11 T22 T20 T13 T5 T16 T10 T14 T21 T17 T15 T18 
&Scoped-Define DISPLAYED-OBJECTS list-order begin_date end_date begin_i-no ~
end_i-no Begin_ven End_ven T1 T4 T7 T19 T2 T8 T6 T9 T3 T12 T11 T22 T20 T13 ~
T5 T16 T10 T14 T21 T17 T15 T18 F1 F-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define F1 F1 F-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(10)" 
     LABEL "From Item #" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Begin_ven AS CHARACTER FORMAT "X(10)":U 
     LABEL "From Vendor" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(10)" 
     LABEL "To Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE End_ven AS CHARACTER FORMAT "X(10)":U 
     LABEL "To Vendor" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE list-order AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Default", 1,
"Description", 2
     SIZE 106 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 1.43.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 3.81.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 5.

DEFINE VARIABLE T1 AS LOGICAL INITIAL yes 
     LABEL "Polyethylene" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T10 AS LOGICAL INITIAL yes 
     LABEL "Foil / Stamping" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T11 AS LOGICAL INITIAL yes 
     LABEL "Glue" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T12 AS LOGICAL INITIAL yes 
     LABEL "Ink" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T13 AS LOGICAL INITIAL yes 
     LABEL "Banding" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T14 AS LOGICAL INITIAL yes 
     LABEL "Laminate" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T15 AS LOGICAL INITIAL yes 
     LABEL "Misc Materials" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T16 AS LOGICAL INITIAL yes 
     LABEL "PVC / Paper" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T17 AS LOGICAL INITIAL yes 
     LABEL "Wrap for Rigid Boxes" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T18 AS LOGICAL INITIAL yes 
     LABEL "Stitching" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T19 AS LOGICAL INITIAL yes 
     LABEL "Tape" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T2 AS LOGICAL INITIAL yes 
     LABEL "Polyurethane" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T20 AS LOGICAL INITIAL yes 
     LABEL "Varnish and Coating" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T21 AS LOGICAL INITIAL yes 
     LABEL "Wax and Window" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T22 AS LOGICAL INITIAL yes 
     LABEL "Truck Trailer" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T3 AS LOGICAL INITIAL yes 
     LABEL "Expanded Polystyrene" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T4 AS LOGICAL INITIAL yes 
     LABEL "Polystyrene" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T5 AS LOGICAL INITIAL yes 
     LABEL "Wood" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T6 AS LOGICAL INITIAL yes 
     LABEL "Adders" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T7 AS LOGICAL INITIAL yes 
     LABEL "Board" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T8 AS LOGICAL INITIAL yes 
     LABEL "Cases / Bundles" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE T9 AS LOGICAL INITIAL yes 
     LABEL "Pallet / Bales" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     list-order AT ROW 1.24 COL 15 HELP
          "Select List Order" NO-LABEL
     begin_date AT ROW 2.91 COL 43 COLON-ALIGNED
     end_date AT ROW 2.91 COL 83 COLON-ALIGNED
     begin_i-no AT ROW 4.1 COL 43 COLON-ALIGNED HELP
          "Enter Beginning <Name>"
     end_i-no AT ROW 4.1 COL 83 COLON-ALIGNED HELP
          "Enter Ending <Name>"
     Begin_ven AT ROW 5.29 COL 43 COLON-ALIGNED
     End_ven AT ROW 5.29 COL 83 COLON-ALIGNED
     T1 AT ROW 7.19 COL 5
     T4 AT ROW 7.19 COL 39
     T7 AT ROW 7.19 COL 69
     T19 AT ROW 7.19 COL 95
     T2 AT ROW 7.91 COL 5
     T8 AT ROW 7.91 COL 39
     T6 AT ROW 7.91 COL 69
     T9 AT ROW 7.91 COL 95
     T3 AT ROW 8.62 COL 5
     T12 AT ROW 8.62 COL 39
     T11 AT ROW 8.62 COL 69
     T22 AT ROW 8.62 COL 95
     T20 AT ROW 9.33 COL 5
     T13 AT ROW 9.33 COL 39
     T5 AT ROW 9.33 COL 69
     T16 AT ROW 9.33 COL 95
     T10 AT ROW 10.05 COL 5
     T14 AT ROW 10.05 COL 39
     T21 AT ROW 10.05 COL 69
     T17 AT ROW 10.76 COL 5
     T15 AT ROW 10.76 COL 39
     T18 AT ROW 10.76 COL 69
     F1 AT ROW 4.33 COL 62 NO-LABEL
     F-2 AT ROW 4.33 COL 102 NO-LABEL
     "List Order:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 1.24 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.43 COL 2
     RECT-5 AT ROW 6.71 COL 1
     "Material Types:" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 6.48 COL 2
          FONT 6
     RECT-1 AT ROW 1 COL 1
     RECT-3 AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartListObject
   External Tables: ASI.item
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 13.38
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       Begin_ven:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       End_ven:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       list-order:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

ASSIGN 
       T1:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T10:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T11:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T12:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T13:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T14:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T15:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T16:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T17:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T18:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T19:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T2:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T20:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T21:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T22:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T3:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T4:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T5:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T6:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T7:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T8:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

ASSIGN 
       T9:PRIVATE-DATA IN FRAME F-Main     = 
                "save".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}
{methods/listobjs/listobjs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME list-order
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL list-order s-object
ON VALUE-CHANGED OF list-order IN FRAME F-Main
DO:
  RUN Get-Display-Values.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Create-List-Logic-Shell s-object 
PROCEDURE Create-List-Logic-Shell :
/*------------------------------------------------------------------------------
  Purpose:     Create List Logic Method
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lstlogic/lstshell.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame-Handle s-object 
PROCEDURE Frame-Handle :
/*------------------------------------------------------------------------------
  Purpose:     Called externally to get THIS-PROCEDURE's Frame handle
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/framehdl.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Show-Parameters s-object 
PROCEDURE Init-Show-Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Supply Show Parameter setting to calling procedure.
  Parameters:  OUTPUT show parameters
  Notes:       
------------------------------------------------------------------------------*/
  {methods/listobjs/initshow.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/listobjs/listinit.i}

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


