&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\d-machex2.w
  
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
DEFINE INPUT PARAMETER ip-def-route AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER op-farmout AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER op-valid-machines AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
{sys/inc/var.i SHARED}

{est/d-machex.i}
{est/d-machex2.i}
{ce/mach-lst.i}

def shared buffer xest for est.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mach-route mach

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-mach-route.form-no tt-mach-route.blank-no tt-mach-route.m-code mach.m-dscr tt-mach-route.reason   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-mach-route NO-LOCK, ~
                                   FIRST mach                             WHERE mach.company EQ cocode                               AND mach.m-code  EQ tt-mach-route.m-code                             NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-mach-route NO-LOCK, ~
                                   FIRST mach                             WHERE mach.company EQ cocode                               AND mach.m-code  EQ tt-mach-route.m-code                             NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-mach-route mach
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-mach-route
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 mach


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 btn-farmout Btn_valid 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-farmout 
     LABEL "Import Farmout Machine" 
     SIZE 25 BY 1.14.

DEFINE BUTTON Btn_valid AUTO-GO 
     LABEL "Import Valid Machines" 
     SIZE 24 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-mach-route, 
      mach SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-mach-route.form-no  LABEL "S"              FORMAT ">>9"
tt-mach-route.blank-no LABEL "B"              FORMAT ">>9"
tt-mach-route.m-code   LABEL "Machine" WIDTH 10  
mach.m-dscr          LABEL "Description"    FORMAT "x(30)"
tt-mach-route.reason   LABEL "Reason" FORMAT "x(80)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 134 BY 12.38
         BGCOLOR 8  FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1.24 COL 2
     btn-farmout AT ROW 14.33 COL 39 WIDGET-ID 2
     Btn_valid AT ROW 14.33 COL 68.2
     SPACE(44.99) SKIP(0.81)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Scheduled Machines".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mach-route NO-LOCK,
                            FIRST mach
                            WHERE mach.company EQ cocode
                              AND mach.m-code  EQ tt-mach-route.m-code
                            NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Scheduled Machines */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON ROW-DISPLAY OF BROWSE-2 IN FRAME D-Dialog
DO:
   IF tt-mach-route.reason NE "Valid Machine" THEN
      ASSIGN
         tt-mach-route.form-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
         tt-mach-route.blank-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12
         tt-mach-route.m-code:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 12      
         mach.m-dscr:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12         
         tt-mach-route.reason:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12.
   ELSE
      ASSIGN
         tt-mach-route.form-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 10
         tt-mach-route.blank-no:BGCOLOR IN BROWSE {&BROWSE-NAME} = 10
         tt-mach-route.m-code:BGCOLOR IN BROWSE {&BROWSE-NAME}  = 10      
         mach.m-dscr:BGCOLOR IN BROWSE {&BROWSE-NAME} = 10         
         tt-mach-route.reason:BGCOLOR IN BROWSE {&BROWSE-NAME} = 10.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-farmout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-farmout D-Dialog
ON CHOOSE OF btn-farmout IN FRAME D-Dialog /* Import Farmout Machine */
DO:
  op-farmout = YES.
  APPLY "WINDOW-CLOSE" TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_valid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_valid D-Dialog
ON CHOOSE OF Btn_valid IN FRAME D-Dialog /* Import Valid Machines */
DO:
   op-valid-machines = YES.
   APPLY "CLOSE" TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

  IF NOT CAN-FIND(FIRST tt-mach-exc) THEN
  DO:
     op-valid-machines = YES.
     RETURN.
  END.

  FOR EACH tt-mach-exc,
      FIRST tt-mach-route WHERE
            tt-mach-route.m-code EQ tt-mach-exc.m-code AND
            tt-mach-route.form-no EQ tt-mach-exc.form-no AND
            tt-mach-route.blank-no EQ tt-mach-exc.blank-no:
      
      ASSIGN
         tt-mach-route.reason = tt-mach-exc.reason
         tt-mach-route.dept = tt-mach-exc.dept
         tt-mach-route.defr = tt-mach-exc.defr.
  END.

  IF NOT CAN-FIND(FIRST tt-mach-route WHERE
     tt-mach-route.reason NE "") THEN
     DO:
        /*op-valid-machines = YES.*/
        EMPTY TEMP-TABLE tt-mach-route.
        RETURN.
     END.

  FOR EACH tt-mach-route WHERE
      tt-mach-route.reason EQ "":
      tt-mach-route.reason = "Valid Machine".
  END.
  
  {src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE BROWSE-2 btn-farmout Btn_valid 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-mach-route"}
  {src/adm/template/snd-list.i "mach"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

