&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES prod
&Scoped-define FIRST-EXTERNAL-TABLE prod


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prod.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prod.prolin prod.dscr prod.wip-mat ~
prod.wip-mat-dscr prod.wip-lab prod.wip-lab-dscr prod.wip-vo ~
prod.wip-vo-dscr prod.wip-fo prod.wip-fo-dscr prod.fg-mat prod.fg-mat-dscr ~
prod.fg-lab prod.fg-lab-dscr prod.fg-vo prod.fg-vo-dscr prod.fg-fo ~
prod.fg-fo-dscr 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}prolin ~{&FP2}prolin ~{&FP3}~
 ~{&FP1}dscr ~{&FP2}dscr ~{&FP3}~
 ~{&FP1}wip-mat ~{&FP2}wip-mat ~{&FP3}~
 ~{&FP1}wip-mat-dscr ~{&FP2}wip-mat-dscr ~{&FP3}~
 ~{&FP1}wip-lab ~{&FP2}wip-lab ~{&FP3}~
 ~{&FP1}wip-lab-dscr ~{&FP2}wip-lab-dscr ~{&FP3}~
 ~{&FP1}wip-vo ~{&FP2}wip-vo ~{&FP3}~
 ~{&FP1}wip-vo-dscr ~{&FP2}wip-vo-dscr ~{&FP3}~
 ~{&FP1}wip-fo ~{&FP2}wip-fo ~{&FP3}~
 ~{&FP1}wip-fo-dscr ~{&FP2}wip-fo-dscr ~{&FP3}~
 ~{&FP1}fg-mat ~{&FP2}fg-mat ~{&FP3}~
 ~{&FP1}fg-mat-dscr ~{&FP2}fg-mat-dscr ~{&FP3}~
 ~{&FP1}fg-lab ~{&FP2}fg-lab ~{&FP3}~
 ~{&FP1}fg-lab-dscr ~{&FP2}fg-lab-dscr ~{&FP3}~
 ~{&FP1}fg-vo ~{&FP2}fg-vo ~{&FP3}~
 ~{&FP1}fg-vo-dscr ~{&FP2}fg-vo-dscr ~{&FP3}~
 ~{&FP1}fg-fo ~{&FP2}fg-fo ~{&FP3}~
 ~{&FP1}fg-fo-dscr ~{&FP2}fg-fo-dscr ~{&FP3}
&Scoped-define ENABLED-TABLES prod
&Scoped-define FIRST-ENABLED-TABLE prod
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS prod.prolin prod.dscr prod.wip-mat ~
prod.wip-mat-dscr prod.wip-lab prod.wip-lab-dscr prod.wip-vo ~
prod.wip-vo-dscr prod.wip-fo prod.wip-fo-dscr prod.fg-mat prod.fg-mat-dscr ~
prod.fg-lab prod.fg-lab-dscr prod.fg-vo prod.fg-vo-dscr prod.fg-fo ~
prod.fg-fo-dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Page 2" 
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     prod.prolin AT ROW 1.71 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     prod.dscr AT ROW 1.71 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.wip-mat AT ROW 4.33 COL 30 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.wip-mat-dscr AT ROW 4.33 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.wip-lab AT ROW 5.29 COL 30 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.wip-lab-dscr AT ROW 5.33 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.wip-vo AT ROW 6.24 COL 30 COLON-ALIGNED
          LABEL "Variable Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.wip-vo-dscr AT ROW 6.33 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.wip-fo AT ROW 7.19 COL 30 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.wip-fo-dscr AT ROW 7.33 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.fg-mat AT ROW 9.57 COL 30 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.fg-mat-dscr AT ROW 9.57 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.fg-lab AT ROW 10.52 COL 30 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.fg-lab-dscr AT ROW 10.57 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.fg-vo AT ROW 11.48 COL 30 COLON-ALIGNED
          LABEL "Variable Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.fg-vo-dscr AT ROW 11.57 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.fg-fo AT ROW 12.43 COL 30 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.fg-fo-dscr AT ROW 12.57 COL 64 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     BUTTON-1 AT ROW 16.48 COL 125
     RECT-1 AT ROW 1 COL 1
     "Finished Goods" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 8.86 COL 9
          FGCOLOR 9 FONT 5
     "Work In Process" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 3.38 COL 11
          FGCOLOR 9 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.prod
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN prod.fg-fo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.fg-lab IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.fg-mat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.fg-vo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.wip-fo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.wip-lab IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.wip-mat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.wip-vo IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Page 2 */
DO:
   run fg/d-proln.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects V-table-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "prod"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prod"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "prod"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


