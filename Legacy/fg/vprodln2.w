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
&Scoped-Define ENABLED-FIELDS prod.prolin prod.dscr prod.aa-mat prod.aa-lab ~
prod.aa-vo prod.aa-fo prod.cgs-mat prod.cgs-mu prod.cgs-dl prod.cgs-dlv ~
prod.cgs-vo prod.cgs-vov prod.cgs-fo prod.cgs-fov 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}prolin ~{&FP2}prolin ~{&FP3}~
 ~{&FP1}dscr ~{&FP2}dscr ~{&FP3}~
 ~{&FP1}aa-mat ~{&FP2}aa-mat ~{&FP3}~
 ~{&FP1}aa-lab ~{&FP2}aa-lab ~{&FP3}~
 ~{&FP1}aa-vo ~{&FP2}aa-vo ~{&FP3}~
 ~{&FP1}aa-fo ~{&FP2}aa-fo ~{&FP3}~
 ~{&FP1}cgs-mat ~{&FP2}cgs-mat ~{&FP3}~
 ~{&FP1}cgs-mu ~{&FP2}cgs-mu ~{&FP3}~
 ~{&FP1}cgs-dl ~{&FP2}cgs-dl ~{&FP3}~
 ~{&FP1}cgs-dlv ~{&FP2}cgs-dlv ~{&FP3}~
 ~{&FP1}cgs-vo ~{&FP2}cgs-vo ~{&FP3}~
 ~{&FP1}cgs-vov ~{&FP2}cgs-vov ~{&FP3}~
 ~{&FP1}cgs-fo ~{&FP2}cgs-fo ~{&FP3}~
 ~{&FP1}cgs-fov ~{&FP2}cgs-fov ~{&FP3}
&Scoped-define ENABLED-TABLES prod
&Scoped-define FIRST-ENABLED-TABLE prod
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS prod.prolin prod.dscr prod.aa-mat ~
prod.aa-lab prod.aa-vo prod.aa-fo prod.cgs-mat prod.cgs-mu prod.cgs-dl ~
prod.cgs-dlv prod.cgs-vo prod.cgs-vov prod.cgs-fo prod.cgs-fov 

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
company||y|ASI.prod.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 144 BY 15.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     prod.prolin AT ROW 1.71 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     prod.dscr AT ROW 1.71 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     prod.aa-mat AT ROW 3.38 COL 32 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.aa-lab AT ROW 4.33 COL 32 COLON-ALIGNED
          LABEL "Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.aa-vo AT ROW 5.29 COL 32 COLON-ALIGNED
          LABEL "Variable Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.aa-fo AT ROW 6.24 COL 32 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-mat AT ROW 8.62 COL 38 COLON-ALIGNED
          LABEL "Material"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-mu AT ROW 9.57 COL 38 COLON-ALIGNED
          LABEL "Materials Usage Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-dl AT ROW 10.52 COL 38 COLON-ALIGNED
          LABEL "Direct Labor"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-dlv AT ROW 11.48 COL 37 COLON-ALIGNED
          LABEL "Direct Labor Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-vo AT ROW 12.43 COL 37 COLON-ALIGNED
          LABEL "Variable Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-vov AT ROW 13.38 COL 42 COLON-ALIGNED
          LABEL "Variable Overhead Eff. Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-fo AT ROW 14.33 COL 38 COLON-ALIGNED
          LABEL "Fixed Overhead"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prod.cgs-fov AT ROW 15.52 COL 38 COLON-ALIGNED
          LABEL "Fixed Overhead Eff. Variance"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     "Actual Applied" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 2.67 COL 11
          FGCOLOR 9 FONT 5
     "Cost Of Goods Sold" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 7.67 COL 16
          FGCOLOR 9 FONT 5
     RECT-1 AT ROW 1 COL 1
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

/* SETTINGS FOR FILL-IN prod.aa-fo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.aa-lab IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.aa-mat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.aa-vo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-dl IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-dlv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-fo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-fov IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-mat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-mu IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-vo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prod.cgs-vov IN FRAME F-Main
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "prod" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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


