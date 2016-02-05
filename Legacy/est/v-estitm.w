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
&Scoped-define EXTERNAL-TABLES eb
&Scoped-define FIRST-EXTERNAL-TABLE eb


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.stock-no eb.plate-no eb.die-no eb.cad-no ~
eb.upc-no eb.spc-no eb.sty-lock eb.len eb.wid eb.dep eb.dust eb.lock ~
eb.fpanel eb.k-len eb.k-wid eb.adhesive eb.tuck eb.gluelap eb.lin-in ~
eb.t-len eb.t-wid eb.t-sqin 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}stock-no ~{&FP2}stock-no ~{&FP3}~
 ~{&FP1}plate-no ~{&FP2}plate-no ~{&FP3}~
 ~{&FP1}die-no ~{&FP2}die-no ~{&FP3}~
 ~{&FP1}cad-no ~{&FP2}cad-no ~{&FP3}~
 ~{&FP1}upc-no ~{&FP2}upc-no ~{&FP3}~
 ~{&FP1}spc-no ~{&FP2}spc-no ~{&FP3}~
 ~{&FP1}sty-lock ~{&FP2}sty-lock ~{&FP3}~
 ~{&FP1}len ~{&FP2}len ~{&FP3}~
 ~{&FP1}wid ~{&FP2}wid ~{&FP3}~
 ~{&FP1}dep ~{&FP2}dep ~{&FP3}~
 ~{&FP1}dust ~{&FP2}dust ~{&FP3}~
 ~{&FP1}lock ~{&FP2}lock ~{&FP3}~
 ~{&FP1}fpanel ~{&FP2}fpanel ~{&FP3}~
 ~{&FP1}k-len ~{&FP2}k-len ~{&FP3}~
 ~{&FP1}k-wid ~{&FP2}k-wid ~{&FP3}~
 ~{&FP1}adhesive ~{&FP2}adhesive ~{&FP3}~
 ~{&FP1}tuck ~{&FP2}tuck ~{&FP3}~
 ~{&FP1}gluelap ~{&FP2}gluelap ~{&FP3}~
 ~{&FP1}lin-in ~{&FP2}lin-in ~{&FP3}~
 ~{&FP1}t-len ~{&FP2}t-len ~{&FP3}~
 ~{&FP1}t-wid ~{&FP2}t-wid ~{&FP3}~
 ~{&FP1}t-sqin ~{&FP2}t-sqin ~{&FP3}
&Scoped-define ENABLED-TABLES eb
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-11 
&Scoped-Define DISPLAYED-FIELDS eb.stock-no eb.plate-no eb.die-no eb.cad-no ~
eb.upc-no eb.spc-no eb.sty-lock eb.len eb.wid eb.dep eb.dust eb.lock ~
eb.fpanel eb.k-len eb.k-wid eb.adhesive eb.tuck eb.gluelap eb.lin-in ~
eb.t-len eb.t-wid eb.t-sqin 

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
DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 121 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     eb.stock-no AT ROW 1.48 COL 19 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.plate-no AT ROW 2.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.die-no AT ROW 3.38 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.cad-no AT ROW 4.33 COL 40 RIGHT-ALIGNED
          LABEL "CAD/Sample#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.upc-no AT ROW 5.29 COL 19 COLON-ALIGNED
          LABEL "UPC#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.spc-no AT ROW 6.24 COL 19 COLON-ALIGNED
          LABEL "SPC/QC Code"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     eb.sty-lock AT ROW 7.43 COL 99 COLON-ALIGNED
          LABEL "Override?"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     eb.len AT ROW 8.38 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.wid AT ROW 8.38 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dep AT ROW 8.38 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.dust AT ROW 9.33 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lock AT ROW 9.33 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.fpanel AT ROW 9.33 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-len AT ROW 10.29 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.k-wid AT ROW 10.29 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.adhesive AT ROW 10.29 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     eb.tuck AT ROW 11.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.gluelap AT ROW 11.24 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.lin-in AT ROW 11.24 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     eb.t-len AT ROW 12.14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-wid AT ROW 12.14 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     eb.t-sqin AT ROW 12.14 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     RECT-11 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.eb
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

/* SETTINGS FOR FILL-IN eb.cad-no IN FRAME F-Main
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN eb.spc-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.stock-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.sty-lock IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.upc-no IN FRAME F-Main
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
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "eb"}

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
  {src/adm/template/snd-list.i "eb"}

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


