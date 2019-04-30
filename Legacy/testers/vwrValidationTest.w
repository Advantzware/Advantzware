&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"testers/sdoValidationTest.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTable 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "testers/sdoValidationTest.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.testType RowObject.testName ~
RowObject.testDesc RowObject.testTable RowObject.testField ~
RowObject.testRequired RowObject.testProcedure RowObject.resultType ~
RowObject.defaultResult RowObject.defaultResultDesc RowObject.passResult ~
RowObject.passResultDesc RowObject.failResult RowObject.failResultDesc 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.testType RowObject.testName ~
RowObject.testDesc RowObject.testTable RowObject.testField ~
RowObject.testRequired RowObject.testProcedure RowObject.resultType ~
RowObject.defaultResult RowObject.defaultResultDesc RowObject.passResult ~
RowObject.passResultDesc RowObject.failResult RowObject.failResultDesc 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.testType AT ROW 1.71 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     RowObject.testName AT ROW 2.91 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.testDesc AT ROW 4.1 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     RowObject.testTable AT ROW 5.76 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.testField AT ROW 6.95 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.testRequired AT ROW 8.38 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     RowObject.testProcedure AT ROW 9.81 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     RowObject.resultType AT ROW 11.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     RowObject.defaultResult AT ROW 12.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     RowObject.defaultResultDesc AT ROW 13.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     RowObject.passResult AT ROW 14.57 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     RowObject.passResultDesc AT ROW 15.57 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     RowObject.failResult AT ROW 16.71 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     RowObject.failResultDesc AT ROW 17.71 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "testers\sdoValidationTest.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {testers/sdoValidationTest.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTable ASSIGN
         HEIGHT             = 19.48
         WIDTH              = 88.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTable 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTable
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTable 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTable  _DEFAULT-DISABLE
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

