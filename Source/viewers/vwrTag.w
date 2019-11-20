&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"sdo/sdoTag.i"}.



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

DEF VAR cTableName AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "sdo/sdoTag.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.rec_key RowObject.linkRecKey ~
RowObject.createDT RowObject.createUser RowObject.updateDT ~
RowObject.updateUser 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS fiTableName 
&Scoped-Define DISPLAYED-FIELDS RowObject.rec_key RowObject.linkTable ~
RowObject.linkRecKey RowObject.tagType RowObject.groupCode ~
RowObject.statusCode RowObject.ownerUser RowObject.description ~
RowObject.Note1 RowObject.Note2 RowObject.Note3 RowObject.Note4 ~
RowObject.Note5 RowObject.createDT RowObject.createUser RowObject.updateDT ~
RowObject.updateUser 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS fiTableName 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiTableName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Linked to" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.rec_key AT ROW 1.24 COL 23 COLON-ALIGNED
          LABEL "RecKey (this tag)"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     fiTableName AT ROW 2.43 COL 23 COLON-ALIGNED
     RowObject.linkTable AT ROW 2.43 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1 NO-TAB-STOP 
     RowObject.linkRecKey AT ROW 3.62 COL 23 COLON-ALIGNED
          LABEL "Using RecKey"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     RowObject.tagType AT ROW 4.81 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.2 BY 1 NO-TAB-STOP 
     RowObject.groupCode AT ROW 4.81 COL 11 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1 NO-TAB-STOP 
     RowObject.statusCode AT ROW 4.81 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1 NO-TAB-STOP 
     RowObject.ownerUser AT ROW 4.81 COL 21 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1 NO-TAB-STOP 
     RowObject.description AT ROW 4.81 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.Note1 AT ROW 4.81 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1 NO-TAB-STOP 
     RowObject.Note2 AT ROW 4.81 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.Note3 AT ROW 4.81 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.Note4 AT ROW 4.81 COL 46 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1 NO-TAB-STOP 
     RowObject.Note5 AT ROW 4.81 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.createDT AT ROW 4.81 COL 58 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.createUser AT ROW 4.81 COL 63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.updateDT AT ROW 4.81 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
     RowObject.updateUser AT ROW 4.81 COL 73 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/sdoTag.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {sdo/sdoTag.i}
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
         HEIGHT             = 5.19
         WIDTH              = 112.6.
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

/* SETTINGS FOR FILL-IN RowObject.createDT IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.createDT:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.createUser IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.createUser:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.description IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.description:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       fiTableName:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.groupCode IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.groupCode:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.linkRecKey IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.linkRecKey:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.linkTable IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.linkTable:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Note1 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.Note1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Note2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Note2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Note3 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Note3:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Note4 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Note4:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Note5 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.Note5:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.ownerUser IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.ownerUser:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.rec_key IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.rec_key:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.statusCode IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.statusCode:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.tagType IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.tagType:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.updateDT IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.updateDT:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.updateUser IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.updateUser:HIDDEN IN FRAME F-Main           = TRUE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord vTable 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST _file NO-LOCK WHERE 
        _file._file-name EQ rowobject.linkTable:SCREEN-VALUE IN FRAME {&frame-name}
        NO-ERROR. 
    ASSIGN 
        fiTableName = IF AVAIL _file THEN _file._file-label ELSE "".
        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

