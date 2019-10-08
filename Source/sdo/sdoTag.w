&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tag

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  createDT createUser description groupCode linkRecKey linkTable Note1 Note2~
 Note3 Note4 Note5 ownerUser rec_key statusCode tagType updateDT updateUser
&Scoped-define ENABLED-FIELDS-IN-tag createDT createUser description ~
groupCode linkRecKey linkTable Note1 Note2 Note3 Note4 Note5 ownerUser ~
rec_key statusCode tagType updateDT updateUser 
&Scoped-Define DATA-FIELDS  createDT createUser description groupCode linkRecKey linkTable Note1 Note2~
 Note3 Note4 Note5 ownerUser rec_key statusCode tagType updateDT updateUser
&Scoped-define DATA-FIELDS-IN-tag createDT createUser description groupCode ~
linkRecKey linkTable Note1 Note2 Note3 Note4 Note5 ownerUser rec_key ~
statusCode tagType updateDT updateUser 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Note1 = tag.Note[1]  rowObject.Note2 = tag.Note[2]~
  rowObject.Note3 = tag.Note[3]  rowObject.Note4 = tag.Note[4]~
  rowObject.Note5 = tag.Note[5]
&Scoped-Define DATA-FIELD-DEFS "sdo/sdoTag.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH tag NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tag NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tag
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tag


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tag SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "asi.tag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > asi.tag.createDT
"createDT" "createDT" ? ? "datetime" ? ? ? ? ? ? yes ? no 26.2 yes ?
     _FldNameList[2]   > asi.tag.createUser
"createUser" "createUser" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[3]   > asi.tag.description
"description" "description" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[4]   > asi.tag.groupCode
"groupCode" "groupCode" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[5]   > asi.tag.linkRecKey
"linkRecKey" "linkRecKey" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[6]   > asi.tag.linkTable
"linkTable" "linkTable" ? ? "character" ? ? ? ? ? ? yes ? no 24 yes ?
     _FldNameList[7]   > asi.tag.Note[1]
"Note[1]" "Note1" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[8]   > asi.tag.Note[2]
"Note[2]" "Note2" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[9]   > asi.tag.Note[3]
"Note[3]" "Note3" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[10]   > asi.tag.Note[4]
"Note[4]" "Note4" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[11]   > asi.tag.Note[5]
"Note[5]" "Note5" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[12]   > asi.tag.ownerUser
"ownerUser" "ownerUser" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[13]   > asi.tag.rec_key
"rec_key" "rec_key" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[14]   > asi.tag.statusCode
"statusCode" "statusCode" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[15]   > asi.tag.tagType
"tagType" "tagType" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[16]   > asi.tag.updateDT
"updateDT" "updateDT" ? ? "datetime" ? ? ? ? ? ? yes ? no 26.2 yes ?
     _FldNameList[17]   > asi.tag.updateUser
"updateUser" "updateUser" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chgQueryWhere dTables
PROCEDURE chgQueryWhere:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcWhere AS CHAR NO-UNDO.
    DYNAMIC-FUNCTION('setQueryWhere':U IN THIS-PROCEDURE, ipcWhere).
    DYNAMIC-FUNCTION('closeQuery':U ).
    DYNAMIC-FUNCTION('openQuery':U ).
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

