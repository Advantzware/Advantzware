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

&SCOPED-DEFINE cControl FA
&SCOPED-DEFINE cEntity1 {&cControl}
&SCOPED-DEFINE cEntity2
&SCOPED-DEFINE cTable fa-control-d
&SCOPED-DEFINE lLarge FALSE
&SCOPED-DEFINE initQuery ""
&SCOPED-DEFINE initSort ""
&SCOPED-DEFINE lJump TRUE
&SCOPED-DEFINE keyField1 fa-entity
&SCOPED-DEFINE keyType1 CHAR
&SCOPED-DEFINE byEntity 

DEF BUFFER bTable FOR {&cTable}.
DEF VAR cKeyValue1 AS {&keyType1}.
DEF VAR rFileRowid AS ROWID.

{src/asicommon.i}

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
&Scoped-define INTERNAL-TABLES fa-control-d

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  dep-a-no fa-a-no FA-entity gl-dist-gno gl-dist-no gl-mast-no glcode-a-no~
 jrnl-no loc-a-no Next-seq-no sort1-no sort2-no tag-no tax-a-no
&Scoped-define ENABLED-FIELDS-IN-fa-control-d dep-a-no fa-a-no FA-entity ~
gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no Next-seq-no ~
sort1-no sort2-no tag-no tax-a-no 
&Scoped-Define DATA-FIELDS  dep-a-no fa-a-no FA-entity gl-dist-gno gl-dist-no gl-mast-no glcode-a-no~
 jrnl-no loc-a-no Next-seq-no sort1-no sort2-no tag-no tax-a-no
&Scoped-define DATA-FIELDS-IN-fa-control-d dep-a-no fa-a-no FA-entity ~
gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no Next-seq-no ~
sort1-no sort2-no tag-no tax-a-no 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "fa/sdoFaCtrlD.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH fa-control-d NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH fa-control-d NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main fa-control-d
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main fa-control-d


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      fa-control-d SCROLLING.
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
     _TblList          = "ASI.fa-control-d"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ASI.fa-control-d.dep-a-no
"dep-a-no" "dep-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[2]   > ASI.fa-control-d.fa-a-no
"fa-a-no" "fa-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[3]   > ASI.fa-control-d.FA-entity
"FA-entity" "FA-entity" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[4]   > ASI.fa-control-d.gl-dist-gno
"gl-dist-gno" "gl-dist-gno" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[5]   > ASI.fa-control-d.gl-dist-no
"gl-dist-no" "gl-dist-no" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[6]   > ASI.fa-control-d.gl-mast-no
"gl-mast-no" "gl-mast-no" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[7]   > ASI.fa-control-d.glcode-a-no
"glcode-a-no" "glcode-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[8]   > ASI.fa-control-d.jrnl-no
"jrnl-no" "jrnl-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[9]   > ASI.fa-control-d.loc-a-no
"loc-a-no" "loc-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[10]   > ASI.fa-control-d.Next-seq-no
"Next-seq-no" "Next-seq-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[11]   > ASI.fa-control-d.sort1-no
"sort1-no" "sort1-no" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[12]   > ASI.fa-control-d.sort2-no
"sort2-no" "sort2-no" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[13]   > ASI.fa-control-d.tag-no
"tag-no" "tag-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[14]   > ASI.fa-control-d.tax-a-no
"tax-a-no" "tax-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

{src/sdoComProcs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

