&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------
  File:  
  Copyright:            (c)2016 Foresight Software LLC - All rights reserved
  Version:              10.7000 - 06/01/2016 - tyndmar
  Description:          from DATA.W - Template For SmartData objects in the ADM
  Input Parameters:     <none>
  Output Parameters:    <none>
  Modified:             06/01/2016
  Notes:                1 - set lLarge = TRUE if large dataset expected, else false
                        2 - populate initQuery, initSort if constants to be set, else use ""
                        3 - cControl specifies which control record
                        4 - cEntity1 specifies which entity code
                        5 - include {pt/sdoComProcs.i} in main block
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&GLOB DATA-LOGIC-PROCEDURE .p

&SCOPED-DEFINE cControl FA
&SCOPED-DEFINE cEntity1 {&cControl}
&SCOPED-DEFINE cEntity2
&SCOPED-DEFINE cTable location
&SCOPED-DEFINE lLarge FALSE
&SCOPED-DEFINE initQuery "" 
&SCOPED-DEFINE initSort ""
&SCOPED-DEFINE lJump FALSE
&SCOPED-DEFINE keyField1 location
&SCOPED-DEFINE keyType1 CHAR
&SCOPED-DEFINE byEntity bTable.{&cEntity1}-entity = v-{&cEntity1}-entity AND

DEF BUFFER bTable FOR {&cTable}.
DEF SHARED VAR v-ap-entity AS CHAR NO-UNDO.
DEF SHARED VAR v-ar-entity AS CHAR NO-UNDO.
DEF SHARED VAR v-fa-entity AS CHAR NO-UNDO.
DEF SHARED VAR v-gl-entity AS CHAR NO-UNDO.
DEF SHARED VAR v-in-entity AS CHAR NO-UNDO.
DEF SHARED VAR terminalid  AS CHAR NO-UNDO.
DEF SHARED VAR condition AS CHAR NO-UNDO.
DEF SHARED VAR init-val AS CHAR EXTENT 250 NO-UNDO.
DEF VAR cKeyValue1 AS {&keyType1}.
DEF VAR rFileRowid AS ROWID.

{pt/getControlRecord.i "{&cControl}"}
IF AVAIL {&cControl}-control THEN ASSIGN
    v-{&cEntity1}-entity = {&cControl}-control.{&cEntity1}-entity.

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
&Scoped-define INTERNAL-TABLES location

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  location description room building work-center city state county entity~
 Country-code
&Scoped-define ENABLED-FIELDS-IN-location location description room ~
building work-center city state county entity Country-code 
&Scoped-Define DATA-FIELDS  location description room building work-center city state county entity~
 Country-code
&Scoped-define DATA-FIELDS-IN-location location description room building ~
work-center city state county entity Country-code 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "fa/sdoLoc.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH location NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH location NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main location
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main location


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      location SCROLLING.
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
         HEIGHT             = 1.63
         WIDTH              = 46.57.
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
     _TblList          = "ptdb1.location"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ptdb1.location.location
"location" "location" ? ? "character" ? ? ? ? ? ? yes ? no 7.4 yes
     _FldNameList[2]   > ptdb1.location.description
"description" "description" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[3]   > ptdb1.location.room
"room" "room" ? ? "character" ? ? ? ? ? ? yes ? no 5.6 yes
     _FldNameList[4]   > ptdb1.location.building
"building" "building" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[5]   > ptdb1.location.work-center
"work-center" "work-center" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes
     _FldNameList[6]   > ptdb1.location.city
"city" "city" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[7]   > ptdb1.location.state
"state" "state" ? ? "character" ? ? ? ? ? ? yes ? no 5 yes
     _FldNameList[8]   > ptdb1.location.county
"county" "county" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[9]   > ptdb1.location.entity
"entity" "entity" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes
     _FldNameList[10]   > ptdb1.location.Country-code
"Country-code" "Country-code" ? ? "character" ? ? ? ? ? ? yes ? no 9.43 yes
     _Design-Parent    is WINDOW dTables @ ( 1.17 , 2.57 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

{pt/sdoComProcs.i}

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

