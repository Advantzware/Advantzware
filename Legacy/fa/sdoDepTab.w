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
&SCOPED-DEFINE cTable dep-table
&SCOPED-DEFINE lLarge FALSE
&SCOPED-DEFINE initQuery "" 
&SCOPED-DEFINE initSort ""
&SCOPED-DEFINE lJump FALSE
&SCOPED-DEFINE keyField1 method
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
&Scoped-define INTERNAL-TABLES dep-table

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  method Auto-sl Ratio Conv Manual soyd Depr1 Depr2 Depr3 Depr4 Depr5 Depr6~
 Depr7 Depr8 Depr9 Depr10 Depr11 Depr12 Depr13 Depr14 Depr15 Depr16 Depr17~
 Depr18 Depr19 Depr20 Depr21 Depr22 Depr23 Depr24 Depr25 Depr26 Depr27~
 Depr28 Depr29 Depr30 Depr31 Depr32 Depr33 Depr34 Depr35 Depr36 Depr37~
 Depr38 Depr39 Depr40
&Scoped-define ENABLED-FIELDS-IN-dep-table method Auto-sl Ratio Conv Manual ~
soyd Depr1 Depr2 Depr3 Depr4 Depr5 Depr6 Depr7 Depr8 Depr9 Depr10 Depr11 ~
Depr12 Depr13 Depr14 Depr15 Depr16 Depr17 Depr18 Depr19 Depr20 Depr21 ~
Depr22 Depr23 Depr24 Depr25 Depr26 Depr27 Depr28 Depr29 Depr30 Depr31 ~
Depr32 Depr33 Depr34 Depr35 Depr36 Depr37 Depr38 Depr39 Depr40 
&Scoped-Define DATA-FIELDS  method Auto-sl Ratio Conv Manual soyd Depr1 Depr2 Depr3 Depr4 Depr5 Depr6~
 Depr7 Depr8 Depr9 Depr10 Depr11 Depr12 Depr13 Depr14 Depr15 Depr16 Depr17~
 Depr18 Depr19 Depr20 Depr21 Depr22 Depr23 Depr24 Depr25 Depr26 Depr27~
 Depr28 Depr29 Depr30 Depr31 Depr32 Depr33 Depr34 Depr35 Depr36 Depr37~
 Depr38 Depr39 Depr40
&Scoped-define DATA-FIELDS-IN-dep-table method Auto-sl Ratio Conv Manual ~
soyd Depr1 Depr2 Depr3 Depr4 Depr5 Depr6 Depr7 Depr8 Depr9 Depr10 Depr11 ~
Depr12 Depr13 Depr14 Depr15 Depr16 Depr17 Depr18 Depr19 Depr20 Depr21 ~
Depr22 Depr23 Depr24 Depr25 Depr26 Depr27 Depr28 Depr29 Depr30 Depr31 ~
Depr32 Depr33 Depr34 Depr35 Depr36 Depr37 Depr38 Depr39 Depr40 
&Scoped-Define MANDATORY-FIELDS  Ratio
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.Depr1 = dep-table.Depr[1]  rowObject.Depr2 = dep-table.Depr[2]~
  rowObject.Depr3 = dep-table.Depr[3]  rowObject.Depr4 = dep-table.Depr[4]~
  rowObject.Depr5 = dep-table.Depr[5]  rowObject.Depr6 = dep-table.Depr[6]~
  rowObject.Depr7 = dep-table.Depr[7]  rowObject.Depr8 = dep-table.Depr[8]~
  rowObject.Depr9 = dep-table.Depr[9]  rowObject.Depr10 = dep-table.Depr[10]~
  rowObject.Depr11 = dep-table.Depr[11]~
  rowObject.Depr12 = dep-table.Depr[12]~
  rowObject.Depr13 = dep-table.Depr[13]~
  rowObject.Depr14 = dep-table.Depr[14]~
  rowObject.Depr15 = dep-table.Depr[15]~
  rowObject.Depr16 = dep-table.Depr[16]~
  rowObject.Depr17 = dep-table.Depr[17]~
  rowObject.Depr18 = dep-table.Depr[18]~
  rowObject.Depr19 = dep-table.Depr[19]~
  rowObject.Depr20 = dep-table.Depr[20]~
  rowObject.Depr21 = dep-table.Depr[21]~
  rowObject.Depr22 = dep-table.Depr[22]~
  rowObject.Depr23 = dep-table.Depr[23]~
  rowObject.Depr24 = dep-table.Depr[24]~
  rowObject.Depr25 = dep-table.Depr[25]~
  rowObject.Depr26 = dep-table.Depr[26]~
  rowObject.Depr27 = dep-table.Depr[27]~
  rowObject.Depr28 = dep-table.Depr[28]~
  rowObject.Depr29 = dep-table.Depr[29]~
  rowObject.Depr30 = dep-table.Depr[30]~
  rowObject.Depr31 = dep-table.Depr[31]~
  rowObject.Depr32 = dep-table.Depr[32]~
  rowObject.Depr33 = dep-table.Depr[33]~
  rowObject.Depr34 = dep-table.Depr[34]~
  rowObject.Depr35 = dep-table.Depr[35]~
  rowObject.Depr36 = dep-table.Depr[36]~
  rowObject.Depr37 = dep-table.Depr[37]~
  rowObject.Depr38 = dep-table.Depr[38]~
  rowObject.Depr39 = dep-table.Depr[39]~
  rowObject.Depr40 = dep-table.Depr[40]
&Scoped-Define DATA-FIELD-DEFS "fa/sdoDepTab.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH dep-table NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH dep-table NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main dep-table
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main dep-table


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      dep-table SCROLLING.
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
     _TblList          = "ptdb1.dep-table"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ptdb1.dep-table.method
"method" "method" ? ? "character" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[2]   > ptdb1.dep-table.Auto-sl
"Auto-sl" "Auto-sl" ? ? "logical" ? ? ? ? ? ? yes ? no 17.8 yes
     _FldNameList[3]   > ptdb1.dep-table.Ratio
"Ratio" "Ratio" "Depreciation" ? "decimal" ? ? ? ? ? ? yes ? yes 25 yes
     _FldNameList[4]   > ptdb1.dep-table.Conv
"Conv" "Conv" "Conv" ? "character" ? ? ? ? ? ? yes ? no 31.4 yes
     _FldNameList[5]   > ptdb1.dep-table.Manual
"Manual" "Manual" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes
     _FldNameList[6]   > ptdb1.dep-table.soyd
"soyd" "soyd" ? ? "logical" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[7]   > ptdb1.dep-table.Depr[1]
"Depr[1]" "Depr1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[8]   > ptdb1.dep-table.Depr[2]
"Depr[2]" "Depr2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[9]   > ptdb1.dep-table.Depr[3]
"Depr[3]" "Depr3" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[10]   > ptdb1.dep-table.Depr[4]
"Depr[4]" "Depr4" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[11]   > ptdb1.dep-table.Depr[5]
"Depr[5]" "Depr5" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[12]   > ptdb1.dep-table.Depr[6]
"Depr[6]" "Depr6" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[13]   > ptdb1.dep-table.Depr[7]
"Depr[7]" "Depr7" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[14]   > ptdb1.dep-table.Depr[8]
"Depr[8]" "Depr8" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[15]   > ptdb1.dep-table.Depr[9]
"Depr[9]" "Depr9" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[16]   > ptdb1.dep-table.Depr[10]
"Depr[10]" "Depr10" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[17]   > ptdb1.dep-table.Depr[11]
"Depr[11]" "Depr11" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[18]   > ptdb1.dep-table.Depr[12]
"Depr[12]" "Depr12" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[19]   > ptdb1.dep-table.Depr[13]
"Depr[13]" "Depr13" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[20]   > ptdb1.dep-table.Depr[14]
"Depr[14]" "Depr14" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[21]   > ptdb1.dep-table.Depr[15]
"Depr[15]" "Depr15" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[22]   > ptdb1.dep-table.Depr[16]
"Depr[16]" "Depr16" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[23]   > ptdb1.dep-table.Depr[17]
"Depr[17]" "Depr17" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[24]   > ptdb1.dep-table.Depr[18]
"Depr[18]" "Depr18" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[25]   > ptdb1.dep-table.Depr[19]
"Depr[19]" "Depr19" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[26]   > ptdb1.dep-table.Depr[20]
"Depr[20]" "Depr20" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[27]   > ptdb1.dep-table.Depr[21]
"Depr[21]" "Depr21" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[28]   > ptdb1.dep-table.Depr[22]
"Depr[22]" "Depr22" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[29]   > ptdb1.dep-table.Depr[23]
"Depr[23]" "Depr23" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[30]   > ptdb1.dep-table.Depr[24]
"Depr[24]" "Depr24" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[31]   > ptdb1.dep-table.Depr[25]
"Depr[25]" "Depr25" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[32]   > ptdb1.dep-table.Depr[26]
"Depr[26]" "Depr26" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[33]   > ptdb1.dep-table.Depr[27]
"Depr[27]" "Depr27" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[34]   > ptdb1.dep-table.Depr[28]
"Depr[28]" "Depr28" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[35]   > ptdb1.dep-table.Depr[29]
"Depr[29]" "Depr29" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[36]   > ptdb1.dep-table.Depr[30]
"Depr[30]" "Depr30" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[37]   > ptdb1.dep-table.Depr[31]
"Depr[31]" "Depr31" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[38]   > ptdb1.dep-table.Depr[32]
"Depr[32]" "Depr32" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[39]   > ptdb1.dep-table.Depr[33]
"Depr[33]" "Depr33" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[40]   > ptdb1.dep-table.Depr[34]
"Depr[34]" "Depr34" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[41]   > ptdb1.dep-table.Depr[35]
"Depr[35]" "Depr35" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[42]   > ptdb1.dep-table.Depr[36]
"Depr[36]" "Depr36" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[43]   > ptdb1.dep-table.Depr[37]
"Depr[37]" "Depr37" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[44]   > ptdb1.dep-table.Depr[38]
"Depr[38]" "Depr38" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[45]   > ptdb1.dep-table.Depr[39]
"Depr[39]" "Depr39" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
     _FldNameList[46]   > ptdb1.dep-table.Depr[40]
"Depr[40]" "Depr40" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes
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

