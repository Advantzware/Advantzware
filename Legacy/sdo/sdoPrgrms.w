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
{sdo/sdoComProcs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES prgrms

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  can_create can_delete can_run can_update dir_group image itemParent~
 menuImage1 menuImage2 menuImage3 menuLevel menuOrder menu_item mfgroup~
 mnemonic modeList module notes popup popUpMenu prgmname prgm_ver prgtitle~
 rec_key run_persistent securityLevelDefault securityLevelUser subjectID~
 systemType track_usage translation1 translation2 translation3 translation4~
 translation5 translation6 translation7 translation8 translation9~
 translation10 use_colors use_fonts widget_bgc1 widget_bgc2 widget_bgc3~
 widget_bgc4 widget_bgc5 widget_bgc6 widget_bgc7 widget_bgc8 widget_bgc9~
 widget_bgc10 widget_bgc11 widget_bgc12 widget_bgc13 widget_fgc1 widget_fgc2~
 widget_fgc3 widget_fgc4 widget_fgc5 widget_fgc6 widget_fgc7 widget_fgc8~
 widget_fgc9 widget_fgc10 widget_fgc11 widget_fgc12 widget_fgc13~
 widget_font1 widget_font2 widget_font3 widget_font4 widget_font5~
 widget_font6 widget_font7 widget_font8 widget_font9 widget_font10~
 widget_font11 widget_font12 widget_font13
&Scoped-define ENABLED-FIELDS-IN-prgrms can_create can_delete can_run ~
can_update dir_group image itemParent menuImage1 menuImage2 menuImage3 ~
menuLevel menuOrder menu_item mfgroup mnemonic modeList module notes popup ~
popUpMenu prgmname prgm_ver prgtitle rec_key run_persistent ~
securityLevelDefault securityLevelUser subjectID systemType track_usage ~
translation1 translation2 translation3 translation4 translation5 ~
translation6 translation7 translation8 translation9 translation10 ~
use_colors use_fonts widget_bgc1 widget_bgc2 widget_bgc3 widget_bgc4 ~
widget_bgc5 widget_bgc6 widget_bgc7 widget_bgc8 widget_bgc9 widget_bgc10 ~
widget_bgc11 widget_bgc12 widget_bgc13 widget_fgc1 widget_fgc2 widget_fgc3 ~
widget_fgc4 widget_fgc5 widget_fgc6 widget_fgc7 widget_fgc8 widget_fgc9 ~
widget_fgc10 widget_fgc11 widget_fgc12 widget_fgc13 widget_font1 ~
widget_font2 widget_font3 widget_font4 widget_font5 widget_font6 ~
widget_font7 widget_font8 widget_font9 widget_font10 widget_font11 ~
widget_font12 widget_font13 
&Scoped-Define DATA-FIELDS  can_create can_delete can_run can_update dir_group image itemParent~
 menuImage1 menuImage2 menuImage3 menuLevel menuOrder menu_item mfgroup~
 mnemonic modeList module notes popup popUpMenu prgmname prgm_ver prgtitle~
 rec_key run_persistent securityLevelDefault securityLevelUser subjectID~
 systemType track_usage translation1 translation2 translation3 translation4~
 translation5 translation6 translation7 translation8 translation9~
 translation10 use_colors use_fonts widget_bgc1 widget_bgc2 widget_bgc3~
 widget_bgc4 widget_bgc5 widget_bgc6 widget_bgc7 widget_bgc8 widget_bgc9~
 widget_bgc10 widget_bgc11 widget_bgc12 widget_bgc13 widget_fgc1 widget_fgc2~
 widget_fgc3 widget_fgc4 widget_fgc5 widget_fgc6 widget_fgc7 widget_fgc8~
 widget_fgc9 widget_fgc10 widget_fgc11 widget_fgc12 widget_fgc13~
 widget_font1 widget_font2 widget_font3 widget_font4 widget_font5~
 widget_font6 widget_font7 widget_font8 widget_font9 widget_font10~
 widget_font11 widget_font12 widget_font13
&Scoped-define DATA-FIELDS-IN-prgrms can_create can_delete can_run ~
can_update dir_group image itemParent menuImage1 menuImage2 menuImage3 ~
menuLevel menuOrder menu_item mfgroup mnemonic modeList module notes popup ~
popUpMenu prgmname prgm_ver prgtitle rec_key run_persistent ~
securityLevelDefault securityLevelUser subjectID systemType track_usage ~
translation1 translation2 translation3 translation4 translation5 ~
translation6 translation7 translation8 translation9 translation10 ~
use_colors use_fonts widget_bgc1 widget_bgc2 widget_bgc3 widget_bgc4 ~
widget_bgc5 widget_bgc6 widget_bgc7 widget_bgc8 widget_bgc9 widget_bgc10 ~
widget_bgc11 widget_bgc12 widget_bgc13 widget_fgc1 widget_fgc2 widget_fgc3 ~
widget_fgc4 widget_fgc5 widget_fgc6 widget_fgc7 widget_fgc8 widget_fgc9 ~
widget_fgc10 widget_fgc11 widget_fgc12 widget_fgc13 widget_font1 ~
widget_font2 widget_font3 widget_font4 widget_font5 widget_font6 ~
widget_font7 widget_font8 widget_font9 widget_font10 widget_font11 ~
widget_font12 widget_font13 
&Scoped-Define MANDATORY-FIELDS  prgmname
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.menuImage1 = prgrms.menuImage[1]~
  rowObject.menuImage2 = prgrms.menuImage[2]~
  rowObject.menuImage3 = prgrms.menuImage[3]~
  rowObject.translation1 = prgrms.translation[1]~
  rowObject.translation2 = prgrms.translation[2]~
  rowObject.translation3 = prgrms.translation[3]~
  rowObject.translation4 = prgrms.translation[4]~
  rowObject.translation5 = prgrms.translation[5]~
  rowObject.translation6 = prgrms.translation[6]~
  rowObject.translation7 = prgrms.translation[7]~
  rowObject.translation8 = prgrms.translation[8]~
  rowObject.translation9 = prgrms.translation[9]~
  rowObject.translation10 = prgrms.translation[10]~
  rowObject.widget_bgc1 = prgrms.widget_bgc[1]~
  rowObject.widget_bgc2 = prgrms.widget_bgc[2]~
  rowObject.widget_bgc3 = prgrms.widget_bgc[3]~
  rowObject.widget_bgc4 = prgrms.widget_bgc[4]~
  rowObject.widget_bgc5 = prgrms.widget_bgc[5]~
  rowObject.widget_bgc6 = prgrms.widget_bgc[6]~
  rowObject.widget_bgc7 = prgrms.widget_bgc[7]~
  rowObject.widget_bgc8 = prgrms.widget_bgc[8]~
  rowObject.widget_bgc9 = prgrms.widget_bgc[9]~
  rowObject.widget_bgc10 = prgrms.widget_bgc[10]~
  rowObject.widget_bgc11 = prgrms.widget_bgc[11]~
  rowObject.widget_bgc12 = prgrms.widget_bgc[12]~
  rowObject.widget_bgc13 = prgrms.widget_bgc[13]~
  rowObject.widget_fgc1 = prgrms.widget_fgc[1]~
  rowObject.widget_fgc2 = prgrms.widget_fgc[2]~
  rowObject.widget_fgc3 = prgrms.widget_fgc[3]~
  rowObject.widget_fgc4 = prgrms.widget_fgc[4]~
  rowObject.widget_fgc5 = prgrms.widget_fgc[5]~
  rowObject.widget_fgc6 = prgrms.widget_fgc[6]~
  rowObject.widget_fgc7 = prgrms.widget_fgc[7]~
  rowObject.widget_fgc8 = prgrms.widget_fgc[8]~
  rowObject.widget_fgc9 = prgrms.widget_fgc[9]~
  rowObject.widget_fgc10 = prgrms.widget_fgc[10]~
  rowObject.widget_fgc11 = prgrms.widget_fgc[11]~
  rowObject.widget_fgc12 = prgrms.widget_fgc[12]~
  rowObject.widget_fgc13 = prgrms.widget_fgc[13]~
  rowObject.widget_font1 = prgrms.widget_font[1]~
  rowObject.widget_font2 = prgrms.widget_font[2]~
  rowObject.widget_font3 = prgrms.widget_font[3]~
  rowObject.widget_font4 = prgrms.widget_font[4]~
  rowObject.widget_font5 = prgrms.widget_font[5]~
  rowObject.widget_font6 = prgrms.widget_font[6]~
  rowObject.widget_font7 = prgrms.widget_font[7]~
  rowObject.widget_font8 = prgrms.widget_font[8]~
  rowObject.widget_font9 = prgrms.widget_font[9]~
  rowObject.widget_font10 = prgrms.widget_font[10]~
  rowObject.widget_font11 = prgrms.widget_font[11]~
  rowObject.widget_font12 = prgrms.widget_font[12]~
  rowObject.widget_font13 = prgrms.widget_font[13]
&Scoped-Define DATA-FIELD-DEFS "sdo/sdoPrgrms.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH prgrms NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH prgrms NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main prgrms
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main prgrms


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      prgrms SCROLLING.
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
     _TblList          = "asi.prgrms"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > asi.prgrms.can_create
"can_create" "can_create" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[2]   > asi.prgrms.can_delete
"can_delete" "can_delete" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[3]   > asi.prgrms.can_run
"can_run" "can_run" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[4]   > asi.prgrms.can_update
"can_update" "can_update" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[5]   > asi.prgrms.dir_group
"dir_group" "dir_group" ? ? "character" ? ? ? ? ? ? yes ? no 64 yes ?
     _FldNameList[6]   > asi.prgrms.image
"image" "image" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ?
     _FldNameList[7]   > asi.prgrms.itemParent
"itemParent" "itemParent" ? ? "character" ? ? ? ? ? ? yes ? no 32 yes ?
     _FldNameList[8]   > asi.prgrms.menuImage[1]
"menuImage[1]" "menuImage1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[9]   > asi.prgrms.menuImage[2]
"menuImage[2]" "menuImage2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[10]   > asi.prgrms.menuImage[3]
"menuImage[3]" "menuImage3" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[11]   > asi.prgrms.menuLevel
"menuLevel" "menuLevel" ? ? "integer" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[12]   > asi.prgrms.menuOrder
"menuOrder" "menuOrder" ? ? "integer" ? ? ? ? ? ? yes ? no 11.4 yes ?
     _FldNameList[13]   > asi.prgrms.menu_item
"menu_item" "menu_item" ? ? "logical" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[14]   > asi.prgrms.mfgroup
"mfgroup" "mfgroup" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[15]   > asi.prgrms.mnemonic
"mnemonic" "mnemonic" ? ? "character" ? ? ? ? ? ? yes ? no 9.4 yes ?
     _FldNameList[16]   > asi.prgrms.modeList
"modeList" "modeList" ? ? "character" ? ? ? ? ? ? yes ? no 24 yes ?
     _FldNameList[17]   > asi.prgrms.module
"module" "module" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[18]   > asi.prgrms.notes
"notes" "notes" ? ? "character" ? ? ? ? ? ? yes ? no 120 yes ?
     _FldNameList[19]   > asi.prgrms.popup
"popup" "popup" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[20]   > asi.prgrms.popUpMenu
"popUpMenu" "popUpMenu" ? ? "character" ? ? ? ? ? ? yes ? no 32 yes ?
     _FldNameList[21]   > asi.prgrms.prgmname
"prgmname" "prgmname" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes ?
     _FldNameList[22]   > asi.prgrms.prgm_ver
"prgm_ver" "prgm_ver" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[23]   > asi.prgrms.prgtitle
"prgtitle" "prgtitle" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[24]   > asi.prgrms.rec_key
"rec_key" "rec_key" ? ? "character" ? ? ? ? ? ? yes ? no 21 yes ?
     _FldNameList[25]   > asi.prgrms.run_persistent
"run_persistent" "run_persistent" ? ? "logical" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[26]   > asi.prgrms.securityLevelDefault
"securityLevelDefault" "securityLevelDefault" ? ? "integer" ? ? ? ? ? ? yes ? no 16.6 yes ?
     _FldNameList[27]   > asi.prgrms.securityLevelUser
"securityLevelUser" "securityLevelUser" ? ? "integer" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[28]   > asi.prgrms.subjectID
"subjectID" "subjectID" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[29]   > asi.prgrms.systemType
"systemType" "systemType" ? ? "character" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[30]   > asi.prgrms.track_usage
"track_usage" "track_usage" ? ? "logical" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[31]   > asi.prgrms.translation[1]
"translation[1]" "translation1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[32]   > asi.prgrms.translation[2]
"translation[2]" "translation2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[33]   > asi.prgrms.translation[3]
"translation[3]" "translation3" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[34]   > asi.prgrms.translation[4]
"translation[4]" "translation4" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[35]   > asi.prgrms.translation[5]
"translation[5]" "translation5" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[36]   > asi.prgrms.translation[6]
"translation[6]" "translation6" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[37]   > asi.prgrms.translation[7]
"translation[7]" "translation7" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[38]   > asi.prgrms.translation[8]
"translation[8]" "translation8" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[39]   > asi.prgrms.translation[9]
"translation[9]" "translation9" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[40]   > asi.prgrms.translation[10]
"translation[10]" "translation10" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[41]   > asi.prgrms.use_colors
"use_colors" "use_colors" ? ? "logical" ? ? ? ? ? ? yes ? no 18.2 yes ?
     _FldNameList[42]   > asi.prgrms.use_fonts
"use_fonts" "use_fonts" ? ? "logical" ? ? ? ? ? ? yes ? no 17.6 yes ?
     _FldNameList[43]   > asi.prgrms.widget_bgc[1]
"widget_bgc[1]" "widget_bgc1" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[44]   > asi.prgrms.widget_bgc[2]
"widget_bgc[2]" "widget_bgc2" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[45]   > asi.prgrms.widget_bgc[3]
"widget_bgc[3]" "widget_bgc3" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[46]   > asi.prgrms.widget_bgc[4]
"widget_bgc[4]" "widget_bgc4" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[47]   > asi.prgrms.widget_bgc[5]
"widget_bgc[5]" "widget_bgc5" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[48]   > asi.prgrms.widget_bgc[6]
"widget_bgc[6]" "widget_bgc6" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[49]   > asi.prgrms.widget_bgc[7]
"widget_bgc[7]" "widget_bgc7" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[50]   > asi.prgrms.widget_bgc[8]
"widget_bgc[8]" "widget_bgc8" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[51]   > asi.prgrms.widget_bgc[9]
"widget_bgc[9]" "widget_bgc9" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[52]   > asi.prgrms.widget_bgc[10]
"widget_bgc[10]" "widget_bgc10" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[53]   > asi.prgrms.widget_bgc[11]
"widget_bgc[11]" "widget_bgc11" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[54]   > asi.prgrms.widget_bgc[12]
"widget_bgc[12]" "widget_bgc12" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[55]   > asi.prgrms.widget_bgc[13]
"widget_bgc[13]" "widget_bgc13" ? ? "integer" ? ? ? ? ? ? yes ? no 24.2 yes ?
     _FldNameList[56]   > asi.prgrms.widget_fgc[1]
"widget_fgc[1]" "widget_fgc1" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[57]   > asi.prgrms.widget_fgc[2]
"widget_fgc[2]" "widget_fgc2" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[58]   > asi.prgrms.widget_fgc[3]
"widget_fgc[3]" "widget_fgc3" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[59]   > asi.prgrms.widget_fgc[4]
"widget_fgc[4]" "widget_fgc4" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[60]   > asi.prgrms.widget_fgc[5]
"widget_fgc[5]" "widget_fgc5" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[61]   > asi.prgrms.widget_fgc[6]
"widget_fgc[6]" "widget_fgc6" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[62]   > asi.prgrms.widget_fgc[7]
"widget_fgc[7]" "widget_fgc7" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[63]   > asi.prgrms.widget_fgc[8]
"widget_fgc[8]" "widget_fgc8" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[64]   > asi.prgrms.widget_fgc[9]
"widget_fgc[9]" "widget_fgc9" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[65]   > asi.prgrms.widget_fgc[10]
"widget_fgc[10]" "widget_fgc10" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[66]   > asi.prgrms.widget_fgc[11]
"widget_fgc[11]" "widget_fgc11" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[67]   > asi.prgrms.widget_fgc[12]
"widget_fgc[12]" "widget_fgc12" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[68]   > asi.prgrms.widget_fgc[13]
"widget_fgc[13]" "widget_fgc13" ? ? "integer" ? ? ? ? ? ? yes ? no 23 yes ?
     _FldNameList[69]   > asi.prgrms.widget_font[1]
"widget_font[1]" "widget_font1" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[70]   > asi.prgrms.widget_font[2]
"widget_font[2]" "widget_font2" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[71]   > asi.prgrms.widget_font[3]
"widget_font[3]" "widget_font3" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[72]   > asi.prgrms.widget_font[4]
"widget_font[4]" "widget_font4" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[73]   > asi.prgrms.widget_font[5]
"widget_font[5]" "widget_font5" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[74]   > asi.prgrms.widget_font[6]
"widget_font[6]" "widget_font6" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[75]   > asi.prgrms.widget_font[7]
"widget_font[7]" "widget_font7" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[76]   > asi.prgrms.widget_font[8]
"widget_font[8]" "widget_font8" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[77]   > asi.prgrms.widget_font[9]
"widget_font[9]" "widget_font9" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[78]   > asi.prgrms.widget_font[10]
"widget_font[10]" "widget_font10" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[79]   > asi.prgrms.widget_font[11]
"widget_font[11]" "widget_font11" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[80]   > asi.prgrms.widget_font[12]
"widget_font[12]" "widget_font12" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[81]   > asi.prgrms.widget_font[13]
"widget_font[13]" "widget_font13" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
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

