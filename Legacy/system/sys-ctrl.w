&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: system/sys-ctrl.w

  Description: System Control Settings

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 8.7.2018

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

&Scoped-define program-id sys-ctrl.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{sys/ref/sys-ctrl.i}

DEFINE VARIABLE hCurrentFilter   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cFilter          AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cSubFilter       AS CHARACTER NO-UNDO INITIAL "ALL".
DEFINE VARIABLE cColumnLabel     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSaveLabel       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAscending       AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lSearchOpen      AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lSuperAdmin      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur    AS HANDLE    NO-UNDO.
DEFINE VARIABLE correct-error    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE gvcMultiSelect   AS CHARACTER NO-UNDO INITIAL "OEDATECHANGE,SSBOLEMAIL".
DEFINE VARIABLE cValidateList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hFieldValue      AS HANDLE    NO-UNDO.
DEFINE VARIABLE iUserPrintOffSet AS INTEGER   NO-UNDO INITIAL 5.
DEFINE VARIABLE lSortMove        AS LOGICAL   NO-UNDO INITIAL YES.

cValidateList = "QUOPRINT,BOLFMT,ACKHEAD,RELPRINT,POPRINT,"
              + "INVPRINT,BOLCERT,JOBCARDF,JOBCARDC,QUOPRICE"
              + "SSBOLEMAIL,OEDATECHANGE,RELPOST"
              .

DEFINE TEMP-TABLE ttSysCtrl NO-UNDO
    FIELD category             LIKE sys-ctrl.category
    FIELD subCategory          LIKE sys-ctrl.subCategory
    FIELD name                 LIKE sys-ctrl.name FORMAT "x(20)"
    FIELD descrip              LIKE sys-ctrl.descrip
    FIELD typeCode             LIKE sys-ctrl.typeCode        LABEL "Type"
    FIELD module               LIKE sys-ctrl.module          LABEL "Module"
    FIELD securityLevelUser    LIKE sys-ctrl.securityLevelUser
    FIELD securityLevelDefault LIKE sys-ctrl.securityLevelDefault
    FIELD dataType               AS CHARACTER FORMAT "x(10)" LABEL "Data Type"
    FIELD fieldDescrip           AS CHARACTER FORMAT "x(40)" LABEL "Value Description"
    FIELD fieldDefault           AS CHARACTER FORMAT "x(40)" LABEL "Value Default"
    FIELD fieldValue             AS CHARACTER FORMAT "x(40)" LABEL "Value"
    FIELD fieldSource            AS CHARACTER FORMAT "x(20)" LABEL "Field Source"
    FIELD fieldExtent            AS INTEGER   FORMAT ">>9"   LABEL "Ext"
    FIELD tableSource            AS CHARACTER FORMAT "x(20)" LABEL "Table Source"
    FIELD allData                AS CHARACTER
    FIELD allowAdd               AS LOGICAL   INITIAL YES
    FIELD allowDelete            AS LOGICAL   INITIAL YES
        INDEX ttSysCtrl IS PRIMARY
            category
            subCategory
            name
            .
{system/menuTree.i}
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME sysCtrlBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSysCtrl sys-ctrl-shipto sys-ctrl

/* Definitions for BROWSE sysCtrlBrowse                                 */
&Scoped-define FIELDS-IN-QUERY-sysCtrlBrowse ttSysCtrl.name ttSysCtrl.fieldDescrip ttSysCtrl.fieldValue ttSysCtrl.descrip ttSysCtrl.module ttSysCtrl.typeCode ttSysCtrl.category ttSysCtrl.subCategory ttSysCtrl.securityLevelUser ttSysCtrl.tableSource ttSysCtrl.fieldSource ttSysCtrl.fieldExtent ttSysCtrl.dataType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sysCtrlBrowse   
&Scoped-define SELF-NAME sysCtrlBrowse
&Scoped-define QUERY-STRING-sysCtrlBrowse FOR EACH ttSysCtrl WHERE (ttSysCtrl.category EQ cFilter    OR  cFilter EQ "ALL")   AND (ttSysCtrl.subCategory EQ cSubFilter    OR  cSubFilter EQ "ALL")   AND ttSysCtrl.name         MATCHES "*" + cNameFilter         + "*"   AND ttSysCtrl.fieldDescrip MATCHES "*" + cFieldDescripFilter + "*"   AND ttSysCtrl.fieldValue   MATCHES "*" + cValueFilter        + "*"   AND ttSysCtrl.descrip      MATCHES "*" + cDescripFilter      + "*"   AND ttSysCtrl.module       MATCHES "*" + cModuleFilter       + "*"   AND ttSysCtrl.typeCode     MATCHES "*" + cTypeCodeFilter     + "*"   AND ttSysCtrl.category     MATCHES "*" + cCategoryFilter     + "*"   AND ttSysCtrl.subCategory  MATCHES "*" + cSubCategoryFilter  + "*"   AND ttSysCtrl.tableSource  MATCHES "*" + cTableSourceFilter  + "*"   AND ttSysCtrl.fieldSource  MATCHES "*" + cFieldSourceFilter  + "*"   AND ttSysCtrl.dataType     MATCHES "*" + cDataTypeFilter     + "*"   AND ttSysCtrl.allData      MATCHES "*" + searchBar           + "*"   ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-sysCtrlBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrl WHERE (ttSysCtrl.category EQ cFilter    OR  cFilter EQ "ALL")   AND (ttSysCtrl.subCategory EQ cSubFilter    OR  cSubFilter EQ "ALL")   AND ttSysCtrl.name         MATCHES "*" + cNameFilter         + "*"   AND ttSysCtrl.fieldDescrip MATCHES "*" + cFieldDescripFilter + "*"   AND ttSysCtrl.fieldValue   MATCHES "*" + cValueFilter        + "*"   AND ttSysCtrl.descrip      MATCHES "*" + cDescripFilter      + "*"   AND ttSysCtrl.module       MATCHES "*" + cModuleFilter       + "*"   AND ttSysCtrl.typeCode     MATCHES "*" + cTypeCodeFilter     + "*"   AND ttSysCtrl.category     MATCHES "*" + cCategoryFilter     + "*"   AND ttSysCtrl.subCategory  MATCHES "*" + cSubCategoryFilter  + "*"   AND ttSysCtrl.tableSource  MATCHES "*" + cTableSourceFilter  + "*"   AND ttSysCtrl.fieldSource  MATCHES "*" + cFieldSourceFilter  + "*"   AND ttSysCtrl.dataType     MATCHES "*" + cDataTypeFilter     + "*"   AND ttSysCtrl.allData      MATCHES "*" + searchBar           + "*"   ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-sysCtrlBrowse ttSysCtrl
&Scoped-define FIRST-TABLE-IN-QUERY-sysCtrlBrowse ttSysCtrl


/* Definitions for BROWSE sysCtrlShipToBrowse                           */
&Scoped-define FIELDS-IN-QUERY-sysCtrlShipToBrowse ~
sys-ctrl-shipto.cust-vend sys-ctrl-shipto.descrip ~
sys-ctrl-shipto.cust-vend-no sys-ctrl-shipto.ship-id ~
sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld sys-ctrl-shipto.dec-fld ~
sys-ctrl-shipto.int-fld sys-ctrl-shipto.log-fld 
&Scoped-define ENABLED-FIELDS-IN-QUERY-sysCtrlShipToBrowse 
&Scoped-define QUERY-STRING-sysCtrlShipToBrowse FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ g_company ~
AND sys-ctrl-shipto.name EQ ttSysCtrl.name NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-sysCtrlShipToBrowse OPEN QUERY sysCtrlShipToBrowse FOR EACH sys-ctrl-shipto ~
      WHERE sys-ctrl-shipto.company EQ g_company ~
AND sys-ctrl-shipto.name EQ ttSysCtrl.name NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-sysCtrlShipToBrowse sys-ctrl-shipto
&Scoped-define FIRST-TABLE-IN-QUERY-sysCtrlShipToBrowse sys-ctrl-shipto


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for FRAME formsFrame                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-formsFrame ~
    ~{&OPEN-QUERY-sysCtrlShipToBrowse}

/* Definitions for FRAME viewFormFrame                                  */
&Scoped-define FIELDS-IN-QUERY-viewFormFrame sys-ctrl-shipto.cust-vend ~
sys-ctrl-shipto.cust-vend-no sys-ctrl-shipto.ship-id ~
sys-ctrl-shipto.descrip sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld ~
sys-ctrl-shipto.dec-fld sys-ctrl-shipto.int-fld sys-ctrl-shipto.log-fld 
&Scoped-define QUERY-STRING-viewFormFrame FOR EACH sys-ctrl-shipto SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFormFrame OPEN QUERY viewFormFrame FOR EACH sys-ctrl-shipto SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFormFrame sys-ctrl-shipto
&Scoped-define FIRST-TABLE-IN-QUERY-viewFormFrame sys-ctrl-shipto


/* Definitions for FRAME viewFrame                                      */
&Scoped-define QUERY-STRING-viewFrame FOR EACH sys-ctrl SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH sys-ctrl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFrame sys-ctrl
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame sys-ctrl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRestoreDefaults btnSortMove svFocus ~
searchBar sysCtrlBrowse 
&Scoped-Define DISPLAYED-OBJECTS svFocus searchBar 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,displayFields,enabledFields,searchFilters */
&Scoped-define transPanel btnClose btnAdd btnCancel btnCopy btnDefaults ~
btnDelete btnForms btnReset btnUpdate 
&Scoped-define transInit btnClose btnAdd btnCopy btnDefaults btnDelete ~
btnForms btnUpdate 
&Scoped-define transUpdate btnCancel btnReset btnUpdate 
&Scoped-define displayFields cCategory cSubcategory iSecurityLevelUser ~
iSecurityLevelDefault cName cTypeCode cModule cDescrip cFieldDescrip ~
cFieldValue cFieldDefault ctableSource cfieldSource cDataType 
&Scoped-define enabledFields cCategory cSubcategory iSecurityLevelUser ~
iSecurityLevelDefault cName cTypeCode cModule cDescrip cFieldDescrip ~
cFieldValue cFieldDefault 
&Scoped-define searchFilters cNameFilter cFieldDescripFilter cValueFilter ~
cDescripFilter cModuleFilter cTypeCodeFilter cCategoryFilter ~
cSubCategoryFilter cTableSourceFilter cFieldSourceFilter cDataTypeFilter 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnExport 
     IMAGE-UP FILE "Graphics/32x32/export.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/export_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 8 BY 1.91 TOOLTIP "Export".

DEFINE BUTTON btnImport 
     IMAGE-UP FILE "Graphics/32x32/import.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/import_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Import" 
     SIZE 8 BY 1.91 TOOLTIP "Import".

DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 105 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE VARIABLE svFocus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE .2 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38.

DEFINE VARIABLE cSysCtrFieldlDescrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 112 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSysCtrlDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "DataType" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSysCtrlDescrip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 112 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSysCtrlName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON btnClear  NO-FOCUS
     LABEL "Clear" 
     SIZE 8.4 BY 1.05 TOOLTIP "Clear Search Filters"
     FONT 1.

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY .95 TOOLTIP "Advanced Search".

DEFINE VARIABLE cCategoryFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Category Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cDataTypeFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data Type" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Data Type Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cDescripFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Description Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cFieldDescripFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value Description" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Description Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cFieldSourceFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field Source" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Field Source Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cModuleFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Module" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Module Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cNameFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Name Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cSubCategoryFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sub Category" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Sub Category Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cTableSourceFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table Source" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Table Source Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cTypeCodeFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Type Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE VARIABLE cValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 40.8 BY 1 TOOLTIP "Value Search"
     BGCOLOR 15 FGCOLOR 1 FONT 1 NO-UNDO.

DEFINE BUTTON btnAdd-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.2 BY 1 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCancel-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnCopy-2 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/element_copy_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnFirst-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_beginning.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_beginning_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnForms-2 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.4 BY 1.05 TOOLTIP "Close".

DEFINE BUTTON btnLast-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_end.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_end_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_right.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_right_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrev-2 
     IMAGE-UP FILE "Graphics/32x32/navigate_left.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_left_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btnReset-2 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUpdate-2 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE VARIABLE cLogLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logical" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE ship_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE type_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE transPanel-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 34 BY 2.38
     BGCOLOR 15 .

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.2 BY 1 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.2 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics\32x32\form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDefaults 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/refresh_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 8 BY 1.91 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnFirst-1 
     IMAGE-UP FILE "Graphics/32x32/navigate_beginning.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_beginning_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "First" 
     SIZE 8 BY 1.91 TOOLTIP "First".

DEFINE BUTTON btnForms 
     IMAGE-UP FILE "Graphics/32x32/form.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Forms" 
     SIZE 8 BY 1.91 TOOLTIP "Forms".

DEFINE BUTTON btnLast-1 
     IMAGE-UP FILE "Graphics/32x32/navigate_end.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_end_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Last" 
     SIZE 8 BY 1.91 TOOLTIP "Last".

DEFINE BUTTON btnNext-1 
     IMAGE-UP FILE "Graphics/32x32/navigate_right.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_right_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Next" 
     SIZE 8 BY 1.91 TOOLTIP "Next".

DEFINE BUTTON btnPrev-1 
     IMAGE-UP FILE "Graphics/32x32/navigate_left.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_left_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Previous" 
     SIZE 8 BY 1.91 TOOLTIP "Previous".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE VARIABLE cDataType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Character","Date","Decimal","Integer","Logical" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCategory AS CHARACTER FORMAT "x(16)" 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cDescrip AS CHARACTER FORMAT "x(256)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cFieldDefault AS CHARACTER FORMAT "x(256)" 
     LABEL "Value Default" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cFieldDescrip AS CHARACTER FORMAT "x(256)" 
     LABEL "Value Description" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cfieldSource AS CHARACTER FORMAT "x(256)" 
     LABEL "Field Source" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cFieldValue AS CHARACTER FORMAT "x(256)" 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cModule AS CHARACTER FORMAT "x(5)" 
     LABEL "System Module" 
     VIEW-AS FILL-IN 
     SIZE 11.2 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cName AS CHARACTER FORMAT "x(256)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cSubcategory AS CHARACTER FORMAT "x(16)" 
     LABEL "SubCategory" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE ctableSource AS CHARACTER FORMAT "x(256)" 
     LABEL "Table Source" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE cTypeCode AS CHARACTER FORMAT "x(8)" 
     LABEL "Type Code" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE hDate AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Date Value" NO-UNDO.

DEFINE VARIABLE hDecimal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Decimal Value" NO-UNDO.

DEFINE VARIABLE hInteger AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Integer Value" NO-UNDO.

DEFINE VARIABLE iSecurityLevelDefault AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "/" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE iSecurityLevelUser AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "User Sec. Lev." 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE hLogical AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Yes", yes,
"No", no,
"Unknown", ?
     SIZE 27 BY 1 NO-UNDO.

DEFINE RECTANGLE transPanel
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 10 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE transPanel-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 34 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY sysCtrlBrowse FOR 
      ttSysCtrl SCROLLING.

DEFINE QUERY sysCtrlShipToBrowse FOR 
      sys-ctrl-shipto SCROLLING.

DEFINE QUERY viewFormFrame FOR 
      sys-ctrl-shipto SCROLLING.

DEFINE QUERY viewFrame FOR 
      sys-ctrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE sysCtrlBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sysCtrlBrowse C-Win _FREEFORM
  QUERY sysCtrlBrowse DISPLAY
      ttSysCtrl.name LABEL-BGCOLOR 14
ttSysCtrl.fieldDescrip LABEL-BGCOLOR 14
ttSysCtrl.fieldValue LABEL-BGCOLOR 14
ttSysCtrl.descrip LABEL-BGCOLOR 14
ttSysCtrl.module LABEL-BGCOLOR 14
ttSysCtrl.typeCode LABEL-BGCOLOR 14
ttSysCtrl.category LABEL-BGCOLOR 14
ttSysCtrl.subCategory LABEL-BGCOLOR 14
ttSysCtrl.securityLevelUser LABEL-BGCOLOR 14
ttSysCtrl.tableSource LABEL-BGCOLOR 14
ttSysCtrl.fieldSource LABEL-BGCOLOR 14
ttSysCtrl.fieldExtent
ttSysCtrl.dataType LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 27.57
         FGCOLOR 1 .

DEFINE BROWSE sysCtrlShipToBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sysCtrlShipToBrowse C-Win _STRUCTURED
  QUERY sysCtrlShipToBrowse NO-LOCK DISPLAY
      sys-ctrl-shipto.cust-vend FORMAT "Cust/Vend":U
      sys-ctrl-shipto.descrip FORMAT "x(40)":U
      sys-ctrl-shipto.cust-vend-no FORMAT "x(8)":U
      sys-ctrl-shipto.ship-id FORMAT "x(8)":U
      sys-ctrl-shipto.char-fld COLUMN-LABEL "Character Value" FORMAT "x(20)":U
      sys-ctrl-shipto.date-fld COLUMN-LABEL "Date" FORMAT "99/99/9999":U
      sys-ctrl-shipto.dec-fld COLUMN-LABEL "Decimal" FORMAT "->>,>>9.99":U
      sys-ctrl-shipto.int-fld COLUMN-LABEL "Integer" FORMAT "->,>>>,>>9":U
      sys-ctrl-shipto.log-fld COLUMN-LABEL "Log" FORMAT "yes/no":U
            WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 12.62
         BGCOLOR 15 FGCOLOR 1  ROW-HEIGHT-CHARS .9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRestoreDefaults AT ROW 1 COL 39 HELP
          "Restore Defaults" WIDGET-ID 42
     btnSortMove AT ROW 1 COL 43 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 44
     btnExport AT ROW 27.43 COL 11 HELP
          "Export" WIDGET-ID 36
     svFocus AT ROW 1 COL 1 NO-LABEL WIDGET-ID 4
     searchBar AT ROW 1 COL 54 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     sysCtrlBrowse AT ROW 2 COL 39 WIDGET-ID 300
     btnImport AT ROW 27.43 COL 20 HELP
          "Import" WIDGET-ID 38
     RECT-1 AT ROW 27.19 COL 10 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnClose AT ROW 1.24 COL 113 HELP
          "Close" WIDGET-ID 72
     cCategory AT ROW 1.24 COL 18 COLON-ALIGNED WIDGET-ID 2
     cSubcategory AT ROW 1.24 COL 58 COLON-ALIGNED WIDGET-ID 12
     iSecurityLevelUser AT ROW 1.24 COL 92 COLON-ALIGNED WIDGET-ID 10
     iSecurityLevelDefault AT ROW 1.24 COL 102 COLON-ALIGNED WIDGET-ID 44
     cName AT ROW 2.43 COL 18 COLON-ALIGNED WIDGET-ID 8
     cTypeCode AT ROW 2.43 COL 58 COLON-ALIGNED WIDGET-ID 14
     cModule AT ROW 2.43 COL 99 COLON-ALIGNED WIDGET-ID 6
     cDescrip AT ROW 3.62 COL 18 COLON-ALIGNED WIDGET-ID 4
     cFieldDescrip AT ROW 4.81 COL 18 COLON-ALIGNED WIDGET-ID 40
     hDecimal AT ROW 6 COL 18 COLON-ALIGNED HELP
          "Enter Decimal Value" NO-LABEL WIDGET-ID 60
     hLogical AT ROW 6 COL 20 NO-LABEL WIDGET-ID 64
     hInteger AT ROW 6 COL 18 COLON-ALIGNED HELP
          "Enter Integer Value" NO-LABEL WIDGET-ID 62
     hDate AT ROW 6 COL 18 COLON-ALIGNED HELP
          "Enter Date Value" NO-LABEL WIDGET-ID 58
     cFieldValue AT ROW 6 COL 18 COLON-ALIGNED WIDGET-ID 30
     cFieldDefault AT ROW 7.19 COL 18 COLON-ALIGNED WIDGET-ID 42
     ctableSource AT ROW 8.38 COL 18 COLON-ALIGNED WIDGET-ID 46
     cfieldSource AT ROW 8.38 COL 54 COLON-ALIGNED WIDGET-ID 48
     cDataType AT ROW 8.38 COL 94 COLON-ALIGNED HELP
          "Select Data Type" WIDGET-ID 52
     btnCalendar-1 AT ROW 6 COL 36 WIDGET-ID 272
     btnFirst-1 AT ROW 9.81 COL 69 HELP
          "First" WIDGET-ID 274
     btnLast-1 AT ROW 9.81 COL 93 HELP
          "Last" WIDGET-ID 68
     btnNext-1 AT ROW 9.81 COL 85 HELP
          "Next" WIDGET-ID 276
     btnPrev-1 AT ROW 9.81 COL 77 HELP
          "Previous" WIDGET-ID 278
     btnAdd AT ROW 9.81 COL 15 HELP
          "Add" WIDGET-ID 20
     btnCancel AT ROW 9.81 COL 47 HELP
          "Cancel" WIDGET-ID 28
     btnCopy AT ROW 9.81 COL 23 HELP
          "Copy" WIDGET-ID 24
     btnDefaults AT ROW 9.81 COL 58 HELP
          "Restore Defaults" WIDGET-ID 34
     btnDelete AT ROW 9.81 COL 31 HELP
          "Delete" WIDGET-ID 26
     btnForms AT ROW 9.81 COL 104 HELP
          "Forms" WIDGET-ID 54
     btnReset AT ROW 9.81 COL 39 HELP
          "Reset" WIDGET-ID 22
     btnUpdate AT ROW 9.81 COL 7 HELP
          "Update/Save" WIDGET-ID 18
     transPanel AT ROW 9.57 COL 6 WIDGET-ID 16
     transPanel-2 AT ROW 9.57 COL 57 WIDGET-ID 32
     transPanel-5 AT ROW 9.57 COL 103 WIDGET-ID 56
     transPanel-8 AT ROW 9.57 COL 68 WIDGET-ID 280
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40 ROW 10.52
         SIZE 117 BY 12.14
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 400.

DEFINE FRAME formsFrame
     cSysCtrlName AT ROW 1.24 COL 11 COLON-ALIGNED WIDGET-ID 4
     cSysCtrlDescrip AT ROW 1.24 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     cSysCtrlDataType AT ROW 2.43 COL 11 COLON-ALIGNED WIDGET-ID 32
     cSysCtrFieldlDescrip AT ROW 2.43 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     sysCtrlShipToBrowse AT ROW 3.62 COL 2 WIDGET-ID 800
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17 ROW 1
         SIZE 144 BY 28.57
         FGCOLOR 1 
         TITLE "System Control Parameter Forms" WIDGET-ID 700.

DEFINE FRAME viewFormFrame
     sys-ctrl-shipto.cust-vend AT ROW 1.24 COL 24 NO-LABEL WIDGET-ID 48
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Customer", yes,
"Vendor", no
          SIZE 26 BY 1
     btnFirst-2 AT ROW 9.57 COL 109 HELP
          "First" WIDGET-ID 62
     sys-ctrl-shipto.cust-vend-no AT ROW 2.43 COL 21 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
          BGCOLOR 15 
     type_name AT ROW 2.43 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     sys-ctrl-shipto.ship-id AT ROW 3.62 COL 21 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
          BGCOLOR 15 
     ship_name AT ROW 3.62 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     sys-ctrl-shipto.descrip AT ROW 4.81 COL 21 COLON-ALIGNED WIDGET-ID 46 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.char-fld AT ROW 6 COL 21 COLON-ALIGNED WIDGET-ID 30
          LABEL "Character" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.date-fld AT ROW 7.19 COL 21 COLON-ALIGNED WIDGET-ID 36
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.dec-fld AT ROW 8.38 COL 21 COLON-ALIGNED WIDGET-ID 38
          LABEL "Decimal"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.int-fld AT ROW 9.57 COL 21 COLON-ALIGNED WIDGET-ID 40
          LABEL "Integer"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     cLogLabel AT ROW 10.76 COL 15.2 WIDGET-ID 78
     sys-ctrl-shipto.log-fld AT ROW 10.76 COL 24 NO-LABEL WIDGET-ID 52
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Yes", yes,
"No", no,
"Unknown", ?
          SIZE 28 BY 1
     btnForms-2 AT ROW 1 COL 138 HELP
          "Close" WIDGET-ID 72
     btnLast-2 AT ROW 9.57 COL 133 HELP
          "Last" WIDGET-ID 68
     btnNext-2 AT ROW 9.57 COL 125 HELP
          "Next" WIDGET-ID 66
     btnPrev-2 AT ROW 9.57 COL 117 HELP
          "Previous" WIDGET-ID 64
     btnAdd-2 AT ROW 9.57 COL 66 HELP
          "Add" WIDGET-ID 20
     btnCancel-2 AT ROW 9.57 COL 98 HELP
          "Cancel" WIDGET-ID 28
     btnCopy-2 AT ROW 9.57 COL 74 HELP
          "Copy" WIDGET-ID 24
     btnDelete-2 AT ROW 9.57 COL 82 HELP
          "Delete" WIDGET-ID 26
     btnReset-2 AT ROW 9.57 COL 90 HELP
          "Reset" WIDGET-ID 22
     btnUpdate-2 AT ROW 9.57 COL 58 HELP
          "Update/Save" WIDGET-ID 18
     btnCalendar-2 AT ROW 7.19 COL 39 WIDGET-ID 272
     "Type:" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 1.24 COL 17 WIDGET-ID 58
     transPanel-3 AT ROW 9.33 COL 57 WIDGET-ID 16
     transPanel-4 AT ROW 9.33 COL 108 WIDGET-ID 60
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 16.48
         SIZE 142 BY 11.86
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 900.

DEFINE FRAME searchFrame
     btnSearch AT ROW 1 COL 1 HELP
          "Advanced Search" WIDGET-ID 40
     cNameFilter AT ROW 1.24 COL 21 COLON-ALIGNED HELP
          "Name Search" WIDGET-ID 2
     cFieldDescripFilter AT ROW 2.19 COL 21 COLON-ALIGNED HELP
          "Description Search" WIDGET-ID 44
     cValueFilter AT ROW 3.14 COL 21 COLON-ALIGNED HELP
          "Value Search" WIDGET-ID 8
     cDescripFilter AT ROW 4.1 COL 21 COLON-ALIGNED HELP
          "Description Search" WIDGET-ID 6
     cModuleFilter AT ROW 5.05 COL 21 COLON-ALIGNED HELP
          "Module Search" WIDGET-ID 12
     cTypeCodeFilter AT ROW 6 COL 21 COLON-ALIGNED HELP
          "Type Search" WIDGET-ID 10
     cCategoryFilter AT ROW 6.95 COL 21 COLON-ALIGNED HELP
          "Category Search" WIDGET-ID 14
     cSubCategoryFilter AT ROW 7.91 COL 21 COLON-ALIGNED HELP
          "Sub Category Search" WIDGET-ID 16
     cTableSourceFilter AT ROW 8.86 COL 21 COLON-ALIGNED HELP
          "Table Source Search" WIDGET-ID 46
     cFieldSourceFilter AT ROW 9.81 COL 21 COLON-ALIGNED HELP
          "Field Source Search" WIDGET-ID 48
     cDataTypeFilter AT ROW 10.76 COL 21 COLON-ALIGNED HELP
          "Data Type Search" WIDGET-ID 50
     btnClear AT ROW 10.76 COL 64 HELP
          "Clear Search Filters" WIDGET-ID 42
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72 BY 10.95
         FGCOLOR 1 FONT 6 WIDGET-ID 600.

DEFINE FRAME filterFrame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 38 BY 26
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 8 "Category / SubCategory" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "System Control Parameters"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME filterFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME formsFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME searchFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME viewFormFrame:FRAME = FRAME formsFrame:HANDLE
       FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME filterFrame:MOVE-AFTER-TAB-ITEM (svFocus:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME filterFrame:MOVE-BEFORE-TAB-ITEM (FRAME searchFrame:HANDLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB sysCtrlBrowse searchBar DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnExport IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnImport IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svFocus IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       sysCtrlBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       sysCtrlBrowse:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       sysCtrlBrowse:ROW-RESIZABLE IN FRAME DEFAULT-FRAME          = TRUE
       sysCtrlBrowse:SEPARATOR-FGCOLOR IN FRAME DEFAULT-FRAME      = 1.

/* SETTINGS FOR FRAME filterFrame
                                                                        */
/* SETTINGS FOR FRAME formsFrame
                                                                        */
ASSIGN XXTABVALXX = FRAME viewFormFrame:MOVE-AFTER-TAB-ITEM (sysCtrlShipToBrowse:HANDLE IN FRAME formsFrame)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB sysCtrlShipToBrowse cSysCtrFieldlDescrip formsFrame */
ASSIGN 
       FRAME formsFrame:HIDDEN           = TRUE
       FRAME formsFrame:MOVABLE          = TRUE.

ASSIGN 
       cSysCtrFieldlDescrip:READ-ONLY IN FRAME formsFrame        = TRUE.

ASSIGN 
       cSysCtrlDataType:READ-ONLY IN FRAME formsFrame        = TRUE.

ASSIGN 
       cSysCtrlDescrip:READ-ONLY IN FRAME formsFrame        = TRUE.

ASSIGN 
       cSysCtrlName:READ-ONLY IN FRAME formsFrame        = TRUE.

ASSIGN 
       sysCtrlShipToBrowse:SEPARATOR-FGCOLOR IN FRAME formsFrame      = 1.

/* SETTINGS FOR FRAME searchFrame
                                                                        */
ASSIGN 
       FRAME searchFrame:HIDDEN           = TRUE
       FRAME searchFrame:SELECTABLE       = TRUE
       FRAME searchFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnClear IN FRAME searchFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCategoryFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cDataTypeFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cDescripFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cFieldDescripFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cFieldSourceFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cModuleFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cNameFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cSubCategoryFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cTableSourceFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cTypeCodeFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FILL-IN cValueFilter IN FRAME searchFrame
   6                                                                    */
/* SETTINGS FOR FRAME viewFormFrame
                                                                        */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnCancel-2 IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnReset-2 IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.char-fld IN FRAME viewFormFrame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN cLogLabel IN FRAME viewFormFrame
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       cLogLabel:READ-ONLY IN FRAME viewFormFrame        = TRUE.

/* SETTINGS FOR RADIO-SET sys-ctrl-shipto.cust-vend IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.cust-vend-no IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.date-fld IN FRAME viewFormFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.dec-fld IN FRAME viewFormFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.descrip IN FRAME viewFormFrame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.int-fld IN FRAME viewFormFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET sys-ctrl-shipto.log-fld IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.ship-id IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_name IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-3 IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-4 IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN type_name IN FRAME viewFormFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:HIDDEN           = TRUE
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME viewFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnCalendar-1:HIDDEN IN FRAME viewFrame           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnClose IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCopy IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDefaults IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnForms IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME viewFrame
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN cCategory IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR COMBO-BOX cDataType IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN cDescrip IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cFieldDefault IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cFieldDescrip IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cfieldSource IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN cFieldValue IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cModule IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cName IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN cSubcategory IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN ctableSource IN FRAME viewFrame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN cTypeCode IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN hDate IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       hDate:HIDDEN IN FRAME viewFrame           = TRUE.

/* SETTINGS FOR FILL-IN hDecimal IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       hDecimal:HIDDEN IN FRAME viewFrame           = TRUE.

/* SETTINGS FOR FILL-IN hInteger IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       hInteger:HIDDEN IN FRAME viewFrame           = TRUE.

/* SETTINGS FOR RADIO-SET hLogical IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       hLogical:HIDDEN IN FRAME viewFrame           = TRUE.

/* SETTINGS FOR FILL-IN iSecurityLevelDefault IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR FILL-IN iSecurityLevelUser IN FRAME viewFrame
   NO-ENABLE 4 5                                                        */
/* SETTINGS FOR RECTANGLE transPanel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-2 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-5 IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE transPanel-8 IN FRAME viewFrame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME filterFrame
/* Query rebuild information for FRAME filterFrame
     _Query            is NOT OPENED
*/  /* FRAME filterFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME formsFrame
/* Query rebuild information for FRAME formsFrame
     _Query            is NOT OPENED
*/  /* FRAME formsFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME searchFrame
/* Query rebuild information for FRAME searchFrame
     _Query            is NOT OPENED
*/  /* FRAME searchFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sysCtrlBrowse
/* Query rebuild information for BROWSE sysCtrlBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrl
WHERE (ttSysCtrl.category EQ cFilter
   OR  cFilter EQ "ALL")
  AND (ttSysCtrl.subCategory EQ cSubFilter
   OR  cSubFilter EQ "ALL")
  AND ttSysCtrl.name         MATCHES "*" + cNameFilter         + "*"
  AND ttSysCtrl.fieldDescrip MATCHES "*" + cFieldDescripFilter + "*"
  AND ttSysCtrl.fieldValue   MATCHES "*" + cValueFilter        + "*"
  AND ttSysCtrl.descrip      MATCHES "*" + cDescripFilter      + "*"
  AND ttSysCtrl.module       MATCHES "*" + cModuleFilter       + "*"
  AND ttSysCtrl.typeCode     MATCHES "*" + cTypeCodeFilter     + "*"
  AND ttSysCtrl.category     MATCHES "*" + cCategoryFilter     + "*"
  AND ttSysCtrl.subCategory  MATCHES "*" + cSubCategoryFilter  + "*"
  AND ttSysCtrl.tableSource  MATCHES "*" + cTableSourceFilter  + "*"
  AND ttSysCtrl.fieldSource  MATCHES "*" + cFieldSourceFilter  + "*"
  AND ttSysCtrl.dataType     MATCHES "*" + cDataTypeFilter     + "*"
  AND ttSysCtrl.allData      MATCHES "*" + searchBar           + "*"
  ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE sysCtrlBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sysCtrlShipToBrowse
/* Query rebuild information for BROWSE sysCtrlShipToBrowse
     _TblList          = "ASI.sys-ctrl-shipto"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "sys-ctrl-shipto.company EQ g_company
AND sys-ctrl-shipto.name EQ ttSysCtrl.name"
     _FldNameList[1]   = ASI.sys-ctrl-shipto.cust-vend
     _FldNameList[2]   = ASI.sys-ctrl-shipto.descrip
     _FldNameList[3]   = ASI.sys-ctrl-shipto.cust-vend-no
     _FldNameList[4]   = ASI.sys-ctrl-shipto.ship-id
     _FldNameList[5]   > ASI.sys-ctrl-shipto.char-fld
"sys-ctrl-shipto.char-fld" "Character Value" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.sys-ctrl-shipto.date-fld
"sys-ctrl-shipto.date-fld" "Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.sys-ctrl-shipto.dec-fld
"sys-ctrl-shipto.dec-fld" "Decimal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.sys-ctrl-shipto.int-fld
"sys-ctrl-shipto.int-fld" "Integer" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.sys-ctrl-shipto.log-fld
"sys-ctrl-shipto.log-fld" "Log" ? "logical" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE sysCtrlShipToBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFormFrame
/* Query rebuild information for FRAME viewFormFrame
     _TblList          = "ASI.sys-ctrl-shipto"
     _Query            is NOT OPENED
*/  /* FRAME viewFormFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "ASI.sys-ctrl"
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* System Control Parameters */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* System Control Parameters */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* System Control Parameters */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnAdd-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd-2 C-Win
ON CHOOSE OF btnAdd-2 IN FRAME viewFormFrame /* Add */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME viewFrame
DO:
    {methods/btnCalendar.i hDate}
    APPLY "LEAVE":U TO hDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME viewFormFrame
DO:
    {methods/btnCalendar.i sys-ctrl-shipto.date-fld}
    APPLY "LEAVE":U TO sys-ctrl-shipto.date-fld.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnCancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel-2 C-Win
ON CHOOSE OF btnCancel-2 IN FRAME viewFormFrame /* Cancel */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME searchFrame /* Clear */
DO:
    RUN pClearSearchFilters.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME viewFrame /* Close */
DO:
    SELF:MOVE-TO-BOTTOM().
    HIDE FRAME viewFrame.
    VIEW FRAME searchFrame.
    FRAME searchFrame:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnCopy-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy-2 C-Win
ON CHOOSE OF btnCopy-2 IN FRAME viewFormFrame /* Copy */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefaults C-Win
ON CHOOSE OF btnDefaults IN FRAME viewFrame /* Defaults */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnDelete-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete-2 C-Win
ON CHOOSE OF btnDelete-2 IN FRAME viewFormFrame /* Delete */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExport C-Win
ON CHOOSE OF btnExport IN FRAME DEFAULT-FRAME /* Export */
DO:
    MESSAGE
        "Export Function Not Yet Implemented"
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnFirst-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst-1 C-Win
ON CHOOSE OF btnFirst-1 IN FRAME viewFrame /* First */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnFirst-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFirst-2 C-Win
ON CHOOSE OF btnFirst-2 IN FRAME viewFormFrame /* First */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnForms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForms C-Win
ON CHOOSE OF btnForms IN FRAME viewFrame /* Forms */
DO:
    DEFINE VARIABLE dCol AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dRow AS DECIMAL NO-UNDO.
    
    ASSIGN
        dCol = {&WINDOW-NAME}:WIDTH
             - FRAME formsFrame:WIDTH + 1
        dRow = {&WINDOW-NAME}:HEIGHT
             - FRAME formsFrame:HEIGHT + 1
             .
    IF dCol LT 1 THEN dCol = 1.
    IF dRow LT 1 THEN dRow = 1.
    ASSIGN
        FRAME formsFrame:COL = dCol
        FRAME formsFrame:ROW = dRow
        .
    RUN pForms.
    VIEW FRAME searchFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnForms-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForms-2 C-Win
ON CHOOSE OF btnForms-2 IN FRAME viewFormFrame /* Close */
DO:
    SELF:MOVE-TO-BOTTOM().
    HIDE FRAME formsFrame.
    ASSIGN
        FRAME formsFrame:COL = 1
        FRAME formsFrame:ROW = 1
        .
    VIEW FRAME searchFrame.
    FRAME searchFrame:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport C-Win
ON CHOOSE OF btnImport IN FRAME DEFAULT-FRAME /* Import */
DO:
    MESSAGE
        "Import Function Not Yet Implemented"
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnLast-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast-1 C-Win
ON CHOOSE OF btnLast-1 IN FRAME viewFrame /* Last */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnLast-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLast-2 C-Win
ON CHOOSE OF btnLast-2 IN FRAME viewFormFrame /* Last */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnNext-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext-1 C-Win
ON CHOOSE OF btnNext-1 IN FRAME viewFrame /* Next */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnNext-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNext-2 C-Win
ON CHOOSE OF btnNext-2 IN FRAME viewFormFrame /* Next */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnPrev-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev-1 C-Win
ON CHOOSE OF btnPrev-1 IN FRAME viewFrame /* Previous */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnPrev-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrev-2 C-Win
ON CHOOSE OF btnPrev-2 IN FRAME viewFormFrame /* Previous */
DO:
    RUN pNavPanel (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnReset-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset-2 C-Win
ON CHOOSE OF btnReset-2 IN FRAME viewFormFrame /* Reset */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults C-Win
ON CHOOSE OF btnRestoreDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
    RUN pGetSettings ("_default").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch C-Win
ON CHOOSE OF btnSearch IN FRAME searchFrame
DO:
    ASSIGN
        FRAME searchFrame:HIDDEN = YES
        lSearchOpen = NOT lSearchOpen
        .
    DO WITH FRAME searchFrame:
        IF lSearchOpen THEN DO:
            ASSIGN
                FRAME searchFrame:VIRTUAL-HEIGHT = btnClear:ROW + btnClear:HEIGHT - .86
                FRAME searchFrame:VIRTUAL-WIDTH  = btnClear:COL + btnClear:WIDTH  - .4
                FRAME searchFrame:HEIGHT = FRAME searchFrame:VIRTUAL-HEIGHT
                FRAME searchFrame:WIDTH  = FRAME searchFrame:VIRTUAL-WIDTH
                .
            VIEW {&searchFilters}.
            ENABLE {&searchFilters} btnClear.
            APPLY "ENTRY":U TO cNameFilter.
        END. /* if searchopen */
        ELSE DO:
            DISABLE {&searchFilters} btnClear.
            HIDE {&searchFilters} btnClear.
            ASSIGN
                FRAME searchFrame:VIRTUAL-HEIGHT = btnSearch:HEIGHT + .1
                FRAME searchFrame:VIRTUAL-WIDTH  = btnSearch:WIDTH + .3
                FRAME searchFrame:HEIGHT = FRAME searchFrame:VIRTUAL-HEIGHT
                FRAME searchFrame:WIDTH  = FRAME searchFrame:VIRTUAL-WIDTH
                .
        END. /* else */
    END. /* with frame */
    FRAME searchFrame:HIDDEN = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSortMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMove C-Win
ON CHOOSE OF btnSortMove IN FRAME DEFAULT-FRAME /* Sort/Move */
DO:
    ASSIGN
        BROWSE sysCtrlBrowse:COLUMN-MOVABLE = lSortMove
        lSortMove = NOT lSortMove
        .
    SELF:LOAD-IMAGE("Graphics/16x16/"
        + IF lSortMove THEN "sort_up_down2.gif"
          ELSE "left_right_arrows.gif")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME btnUpdate-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate-2 C-Win
ON CHOOSE OF btnUpdate-2 IN FRAME viewFormFrame /* Update */
DO:
    RUN pCRUD-2 (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME cCategoryFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategoryFilter C-Win
ON VALUE-CHANGED OF cCategoryFilter IN FRAME searchFrame /* Category */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME cDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDataType C-Win
ON VALUE-CHANGED OF cDataType IN FRAME viewFrame /* Data Type */
DO:
    ASSIGN
        {&SELF-NAME}
        cFieldSource = IF {&SELF-NAME} EQ "Character" THEN "char-fld"
                  ELSE IF {&SELF-NAME} EQ "Date"      THEN "date-fld"
                  ELSE IF {&SELF-NAME} EQ "Decimal"   THEN "dec-fld"
                  ELSE IF {&SELF-NAME} EQ "Integer"   THEN "int-fld"
                  ELSE IF {&SELF-NAME} EQ "Logical"   THEN "log-fld"
                  ELSE ""
                  .
    DISPLAY cFieldSource WITH FRAME viewFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME cDataTypeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDataTypeFilter C-Win
ON VALUE-CHANGED OF cDataTypeFilter IN FRAME searchFrame /* Data Type */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDescripFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDescripFilter C-Win
ON VALUE-CHANGED OF cDescripFilter IN FRAME searchFrame /* Description */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFieldDescripFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldDescripFilter C-Win
ON VALUE-CHANGED OF cFieldDescripFilter IN FRAME searchFrame /* Value Description */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFieldSourceFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldSourceFilter C-Win
ON VALUE-CHANGED OF cFieldSourceFilter IN FRAME searchFrame /* Field Source */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME cFieldValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldValue C-Win
ON ENTRY OF cFieldValue IN FRAME viewFrame /* Value */
DO:
    DEFINE VARIABLE ls-name-value AS CHARACTER FORMAT "x(100)" NO-UNDO. 

    STATUS INPUT "".
    IF cDataType:SCREEN-VALUE EQ "Character" THEN DO:
        IF CAN-DO(name-fld-list,ttSysCtrl.name) THEN DO:
            ls-name-value = str-init[LOOKUP(ttSysCtrl.name, name-fld-list)].
            STATUS INPUT ls-name-value.
        END.
    END. /* if character */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldValue C-Win
ON HELP OF cFieldValue IN FRAME viewFrame /* Value */
DO:
    DEFINE VARIABLE cNameValue AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cCharValue AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cLoc     LIKE fg-bin.loc     NO-UNDO.
    DEFINE VARIABLE cLocBin  LIKE fg-bin.loc-bin NO-UNDO.
    DEFINE VARIABLE cCharFld   AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cRtnValue  AS CHARACTER      NO-UNDO.

    IF cDataType:SCREEN-VALUE EQ "Character" THEN DO:
        cNameValue = ttSysCtrl.name.
        IF LOOKUP(cNameValue, gvcMultiSelect) GT 0 AND
           LOOKUP(cNameValue,name-fld-list)   GT 0 THEN DO:
            RUN windows/w-syschr.w (
                cNameValue,
                cFieldValue:SCREEN-VALUE,
                str-init[LOOKUP(cNameValue,name-fld-list)],
                OUTPUT cCharValue
                ) .
            cFieldValue:SCREEN-VALUE = cCharValue.
            RETURN NO-APPLY.
        END.
        ELSE
        CASE cNameValue:
            WHEN "FGWHSBIN" THEN DO:
                IF cFieldValue:SCREEN-VALUE EQ "AUTOPOST" THEN
                ASSIGN
                    cLoc    = ""
                    cLocBin = ""
                    .
                ELSE
                ASSIGN
                    cLoc    = SUBSTR(cFieldValue:SCREEN-VALUE,1,5)
                    cLocBin = SUBSTR(cFieldValue:SCREEN-VALUE,6,8)
                    .
                RUN windows/l-fgbin.w (gcompany, cLoc, cLocBin, OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = STRING(ENTRY(2,cCharValue),"x(5)") + TRIM(ENTRY(1,cCharValue)).
            END. /* fgwhsbin */
            WHEN "TSPOSTFG" THEN DO:
                RUN windows/l-mach2.w (gcompany,g_loc,"",OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = cCharValue.
                RETURN NO-APPLY.
            END. /* tspostfg */
            WHEN "RMWHSBIN" THEN DO:
                IF cFieldValue:SCREEN-VALUE EQ "RMITEM" THEN
                ASSIGN
                    cLoc    = ""
                    cLocBin = ""
                    .
                ELSE
                ASSIGN
                    cLoc    = SUBSTR(cFieldValue:SCREEN-VALUE,1,5)
                    cLocBin = SUBSTR(cFieldValue:SCREEN-VALUE,6,8)
                    .
                RUN windows/l-rmbin.w (gcompany,cLoc,cLocBin,OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = STRING(ENTRY(2,cCharValue),"x(5)") + TRIM(ENTRY(1,cCharValue)).
            END. /* rmwhsbin */
            WHEN "FGMASTER" THEN DO:
                RUN windows/l-itemfg.w (gcompany,g_loc,"",OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = ENTRY(1,cCharValue).
                RETURN NO-APPLY.
            END. /* fgmaster */
            WHEN "CASLABEL" THEN DO:
                cCharFld = cFieldValue:SCREEN-VALUE.
                RUN sys\ref\char-fld-help.w (gcompany, cCharFld, OUTPUT cRtnValue).
                IF TRIM(cRtnValue) NE "" THEN
                cFieldValue:SCREEN-VALUE = cRtnValue.
            END. /* caslabel */
            WHEN "GRAPHIC" THEN DO:
                ASSIGN 
                    cCharFld = cFieldValue:SCREEN-VALUE
                    cCharFld = IF TRIM(cCharFld) EQ "" THEN "GRAPHIC"
                               ELSE TRIM(cCharFld)
                                . 
                RUN sys\ref\char-fld-help.w (gcompany, cCharFld, OUTPUT cRtnValue).
                IF TRIM(cRtnValue) NE "" THEN
                cFieldValue:SCREEN-VALUE = cRtnValue.      
            END. /* graphic */
            WHEN "BARDIR" THEN DO:
                MESSAGE
                    "Do you want to display Xprint Values.... "
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE ""
                UPDATE lChoice AS LOGICAL.
                IF NOT lChoice THEN DO:
                    cCharFld = cFieldDescrip:SCREEN-VALUE.
                    RUN sys\ref\char-fld-help.w (gcompany, cCharFld, OUTPUT cRtnValue).
                    IF TRIM(cRtnValue) NE "" THEN
                    cFieldValue:SCREEN-VALUE = cRtnValue. 
                END. /* if not lchoice */
                ELSE DO:
                    RUN windows/l-typxpr.w (OUTPUT cCharValue).
                    IF cCharValue NE "" THEN
                    cFieldValue:SCREEN-VALUE = cCharValue.
                    RETURN NO-APPLY.    
                END. /* else */
            END. /* bardir */
            WHEN "SALESREP" THEN DO:
                RUN windows/l-sman.w (gcompany,OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = ENTRY(1,cCharValue).
                RETURN NO-APPLY.
            END. /* salesrep */
            WHEN "BolPrint" THEN DO:
                RUN windows/l-fgbin2.w (gcompany,"","",OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = ENTRY(1,cCharValue).
                RETURN NO-APPLY.
            END. /* bolprint */
            OTHERWISE DO:
                RUN windows/l-syschr.w (gcompany, cNameValue,FOCUS:SCREEN-VALUE, OUTPUT cCharValue).
                IF cCharValue NE "" THEN
                cFieldValue:SCREEN-VALUE = ENTRY(1,cCharValue).
            END. /* otherwise */
        END CASE.
        RETURN NO-APPLY.
    END. /* if character */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFieldValue C-Win
ON LEAVE OF cFieldValue IN FRAME viewFrame /* Value */
DO:
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    CASE ttSysCtrl.dataType:
        WHEN "Character" THEN DO:
            STATUS INPUT "".
            IF LASTKEY NE -1 THEN DO:
                RUN valid-char-fld NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END.
        END. /* character */
        WHEN "Logical" THEN DO:
            CASE ttSysCtrl.name:
                WHEN "RELCREDT" THEN
                cMessage = "Credit Checks for Past Due Invoices must be purchased, please call ASI."  .
                WHEN "SalesBudget" THEN
                cMessage = "Budget Report are available for purchase, please call ASI."  .     
            END CASE.
            IF LASTKEY NE -1 THEN DO:
                RUN valid-log-fld (SELF) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            END. /* if lastkey */
            IF ttSysCtrl.name EQ "RELCREDT" OR
               ttSysCtrl.name EQ "SalesBudget" THEN DO:       
                RUN check-flg.
                IF NOT lValid AND cFieldValue:SCREEN-VALUE EQ "NO" THEN DO:
                    MESSAGE 
                        cMessage
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN.
                END. /* if not valid */
            END. /* if relcredt slaesbudget */
        END. /* logical */
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME sys-ctrl-shipto.char-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld C-Win
ON ENTRY OF sys-ctrl-shipto.char-fld IN FRAME viewFormFrame /* Character */
DO:
    DEFINE VARIABLE cNameValue AS CHARACTER FORMAT "x(100)" NO-UNDO.

    STATUS INPUT "".
    IF CAN-DO(name-fld-list,ttSysCtrl.name) THEN DO:
        cNameValue = str-init[LOOKUP(ttSysCtrl.name,name-fld-list)].
        STATUS INPUT cNameValue.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld C-Win
ON HELP OF sys-ctrl-shipto.char-fld IN FRAME viewFormFrame /* Character */
DO:
    DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCharFieldValue AS CHARACTER NO-UNDO INIT "c:\".

    DO WITH FRAME viewFormFrame:
        CASE ttSysCtrl.name:
            WHEN "RELMERGE" THEN DO:
                RUN windows/l-sysfchr.w (g_company,ttSysCtrl.name, FOCUS:SCREEN-VALUE, OUTPUT cValue).
                IF cValue NE "" THEN
                sys-ctrl-shipto.char-fld:SCREEN-VALUE = STRING(cValue).
                RETURN NO-APPLY.
            END.
            WHEN "CINVOICE" THEN DO:
                RUN windows/l-sysfchr.w (g_company,ttSysCtrl.name, FOCUS:SCREEN-VALUE, OUTPUT cValue).
                IF cValue NE "" THEN
                sys-ctrl-shipto.char-fld:SCREEN-VALUE = ENTRY(1,cValue).
                RETURN NO-APPLY.
            END.
            WHEN "BOLPrint" THEN DO: 
                RUN windows/l-fgbin2.w (g_company, "", FOCUS:SCREEN-VALUE, OUTPUT cValue).
                IF cValue NE "" THEN
                sys-ctrl-shipto.char-fld:SCREEN-VALUE = ENTRY(1,cValue).
                RETURN NO-APPLY.
            END.
            WHEN "CASLABEL" THEN DO:          
                ASSIGN 
                    cCharField = ""
                    cCharField = sys-ctrl-shipto.char-fld:SCREEN-VALUE
                    .
                IF cCharField NE "" THEN
                RUN sys\ref\char-fld-help.w (g_company, cCharField, OUTPUT cCharFieldValue).
                ELSE DO:
                    FIND FIRST sys-ctrl NO-LOCK
                         WHERE sys-ctrl.company EQ gcompany
                           AND sys-ctrl.name    EQ "CASLABEL"
                         NO-ERROR.
                    IF AVAILABLE sys-ctrl THEN
                    cCharField = TRIM(sys-ctrl.char-fld).
                    RUN sys\ref\char-fld-help.w (g_company, cCharField, OUTPUT cCharFieldValue).
                END.

                IF TRIM(cCharFieldValue) NE "" THEN
                sys-ctrl-shipto.char-fld:SCREEN-VALUE = cCharFieldValue.
            END. /* gdm - 11050804 end */
            WHEN "BARDIR" THEN DO:
                MESSAGE
                    "Do you want to display Xprint Values.... "
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE ""
                UPDATE lChoiceXprnt AS LOGICAL.
                IF NOT lChoiceXprnt THEN DO:
                    ASSIGN 
                        cCharField = ""
                        cCharField = TRIM(sys-ctrl-shipto.char-fld:SCREEN-VALUE)
                        .
                    RELEASE sys-ctrl.
                    IF cCharField EQ "" THEN
                    FIND FIRST sys-ctrl NO-LOCK 
                         WHERE sys-ctrl.company EQ gcompany
                           AND sys-ctrl.name    EQ "BARDIR"
                         NO-ERROR.
                    IF AVAILABLE sys-ctrl THEN
                    cCharField = TRIM(sys-ctrl.char-fld).
                    RUN sys\ref\char-fld-help.w (g_company, cCharField, OUTPUT cCharFieldValue).
                    IF TRIM(cCharFieldValue) NE "" THEN
                    sys-ctrl-shipto.char-fld:SCREEN-VALUE = cCharFieldValue.
                END.
                ELSE DO:
                    RUN windows/l-typxpr.w (OUTPUT cValue).
                    IF cValue NE "" THEN
                    sys-ctrl-shipto.char-fld:SCREEN-VALUE = cValue.
                    RETURN NO-APPLY.
                END.
            END.
            OTHERWISE 
            IF CAN-DO(cValidateList,ttSysCtrl.name) THEN DO:
                &Scoped-define nameField ttSysCtrl.name
                &Scoped-define tableName sys-ctrl-shipto
                {sys/ref/char-fld-help.i}
            END.
        END CASE.
    END. /* with frame */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld C-Win
ON LEAVE OF sys-ctrl-shipto.char-fld IN FRAME viewFormFrame /* Character */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-char-fld-2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END. /* if lastkey */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cLogLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cLogLabel C-Win
ON ENTRY OF cLogLabel IN FRAME viewFormFrame /* Logical */
DO:
    IF sys-ctrl-shipto.log-fld:SENSITIVE THEN
    APPLY "ENTRY":U TO sys-ctrl-shipto.log-fld.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME cModuleFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cModuleFilter C-Win
ON VALUE-CHANGED OF cModuleFilter IN FRAME searchFrame /* Module */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cNameFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cNameFilter C-Win
ON VALUE-CHANGED OF cNameFilter IN FRAME searchFrame /* Name */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSubCategoryFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSubCategoryFilter C-Win
ON VALUE-CHANGED OF cSubCategoryFilter IN FRAME searchFrame /* Sub Category */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME formsFrame
&Scoped-define SELF-NAME cSysCtrFieldlDescrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSysCtrFieldlDescrip C-Win
ON ENTRY OF cSysCtrFieldlDescrip IN FRAME formsFrame
DO:
    APPLY "ENTRY":U TO sysCtrlShipToBrowse.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSysCtrlDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSysCtrlDataType C-Win
ON ENTRY OF cSysCtrlDataType IN FRAME formsFrame /* DataType */
DO:
    APPLY "ENTRY":U TO sysCtrlShipToBrowse.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSysCtrlDescrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSysCtrlDescrip C-Win
ON ENTRY OF cSysCtrlDescrip IN FRAME formsFrame
DO:
    APPLY "ENTRY":U TO sysCtrlShipToBrowse.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSysCtrlName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSysCtrlName C-Win
ON ENTRY OF cSysCtrlName IN FRAME formsFrame /* Name */
DO:
    APPLY "ENTRY":U TO sysCtrlShipToBrowse.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME cTableSourceFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cTableSourceFilter C-Win
ON VALUE-CHANGED OF cTableSourceFilter IN FRAME searchFrame /* Table Source */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cTypeCodeFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cTypeCodeFilter C-Win
ON VALUE-CHANGED OF cTypeCodeFilter IN FRAME searchFrame /* Type */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME sys-ctrl-shipto.cust-vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend C-Win
ON VALUE-CHANGED OF sys-ctrl-shipto.cust-vend IN FRAME viewFormFrame /* Cust/Vend */
DO:
    ASSIGN
        sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE = ""
        sys-ctrl-shipto.ship-id:SCREEN-VALUE = ""
        type_name:SCREEN-VALUE = ""
        ship_name:SCREEN-VALUE = ""
        sys-ctrl-shipto.ship-id:SENSITIVE = SELF:SCREEN-VALUE EQ "YES"
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl-shipto.cust-vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend-no C-Win
ON HELP OF sys-ctrl-shipto.cust-vend-no IN FRAME viewFormFrame /* Cust or Vend */
DO:
    IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ "YES" THEN
    RUN lookups/cust.p.
    ELSE
    RUN lookups/vend.p.
    SELF:SCREEN-VALUE = g_lookup-var.
    APPLY "ENTRY":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend-no C-Win
ON LEAVE OF sys-ctrl-shipto.cust-vend-no IN FRAME viewFormFrame /* Cust or Vend */
DO:
    IF SELF:SCREEN-VALUE NE "" THEN DO:
        IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ "Yes" THEN DO:
            {methods/entryerr.i
                &can-find="FIRST cust 
                           WHERE cust.company EQ gcompany
                             AND cust.cust-no EQ sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE"
                &error-message="Invalid Customer"}
        END.
        ELSE DO:
            {methods/entryerr.i
                &can-find="FIRST vend 
                           WHERE vend.company EQ gcompany
                             AND vend.vend-no EQ sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE"
                &error-message="Invalid Vendor"}
        END.
    END.
    {methods/dispflds.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME searchFrame
&Scoped-define SELF-NAME cValueFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cValueFilter C-Win
ON VALUE-CHANGED OF cValueFilter IN FRAME searchFrame /* Value */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME sys-ctrl-shipto.date-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.date-fld C-Win
ON HELP OF sys-ctrl-shipto.date-fld IN FRAME viewFormFrame /* Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME hDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hDate C-Win
ON HELP OF hDate IN FRAME viewFrame
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hDate C-Win
ON LEAVE OF hDate IN FRAME viewFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cFieldValue:SCREEN-VALUE = STRING({&SELF-NAME},"99/99/9999")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hDecimal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hDecimal C-Win
ON VALUE-CHANGED OF hDecimal IN FRAME viewFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cFieldValue:SCREEN-VALUE = LEFT-TRIM(STRING({&SELF-NAME},"->>,>>9.99"))
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hInteger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hInteger C-Win
ON VALUE-CHANGED OF hInteger IN FRAME viewFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cFieldValue:SCREEN-VALUE = LEFT-TRIM(STRING({&SELF-NAME},"->,>>>,>>9"))
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hLogical
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hLogical C-Win
ON VALUE-CHANGED OF hLogical IN FRAME viewFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cFieldValue:SCREEN-VALUE = STRING({&SELF-NAME})
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME sys-ctrl-shipto.log-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.log-fld C-Win
ON VALUE-CHANGED OF sys-ctrl-shipto.log-fld IN FRAME viewFormFrame /* log-fld */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN valid-log-fld (SELF) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar C-Win
ON VALUE-CHANGED OF searchBar IN FRAME DEFAULT-FRAME /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFormFrame
&Scoped-define SELF-NAME sys-ctrl-shipto.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.ship-id C-Win
ON HELP OF sys-ctrl-shipto.ship-id IN FRAME viewFormFrame /* Ship To ID */
DO:
    IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ "YES" THEN DO:
        RUN windows/l-shipto.w (
            g_company,
            g_loc,
            sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE,
            SELF:SCREEN-VALUE,
            OUTPUT g_lookup-var
            ).
        g_lookup-var = ENTRY(1,g_lookup-var).
    END.
    ELSE
    RUN lookups/vend.p.
    SELF:SCREEN-VALUE = g_lookup-var.
    APPLY "ENTRY":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.ship-id C-Win
ON LEAVE OF sys-ctrl-shipto.ship-id IN FRAME viewFormFrame /* Ship To ID */
DO:
    IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ "Yes" AND
       SELF:SCREEN-VALUE NE "" THEN DO:
        {methods/entryerr.i
            &can-find="FIRST shipto NO-LOCK
                       WHERE shipto.company EQ gcompany
                         AND shipto.cust-no EQ sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE
                         AND shipto.ship-id EQ sys-ctrl-shipto.ship-id:SCREEN-VALUE"
            &error-message="Invalid Ship To"}
    END.
    {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME sysCtrlBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME sysCtrlBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlBrowse C-Win
ON DEFAULT-ACTION OF sysCtrlBrowse IN FRAME DEFAULT-FRAME
DO:
    VIEW FRAME viewFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlBrowse C-Win
ON START-SEARCH OF sysCtrlBrowse IN FRAME DEFAULT-FRAME
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE sysCtrlBrowse:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlBrowse C-Win
ON VALUE-CHANGED OF sysCtrlBrowse IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME viewFrame:
        RUN pDisplay.
        IF users.securityLevel LT ttSysCtrl.securityLevelDefault THEN
        DISABLE {&transPanel}.
        ELSE DO:
            ENABLE {&transInit}.
            IF NOT ttSysCtrl.allowAdd THEN
            DISABLE btnAdd btnCopy.
            IF NOT ttSysCtrl.allowDelete THEN
            DISABLE btnDelete.
            IF ttSysCtrl.tableSource NE "sys-ctrl" THEN
            DISABLE btnForms btnDefaults.
        END. /* else */
    END. /* with frame */
    RUN pRefreshFormsFrame.
    DO WITH FRAME formsFrame:
        ASSIGN
            cSysCtrlName:SCREEN-VALUE         = ttSysCtrl.name
            cSysCtrlDescrip:SCREEN-VALUE      = ttSysCtrl.descrip
            cSysCtrFieldlDescrip:SCREEN-VALUE = ttSysCtrl.fieldDescrip
            cSysCtrlDataType:SCREEN-VALUE     = ttSysCtrl.dataType
            .
        {&OPEN-QUERY-sysCtrlShiptoBrowse}
        IF AVAILABLE sys-ctrl-shipto THEN
        APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlShipToBrowse.
    END. /* with frame */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME sysCtrlShipToBrowse
&Scoped-define FRAME-NAME formsFrame
&Scoped-define SELF-NAME sysCtrlShipToBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlShipToBrowse C-Win
ON DEFAULT-ACTION OF sysCtrlShipToBrowse IN FRAME formsFrame
DO:
    APPLY "CHOOSE":U TO btnUpdate-2 IN FRAME viewFormFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sysCtrlShipToBrowse C-Win
ON VALUE-CHANGED OF sysCtrlShipToBrowse IN FRAME formsFrame
DO:
    RUN pDisplayForm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME sysCtrlBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  gcompany = g_company.
  RUN enable_UI.
  FIND FIRST users NO-LOCK
       WHERE users.user_id EQ USERID("ASI")
       NO-ERROR.
  IF NOT AVAILABLE users THEN RETURN.
  IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
  RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
  IF VALID-HANDLE(hPgmMstrSecur) THEN
  RUN epCanAccess IN hPgmMstrSecur (
      "system/sys-ctrl.w",
      "SuperAdmin",
      OUTPUT lSuperAdmin
      ).
  APPLY "CHOOSE":U TO btnSearch.
  hFocus = svFocus:HANDLE.
  RUN pGetSettings (USERID("ASI")).
  RUN pInit.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flg C-Win 
PROCEDURE check-flg :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hPgmSecurity AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lSecure      AS LOGICAL NO-UNDO.

    RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
    RUN epCanAccess IN hPgmSecurity ("system/sys-ctrl.w", "", OUTPUT lSecure).
    DELETE OBJECT hPgmSecurity.
    lValid = YES.
    DO WITH FRAME viewFrame:
        IF NOT lSecure AND cFieldValue:MODIFIED THEN DO:
            IF ttSysCtrl.name EQ "RELCREDT" AND
               cFieldValue:SCREEN-VALUE EQ "YES" THEN
            lSecure = YES.
            ELSE
            RUN sys/ref/d-ASIpwd.w (OUTPUT lSecure).
            IF NOT lSecure THEN
            ASSIGN
                lValid = NO
                cFieldValue:SCREEN-VALUE = cFieldValue
                .
        END. /* if not lsecure */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Display-Field C-Win 
PROCEDURE Display-Field :
/*------------------------------------------------------------------------------
  Purpose:     Display Field Values
  Parameters:  Widget Name ({&SELF-NAME}:NAME)
  Notes:       
------------------------------------------------------------------------------*/
&Scoped-define FIRST-EXTERNAL-TABLE sys-ctrl-shipto

    DEFINE INPUT PARAMETER widget-name AS CHARACTER NO-UNDO.
    
    DO WITH FRAME viewFormFrame:
        CASE widget-name:
              {methods/dispflds/{&FIRST-EXTERNAL-TABLE}.i}
        END CASE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY svFocus searchBar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnRestoreDefaults btnSortMove svFocus searchBar sysCtrlBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME filterFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-filterFrame}
  DISPLAY cNameFilter cFieldDescripFilter cValueFilter cDescripFilter 
          cModuleFilter cTypeCodeFilter cCategoryFilter cSubCategoryFilter 
          cTableSourceFilter cFieldSourceFilter cDataTypeFilter 
      WITH FRAME searchFrame IN WINDOW C-Win.
  ENABLE btnSearch cNameFilter cFieldDescripFilter cValueFilter cDescripFilter 
         cModuleFilter cTypeCodeFilter cCategoryFilter cSubCategoryFilter 
         cTableSourceFilter cFieldSourceFilter cDataTypeFilter 
      WITH FRAME searchFrame IN WINDOW C-Win.
  VIEW FRAME searchFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-searchFrame}
  DISPLAY cSysCtrlName cSysCtrlDescrip cSysCtrlDataType cSysCtrFieldlDescrip 
      WITH FRAME formsFrame IN WINDOW C-Win.
  ENABLE cSysCtrlName cSysCtrlDescrip cSysCtrlDataType cSysCtrFieldlDescrip 
         sysCtrlShipToBrowse 
      WITH FRAME formsFrame IN WINDOW C-Win.
  VIEW FRAME formsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-formsFrame}
  DISPLAY cCategory cSubcategory iSecurityLevelUser iSecurityLevelDefault cName 
          cTypeCode cModule cDescrip cFieldDescrip cFieldValue cFieldDefault 
          ctableSource cfieldSource cDataType 
      WITH FRAME viewFrame IN WINDOW C-Win.
  ENABLE btnClose btnFirst-1 btnLast-1 btnNext-1 btnPrev-1 btnAdd btnCopy 
         btnDefaults btnDelete btnForms btnUpdate 
      WITH FRAME viewFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFrame}
  DISPLAY type_name ship_name cLogLabel 
      WITH FRAME viewFormFrame IN WINDOW C-Win.
  IF AVAILABLE sys-ctrl-shipto THEN 
    DISPLAY sys-ctrl-shipto.cust-vend sys-ctrl-shipto.cust-vend-no 
          sys-ctrl-shipto.ship-id sys-ctrl-shipto.descrip 
          sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld 
          sys-ctrl-shipto.dec-fld sys-ctrl-shipto.int-fld 
          sys-ctrl-shipto.log-fld 
      WITH FRAME viewFormFrame IN WINDOW C-Win.
  ENABLE btnFirst-2 btnForms-2 btnLast-2 btnNext-2 btnPrev-2 btnAdd-2 btnCopy-2 
         btnDelete-2 btnUpdate-2 
      WITH FRAME viewFormFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFormFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign C-Win 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable  AS HANDLE    NO-UNDO.

    IF AVAILABLE ttSysCtrl THEN DO WITH FRAME viewFrame:
        ASSIGN
            {&displayFields}
            hTable  = BUFFER ttSysCtrl:HANDLE
            hWidget = FRAME viewFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:NAME NE ? AND
               INDEX("{&displayFields}",hWidget:NAME) NE 0 THEN
            ASSIGN
                cField = SUBSTR(hWidget:NAME,2)
                hTable:BUFFER-FIELD(cField):BUFFER-VALUE() = hWidget:SCREEN-VALUE
                .
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildMenuTree C-Win 
PROCEDURE pBuildMenuTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    
    RUN pInitMenuTree.

    iOrder = iOrder + 1.
    RUN pCreatettMenuTree (
        FRAME filterFrame:HANDLE,
        iOrder,
        1,
        YES,
        "",
        "ALL",
        "ALL",
        "tab_pane.png",
        "",
        YES
        ).
    FOR EACH ttSysCtrl
        BREAK BY ttSysCtrl.subCategory
        :
        IF FIRST-OF(ttSysCtrl.subCategory) THEN DO:
            iOrder = iOrder + 1.
            RUN pCreatettMenuTree (
                FRAME filterFrame:HANDLE,
                iOrder,
                2,
                NO,
                "ALL",
                ttSysCtrl.subCategory,
                ttSysCtrl.subCategory,
                "hand_point_right2.png",
                "",
                YES
                ).
        END. /* if first-of */
    END. /* each ttsysctrl */
    FOR EACH ttSysCtrl
        BREAK BY ttSysCtrl.category
              BY ttSysCtrl.subCategory
        :
        IF FIRST-OF(ttSysCtrl.category) THEN DO:
            iOrder = iOrder + 1.
            RUN pCreatettMenuTree (
                FRAME filterFrame:HANDLE,
                iOrder,
                1,
                YES,
                "",
                ttSysCtrl.category,
                ttSysCtrl.category,
                "tab_pane.png",
                "",
                YES
                ).
        END. /* if first-of category */
        IF FIRST-OF(ttSysCtrl.subCategory) THEN DO:
            iOrder = iOrder + 1.
            RUN pCreatettMenuTree (
                FRAME filterFrame:HANDLE,
                iOrder,
                2,
                NO,
                ttSysCtrl.category,
                ttSysCtrl.subCategory,
                ttSysCtrl.subCategory,
                "hand_point_right2.png",
                "",
                YES
                ).
        END. /* if first-of category */
    END. /* each ttsysctrl */
    iOrder = iOrder + 1.
    RUN pCreatettMenuTree (
        FRAME filterFrame:HANDLE,
        iOrder,
        1,
        NO,
        "",
        "Exit",
        "Exit",
        "navigate_cross.png",
        "",
        YES
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByCategory C-Win 
PROCEDURE pByCategory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.category
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.category DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByDataType C-Win 
PROCEDURE pByDataType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.dataType
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.dataType DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByDescrip C-Win 
PROCEDURE pByDescrip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.descrip
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.descrip DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByFieldDescrip C-Win 
PROCEDURE pByFieldDescrip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldDescrip
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldDescrip DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByFieldSource C-Win 
PROCEDURE pByFieldSource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldSource
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldSource DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByFieldValue C-Win 
PROCEDURE pByFieldValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldValue
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.fieldValue DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByModule C-Win 
PROCEDURE pByModule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.module
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.module DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByName C-Win 
PROCEDURE pByName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.name
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.name DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBySubCategory C-Win 
PROCEDURE pBySubCategory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.subCategory
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.subCategory DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByTableSource C-Win 
PROCEDURE pByTableSource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.tableSource
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.tableSource DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByTypeCode C-Win 
PROCEDURE pByTypeCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.typeCode
    {&OPEN-QUERY-sysCtrlBrowse}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttSysCtrl.typeCode DESCENDING
    {&OPEN-QUERY-sysCtrlBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearSearchFilters C-Win 
PROCEDURE pClearSearchFilters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

    ASSIGN
        hWidget = FRAME searchFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "BUTTON" AND
           hWidget:SENSITIVE THEN
        hWidget:SCREEN-VALUE = "".
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    ASSIGN {&searchFilters}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreatettSysCtrl C-Win 
PROCEDURE pCreatettSysCtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDataType AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER sys-ctrl FOR sys-ctrl.
    
    CREATE ttSysCtrl.
    ASSIGN
        ttSysCtrl.category             = sys-ctrl.category
        ttSysCtrl.subCategory          = sys-ctrl.subCategory
        ttSysCtrl.name                 = sys-ctrl.name
        ttSysCtrl.descrip              = sys-ctrl.descrip
        ttSysCtrl.typeCode             = sys-ctrl.typeCode
        ttSysCtrl.module               = sys-ctrl.module
        ttSysCtrl.dataType             = ipcDataType
        ttSysCtrl.securityLevelUser    = sys-ctrl.securityLevelUser
        ttSysCtrl.securityLevelDefault = sys-ctrl.securityLevelDefault
        ttSysCtrl.tableSource          = "sys-ctrl"
        .
    CASE ipcDataType:
        WHEN "Character" THEN
        ASSIGN
            ttSysCtrl.fieldDescrip = sys-ctrl.char-fld_descrip
            ttSysCtrl.fieldDefault = sys-ctrl.char_field_default
            ttSysCtrl.fieldSource  = "char-fld"
            ttSysCtrl.fieldValue   = sys-ctrl.char-fld
            .
        WHEN "Date" THEN
        ASSIGN
            ttSysCtrl.fieldDescrip = sys-ctrl.date-fld_descrip
            ttSysCtrl.fieldDefault = IF sys-ctrl.date-fld_default NE ? THEN 
                                     STRING(sys-ctrl.date-fld_default,"99/99/9999")
                                     ELSE ""
            ttSysCtrl.fieldSource  = "date-fld"
            ttSysCtrl.fieldValue   = IF sys-ctrl.date-fld NE ? THEN 
                                     STRING(sys-ctrl.date-fld,"99/99/9999")
                                     ELSE ""
            .
        WHEN "Decimal" THEN
        ASSIGN
            ttSysCtrl.fieldDescrip = sys-ctrl.dec-fld_descrip
            ttSysCtrl.fieldDefault = LEFT-TRIM(STRING(sys-ctrl.dec-fld_default,"->>,>>9.99"))
            ttSysCtrl.fieldSource  = "dec-fld"
            ttSysCtrl.fieldValue   = LEFT-TRIM(STRING(sys-ctrl.dec-fld,"->>,>>9.99"))
            .
        WHEN "Integer" THEN
        ASSIGN
            ttSysCtrl.fieldDescrip = sys-ctrl.int-fld_descrip
            ttSysCtrl.fieldDefault = LEFT-TRIM(STRING(sys-ctrl.int-fld_default,"->,>>>,>>9"))
            ttSysCtrl.fieldSource  = "int-fld"
            ttSysCtrl.fieldValue   = LEFT-TRIM(STRING(sys-ctrl.int-fld,"->,>>>,>>9"))
            .
        WHEN "Logical" THEN
        ASSIGN
            ttSysCtrl.fieldDescrip = sys-ctrl.log-fld_descrip
            ttSysCtrl.fieldDefault = STRING(sys-ctrl.log-fld_default)
            ttSysCtrl.fieldSource  = "log-fld"
            ttSysCtrl.fieldValue   = STRING(sys-ctrl.log-fld)
            .
    END CASE.
    ttSysCtrl.allData = ttSysCtrl.category + "|"
                      + ttSysCtrl.subCategory + "|"
                      + ttSysCtrl.name + "|"
                      + ttSysCtrl.descrip + "|"
                      + ttSysCtrl.typeCode + "|"
                      + ttSysCtrl.module + "|"
                      + ttSysCtrl.dataType + "|"
                      + ttSysCtrl.tableSource + "|"
                      + ttSysCtrl.fieldDescrip + "|"
                      + ttSysCtrl.fieldDefault + "|"
                      + ttSysCtrl.fieldSource + "|"
                      + ttSysCtrl.fieldValue
                      .
    IF ttSysCtrl.category EQ "" THEN
    ttSysCtrl.category = "Category".
    IF ttSysCtrl.subCategory EQ "" THEN
    ttSysCtrl.subCategory = "SubCategory".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreatettSysCtrlTable C-Win 
PROCEDURE pCreatettSysCtrlTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphTable     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFormat     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExtent     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtentBase AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    
    DO idx = 1 TO iphTable:NUM-FIELDS:
        hField = iphTable:BUFFER-FIELD(idx).
        IF CAN-DO(ipcFieldList,hField:NAME) THEN NEXT.
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ iphTable:NAME.
        FIND FIRST ASI._field OF ASI._file NO-LOCK
             WHERE ASI._field._field-name EQ hField:NAME.
        iExtentBase = IF hField:EXTENT GT 0 THEN 1 ELSE 0.
        DO iExtent = iExtentBase TO hField:EXTENT:
            CREATE ttSysCtrl.
            ASSIGN
                cLabel                 = ASI._field._label
                cLabel                 = IF cLabel NE ? AND cLabel NE "" THEN cLabel
                                         ELSE "<No Label>"
                cExtent                = IF iExtent NE 0 THEN "[" + STRING(iExtent,"99") + "]"  ELSE ""
                cFormat                = IF hField:DATA-TYPE EQ "Date"    THEN "99/99/9999"
                                    ELSE IF hField:DATA-TYPE EQ "Decimal" THEN "->>,>>9.99"
                                    ELSE IF hField:DATA-TYPE EQ "Integer" THEN "->,>>>,>>9"
                                    ELSE "x(256)"
                ttSysCtrl.category     = "Tables"
                ttSysCtrl.subCategory  = "Table: " + iphTable:NAME
                ttSysCtrl.name         = hField:NAME + cExtent
                ttSysCtrl.descrip      = cLabel + " " + cExtent
                ttSysCtrl.dataType     = hField:DATA-TYPE
                ttSysCtrl.module       = "DB"
                ttSysCtrl.tableSource  = iphTable:NAME
                ttSysCtrl.fieldSource  = hField:NAME
                ttSysCtrl.fieldExtent  = iExtent
                ttSysCtrl.fieldDescrip = ttSysCtrl.descrip
                ttSysCtrl.fieldDefault = IF hField:DATA-TYPE EQ "Logical" THEN hField:BUFFER-VALUE(iExtent)
                                         ELSE LEFT-TRIM(STRING(hField:BUFFER-VALUE(iExtent),cFormat))
                ttSysCtrl.fieldValue   = ttSysCtrl.fieldDefault
                ttSysCtrl.allowAdd     = NO
                ttSysCtrl.allData = ttSysCtrl.category + "|"
                                  + ttSysCtrl.subCategory + "|"
                                  + ttSysCtrl.name + "|"
                                  + ttSysCtrl.descrip + "|"
                                  + ttSysCtrl.typeCode + "|"
                                  + ttSysCtrl.module + "|"
                                  + ttSysCtrl.dataType + "|"
                                  + ttSysCtrl.tableSource + "|"
                                  + ttSysCtrl.fieldDescrip + "|"
                                  + ttSysCtrl.fieldDefault + "|"
                                  + ttSysCtrl.fieldSource + "|"
                                  + ttSysCtrl.fieldValue
                                  .
        END. /* do iextent */
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE lContinue      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRemove        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hWidget        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cSaveName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rRowID         AS ROWID     NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOldFieldValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttSysCtrl FOR ttSysCtrl.
    
    DO WITH FRAME viewFrame:
        ASSIGN
            hDate:HIDDEN    = YES
            hDecimal:HIDDEN = YES
            hInteger:HIDDEN = YES
            hLogical:HIDDEN = YES
            .
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                ASSIGN
                    FRAME filterFrame:SENSITIVE    = NO
                    BROWSE sysCtrlBrowse:SENSITIVE = NO
                    .
                ENABLE {&transUpdate}.
                IF AVAILABLE ttSysCtrl   AND
                   ttSysCtrl.tableSource NE "sys-ctrl" THEN
                ENABLE cFieldValue.
                ELSE DO:
                    ENABLE {&enabledFields}.
                    IF NOT lSuperAdmin THEN
                    DISABLE iSecurityLevelDefault cFieldDefault.
                END. /* else */
                CASE cDataType:SCREEN-VALUE:
                    WHEN "Character" THEN
                    hFieldValue = cFieldValue:HANDLE.
                    WHEN "Date" THEN
                    ASSIGN
                        hFieldValue = hDate:HANDLE
                        btnCalendar-1:HIDDEN = NO
                        btnCalendar-1:SENSITIVE = YES
                        .
                    WHEN "Decimal" THEN
                    hFieldValue = hDecimal:HANDLE.
                    WHEN "Integer" THEN
                    hFieldValue = hInteger:HANDLE.
                    WHEN "Logical" THEN
                    hFieldValue = hLogical:HANDLE.
                END CASE. /* cdatatype */
                IF cDataType:SCREEN-VALUE NE "Character" THEN
                ASSIGN
                    hFieldValue:HIDDEN       = NO
                    hFieldValue:SENSITIVE    = YES
                    hFieldValue:SCREEN-VALUE = cFieldValue:SCREEN-VALUE
                    cFieldValue:SENSITIVE    = NO
                    .
                hFieldValue:MOVE-TO-TOP().
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    ASSIGN
                        hWidget = FRAME viewFrame:HANDLE
                        hWidget = hWidget:FIRST-CHILD
                        hWidget = hWidget:FIRST-CHILD
                        .
                    DO WHILE VALID-HANDLE(hWidget):
                        IF hWidget:TYPE NE "BUTTON" AND
                           hWidget:SENSITIVE THEN
                        hWidget:SCREEN-VALUE = "".
                        hWidget = hWidget:NEXT-SIBLING.
                    END. /* do while */
                    ASSIGN {&enabledFields}.
                    DISPLAY {&displayFields}.
                    ENABLE cDataType.
                    DISABLE btnReset.
                END. /* add */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL = "Save"
                    .                
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        IF CAN-FIND(FIRST sys-ctrl
                                    WHERE sys-ctrl.company EQ g_company
                                      AND sys-ctrl.name    EQ cName:SCREEN-VALUE) THEN DO:
                            MESSAGE
                                "System Control Parameter" SKIP(1)
                                "Company:" g_company SKIP
                                "Name:" cName:SCREEN-VALUE SKIP(1)
                                "Already Exists."                                
                            VIEW-AS ALERT-BOX ERROR.
                            APPLY "ENTRY":U TO cName.
                            RETURN.
                        END. /* if can-find */

                        /* custom logic for individual parameters */
                        IF cDataType:SCREEN-VALUE EQ "Character" THEN DO:
                            CASE cName:SCREEN-VALUE:
                                WHEN "APTAX" THEN
                                    cOldFieldValue = ttSysCtrl.fieldValue.
                                WHEN "OEFGUPDT" THEN DO:
                                    SUBSTRING(cFieldValue:SCREEN-VALUE,8,1) = "N".
                                END. /* oefgupdt */
                                WHEN "RELCREDT"    OR
                                WHEN "SalesBudget" THEN DO: 
                                   RUN check-flg.
                                   IF NOT lValid THEN DO:
                                       MESSAGE 
                                          "Please call ASI, to purchase this functionality."
                                           VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                       RETURN.
                                   END. /* if lvalid */
                                END. /* relcrdt or salesbudget */
                            END CASE.
                        END. /* if character */
                        /* custom logic for individual parameters */

                        CREATE sys-ctrl.
                        ASSIGN
                            sys-ctrl.company      = g_company
                            sys-ctrl.name         = cName:SCREEN-VALUE
                            sys-ctrl.rec_key      = STRING(TODAY,"99999999")
                                                  + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
                                                  .
                        CREATE ttSysCtrl.
                        ASSIGN
                            ttSysCtrl.tableSource = "sys-ctrl"
                            ttSysCtrl.fieldSource = IF cDataType:SCREEN-VALUE EQ "Character" THEN "char-fld"
                                               ELSE IF cDataType:SCREEN-VALUE EQ "Date"      THEN "date-fld"
                                               ELSE IF cDataType:SCREEN-VALUE EQ "Decimal"   THEN "dec-fld"
                                               ELSE IF cDataType:SCREEN-VALUE EQ "Integer"   THEN "int-fld"
                                               ELSE IF cDataType:SCREEN-VALUE EQ "Logical"   THEN "log-fld"
                                               ELSE ""
                                               .
                        CREATE rec_key.
                        ASSIGN
                            rec_key.rec_key    = sys-ctrl.rec_key
                            rec_key.table_name = "sys-ctrl"
                            .
                    END. /* if add/copy */
                    
                    RUN pAssign.
                    RUN pUpdateTable (BUFFER ttSysCtrl).
                    
                    /* custom logic for individual parameters */
                    CASE ttSysCtrl.dataType:
                        WHEN "Character" THEN DO:
                            CASE ttSysCtrl.name:
                                WHEN "APTAX" THEN DO:
                                    IF ttSysCtrl.fieldValue EQ "NoTax" AND
                                       ttSysCtrl.fieldValue NE cOldFieldValue THEN DO:
                                        FOR EACH po-ord NO-LOCK
                                            WHERE po-ord.company EQ g_company
                                              AND po-ord.opened  EQ YES,
                                             EACH po-ordl EXCLUSIVE-LOCK
                                            WHERE po-ordl.company EQ po-ord.company
                                              AND po-ordl.po-no   EQ po-ord.po-no
                                            :
                                            po-ordl.tax = NO.
                                        END. /* for each */
                                    END. /* if notax */
                                END. /* aptax */
                                WHEN "FGRECPT" THEN DO:
                                    idx = LOOKUP(ttSysCtrl.fieldValue,str-init[66]).
                                    RUN update-other-record ("AUTOPOST", idx EQ 1).
                                    /* TSPOSTFG or TSPARTS */
                                    RUN update-other-record ("TSPOSTFG", idx EQ 2 OR idx EQ 5).
                                END. /* fgrecpt */
                                WHEN "SSRMISSUE" THEN DO: 
                                    IF ttSysCtrl.fieldValue EQ "ScanTagOnly" THEN DO:
                                        FIND FIRST bttSysCtrl
                                             WHERE bttSysCtrl.name     EQ ttSysCtrl.name
                                               AND bttSysCtrl.dataType EQ "Logical"
                                             NO-ERROR.
                                        IF AVAILABLE bttSysCtrl THEN DO:
                                            bttSysCtrl.fieldValue = "NO".
                                            FIND FIRST sys-ctrl EXCLUSIVE-LOCK
                                                 WHERE sys-ctrl.company EQ g_company
                                                   AND sys-ctrl.name    EQ bttSysCtrl.name
                                                 NO-ERROR.
                                            IF AVAILABLE sys-ctrl THEN
                                            sys-ctrl.log-fld = NO.
                                            RELEASE sys-ctrl.
                                        END. /* if avail */ 
                                    END. /* if cfieldvalue */
                                END. /* ssrmissue */
                            END CASE. /* name */
                        END. /* character */
                        WHEN "Logical" THEN DO:
                            CASE ttSysCtrl.name:
                                WHEN "RELCREDT" THEN DO:
                                    IF ttSysCtrl.fieldValue EQ "NO" THEN DO:
                                        FIND FIRST bttSysCtrl
                                             WHERE bttSysCtrl.name     EQ ttSysCtrl.name
                                               AND bttSysCtrl.dataType EQ "Character"
                                             NO-ERROR.
                                        IF AVAILABLE bttSysCtrl THEN DO:
                                            bttSysCtrl.fieldValue = "".
                                            FIND FIRST sys-ctrl EXCLUSIVE-LOCK
                                                 WHERE sys-ctrl.company EQ g_company
                                                   AND sys-ctrl.name    EQ bttSysCtrl.name
                                                 NO-ERROR.
                                            IF AVAILABLE sys-ctrl THEN
                                            sys-ctrl.char-fld = "".
                                            RELEASE sys-ctrl.
                                        END. /* if avail */ 
                                    END. /* if logical */
                                END. /* relcredt */
                            END CASE. /* name */
                        END. /* logical */
                    END CASE. /* datatype */
                    /* custom logic for individual parameters */
                    
                    IF cMode EQ "Add"     OR
                       cMode EQ "Copy"    OR
                       cCategory:MODIFIED OR
                       cSubCategory:MODIFIED THEN DO:
                        cSaveName = ttSysCtrl.name.
                        RUN pInit.
                        ASSIGN
                            cFilter      = "ALL"
                            cSubFilter   = "ALL"
                            .
                        RUN pReopenBrowse.
                        FIND FIRST ttSysCtrl
                             WHERE ttSysCtrl.name EQ cSaveName
                             NO-ERROR.
                        IF AVAILABLE ttSysCtrl THEN DO:
                            rRowID = ROWID(ttSysCtrl).
                            REPOSITION sysCtrlBrowse TO ROWID rRowID.
                        END. /* if avail */
                    END. /* if add */
                    ELSE
                    BROWSE sysCtrlBrowse:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields} cDataType.
                ENABLE {&transInit}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    FRAME viewFrame:TITLE          = "View"
                    btnUpdate:LABEL                = "Update"
                    FRAME filterFrame:SENSITIVE    = YES
                    BROWSE sysCtrlBrowse:SENSITIVE = YES
                    btnCalendar-1:HIDDEN           = YES
                    btnCalendar-1:SENSITIVE        = NO
                    .
                IF hFieldValue:NAME NE "cFieldValue" THEN
                ASSIGN
                    hFieldValue:SENSITIVE = NO
                    hFieldValue:HIDDEN    = YES
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlBrowse.
            END. /* cancel save */
            WHEN "Defaults" THEN DO:
                IF AVAILABLE ttSysCtrl THEN DO:
                    MESSAGE
                        "Restore Defaults to Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO:
                        ASSIGN
                            iSecurityLevelUser:SCREEN-VALUE = STRING(ttSysCtrl.securityLevelDefault)
                            cFieldValue:SCREEN-VALUE        = ttSysCtrl.fieldDefault
                            .
                        RUN pAssign.
                        RUN pUpdateTable (BUFFER ttSysCtrl).
                        BROWSE sysCtrlBrowse:REFRESH().
                        RUN pDisplay.
                    END. /* if lcontinue */
                END. /* if avail */
            END. /* defaults */
            WHEN "Delete" THEN DO:
                IF AVAILABLE ttSysCtrl AND ttSysCtrl.allowDelete THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO:
                        ttSysCtrl.fieldValue = "".
                        IF ttSysCtrl.tableSource EQ "sys-ctrl" AND
                           lSuperAdmin THEN DO:
                            ttSysCtrl.fieldDefault = "".
                            MESSAGE
                                "REMOVE sys-ctrl Record?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                            UPDATE lContinue.
                            IF lContinue THEN DO:
                                FIND FIRST sys-ctrl EXCLUSIVE-LOCK
                                     WHERE sys-ctrl.company EQ g_company
                                       AND sys-ctrl.name    EQ ttSysCtrl.name
                                     NO-ERROR.                                     
                                IF AVAILABLE sys-ctrl THEN DO:
                                    DELETE sys-ctrl.
                                    cMode = iphMode:LABEL.
                                    RUN pInit.
                                    RETURN.
                                END. /* if avail */
                            END. /* if lContinue*/
                        END. /* if sys-ctrl superadmin */
                        RUN pUpdateTable (BUFFER ttSysCtrl).
                        IF NOT lSuperAdmin THEN DO:
                            DELETE ttSysCtrl.
                            BROWSE sysCtrlBrowse:DELETE-CURRENT-ROW().
                        END. /* not super admin */
                        ELSE
                        BROWSE sysCtrlBrowse:REFRESH().
                    END. /* if lcontinue */
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN
                RUN pDisplay.
        END CASE. /* ipcmode:label */
        IF iphMode:LABEL EQ "Add" AND cCategory:SENSITIVE THEN
        APPLY "ENTRY":U TO cCategory.
        ELSE IF iphMode:LABEL NE "Defaults" AND hFieldValue:SENSITIVE THEN
        APPLY "ENTRY":U TO hFieldValue.
        ELSE
        APPLY "ENTRY":U TO BROWSE sysCtrlBrowse.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD-2 C-Win 
PROCEDURE pCRUD-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&Scoped-define transPanel-2 btnUpdate-2 btnAdd-2 btnCopy-2 btnDelete-2 btnReset-2 ~
btnCancel-2 btnForms-2 btnFirst-2 btnPrev-2 btnNext-2 btnLast-2 btnCalendar-2
&Scoped-define transInit-2 btnUpdate-2 btnAdd-2 btnCopy-2 btnDelete-2 btnForms-2 ~
 btnFirst-2 btnPrev-2 btnNext-2 btnLast-2
&Scoped-define transUpdate-2 btnUpdate-2 btnCancel-2 btnReset-2 btnCalendar-2
&Scoped-define navPanel btnFirst-2 btnPrev-2 btnNext-2 btnLast-2

    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE lContinue      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE hWidget        AS HANDLE  NO-UNDO.
    DEFINE VARIABLE rRowID         AS ROWID   NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCustVend      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lUpdateReports AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bttSysCtrl     FOR ttSysCtrl.
    DEFINE BUFFER bSysCtrlShipTo FOR sys-ctrl-shipto.
    
    DO WITH FRAME viewFormFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel-2} {&navPanel}.
                BROWSE sysCtrlShipToBrowse:SENSITIVE = NO.
                ENABLE {&transUpdate-2}.
                ENABLE {&FIELDS-IN-QUERY-viewFormFrame}.
                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Save_As.ico").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    ASSIGN
                        hWidget = FRAME viewFormFrame:HANDLE
                        hWidget = hWidget:FIRST-CHILD
                        hWidget = hWidget:FIRST-CHILD
                        .
                    DO WHILE VALID-HANDLE(hWidget):
                        IF hWidget:TYPE NE "BUTTON" AND
                           hWidget:SENSITIVE THEN
                        hWidget:SCREEN-VALUE = "".
                        hWidget = hWidget:NEXT-SIBLING.
                    END. /* do while */
                    DISABLE btnReset-2.
                    APPLY "ENTRY":U TO sys-ctrl-shipto.cust-vend.
                END. /* add */
                ELSE IF iphMode:LABEL EQ "Update" THEN DO:
                    ASSIGN
                        sys-ctrl-shipto.cust-vend:SENSITIVE    = NO
                        sys-ctrl-shipto.cust-vend-no:SENSITIVE = NO
                        .
                    APPLY "ENTRY":U TO sys-ctrl-shipto.ship-id.
                END.
                ASSIGN
                    FRAME viewFormFrame:TITLE = iphMode:LABEL
                    cMode = iphMode:LABEL
                    btnUpdate-2:LABEL = "Save"
                    .                
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        lCustVend = sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ "yes".
                        IF CAN-FIND(FIRST sys-ctrl-shipto
                                    WHERE sys-ctrl-shipto.company      EQ g_company
                                      AND sys-ctrl-shipto.name         EQ ttSysCtrl.name
                                      AND sys-ctrl-shipto.cust-vend    EQ lCustVend
                                      AND sys-ctrl-shipto.cust-vend-no EQ sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE
                                      AND sys-ctrl-shipto.ship-id      EQ sys-ctrl-shipto.ship-id:SCREEN-VALUE
                                    ) THEN DO:
                            MESSAGE
                                "System Control Parameter" SKIP(1)
                                "Company:" g_company SKIP
                                "Name:" ttSysCtrl.name SKIP
                                "Type:" STRING(lCustVend,"Customer/Vendor") SKIP
                                "Cust/Vend:" sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE SKIP
                                "Ship ID:" sys-ctrl-shipto.ship-id:SCREEN-VALUE SKIP(1)
                                "Already Exists."                                
                            VIEW-AS ALERT-BOX ERROR.
                            APPLY "ENTRY":U TO sys-ctrl-shipto.cust-vend.
                            RETURN.
                        END. /* if can-find */
                        CREATE sys-ctrl-shipto.
                        ASSIGN
                            sys-ctrl-shipto.company = g_company
                            sys-ctrl-shipto.name    = ttSysCtrl.name
                            {&FIELDS-IN-QUERY-viewFormFrame}
                            .
                        {&OPEN-QUERY-sysCtrlShipToBrowse}
                    END. /* if add/copy */
                    ELSE IF cMode EQ "Update" THEN DO:
                        FIND CURRENT sys-ctrl-shipto EXCLUSIVE-LOCK.
                        ASSIGN {&FIELDS-IN-QUERY-viewFormFrame}.
                        {&OPEN-QUERY-sysCtrlShipToBrowse}
                    END.
                    FIND CURRENT sys-ctrl-shipto NO-LOCK.
                    IF ttSysCtrl.name EQ "Reports" AND
                       sys-ctrl-shipto.log-fld THEN DO:
                        MESSAGE
                            "Would you like to set all reports to yes?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE lUpdateReports.
                        IF lUpdateReports THEN
                        FOR EACH bSysCtrlShipTo EXCLUSIVE-LOCK
                            WHERE bSysCtrlShipTo.company EQ g_company
                              AND bSysCtrlShipTo.name EQ ttSysCtrl.name
                            :
                            bSysCtrlShipTo.log-fld = YES .
                        END. /* each bsysctrlshipto */
                    END. /* if reports */
                    BROWSE sysCtrlShipToBrowse:REFRESH().
                END. /* save */
                DISABLE {&transPanel-2} {&FIELDS-IN-QUERY-viewFormFrame}.
                ENABLE {&transInit-2}.
                btnUpdate-2:LOAD-IMAGE("Graphics\32x32\Pencil.ico").
                ASSIGN
                    cMode = iphMode:LABEL
                    FRAME viewFormFrame:TITLE = "View"
                    btnUpdate-2:LABEL = "Update"
                    BROWSE sysCtrlShipToBrowse:SENSITIVE = YES
                    .
                IF AVAILABLE sys-ctrl-shipto THEN
                APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlShipToBrowse.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE sys-ctrl-shipto THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO:
                        FIND CURRENT sys-ctrl-shipto EXCLUSIVE-LOCK.
                        DELETE sys-ctrl-shipto.
                        BROWSE sysCtrlShipToBrowse:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN
                RUN pDisplayForm.
        END CASE.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay C-Win 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hWidget AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hNext   AS HANDLE    NO-UNDO.

    IF AVAILABLE ttSysCtrl THEN DO WITH FRAME viewFrame:
        ASSIGN
            hTable  = BUFFER ttSysCtrl:HANDLE
            hWidget = FRAME viewFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:NAME NE ? AND
               INDEX("{&displayFields}",hWidget:NAME) NE 0 THEN DO:
                IF (hWidget:NAME EQ "cFieldValue"    OR
                    hWidget:NAME EQ "cFieldDefault") THEN DO:
                    CASE ttSysCtrl.dataType:
                        WHEN "Character" THEN
                        ASSIGN
                            hWidget:WIDTH  = 92
                            hWidget:FORMAT = "x(256)"
                            .
                        WHEN "Date" THEN
                        hWidget:WIDTH  = 16.
                        WHEN "Decimal" THEN
                        hWidget:WIDTH  = 16.
                        WHEN "Integer" THEN
                        hWidget:WIDTH  = 16.
                        WHEN "Logical" THEN
                        hWidget:WIDTH  = 5.
                    END CASE.
                END. /* if cfield* */
                ASSIGN
                    cField = SUBSTR(hWidget:NAME,2)
                    hWidget:SCREEN-VALUE = hTable:BUFFER-FIELD(cField):BUFFER-VALUE()
                    .
            END. /* if */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            cFieldValue:LABEL = "Value"
            cDescrip:LABEL    = "Description"
            .
        IF ttSysCtrl.dataType EQ "Character" THEN DO:
            CASE ttSysCtrl.name:
                WHEN "CHKFMT"   OR
                WHEN "RelPrint" OR
                WHEN "BolFmt"   OR
                WHEN "Ackhead"  OR
                WHEN "POPrint"  OR
                WHEN "InvPrint" OR
                WHEN "QuoPrint" OR
                WHEN "BolFmtX"  THEN
                    cFieldValue:LABEL = "Business Form".
                WHEN "CasLabel" OR
                WHEN "BarDir"   OR
                WHEN "RMTags"   THEN
                    cFieldValue:LABEL = "Label Location".
                WHEN "PushPin" THEN
                    cFieldValue:LABEL = "File Directory".
            END CASE.
            IF ttSysCtrl.name EQ "BarDir" THEN
            cDescrip:LABEL = "File Directory".
        END. /* if character */
        ASSIGN {&displayFields}.
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayForm C-Win 
PROCEDURE pDisplayForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME viewFormFrame:
        ASSIGN
            sys-ctrl-shipto.char-fld:LABEL = "Business Form Name"
            cLogLabel:LABEL = "Logical"
            .
        CASE ttSysCtrl.name:
            WHEN "CustomerList" THEN
            ASSIGN
                sys-ctrl-shipto.char-fld:LABEL = "Menu Hot Key"
                cLogLabel:LABEL = "Limit Customers?"
                .
            WHEN "BarDir" OR WHEN "CASLABEL" OR WHEN "RMTags" THEN
            sys-ctrl-shipto.char-fld:LABEL = "Label Location".
            WHEN "PushPin" THEN
            sys-ctrl-shipto.char-fld:LABEL = "File Directory".
            WHEN "RELMERGE" THEN
            sys-ctrl-shipto.char-fld:LABEL = "Character" .
        END CASE.
        IF AVAILABLE sys-ctrl-shipto THEN DO:
            DISPLAY
                sys-ctrl-shipto.cust-vend
                sys-ctrl-shipto.cust-vend-no
                sys-ctrl-shipto.ship-id
                sys-ctrl-shipto.descrip
                sys-ctrl-shipto.char-fld
                sys-ctrl-shipto.date-fld
                sys-ctrl-shipto.dec-fld
                sys-ctrl-shipto.int-fld
                sys-ctrl-shipto.log-fld
                .
            APPLY "LEAVE":U TO sys-ctrl-shipto.cust-vend-no.
            APPLY "LEAVE":U TO sys-ctrl-shipto.ship-id.
        END. /* if avail */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pForms C-Win 
PROCEDURE pForms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME formsFrame:
        IF FRAME formsFrame:HIDDEN THEN
        VIEW FRAME formsFrame.
        RUN pRefreshFormsFrame.
        APPLY "ENTRY":U TO sysCtrlShipToBrowse.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&program-id}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO iUserPrintOffSet:
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
                WHEN "BrowseRowHeight" THEN
                ASSIGN
                    BROWSE sysCtrlBrowse:ROW-HEIGHT = DECIMAL(user-print.field-value[idx])
                    .
            END CASE.
        END. /* do idx */
        DO idx = iUserPrintOffSet + 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE sysCtrlBrowse:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE sysCtrlBrowse:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        jdx = idx - iUserPrintOffSet
                        hColumn = BROWSE sysCtrlBrowse:GET-BROWSE-COLUMN(jdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE sysCtrlBrowse:MOVE-COLUMN(kdx,jdx).
                END. /* if name */
            END. /* do kdx */
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pParseTables.  
    RUN pDisplayMenuTree (FRAME filterFrame:HANDLE, "", YES, 1).
    FIND FIRST ttMenuTree.
    IF AVAILABLE ttMenuTree AND VALID-HANDLE(ttMenuTree.hEditor) THEN
    RUN pClickMenuTree (ttMenuTree.hEditor).
    RUN pSetFocus.
    IF lSuperAdmin THEN
    ENABLE btnExport btnImport WITH FRAME {&FRAME-NAME}.
    searchBar:TOOLTIP = "Search Columns:" + CHR(10) + CHR(10)
                      + "Name" + CHR(10)
                      + "Value Description" + CHR(10)
                      + "Value" + CHR(10)
                      + "Description" + CHR(10)
                      + "Module" + CHR(10)
                      + "Type" + CHR(10)
                      + "Category" + CHR(10)
                      + "SubCategory" + CHR(10)
                      + "Table Source" + CHR(10)
                      + "Field Source" + CHR(10)
                      + "Data Type"
                      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNavPanel C-Win 
PROCEDURE pNavPanel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphNavPanel AS HANDLE NO-UNDO.
    
    CASE iphNavPanel:LABEL:
        WHEN "First" THEN
        APPLY "HOME":U TO BROWSE sysCtrlBrowse.
        WHEN "Previous" THEN
        BROWSE sysCtrlBrowse:SELECT-PREV-ROW().
        WHEN "Next" THEN
        BROWSE sysCtrlBrowse:SELECT-NEXT-ROW().
        WHEN "Last" THEN
        APPLY "END":U TO BROWSE sysCtrlBrowse.
    END CASE.
    IF AVAILABLE ttSysCtrl THEN DO:
        APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlBrowse.
        IF NOT FRAME formsFrame:HIDDEN THEN DO:
            RUN pForms.
            IF ttSysCtrl.tableSource NE "sys-ctrl" THEN
            APPLY "CHOOSE":U TO btnForms-2 IN FRAME viewFormFrame.
        END. /* if viewformframe open */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParseSysCtrl C-Win 
PROCEDURE pParseSysCtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
    DISABLE TRIGGERS FOR DUMP OF sys-ctrl-shipto.
    
    FOR EACH sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ g_company
          AND sys-ctrl.securityLevelUser LE users.securityLevel
        :
        IF sys-ctrl.char-fld_descrip NE "" OR lSuperAdmin THEN
        RUN pCreatettSysCtrl ("Character", BUFFER sys-ctrl).
        IF sys-ctrl.date-fld_descrip NE "" OR lSuperAdmin THEN
        RUN pCreatettSysCtrl ("Date",      BUFFER sys-ctrl).        
        IF sys-ctrl.dec-fld_descrip  NE "" OR lSuperAdmin THEN
        RUN pCreatettSysCtrl ("Decimal",   BUFFER sys-ctrl).        
        IF sys-ctrl.int-fld_descrip  NE "" OR lSuperAdmin THEN
        RUN pCreatettSysCtrl ("Integer",   BUFFER sys-ctrl).        
        IF sys-ctrl.log-fld_descrip  NE "" OR lSuperAdmin THEN
        RUN pCreatettSysCtrl ("Logical",   BUFFER sys-ctrl).
    END. /* each sys-ctrl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParseTables C-Win 
PROCEDURE pParseTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE hTable     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttSysCtrl.
    /* table sys-ctrl */
    RUN pParseSysCtrl.
    /* all other tables */
    IF lSuperAdmin THEN DO:
        ASSIGN
            cTableName[1]  = "ap-ctrl"  cFieldList[1]  = "rec_key"
            cTableName[2]  = "ar-ctrl"  cFieldList[2]  = "rec_key"
            cTableName[3]  = "ce-ctrl"  cFieldList[3]  = "rec_key"
            cTableName[4]  = "config"   cFieldList[4]  = "audit_tables"
            cTableName[5]  = "db-ctrl"  cFieldList[5]  = "rec_key"
            cTableName[6]  = "fg-ctrl"  cFieldList[6]  = "rec_key"
            cTableName[7]  = "gl-ctrl"  cFieldList[7]  = "rec_key"
            cTableName[8]  = "jc-ctrl"  cFieldList[8]  = "rec_key"
            cTableName[9]  = "oe-ctrl"  cFieldList[9]  = "rec_key"
            cTableName[10] = "po-ctrl"  cFieldList[10] = "rec_key"
            cTableName[11] = "rfq-ctrl" cFieldList[11] = "rec_key"
            cTableName[12] = "rm-ctrl"  cFieldList[12] = "rec_key"
            .
        DO idx = 1 TO EXTENT(cTableName):
            IF cTableName[idx] EQ "" THEN LEAVE.
            CREATE BUFFER hTable FOR TABLE cTableName[idx].
            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(hTable).
            hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME + " NO-LOCK").
            hQuery:QUERY-OPEN().
            hQuery:GET-FIRST().
            IF hQuery:QUERY-OFF-END THEN NEXT.        
            RUN pCreatettSysCtrlTable (hTable:HANDLE, cFieldList[idx]).
        END. /* do idx */
    END. /* if lsuperadmin */
    RUN pBuildMenuTree.
    ASSIGN
        cFilter    = ""
        cSubFilter = ""
        .
    RUN pReopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessClick C-Win 
PROCEDURE pProcessClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttMenuTree FOR ttMenuTree.
    
    IF AVAILABLE ttMenuTree THEN DO:
        ASSIGN
            cFilter    = ttMenuTree.treeParent
            cSubFilter = ttMenuTree.treeChild
            .
        IF cFilter EQ "" THEN
        ASSIGN
            cFilter    = cSubFilter
            cSubFilter = "ALL"
            .
        RUN pReopenBrowse.
    END. /* if avail not ismenu */
    FRAME searchFrame:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefreshFormsFrame C-Win 
PROCEDURE pRefreshFormsFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME formsFrame:
        ASSIGN
            cSysCtrlName:SCREEN-VALUE         = ttSysCtrl.name
            cSysCtrlDescrip:SCREEN-VALUE      = ttSysCtrl.descrip
            cSysCtrFieldlDescrip:SCREEN-VALUE = ttSysCtrl.fieldDescrip
            cSysCtrlDataType:SCREEN-VALUE     = ttSysCtrl.dataType
            .
        {&OPEN-QUERY-sysCtrlShiptoBrowse}
        IF AVAILABLE sys-ctrl-shipto THEN DO:
            APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlShipToBrowse.
            ENABLE {&transInit-2} WITH FRAME viewFormFrame.
        END. /* if avail */
        ELSE
        DISABLE btnUpdate-2 btnCopy-2 btnDelete-2 WITH FRAME viewFormFrame.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "category" THEN
        RUN pByCategory.
        WHEN "dataType" THEN
        RUN pByDataType.
        WHEN "descrip" THEN
        RUN pByDescrip.
        WHEN "fieldDescrip" THEN
        RUN pByFieldDescrip.
        WHEN "fieldSource" THEN
        RUN pByFieldSource.
        WHEN "module" THEN
        RUN pByModule.
        WHEN "name" THEN
        RUN pByName.
        WHEN "fieldValue" THEN
        RUN pByFieldValue.
        WHEN "subCategory" THEN
        RUN pBySubCategory.
        WHEN "tableSource" THEN
        RUN pByTableSource.
        WHEN "typeCode" THEN
        RUN pByTypeCode.
        OTHERWISE
        &SCOPED-DEFINE SORTBY-PHRASE
        {&OPEN-QUERY-sysCtrlBrowse}
    END CASE.
    SESSION:SET-WAIT-STATE("").
    IF AVAILABLE ttSysCtrl THEN
    APPLY "VALUE-CHANGED":U TO BROWSE sysCtrlBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        idx = idx + 1
        user-print.field-name[idx]  = "BrowseRowHeight"
        user-print.field-label[idx] = "BrowseRowHeight"
        user-print.field-value[idx] = STRING(BROWSE sysCtrlBrowse:ROW-HEIGHT)
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE sysCtrlBrowse:NUM-COLUMNS:
        ASSIGN
            idx = idx + 1
            hColumn = BROWSE sysCtrlBrowse:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(
                                          MAX(hColumn:WIDTH,
                                              BROWSE sysCtrlBrowse:MIN-COLUMN-WIDTH-CHARS)
                                                )
            .
    END. /* do jdx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTable C-Win 
PROCEDURE pUpdateTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ttSysCtrl FOR ttSysCtrl.
    
    DEFINE VARIABLE hTable AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hField AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.

    IF ttSysCtrl.tableSource EQ "sys-ctrl" THEN DO:
        FIND FIRST sys-ctrl EXCLUSIVE-LOCK
             WHERE sys-ctrl.company EQ g_company
               AND sys-ctrl.name    EQ ttSysCtrl.name
             NO-ERROR.
        IF AVAILABLE sys-ctrl THEN DO:
            ASSIGN
                sys-ctrl.category          = ttSysCtrl.category
                sys-ctrl.subCategory       = ttSysCtrl.subCategory
                sys-ctrl.name              = ttSysCtrl.name
                sys-ctrl.typeCode          = ttSysCtrl.typeCode
                sys-ctrl.module            = ttSysCtrl.module
                sys-ctrl.descrip           = ttSysCtrl.descrip
                sys-ctrl.securityLevelUser = ttSysCtrl.securityLevelUser
                .
            CASE ttSysCtrl.dataType:
                WHEN "Character" THEN
                ASSIGN
                    sys-ctrl.char-fld           = ttSysCtrl.fieldValue
                    sys-ctrl.char-fld_descrip   = ttSysCtrl.fieldDescrip
                    sys-ctrl.char_field_default = ttSysCtrl.fieldDefault
                    .
                WHEN "Date" THEN
                ASSIGN
                    sys-ctrl.date-fld         = DATE(ttSysCtrl.fieldValue)
                    sys-ctrl.date-fld_descrip = ttSysCtrl.fieldDescrip
                    sys-ctrl.date-fld_default = DATE(ttSysCtrl.fieldDefault)
                    .
                WHEN "Decimal" THEN
                ASSIGN
                    sys-ctrl.dec-fld         = DECIMAL(ttSysCtrl.fieldValue)
                    sys-ctrl.dec-fld_descrip = ttSysCtrl.fieldDescrip
                    sys-ctrl.dec-fld_default = DECIMAL(ttSysCtrl.fieldDefault)
                    .
                WHEN "Integer" THEN
                ASSIGN
                    sys-ctrl.int-fld         = INTEGER(ttSysCtrl.fieldValue)
                    sys-ctrl.int-fld_descrip = ttSysCtrl.fieldDescrip
                    sys-ctrl.int-fld_default = INTEGER(ttSysCtrl.fieldDefault)
                    .
                WHEN "Logical" THEN
                ASSIGN
                    sys-ctrl.log-fld         = ttSysCtrl.fieldValue EQ "YES"
                    sys-ctrl.log-fld_descrip = ttSysCtrl.fieldDescrip
                    sys-ctrl.log-fld_default = ttSysCtrl.fieldDefault EQ "YES"
                    .
            END CASE.
            RELEASE sys-ctrl.
        END. /* if avail */
        ELSE
        MESSAGE
            "Unable to UPDATE Selected Record"
        VIEW-AS ALERT-BOX ERROR.
    END. /* if sys-ctrl */
    /* tables other than sys-ctrl */
    ELSE DO:
        CREATE BUFFER hTable FOR TABLE ttSysCtrl.tableSource.
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hTable).
        hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME + "  EXCLUSIVE-LOCK").
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST().
        IF hQuery:QUERY-OFF-END THEN
        MESSAGE
            "Unable to UPDATE Selected Record"
        VIEW-AS ALERT-BOX ERROR.
        ELSE
        ASSIGN
            hField = hTable:BUFFER-FIELD(ttSysCtrl.fieldSource)
            hField:BUFFER-VALUE(ttSysCtrl.fieldExtent) = ttSysCtrl.fieldValue
            .
    END. /* not sys-ctrl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME formsFrame.
        HIDE FRAME searchFrame.
        HIDE FRAME filterFrame.
        HIDE BROWSE sysCtrlBrowse.
        HIDE FRAME viewFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            /* import/export objects */
            RECT-1:HIDDEN    = YES
            btnImport:HIDDEN = YES
            btnExport:HIDDEN = YES
            /* search frame */
            FRAME searchFrame:COL = 1
            FRAME searchFrame:ROW = 1
            /* default frame */
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            /* search bar */
            searchBar:WIDTH = FRAME {&FRAME-NAME}:WIDTH
                            - searchBar:COL + 1
            /* filter frame */
            FRAME filterFrame:VIRTUAL-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            FRAME filterFrame:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT - 3
            /* import/export objects */
            RECT-1:ROW       = FRAME filterFrame:HEIGHT + 1.19
            btnImport:ROW    = RECT-1:ROW + .24
            btnExport:ROW    = RECT-1:ROW + .24
            RECT-1:HIDDEN    = NO
            btnImport:HIDDEN = NO
            btnExport:HIDDEN = NO
            /* browse frame */
            BROWSE sysCtrlBrowse:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
                                        - BROWSE sysCtrlBrowse:ROW + 1
            BROWSE sysCtrlBrowse:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
                                        - FRAME filterFrame:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW FRAME filterFrame.
        VIEW BROWSE sysCtrlBrowse.
        VIEW FRAME searchFrame.
        FRAME searchFrame:MOVE-TO-TOP().
    END. /* do with */

    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-other-record C-Win 
PROCEDURE update-other-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplLog  AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bSysCtrl FOR sys-ctrl.

    FIND FIRST bSysCtrl EXCLUSIVE-LOCK 
         WHERE bSysCtrl.company EQ g_company
           AND bSysCtrl.name    EQ ipcName
         NO-ERROR.
    IF AVAILABLE bSysCtrl THEN
    bSysCtrl.log-fld = iplLog.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-char-fld C-Win 
PROCEDURE valid-char-fld :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE thisOne       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE comp-char-val AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEntryTo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSingleValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j             AS INTEGER   NO-UNDO. 
    
    DO WITH FRAME viewFrame:
        /* Task 11011321 */
        IF cFieldValue:SCREEN-VALUE NE "" AND
           CAN-DO(cValidateList,ttSysCtrl.name) THEN DO: 
            lValid = TRUE.     
            /* Process NK1 options where user can select more than one */
            /* option - validate each option individually              */
            IF LOOKUP(ttSysCtrl.name, gvcMultiSelect) GT 0 AND
               INDEX(cFieldValue:SCREEN-VALUE, ",") GT 0 THEN DO:
                DO i = 1 TO NUM-ENTRIES(cFieldValue:SCREEN-VALUE):
                    cSingleValue = ENTRY(i, cFieldValue:SCREEN-VALUE).
                    RUN sys/ref/validSysCtrlChar.p (
                        g_company,
                        g_loc,
                        ttSysCtrl.name,
                        cFieldValue:LABEL,
                        cFieldValue:SCREEN-VALUE, /* log-fld */
                        cSingleValue,
                        name-fld-list,
                        str-init[LOOKUP(ttSysCtrl.name, name-fld-list)],
                        OUTPUT cEntryTo,
                        OUTPUT lValid
                        ).
                    IF NOT lValid THEN DO:   
                        CASE cEntryTo:
                            WHEN "Char" THEN
                                APPLY "ENTRY":U TO cFieldValue.
                            WHEN "Log" THEN
                                APPLY "ENTRY":U TO cFieldValue. /* log-fld */
                        END CASE.
                        LEAVE.
                    END. /* if not lvalid */
                END. /* do i = ... */
            END. /* if multiple values to validate */
            ELSE DO:
                RUN sys/ref/validSysCtrlChar.p ( 
                    g_company,
                    g_loc,
                    ttSysCtrl.name,
                    cFieldValue:LABEL,
                    cFieldValue:SCREEN-VALUE, /* log-fld */
                    cFieldValue:SCREEN-VALUE,
                    name-fld-list,
                    str-init[LOOKUP(ttSysCtrl.name, name-fld-list)],
                    OUTPUT cEntryTo,
                    OUTPUT lValid
                    ). 
                IF NOT lValid THEN DO:   
                    CASE cEntryTo:
                        WHEN "Char" THEN
                            APPLY "ENTRY":U TO cFieldValue.
                        WHEN "Log" THEN
                            APPLY "ENTRY":U TO cFieldValue. /* log-fld */
                    END CASE.
                END. /* Not lvalid */
            END. /* Single value to validate */
            IF NOT lValid THEN
                RETURN ERROR.
        END.  /* End if non-blank value */   /* Task 11011321 */
        ELSE DO:         
            CASE ttSysCtrl.name:
                WHEN "CINVOICE" THEN DO:
                    IF cFieldValue:SCREEN-VALUE NE "FIBREMEXICO" THEN DO:
                        MESSAGE
                            "Invalid Business Form Name."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                        APPLY "ENTRY":U TO cFieldValue.
                        RETURN ERROR.
                    END.
                END. /* end when cinvoice */
            END CASE.
        END. /* else do */
    END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-char-fld-2 C-Win 
PROCEDURE valid-char-fld-2 :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE thisOne       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE comp-char-val AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEntryTo      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSingleValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j             AS INTEGER   NO-UNDO. 
    
    DO WITH FRAME viewFormFrame:
        /* Task 11011321 */
        IF sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "" AND
           CAN-DO(cValidateList,ttSysCtrl.name) THEN DO: 
            lValid = TRUE.     
            /* Process NK1 options where user can select more than one */
            /* option - validate each option individually              */
            IF LOOKUP(ttSysCtrl.name, gvcMultiSelect) GT 0 AND
               INDEX(sys-ctrl-shipto.char-fld:SCREEN-VALUE, ",") GT 0 THEN DO:
                DO i = 1 TO NUM-ENTRIES(sys-ctrl-shipto.char-fld:SCREEN-VALUE):
                    cSingleValue = ENTRY(i, sys-ctrl-shipto.char-fld:SCREEN-VALUE).
                    RUN sys/ref/validSysCtrlChar.p (
                        g_company,
                        g_loc,
                        ttSysCtrl.name,
                        sys-ctrl-shipto.char-fld:LABEL,
                        sys-ctrl-shipto.log-fld:SCREEN-VALUE,
                        cSingleValue,
                        name-fld-list,
                        str-init[LOOKUP(ttSysCtrl.name, name-fld-list)],
                        OUTPUT cEntryTo,
                        OUTPUT lValid
                        ).
                    IF NOT lValid THEN DO:   
                        CASE cEntryTo:
                            WHEN "Char" THEN
                                APPLY "ENTRY":U TO sys-ctrl-shipto.char-fld.
                            WHEN "Log" THEN
                                APPLY "ENTRY":U TO sys-ctrl-shipto.log-fld.
                        END CASE.
                        LEAVE.
                    END. /* if not lvalid */
                END. /* do i = ... */
            END. /* if multiple values to validate */
            ELSE DO:
                RUN sys/ref/validSysCtrlChar.p ( 
                    g_company,
                    g_loc,
                    ttSysCtrl.name,
                    sys-ctrl-shipto.char-fld:LABEL,
                    sys-ctrl-shipto.log-fld:SCREEN-VALUE,
                    sys-ctrl-shipto.char-fld:SCREEN-VALUE,
                    name-fld-list,
                    str-init[LOOKUP(ttSysCtrl.name, name-fld-list)],
                    OUTPUT cEntryTo,
                    OUTPUT lValid
                    ). 
                IF NOT lValid THEN DO:   
                    CASE cEntryTo:
                        WHEN "Char" THEN
                            APPLY "ENTRY":U TO sys-ctrl-shipto.char-fld.
                        WHEN "Log" THEN
                            APPLY "ENTRY":U TO sys-ctrl-shipto.log-fld.
                    END CASE.
                END. /* Not lvalid */
            END. /* Single value to validate */
            IF NOT lValid THEN
                RETURN ERROR.
        END.  /* End if non-blank value */   /* Task 11011321 */
        ELSE DO:         
            CASE ttSysCtrl.name:
                WHEN "CINVOICE" THEN DO:
                    IF sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "FIBREMEXICO" THEN DO:
                        MESSAGE
                            "Invalid Business Form Name."
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                        APPLY "ENTRY":U TO sys-ctrl-shipto.char-fld.
                        RETURN ERROR.
                    END.
                END. /* end when cinvoice */
            END CASE.
        END. /* else do */
    END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-log-fld C-Win 
PROCEDURE valid-log-fld :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    IF ttSysCtrl.name EQ "POEXPORT" AND
       iphWidget:SCREEN-VALUE EQ "yes" THEN DO:
        RUN util/chk-mod2.p ("POEXPORT","NK1","PO Export Module.") NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            iphWidget:SCREEN-VALUE = "no".
            APPLY "ENTRY":U TO iphWidget.
        RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

