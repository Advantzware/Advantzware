&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
DEF TEMP-TABLE ttTableAcct
    FIELD cTable AS CHAR
    FIELD cField AS CHAR.
    
DEF TEMP-TABLE ttAccountConv
    FIELD cOldAcct AS CHAR 
    FIELD cNewAcct AS CHAR.    
    
DEF TEMP-TABLE ttBadAccounts
    FIELD cOldNew AS CHAR 
    FIELD cAcctNo AS CHAR.    
    
DEF TEMP-TABLE ttFullTableList
    FIELD cTable          AS CHAR 
    FIELD cTableDesc      AS CHAR
    FIELD cAllIndexFields AS CHAR
    FIELD cUIndexFields   AS CHAR
    FIELD lHasDate        AS LOG 
    FIELD cDateField      AS CHAR 
    FIELD iRecordCount    AS INT 
    FIELD lConvert        AS LOG 
    .

DEF TEMP-TABLE ttTablesWithMergeFields
    FIELD cFieldType AS CHAR FORMAT "x(12)"
    FIELD cTableName AS CHAR FORMAT "x(24)"
    FIELD cFieldName AS CHAR FORMAT "x(24)"
    . 
    
DEF TEMP-TABLE ttNewCoA
    FIELD fromCompany AS CHAR 
    FIELD fromAcct    AS CHAR 
    FIELD toCompany   AS CHAR 
    FIELD toAcct      AS CHAR 
    FIELD AcctDesc    AS CHAR 
    .
    
DEF TEMP-TABLE ttConvResults
    FIELD cTable AS CHAR 
    FIELD iTotRecs AS INT 
    FIELD iExclCompany AS INT 
    FIELD iExclDate AS INT 
    FIELD iExclAcct AS INT 
    FIELD iTotConv AS INT.    
    
DEF VAR cCompanyList AS CHAR NO-UNDO.
DEF VAR lIgnoreDates AS LOG NO-UNDO.
DEF VAR daStartDate AS DATE NO-UNDO.
DEF VAR daEndDate AS DATE NO-UNDO.
DEF VAR cMapFile AS CHAR NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cTablesToDisplay AS CHAR NO-UNDO.
DEF VAR cTablesToConvert AS CHAR NO-UNDO.
DEF VAR hTBhandle AS HANDLE NO-UNDO.
DEF VAR lCount AS LOG NO-UNDO.
DEF VAR lConvert AS LOG NO-UNDO.
DEF VAR iMergedAccts AS INT NO-UNDO.
DEF VAR iConvList AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsBalances eInstructions fiConvFileLoc ~
fiFromDate fiToDate tbNoDate slCompanyList tbAllCompanies bSelectAll ~
bSimulate bConvert IMAGE-1 
&Scoped-Define DISPLAYED-OBJECTS rsBalances eInstructions fiConvFileLoc ~
fiText-1 fiFromDate fiToDate tbNoDate slCompanyList tbAllCompanies ~
bSelectAll tbFile-1 tbFile-2 tbFile-3 tbFile-4 tbFile-5 tbFile-6 tbFile-7 ~
tbFile-8 tbFile-9 tbFile-10 tbFile-11 tbFile-12 tbFile-13 tbFile-14 ~
tbFile-15 tbFile-16 tbFile-17 tbFile-18 tbFile-19 tbFile-20 tbFile-21 ~
tbFile-22 tbFile-23 tbFile-24 tbFile-25 tbFile-26 tbFile-27 tbFile-28 ~
tbFile-29 tbFile-30 tbFile-31 tbFile-32 tbFile-33 tbFile-34 tbFile-35 ~
tbFile-36 tbFile-37 tbFile-38 tbFile-39 tbFile-40 tbFile-41 tbFile-42 ~
tbFile-43 tbFile-44 tbFile-45 fiCompany fiText-2 fiBalances 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bConvert 
     LABEL "Convert" 
     SIZE 20 BY 2.14
     FONT 6.

DEFINE BUTTON bSimulate 
     LABEL "Simulate Only" 
     SIZE 20 BY 2.14
     FONT 6.

DEFINE VARIABLE eInstructions AS CHARACTER INITIAL "These are the instructions/warnings for running this function.  They are populated in the program's Main Block." 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 152 BY 7.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE fiBalances AS CHARACTER FORMAT "X(256)":U INITIAL "For any balances in account table (GF2):" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95 NO-UNDO.

DEFINE VARIABLE fiCompany AS CHARACTER FORMAT "X(256)":U INITIAL "Select companies to convert:" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE fiConvFileLoc AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\tmp~\GlConv.csv" 
     LABEL "Use this CSV file for Old/New Account map" 
     VIEW-AS FILL-IN 
     SIZE 92 BY 1 NO-UNDO.

DEFINE VARIABLE fiFromDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "For date-specific tables, convert from date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fiText-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Convert GL accounts in these tables:" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fiText-2 AS CHARACTER FORMAT "X(256)":U INITIAL "(tables listed in BOLD are date-specific)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fiToDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2099 
     LABEL "to Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/16x16/magnifying_glass.gif":U
     SIZE 5 BY .71.

DEFINE VARIABLE rsBalances AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Move", "Move",
"Merge", "Merge",
"Ignore", "Ignore"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE slCompanyList AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 39 BY 3.33 NO-UNDO.

DEFINE VARIABLE bSelectAll AS LOGICAL INITIAL no 
     LABEL "Select ALL listed tables" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tbAllCompanies AS LOGICAL INITIAL no 
     LABEL "All?" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .95 NO-UNDO.

DEFINE VARIABLE tbFile-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-18 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-19 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-20 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-21 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-22 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-23 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-24 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-25 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-26 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-27 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-28 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-29 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-30 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-31 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-32 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-33 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-34 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-35 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-36 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-37 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-38 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-39 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-40 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-41 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-42 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-43 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-44 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-45 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbFile-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tbNoDate AS LOGICAL INITIAL no 
     LABEL "Ignore dates, convert all records" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsBalances AT ROW 11.95 COL 46 NO-LABEL
     eInstructions AT ROW 1 COL 1 NO-LABEL NO-TAB-STOP 
     fiConvFileLoc AT ROW 8.62 COL 44 COLON-ALIGNED
     fiText-1 AT ROW 13.38 COL 4 NO-LABEL NO-TAB-STOP 
     fiFromDate AT ROW 9.81 COL 44 COLON-ALIGNED
     fiToDate AT ROW 9.81 COL 72 COLON-ALIGNED
     tbNoDate AT ROW 11 COL 46
     slCompanyList AT ROW 10.76 COL 111 NO-LABEL
     tbAllCompanies AT ROW 9.81 COL 139
     bSelectAll AT ROW 13.38 COL 43
     tbFile-1 AT ROW 14.57 COL 11
     tbFile-2 AT ROW 15.52 COL 11
     tbFile-3 AT ROW 16.48 COL 11
     tbFile-4 AT ROW 17.43 COL 11
     tbFile-5 AT ROW 18.38 COL 11
     tbFile-6 AT ROW 19.33 COL 11
     tbFile-7 AT ROW 20.29 COL 11
     tbFile-8 AT ROW 21.24 COL 11
     tbFile-9 AT ROW 22.19 COL 11
     tbFile-10 AT ROW 23.14 COL 11
     tbFile-11 AT ROW 24.1 COL 11
     tbFile-12 AT ROW 25.05 COL 11
     tbFile-13 AT ROW 26 COL 11
     tbFile-14 AT ROW 26.95 COL 11
     tbFile-15 AT ROW 27.91 COL 11
     tbFile-16 AT ROW 14.57 COL 51
     tbFile-17 AT ROW 15.52 COL 51
     tbFile-18 AT ROW 16.48 COL 51
     tbFile-19 AT ROW 17.43 COL 51
     tbFile-20 AT ROW 18.38 COL 51
     tbFile-21 AT ROW 19.33 COL 51
     tbFile-22 AT ROW 20.29 COL 51
     tbFile-23 AT ROW 21.24 COL 51
     tbFile-24 AT ROW 22.19 COL 51
     tbFile-25 AT ROW 23.14 COL 51
     tbFile-26 AT ROW 24.1 COL 51
     tbFile-27 AT ROW 25.05 COL 51
     tbFile-28 AT ROW 26 COL 51
     tbFile-29 AT ROW 26.95 COL 51
     tbFile-30 AT ROW 27.91 COL 51
     tbFile-31 AT ROW 14.57 COL 91
     tbFile-32 AT ROW 15.52 COL 91
     tbFile-33 AT ROW 16.48 COL 91
     tbFile-34 AT ROW 17.43 COL 91
     tbFile-35 AT ROW 18.38 COL 91
     tbFile-36 AT ROW 19.33 COL 91
     tbFile-37 AT ROW 20.29 COL 91
     tbFile-38 AT ROW 21.24 COL 91
     tbFile-39 AT ROW 22.19 COL 91
     tbFile-40 AT ROW 23.14 COL 91
     tbFile-41 AT ROW 24.1 COL 91
     tbFile-42 AT ROW 25.05 COL 91
     tbFile-43 AT ROW 26 COL 91
     tbFile-44 AT ROW 26.95 COL 91
     tbFile-45 AT ROW 27.91 COL 91
     fiCompany AT ROW 9.81 COL 106 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     bSimulate AT ROW 24.1 COL 132
     bConvert AT ROW 26.71 COL 132
     fiText-2 AT ROW 10.76 COL 3 NO-LABEL NO-TAB-STOP 
     fiBalances AT ROW 11.95 COL 2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     IMAGE-1 AT ROW 8.86 COL 139
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152.4 BY 28.43.


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
         TITLE              = "GL Account Converter"
         HEIGHT             = 28.43
         WIDTH              = 152.4
         MAX-HEIGHT         = 28.43
         MAX-WIDTH          = 157.4
         VIRTUAL-HEIGHT     = 28.43
         VIRTUAL-WIDTH      = 157.4
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiBalances IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiBalances:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiCompany IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiCompany:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiText-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiText-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       fiText-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbFile-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-1:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-10 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-10:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-11:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-12 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-12:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-13 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-13:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-14 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-14:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-15 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-15:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-16 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-16:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-17 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-17:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-18 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-18:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-19 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-19:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-20 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-20:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-21 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-21:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-22 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-22:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-23 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-23:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-24 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-24:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-25 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-25:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-26 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-26:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-27 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-27:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-28 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-28:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-29 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-29:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-3:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-30 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-30:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-31 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-31:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-32 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-32:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-33 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-33:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-34 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-34:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-35 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-35:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-36 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-36:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-37 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-37:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-38 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-38:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-39 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-39:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-4:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-40 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-40:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-41 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-41:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-42 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-42:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-43 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-43:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-44 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-44:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-45 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-45:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-5:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-6:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-7:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-8 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-8:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

/* SETTINGS FOR TOGGLE-BOX tbFile-9 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbFile-9:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "|".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* GL Account Converter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* GL Account Converter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSelectAll C-Win
ON VALUE-CHANGED OF bSelectAll IN FRAME DEFAULT-FRAME /* Select ALL listed tables */
DO:
    IF SELF:CHECKED THEN ASSIGN   
            tbFile-1:CHECKED = TRUE 
            tbFile-2:CHECKED = TRUE 
            tbFile-3:CHECKED = TRUE 
            tbFile-4:CHECKED = TRUE 
            tbFile-5:CHECKED = TRUE 
            tbFile-6:CHECKED = TRUE 
            tbFile-7:CHECKED = TRUE 
            tbFile-8:CHECKED = TRUE 
            tbFile-9:CHECKED = TRUE 
            tbFile-10:CHECKED = TRUE 
            tbFile-11:CHECKED = TRUE 
            tbFile-12:CHECKED = TRUE 
            tbFile-13:CHECKED = TRUE 
            tbFile-14:CHECKED = TRUE 
            tbFile-15:CHECKED = TRUE 
            tbFile-16:CHECKED = TRUE 
            tbFile-17:CHECKED = TRUE 
            tbFile-18:CHECKED = TRUE 
            tbFile-19:CHECKED = TRUE 
            tbFile-20:CHECKED = TRUE 
            tbFile-21:CHECKED = TRUE 
            tbFile-22:CHECKED = TRUE 
            tbFile-23:CHECKED = TRUE 
            tbFile-24:CHECKED = TRUE 
            tbFile-25:CHECKED = TRUE 
            tbFile-26:CHECKED = TRUE 
            tbFile-27:CHECKED = TRUE 
            tbFile-28:CHECKED = TRUE 
            tbFile-29:CHECKED = TRUE 
            tbFile-30:CHECKED = TRUE 
            tbFile-31:CHECKED = TRUE 
            tbFile-32:CHECKED = TRUE 
            tbFile-33:CHECKED = TRUE 
            tbFile-34:CHECKED = TRUE 
            tbFile-35:CHECKED = TRUE 
            tbFile-36:CHECKED = TRUE 
            tbFile-37:CHECKED = TRUE 
            tbFile-38:CHECKED = TRUE 
            tbFile-39:CHECKED = TRUE 
            tbFile-40:CHECKED = TRUE 
            tbFile-41:CHECKED = TRUE 
            tbFile-42:CHECKED = TRUE 
            tbFile-43:CHECKED = TRUE 
            tbFile-44:CHECKED = TRUE 
            tbFile-45:CHECKED = TRUE
            . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSimulate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSimulate C-Win
ON CHOOSE OF bSimulate IN FRAME DEFAULT-FRAME /* Simulate Only */
OR CHOOSE OF bConvert
DO:
    DEF VAR lError AS LOG NO-UNDO.
    
    STATUS DEFAULT "Reading settings and options...".
    STATUS INPUT "Reading settings and options...".
    OUTPUT TO c:\tmp\GLConversionReport.txt.
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Beginning GL Account Conversion on " + STRING(TODAY) + " at " + STRING(time,"HH:MM:SS AM") + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Options selected:" + CHR(10).
    PUT UNFORMATTED "   CSV file used to convert: " + fiConvFileLoc:SCREEN-VALUE + CHR(10).
    PUT UNFORMATTED "   Date range: FROM " + fiFromDate:SCREEN-VALUE + " TO " + fiToDate:SCREEN-VALUE + CHR(10).
    PUT UNFORMATTED "   Ignore dates: " + STRING(tbNoDate:CHECKED) + CHR(10).
    PUT UNFORMATTED "   GL account balances action: " + rsBalances:SCREEN-VALUE + CHR(10) + CHR(10). 
    PUT UNFORMATTED "Selected Companies:" + CHR(10).
    DO ictr = 1 TO NUM-ENTRIES(slCompanyList:SCREEN-VALUE):
        PUT UNFORMATTED "   " + ENTRY(iCtr,slCompanyList:SCREEN-VALUE) + CHR(10).
    END.
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    
    IF SELF:NAME EQ "bSimulate" THEN ASSIGN 
        lConvert = NO.
    ELSE IF SELF:NAME EQ "bConvert" THEN ASSIGN 
        lConvert = YES.
        
    ASSIGN 
        cCompanyList = "".
    DO iCtr = 1 TO NUM-ENTRIES(slCompanyList:SCREEN-VALUE):
        ASSIGN 
            cCompanyList = cCompanyList + ENTRY(1,ENTRY(iCtr,slCompanyList:SCREEN-VALUE),"-") + ",".
    END.
    ASSIGN 
        cCompanyList = TRIM(cCompanyList,",").
            
    ASSIGN 
        cTablesToConvert = 
            (IF tbFile-1:CHECKED THEN tbFile-1:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-2:CHECKED THEN tbFile-2:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-3:CHECKED THEN tbFile-3:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-4:CHECKED THEN tbFile-4:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-5:CHECKED THEN tbFile-5:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-6:CHECKED THEN tbFile-6:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-7:CHECKED THEN tbFile-7:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-8:CHECKED THEN tbFile-8:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-9:CHECKED THEN tbFile-9:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-10:CHECKED THEN tbFile-10:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-11:CHECKED THEN tbFile-11:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-12:CHECKED THEN tbFile-12:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-13:CHECKED THEN tbFile-13:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-14:CHECKED THEN tbFile-14:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-15:CHECKED THEN tbFile-15:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-16:CHECKED THEN tbFile-16:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-17:CHECKED THEN tbFile-17:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-18:CHECKED THEN tbFile-18:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-19:CHECKED THEN tbFile-19:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-20:CHECKED THEN tbFile-20:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-21:CHECKED THEN tbFile-21:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-22:CHECKED THEN tbFile-22:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-23:CHECKED THEN tbFile-23:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-24:CHECKED THEN tbFile-24:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-25:CHECKED THEN tbFile-25:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-26:CHECKED THEN tbFile-26:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-27:CHECKED THEN tbFile-27:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-28:CHECKED THEN tbFile-28:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-29:CHECKED THEN tbFile-29:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-30:CHECKED THEN tbFile-30:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-31:CHECKED THEN tbFile-31:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-32:CHECKED THEN tbFile-32:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-33:CHECKED THEN tbFile-33:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-34:CHECKED THEN tbFile-34:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-35:CHECKED THEN tbFile-35:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-36:CHECKED THEN tbFile-36:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-37:CHECKED THEN tbFile-37:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-38:CHECKED THEN tbFile-38:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-39:CHECKED THEN tbFile-39:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-40:CHECKED THEN tbFile-40:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-41:CHECKED THEN tbFile-41:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-42:CHECKED THEN tbFile-42:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-43:CHECKED THEN tbFile-43:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-44:CHECKED THEN tbFile-44:PRIVATE-DATA  + ","  ELSE "") +
            (IF tbFile-45:CHECKED THEN tbFile-45:PRIVATE-DATA  + ","  ELSE "").
        cTablesToConvert = REPLACE(cTablesToConvert,"|","").
        cTablesToConvert = REPLACE(cTablesToConvert,",,",",").
    cTablesToConvert = TRIM(cTablesToConvert,",").
        
    FOR EACH ttFullTableList:
        IF CAN-DO(cTablesToConvert,ttFullTableList.cTable) THEN ASSIGN 
            ttFullTableList.lConvert = TRUE.
    END.   
    PUT UNFORMATTED "Tables/fields selected for conversion:" + CHR(10).
    FOR EACH ttFullTableList WHERE 
        ttFullTableList.lConvert EQ TRUE
        BY ttFullTableList.cTable:
        FIND FIRST ttTablesWithMergeFields WHERE 
            ttTablesWithMergeFields.cFieldType EQ "Account" AND 
            ttTablesWithMergeFields.cTableName EQ ttFullTableList.cTable
            NO-ERROR.
        PUT UNFORMATTED "   " + ttFullTableList.cTable + " - " + ttFullTableList.cTableDesc + CHR(10).
        IF AVAIL ttTablesWithMergeFields THEN 
            PUT UNFORMATTED "      " + ttTablesWithMergeFields.cFieldName + CHR(10).
    END.    
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    
    STATUS DEFAULT "Reading CoA conversion file...".
    STATUS INPUT "Reading CoA conversion file...".
    RUN pLoadCoAFromCsv (INPUT fiConvFileLoc:SCREEN-VALUE, OUTPUT lError).
    IF lError THEN RETURN NO-APPLY.
    PUT UNFORMATTED "List of accounts to convert:" + CHR(10).
    PUT UNFORMATTED "   Old Account No.          New Account No." + CHR(10). 
    FOR EACH ttAccountConv:
        PUT UNFORMATTED "   " + STRING(ttAccountConv.cOldAcct,"x(22)") + "   " +  STRING(ttAccountConv.cNewAcct,"x(22)") + CHR(10). 
    END.
    OUTPUT CLOSE.
    
    FOR EACH ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = "Account":
        FIND FIRST ttFullTableList WHERE 
            ttFullTableList.cTable EQ ttTablesWithMergeFields.cTableName AND 
            ttFullTableList.lConvert EQ TRUE 
            NO-ERROR.
        IF AVAIL ttFullTableList THEN 
            RUN pConvertAccountTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName, ttFullTableList.cDateField).
    END.
    
    IF rsBalances:SCREEN-VALUE EQ "Move"
    OR rsBalances:SCREEN-VALUE EQ "Merge" THEN 
        RUN pMergeAccountBalances.
    
    OUTPUT TO c:\tmp\GLConversionReport.txt APPEND.
    PUT UNFORMATTED CHR(10) + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Conversion Summary:" + CHR(10).
    PUT  
        "Table Name"            AT 1
        "Total Records"         TO 25
        "Co. not selected"      TO 44
        "Outside date range"    TO 66
        "Account not listed"    TO 86
        "Converted records"     TO 105
        SKIP.
    FOR EACH ttConvResults:
        PUT 
            ttConvResults.cTable        AT 1
            ttConvResults.iTotRecs      TO 25
            ttConvResults.iExclCompany  TO 44
            ttConvResults.iExclDate     TO 66
            ttConvResults.iExclAcct     TO 86
            ttConvResults.iTotConv      TO 105
            SKIP.
    END.
    PUT UNFORMATTED CHR(10) + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Total accounts in conversion table = " + STRING(iConvList) + CHR(10).
    PUT UNFORMATTED "Total accounts with merged balances = " + STRING(iMergedAccts) + CHR(10).
    PUT UNFORMATTED CHR(10) + CHR(10).
    PUT UNFORMATTED "OLD accounts with invalid account numbers" + CHR(10).
    FOR EACH ttBadAccounts WHERE 
        ttBadAccounts.cOldNew EQ "OLD":
        PUT UNFORMATTED ttBadAccounts.cAcctNo + CHR(10).
    END.
    PUT UNFORMATTED "NEW accounts with invalid account numbers" + CHR(10).
    FOR EACH ttBadAccounts WHERE 
        ttBadAccounts.cOldNew EQ "NEW":
        PUT UNFORMATTED ttBadAccounts.cAcctNo + CHR(10).
    END.
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    PUT UNFORMATTED "Ending GL Account Conversion on " + STRING(TODAY) + " at " + STRING(time,"HH:MM:SS AM") + CHR(10).
    PUT UNFORMATTED FILL("-",80) + CHR(10).
    OUTPUT CLOSE.
    
    STATUS DEFAULT "Conversion complete.".
    STATUS INPUT "Conversion complete.".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 C-Win
ON MOUSE-SELECT-CLICK OF IMAGE-1 IN FRAME DEFAULT-FRAME
DO:
    DEF VAR cCSVfilename AS CHAR NO-UNDO.
    SYSTEM-DIALOG GET-FILE cCSVfilename
        TITLE "Choose GL Converter Import File"
        FILTERS "CSV Files (*.csv)"  "*.csv"
        CREATE-TEST-FILE
        USE-FILENAME
        DEFAULT-EXTENSION ".csv"
        INITIAL-DIR "c:\tmp".
    ASSIGN 
        fiConvFileLoc:SCREEN-VALUE = cCSVfilename.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAllCompanies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAllCompanies C-Win
ON VALUE-CHANGED OF tbAllCompanies IN FRAME DEFAULT-FRAME /* All? */
DO:
    IF SELF:CHECKED THEN DO:
        ASSIGN
            slCompanyList:SCREEN-VALUE = slCompanyList:LIST-ITEMS 
            slCompanyList:SENSITIVE = FALSE.
        slCompanyList:SCROLL-TO-ITEM(1).
    END.
    ELSE ASSIGN 
        slCompanyList:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbNoDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbNoDate C-Win
ON VALUE-CHANGED OF tbNoDate IN FRAME DEFAULT-FRAME /* Ignore dates, convert all records */
DO:
    IF SELF:CHECKED THEN ASSIGN 
        lIgnoreDates = TRUE 
        fiFromDate:SENSITIVE = FALSE  
        fiToDate:SENSITIVE = FALSE
        fiFromDate:SCREEN-VALUE = "01/01/1900"
        fiToDate:SCREEN-VALUE = "12/31/2099".
    ELSE ASSIGN 
        lIgnoreDates = FALSE  
        fiFromDate:SENSITIVE = TRUE  
        fiToDate:SENSITIVE = TRUE.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  
    RUN enable_UI.

    ASSIGN 
        eInstructions:SCREEN-VALUE = "INSTRUCTIONS:" + CHR(10) + "This function allows an administrator to selectively or globally " + 
        "reassign General Ledger Account Numbers.  The use of this program requires a separate file on " + 
        "the filesystem that contains a 'map' of the old and new account numbers you wish to reassign, " +
        "stored in a .CSV file.  A sample file 'SampleGlConv.csv' can be found in the /Resources/Template " +
        "directory on your system.  You should make a copy of this file and store it anywhere on your " + 
        "server.  Use the fill-in below to 'point' to your file for conversion." + CHR(10) +
        "Many of the files that contain Account numbers (like GL History, for example) are 'date specific'. " +
        "You can use the date controls below to limit the conversion of these files based on the original " +
        "transaction dates.  Checking the 'Ignore dates' box will allow the converter to process ALL records " + 
        "in these files. You may also specify one or more Companies to process in the selection list provided." + CHR(10) +
        "Choosing the 'Simulate' button will create a file of records that would be converted during the full " +
        "conversion.  Choosing 'Convert will generate the log AND convert the records." + CHR(10) +
        "YOU SHOULD MAKE A FULL BACKUP OF YOUR DATABASE WITH THE DB MAINTENANCE TOOL BEFORE CONVERTING!!!".
        
    FOR EACH company NO-LOCK:
        slCompanyList:ADD-LAST (company.company + "-" + company.name).
    END.
        
    MESSAGE 
        "This program can COUNT the number of records in each account-related table, " + 
        "and display that count when you hover your mouse over the table name.  " + 
        "Enabling this function can significantly slow down the initial scan for files." SKIP 
        "Do you want to enable this record count?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lCount.
        
    STATUS DEFAULT "Scanning database for tables with GL account information".
    SESSION:SET-WAIT-STATE ("GENERAL"). 
    RUN pScanDbForTablesWithAccountField.  
    
    iCtr = 0.
    FOR EACH ttFullTableList:
        IF INDEX(ttFullTableList.cTableDesc,"(del)") NE 0 THEN NEXT.
        iCtr = iCtr + 1.
        CASE iCtr:
            WHEN 1 THEN ASSIGN hTBhandle = tbFile-1:HANDLE.
            WHEN 2 THEN ASSIGN hTBhandle = tbFile-2:HANDLE.
            WHEN 3 THEN ASSIGN hTBhandle = tbFile-3:HANDLE. 
            WHEN 4 THEN ASSIGN hTBhandle = tbFile-4:HANDLE.
            WHEN 5 THEN ASSIGN hTBhandle = tbFile-5:HANDLE.
            WHEN 6 THEN ASSIGN hTBhandle = tbFile-6:HANDLE.
            WHEN 7 THEN ASSIGN hTBhandle = tbFile-7:HANDLE.
            WHEN 8 THEN ASSIGN hTBhandle = tbFile-8:HANDLE.
            WHEN 9 THEN ASSIGN hTBhandle = tbFile-9:HANDLE.
            WHEN 10 THEN ASSIGN hTBhandle = tbFile-10:HANDLE. 
            WHEN 11 THEN ASSIGN hTBhandle = tbFile-11:HANDLE.
            WHEN 12 THEN ASSIGN hTBhandle = tbFile-12:HANDLE.
            WHEN 13 THEN ASSIGN hTBhandle = tbFile-13:HANDLE.
            WHEN 14 THEN ASSIGN hTBhandle = tbFile-14:HANDLE.
            WHEN 15 THEN ASSIGN hTBhandle = tbFile-15:HANDLE.
            WHEN 16 THEN ASSIGN hTBhandle = tbFile-16:HANDLE.
            WHEN 17 THEN ASSIGN hTBhandle = tbFile-17:HANDLE.
            WHEN 18 THEN ASSIGN hTBhandle = tbFile-18:HANDLE.
            WHEN 19 THEN ASSIGN hTBhandle = tbFile-19:HANDLE.
            WHEN 20 THEN ASSIGN hTBhandle = tbFile-20:HANDLE.
            WHEN 21 THEN ASSIGN hTBhandle = tbFile-21:HANDLE.
            WHEN 22 THEN ASSIGN hTBhandle = tbFile-22:HANDLE.
            WHEN 23 THEN ASSIGN hTBhandle = tbFile-23:HANDLE.
            WHEN 24 THEN ASSIGN hTBhandle = tbFile-24:HANDLE.
            WHEN 25 THEN ASSIGN hTBhandle = tbFile-25:HANDLE.
            WHEN 26 THEN ASSIGN hTBhandle = tbFile-26:HANDLE.
            WHEN 27 THEN ASSIGN hTBhandle = tbFile-27:HANDLE.
            WHEN 28 THEN ASSIGN hTBhandle = tbFile-28:HANDLE.
            WHEN 29 THEN ASSIGN hTBhandle = tbFile-29:HANDLE.
            WHEN 30 THEN ASSIGN hTBhandle = tbFile-30:HANDLE.
            WHEN 31 THEN ASSIGN hTBhandle = tbFile-31:HANDLE.
            WHEN 32 THEN ASSIGN hTBhandle = tbFile-32:HANDLE.
            WHEN 33 THEN ASSIGN hTBhandle = tbFile-33:HANDLE.
            WHEN 34 THEN ASSIGN hTBhandle = tbFile-34:HANDLE.
            WHEN 35 THEN ASSIGN hTBhandle = tbFile-35:HANDLE.
            WHEN 36 THEN ASSIGN hTBhandle = tbFile-36:HANDLE.
            WHEN 37 THEN ASSIGN hTBhandle = tbFile-37:HANDLE.
            WHEN 38 THEN ASSIGN hTBhandle = tbFile-38:HANDLE.
            WHEN 39 THEN ASSIGN hTBhandle = tbFile-39:HANDLE.
            WHEN 40 THEN ASSIGN hTBhandle = tbFile-40:HANDLE.
            WHEN 41 THEN ASSIGN hTBhandle = tbFile-41:HANDLE.
            WHEN 42 THEN ASSIGN hTBhandle = tbFile-42:HANDLE.
            WHEN 43 THEN ASSIGN hTBhandle = tbFile-43:HANDLE.
            WHEN 44 THEN ASSIGN hTBhandle = tbFile-44:HANDLE.
            WHEN 45 THEN ASSIGN hTBhandle = tbFile-45:HANDLE.
        END CASE.
        ASSIGN 
            hTBhandle:LABEL = ttFullTableList.cTableDesc
            hTBhandle:SENSITIVE = TRUE
            hTBhandle:PRIVATE-DATA = ttFullTableList.cTable
            hTBhandle:FONT = IF ttFullTableList.lHasDate THEN 6 ELSE ?
            hTBhandle:TOOLTIP = "(" + ttFullTableList.cTable + ") " + STRING(ttFullTableList.iRecordCount). 
    END.
    SESSION:SET-WAIT-STATE (""). 
    
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY rsBalances eInstructions fiConvFileLoc fiText-1 fiFromDate fiToDate 
          tbNoDate slCompanyList tbAllCompanies bSelectAll tbFile-1 tbFile-2 
          tbFile-3 tbFile-4 tbFile-5 tbFile-6 tbFile-7 tbFile-8 tbFile-9 
          tbFile-10 tbFile-11 tbFile-12 tbFile-13 tbFile-14 tbFile-15 tbFile-16 
          tbFile-17 tbFile-18 tbFile-19 tbFile-20 tbFile-21 tbFile-22 tbFile-23 
          tbFile-24 tbFile-25 tbFile-26 tbFile-27 tbFile-28 tbFile-29 tbFile-30 
          tbFile-31 tbFile-32 tbFile-33 tbFile-34 tbFile-35 tbFile-36 tbFile-37 
          tbFile-38 tbFile-39 tbFile-40 tbFile-41 tbFile-42 tbFile-43 tbFile-44 
          tbFile-45 fiCompany fiText-2 fiBalances 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rsBalances eInstructions fiConvFileLoc fiFromDate fiToDate tbNoDate 
         slCompanyList tbAllCompanies bSelectAll bSimulate bConvert IMAGE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildSingleFieldList C-Win 
PROCEDURE pBuildSingleFieldList :
/*------------------------------------------------------------------------------
 Purpose:   Updates ttTablesWithMergeFields to populate list of fields in a single table
            that require conversion BY TYPE
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcField AS CHAR NO-UNDO.
    
    FIND FIRST ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = ipcType AND 
        ttTablesWithMergeFields.cTableName EQ ipcTable
        NO-ERROR.
    IF NOT AVAIL ttTablesWithMergeFields THEN 
    DO:
        CREATE ttTablesWithMergeFields.
        ASSIGN 
            ttTablesWithMergeFields.cFieldType = ipcType
            ttTablesWithMergeFields.cTableName = ipcTable
            ttTablesWithMergeFields.cFieldName = ipcField.
    END.
    ELSE ASSIGN 
            ttTablesWithMergeFields.cFieldName = ttTablesWithMergeFields.cFieldName + "," + ipcField.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConvertAccountTable C-Win 
PROCEDURE pConvertAccountTable :
/*------------------------------------------------------------------------------
 Purpose:   Convert all account-type fields in each record of a single table
 Notes:     Note this method of conversion effectively eliminates the collision problem, 
            since company code DOES NOT change until all other index info has been converted.
            Since new records are not created, the rec_key for related tables does note change
            Also note, this logic is dependent on customer-supplied GL account mapping
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcDateField AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hDateField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR cCompVal AS CHAR NO-UNDO.
    DEF VAR cFields AS CHAR NO-UNDO.
    DEF VAR cThisTable AS CHAR NO-UNDO.
    DEF VAR iTotRecs AS INT NO-UNDO.
    DEF VAR iExclCompany AS INT NO-UNDO.
    DEF VAR iExclDate AS INT NO-UNDO.
    DEF VAR iExclAcct AS INT NO-UNDO.
    DEF VAR lBadAcct AS LOG NO-UNDO.
    
    IF lConvert THEN STATUS DEFAULT "Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList.
    ELSE STATUS DEFAULT "Reviewing Fields in Table - " + ipcTableName + " - " + ipcFieldList.
    IF lConvert THEN STATUS INPUT "Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList.
    ELSE STATUS INPUT "Reviewing Fields in Table - " + ipcTableName + " - " + ipcFieldList.

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName + " BY ROWID(" + ipcTableName + ")").
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    checkDate:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hDateField = hBuffer:BUFFER-FIELD (iCtr).
        IF hDateField:NAME = ipcDateField THEN  
            LEAVE checkDate.
    END.

    DO-LOOP:
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF hQuery:QUERY-OFF-END THEN LEAVE DO-LOOP.
        ASSIGN 
            iTotRecs = iTotRecs + 1.
            
        /* If the record is not in a company selected for conversion, skip it */
        IF hCoField:NAME NE ""
        AND NOT CAN-DO(cCompanyList, hCoField:BUFFER-VALUE) THEN DO:
            ASSIGN 
                iExclCompany = iExclCompany + 1.
            NEXT.
        END.
        
        /* If date checking is on and the record falls outside the specified date range, skip it */
        IF NOT tbNoDate:CHECKED IN FRAME {&frame-name}  
        AND hDateField:NAME NE ?
        AND (hDateField:BUFFER-VALUE LT DATE(fiFromDate:SCREEN-VALUE)
            OR hDateField:BUFFER-VALUE GT DATE(fiToDate:SCREEN-VALUE)) THEN DO: 
            ASSIGN 
                iExclDate = iExclDate + 1.
            NEXT.
        END.
            
        ASSIGN 
            lBadAcct = TRUE.
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    FIND FIRST ttAccountConv WHERE 
                        ttAccountConv.cOldAcct EQ hField:BUFFER-VALUE
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttAccountConv THEN ASSIGN
                        lBadAcct = FALSE.
                    IF AVAIL ttAccountConv 
                    AND lConvert THEN ASSIGN 
                        hField:BUFFER-VALUE = ttAccountConv.cNewAcct NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    FIND FIRST ttAccountConv WHERE 
                        ttAccountconv.cOldAcct EQ hField:BUFFER-VALUE(jCtr)
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttAccountconv THEN ASSIGN 
                        lBadAcct = FALSE.
                    IF AVAIL ttAccountConv
                    AND lConvert THEN ASSIGN 
                        hField:BUFFER-VALUE(jCtr) = ttAccountConv.cNewAcct NO-ERROR.
                END.
            END.
        END.
        IF lBadAcct THEN ASSIGN 
            iExclAcct = iExclAcct + 1.
    END.
    
    CREATE ttConvResults.
    ASSIGN 
        ttConvResults.cTable = ipcTableName
        ttConvResults.iTotRecs = iTotRecs
        ttConvResults.iExclCompany = iExclCompany
        ttConvResults.iExclDate = iExclDate
        ttConvResults.iExclAcct = iExclAcct
        ttConvResults.iTotConv = iTotRecs - iExclCompany - iExclDate - iExclAcct.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateQuery C-Win 
PROCEDURE pCreateQuery :
/*------------------------------------------------------------------------------
 Purpose:   Returns handle to opened query
 Notes:     Used to provide record counts to ttFullTableList
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ophQuery AS HANDLE NO-UNDO.

    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.

    IF VALID-HANDLE(hBuffer) THEN DELETE WIDGET hBuffer.                                                          
    IF VALID-HANDLE(ophQuery) THEN DELETE WIDGET ophQuery.                                                
    CREATE BUFFER hBuffer FOR TABLE ipcTable.
    CREATE QUERY ophQuery.
    ophQuery:ADD-BUFFER(hBuffer).
    ophQuery:QUERY-PREPARE("FOR EACH " + ipcTable + " BY ROWID(" + ipcTable + ")").
    ophQuery:QUERY-OPEN().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadCoAFromCsv C-Win 
PROCEDURE pLoadCoAFromCsv :
/*------------------------------------------------------------------------------
 Purpose:   Creates COA mapping temp-table from user-supplied spreadsheet
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCoAConvFile AS CHAR.
    DEF OUTPUT PARAMETER oplError AS LOG.
     
    DEF VAR cLine AS CHAR NO-UNDO.
    DEF VAR lOneBad AS LOG NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iOldCol AS INT NO-UNDO.
    DEF VAR iNewCol AS INT NO-UNDO.
    
    IF SEARCH(ipcCoAConvFile) = ? THEN DO:
        MESSAGE 
            "Unable to locate the GL Conversion spreadsheet." SKIP 
            "Please correct and try again."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplError = TRUE.
        RETURN.
    END.
        
    INPUT FROM VALUE(ipcCoAConvFile).
    IMPORT UNFORMATTED cLine. /* Get and discard the header line */
    /* If the user has exported from GF2, this allows using that exported .csv */
    DO jCtr = 1 TO NUM-ENTRIES(cLine,","):
        IF ENTRY(jCtr,cLine,",") EQ "Old Acct" 
        OR ENTRY(jCtr,cLine,",") EQ "Account No" THEN ASSIGN 
                iOldCol = jCtr.
        IF ENTRY(jCtr,cLine,",") EQ "New Acct" THEN ASSIGN 
            iNewCol = jCtr.
    END.

    IF iOldCol EQ 0
    OR iNewCol EQ 0 THEN DO:
        MESSAGE 
            "There was an error reading your input file. Please review the" skip
            "instructions for creating the input CSV file and try again."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN 
            oplError = TRUE.
        RETURN.
    END. 
    
    REPEAT:
        IMPORT UNFORMATTED cLine.
        IF ENTRY(iOldCol,cLine,",") EQ ""
        OR ENTRY(iNewCol,cLine,",") EQ "" THEN NEXT.
        CREATE ttAccountConv.
        ASSIGN 
            iConvList = iConvList + 1
            ttAccountConv.cOldAcct = ENTRY(iOldCol,cLine,",")
            ttAccountConv.cNewAcct = ENTRY(iNewCol,cLine,",").
        IF NOT CAN-FIND(FIRST account WHERE account.actnum EQ ttAccountConv.cOldAcct)
        OR NOT CAN-FIND(FIRST account WHERE account.actnum EQ ttAccountConv.cNewAcct) THEN ASSIGN 
            lOneBad = TRUE. 
        IF NOT CAN-FIND(FIRST account WHERE account.actnum EQ ttAccountConv.cOldAcct) THEN DO:
            CREATE ttBadAccounts.
            ASSIGN 
                ttBadAccounts.cOldNew = "OLD"
                ttBadAccounts.cAcctNo = ttAccountConv.cOldAcct.
        END. 
        IF NOT CAN-FIND(FIRST account WHERE account.actnum EQ ttAccountConv.cNewAcct) THEN 
        DO:
            CREATE ttBadAccounts.
            ASSIGN 
                ttBadAccounts.cOldNew = "NEW"
                ttBadAccounts.cAcctNo = ttAccountConv.cNewAcct.
        END. 
    END.
    IF lOneBad THEN DO:
        MESSAGE 
            "At least one account in the conversion table does not exist in the " +
            "GL Account master.  Note: If you are updating the Account master table," + 
            "this is expected behavior.  A list of invalid accounts will be provided " +
            "in the Conversion output report (c:\tmp\GLConversionReport.csv)." SKIP 
            "Do you want to proceed anyway?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOG.
        IF NOT lContinue THEN DO:
            ASSIGN 
                oplError = TRUE.
            RETURN.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMergeAccountBalances C-Win 
PROCEDURE pMergeAccountBalances :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF BUFFER baccount FOR account.
    DEF VAR jctr AS INT NO-UNDO.
    
    STATUS DEFAULT "Merging CoA account balances...".
    STATUS INPUT "Merging CoA account balances...".

    DO ictr = 1 TO NUM-ENTRIES(cCompanyList):
        FOR EACH ttAccountConv:
            FIND FIRST account WHERE 
                account.company EQ ENTRY(iCtr,cCompanyList) AND 
                account.actnum EQ ttAccountConv.cOldAcct
                NO-LOCK NO-ERROR.
            IF NOT AVAIL account THEN NEXT.   
            FIND FIRST baccount WHERE 
                baccount.company EQ ENTRY(iCtr,cCompanyList) AND 
                baccount.actnum EQ ttAccountConv.cNewAcct
                EXCLUSIVE NO-ERROR.
            IF NOT AVAIL baccount THEN NEXT.
            IF baccount.cyr-open EQ account.cyr-open THEN NEXT.
            IF baccount.lyr-open EQ account.lyr-open THEN NEXT.
            IF rsBalances:SCREEN-VALUE IN FRAME {&frame-name} EQ "Merge" THEN DO:
                ASSIGN 
                    baccount.cyr-open = baccount.cyr-open + account.cyr-open
                    baccount.lyr-open = baccount.lyr-open + account.lyr-open
                    account.cyr-open = 0
                    account.lyr-open = 0
                    iMergedAccts = iMergedAccts + 1.
                DO jctr = 1 TO 13:
                    ASSIGN 
                        baccount.cyr[jctr] = baccount.cyr[jctr] + account.cyr[jctr]
                        baccount.lyr[jctr] = baccount.lyr[jctr] + account.lyr[jctr]
                        baccount.bud[jctr] = baccount.bud[jctr] + account.bud[jctr]
                        baccount.ly-bud[jctr] = baccount.ly-bud[jctr] + account.ly-bud[jctr]
                        baccount.ny-bud[jctr] = baccount.ny-bud[jctr] + account.ny-bud[jctr]
                        account.cyr[jctr] = 0
                        account.lyr[jctr] = 0
                        account.bud[jctr] = 0
                        account.ly-bud[jctr] = 0 
                        account.ny-bud[jctr] = 0
                        .
                END.
            END.
            ELSE IF rsBalances:SCREEN-VALUE IN FRAME {&frame-name} EQ "Move" THEN DO:
                ASSIGN 
                    baccount.cyr-open = account.cyr-open
                    baccount.lyr-open = account.lyr-open
                    account.cyr-open = 0
                    account.lyr-open = 0
                    iMergedAccts = iMergedAccts + 1.
                DO jctr = 1 TO 13:
                    ASSIGN 
                        baccount.cyr[jctr] = account.cyr[jctr]
                        baccount.lyr[jctr] = account.lyr[jctr]
                        baccount.bud[jctr] = account.bud[jctr]
                        baccount.ly-bud[jctr] = account.ly-bud[jctr]
                        baccount.ny-bud[jctr] = account.ny-bud[jctr]
                        account.cyr[jctr] = 0
                        account.lyr[jctr] = 0
                        account.bud[jctr] = 0
                        account.ly-bud[jctr] = 0 
                        account.ny-bud[jctr] = 0
                        .
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScanDbForTablesWithAccountField C-Win 
PROCEDURE pScanDbForTablesWithAccountField :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER b_field FOR _field.
    DEFINE BUFFER c_field FOR _field.
    
    STATUS DEFAULT "Building lists of tables/fields to convert...".
    STATUS INPUT "Building lists of tables/fields to convert...".

    DEF VAR hQuery AS HANDLE NO-UNDO.
    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "actnum" OR
        _field._field-name MATCHES "acct*" OR 
        _field._field-name EQ "check-act" OR 
        _field._field-name MATCHES "*acct*" OR 
        _field._label MATCHES "*Account*" OR
        _field._label MATCHES "*Acct*" OR
        _field._label MATCHES "Account*" OR
        _field._label MATCHES "Acct*" OR
        _field._label MATCHES "*Account" OR
        _field._label MATCHES "*Acct*" OR
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" OR
        _field._help MATCHES "account*" OR
        _field._help MATCHES "acct*" OR  
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" 
        :
        IF _field._label MATCHES "*desc*" 
            OR _field._label MATCHES "*period*" 
            OR _field._label MATCHES "*bank*" 
            OR _field._label MATCHES "desc*" 
            OR _field._label MATCHES "period*" 
            OR _field._label MATCHES "bank*" 
            OR _field._label MATCHES "*desc"
            OR _field._label MATCHES "*period" 
            OR _field._label MATCHES "*bank" 
            OR _field._label MATCHES "*Desc*" 
            OR _field._label MATCHES "*Period*" 
            OR _field._label MATCHES "*Bank*" 
            OR _field._label MATCHES "Desc*" 
            OR _field._label MATCHES "Period*" 
            OR _field._label MATCHES "Bank*" 
            OR _field._label MATCHES "*Desc"
            OR _field._label MATCHES "*Period" 
            OR _field._label MATCHES "*Bank"
            OR _field._help MATCHES "*desc*" 
            OR _field._help MATCHES "*credit card*" 
            OR _field._help MATCHES "*period*" 
            OR _field._help MATCHES "desc*" 
            OR _field._help MATCHES "credit card*" 
            OR _field._help MATCHES "period*"
            OR _field._help MATCHES "*desc" 
            OR _field._help MATCHES "*credit card" 
            OR _field._help MATCHES "*period"
            OR _field._help MATCHES "*balance"
            THEN NEXT.
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file 
        AND NOT CAN-FIND(FIRST ttFullTableList WHERE ttFullTableList.cTable EQ _file._file-name) THEN 
        DO:
            CREATE ttFullTableList.
            ASSIGN 
                ttFullTableList.cTable     = _file._file-name
                ttFullTableList.cTableDesc = REPLACE(REPLACE(_file._file-label,","," "),CHR(10)," ")
                .
            FOR EACH _index NO-LOCK OF _file:
                FOR EACH _index-field OF _index NO-LOCK,
                    EACH b_field OF _index-field:
                    IF NOT CAN-DO(ttFullTableList.cAllIndexFields,b_field._field-name) THEN ASSIGN 
                            ttFullTableList.cAllIndexFields = ttFullTableList.cAllIndexFields + b_field._field-name + ",".
                    IF b_field._Data-Type EQ "date" THEN ASSIGN 
                        ttFullTableList.lHasDate = TRUE 
                        ttFullTableList.cDateField = b_field._field-name.
                END.
                IF _index._unique THEN 
                DO:
                    FOR EACH _index-field OF _index NO-LOCK,
                        EACH b_field OF _index-field:
                        IF NOT CAN-DO(ttFullTableList.cUIndexFields,b_field._field-name) THEN ASSIGN 
                                ttFullTableList.cUIndexFields = ttFullTableList.cUIndexFields + b_field._field-name + ",".
                        IF b_field._Data-Type EQ "date" THEN ASSIGN 
                            ttFullTableList.lHasDate = TRUE 
                            ttFullTableList.cDateField = b_field._field-name.
                    END.
                END.
            END.
            ASSIGN 
                ttFullTableList.cAllIndexFields = TRIM(ttFullTableList.cAllIndexFields,",") 
                ttFullTableList.cUIndexFields = TRIM(ttFullTableList.cUIndexFields,","). 
                
/*          This counts the records in the file to be converted; optional choice in main block */
            IF lCount THEN DO:
                RUN pCreateQuery(_file._file-name, OUTPUT hQuery).
                IF VALID-HANDLE(hQuery) THEN
                    ttFullTableList.iRecordCount = hQuery:NUM-RESULTS.
            END.

        END.
        RUN pBuildSingleFieldList ("Account",_file._file-name,_field._field-name).
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateTableConvert C-Win 
PROCEDURE pUpdateTableConvert :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    
    IF NOT CAN-FIND(FIRST ttFullTableList WHERE 
        ttFullTableList.cTable EQ ipcTableName AND 
        ttFullTableList.lConvert EQ TRUE) THEN 
    DO:
        FIND ttFullTableList WHERE 
            ttFullTableList.cTable EQ ipcTableName
            NO-ERROR.
        IF AVAIL ttFullTableList THEN ASSIGN 
                ttFullTableList.lConvert = TRUE.
    END. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

