&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
  File: pt/wQueryBuilder.w
  Description: Generic Program to allow users to build/save/open reports for display in Excel
  Input Parameters: <none>
  Output Parameters: <none>
  History: First draft - MYT - 9/22/16
  Notes:
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
&SCOPED-DEFINE FRAME-NAME f-main
&SCOPED-DEFINE IN IN FRAME {&FRAME-NAME}
&SCOPED-DEFINE SV SCREEN-VALUE {&IN}

DEF VAR cDataType AS CHAR EXTENT 20 NO-UNDO.
DEF VAR cColList AS CHAR NO-UNDO.
DEF VAR cInputQuery AS CHAR NO-UNDO.
DEF VAR cFileList AS CHAR NO-UNDO.
DEF VAR cFieldList AS CHAR EXTENT 4 NO-UNDO.
DEF VAR cExcludeList AS CHAR EXTENT 4 NO-UNDO.
DEF VAR lLoading AS LOG NO-UNDO.
DEF VAR h_frame AS HANDLE NO-UNDO.
DEF VAR h_field AS HANDLE NO-UNDO.
DEF VAR h_continue AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE EXTENT 4 NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hBrowse AS HANDLE NO-UNDO.
DEF VAR hColumn AS HANDLE EXTENT 300 NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.
DEF VAR cStatus AS CHAR NO-UNDO.
DEF VAR lProcessed AS LOG NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.
DEF VAR hCalcCol AS HANDLE EXTENT 20 NO-UNDO.
DEF VAR cFldDataType AS CHAR NO-UNDO.
DEF VAR cFldFormat AS CHAR NO-UNDO.
DEF VAR cFldInitVal AS CHAR NO-UNDO.
DEF VAR cFldColLabel AS CHAR NO-UNDO. 
DEF VAR iFldPosition AS INT NO-UNDO.
DEF VAR cFldFormula AS CHAR NO-UNDO.
DEF VAR cFullFieldList AS CHAR NO-UNDO.
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR lQueryOK AS LOG NO-UNDO.
DEF VAR cDataString AS CHAR NO-UNDO.
DEF VAR cTempString AS CHAR NO-UNDO.

DEF TEMP-TABLE ttColumns
    FIELD cColLabel AS CHAR
    FIELD cColType AS CHAR
    FIELD cColumn AS CHAR
    FIELD cDataType AS CHAR
    FIELD cDispLabel AS CHAR
    FIELD cField AS CHAR
    FIELD cFormat AS CHAR
    FIELD cFormula AS CHAR
    FIELD cInitVal AS CHAR
    FIELD cLabel AS CHAR
    FIELD cTable AS CHAR
    FIELD iBand AS INT
    FIELD iColumn AS INT
    FIELD iExtent AS INT
    FIELD iIdx AS INT
    FIELD iPos AS INT
    FIELD iRow AS INT
    INDEX idxPos IS PRIMARY iPos.

DEF TEMP-TABLE ttCalcCols
    FIELD cColLabel AS CHAR
    FIELD cDataType AS CHAR
    FIELD cFormat AS CHAR
    FIELD cFormula AS CHAR
    FIELD cInitVal AS CHAR
    FIELD iPos AS INT
    FIELD iIdx AS INT.

DEF BUFFER bttColumns FOR ttColumns.
DEF BUFFER bttCalcCols FOR ttCalcCols.

ASSIGN
    cColList = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,"
    cColList = cColList + "AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,"
    cColList = cColList + "BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,"
    cColList = cColList + "CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ,"
    cColList = cColList + "DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM,DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ,"
    cColList = cColList + "EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM,EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ,"
    cColList = cColList + "FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM,FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ,"
    cColList = cColList + "GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM,GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bExpand cbFile-1 fiRptName tOpenParen-1 ~
cbWhere-1 cbOp-1 fiVal-1 tCloseParen-1 cbAnd-1 bCheckSyntax tOpenParen-2 ~
cbWhere-2 cbOp-2 fiVal-2 tCloseParen-2 cbAnd-2 eQuery tOpenParen-3 ~
cbWhere-3 cbOp-3 fiVal-3 tCloseParen-3 cbAnd-3 tOpenParen-4 cbWhere-4 ~
cbOp-4 fiVal-4 tCloseParen-4 cbAnd-4 tOpenParen-5 cbWhere-5 cbOp-5 fiVal-5 ~
tCloseParen-5 tMore-1 cbFile-2 cbOf-2 cbRel-2 tOpenParen-6 cbWhere-6 cbOp-6 ~
fiVal-6 tCloseParen-6 cbAnd-6 slDisplayFields fiLockedCols tOpenParen-7 ~
cbWhere-7 cbOp-7 fiVal-7 tCloseParen-7 cbAnd-7 tOpenParen-8 cbWhere-8 ~
cbOp-8 fiVal-8 tCloseParen-8 cbAnd-8 fiPagesWide tOpenParen-9 cbWhere-9 ~
cbOp-9 fiVal-9 tCloseParen-9 cbAnd-9 bMoveUp tOpenParen-10 cbWhere-10 ~
cbOp-10 fiVal-10 tCloseParen-10 bMoveDown tMore-2 cbFile-3 cbOf-3 cbRel-3 ~
bRemove tOpenParen-11 cbWhere-11 cbOp-11 fiVal-11 tCloseParen-11 cbAnd-11 ~
bCalcField tOpenParen-12 cbWhere-12 cbOp-12 fiVal-12 tCloseParen-12 ~
cbAnd-12 bEditField tOpenParen-13 cbWhere-13 cbOp-13 fiVal-13 ~
tCloseParen-13 cbAnd-13 bLineBreak tOpenParen-14 cbWhere-14 cbOp-14 ~
fiVal-14 tCloseParen-14 cbAnd-14 tOpenParen-15 cbWhere-15 cbOp-15 fiVal-15 ~
tCloseParen-15 bProcess bExport tMore-3 cbFile-4 cbOf-4 cbRel-4 ~
tOpenParen-16 cbWhere-16 cbOp-16 fiVal-16 tCloseParen-16 cbAnd-16 ~
tOpenParen-17 cbWhere-17 cbOp-17 fiVal-17 tCloseParen-17 cbAnd-17 ~
tOpenParen-18 cbWhere-18 cbOp-18 fiVal-18 tCloseParen-18 cbAnd-18 ~
tOpenParen-19 cbWhere-19 cbOp-19 fiVal-19 tCloseParen-19 cbAnd-19 ~
tOpenParen-20 cbWhere-20 cbOp-20 fiVal-20 tCloseParen-20 
&Scoped-Define DISPLAYED-OBJECTS cbFile-1 fiRptName tOpenParen-1 cbWhere-1 ~
cbOp-1 fiVal-1 tCloseParen-1 cbAnd-1 tOpenParen-2 cbWhere-2 cbOp-2 fiVal-2 ~
tCloseParen-2 cbAnd-2 eQuery tOpenParen-3 cbWhere-3 cbOp-3 fiVal-3 ~
tCloseParen-3 cbAnd-3 tOpenParen-4 cbWhere-4 cbOp-4 fiVal-4 tCloseParen-4 ~
cbAnd-4 tOpenParen-5 cbWhere-5 cbOp-5 fiVal-5 tCloseParen-5 tMore-1 ~
cbFile-2 cbOf-2 cbRel-2 fiWhere-2 tOpenParen-6 cbWhere-6 cbOp-6 fiVal-6 ~
tCloseParen-6 cbAnd-6 slDisplayFields fiLockedCols tOpenParen-7 cbWhere-7 ~
cbOp-7 fiVal-7 tCloseParen-7 cbAnd-7 tOpenParen-8 cbWhere-8 cbOp-8 fiVal-8 ~
tCloseParen-8 cbAnd-8 fiPagesWide tOpenParen-9 cbWhere-9 cbOp-9 fiVal-9 ~
tCloseParen-9 cbAnd-9 tOpenParen-10 cbWhere-10 cbOp-10 fiVal-10 ~
tCloseParen-10 tMore-2 cbFile-3 cbOf-3 cbRel-3 fiWhere-3 tOpenParen-11 ~
cbWhere-11 cbOp-11 fiVal-11 tCloseParen-11 cbAnd-11 tOpenParen-12 ~
cbWhere-12 cbOp-12 fiVal-12 tCloseParen-12 cbAnd-12 tOpenParen-13 ~
cbWhere-13 cbOp-13 fiVal-13 tCloseParen-13 cbAnd-13 tOpenParen-14 ~
cbWhere-14 cbOp-14 fiVal-14 tCloseParen-14 cbAnd-14 tOpenParen-15 ~
cbWhere-15 cbOp-15 fiVal-15 tCloseParen-15 tMore-3 cbFile-4 cbOf-4 cbRel-4 ~
fiWhere-4 tOpenParen-16 cbWhere-16 cbOp-16 fiVal-16 tCloseParen-16 cbAnd-16 ~
tOpenParen-17 cbWhere-17 cbOp-17 fiVal-17 tCloseParen-17 cbAnd-17 ~
tOpenParen-18 cbWhere-18 cbOp-18 fiVal-18 tCloseParen-18 cbAnd-18 ~
tOpenParen-19 cbWhere-19 cbOp-19 fiVal-19 tCloseParen-19 cbAnd-19 ~
tOpenParen-20 cbWhere-20 cbOp-20 fiVal-20 tCloseParen-20 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_New          LABEL "New"           
       MENU-ITEM m_Open         LABEL "Open..."       
       MENU-ITEM m_Save         LABEL "Save"          
       MENU-ITEM m_Save_As      LABEL "Save As..."    
       RULE
       MENU-ITEM m_Quit         LABEL "Quit"          .

DEFINE SUB-MENU m_Edit 
       MENU-ITEM m_Check_Syntax LABEL "Check Syntax"  
       RULE
       MENU-ITEM m_Process      LABEL "Process"       
       MENU-ITEM m_Export_to_Excel LABEL "Export to Excel".

DEFINE MENU MENU-BAR-wWin MENUBAR
       SUB-MENU  m_File         LABEL "File"          
       SUB-MENU  m_Edit         LABEL "Edit"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bCalcField 
     LABEL "Add Calc Field" 
     SIZE 17 BY 1.13.

DEFINE BUTTON bCheckSyntax 
     LABEL "Check syntax" 
     SIZE 15 BY 1.

DEFINE BUTTON bEditField 
     LABEL "Edit Field" 
     SIZE 17 BY 1.13.

DEFINE BUTTON bExpand 
     IMAGE-UP FILE "images/expand.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 4.14 BY 1.21 TOOLTIP "Expand/Contract Result List".

DEFINE BUTTON bExport 
     LABEL "Export" 
     SIZE 15 BY 1.

DEFINE BUTTON bLineBreak 
     LABEL "Insert Line Break" 
     SIZE 17 BY 1.13.

DEFINE BUTTON bMoveDown 
     LABEL "Move Down" 
     SIZE 17 BY 1.

DEFINE BUTTON bMoveUp 
     LABEL "Move Up" 
     SIZE 17 BY 1.

DEFINE BUTTON bProcess 
     LABEL "Process" 
     SIZE 15 BY 1.

DEFINE BUTTON bRemove 
     LABEL "Remove Field" 
     SIZE 17 BY 1.13.

DEFINE VARIABLE cbAnd-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbAnd-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbFile-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "FOR EACH" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbFile-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbFile-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbFile-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cbOf-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "OF","WHERE" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOf-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "OF","WHERE" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOf-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "OF","WHERE" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbOp-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "=","<>",">",">=","<","<=","BEGINS","MATCHES" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cbRel-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 1
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbRel-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbRel-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cbWhere-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 32
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE eDummy AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 59 BY 7.5 NO-UNDO.

DEFINE VARIABLE eQuery AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59 BY 6.25 NO-UNDO.

DEFINE VARIABLE fiLockedCols AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Locked Cols" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fiPagesWide AS INTEGER FORMAT ">9":U INITIAL 99 
     LABEL "Pages Wide" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fiRptName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report Name" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-16 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-17 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-18 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-19 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-20 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiVal-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhere-2 AS CHARACTER FORMAT "X(256)":U INITIAL "WHERE" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhere-3 AS CHARACTER FORMAT "X(256)":U INITIAL "WHERE" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE fiWhere-4 AS CHARACTER FORMAT "X(256)":U INITIAL "WHERE" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE slDisplayFields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 42 BY 11.75 NO-UNDO.

DEFINE VARIABLE tCloseParen-1 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-10 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-11 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-12 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-13 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-14 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-15 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-16 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-17 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-18 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-19 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-2 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-20 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-3 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-4 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-5 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-6 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-7 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-8 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tCloseParen-9 AS LOGICAL INITIAL no 
     LABEL ")" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tMore-1 AS LOGICAL INITIAL no 
     LABEL "EACH" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE tMore-2 AS LOGICAL INITIAL no 
     LABEL "EACH" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE tMore-3 AS LOGICAL INITIAL no 
     LABEL "EACH" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-1 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-10 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-11 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-12 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-13 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-14 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-15 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-16 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-17 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-18 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-19 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-2 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-20 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-3 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-4 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-5 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-6 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-7 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-8 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tOpenParen-9 AS LOGICAL INITIAL no 
     LABEL "(" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-Main
     bExpand AT ROW 24.75 COL 96
     cbFile-1 AT ROW 1.5 COL 15 COLON-ALIGNED
     fiRptName AT ROW 1.5 COL 70 COLON-ALIGNED
     tOpenParen-1 AT ROW 2.75 COL 17
     cbWhere-1 AT ROW 2.75 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-1 AT ROW 2.75 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-1 AT ROW 2.75 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-1 AT ROW 2.75 COL 78
     cbAnd-1 AT ROW 2.75 COL 81 COLON-ALIGNED NO-LABEL
     bCheckSyntax AT ROW 2.75 COL 145
     tOpenParen-2 AT ROW 4 COL 17
     cbWhere-2 AT ROW 4 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-2 AT ROW 4 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-2 AT ROW 4 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-2 AT ROW 4 COL 78
     cbAnd-2 AT ROW 4 COL 81 COLON-ALIGNED NO-LABEL
     eQuery AT ROW 4 COL 101 NO-LABEL
     tOpenParen-3 AT ROW 5.25 COL 17
     cbWhere-3 AT ROW 5.25 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-3 AT ROW 5.25 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-3 AT ROW 5.25 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-3 AT ROW 5.25 COL 78
     cbAnd-3 AT ROW 5.25 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-4 AT ROW 6.5 COL 17
     cbWhere-4 AT ROW 6.5 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-4 AT ROW 6.5 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-4 AT ROW 6.5 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-4 AT ROW 6.5 COL 78
     cbAnd-4 AT ROW 6.5 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-5 AT ROW 7.75 COL 17
     cbWhere-5 AT ROW 7.75 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-5 AT ROW 7.75 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-5 AT ROW 7.75 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-5 AT ROW 7.75 COL 78
     tMore-1 AT ROW 9.25 COL 8
     cbFile-2 AT ROW 9.25 COL 15 COLON-ALIGNED NO-LABEL
     cbOf-2 AT ROW 9.25 COL 41 COLON-ALIGNED NO-LABEL
     cbRel-2 AT ROW 9.25 COL 53 COLON-ALIGNED NO-LABEL
     fiWhere-2 AT ROW 9.25 COL 79 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     tOpenParen-6 AT ROW 10.5 COL 17
     cbWhere-6 AT ROW 10.5 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-6 AT ROW 10.5 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-6 AT ROW 10.5 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-6 AT ROW 10.5 COL 78
     cbAnd-6 AT ROW 10.5 COL 81 COLON-ALIGNED NO-LABEL
     slDisplayFields AT ROW 11.5 COL 101 NO-LABEL
     fiLockedCols AT ROW 11.5 COL 153 COLON-ALIGNED
     tOpenParen-7 AT ROW 11.75 COL 17
     cbWhere-7 AT ROW 11.75 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-7 AT ROW 11.75 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-7 AT ROW 11.75 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-7 AT ROW 11.75 COL 78
     cbAnd-7 AT ROW 11.75 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-8 AT ROW 13 COL 17
     cbWhere-8 AT ROW 13 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-8 AT ROW 13 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-8 AT ROW 13 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-8 AT ROW 13 COL 78
     cbAnd-8 AT ROW 13 COL 81 COLON-ALIGNED NO-LABEL
     fiPagesWide AT ROW 13 COL 153 COLON-ALIGNED
     tOpenParen-9 AT ROW 14.25 COL 17
     cbWhere-9 AT ROW 14.25 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-9 AT ROW 14.25 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-9 AT ROW 14.25 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-9 AT ROW 14.25 COL 78
     cbAnd-9 AT ROW 14.25 COL 81 COLON-ALIGNED NO-LABEL
     bMoveUp AT ROW 14.5 COL 144
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164 BY 32.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-Main
     tOpenParen-10 AT ROW 15.5 COL 17
     cbWhere-10 AT ROW 15.5 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-10 AT ROW 15.5 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-10 AT ROW 15.5 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-10 AT ROW 15.5 COL 78
     bMoveDown AT ROW 15.75 COL 144
     tMore-2 AT ROW 17 COL 8
     cbFile-3 AT ROW 17 COL 15 COLON-ALIGNED NO-LABEL
     cbOf-3 AT ROW 17 COL 41 COLON-ALIGNED NO-LABEL
     cbRel-3 AT ROW 17 COL 53 COLON-ALIGNED NO-LABEL
     fiWhere-3 AT ROW 17 COL 79 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     bRemove AT ROW 17 COL 144
     tOpenParen-11 AT ROW 18.25 COL 17
     cbWhere-11 AT ROW 18.25 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-11 AT ROW 18.25 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-11 AT ROW 18.25 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-11 AT ROW 18.25 COL 78
     cbAnd-11 AT ROW 18.25 COL 81 COLON-ALIGNED NO-LABEL
     bCalcField AT ROW 18.5 COL 144
     tOpenParen-12 AT ROW 19.5 COL 17
     cbWhere-12 AT ROW 19.5 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-12 AT ROW 19.5 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-12 AT ROW 19.5 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-12 AT ROW 19.5 COL 78
     cbAnd-12 AT ROW 19.5 COL 81 COLON-ALIGNED NO-LABEL
     bEditField AT ROW 20 COL 144
     tOpenParen-13 AT ROW 20.75 COL 17
     cbWhere-13 AT ROW 20.75 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-13 AT ROW 20.75 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-13 AT ROW 20.75 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-13 AT ROW 20.75 COL 78
     cbAnd-13 AT ROW 20.75 COL 81 COLON-ALIGNED NO-LABEL
     bLineBreak AT ROW 21.5 COL 144
     tOpenParen-14 AT ROW 22 COL 17
     cbWhere-14 AT ROW 22 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-14 AT ROW 22 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-14 AT ROW 22 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-14 AT ROW 22 COL 78
     cbAnd-14 AT ROW 22 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-15 AT ROW 23.25 COL 17
     cbWhere-15 AT ROW 23.25 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-15 AT ROW 23.25 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-15 AT ROW 23.25 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-15 AT ROW 23.25 COL 78
     bProcess AT ROW 23.5 COL 124
     bExport AT ROW 23.5 COL 145
     tMore-3 AT ROW 24.75 COL 8
     cbFile-4 AT ROW 24.75 COL 15 COLON-ALIGNED NO-LABEL
     cbOf-4 AT ROW 24.75 COL 41 COLON-ALIGNED NO-LABEL
     cbRel-4 AT ROW 24.75 COL 53 COLON-ALIGNED NO-LABEL
     fiWhere-4 AT ROW 24.75 COL 79 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     eDummy AT ROW 24.75 COL 101 NO-LABEL NO-TAB-STOP 
     tOpenParen-16 AT ROW 26 COL 17
     cbWhere-16 AT ROW 26 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-16 AT ROW 26 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-16 AT ROW 26 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-16 AT ROW 26 COL 78
     cbAnd-16 AT ROW 26 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-17 AT ROW 27.25 COL 17
     cbWhere-17 AT ROW 27.25 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-17 AT ROW 27.25 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-17 AT ROW 27.25 COL 53 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164 BY 32.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-Main
     tCloseParen-17 AT ROW 27.25 COL 78
     cbAnd-17 AT ROW 27.25 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-18 AT ROW 28.5 COL 17
     cbWhere-18 AT ROW 28.5 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-18 AT ROW 28.5 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-18 AT ROW 28.5 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-18 AT ROW 28.5 COL 78
     cbAnd-18 AT ROW 28.5 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-19 AT ROW 29.75 COL 17
     cbWhere-19 AT ROW 29.75 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-19 AT ROW 29.75 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-19 AT ROW 29.75 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-19 AT ROW 29.75 COL 78
     cbAnd-19 AT ROW 29.75 COL 81 COLON-ALIGNED NO-LABEL
     tOpenParen-20 AT ROW 31 COL 17
     cbWhere-20 AT ROW 31 COL 19 COLON-ALIGNED NO-LABEL
     cbOp-20 AT ROW 31 COL 41 COLON-ALIGNED NO-LABEL
     fiVal-20 AT ROW 31 COL 53 COLON-ALIGNED NO-LABEL
     tCloseParen-20 AT ROW 31 COL 78
     "QUERY STRING:" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 3 COL 100
     "WHERE" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 1.5 COL 43
     "RESULT LIST" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 23.5 COL 100
     "DISPLAY FIELDS" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 10.5 COL 100
     "(Calculated field values are only displayed after Export.)" VIEW-AS TEXT
          SIZE 48 BY .67 AT ROW 32.25 COL 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164 BY 32.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Decade Report Exporter"
         HEIGHT             = 32
         WIDTH              = 164
         MAX-HEIGHT         = 44.04
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 44.04
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-wWin:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-Main
                                                                        */
/* SETTINGS FOR EDITOR eDummy IN FRAME f-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       eDummy:HIDDEN IN FRAME f-Main           = TRUE.

/* SETTINGS FOR FILL-IN fiWhere-2 IN FRAME f-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiWhere-3 IN FRAME f-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiWhere-4 IN FRAME f-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Decade Report Exporter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Decade Report Exporter */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    IF VALID-HANDLE(chExcelApplication) THEN DO:
        chExcelApplication:QUIT().
        RELEASE OBJECT chWorkSheet        NO-ERROR.
        RELEASE OBJECT chWorkBook         NO-ERROR.
        RELEASE OBJECT chExcelApplication NO-ERROR.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExpand
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExpand wWin
ON CHOOSE OF bExpand IN FRAME f-Main /* Button 1 */
DO:
    IF VALID-HANDLE(hBrowse) THEN DO:
        IF SELF:ROW = 24.75 THEN DO:
            ASSIGN
                SELF:ROW = 2.5
                SELF:COLUMN = 3.
            SELF:LOAD-IMAGE-UP("img/contract.jpg").
            ASSIGN
                hBrowse:COL = 8
                hBrowse:ROW = 2.5
                hBrowse:WIDTH = 153
                hBrowse:HEIGHT = 28.75
                bExport:ROW = 31.75.  
            hBrowse:REFRESH().
        END.
        ELSE DO:
            ASSIGN
                SELF:ROW = 24.75
                SELF:COLUMN = 96.
            SELF:LOAD-IMAGE-UP("img/expand.jpg").
            ASSIGN
                hBrowse:COL = 101
                hBrowse:ROW = 24.75
                hBrowse:WIDTH = 59
                hBrowse:HEIGHT = 7.5
                bExport:ROW = 23.5.    
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExport wWin
ON CHOOSE OF bExport IN FRAME f-Main /* Export */
DO:
    IF NOT lProcessed THEN APPLY 'choose' TO bProcess {&IN}.
    RUN ipExcel IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bMoveUp wWin
ON CHOOSE OF bMoveUp IN FRAME f-Main /* Move Up */
OR CHOOSE OF bMoveDown
OR CHOOSE OF bRemove
OR CHOOSE OF bCalcField
OR CHOOSE OF bEditField
OR CHOOSE OF bLineBreak
DO:

    DEF VAR cTempFrom AS CHAR NO-UNDO.
    DEF VAR cTempTo AS CHAR NO-UNDO.
    DEF VAR iRow AS INT NO-UNDO.
    DEF VAR iCol AS INT NO-UNDO.
    DEF VAR iHold AS INT NO-UNDO.
    DEF VAR cTempFile AS CHAR NO-UNDO.
    DEF VAR cTempField AS CHAR NO-UNDO.
    ASSIGN lProcessed = FALSE.

    CASE SELF:NAME:
        WHEN "bMoveUp" THEN DO:
            ASSIGN
                iRow = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS)
                iHold = iRow - 1.
            FIND FIRST bttColumns WHERE
                bttColumns.iPos = iRow - 1
                NO-LOCK NO-ERROR.
            FIND FIRST ttColumns WHERE
                ttColumns.iPos = iRow 
                NO-LOCK NO-ERROR.
            ASSIGN
                bttColumns.iPos = iRow
                ttColumns.iPos = iRow - 1.

            ASSIGN
                cTempFrom = slDisplayFields:{&SV}
                cTempTo = ENTRY(iRow - 1,slDisplayFields:LIST-ITEMS).
            slDisplayFields:REPLACE("x",cTempFrom).
            slDisplayFields:REPLACE(cTempFrom,cTempTo).
            slDisplayFields:REPLACE(cTempTo,"x").
            slDisplayFields:{&SV} = cTempFrom.
        END.
        WHEN "bMoveDown" THEN DO:
            ASSIGN
                iRow = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS)
                iHold = iRow + 1.
            
            FIND FIRST bttColumns WHERE
                bttColumns.iPos = iRow + 1
                NO-LOCK NO-ERROR.
            FIND FIRST ttColumns WHERE
                ttColumns.iPos = iRow 
                NO-LOCK NO-ERROR.
            ASSIGN
                bttColumns.iPos = iRow
                ttColumns.iPos = iRow + 1.
            
            ASSIGN
                cTempFrom = slDisplayFields:{&SV}
                cTempTo = ENTRY(iRow + 1,slDisplayFields:LIST-ITEMS).
            slDisplayFields:REPLACE("x",cTempFrom).
            slDisplayFields:REPLACE(cTempFrom,cTempTo).
            slDisplayFields:REPLACE(cTempTo,"x").
            slDisplayFields:{&SV} = cTempFrom.
        END.
        WHEN "bRemove" THEN DO:
            FIND FIRST ttColumns WHERE
                ttColumns.iPos = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS) 
                NO-LOCK NO-ERROR.
            IF NOT AVAIL ttColumns THEN DO:
                MESSAGE
                    "You must select a row in the Display List to remove."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            ELSE DO:
                FIND ttCalcCols WHERE 
                    ttCalcCols.iPos = ttColumns.iPos
                    NO-ERROR.
                IF AVAIL ttCalcCols THEN
                    DELETE ttCalcCols.
                FOR EACH bttColumns WHERE
                    bttColumns.iPos > ttColumns.iPos:
                    ASSIGN
                        bttColumns.iPos = bttColumns.iPos - 1.
                END.
                FOR EACH ttCalcCols WHERE
                    ttCalcCols.iPos > ttColumns.iPos:
                    ASSIGN
                        ttCalcCols.iPos = ttCalcCols.iPos - 1.
                END.
                ASSIGN iHold = ttColumns.iRow.
                DELETE ttColumns.
            END.
            slDisplayFields:DELETE(slDisplayFields:{&SV}).
        END.
        WHEN "bCalcField" THEN DO:
            IF NOT lProcessed THEN
                APPLY 'choose' TO bProcess.
            ASSIGN
                iCtr = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS).
            IF iCtr = ? OR iCtr = 0 THEN ASSIGN
                iCtr = 1.

            RUN util/dCalcField.w (INPUT cFullFieldList,
                                 INPUT-OUTPUT cFldDataType,
                                 INPUT-OUTPUT cFldFormat,
                                 INPUT-OUTPUT cFldInitVal,
                                 INPUT-OUTPUT cFldColLabel, 
                                 INPUT-OUTPUT cFldFormula).
            IF cFldColLabel = "" THEN DO:
                MESSAGE 
                    "Add field cancelled."
                    VIEW-AS ALERT-BOX.
                RETURN.
            END.

            FIND LAST ttColumns 
                NO-LOCK NO-ERROR.
            ASSIGN 
                iHold = IF AVAIL ttColumns THEN ttColumns.iPos + 1 ELSE 1.
            CREATE ttCalcCols.
            ASSIGN
                ttCalcCols.iIdx = iHold.
            ASSIGN
                ttCalcCols.iPos = iCtr
                ttCalcCols.cDataType = cFldDataType
                ttCalcCols.cFormat = cFldFormat
                ttCalcCols.cInitVal = cFldInitVal
                ttCalcCols.cColLabel = cFldColLabel
                ttCalcCols.cFormula = cFldFormula.
            CREATE ttColumns.
            ASSIGN
                ttColumns.cColLabel = ttCalcCols.cColLabel
                ttColumns.cColType = "CALC"
                ttColumns.cColumn = ""
                ttColumns.cDataType = ttCalcCols.cDataType
                ttColumns.cDispLabel = ttCalcCols.cColLabel + " (CALC)"
                ttColumns.cField = ""
                ttColumns.cFormat = ttCalcCols.cFormat
                ttColumns.cFormula = ttCalcCols.cFormula
                ttColumns.cInitVal = ttCalcCols.cInitVal
                ttColumns.cLabel = ttCalcCols.cColLabel
                ttColumns.cTable = ""
                ttColumns.iBand = 1    
                ttColumns.iColumn = 0    
                ttColumns.iExtent = 0
                ttColumns.iIdx = iHold
                ttColumns.iPos = ttCalcCols.iPos
                ttColumns.iRow = 1
                .
            FOR EACH bttColumns WHERE
                bttColumns.iPos >= ttColumns.iPos AND
                bttColumns.iIdx <> iHold
                BY bttColumns.iPos DESCENDING:
                ASSIGN
                    bttColumns.iPos = bttColumns.iPos + 1.
            END.
            ASSIGN
                ttColumns.iPos = iHold.
            FOR EACH bttCalcCols WHERE
                bttCalcCols.iPos >= ttCalcCols.iPos AND
                bttCalcCols.iIdx <> iHold
                BY ttCalcCols.iPos DESCENDING:
                ASSIGN
                    bttCalcCols.iPos = bttCalcCols.iPos + 1.
            END.
            ASSIGN
                ttCalcCols.iPos = iHold.
            slDisplayFields:INSERT(ttColumns.cDispLabel,ttColumns.iPos).
            iHold = ttColumns.iPos.
            FOR EACH ttColumns:
                ASSIGN
                    ttColumns.iIdx = ttColumns.iPos.
            END.
            FOR EACH ttCalcCols:
                ASSIGN
                    ttCalcCols.iIdx = ttCalcCols.iPos.
            END.
        END.
        WHEN "bEditField" THEN DO:
            IF NOT lProcessed THEN
                APPLY 'choose' TO bProcess.
            FIND LAST ttColumns WHERE
                ttColumns.iPos = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS) AND
                INDEX(slDisplayFields:{&SV},"(CALC)") <> 0
                EXCLUSIVE NO-ERROR.
            IF NOT AVAIL ttColumns THEN DO:
                MESSAGE
                    "You must select a Calculated Column to use this function."
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            RUN util/dCalcField.w (INPUT cFullFieldList,
                                 INPUT-OUTPUT ttColumns.cDataType,
                                 INPUT-OUTPUT ttColumns.cFormat,
                                 INPUT-OUTPUT ttColumns.cInitVal,
                                 INPUT-OUTPUT ttColumns.cColLabel, 
                                 INPUT-OUTPUT ttColumns.cFormula).
            FIND LAST ttCalcCols WHERE
                ttCalcCols.iPos = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS)
                EXCLUSIVE NO-ERROR.
            ASSIGN
                ttColumns.cDispLabel = ttColumns.cColLabel + " (CALC)"
                ttCalcCols.cColLabel = ttColumns.cColLabel
                ttCalcCols.cDataType = ttColumns.cDataType
                ttCalcCols.cFormat = ttColumns.cFormat
                ttCalcCols.cFormula = ttColumns.cFormula
                ttCalcCols.cInitVal = ttColumns.cInitVal
                ttCalcCols.iPos = ttColumns.iPos
                iHold = ttColumns.iPos.
        END.
        WHEN "bLineBreak" THEN DO:
            IF slDisplayFields:{&SV} = ""
            OR slDisplayFields:{&SV} = ? THEN DO:
                MESSAGE
                    "You must a select a displayed field prior to inserting a line break."
                    VIEW-AS ALERT-BOX WARNING.
                RETURN NO-APPLY.
            END.
            FIND LAST ttColumns 
                NO-LOCK NO-ERROR.
            ASSIGN 
                iHold = IF AVAIL ttColumns THEN ttColumns.iPos + 1 ELSE 1.
            CREATE ttColumns.
            ASSIGN
                ttColumns.iIdx = iHold
                ttColumns.iPos = LOOKUP(slDisplayFields:{&SV},slDisplayFields:LIST-ITEMS)
                ttColumns.cDataType = "char"
                ttColumns.cDispLabel = "<skip>"
                ttColumns.cFormat = "x(2)"
                ttColumns.cInitVal = ""
                ttColumns.cColLabel = "<skip>"
                ttColumns.cFormula = "".
            CREATE ttCalcCols.
            ASSIGN
                ttCalcCols.iIdx = iHold
                ttCalcCols.iPos = ttColumns.iPos
                ttCalcCols.cColLabel = ttColumns.cColLabel
                ttCalcCols.cDataType = ttColumns.cDataType
                ttCalcCols.cFormat = ttColumns.cFormat
                ttCalcCols.cFormula = ttColumns.cFormula
                ttCalcCols.cInitVal = ttColumns.cInitVal.
            FOR EACH bttColumns WHERE
                bttColumns.iPos >= ttColumns.iPos AND
                bttColumns.iIdx <> iHold
                BY bttColumns.iPos DESCENDING:
                ASSIGN
                    bttColumns.iPos = bttColumns.iPos + 1.
            END.
            ASSIGN
                ttColumns.iPos = iHold.
            FOR EACH bttCalcCols WHERE
                bttCalcCols.iPos >= ttCalcCols.iPos AND
                bttCalcCols.iIdx <> iHold
                BY ttCalcCols.iPos DESCENDING:
                ASSIGN
                    bttCalcCols.iPos = bttCalcCols.iPos + 1.
            END.
            ASSIGN
                bttCalcCols.iPos = iHold.
            slDisplayFields:INSERT(ttColumns.cDispLabel,ttColumns.iPos).
            iHold = ttColumns.iPos.
            FOR EACH ttColumns:
                ASSIGN
                    ttColumns.iIdx = ttColumns.iPos.
            END.
            FOR EACH ttCalcCols:
                ASSIGN
                    ttCalcCols.iIdx = ttCalcCols.iPos.
            END.
        END.
    END CASE.

    ASSIGN
        iRow = 1
        iCol = 1
        slDisplayFields:LIST-ITEMS = ""
        cFullFieldList = "".
    FOR EACH ttColumns BY ttColumns.iPos:
        slDisplayFields:ADD-LAST(ttColumns.cDispLabel).
        ASSIGN
            ttColumns.iRow = iRow
            ttColumns.iCol = iCol
            ttColumns.cColumn = ENTRY(iCol,cColList)
            iCol = iCol + 1.
        IF ttColumns.cColType = "SKIP" 
        OR ttColumns.cColLabel = "<skip>" THEN ASSIGN
            iRow = iRow + 1
            iCol = 1.
        IF ttColumns.cColType = "DATA" THEN ASSIGN
            cFullFieldList = cFullFieldList + ttColumns.cTable + "." + ttColumns.cField + (IF ttColumns.iExtent <> 0 THEN "[" + STRING(ttColumns.iExtent) + "]" ELSE "") + ",".
    END.
    ASSIGN 
        slDisplayFields:{&SV} = ENTRY(iHold,slDisplayFields:LIST-ITEMS)
        cFullFieldList = TRIM(cFullFieldList,",").
    
    APPLY 'value-changed' TO slDisplayFields.
    IF lProcessed THEN APPLY 'choose' TO bProcess.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bProcess wWin
ON CHOOSE OF bProcess IN FRAME f-Main /* Process */
OR CHOOSE OF bCheckSyntax
DO:
    RUN ipDefineQuery IN THIS-PROCEDURE.
    IF NOT lQueryOK THEN RETURN.
    
    /* If only checking syntax, stop here */
    IF SELF:NAME = "bCheckSyntax" THEN DO:
        MESSAGE
            "Syntax is correct."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END.
    
    RUN ipProcessQuery IN THIS-PROCEDURE.
    ASSIGN
        bExpand:VISIBLE = TRUE.
        lProcessed = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd-1 wWin
ON VALUE-CHANGED OF cbAnd-1 IN FRAME f-Main
OR VALUE-CHANGED OF cbAnd-2
OR VALUE-CHANGED OF cbAnd-3
OR VALUE-CHANGED OF cbAnd-4
DO:
    ASSIGN lProcessed = FALSE.
    IF SELF:{&SV} = ? THEN ASSIGN
        SELF:{&SV} = "".

    CASE SELF:NAME:
        WHEN "cbAnd-1" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-2:VISIBLE THEN ASSIGN
                    tOpenParen-2:VISIBLE = FALSE
                    cbWhere-2:VISIBLE = FALSE
                    cbOp-2:VISIBLE = FALSE
                    fiVal-2:{&SV} = ""
                    fiVal-2:VISIBLE = FALSE
                    tCloseParen-2:VISIBLE = FALSE
                    cbAnd-2:VISIBLE = FALSE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-2:HIDDEN THEN ASSIGN
                    tOpenParen-2:VISIBLE = TRUE
                    cbWhere-2:VISIBLE = TRUE
                    cbOp-2:VISIBLE = TRUE
                    fiVal-2:VISIBLE = TRUE
                    tCloseParen-2:VISIBLE = TRUE
                    cbAnd-2:VISIBLE = TRUE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-2.
        END.
        WHEN "cbAnd-2" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = ? THEN DO:
                IF fiVal-3:VISIBLE THEN ASSIGN
                    tOpenParen-3:VISIBLE = FALSE
                    cbWhere-3:VISIBLE = FALSE
                    cbOp-3:VISIBLE = FALSE
                    fiVal-3:{&SV} = ""
                    fiVal-3:VISIBLE = FALSE
                    tCloseParen-3:VISIBLE = FALSE
                    cbAnd-3:VISIBLE = FALSE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-3:HIDDEN THEN ASSIGN
                    tOpenParen-3:VISIBLE = TRUE
                    cbWhere-3:VISIBLE = TRUE
                    cbOp-3:VISIBLE = TRUE
                    fiVal-3:VISIBLE = TRUE
                    tCloseParen-3:VISIBLE = TRUE
                    cbAnd-3:VISIBLE = TRUE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-3.
        END.
        WHEN "cbAnd-3" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = ? THEN DO:
                IF fiVal-4:VISIBLE THEN ASSIGN
                    tOpenParen-4:VISIBLE = FALSE
                    cbWhere-4:VISIBLE = FALSE
                    cbOp-4:VISIBLE = FALSE
                    fiVal-4:{&SV} = ""
                    fiVal-4:VISIBLE = FALSE
                    tCloseParen-4:VISIBLE = FALSE
                    cbAnd-4:VISIBLE = FALSE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-4:HIDDEN THEN ASSIGN
                    tOpenParen-4:VISIBLE = TRUE
                    cbWhere-4:VISIBLE = TRUE
                    cbOp-4:VISIBLE = TRUE
                    fiVal-4:VISIBLE = TRUE
                    tCloseParen-4:VISIBLE = TRUE
                    cbAnd-4:VISIBLE = TRUE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-4.
        END.
        WHEN "cbAnd-4" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = ? THEN DO:
                IF fiVal-5:VISIBLE THEN ASSIGN
                    tOpenParen-5:VISIBLE = FALSE
                    cbWhere-5:VISIBLE = FALSE
                    cbOp-5:VISIBLE = FALSE
                    fiVal-5:{&SV} = ""
                    fiVal-5:VISIBLE = FALSE
                    tCloseParen-5:VISIBLE = FALSE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-5:HIDDEN THEN ASSIGN
                    tOpenParen-5:VISIBLE = TRUE
                    cbWhere-5:VISIBLE = TRUE
                    cbOp-5:VISIBLE = TRUE
                    fiVal-5:VISIBLE = TRUE
                    tCloseParen-5:VISIBLE = TRUE
                    tMore-1:ROW = MINIMUM(tMore-1:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-5.
        END.
    END CASE.
    
    APPLY 'value-changed' TO tMore-1.
    APPLY 'value-changed' TO tMore-2.
    APPLY 'value-changed' TO tMore-3.
    
    RUN ipShowQuery IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd-11 wWin
ON VALUE-CHANGED OF cbAnd-11 IN FRAME f-Main
OR VALUE-CHANGED OF cbAnd-12
OR VALUE-CHANGED OF cbAnd-13
OR VALUE-CHANGED OF cbAnd-14
DO:
    ASSIGN lProcessed = FALSE.
    IF SELF:{&SV} = ? THEN ASSIGN
    SELF:{&SV} = "".
    
    CASE SELF:NAME:
        WHEN "cbAnd-11" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-12:VISIBLE THEN ASSIGN
                    tOpenParen-12:VISIBLE = FALSE
                    cbWhere-12:VISIBLE = FALSE
                    cbOp-12:VISIBLE = FALSE
                    fiVal-12:{&SV} = ""
                    fiVal-12:VISIBLE = FALSE
                    tCloseParen-12:VISIBLE = FALSE
                    cbAnd-12:VISIBLE = FALSE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-12:HIDDEN THEN ASSIGN
                    tOpenParen-12:VISIBLE = TRUE
                    cbWhere-12:VISIBLE = TRUE
                    cbOp-12:VISIBLE = TRUE
                    fiVal-12:VISIBLE = TRUE
                    tCloseParen-12:VISIBLE = TRUE
                    cbAnd-12:VISIBLE = TRUE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-12.
        END.
        WHEN "cbAnd-12" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-13:VISIBLE THEN ASSIGN
                    tOpenParen-13:VISIBLE = FALSE
                    cbWhere-13:VISIBLE = FALSE
                    cbOp-13:VISIBLE = FALSE
                    fiVal-13:{&SV} = ""
                    fiVal-13:VISIBLE = FALSE
                    tCloseParen-13:VISIBLE = FALSE
                    cbAnd-13:VISIBLE = FALSE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-13:HIDDEN THEN ASSIGN
                    tOpenParen-13:VISIBLE = TRUE
                    cbWhere-13:VISIBLE = TRUE
                    cbOp-13:VISIBLE = TRUE
                    fiVal-13:VISIBLE = TRUE
                    tCloseParen-13:VISIBLE = TRUE
                    cbAnd-13:VISIBLE = TRUE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-13.
        END.
        WHEN "cbAnd-13" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-14:VISIBLE THEN ASSIGN
                    tOpenParen-14:VISIBLE = FALSE
                    cbWhere-14:VISIBLE = FALSE
                    cbOp-14:VISIBLE = FALSE
                    fiVal-14:{&SV} = ""
                    fiVal-14:VISIBLE = FALSE
                    tCloseParen-14:VISIBLE = FALSE
                    cbAnd-14:VISIBLE = FALSE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-14:HIDDEN THEN ASSIGN
                    tOpenParen-14:VISIBLE = TRUE
                    cbWhere-14:VISIBLE = TRUE
                    cbOp-14:VISIBLE = TRUE
                    fiVal-14:VISIBLE = TRUE
                    tCloseParen-14:VISIBLE = TRUE
                    cbAnd-14:VISIBLE = TRUE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-14.
        END.
        WHEN "cbAnd-14" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-15:VISIBLE THEN ASSIGN
                    tOpenParen-15:VISIBLE = FALSE
                    cbWhere-15:VISIBLE = FALSE
                    cbOp-15:VISIBLE = FALSE
                    fiVal-15:{&SV} = ""
                    fiVal-15:VISIBLE = FALSE
                    tCloseParen-15:VISIBLE = FALSE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-15:HIDDEN THEN ASSIGN
                    tOpenParen-15:VISIBLE = TRUE
                    cbWhere-15:VISIBLE = TRUE
                    cbOp-15:VISIBLE = TRUE
                    fiVal-15:VISIBLE = TRUE
                    tCloseParen-15:VISIBLE = TRUE
                    tMore-3:ROW = MINIMUM(tMore-3:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-15.
        END.
    END CASE.
    
    APPLY 'value-changed' TO tMore-3.
    
    RUN ipShowQuery IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd-16 wWin
ON VALUE-CHANGED OF cbAnd-16 IN FRAME f-Main
OR VALUE-CHANGED OF cbAnd-17
OR VALUE-CHANGED OF cbAnd-18
OR VALUE-CHANGED OF cbAnd-19
DO:
    ASSIGN lProcessed = FALSE.
    IF SELF:{&SV} = ? THEN ASSIGN
        SELF:{&SV} = "".
    
    CASE SELF:NAME:
        WHEN "cbAnd-16" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-17:VISIBLE THEN ASSIGN
                    tOpenParen-17:VISIBLE = FALSE
                    cbWhere-17:VISIBLE = FALSE
                    cbOp-17:VISIBLE = FALSE
                    fiVal-17:{&SV} = ""
                    fiVal-17:VISIBLE = FALSE
                    tCloseParen-17:VISIBLE = FALSE
                    cbAnd-17:VISIBLE = FALSE.
            END.
            ELSE DO:
                IF fiVal-17:HIDDEN THEN ASSIGN
                    tOpenParen-17:VISIBLE = TRUE
                    cbWhere-17:VISIBLE = TRUE
                    cbOp-17:VISIBLE = TRUE
                    fiVal-17:VISIBLE = TRUE
                    tCloseParen-17:VISIBLE = TRUE
                    cbAnd-17:VISIBLE = TRUE.
            END.
            APPLY 'value-changed' TO cbWhere-17.
        END.
        WHEN "cbAnd-17" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-18:VISIBLE THEN ASSIGN
                    tOpenParen-18:VISIBLE = FALSE
                    cbWhere-18:VISIBLE = FALSE
                    cbOp-18:VISIBLE = FALSE
                    fiVal-18:{&SV} = ""
                    fiVal-18:VISIBLE = FALSE
                    tCloseParen-18:VISIBLE = FALSE
.                    cbAnd-18:VISIBLE = FALSE.
            END.
            ELSE DO:
                IF fiVal-18:HIDDEN THEN ASSIGN
                    tOpenParen-18:VISIBLE = TRUE
                    cbWhere-18:VISIBLE = TRUE
                    cbOp-18:VISIBLE = TRUE
                    fiVal-18:VISIBLE = TRUE
                    tCloseParen-18:VISIBLE = TRUE
                    cbAnd-18:VISIBLE = TRUE.
            END.
            APPLY 'value-changed' TO cbWhere-18.
        END.
        WHEN "cbAnd-18" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-19:VISIBLE THEN ASSIGN
                    tOpenParen-19:VISIBLE = FALSE
                    cbWhere-19:VISIBLE = FALSE
                    cbOp-19:VISIBLE = FALSE
                    fiVal-19:{&SV} = ""
                    fiVal-19:VISIBLE = FALSE
                    tCloseParen-19:VISIBLE = FALSE
                    cbAnd-19:VISIBLE = FALSE.
            END.
            ELSE DO:
                IF fiVal-19:HIDDEN THEN ASSIGN
                    tOpenParen-19:VISIBLE = TRUE
                    cbWhere-19:VISIBLE = TRUE
                    cbOp-19:VISIBLE = TRUE
                    fiVal-19:VISIBLE = TRUE
                    tCloseParen-19:VISIBLE = TRUE
                    cbAnd-19:VISIBLE = TRUE.
            END.
            APPLY 'value-changed' TO cbWhere-19.
        END.
        WHEN "cbAnd-19" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-20:VISIBLE THEN ASSIGN
                    tOpenParen-20:VISIBLE = FALSE
                    cbWhere-20:VISIBLE = FALSE
                    cbOp-20:VISIBLE = FALSE
                    fiVal-20:{&SV} = ""
                    fiVal-20:VISIBLE = FALSE
                    tCloseParen-20:VISIBLE = FALSE.
            END.
            ELSE DO:
                IF fiVal-20:HIDDEN THEN ASSIGN
                    tOpenParen-20:VISIBLE = TRUE
                    cbWhere-20:VISIBLE = TRUE
                    cbOp-20:VISIBLE = TRUE
                    fiVal-20:VISIBLE = TRUE
                    tCloseParen-20:VISIBLE = TRUE.
            END.
            APPLY 'value-changed' TO cbWhere-20.
        END.
    END CASE.
    RUN ipShowQuery IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAnd-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAnd-6 wWin
ON VALUE-CHANGED OF cbAnd-6 IN FRAME f-Main
OR VALUE-CHANGED OF cbAnd-7
OR VALUE-CHANGED OF cbAnd-8
OR VALUE-CHANGED OF cbAnd-9
DO:
    ASSIGN lProcessed = FALSE.
    IF SELF:{&SV} = ? THEN ASSIGN
    SELF:{&SV} = "".
    
    CASE SELF:NAME:
        WHEN "cbAnd-6" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-7:VISIBLE THEN ASSIGN
                    tOpenParen-7:VISIBLE = FALSE
                    cbWhere-7:VISIBLE = FALSE
                    cbOp-7:VISIBLE = FALSE
                    fiVal-7:{&SV} = ""
                    fiVal-7:VISIBLE = FALSE
                    tCloseParen-7:VISIBLE = FALSE
                    cbAnd-7:VISIBLE = FALSE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-7:HIDDEN THEN ASSIGN
                    tOpenParen-7:VISIBLE = TRUE
                    cbWhere-7:VISIBLE = TRUE
                    cbOp-7:VISIBLE = TRUE
                    fiVal-7:VISIBLE = TRUE
                    tCloseParen-7:VISIBLE = TRUE
                    cbAnd-7:VISIBLE = TRUE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-7.
        END.
        WHEN "cbAnd-7" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-8:VISIBLE THEN ASSIGN
                    tOpenParen-8:VISIBLE = FALSE
                    cbWhere-8:VISIBLE = FALSE
                    cbOp-8:VISIBLE = FALSE
                    fiVal-8:{&SV} = ""
                    fiVal-8:VISIBLE = FALSE
                    tCloseParen-8:VISIBLE = FALSE
                    cbAnd-8:VISIBLE = FALSE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-8:VISIBLE THEN ASSIGN
                    tOpenParen-8:VISIBLE = TRUE
                    cbWhere-8:VISIBLE = TRUE
                    cbOp-8:VISIBLE = TRUE
                    fiVal-8:VISIBLE = TRUE
                    tCloseParen-8:VISIBLE = TRUE
                    cbAnd-8:VISIBLE = TRUE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-8.
        END.
        WHEN "cbAnd-8" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-9:VISIBLE THEN ASSIGN
                    tOpenParen-9:VISIBLE = FALSE
                    cbWhere-9:VISIBLE = FALSE
                    cbOp-9:VISIBLE = FALSE
                    fiVal-9:{&SV} = ""
                    fiVal-9:VISIBLE = FALSE
                    tCloseParen-9:VISIBLE = FALSE
                    cbAnd-9:VISIBLE = FALSE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-9:HIDDEN THEN ASSIGN
                    tOpenParen-9:VISIBLE = TRUE
                    cbWhere-9:VISIBLE = TRUE
                    cbOp-9:VISIBLE = TRUE
                    fiVal-9:VISIBLE = TRUE
                    tCloseParen-9:VISIBLE = TRUE
                    cbAnd-9:VISIBLE = TRUE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-9.
        END.
        WHEN "cbAnd-9" THEN DO:
            IF SELF:{&SV} = "" OR SELF:{&SV} = " " OR SELF:{&SV} = ? THEN DO:
                IF fiVal-10:VISIBLE THEN ASSIGN
                    tOpenParen-10:VISIBLE = FALSE
                    cbWhere-10:VISIBLE = FALSE
                    cbOp-10:VISIBLE = FALSE
                    fiVal-10:{&SV} = ""
                    fiVal-10:VISIBLE = FALSE
                    tCloseParen-10:VISIBLE = FALSE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW - 1.25,9.25).
            END.
            ELSE DO:
                IF fiVal-10:HIDDEN THEN ASSIGN
                    tOpenParen-10:VISIBLE = TRUE
                    cbWhere-10:VISIBLE = TRUE
                    cbOp-10:VISIBLE = TRUE
                    fiVal-10:VISIBLE = TRUE
                    tCloseParen-10:VISIBLE = TRUE
                    tMore-2:ROW = MINIMUM(tMore-2:ROW + 1.25,9.25).
            END.
            APPLY 'value-changed' TO cbWhere-10.
        END.
    END CASE.
    
    APPLY 'value-changed' TO tMore-2.
    APPLY 'value-changed' TO tMore-3.
    
    RUN ipShowQuery IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFile-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFile-1 wWin
ON VALUE-CHANGED OF cbFile-1 IN FRAME f-Main /* FOR EACH */
OR VALUE-CHANGED OF cbFile-2
OR VALUE-CHANGED OF cbFile-3
OR VALUE-CHANGED OF cbFile-4
DO:
    ASSIGN lProcessed = FALSE.
    FIND _file WHERE
        _file._file-name = SELF:{&SV}
        NO-LOCK NO-ERROR.
    IF AVAIL _file THEN DO:
        CASE SELF:NAME:
            WHEN "cbFile-1" THEN DO:
                ASSIGN
                    cbWhere-1:LIST-ITEMS = " "
                    cbWhere-2:LIST-ITEMS = " "
                    cbWhere-3:LIST-ITEMS = " "
                    cbWhere-4:LIST-ITEMS = " "
                    cbWhere-5:LIST-ITEMS = " "
                    cbRel-2:LIST-ITEMS = ""
                    .
                FOR EACH _field OF _file:
                    cbWhere-1:ADD-LAST(_field._field-name).
                    cbWhere-2:ADD-LAST(_field._field-name).
                    cbWhere-3:ADD-LAST(_field._field-name).
                    cbWhere-4:ADD-LAST(_field._field-name).
                    cbWhere-5:ADD-LAST(_field._field-name).
                END.
                cbRel-2:ADD-LAST(SELF:{&SV}).
                ASSIGN
                    cbWhere-1:{&SV} = ENTRY(1,cbWhere-1:LIST-ITEMS)
                    cbWhere-2:{&SV} = ENTRY(1,cbWhere-2:LIST-ITEMS)
                    cbWhere-3:{&SV} = ENTRY(1,cbWhere-3:LIST-ITEMS)
                    cbWhere-4:{&SV} = ENTRY(1,cbWhere-4:LIST-ITEMS)
                    cbWhere-5:{&SV} = ENTRY(1,cbWhere-5:LIST-ITEMS)
                    cbRel-2:{&SV} = ENTRY(1,cbRel-2:LIST-ITEMS)
                    .
                IF NOT lLoading THEN RUN util/dFieldSelector.w (INPUT _file._file-name, 
                                                              INPUT-OUTPUT cFieldList[1]).
            END.
            WHEN "cbFile-2" THEN DO:
                ASSIGN
                    cbWhere-6:LIST-ITEMS = " "
                    cbWhere-7:LIST-ITEMS = " "
                    cbWhere-8:LIST-ITEMS = " "
                    cbWhere-9:LIST-ITEMS = " "
                    cbWhere-10:LIST-ITEMS = " "
                    cbRel-3:LIST-ITEMS = ""
                    .
                FOR EACH _field OF _file:
                    cbWhere-6:ADD-LAST(_field._field-name).
                    cbWhere-7:ADD-LAST(_field._field-name).
                    cbWhere-8:ADD-LAST(_field._field-name).
                    cbWhere-9:ADD-LAST(_field._field-name).
                    cbWhere-10:ADD-LAST(_field._field-name).
                END.
                cbRel-3:ADD-LAST(cbFile-1:{&SV}).
                cbRel-3:ADD-LAST(SELF:{&SV}).
                ASSIGN
                    cbWhere-6:{&SV} = ENTRY(1,cbWhere-6:LIST-ITEMS)
                    cbWhere-7:{&SV} = ENTRY(1,cbWhere-7:LIST-ITEMS)
                    cbWhere-8:{&SV} = ENTRY(1,cbWhere-8:LIST-ITEMS)
                    cbWhere-9:{&SV} = ENTRY(1,cbWhere-9:LIST-ITEMS)
                    cbWhere-10:{&SV} = ENTRY(1,cbWhere-10:LIST-ITEMS)
                    cbRel-3:{&SV} = ENTRY(1,cbRel-3:LIST-ITEMS)
                    .
                IF NOT lLoading THEN RUN util/dFieldSelector.w (INPUT _file._file-name, 
                                                              INPUT-OUTPUT cFieldList[2]).
            END.
            WHEN "cbFile-3" THEN DO:
                ASSIGN
                    cbWhere-11:LIST-ITEMS = " "
                    cbWhere-12:LIST-ITEMS = " "
                    cbWhere-13:LIST-ITEMS = " "
                    cbWhere-14:LIST-ITEMS = " "
                    cbWhere-15:LIST-ITEMS = " "
                    cbRel-4:LIST-ITEMS = ""
                    .
                FOR EACH _field OF _file:
                    cbWhere-11:ADD-LAST(_field._field-name).
                    cbWhere-12:ADD-LAST(_field._field-name).
                    cbWhere-13:ADD-LAST(_field._field-name).
                    cbWhere-14:ADD-LAST(_field._field-name).
                    cbWhere-15:ADD-LAST(_field._field-name).
                END.
                cbRel-4:ADD-LAST(cbFile-1:{&SV}).
                cbRel-4:ADD-LAST(cbFile-2:{&SV}).
                cbRel-4:ADD-LAST(SELF:{&SV}).
                ASSIGN
                    cbWhere-11:{&SV} = ENTRY(1,cbWhere-11:LIST-ITEMS)
                    cbWhere-12:{&SV} = ENTRY(1,cbWhere-12:LIST-ITEMS)
                    cbWhere-13:{&SV} = ENTRY(1,cbWhere-13:LIST-ITEMS)
                    cbWhere-14:{&SV} = ENTRY(1,cbWhere-14:LIST-ITEMS)
                    cbWhere-15:{&SV} = ENTRY(1,cbWhere-15:LIST-ITEMS)
                    cbRel-4:{&SV} = ENTRY(1,cbRel-4:LIST-ITEMS)
                    .
                IF NOT lLoading THEN RUN util/dFieldSelector.w (INPUT _file._file-name, 
                                                              INPUT-OUTPUT cFieldList[3]).
            END.
            WHEN "cbFile-4" THEN DO:
                ASSIGN
                    cbWhere-16:LIST-ITEMS = " "
                    cbWhere-17:LIST-ITEMS = " "
                    cbWhere-18:LIST-ITEMS = " "
                    cbWhere-19:LIST-ITEMS = " "
                    cbWhere-20:LIST-ITEMS = " "
                    .
                FOR EACH _field OF _file:
                    cbWhere-16:ADD-LAST(_field._field-name).
                    cbWhere-17:ADD-LAST(_field._field-name).
                    cbWhere-18:ADD-LAST(_field._field-name).
                    cbWhere-19:ADD-LAST(_field._field-name).
                    cbWhere-20:ADD-LAST(_field._field-name).
                END.
                ASSIGN
                    cbWhere-16:{&SV} = ENTRY(1,cbWhere-16:LIST-ITEMS)
                    cbWhere-17:{&SV} = ENTRY(1,cbWhere-17:LIST-ITEMS)
                    cbWhere-18:{&SV} = ENTRY(1,cbWhere-18:LIST-ITEMS)
                    cbWhere-19:{&SV} = ENTRY(1,cbWhere-19:LIST-ITEMS)
                    cbWhere-20:{&SV} = ENTRY(1,cbWhere-20:LIST-ITEMS)
                    .
                IF NOT lLoading THEN RUN util/dFieldSelector.w (INPUT _file._file-name, 
                                                              INPUT-OUTPUT cFieldList[4]).
            END.
        END CASE.
    END.

    ASSIGN
        cFileList = cbFile-1:{&SV} + "," +
                    (IF cbFile-2:{&SV} <> ? THEN cbFile-2:{&SV} ELSE "") + "," +
                    (IF cbFile-3:{&SV} <> ? THEN cbFile-3:{&SV} ELSE "") + "," +
                    (IF cbFile-4:{&SV} <> ? THEN cbFile-4:{&SV} ELSE "").
    
    IF NOT lLoading THEN DO:
        RUN ipShowQuery IN THIS-PROCEDURE.
        RUN ipSetDisplay IN THIS-PROCEDURE.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOf-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOf-2 wWin
ON VALUE-CHANGED OF cbOf-2 IN FRAME f-Main
OR VALUE-CHANGED OF cbOf-3
OR VALUE-CHANGED OF cbOf-4
DO:
    ASSIGN lProcessed = FALSE.
    CASE SELF:NAME:
        WHEN "cbOf-2" THEN DO:
            IF SELF:{&SV} = "OF" THEN ASSIGN
                cbRel-2:VISIBLE = TRUE
                fiWhere-2:VISIBLE = TRUE.
            ELSE ASSIGN
                cbRel-2:VISIBLE = FALSE
                fiWhere-2:VISIBLE = FALSE.
        END.
        WHEN "cbOf-3" THEN DO:
            IF SELF:{&SV} = "OF" THEN ASSIGN
                cbRel-3:VISIBLE = TRUE
                fiWhere-3:VISIBLE = TRUE.
            ELSE ASSIGN
                cbRel-3:VISIBLE = FALSE
                fiWhere-3:VISIBLE = FALSE.
        END.
        WHEN "cbOf-4" THEN DO:
            IF SELF:{&SV} = "OF" THEN ASSIGN
                cbRel-4:VISIBLE = TRUE
                fiWhere-4:VISIBLE = TRUE.
            ELSE ASSIGN
                cbRel-4:VISIBLE = FALSE
                fiWhere-4:VISIBLE = FALSE.
        END.
    END CASE.

    RUN ipShowQuery IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbWhere-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbWhere-1 wWin
ON VALUE-CHANGED OF cbWhere-1 IN FRAME f-Main
OR VALUE-CHANGED OF cbWhere-2
OR VALUE-CHANGED OF cbWhere-3
OR VALUE-CHANGED OF cbWhere-4
OR VALUE-CHANGED OF cbWhere-5
OR VALUE-CHANGED OF cbWhere-6
OR VALUE-CHANGED OF cbWhere-7
OR VALUE-CHANGED OF cbWhere-8
OR VALUE-CHANGED OF cbWhere-9
OR VALUE-CHANGED OF cbWhere-10
OR VALUE-CHANGED OF cbWhere-11
OR VALUE-CHANGED OF cbWhere-12
OR VALUE-CHANGED OF cbWhere-13
OR VALUE-CHANGED OF cbWhere-14
OR VALUE-CHANGED OF cbWhere-15
OR VALUE-CHANGED OF cbWhere-16
OR VALUE-CHANGED OF cbWhere-17
OR VALUE-CHANGED OF cbWhere-18
OR VALUE-CHANGED OF cbWhere-19
OR VALUE-CHANGED OF cbWhere-20
DO:
    ASSIGN lProcessed = FALSE.
    CASE SELF:NAME:
        WHEN "cbWhere-1" OR
        WHEN "cbWhere-2" OR
        WHEN "cbWhere-3" OR
        WHEN "cbWhere-4" OR
        WHEN "cbWhere-5" THEN FIND _file WHERE
            _file._file-name = cbFile-1:{&SV}
            NO-LOCK NO-ERROR.
        WHEN "cbWhere-6" OR
        WHEN "cbWhere-7" OR
        WHEN "cbWhere-8" OR
        WHEN "cbWhere-9" OR
        WHEN "cbWhere-10" THEN FIND _file WHERE
            _file._file-name = cbFile-2:{&SV}
            NO-LOCK NO-ERROR.
        WHEN "cbWhere-11" OR
        WHEN "cbWhere-12" OR
        WHEN "cbWhere-13" OR
        WHEN "cbWhere-14" OR
        WHEN "cbWhere-15" THEN FIND _file WHERE
            _file._file-name = cbFile-3:{&SV}
            NO-LOCK NO-ERROR.
        WHEN "cbWhere-16" OR
        WHEN "cbWhere-17" OR
        WHEN "cbWhere-18" OR
        WHEN "cbWhere-19" OR
        WHEN "cbWhere-20" THEN FIND _file WHERE
            _file._file-name = cbFile-4:{&SV}
            NO-LOCK NO-ERROR.
    END CASE.

    FIND _field OF _file WHERE
        _field._field-name = SELF:{&SV}
        NO-LOCK NO-ERROR.
    
    IF AVAIL _field THEN DO:
        CASE SELF:NAME:
            WHEN "cbWhere-1" THEN ASSIGN cDataType[1] = _field._data-type.
            WHEN "cbWhere-2" THEN ASSIGN cDataType[2] = _field._data-type.
            WHEN "cbWhere-3" THEN ASSIGN cDataType[3] = _field._data-type.
            WHEN "cbWhere-4" THEN ASSIGN cDataType[4] = _field._data-type.
            WHEN "cbWhere-5" THEN ASSIGN cDataType[5] = _field._data-type.
            WHEN "cbWhere-6" THEN ASSIGN cDataType[6] = _field._data-type.
            WHEN "cbWhere-7" THEN ASSIGN cDataType[7] = _field._data-type.
            WHEN "cbWhere-8" THEN ASSIGN cDataType[8] = _field._data-type.
            WHEN "cbWhere-9" THEN ASSIGN cDataType[9] = _field._data-type.
            WHEN "cbWhere-10" THEN ASSIGN cDataType[10] = _field._data-type.
            WHEN "cbWhere-11" THEN ASSIGN cDataType[11] = _field._data-type.
            WHEN "cbWhere-12" THEN ASSIGN cDataType[12] = _field._data-type.
            WHEN "cbWhere-13" THEN ASSIGN cDataType[13] = _field._data-type.
            WHEN "cbWhere-14" THEN ASSIGN cDataType[14] = _field._data-type.
            WHEN "cbWhere-15" THEN ASSIGN cDataType[15] = _field._data-type.
            WHEN "cbWhere-16" THEN ASSIGN cDataType[16] = _field._data-type.
            WHEN "cbWhere-17" THEN ASSIGN cDataType[17] = _field._data-type.
            WHEN "cbWhere-18" THEN ASSIGN cDataType[18] = _field._data-type.
            WHEN "cbWhere-19" THEN ASSIGN cDataType[19] = _field._data-type.
            WHEN "cbWhere-20" THEN ASSIGN cDataType[20] = _field._data-type.
        END CASE.
    END.

    RUN ipShowQuery IN THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiVal-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiVal-1 wWin
ON LEAVE OF fiVal-1 IN FRAME f-Main
OR LEAVE OF fiVal-2
OR LEAVE OF fiVal-3
OR LEAVE OF fiVal-4
OR LEAVE OF fiVal-5
OR LEAVE OF fiVal-6
OR LEAVE OF fiVal-7
OR LEAVE OF fiVal-8
OR LEAVE OF fiVal-9
OR LEAVE OF fiVal-10
OR LEAVE OF fiVal-11
OR LEAVE OF fiVal-12
OR LEAVE OF fiVal-13
OR LEAVE OF fiVal-14
OR LEAVE OF fiVal-15
OR LEAVE OF fiVal-16
OR LEAVE OF fiVal-17
OR LEAVE OF fiVal-18
OR LEAVE OF fiVal-19
OR LEAVE OF fiVal-20
DO:
    ASSIGN lProcessed = FALSE.
    RUN ipShowQuery IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Open wWin
ON CHOOSE OF MENU-ITEM m_Open /* Open... */
DO:
    RUN ipGetFile IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Quit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Quit wWin
ON CHOOSE OF MENU-ITEM m_Quit /* Quit */
DO:
    APPLY 'window-close' TO wWin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save wWin
ON CHOOSE OF MENU-ITEM m_Save /* Save */
DO:
    RUN ipSaveFile IN THIS-PROCEDURE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Save_As
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Save_As wWin
ON CHOOSE OF MENU-ITEM m_Save_As /* Save As... */
DO:
    RUN ipSaveFile IN THIS-PROCEDURE ("SaveAs").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slDisplayFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slDisplayFields wWin
ON VALUE-CHANGED OF slDisplayFields IN FRAME f-Main
DO:
    IF SELF:{&SV} = ENTRY(1,SELF:LIST-ITEMS) THEN ASSIGN
        bMoveUp:SENSITIVE = FALSE.
    ELSE ASSIGN
        bMoveUp:SENSITIVE = TRUE.
    
    IF SELF:{&SV} = ENTRY(SELF:NUM-ITEMS,SELF:LIST-ITEMS) THEN ASSIGN
        bMoveDown:SENSITIVE = FALSE.
    ELSE ASSIGN
        bMoveDown:SENSITIVE = TRUE.

    IF INDEX(SELF:{&SV},"(Calc)") <> 0 THEN ASSIGN
        bEditField:SENSITIVE = TRUE.
    ELSE ASSIGN
        bEditField:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tMore-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tMore-1 wWin
ON VALUE-CHANGED OF tMore-1 IN FRAME f-Main /* EACH */
DO:
    &SCOPED-DEFINE cFile 2
    &SCOPED-DEFINE cFirst 6
    &SCOPED-DEFINE cSecond 7
    &SCOPED-DEFINE cThird 8
    &SCOPED-DEFINE cFourth 9
    &SCOPED-DEFINE cFifth 10
    
    ASSIGN lProcessed = FALSE.
    ASSIGN
        cbFile-{&cFile}:ROW = SELF:ROW
        cbOf-{&cFile}:ROW = SELF:ROW
        cbRel-{&cFile}:ROW = SELF:ROW
        fiWhere-{&cFile}:ROW = SELF:ROW
        tOpenParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbWhere-{&cFirst}:ROW = SELF:ROW + 1.25
        cbOp-{&cFirst}:ROW = SELF:ROW + 1.25
        fiVal-{&cFirst}:ROW = SELF:ROW + 1.25
        tCloseParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbAnd-{&cFirst}:ROW = SELF:ROW + 1.25
        tOpenParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbWhere-{&cSecond}:ROW = SELF:ROW + 2.50
        cbOp-{&cSecond}:ROW = SELF:ROW + 2.50
        fiVal-{&cSecond}:ROW = SELF:ROW + 2.50
        tCloseParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbAnd-{&cSecond}:ROW = SELF:ROW + 2.50
        tOpenParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbWhere-{&cThird}:ROW = SELF:ROW + 3.75
        cbOp-{&cThird}:ROW = SELF:ROW + 3.75
        fiVal-{&cThird}:ROW = SELF:ROW + 3.75
        tCloseParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbAnd-{&cThird}:ROW = SELF:ROW + 3.75
        tOpenParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbWhere-{&cFourth}:ROW = SELF:ROW + 5.00
        cbOp-{&cFourth}:ROW = SELF:ROW + 5.00
        fiVal-{&cFourth}:ROW = SELF:ROW + 5.00
        tCloseParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbAnd-{&cFourth}:ROW = SELF:ROW + 5.00
        tOpenParen-{&cFifth}:ROW = SELF:ROW + 6.25
        cbWhere-{&cFifth}:ROW = SELF:ROW + 6.25
        cbOp-{&cFifth}:ROW = SELF:ROW + 6.25
        fiVal-{&cFifth}:ROW = SELF:ROW + 6.25
        tCloseParen-{&cFifth}:ROW = SELF:ROW + 6.25
        .
    IF NOT SELF:CHECKED THEN ASSIGN
        cbFile-{&cFile}:VISIBLE = FALSE
        cbOf-{&cFile}:VISIBLE = FALSE
        cbRel-{&cFile}:VISIBLE = FALSE
        fiWhere-{&cFile}:VISIBLE = FALSE
        tOpenParen-{&cFirst}:VISIBLE = FALSE
        cbWhere-{&cFirst}:VISIBLE = FALSE
        cbOp-{&cFirst}:VISIBLE = FALSE
        fiVal-{&cFirst}:VISIBLE = FALSE
        tCloseParen-{&cFirst}:VISIBLE = FALSE
        cbAnd-{&cFirst}:VISIBLE = FALSE
        tOpenParen-{&cSecond}:VISIBLE = FALSE
        cbWhere-{&cSecond}:VISIBLE = FALSE
        cbOp-{&cSecond}:VISIBLE = FALSE
        fiVal-{&cSecond}:VISIBLE = FALSE
        tCloseParen-{&cSecond}:VISIBLE = FALSE
        cbAnd-{&cSecond}:VISIBLE = FALSE
        tOpenParen-{&cThird}:VISIBLE = FALSE
        cbWhere-{&cThird}:VISIBLE = FALSE
        cbOp-{&cThird}:VISIBLE = FALSE
        fiVal-{&cThird}:VISIBLE = FALSE
        tCloseParen-{&cThird}:VISIBLE = FALSE
        cbAnd-{&cThird}:VISIBLE = FALSE
        tOpenParen-{&cFourth}:VISIBLE = FALSE
        cbWhere-{&cFourth}:VISIBLE = FALSE
        cbOp-{&cFourth}:VISIBLE = FALSE
        fiVal-{&cFourth}:VISIBLE = FALSE
        tCloseParen-{&cFourth}:VISIBLE = FALSE
        cbAnd-{&cFourth}:VISIBLE = FALSE
        tOpenParen-{&cFifth}:VISIBLE = FALSE
        cbWhere-{&cFifth}:VISIBLE = FALSE
        cbOp-{&cFifth}:VISIBLE = FALSE
        fiVal-{&cFifth}:VISIBLE = FALSE
        tCloseParen-{&cFifth}:VISIBLE = FALSE
        tMore-{&cFile}:VISIBLE = FALSE
        .
    ELSE ASSIGN
        cbFile-{&cFile}:VISIBLE = TRUE
        cbOf-{&cFile}:VISIBLE = TRUE
        cbRel-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        fiWhere-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        tOpenParen-{&cFirst}:VISIBLE = TRUE
        cbWhere-{&cFirst}:VISIBLE = TRUE
        cbOp-{&cFirst}:VISIBLE = TRUE
        fiVal-{&cFirst}:VISIBLE = TRUE
        tCloseParen-{&cFirst}:VISIBLE = TRUE
        cbAnd-{&cFirst}:VISIBLE = TRUE
        tMore-{&cFile}:ROW = SELF:ROW + 2.5
        tMore-{&cFile}:VISIBLE = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tMore-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tMore-2 wWin
ON VALUE-CHANGED OF tMore-2 IN FRAME f-Main /* EACH */
DO:
    &SCOPED-DEFINE cFile 3
    &SCOPED-DEFINE cFirst 11
    &SCOPED-DEFINE cSecond 12
    &SCOPED-DEFINE cThird 13
    &SCOPED-DEFINE cFourth 14
    &SCOPED-DEFINE cFifth 15
    
    ASSIGN lProcessed = FALSE.
    ASSIGN
        cbFile-{&cFile}:ROW = SELF:ROW
        cbOf-{&cFile}:ROW = SELF:ROW
        cbRel-{&cFile}:ROW = SELF:ROW
        fiWhere-{&cFile}:ROW = SELF:ROW
        tOpenParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbWhere-{&cFirst}:ROW = SELF:ROW + 1.25
        cbOp-{&cFirst}:ROW = SELF:ROW + 1.25
        fiVal-{&cFirst}:ROW = SELF:ROW + 1.25
        tCloseParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbAnd-{&cFirst}:ROW = SELF:ROW + 1.25
        tOpenParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbWhere-{&cSecond}:ROW = SELF:ROW + 2.50
        cbOp-{&cSecond}:ROW = SELF:ROW + 2.50
        fiVal-{&cSecond}:ROW = SELF:ROW + 2.50
        tCloseParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbAnd-{&cSecond}:ROW = SELF:ROW + 2.50
        tOpenParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbWhere-{&cThird}:ROW = SELF:ROW + 3.75
        cbOp-{&cThird}:ROW = SELF:ROW + 3.75
        fiVal-{&cThird}:ROW = SELF:ROW + 3.75
        tCloseParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbAnd-{&cThird}:ROW = SELF:ROW + 3.75
        tOpenParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbWhere-{&cFourth}:ROW = SELF:ROW + 5.00
        cbOp-{&cFourth}:ROW = SELF:ROW + 5.00
        fiVal-{&cFourth}:ROW = SELF:ROW + 5.00
        tCloseParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbAnd-{&cFourth}:ROW = SELF:ROW + 5.00
        tOpenParen-{&cFifth}:ROW = SELF:ROW + 6.25
        cbWhere-{&cFifth}:ROW = SELF:ROW + 6.25
        cbOp-{&cFifth}:ROW = SELF:ROW + 6.25
        fiVal-{&cFifth}:ROW = SELF:ROW + 6.25
        tCloseParen-{&cFifth}:ROW = SELF:ROW + 6.25
        .
    IF NOT SELF:CHECKED THEN ASSIGN
        cbFile-{&cFile}:VISIBLE = FALSE
        cbOf-{&cFile}:VISIBLE = FALSE
        cbRel-{&cFile}:VISIBLE = FALSE
        fiWhere-{&cFile}:VISIBLE = FALSE
        tOpenParen-{&cFirst}:VISIBLE = FALSE
        cbWhere-{&cFirst}:VISIBLE = FALSE
        cbOp-{&cFirst}:VISIBLE = FALSE
        fiVal-{&cFirst}:VISIBLE = FALSE
        tCloseParen-{&cFirst}:VISIBLE = FALSE
        cbAnd-{&cFirst}:VISIBLE = FALSE
        tOpenParen-{&cSecond}:VISIBLE = FALSE
        cbWhere-{&cSecond}:VISIBLE = FALSE
        cbOp-{&cSecond}:VISIBLE = FALSE
        fiVal-{&cSecond}:VISIBLE = FALSE
        tCloseParen-{&cSecond}:VISIBLE = FALSE
        cbAnd-{&cSecond}:VISIBLE = FALSE
        tOpenParen-{&cThird}:VISIBLE = FALSE
        cbWhere-{&cThird}:VISIBLE = FALSE
        cbOp-{&cThird}:VISIBLE = FALSE
        fiVal-{&cThird}:VISIBLE = FALSE
        tCloseParen-{&cThird}:VISIBLE = FALSE
        cbAnd-{&cThird}:VISIBLE = FALSE
        tOpenParen-{&cFourth}:VISIBLE = FALSE
        cbWhere-{&cFourth}:VISIBLE = FALSE
        cbOp-{&cFourth}:VISIBLE = FALSE
        fiVal-{&cFourth}:VISIBLE = FALSE
        tCloseParen-{&cFourth}:VISIBLE = FALSE
        cbAnd-{&cFourth}:VISIBLE = FALSE
        tOpenParen-{&cFifth}:VISIBLE = FALSE
        cbWhere-{&cFifth}:VISIBLE = FALSE
        cbOp-{&cFifth}:VISIBLE = FALSE
        fiVal-{&cFifth}:VISIBLE = FALSE
        tCloseParen-{&cFifth}:VISIBLE = FALSE
        tMore-{&cFile}:VISIBLE = FALSE
        .
    ELSE ASSIGN
        cbFile-{&cFile}:VISIBLE = TRUE
        cbOf-{&cFile}:VISIBLE = TRUE
        cbRel-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        fiWhere-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        tOpenParen-{&cFirst}:VISIBLE = TRUE
        cbWhere-{&cFirst}:VISIBLE = TRUE
        cbOp-{&cFirst}:VISIBLE = TRUE
        fiVal-{&cFirst}:VISIBLE = TRUE
        tCloseParen-{&cFirst}:VISIBLE = TRUE
        cbAnd-{&cFirst}:VISIBLE = TRUE
        tMore-{&cFile}:ROW = SELF:ROW + 2.5
        tMore-{&cFile}:VISIBLE = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tMore-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tMore-3 wWin
ON VALUE-CHANGED OF tMore-3 IN FRAME f-Main /* EACH */
DO:
    &SCOPED-DEFINE cFile 4
    &SCOPED-DEFINE cFirst 16
    &SCOPED-DEFINE cSecond 17
    &SCOPED-DEFINE cThird 18
    &SCOPED-DEFINE cFourth 19
    &SCOPED-DEFINE cFifth 20
    
    ASSIGN lProcessed = FALSE.
    ASSIGN
        cbFile-{&cFile}:ROW = SELF:ROW
        cbOf-{&cFile}:ROW = SELF:ROW
        cbRel-{&cFile}:ROW = SELF:ROW
        fiWhere-{&cFile}:ROW = SELF:ROW
        tOpenParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbWhere-{&cFirst}:ROW = SELF:ROW + 1.25
        cbOp-{&cFirst}:ROW = SELF:ROW + 1.25
        fiVal-{&cFirst}:ROW = SELF:ROW + 1.25
        tCloseParen-{&cFirst}:ROW = SELF:ROW + 1.25
        cbAnd-{&cFirst}:ROW = SELF:ROW + 1.25
        tOpenParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbWhere-{&cSecond}:ROW = SELF:ROW + 2.50
        cbOp-{&cSecond}:ROW = SELF:ROW + 2.50
        fiVal-{&cSecond}:ROW = SELF:ROW + 2.50
        tCloseParen-{&cSecond}:ROW = SELF:ROW + 2.50
        cbAnd-{&cSecond}:ROW = SELF:ROW + 2.50
        tOpenParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbWhere-{&cThird}:ROW = SELF:ROW + 3.75
        cbOp-{&cThird}:ROW = SELF:ROW + 3.75
        fiVal-{&cThird}:ROW = SELF:ROW + 3.75
        tCloseParen-{&cThird}:ROW = SELF:ROW + 3.75
        cbAnd-{&cThird}:ROW = SELF:ROW + 3.75
        tOpenParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbWhere-{&cFourth}:ROW = SELF:ROW + 5.00
        cbOp-{&cFourth}:ROW = SELF:ROW + 5.00
        fiVal-{&cFourth}:ROW = SELF:ROW + 5.00
        tCloseParen-{&cFourth}:ROW = SELF:ROW + 5.00
        cbAnd-{&cFourth}:ROW = SELF:ROW + 5.00
        tOpenParen-{&cFifth}:ROW = SELF:ROW + 6.25
        cbWhere-{&cFifth}:ROW = SELF:ROW + 6.25
        cbOp-{&cFifth}:ROW = SELF:ROW + 6.25
        fiVal-{&cFifth}:ROW = SELF:ROW + 6.25
        tCloseParen-{&cFifth}:ROW = SELF:ROW + 6.25
        .
    IF NOT SELF:CHECKED THEN ASSIGN
        cbFile-{&cFile}:VISIBLE = FALSE
        cbOf-{&cFile}:VISIBLE = FALSE
        cbRel-{&cFile}:VISIBLE = FALSE
        fiWhere-{&cFile}:VISIBLE = FALSE
        tOpenParen-{&cFirst}:VISIBLE = FALSE
        cbWhere-{&cFirst}:VISIBLE = FALSE
        cbOp-{&cFirst}:VISIBLE = FALSE
        fiVal-{&cFirst}:VISIBLE = FALSE
        tCloseParen-{&cFirst}:VISIBLE = FALSE
        cbAnd-{&cFirst}:VISIBLE = FALSE
        tOpenParen-{&cSecond}:VISIBLE = FALSE
        cbWhere-{&cSecond}:VISIBLE = FALSE
        cbOp-{&cSecond}:VISIBLE = FALSE
        fiVal-{&cSecond}:VISIBLE = FALSE
        tCloseParen-{&cSecond}:VISIBLE = FALSE
        cbAnd-{&cSecond}:VISIBLE = FALSE
        tOpenParen-{&cThird}:VISIBLE = FALSE
        cbWhere-{&cThird}:VISIBLE = FALSE
        cbOp-{&cThird}:VISIBLE = FALSE
        fiVal-{&cThird}:VISIBLE = FALSE
        tCloseParen-{&cThird}:VISIBLE = FALSE
        cbAnd-{&cThird}:VISIBLE = FALSE
        tOpenParen-{&cFourth}:VISIBLE = FALSE
        cbWhere-{&cFourth}:VISIBLE = FALSE
        cbOp-{&cFourth}:VISIBLE = FALSE
        fiVal-{&cFourth}:VISIBLE = FALSE
        tCloseParen-{&cFourth}:VISIBLE = FALSE
        cbAnd-{&cFourth}:VISIBLE = FALSE
        tOpenParen-{&cFifth}:VISIBLE = FALSE
        cbWhere-{&cFifth}:VISIBLE = FALSE
        cbOp-{&cFifth}:VISIBLE = FALSE
        fiVal-{&cFifth}:VISIBLE = FALSE
        tCloseParen-{&cFifth}:VISIBLE = FALSE
        .
    ELSE ASSIGN
        cbFile-{&cFile}:VISIBLE = TRUE
        cbOf-{&cFile}:VISIBLE = TRUE
        cbRel-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        fiWhere-{&cFile}:VISIBLE = IF cbOf-{&cFile}:{&SV} = "OF" THEN TRUE ELSE FALSE
        tOpenParen-{&cFirst}:VISIBLE = TRUE
        cbWhere-{&cFirst}:VISIBLE = TRUE
        cbOp-{&cFirst}:VISIBLE = TRUE
        fiVal-{&cFirst}:VISIBLE = TRUE
        tCloseParen-{&cFirst}:VISIBLE = TRUE
        cbAnd-{&cFirst}:VISIBLE = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tOpenParen-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tOpenParen-1 wWin
ON VALUE-CHANGED OF tOpenParen-1 IN FRAME f-Main /* ( */
OR VALUE-CHANGED OF tOpenParen-2
OR VALUE-CHANGED OF tOpenParen-3
OR VALUE-CHANGED OF tOpenParen-4
OR VALUE-CHANGED OF tOpenParen-5
OR VALUE-CHANGED OF tOpenParen-6
OR VALUE-CHANGED OF tOpenParen-7
OR VALUE-CHANGED OF tOpenParen-8
OR VALUE-CHANGED OF tOpenParen-9
OR VALUE-CHANGED OF tOpenParen-10
OR VALUE-CHANGED OF tOpenParen-11
OR VALUE-CHANGED OF tOpenParen-12
OR VALUE-CHANGED OF tOpenParen-13
OR VALUE-CHANGED OF tOpenParen-14
OR VALUE-CHANGED OF tOpenParen-15
OR VALUE-CHANGED OF tOpenParen-16
OR VALUE-CHANGED OF tOpenParen-17
OR VALUE-CHANGED OF tOpenParen-18
OR VALUE-CHANGED OF tOpenParen-19
OR VALUE-CHANGED OF tOpenParen-20
OR VALUE-CHANGED OF tCloseParen-1
OR VALUE-CHANGED OF tCloseParen-2
OR VALUE-CHANGED OF tCloseParen-3
OR VALUE-CHANGED OF tCloseParen-4
OR VALUE-CHANGED OF tCloseParen-5
OR VALUE-CHANGED OF tCloseParen-6
OR VALUE-CHANGED OF tCloseParen-7
OR VALUE-CHANGED OF tCloseParen-8
OR VALUE-CHANGED OF tCloseParen-9
OR VALUE-CHANGED OF tCloseParen-10
OR VALUE-CHANGED OF tCloseParen-11
OR VALUE-CHANGED OF tCloseParen-12
OR VALUE-CHANGED OF tCloseParen-13
OR VALUE-CHANGED OF tCloseParen-14
OR VALUE-CHANGED OF tCloseParen-15
OR VALUE-CHANGED OF tCloseParen-16
OR VALUE-CHANGED OF tCloseParen-17
OR VALUE-CHANGED OF tCloseParen-18
OR VALUE-CHANGED OF tCloseParen-19
OR VALUE-CHANGED OF tCloseParen-20
OR VALUE-CHANGED OF cbOp-1
OR VALUE-CHANGED OF cbOp-2
OR VALUE-CHANGED OF cbOp-3
OR VALUE-CHANGED OF cbOp-4
OR VALUE-CHANGED OF cbOp-5
OR VALUE-CHANGED OF cbOp-6
OR VALUE-CHANGED OF cbOp-7
OR VALUE-CHANGED OF cbOp-8
OR VALUE-CHANGED OF cbOp-9
OR VALUE-CHANGED OF cbOp-10
OR VALUE-CHANGED OF cbOp-11
OR VALUE-CHANGED OF cbOp-12
OR VALUE-CHANGED OF cbOp-13
OR VALUE-CHANGED OF cbOp-14
OR VALUE-CHANGED OF cbOp-15
OR VALUE-CHANGED OF cbOp-16
OR VALUE-CHANGED OF cbOp-17
OR VALUE-CHANGED OF cbOp-18
OR VALUE-CHANGED OF cbOp-19
OR VALUE-CHANGED OF cbOp-20
DO:
    ASSIGN lProcessed = FALSE.
    RUN ipShowQuery IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY cbFile-1 fiRptName tOpenParen-1 cbWhere-1 cbOp-1 fiVal-1 tCloseParen-1 
          cbAnd-1 tOpenParen-2 cbWhere-2 cbOp-2 fiVal-2 tCloseParen-2 cbAnd-2 
          eQuery tOpenParen-3 cbWhere-3 cbOp-3 fiVal-3 tCloseParen-3 cbAnd-3 
          tOpenParen-4 cbWhere-4 cbOp-4 fiVal-4 tCloseParen-4 cbAnd-4 
          tOpenParen-5 cbWhere-5 cbOp-5 fiVal-5 tCloseParen-5 tMore-1 cbFile-2 
          cbOf-2 cbRel-2 fiWhere-2 tOpenParen-6 cbWhere-6 cbOp-6 fiVal-6 
          tCloseParen-6 cbAnd-6 slDisplayFields fiLockedCols tOpenParen-7 
          cbWhere-7 cbOp-7 fiVal-7 tCloseParen-7 cbAnd-7 tOpenParen-8 cbWhere-8 
          cbOp-8 fiVal-8 tCloseParen-8 cbAnd-8 fiPagesWide tOpenParen-9 
          cbWhere-9 cbOp-9 fiVal-9 tCloseParen-9 cbAnd-9 tOpenParen-10 
          cbWhere-10 cbOp-10 fiVal-10 tCloseParen-10 tMore-2 cbFile-3 cbOf-3 
          cbRel-3 fiWhere-3 tOpenParen-11 cbWhere-11 cbOp-11 fiVal-11 
          tCloseParen-11 cbAnd-11 tOpenParen-12 cbWhere-12 cbOp-12 fiVal-12 
          tCloseParen-12 cbAnd-12 tOpenParen-13 cbWhere-13 cbOp-13 fiVal-13 
          tCloseParen-13 cbAnd-13 tOpenParen-14 cbWhere-14 cbOp-14 fiVal-14 
          tCloseParen-14 cbAnd-14 tOpenParen-15 cbWhere-15 cbOp-15 fiVal-15 
          tCloseParen-15 tMore-3 cbFile-4 cbOf-4 cbRel-4 fiWhere-4 tOpenParen-16 
          cbWhere-16 cbOp-16 fiVal-16 tCloseParen-16 cbAnd-16 tOpenParen-17 
          cbWhere-17 cbOp-17 fiVal-17 tCloseParen-17 cbAnd-17 tOpenParen-18 
          cbWhere-18 cbOp-18 fiVal-18 tCloseParen-18 cbAnd-18 tOpenParen-19 
          cbWhere-19 cbOp-19 fiVal-19 tCloseParen-19 cbAnd-19 tOpenParen-20 
          cbWhere-20 cbOp-20 fiVal-20 tCloseParen-20 
      WITH FRAME f-Main IN WINDOW wWin.
  ENABLE bExpand cbFile-1 fiRptName tOpenParen-1 cbWhere-1 cbOp-1 fiVal-1 
         tCloseParen-1 cbAnd-1 bCheckSyntax tOpenParen-2 cbWhere-2 cbOp-2 
         fiVal-2 tCloseParen-2 cbAnd-2 eQuery tOpenParen-3 cbWhere-3 cbOp-3 
         fiVal-3 tCloseParen-3 cbAnd-3 tOpenParen-4 cbWhere-4 cbOp-4 fiVal-4 
         tCloseParen-4 cbAnd-4 tOpenParen-5 cbWhere-5 cbOp-5 fiVal-5 
         tCloseParen-5 tMore-1 cbFile-2 cbOf-2 cbRel-2 tOpenParen-6 cbWhere-6 
         cbOp-6 fiVal-6 tCloseParen-6 cbAnd-6 slDisplayFields fiLockedCols 
         tOpenParen-7 cbWhere-7 cbOp-7 fiVal-7 tCloseParen-7 cbAnd-7 
         tOpenParen-8 cbWhere-8 cbOp-8 fiVal-8 tCloseParen-8 cbAnd-8 
         fiPagesWide tOpenParen-9 cbWhere-9 cbOp-9 fiVal-9 tCloseParen-9 
         cbAnd-9 bMoveUp tOpenParen-10 cbWhere-10 cbOp-10 fiVal-10 
         tCloseParen-10 bMoveDown tMore-2 cbFile-3 cbOf-3 cbRel-3 bRemove 
         tOpenParen-11 cbWhere-11 cbOp-11 fiVal-11 tCloseParen-11 cbAnd-11 
         bCalcField tOpenParen-12 cbWhere-12 cbOp-12 fiVal-12 tCloseParen-12 
         cbAnd-12 bEditField tOpenParen-13 cbWhere-13 cbOp-13 fiVal-13 
         tCloseParen-13 cbAnd-13 bLineBreak tOpenParen-14 cbWhere-14 cbOp-14 
         fiVal-14 tCloseParen-14 cbAnd-14 tOpenParen-15 cbWhere-15 cbOp-15 
         fiVal-15 tCloseParen-15 bProcess bExport tMore-3 cbFile-4 cbOf-4 
         cbRel-4 tOpenParen-16 cbWhere-16 cbOp-16 fiVal-16 tCloseParen-16 
         cbAnd-16 tOpenParen-17 cbWhere-17 cbOp-17 fiVal-17 tCloseParen-17 
         cbAnd-17 tOpenParen-18 cbWhere-18 cbOp-18 fiVal-18 tCloseParen-18 
         cbAnd-18 tOpenParen-19 cbWhere-19 cbOp-19 fiVal-19 tCloseParen-19 
         cbAnd-19 tOpenParen-20 cbWhere-20 cbOp-20 fiVal-20 tCloseParen-20 
      WITH FRAME f-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-f-Main}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    cbFile-1:ADD-LAST(" ") {&IN}.
    cbFile-2:ADD-LAST(" ") {&IN}.
    cbFile-3:ADD-LAST(" ") {&IN}.
    cbFile-4:ADD-LAST(" ") {&IN}.
    FOR EACH _file:
        IF _file._file-name BEGINS "_" THEN NEXT.
        IF _file._file-name BEGINS "SYS" THEN NEXT.
        cbFile-1:ADD-LAST(_file._file-name) {&IN}.
        cbFile-2:ADD-LAST(_file._file-name) {&IN}.
        cbFile-3:ADD-LAST(_file._file-name) {&IN}.
        cbFile-4:ADD-LAST(_file._file-name) {&IN}.
    END.

  RUN SUPER.

    /* Hide all except the first group of items for entry */
    h_frame = FRAME f-main:HANDLE.
    IF VALID-HANDLE (h_frame) THEN do:
        h_frame = h_frame:FIRST-CHILD.  /* field-group */
        h_field = h_frame:FIRST-CHILD.  /* field-group */
        outer-loop:
        repeat:
            if CAN-DO("toggle-box,fill-in,combo-box",h_field:type) 
            AND NOT SUBSTRING(h_field:NAME,LENGTH(h_field:name) - 1,2) = "-1"  
            AND NOT h_field:NAME = "fiRptName"
            AND NOT h_field:NAME = "fiLockedCols"
            AND NOT h_field:NAME = "fiPagesWide" THEN ASSIGN
                h_field:VISIBLE = FALSE.
            IF h_field:NAME = "tMore-1" THEN ASSIGN
                h_field:ROW = 4.
            h_field = h_field:NEXT-SIBLING.
            IF NOT VALID-HANDLE (h_field) THEN LEAVE.
        END.
        ASSIGN
            bExpand:VISIBLE = FALSE.
    END.
    
    ASSIGN
        cbFile-1:{&SV} = ENTRY(1,cbFile-1:LIST-ITEMS)
        cbFile-2:{&SV} = ENTRY(1,cbFile-2:LIST-ITEMS)
        cbFile-3:{&SV} = ENTRY(1,cbFile-3:LIST-ITEMS)
        cbFile-4:{&SV} = ENTRY(1,cbFile-4:LIST-ITEMS)
        .
    /*
    APPLY 'value-changed' TO cbFile-1.
    APPLY 'value-changed' TO cbWhere-1.
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipDefineQuery wWin 
PROCEDURE ipDefineQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR jCtr AS INT NO-UNDO.

    ASSIGN 
        lQueryOK = FALSE.

    IF VALID-HANDLE(hBrowse) THEN DELETE WIDGET hBrowse.
    IF VALID-HANDLE(hQuery) THEN DELETE WIDGET hQuery.
    IF VALID-HANDLE(hBuffer[1]) THEN DELETE WIDGET hBuffer[1].
    IF VALID-HANDLE(hBuffer[2]) THEN DELETE WIDGET hBuffer[2].
    IF VALID-HANDLE(hBuffer[3]) THEN DELETE WIDGET hBuffer[3].
    IF VALID-HANDLE(hBuffer[4]) THEN DELETE WIDGET hBuffer[4].

    IF cbFile-1:{&SV} <> ? THEN CREATE BUFFER hBuffer[1] FOR TABLE cbFile-1:{&SV}.
    ELSE DO:
        MESSAGE
            "You have not specified any files to process."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    DO iCtr = 1 TO hBuffer[1]:NUM-FIELDS:
        ASSIGN
            h_field = hBuffer[1]:BUFFER-FIELD(iCtr)
            h_field:VALIDATE-EXPRESSION = "".

    END.
    IF cbFile-2:VISIBLE AND cbFile-2:{&SV} <> ? THEN CREATE BUFFER hBuffer[2] FOR TABLE cbFile-2:{&SV}.
    IF cbFile-3:VISIBLE AND cbFile-3:{&SV} <> ? THEN CREATE BUFFER hBuffer[3] FOR TABLE cbFile-3:{&SV}.
    IF cbFile-4:VISIBLE AND cbFile-4:{&SV} <> ? THEN CREATE BUFFER hBuffer[4] FOR TABLE cbFile-4:{&SV}.
    
    DO jCtr = 2 TO 4:
        IF VALID-HANDLE(hBuffer[jCtr]) THEN DO iCtr = 1 TO hBuffer[jCtr]:NUM-FIELDS:
            ASSIGN
                h_field = hBuffer[jCtr]:BUFFER-FIELD(iCtr)
                h_field:VALIDATE-EXPRESSION = "".
        END.
    END.

    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hBuffer[1]).
    IF VALID-HANDLE(hBuffer[2]) THEN hQuery:ADD-BUFFER(hBuffer[2]).
    IF VALID-HANDLE(hBuffer[3]) THEN hQuery:ADD-BUFFER(hBuffer[3]).
    IF VALID-HANDLE(hBuffer[4]) THEN hQuery:ADD-BUFFER(hBuffer[4]).
    lQueryOK = hQuery:QUERY-PREPARE(eQuery:{&SV}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipExcel wWin 
PROCEDURE ipExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTable              AS CHARACTER    NO-UNDO.
    DEF VAR cFields             AS CHARACTER    NO-UNDO.
    DEF VAR hQuery              AS HANDLE       NO-UNDO.
    DEF VAR hBuffer2            AS HANDLE       NO-UNDO.
    DEF VAR hField              AS HANDLE       NO-UNDO.
    DEF VAR iFields             AS INTEGER      NO-UNDO.
    DEF VAR iDispRow                AS INTEGER      NO-UNDO.
    DEF VAR cRow                AS CHARACTER    NO-UNDO INITIAL "A".
    DEF VAR hThisField          AS HANDLE       NO-UNDO.
    DEF VAR hThisColumn         AS HANDLE       NO-UNDO.
    DEF VAR iCtr2               AS INT          NO-UNDO.
    DEF VAR iCtr3               AS INT          NO-UNDO.
    DEF VAR iColumn             AS INT          NO-UNDO.
    DEF VAR lExtentCleared      AS LOG          NO-UNDO.
    DEF VAR cLastField          AS CHAR         NO-UNDO.
    DEF VAR iBrowseCount        AS INT          NO-UNDO.
    DEF VAR tWidth              AS INT          NO-UNDO.
    DEF VAR cExtString AS CHAR NO-UNDO.
    DEF VAR iHdrRows AS INT NO-UNDO.
    DEF VAR cTestField AS CHAR NO-UNDO.
    DEF VAR iStartRow AS INT NO-UNDO.
    DEF VAR cTempFormula AS CHAR EXTENT 100 NO-UNDO.
    DEF VAR cTempFormula2 AS CHAR EXTENT 100 NO-UNDO.

    IF hBrowse:NUM-SELECTED-ROWS = 0 THEN hBrowse:SELECT-ALL().

    STATUS DEFAULT "Exporting data. Please wait...".
    /*
    SESSION:SET-WAIT-STATE("general").
    */
    CREATE "Excel.Application" chExcelApplication.

    ASSIGN 
        chWorkbook                 = chExcelApplication:Workbooks:Add()
        chWorkSheet                = chExcelApplication:Sheets:Item(1)
        cTable                     = IF fiRptName:{&SV} <> "" THEN fiRptName:{&SV} ELSE "TEMP"
        chWorkSheet:NAME           = cTable
        iDispRow = 1
        iHdrRows = 1
        iColumn = 0.
    
    DO iFields = 1 TO hBrowse:NUM-COLUMNS:
        ASSIGN 
            iColumn = iColumn + 1
            hThisColumn = hBrowse:GET-BROWSE-COLUMN(iFields).
        IF hThisColumn:LABEL BEGINS "<skip" THEN ASSIGN
            iDispRow = iDispRow + 1
            iHdrRows = iHdrRows + 1
            iColumn = 0.
        ELSE DO:
            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):Value = (hThisColumn:LABEL).
            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):Font:Bold = TRUE.
        END.
    END.

    FOR EACH ttColumns WHERE 
        ttColumns.cFormula <> ? AND ttColumns.cFormula <> "":
        ASSIGN
            cTempFormula[ttColumns.iPos] = ttColumns.cFormula
            cTestField = "".
        FOR EACH bttColumns:
            ASSIGN
                cTestField = IF INDEX(bttColumns.cColLabel,"[") <> 0 THEN SUBSTRING(bttColumns.cColLabel,INDEX(bttColumns.cColLabel,"[")) ELSE ""
                cTestField = bttColumns.cTable + "." + bttColumns.cField + cTestField.
            IF cTestField <> ? 
            AND cTestField <> "."
            AND INDEX(ttColumns.cFormula,cTestField) <> 0 THEN ASSIGN
                cTempFormula[ttColumns.iPos] = REPLACE(cTempFormula[ttColumns.iPos],cTestField,bttColumns.cColumn + "|" + STRING(bttColumns.iRow - 1) + "|").
        END.
    END.

    iDispRow = iHdrRows.
    
    DO iCtr3 = 1 TO hBrowse:NUM-SELECTED-ROWS:
        hBrowse:FETCH-SELECTED-ROW(iCtr3).
        ASSIGN
            iDispRow = iDispRow + 1
            iColumn = 1
            iStartRow = iDispRow.
        IF iCtr3 MODULO 100 = 0 THEN STATUS DEFAULT 
            "Exporting data. Please wait... (" + string(iCtr3) +
            " of " + STRING(hBrowse:NUM-SELECTED-ROWS) + " records exported)".
        
        DO iFields = 1 TO hBrowse:NUM-COLUMNS:
            hThisColumn = hBrowse:GET-BROWSE-COLUMN(iFields).
            FIND ttColumns WHERE 
                ttColumns.iPos = iFields.
            IF hThisColumn:LABEL = "<skip>" THEN ASSIGN
                iDispRow = iDispRow + 1
                iColumn = 1.
            ELSE DO:
                ASSIGN 
                    hField = hThisColumn:BUFFER-FIELD.
                IF NOT VALID-HANDLE(hField) THEN DO: /* It's a calc field, needs a formula */
                    ASSIGN 
                        cTempFormula2[iFields] = cTempFormula[iFields]
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|0|",STRING(iDispRow - (ttColumns.iRow - 1)))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|1|",STRING(iDispRow - (ttColumns.iRow - 1) + 1))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|2|",STRING(iDispRow - (ttColumns.iRow - 1) + 2))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|3|",STRING(iDispRow - (ttColumns.iRow - 1) + 3))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|4|",STRING(iDispRow - (ttColumns.iRow - 1) + 4))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|5|",STRING(iDispRow - (ttColumns.iRow - 1) + 5))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|6|",STRING(iDispRow - (ttColumns.iRow - 1) + 6))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|7|",STRING(iDispRow - (ttColumns.iRow - 1) + 7))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|8|",STRING(iDispRow - (ttColumns.iRow - 1) + 8))
                        cTempFormula2[iFields] = REPLACE(cTempFormula2[iFields],"|9|",STRING(iDispRow - (ttColumns.iRow - 1) + 9)).
                    chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):NumberFormat = 0.
                    chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):formula = cTempFormula2[iFields].
                    iColumn = iColumn + 1.
                END.
                ELSE IF hThisColumn:NAME = hField:NAME THEN DO: /* It's a data field */
                    chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#####0"
                                                                             ELSE IF hField:DATA-TYPE = "decimal" THEN "###,###,##0.00"
                                                                             ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY"
                                                                             ELSE "@".
                    IF hField:EXTENT < 2 THEN DO: 
                        chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):Value = hField:BUFFER-VALUE.
                        iColumn = iColumn + 1.
                    END.
                    ELSE DO:
                        chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iDispRow)):Value = hField:BUFFER-VALUE(ttColumns.iExtent).
                        iColumn = iColumn + 1.
                    END.
                END.
            END.
        END.
    END.

    chWorksheet:COLUMNS("A:" + STRING(ENTRY(hBrowse:NUM-COLUMNS,cColList))):AutoFit.
    chWorksheet:PageSetup:LeftHeader = fiRptName:{&SV}.
    chWorksheet:PageSetup:LeftFooter = "MXP Query Builder - (c)2016, Foresight Software".                    
    chWorksheet:PageSetup:RightHeader = "User: " + USERID(LDBNAME(1)) + " DB: " + LDBNAME(1) + " Print Date: " +
                                         STRING(MONTH(TODAY),"99") + "/" +
                                         STRING(DAY(TODAY),"99") + "/" +
                                         STRING(YEAR(TODAY),"9999").
    chWorksheet:PageSetup:FirstPageNumber = 1.
    chWorksheet:PageSetup:RightFooter = "Page: &P".
    chWorksheet:PageSetup:PrintGridlines = TRUE.
    chWorksheet:PageSetup:PrintTitleRows = "1:" + STRING(iHdrRows).
    chWorksheet:PageSetup:PrintTitleColumns = "A:" + ENTRY(INTEGER(fiLockedCols:{&SV}),cColList).
    chWorksheet:PageSetup:ORIENTATION = 2.
    chWorksheet:PageSetup:LeftMargin = chExcelApplication:InchesToPoints(0.3).   
    chWorksheet:PageSetup:RightMargin = chExcelApplication:InchesToPoints(0.3).  
    chWorksheet:PageSetup:TopMargin = chExcelApplication:InchesToPoints(1.0).    
    chWorksheet:PageSetup:BottomMargin = chExcelApplication:InchesToPoints(0.75).
    chWorksheet:PageSetup:HeaderMargin = chExcelApplication:InchesToPoints(0.3). 
    chWorksheet:PageSetup:ZOOM = FALSE.
    chWorksheet:PageSetup:FitToPagesWide = MAXIMUM(1,INTEGER(fiPagesWide:{&SV})).
    chWorksheet:PageSetup:FitToPagesTall = 9999.
    chWorkSheet:range(ENTRY(INTEGER(fiLockedCols:{&SV}) + 1,cColList) + STRING(iHdrRows + 1)):SELECT.
    chExcelApplication:ActiveWindow:FreezePanes = TRUE.

    ASSIGN    
        chExcelApplication:Visible = TRUE.

    SESSION:SET-WAIT-STATE("").
    STATUS DEFAULT cStatus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipGetFile wWin 
PROCEDURE ipGetFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCalcCols.
    EMPTY TEMP-TABLE ttColumns.

    ASSIGN
        lLoading = TRUE.
    SYSTEM-DIALOG GET-FILE cFileName
    TITLE "Select a Query File to Open..."
    FILTERS "Query Files (*.qry)"  "*.qry"
    MUST-EXIST
    USE-FILENAME
    DEFAULT-EXTENSION ".qry"
    INITIAL-DIR "..\Reports".
    IF SEARCH(cFileName) = ? THEN DO:
        MESSAGE
            "You did not select a valid report file."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END.
    INPUT FROM VALUE(cFileName).
    IMPORT UNFORMATTED cDataString.
    INPUT CLOSE.
    IF NUM-ENTRIES(cDataString,"|") < 130 THEN DO:
        MESSAGE
            "The report file you selected is invalid or corrupt. Please try again."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    h_frame = FRAME f-main:HANDLE.
    IF VALID-HANDLE (h_frame) THEN do:
        h_frame = h_frame:FIRST-CHILD.  /* field-group */
        h_field = h_frame:FIRST-CHILD.  /* field-group */
        outer-loop:
        repeat:
            if CAN-DO("toggle-box,fill-in,combo-box",h_field:type) 
            AND NOT SUBSTRING(h_field:NAME,LENGTH(h_field:name) - 1,2) = "-1"
            AND NOT h_field:NAME = "fiRptName"
            AND NOT h_field:NAME = "fiLockedCols"
            AND NOT h_field:NAME = "fiPagesWide" THEN ASSIGN
                h_field:VISIBLE = FALSE.
            IF h_field:NAME = "tMore-1" THEN ASSIGN
                h_field:ROW = 4.
            h_field = h_field:NEXT-SIBLING.
            IF NOT VALID-HANDLE (h_field) THEN LEAVE.
        END.
    END.
    DO iCtr = 1 TO 4:
        CASE iCtr:
            WHEN 1 THEN ASSIGN cbFile-1:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 2 THEN ASSIGN cbFile-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 3 THEN ASSIGN cbFile-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 4 THEN ASSIGN cbFile-4:{&SV} = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END.
    ASSIGN
        cFileList = cbFile-1:{&SV} + "," +
                    (IF cbFile-2:{&SV} <> ? THEN cbFile-2:{&SV} ELSE "") + "," +
                    (IF cbFile-3:{&SV} <> ? THEN cbFile-3:{&SV} ELSE "") + "," +
                    (IF cbFile-4:{&SV} <> ? THEN cbFile-4:{&SV} ELSE "").
    APPLY 'value-changed' to cbFile-1.
    APPLY 'value-changed' to cbFile-2.
    APPLY 'value-changed' to cbFile-3.
    APPLY 'value-changed' to cbFile-4.
    DO iCtr = 5 TO 10:
        CASE iCtr:
            WHEN 5 THEN ASSIGN cbOf-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 6 THEN ASSIGN cbOf-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 7 THEN ASSIGN cbOf-4:{&SV} = ENTRY(iCtr,cDataString,"|").

            WHEN 8 THEN ASSIGN cbRel-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 9 THEN ASSIGN cbRel-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 10 THEN ASSIGN cbRel-4:{&SV} = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END.
    DO iCtr = 11 TO 30:
        CASE iCtr:
            WHEN 11 THEN ASSIGN cbWhere-1:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 12 THEN ASSIGN cbWhere-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 13 THEN ASSIGN cbWhere-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 14 THEN ASSIGN cbWhere-4:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 15 THEN ASSIGN cbWhere-5:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 16 THEN ASSIGN cbWhere-6:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 17 THEN ASSIGN cbWhere-7:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 18 THEN ASSIGN cbWhere-8:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 19 THEN ASSIGN cbWhere-9:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 20 THEN ASSIGN cbWhere-10:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 21 THEN ASSIGN cbWhere-11:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 22 THEN ASSIGN cbWhere-12:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 23 THEN ASSIGN cbWhere-13:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 24 THEN ASSIGN cbWhere-14:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 25 THEN ASSIGN cbWhere-15:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 26 THEN ASSIGN cbWhere-16:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 27 THEN ASSIGN cbWhere-17:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 28 THEN ASSIGN cbWhere-18:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 29 THEN ASSIGN cbWhere-19:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 30 THEN ASSIGN cbWhere-20:{&SV} = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END.
    DO iCtr = 31 TO 50:
        CASE iCtr:
            WHEN 31 THEN ASSIGN cbOp-1:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 32 THEN ASSIGN cbOp-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 33 THEN ASSIGN cbOp-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 34 THEN ASSIGN cbOp-4:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 35 THEN ASSIGN cbOp-5:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 36 THEN ASSIGN cbOp-6:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 37 THEN ASSIGN cbOp-7:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 38 THEN ASSIGN cbOp-8:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 39 THEN ASSIGN cbOp-9:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 40 THEN ASSIGN cbOp-10:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 41 THEN ASSIGN cbOp-11:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 42 THEN ASSIGN cbOp-12:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 43 THEN ASSIGN cbOp-13:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 44 THEN ASSIGN cbOp-14:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 45 THEN ASSIGN cbOp-15:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 46 THEN ASSIGN cbOp-16:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 47 THEN ASSIGN cbOp-17:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 48 THEN ASSIGN cbOp-18:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 49 THEN ASSIGN cbOp-19:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 50 THEN ASSIGN cbOp-20:{&SV} = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END.
    DO iCtr = 51 TO 70:
        CASE iCtr:
            WHEN 51 THEN ASSIGN fiVal-1:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 52 THEN ASSIGN fiVal-2:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 53 THEN ASSIGN fiVal-3:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 54 THEN ASSIGN fiVal-4:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 55 THEN ASSIGN fiVal-5:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 56 THEN ASSIGN fiVal-6:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 57 THEN ASSIGN fiVal-7:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 58 THEN ASSIGN fiVal-8:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 59 THEN ASSIGN fiVal-9:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 60 THEN ASSIGN fiVal-10:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 61 THEN ASSIGN fiVal-11:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 62 THEN ASSIGN fiVal-12:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 63 THEN ASSIGN fiVal-13:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 64 THEN ASSIGN fiVal-14:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 65 THEN ASSIGN fiVal-15:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 66 THEN ASSIGN fiVal-16:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 67 THEN ASSIGN fiVal-17:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 68 THEN ASSIGN fiVal-18:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 69 THEN ASSIGN fiVal-19:{&SV} = ENTRY(iCtr,cDataString,"|").
            WHEN 70 THEN ASSIGN fiVal-20:{&SV} = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END.
    DO iCtr = 71 TO 86:
        CASE iCtr:
            WHEN 71 THEN ASSIGN cbAnd-1:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 72 THEN ASSIGN cbAnd-2:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 73 THEN ASSIGN cbAnd-3:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 74 THEN ASSIGN cbAnd-4:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 75 THEN ASSIGN cbAnd-6:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 76 THEN ASSIGN cbAnd-7:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 77 THEN ASSIGN cbAnd-8:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 78 THEN ASSIGN cbAnd-9:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ". 
            WHEN 79 THEN ASSIGN cbAnd-11:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 80 THEN ASSIGN cbAnd-12:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 81 THEN ASSIGN cbAnd-13:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 82 THEN ASSIGN cbAnd-14:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 83 THEN ASSIGN cbAnd-16:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 84 THEN ASSIGN cbAnd-17:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 85 THEN ASSIGN cbAnd-18:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
            WHEN 86 THEN ASSIGN cbAnd-19:{&SV} = IF ENTRY(iCtr,cDataString,"|") <> "" THEN ENTRY(iCtr,cDataString,"|") ELSE " ".
        END CASE.
    END.
    DO iCtr = 87 TO 106:
        CASE iCtr:
            WHEN 87 THEN ASSIGN tOpenParen-1:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 88 THEN ASSIGN tOpenParen-2:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 89 THEN ASSIGN tOpenParen-3:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 90 THEN ASSIGN tOpenParen-4:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 91 THEN ASSIGN tOpenParen-5:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 92 THEN ASSIGN tOpenParen-6:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 93 THEN ASSIGN tOpenParen-7:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 94 THEN ASSIGN tOpenParen-8:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 95 THEN ASSIGN tOpenParen-9:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 96 THEN ASSIGN tOpenParen-10:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 97 THEN ASSIGN tOpenParen-11:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 98 THEN ASSIGN tOpenParen-12:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 99 THEN ASSIGN tOpenParen-13:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 100 THEN ASSIGN tOpenParen-14:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 101 THEN ASSIGN tOpenParen-15:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 102 THEN ASSIGN tOpenParen-16:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 103 THEN ASSIGN tOpenParen-17:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 104 THEN ASSIGN tOpenParen-18:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 105 THEN ASSIGN tOpenParen-19:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 106 THEN ASSIGN tOpenParen-20:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
        END CASE.
    END.
    DO iCtr = 107 TO 126:
        CASE iCtr:
            WHEN 107 THEN ASSIGN tCloseParen-1:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 108 THEN ASSIGN tCloseParen-2:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 109 THEN ASSIGN tCloseParen-3:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 110 THEN ASSIGN tCloseParen-4:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 111 THEN ASSIGN tCloseParen-5:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 112 THEN ASSIGN tCloseParen-6:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 113 THEN ASSIGN tCloseParen-7:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 114 THEN ASSIGN tCloseParen-8:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 115 THEN ASSIGN tCloseParen-9:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 116 THEN ASSIGN tCloseParen-10:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 117 THEN ASSIGN tCloseParen-11:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 118 THEN ASSIGN tCloseParen-12:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 119 THEN ASSIGN tCloseParen-13:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 120 THEN ASSIGN tCloseParen-14:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 121 THEN ASSIGN tCloseParen-15:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 122 THEN ASSIGN tCloseParen-16:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 123 THEN ASSIGN tCloseParen-17:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 124 THEN ASSIGN tCloseParen-18:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 125 THEN ASSIGN tCloseParen-19:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
            WHEN 126 THEN ASSIGN tCloseParen-20:CHECKED = IF ENTRY(iCtr,cDataString,"|") = "Y" THEN TRUE ELSE FALSE.
        END CASE.
    END.
    DO iCtr = 127 TO 130:
        CASE iCtr:
            WHEN 127 THEN ASSIGN cFieldList[1] = ENTRY(iCtr,cDataString,"|").
            WHEN 128 THEN ASSIGN cFieldList[2] = ENTRY(iCtr,cDataString,"|").
            WHEN 129 THEN ASSIGN cFieldList[3] = ENTRY(iCtr,cDataString,"|").
            WHEN 130 THEN ASSIGN cFieldList[4] = ENTRY(iCtr,cDataString,"|").
        END CASE.
    END. 
    IF NUM-ENTRIES(cDataString,"|") > 132 THEN ASSIGN
        fiRptName:{&SV} = ENTRY(131,cDataString,"|")
        fiLockedCols:{&SV} = ENTRY(132,cDataString,"|")
        fiPagesWide:{&SV} = ENTRY(133,cDataString,"|").
    
    IF NUM-ENTRIES(cDataString,"|") > 133 THEN RUN ipLoadttColumns.

    IF cbAnd-1:{&SV} = "" THEN ASSIGN cbAnd-1:{&SV} = " ".
    IF cbAnd-2:{&SV} = "" THEN ASSIGN cbAnd-2:{&SV} = " ".
    IF cbAnd-3:{&SV} = "" THEN ASSIGN cbAnd-3:{&SV} = " ".
    IF cbAnd-4:{&SV} = "" THEN ASSIGN cbAnd-4:{&SV} = " ".
    IF cbAnd-6:{&SV} = "" THEN ASSIGN cbAnd-6:{&SV} = " ".
    IF cbAnd-7:{&SV} = "" THEN ASSIGN cbAnd-7:{&SV} = " ".
    IF cbAnd-8:{&SV} = "" THEN ASSIGN cbAnd-8:{&SV} = " ".
    IF cbAnd-9:{&SV} = "" THEN ASSIGN cbAnd-9:{&SV} = " ".
    IF cbAnd-11:{&SV} = "" THEN ASSIGN cbAnd-11:{&SV} = " ".
    IF cbAnd-12:{&SV} = "" THEN ASSIGN cbAnd-12:{&SV} = " ".
    IF cbAnd-13:{&SV} = "" THEN ASSIGN cbAnd-13:{&SV} = " ".
    IF cbAnd-14:{&SV} = "" THEN ASSIGN cbAnd-14:{&SV} = " ".
    IF cbAnd-16:{&SV} = "" THEN ASSIGN cbAnd-16:{&SV} = " ".
    IF cbAnd-17:{&SV} = "" THEN ASSIGN cbAnd-17:{&SV} = " ".
    IF cbAnd-18:{&SV} = "" THEN ASSIGN cbAnd-18:{&SV} = " ".
    IF cbAnd-19:{&SV} = "" THEN ASSIGN cbAnd-19:{&SV} = " ".
    APPLY 'value-changed' TO cbWhere-1.
    APPLY 'value-changed' TO cbAnd-1.
    APPLY 'value-changed' TO cbWhere-2.
    APPLY 'value-changed' TO cbAnd-2.
    APPLY 'value-changed' TO cbWhere-3.
    APPLY 'value-changed' TO cbAnd-3.
    APPLY 'value-changed' TO cbWhere-4.
    APPLY 'value-changed' TO cbAnd-4.
    APPLY 'value-changed' TO cbWhere-5.
    IF cbFile-2:{&SV} <> ? THEN DO:
        ASSIGN
            tMore-1:CHECKED = TRUE.
        APPLY 'value-changed' TO tMore-1.
    END.
    APPLY 'value-changed' TO cbWhere-6.
    APPLY 'value-changed' TO cbAnd-6.
    APPLY 'value-changed' TO cbWhere-7.
    APPLY 'value-changed' TO cbAnd-7.
    APPLY 'value-changed' TO cbWhere-8.
    APPLY 'value-changed' TO cbAnd-8.
    APPLY 'value-changed' TO cbWhere-9.
    APPLY 'value-changed' TO cbAnd-9.
    APPLY 'value-changed' TO cbWhere-10.
    IF cbFile-3:{&SV} <> ? THEN DO:
        ASSIGN
            tMore-2:CHECKED = TRUE.
        APPLY 'value-changed' TO tMore-2.
    END.
    APPLY 'value-changed' TO cbWhere-11.
    APPLY 'value-changed' TO cbAnd-11.
    APPLY 'value-changed' TO cbWhere-12.
    APPLY 'value-changed' TO cbAnd-12.
    APPLY 'value-changed' TO cbWhere-13.
    APPLY 'value-changed' TO cbAnd-13.
    APPLY 'value-changed' TO cbWhere-14.
    APPLY 'value-changed' TO cbAnd-14.
    APPLY 'value-changed' TO cbWhere-15.
    IF cbFile-4:{&SV} <> ? THEN DO:
        ASSIGN
            tMore-3:CHECKED = TRUE.
        APPLY 'value-changed' TO tMore-3.
    END.
    APPLY 'value-changed' TO cbWhere-16.
    APPLY 'value-changed' TO cbAnd-16.
    APPLY 'value-changed' TO cbWhere-17.
    APPLY 'value-changed' TO cbAnd-17.
    APPLY 'value-changed' TO cbWhere-18.
    APPLY 'value-changed' TO cbAnd-18.
    APPLY 'value-changed' TO cbWhere-19.
    APPLY 'value-changed' TO cbAnd-19.
    APPLY 'value-changed' TO cbWhere-20.
    ASSIGN 
        lLoading = FALSE.
    RUN ipShowQuery IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipLoadttColumns wWin 
PROCEDURE ipLoadttColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iRow AS INT NO-UNDO.
    DEF VAR iCol AS INT NO-UNDO.

    DO iCtr = 134 TO NUM-ENTRIES(cDataString,"|"):
        IF INDEX(ENTRY(iCtr,cDataString,"|"),"`") <> 0 THEN DO:
            ASSIGN
                cTempString = ENTRY(iCtr,cDataString,"|").
            CREATE ttColumns.
            ASSIGN
                ttColumns.cColLabel = ENTRY(1,cTempString,"`")
                ttColumns.cColType = ENTRY(2,cTempString,"`")
                ttColumns.cColumn = ENTRY(3,cTempString,"`")
                ttColumns.cDataType = ENTRY(4,cTempString,"`")
                ttColumns.cDispLabel = ENTRY(5,cTempString,"`")
                ttColumns.cField = ENTRY(6,cTempString,"`")
                ttColumns.cFormat = ENTRY(7,cTempString,"`")
                ttColumns.cFormula = ENTRY(8,cTempString,"`")
                ttColumns.cInitVal = ENTRY(9,cTempString,"`")
                ttColumns.cLabel = ENTRY(10,cTempString,"`")
                ttColumns.cTable = ENTRY(11,cTempString,"`")
                ttColumns.iBand = INTEGER(ENTRY(12,cTempString,"`"))
                ttColumns.iColumn = INTEGER(ENTRY(13,cTempString,"`"))
                ttColumns.iExtent = INTEGER(ENTRY(14,cTempString,"`"))
                ttColumns.iIdx = INTEGER(ENTRY(15,cTempString,"`"))
                ttColumns.iPos = INTEGER(ENTRY(16,cTempString,"`"))
                ttColumns.iRow = INTEGER(ENTRY(17,cTempString,"`")).
        END.
    END.

    FOR EACH ttColumns BY ttColumns.iPos:
        slDisplayFields:ADD-LAST(ttColumns.cDispLabel) {&IN}.
        IF ttColumns.cColType = "DATA" THEN ASSIGN
            cFullFieldList = cFullFieldList + ttColumns.cTable + "." + ttColumns.cField + (IF ttColumns.iExtent <> 0 THEN "[" + STRING(ttColumns.iExtent) + "]" ELSE "") + ",".
        ELSE DO:
            CREATE ttCalcCols.
            ASSIGN
                ttCalcCols.cColLabel = ttColumns.cColLabel
                ttCalcCols.cDataType = ttColumns.cDataType
                ttCalcCols.cFormat = ttColumns.cFormat
                ttCalcCols.cFormula = ttColumns.cFormula
                ttCalcCols.cInitVal = ttColumns.cInitVal
                ttCalcCols.iPos = ttColumns.iPos
                ttCalcCols.iIdx = ttColumns.iIdx.
        END.
    END.
    
    ASSIGN
        iRow = 1
        iCol = 1
        cFullFieldList = "".
    FOR EACH ttColumns BY ttColumns.iPos:
        ASSIGN
            ttColumns.iRow = iRow
            ttColumns.iCol = iCol
            ttColumns.cColumn = ENTRY(iCol,cColList)
            iCol = iCol + 1.
        IF ttColumns.cColType = "SKIP" 
        OR ttColumns.cColLabel = "<skip>" THEN ASSIGN
            iRow = iRow + 1
            iCol = 1.
        IF ttColumns.cColType = "DATA" THEN ASSIGN
            cFullFieldList = cFullFieldList + ttColumns.cTable + "." + ttColumns.cField + (IF ttColumns.iExtent <> 0 THEN "[" + STRING(ttColumns.iExtent) + "]" ELSE "") + ",".
    END.
    ASSIGN
        cFullFieldList = TRIM(cFullFieldList,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipProcessQuery wWin 
PROCEDURE ipProcessQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR hf AS HANDLE NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iSrcCol AS INT NO-UNDO.
    DEF VAR iTgtCol AS INT NO-UNDO.
    DEF VAR cSrcList AS CHAR NO-UNDO.
    DEF VAR cTgtList AS CHAR NO-UNDO.
    DEF VAR cTmpLabel AS CHAR NO-UNDO.
    DEF VAR iStart AS INT NO-UNDO.
    DEF VAR iEnd AS INT NO-UNDO.
    DEF VAR cTmpStr AS CHAR NO-UNDO.
    DEF VAR cNewLabel AS CHAR NO-UNDO.
    DEF VAR iExtents AS INT NO-UNDO.
    DEF VAR iLast AS INT NO-UNDO.
    DEF VAR cTempFieldName AS CHAR NO-UNDO.
    DEF VAR cTempExtent AS CHAR NO-UNDO.
    DO iCtr = 1 TO 20:
        IF VALID-HANDLE(hCalcCol[iCtr]) THEN DELETE WIDGET hCalcCol[iCtr].
    END.

    hQuery:QUERY-OPEN.
    CREATE BROWSE hBrowse
        ASSIGN 
        FRAME = FRAME f-Main:HANDLE
        SENSITIVE = TRUE
        SEPARATORS = TRUE
        ROW-MARKERS = FALSE
        COLUMN-RESIZABLE = TRUE
        COLUMN-MOVABLE = TRUE 
        ALLOW-COLUMN-SEARCHING = TRUE
        MULTIPLE = TRUE
        QUERY = hQuery
        COL = 101
        ROW = 24.75
        WIDTH = 59
        HEIGHT = 7.5    
        VISIBLE = TRUE
        NO-VALIDATE = TRUE
        TRIGGERS:
        END TRIGGERS.
        . 

    ASSIGN
        iCtr = 1.
    FOR EACH ttColumns BY ttColumns.iPos:
        IF ttColumns.cTable <> "" THEN DO:
            ASSIGN
                cTempExtent = IF ttColumns.iExtent = 0 THEN "" ELSE "[" + STRING(ttColumns.iExtent) + "]".
            hBrowse:ADD-LIKE-COLUMN(ttColumns.cTable + "." + ttColumns.cField + cTempExtent).
        END.
        ELSE DO:
            hCalcCol[iCtr] = hBrowse:ADD-CALC-COLUMN(ttColumns.cDataType,
                                                     ttColumns.cFormat,
                                                     ttColumns.cInitVal,
                                                     ttColumns.cColLabel,
                                                     ttColumns.iPos).
            iCtr = iCtr + 1.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSaveFile wWin 
PROCEDURE ipSaveFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cHowSave AS CHAR NO-UNDO.
    
    DEF VAR cDataString AS CHAR NO-UNDO.
    DEF VAR cCalcCol AS CHAR NO-UNDO.

    IF fiRptName:{&SV} = "" THEN DO:
        MESSAGE
            "You are saving this report without a report name." SKIP
            "Would you like to add a report name now?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lName AS LOG.
        IF lName THEN DO:
            APPLY 'entry' TO fiRptName.
            RETURN.
        END.
    END.

    /* Note: this order is for PROGRAMMER's conventienc, nothing else */
    ASSIGN cDataString = 
        (IF cbFile-1:{&SV} <> ? THEN cbFile-1:{&SV} ELSE "") + "|" +
        (IF cbFile-2:{&SV} <> ? THEN cbFile-2:{&SV} ELSE "") + "|" +
        (IF cbFile-3:{&SV} <> ? THEN cbFile-3:{&SV} ELSE "") + "|" +
        (IF cbFile-4:{&SV} <> ? THEN cbFile-4:{&SV} ELSE "") + "|".
    ASSIGN cDataString = cDataString +
        (IF cbOf-2:{&SV} <> ? THEN cbOf-2:{&SV} ELSE "") + "|" +
        (IF cbOf-3:{&SV} <> ? THEN cbOf-3:{&SV} ELSE "") + "|" +
        (IF cbOf-4:{&SV} <> ? THEN cbOf-4:{&SV} ELSE "") + "|".
    ASSIGN cDataString = cDataString +
        (IF cbRel-2:{&SV} <> ? THEN cbRel-2:{&SV} ELSE "") + "|" +
        (IF cbRel-3:{&SV} <> ? THEN cbRel-3:{&SV} ELSE "") + "|" +
        (IF cbRel-4:{&SV} <> ? THEN cbRel-4:{&SV} ELSE "") + "|".
    ASSIGN cDataString = cDataString +
        (IF cbWhere-1:{&SV} <> ? THEN cbWhere-1:{&SV} ELSE "") + "|" +
        (IF cbWhere-2:{&SV} <> ? THEN cbWhere-2:{&SV} ELSE "") + "|" +
        (IF cbWhere-3:{&SV} <> ? THEN cbWhere-3:{&SV} ELSE "") + "|" +
        (IF cbWhere-4:{&SV} <> ? THEN cbWhere-4:{&SV} ELSE "") + "|" +
        (IF cbWhere-5:{&SV} <> ? THEN cbWhere-5:{&SV} ELSE "") + "|" +
        (IF cbWhere-6:{&SV} <> ? THEN cbWhere-6:{&SV} ELSE "") + "|" +
        (IF cbWhere-7:{&SV} <> ? THEN cbWhere-7:{&SV} ELSE "") + "|" +
        (IF cbWhere-8:{&SV} <> ? THEN cbWhere-8:{&SV} ELSE "") + "|" +
        (IF cbWhere-9:{&SV} <> ? THEN cbWhere-9:{&SV} ELSE "") + "|" +
        (IF cbWhere-10:{&SV} <> ? THEN cbWhere-10:{&SV} ELSE "") + "|" +
        (IF cbWhere-11:{&SV} <> ? THEN cbWhere-11:{&SV} ELSE "") + "|" +
        (IF cbWhere-12:{&SV} <> ? THEN cbWhere-12:{&SV} ELSE "") + "|" +
        (IF cbWhere-13:{&SV} <> ? THEN cbWhere-13:{&SV} ELSE "") + "|" +
        (IF cbWhere-14:{&SV} <> ? THEN cbWhere-14:{&SV} ELSE "") + "|" +
        (IF cbWhere-15:{&SV} <> ? THEN cbWhere-15:{&SV} ELSE "") + "|" +
        (IF cbWhere-16:{&SV} <> ? THEN cbWhere-16:{&SV} ELSE "") + "|" +
        (IF cbWhere-17:{&SV} <> ? THEN cbWhere-17:{&SV} ELSE "") + "|" +
        (IF cbWhere-18:{&SV} <> ? THEN cbWhere-18:{&SV} ELSE "") + "|" +
        (IF cbWhere-19:{&SV} <> ? THEN cbWhere-19:{&SV} ELSE "") + "|" +
        (IF cbWhere-20:{&SV} <> ? THEN cbWhere-20:{&SV} ELSE "") + "|".

    ASSIGN cDataString = cDataString +
        (IF cbOp-1:{&SV} <> ? THEN cbOp-1:{&SV} ELSE "") + "|" +
        (IF cbOp-2:{&SV} <> ? THEN cbOp-2:{&SV} ELSE "") + "|" +
        (IF cbOp-3:{&SV} <> ? THEN cbOp-3:{&SV} ELSE "") + "|" +
        (IF cbOp-4:{&SV} <> ? THEN cbOp-4:{&SV} ELSE "") + "|" +
        (IF cbOp-5:{&SV} <> ? THEN cbOp-5:{&SV} ELSE "") + "|" +
        (IF cbOp-6:{&SV} <> ? THEN cbOp-6:{&SV} ELSE "") + "|" +
        (IF cbOp-7:{&SV} <> ? THEN cbOp-7:{&SV} ELSE "") + "|" +
        (IF cbOp-8:{&SV} <> ? THEN cbOp-8:{&SV} ELSE "") + "|" +
        (IF cbOp-9:{&SV} <> ? THEN cbOp-9:{&SV} ELSE "") + "|" +
        (IF cbOp-10:{&SV} <> ? THEN cbOp-10:{&SV} ELSE "") + "|" +
        (IF cbOp-11:{&SV} <> ? THEN cbOp-11:{&SV} ELSE "") + "|" +
        (IF cbOp-12:{&SV} <> ? THEN cbOp-12:{&SV} ELSE "") + "|" +
        (IF cbOp-13:{&SV} <> ? THEN cbOp-13:{&SV} ELSE "") + "|" +
        (IF cbOp-14:{&SV} <> ? THEN cbOp-14:{&SV} ELSE "") + "|" +
        (IF cbOp-15:{&SV} <> ? THEN cbOp-15:{&SV} ELSE "") + "|" +
        (IF cbOp-16:{&SV} <> ? THEN cbOp-16:{&SV} ELSE "") + "|" +
        (IF cbOp-17:{&SV} <> ? THEN cbOp-17:{&SV} ELSE "") + "|" +
        (IF cbOp-18:{&SV} <> ? THEN cbOp-18:{&SV} ELSE "") + "|" +
        (IF cbOp-19:{&SV} <> ? THEN cbOp-19:{&SV} ELSE "") + "|" +
        (IF cbOp-20:{&SV} <> ? THEN cbOp-20:{&SV} ELSE "") + "|".
.
    ASSIGN cDataString = cDataString +
        (IF fiVal-1:{&SV} <> ? THEN fiVal-1:{&SV} ELSE "") + "|" +
        (IF fiVal-2:{&SV} <> ? THEN fiVal-2:{&SV} ELSE "") + "|" +
        (IF fiVal-3:{&SV} <> ? THEN fiVal-3:{&SV} ELSE "") + "|" +
        (IF fiVal-4:{&SV} <> ? THEN fiVal-4:{&SV} ELSE "") + "|" +
        (IF fiVal-5:{&SV} <> ? THEN fiVal-5:{&SV} ELSE "") + "|" +
        (IF fiVal-6:{&SV} <> ? THEN fiVal-6:{&SV} ELSE "") + "|" +
        (IF fiVal-7:{&SV} <> ? THEN fiVal-7:{&SV} ELSE "") + "|" +
        (IF fiVal-8:{&SV} <> ? THEN fiVal-8:{&SV} ELSE "") + "|" +
        (IF fiVal-9:{&SV} <> ? THEN fiVal-9:{&SV} ELSE "") + "|" +
        (IF fiVal-10:{&SV} <> ? THEN fiVal-10:{&SV} ELSE "") + "|" +
        (IF fiVal-11:{&SV} <> ? THEN fiVal-11:{&SV} ELSE "") + "|" +
        (IF fiVal-12:{&SV} <> ? THEN fiVal-12:{&SV} ELSE "") + "|" +
        (IF fiVal-13:{&SV} <> ? THEN fiVal-13:{&SV} ELSE "") + "|" +
        (IF fiVal-14:{&SV} <> ? THEN fiVal-14:{&SV} ELSE "") + "|" +
        (IF fiVal-15:{&SV} <> ? THEN fiVal-15:{&SV} ELSE "") + "|" +
        (IF fiVal-16:{&SV} <> ? THEN fiVal-16:{&SV} ELSE "") + "|" +
        (IF fiVal-17:{&SV} <> ? THEN fiVal-17:{&SV} ELSE "") + "|" +
        (IF fiVal-18:{&SV} <> ? THEN fiVal-18:{&SV} ELSE "") + "|" +
        (IF fiVal-19:{&SV} <> ? THEN fiVal-19:{&SV} ELSE "") + "|" +
        (IF fiVal-20:{&SV} <> ? THEN fiVal-20:{&SV} ELSE "") + "|".
.
    ASSIGN cDataString = cDataString +
        (IF cbAnd-1:{&SV} <> ? THEN cbAnd-1:{&SV} ELSE "") + "|" +
        (IF cbAnd-2:{&SV} <> ? THEN cbAnd-2:{&SV} ELSE "") + "|" +
        (IF cbAnd-3:{&SV} <> ? THEN cbAnd-3:{&SV} ELSE "") + "|" +
        (IF cbAnd-4:{&SV} <> ? THEN cbAnd-4:{&SV} ELSE "") + "|" +
        (IF cbAnd-6:{&SV} <> ? THEN cbAnd-6:{&SV} ELSE "") + "|" +
        (IF cbAnd-7:{&SV} <> ? THEN cbAnd-7:{&SV} ELSE "") + "|" +
        (IF cbAnd-8:{&SV} <> ? THEN cbAnd-8:{&SV} ELSE "") + "|" +
        (IF cbAnd-9:{&SV} <> ? THEN cbAnd-9:{&SV} ELSE "") + "|" +
        (IF cbAnd-11:{&SV} <> ? THEN cbAnd-11:{&SV} ELSE "") + "|" +
        (IF cbAnd-12:{&SV} <> ? THEN cbAnd-12:{&SV} ELSE "") + "|" +
        (IF cbAnd-13:{&SV} <> ? THEN cbAnd-13:{&SV} ELSE "") + "|" +
        (IF cbAnd-14:{&SV} <> ? THEN cbAnd-14:{&SV} ELSE "") + "|" +
        (IF cbAnd-16:{&SV} <> ? THEN cbAnd-16:{&SV} ELSE "") + "|" +
        (IF cbAnd-17:{&SV} <> ? THEN cbAnd-17:{&SV} ELSE "") + "|" +
        (IF cbAnd-18:{&SV} <> ? THEN cbAnd-18:{&SV} ELSE "") + "|" +
        (IF cbAnd-19:{&SV} <> ? THEN cbAnd-19:{&SV} ELSE "") + "|".
.
    ASSIGN cDataString = cDataString +
        (IF tOpenParen-1:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-2:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-3:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-4:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-5:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-6:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-7:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-8:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-9:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-10:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-11:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-12:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-13:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-14:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-15:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-16:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-17:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-18:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-19:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tOpenParen-20:CHECKED THEN "Y" ELSE "N") + "|".
.
    ASSIGN cDataString = cDataString +
        (IF tCloseParen-1:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-2:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-3:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-4:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-5:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-6:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-7:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-8:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-9:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-10:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-11:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-12:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-13:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-14:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-15:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-16:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-17:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-18:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-19:CHECKED THEN "Y" ELSE "N") + "|" +
        (IF tCloseParen-20:CHECKED THEN "Y" ELSE "N") + "|".
    
    ASSIGN cDataString = cDataString + 
        (IF cFieldList[1] <> ? THEN cFieldList[1] ELSE "") + "|" +
        (IF cFieldList[2] <> ? THEN cFieldList[2] ELSE "") + "|" +
        (IF cFieldList[3] <> ? THEN cFieldList[3] ELSE "") + "|" +
        (IF cFieldList[4] <> ? THEN cFieldList[4] ELSE "") + "|".
    
    ASSIGN cDataString = cDataString + 
        fiRptName:{&SV} + "|" + 
        fiLockedCols:{&SV} + "|" +
        fiPagesWide:{&SV} + "|".

    FOR EACH ttColumns:
        ASSIGN 
            cCalcCol = ttColumns.cColLabel + "`" + 
                       ttColumns.cColType + "`" + 
                       ttColumns.cColumn + "`" + 
                       ttColumns.cDataType + "`" + 
                       ttColumns.cDispLabel + "`" + 
                       ttColumns.cField + "`" + 
                       ttColumns.cFormat + "`" + 
                       ttColumns.cFormula + "`" + 
                       ttColumns.cInitVal + "`" + 
                       ttColumns.cLabel + "`" + 
                       ttColumns.cTable + "`" + 
                       STRING(ttColumns.iBand) + "`" + 
                       STRING(ttColumns.iColumn) + "`" + 
                       STRING(ttColumns.iExtent) + "`" + 
                       STRING(ttColumns.iIdx) + "`" + 
                       STRING(ttColumns.iPos) + "`" + 
                       STRING(ttColumns.iRow) 
            cDataString = cDataString + cCalcCol + "|".
    END.
        
    /* Mark the end of file */
    ASSIGN cDataString = cDataString + CHR(10).

    IF cHowSave = "SaveAs" OR cFileName = "" THEN DO:
        ASSIGN
            cFileName = fiRptName:{&SV} + " (" + STRING(YEAR(TODAY),"9999") + "_" +
                                                 STRING(MONTH(TODAY),"99") + "_" +
                                                 STRING(DAY(TODAY),"99") + ")" + ".qry".
        SYSTEM-DIALOG GET-FILE cFileName
        TITLE "Save this Query..."
        FILTERS "Query Files (*.qry)"  "*.qry"
        USE-FILENAME
        SAVE-AS
        DEFAULT-EXTENSION ".qry"
        INITIAL-DIR "..\Reports\".
    END.

    OUTPUT TO VALUE(cFileName).
    PUT UNFORMATTED cDataString.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetDisplay wWin 
PROCEDURE ipSetDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cTempFieldName AS CHAR NO-UNDO.
    DEF VAR iExtents AS INT NO-UNDO.
    DEF VAR iLast AS INT NO-UNDO.
    DEF VAR cTemp AS CHAR NO-UNDO.
    DEF VAR tTempPos AS INT NO-UNDO.
    DEF VAR cReservedPos AS CHAR NO-UNDO.
    DEF VAR iRow AS INT NO-UNDO.
    DEF VAR iCol AS INT NO-UNDO.
    
    EMPTY TEMP-TABLE ttColumns.

    FOR EACH ttCalcCols:
        ASSIGN cReservedPos = cReservedPos + STRING(ttCalcCols.iPos) + ",".
    END.
    ASSIGN cReservedPos = TRIM(cReservedPos,",").

    ASSIGN
        tTempPos = 1
        slDisplayFields:LIST-ITEMS {&IN} = "".
    DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
        ASSIGN
            tTempPos = tTempPos + 1.
    END.

    IF cbFile-1:{&SV} <> ? 
    THEN DO:
        &SCOPED-DEFINE iSet 1
        FIND _file WHERE
            _file._file-name = cbFile-{&iSet}:{&SV}
            NO-LOCK NO-ERROR.
        DO iCtr = 1 TO NUM-ENTRIES(cFieldList[{&iSet}]):
            FIND _field OF _file WHERE
                _field._field-name = SUBSTRING(ENTRY(iCtr,cFieldList[{&iSet}]),1,INDEX(ENTRY(iCtr,cFieldList[{&iSet}]),"[") - 1)
                NO-LOCK NO-ERROR.
            IF AVAIL _field THEN DO:
                IF _field._extent < 2 THEN DO:
                    DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                        ASSIGN
                            tTempPos = tTempPos + 1.
                    END.
                    CREATE ttColumns.
                    ASSIGN
                        ttColumns.cColLabel = _field._label
                        ttColumns.cColType = "DATA"
                        ttColumns.cColumn = ""
                        ttColumns.cDataType = _field._data-type
                        ttColumns.cDispLabel = _field._label + " (" + _file._file-name + ")"
                        ttColumns.cField = _field._field-name
                        ttColumns.cFormat = _field._format
                        ttColumns.cFormula = ""
                        ttColumns.cInitVal = ""
                        ttColumns.cLabel = _field._label
                        ttColumns.cTable = _file._file-name
                        ttColumns.iBand = 1    
                        ttColumns.iColumn = 0    
                        ttColumns.iExtent = 0    
                        ttColumns.iIdx = tTempPos    
                        ttColumns.iPos = tTempPos
                        ttColumns.iRow = 1
                        tTempPos = tTempPos + 1
                        .
                END.
                ELSE DO jCtr = 1 TO _field._extent:
                    ASSIGN
                        cTemp = _field._field-name + "[" + STRING(jCtr) + "]".
                    IF CAN-DO(cFieldList[{&iSet}],cTemp) 
                    AND NOT CAN-FIND(FIRST ttColumns WHERE
                        ttColumns.cTable = _file._file-name AND
                        ttColumns.cField = _field._field-name AND
                        ttColumns.cLabel = _field._label AND
                        ttColumns.iExtent = jCtr)
                    THEN DO:
                        DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                            ASSIGN
                                tTempPos = tTempPos + 1.
                        END.
                        CREATE ttColumns.
                        ASSIGN
                            ttColumns.cColLabel = _field._label + "[" + STRING(jCtr) + "]"
                            ttColumns.cColType = "DATA"
                            ttColumns.cColumn = ""
                            ttColumns.cDataType = _field._data-type
                            ttColumns.cDispLabel = _field._label + "[" + STRING(jCtr) + "]" + " (" + _file._file-name + ")"
                            ttColumns.cField = _field._field-name
                            ttColumns.cFormat = _field._format
                            ttColumns.cFormula = ""
                            ttColumns.cInitVal = ""
                            ttColumns.cLabel = _field._label
                            ttColumns.cTable = _file._file-name
                            ttColumns.iBand = 1    
                            ttColumns.iColumn = 0    
                            ttColumns.iExtent = jCtr    
                            ttColumns.iIdx = tTempPos    
                            ttColumns.iPos = tTempPos
                            ttColumns.iRow = 1
                            tTempPos = tTempPos + 1
                            .
                    END.
                END.
            END.
        END.
    END.
    IF cbFile-2:{&SV} <> ? 
    AND cbFile-2:{&SV} <> cbFile-1:{&SV} 
    THEN DO:
        &SCOPED-DEFINE iSet 2
        FIND _file WHERE
            _file._file-name = cbFile-{&iSet}:{&SV}
            NO-LOCK NO-ERROR.
        DO iCtr = 1 TO NUM-ENTRIES(cFieldList[{&iSet}]):
            FIND _field OF _file WHERE
                _field._field-name = SUBSTRING(ENTRY(iCtr,cFieldList[{&iSet}]),1,INDEX(ENTRY(iCtr,cFieldList[{&iSet}]),"[") - 1)
                NO-LOCK NO-ERROR.
            IF AVAIL _field THEN DO:
                IF _field._extent < 2 THEN DO:
                    DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                        ASSIGN
                            tTempPos = tTempPos + 1.
                    END.
                    CREATE ttColumns.
                    ASSIGN
                        ttColumns.cColLabel = _field._label
                        ttColumns.cColType = "DATA"
                        ttColumns.cColumn = ""
                        ttColumns.cDataType = _field._data-type
                        ttColumns.cDispLabel = _field._label + " (" + _file._file-name + ")"
                        ttColumns.cField = _field._field-name
                        ttColumns.cFormat = _field._format
                        ttColumns.cFormula = ""
                        ttColumns.cInitVal = ""
                        ttColumns.cLabel = _field._label
                        ttColumns.cTable = _file._file-name
                        ttColumns.iBand = 1    
                        ttColumns.iColumn = 0    
                        ttColumns.iExtent = 0    
                        ttColumns.iIdx = tTempPos    
                        ttColumns.iPos = tTempPos
                        ttColumns.iRow = 1
                        tTempPos = tTempPos + 1
                        .
                END.
                ELSE DO jCtr = 1 TO _field._extent:
                    ASSIGN
                        cTemp = _field._field-name + "[" + STRING(jCtr) + "]".
                    IF CAN-DO(cFieldList[{&iSet}],cTemp) 
                    AND NOT CAN-FIND(FIRST ttColumns WHERE
                        ttColumns.cTable = _file._file-name AND
                        ttColumns.cField = _field._field-name AND
                        ttColumns.cLabel = _field._label AND
                        ttColumns.iExtent = jCtr)
                    THEN DO:
                        DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                            ASSIGN
                                tTempPos = tTempPos + 1.
                        END.
                        CREATE ttColumns.
                        ASSIGN
                            ttColumns.cColLabel = _field._label + "[" + STRING(jCtr) + "]"
                            ttColumns.cColType = "DATA"
                            ttColumns.cColumn = ""
                            ttColumns.cDataType = _field._data-type
                            ttColumns.cDispLabel = _field._label + "[" + STRING(jCtr) + "]" + " (" + _file._file-name + ")"
                            ttColumns.cField = _field._field-name
                            ttColumns.cFormat = _field._format
                            ttColumns.cFormula = ""
                            ttColumns.cInitVal = ""
                            ttColumns.cLabel = _field._label
                            ttColumns.cTable = _file._file-name
                            ttColumns.iBand = 1    
                            ttColumns.iColumn = 0    
                            ttColumns.iExtent = jCtr    
                            ttColumns.iIdx = tTempPos    
                            ttColumns.iPos = tTempPos
                            ttColumns.iRow = 1
                            tTempPos = tTempPos + 1
                            .
                    END.
                END.
            END.
        END.
    END.
    IF cbFile-3:{&SV} <> ? 
    AND cbFile-3:{&SV} <> cbFile-1:{&SV}
    AND cbFile-3:{&SV} <> cbFile-2:{&SV}
    THEN DO:
        &SCOPED-DEFINE iSet 3
        FIND _file WHERE
            _file._file-name = cbFile-{&iSet}:{&SV}
            NO-LOCK NO-ERROR.
        DO iCtr = 1 TO NUM-ENTRIES(cFieldList[{&iSet}]):
            FIND _field OF _file WHERE
                _field._field-name = SUBSTRING(ENTRY(iCtr,cFieldList[{&iSet}]),1,INDEX(ENTRY(iCtr,cFieldList[{&iSet}]),"[") - 1)
                NO-LOCK NO-ERROR.
            IF AVAIL _field THEN DO:
                IF _field._extent < 2 THEN DO:
                    DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                        ASSIGN
                            tTempPos = tTempPos + 1.
                    END.
                    CREATE ttColumns.
                    ASSIGN
                        ttColumns.cColLabel = _field._label
                        ttColumns.cColType = "DATA"
                        ttColumns.cColumn = ""
                        ttColumns.cDataType = _field._data-type
                        ttColumns.cDispLabel = _field._label + " (" + _file._file-name + ")"
                        ttColumns.cField = _field._field-name
                        ttColumns.cFormat = _field._format
                        ttColumns.cFormula = ""
                        ttColumns.cInitVal = ""
                        ttColumns.cLabel = _field._label
                        ttColumns.cTable = _file._file-name
                        ttColumns.iBand = 1    
                        ttColumns.iColumn = 0    
                        ttColumns.iExtent = 0    
                        ttColumns.iIdx = tTempPos    
                        ttColumns.iPos = tTempPos
                        ttColumns.iRow = 1
                        tTempPos = tTempPos + 1
                        .
                END.
                ELSE DO jCtr = 1 TO _field._extent:
                    ASSIGN
                        cTemp = _field._field-name + "[" + STRING(jCtr) + "]".
                    IF CAN-DO(cFieldList[{&iSet}],cTemp) 
                    AND NOT CAN-FIND(FIRST ttColumns WHERE
                        ttColumns.cTable = _file._file-name AND
                        ttColumns.cField = _field._field-name AND
                        ttColumns.cLabel = _field._label AND
                        ttColumns.iExtent = jCtr)
                    THEN DO:
                        DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                            ASSIGN
                                tTempPos = tTempPos + 1.
                        END.
                        CREATE ttColumns.
                        ASSIGN
                            ttColumns.cColLabel = _field._label + "[" + STRING(jCtr) + "]"
                            ttColumns.cColType = "DATA"
                            ttColumns.cColumn = ""
                            ttColumns.cDataType = _field._data-type
                            ttColumns.cDispLabel = _field._label + "[" + STRING(jCtr) + "]" + " (" + _file._file-name + ")"
                            ttColumns.cField = _field._field-name
                            ttColumns.cFormat = _field._format
                            ttColumns.cFormula = ""
                            ttColumns.cInitVal = ""
                            ttColumns.cLabel = _field._label
                            ttColumns.cTable = _file._file-name
                            ttColumns.iBand = 1    
                            ttColumns.iColumn = 0    
                            ttColumns.iExtent = jCtr    
                            ttColumns.iIdx = tTempPos    
                            ttColumns.iPos = tTempPos
                            ttColumns.iRow = 1
                            tTempPos = tTempPos + 1
                            .
                    END.
                END.
            END.
        END.
    END.
    IF cbFile-4:{&SV} <> ? 
    AND cbFile-4:{&SV} <> cbFile-1:{&SV}
    AND cbFile-4:{&SV} <> cbFile-2:{&SV}
    AND cbFile-4:{&SV} <> cbFile-3:{&SV}
    THEN DO:
        &SCOPED-DEFINE iSet 4
        FIND _file WHERE
            _file._file-name = cbFile-{&iSet}:{&SV}
            NO-LOCK NO-ERROR.
        DO iCtr = 1 TO NUM-ENTRIES(cFieldList[{&iSet}]):
            FIND _field OF _file WHERE
                _field._field-name = SUBSTRING(ENTRY(iCtr,cFieldList[{&iSet}]),1,INDEX(ENTRY(iCtr,cFieldList[{&iSet}]),"[") - 1)
                NO-LOCK NO-ERROR.
            IF AVAIL _field THEN DO:
                IF _field._extent < 2 THEN DO:
                    DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                        ASSIGN
                            tTempPos = tTempPos + 1.
                    END.
                    CREATE ttColumns.
                    ASSIGN
                        ttColumns.cColLabel = _field._label
                        ttColumns.cColType = "DATA"
                        ttColumns.cColumn = ""
                        ttColumns.cDataType = _field._data-type
                        ttColumns.cDispLabel = _field._label + " (" + _file._file-name + ")"
                        ttColumns.cField = _field._field-name
                        ttColumns.cFormat = _field._format
                        ttColumns.cFormula = ""
                        ttColumns.cInitVal = ""
                        ttColumns.cLabel = _field._label
                        ttColumns.cTable = _file._file-name
                        ttColumns.iBand = 1    
                        ttColumns.iColumn = 0    
                        ttColumns.iExtent = 0    
                        ttColumns.iIdx = tTempPos    
                        ttColumns.iPos = tTempPos
                        ttColumns.iRow = 1
                        tTempPos = tTempPos + 1
                        .
                END.
                ELSE DO jCtr = 1 TO _field._extent:
                    ASSIGN
                        cTemp = _field._field-name + "[" + STRING(jCtr) + "]".
                    IF CAN-DO(cFieldList[{&iSet}],cTemp) 
                    AND NOT CAN-FIND(FIRST ttColumns WHERE
                        ttColumns.cTable = _file._file-name AND
                        ttColumns.cField = _field._field-name AND
                        ttColumns.cLabel = _field._label AND
                        ttColumns.iExtent = jCtr)
                    THEN DO:
                        DO WHILE CAN-DO(cReservedPos,STRING(tTempPos)):
                            ASSIGN
                                tTempPos = tTempPos + 1.
                        END.
                        CREATE ttColumns.
                        ASSIGN
                            ttColumns.cColLabel = _field._label + "[" + STRING(jCtr) + "]"
                            ttColumns.cColType = "DATA"
                            ttColumns.cColumn = ""
                            ttColumns.cDataType = _field._data-type
                            ttColumns.cDispLabel = _field._label + "[" + STRING(jCtr) + "]" + " (" + _file._file-name + ")"
                            ttColumns.cField = _field._field-name
                            ttColumns.cFormat = _field._format
                            ttColumns.cFormula = ""
                            ttColumns.cInitVal = ""
                            ttColumns.cLabel = _field._label
                            ttColumns.cTable = _file._file-name
                            ttColumns.iBand = 1    
                            ttColumns.iColumn = 0    
                            ttColumns.iExtent = jCtr    
                            ttColumns.iIdx = tTempPos    
                            ttColumns.iPos = tTempPos
                            ttColumns.iRow = 1
                            tTempPos = tTempPos + 1
                            .
                    END.
                END.
            END.
        END.
    END.
    
    FOR EACH ttCalcCols:
        CREATE ttColumns.
        ASSIGN
            ttColumns.cColLabel = ttCalcCols.cColLabel
            ttColumns.cColType = IF ttCalcCols.cFormula <> "" THEN "CALC" ELSE "SKIP"
            ttColumns.cColumn = ""
            ttColumns.cDataType = ttCalcCols.cDataType
            ttColumns.cDispLabel = IF ttColumns.cColType = "CALC" THEN ttCalcCols.cColLabel + " (CALC)" ELSE "<skip>"
            ttColumns.cField = ""
            ttColumns.cFormat = ttCalcCols.cFormat
            ttColumns.cFormula = ttCalcCols.cFormula
            ttColumns.cInitVal = ttCalcCols.cInitVal
            ttColumns.cLabel = ttCalcCols.cColLabel
            ttColumns.cTable = ""
            ttColumns.iBand = 1    
            ttColumns.iColumn = 0    
            ttColumns.iExtent = 0
            ttColumns.iIdx = ttCalcCols.iPos
            ttColumns.iPos = ttCalcCols.iPos
            ttColumns.iRow = 1
            .
    END.

    ASSIGN
        iRow = 1
        iCol = 1
        slDisplayFields:LIST-ITEMS = "".
    FOR EACH ttColumns BY ttColumns.iPos:
        slDisplayFields:ADD-LAST(ttColumns.cDispLabel).
        ASSIGN
            ttColumns.iRow = iRow
            ttColumns.iCol = iCol
            ttColumns.cColumn = ENTRY(iCol,cColList)
            iCol = iCol + 1.
        IF ttColumns.cColType = "SKIP" THEN ASSIGN
            iRow = iRow + 1
            iCol = 1.
        IF ttColumns.cColType = "DATA" THEN ASSIGN
            cFullFieldList = cFullFieldList + ttColumns.cTable + "." + ttColumns.cField + (IF ttColumns.iExtent <> 0 THEN "[" + STRING(ttColumns.iExtent) + "]" ELSE "") + ",".
    END.
    ASSIGN
        cFullFieldList = TRIM(cFullFieldList,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipShowQuery wWin 
PROCEDURE ipShowQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lField AS LOG.

    ASSIGN
        eQuery:{&SV} = "FOR EACH " + cbFile-1:{&SV} + " NO-LOCK ".

    &SCOPED-DEFINE iFileLevel 1
    
    &SCOPED-DEFINE iFieldLevel 1
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 2
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 3
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 4
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 5
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
    END.
    
    IF tMore-1:CHECKED THEN DO:
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + ", EACH ".
        IF cbFile-2:{&SV} <> "" 
        AND cbFile-2:{&SV} <> cbFile-1:{&SV} THEN ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + cbFile-2:{&SV} + 
                           (IF cbOf-2:{&SV} = "OF" THEN (" OF " + cbRel-2:{&SV} + " NO-LOCK ") ELSE " NO-LOCK ").
    END.
    ELSE RETURN.

    &SCOPED-DEFINE iFileLevel 2
    
    &SCOPED-DEFINE iFieldLevel 6
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 7
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 8
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 9
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 10
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
    END.

    IF tMore-2:CHECKED THEN DO:
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + ", EACH ".
        IF cbFile-3:{&SV} <> "" 
        AND cbFile-3:{&SV} <> cbFile-1:{&SV} 
        AND cbFile-3:{&SV} <> cbFile-2:{&SV} THEN ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + cbFile-3:{&SV} + 
                           (IF cbOf-3:{&SV} = "OF" THEN (" OF " + cbRel-3:{&SV} + " NO-LOCK ") ELSE " NO-LOCK ").
    END.
    ELSE RETURN.

    RUN ipShowQuery2 IN THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipShowQuery2 wWin 
PROCEDURE ipShowQuery2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR lField AS LOG.

    &SCOPED-DEFINE iFileLevel 3
    
    &SCOPED-DEFINE iFieldLevel 11
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 12
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 13
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 14
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 15
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + 
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
    END.
    
    IF tMore-3:CHECKED THEN DO:
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + ", EACH ".
        IF cbFile-4:{&SV} <> "" 
        AND cbFile-4:{&SV} <> cbFile-1:{&SV} 
        AND cbFile-4:{&SV} <> cbFile-2:{&SV} 
        AND cbFile-4:{&SV} <> cbFile-3:{&SV} THEN ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + cbFile-4:{&SV} + 
                           (IF cbOf-4:{&SV} = "OF" THEN (" OF " + cbRel-4:{&SV} + " NO-LOCK ") ELSE " NO-LOCK ").
    END.
    ELSE RETURN.

    &SCOPED-DEFINE iFileLevel 4
    
    &SCOPED-DEFINE iFieldLevel 16
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 17
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 18
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 19
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
        ASSIGN 
            eQuery:{&SV} = eQuery:{&SV} + (IF cbAnd-{&iFieldLevel}:{&SV} <> ? AND cbAnd-{&iFieldLevel}:{&SV} <> "" THEN " " + cbAnd-{&iFieldLevel}:{&SV} + " " ELSE "").
    END.
    &SCOPED-DEFINE iFieldLevel 20
    IF fiVal-{&iFieldLevel}:{&SV} <> "" THEN DO:
        IF NUM-ENTRIES(fiVal-{&iFieldLevel}:{&SV},".") > 1
        AND CAN-DO(cFileList,ENTRY(1,fiVal-{&iFieldLevel}:{&SV},".")) THEN ASSIGN
            lField = TRUE.
        ASSIGN
            eQuery:{&SV} = eQuery:{&SV} + "WHERE " +
            (IF tOpenParen-{&iFieldLevel}:CHECKED THEN "(" ELSE "") +
            cbFile-{&iFileLevel}:{&SV} + "." + cbWhere-{&iFieldLevel}:{&SV} + " " +
            cbOp-{&iFieldLevel}:{&SV} + " " +
            (IF cDataType[{&iFieldLevel}] = "CHARACTER" AND NOT lField THEN quoter(fiVal-{&iFieldLevel}:{&SV}) ELSE fiVal-{&iFieldLevel}:{&SV}) + 
            (IF tCloseParen-{&iFieldLevel}:CHECKED THEN ")" ELSE "").
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

