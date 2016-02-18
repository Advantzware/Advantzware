&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-artioscad.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE var opCADCAM AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompany AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE opCADCAM AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttblCADCAM NO-UNDO
  FIELD board LIKE ef.board
  FIELD boardID AS INTEGER
  FIELD cad-no LIKE eb.cad-no
  FIELD cal LIKE ef.cal
  FIELD dep LIKE eb.dep
  FIELD die-no LIKE eb.die-no
  FIELD len LIKE eb.len
  FIELD lin-in LIKE eb.lin-in
  FIELD style LIKE eb.style
  FIELD t-len LIKE eb.t-len
  FIELD t-sqin LIKE eb.t-sqin
  FIELD t-wid LIKE eb.t-wid
  FIELD weight LIKE ef.weight
  FIELD wid LIKE eb.wid.

{est/artiosvar.i "shared"}


def temp-table tt-CompStyle NO-UNDO
    field form-num as int
    field style as cha
    FIELD pur-man AS LOG
    field blank-num as int
    field NumOfComponents as int
    field compRatio as dec extent 30
    field compStyle as cha extent 30
    field compQty as int extent 30
    field compNumUp as int extent 30
    FIELD compPurMan AS LOG EXTENT 10.
                                 
{sys/inc/var.i shared}
{custom/gcompany.i}  
gcompany = cocode.

do transaction:
  {sys/inc/artioscad.i}
end.

def var iFormNumber as int no-undo.
def var iBlankNumber as int no-undo.
def var iNumofCADForm as int no-undo.
def var iProjectCount as int init 50 no-undo.

def temp-table tt-SubDir NO-UNDO field DirName as cha.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cadPath btnCADPathLookup cadNumber ~
btnCAD#Lookup iSetQty cCategory cb-CadSeq cStyle rs-man-pur iNumOfParts ~
dRatio-1 cStyleComp-1 iCompQty-1 iNumUp-1 rs-man-pur-1 dRatio-2 ~
cStyleComp-2 iCompQty-2 iNumUp-2 rs-man-pur-2 dRatio-3 cStyleComp-3 ~
iCompQty-3 iNumUp-3 rs-man-pur-3 dRatio-4 cStyleComp-4 iCompQty-4 iNumUp-4 ~
rs-man-pur-4 dRatio-5 cStyleComp-5 iCompQty-5 iNumUp-5 rs-man-pur-5 ~
dRatio-6 cStyleComp-6 iCompQty-6 iNumUp-6 rs-man-pur-6 dRatio-7 ~
cStyleComp-7 iCompQty-7 iNumUp-7 rs-man-pur-7 dRatio-8 cStyleComp-8 ~
iCompQty-8 iNumUp-8 rs-man-pur-8 dRatio-9 cStyleComp-9 iCompQty-9 iNumUp-9 ~
rs-man-pur-9 dRatio-10 cStyleComp-10 iCompQty-10 iNumUp-10 rs-man-pur-10 ~
Btn_OK Btn_Cancel RECT-1 
&Scoped-Define DISPLAYED-OBJECTS cadPath cadNumber iSetQty cCategory ~
cb-CadSeq vNumOfCADForm cStyle rs-man-pur iNumOfParts dRatio-1 cStyleComp-1 ~
iCompQty-1 iNumUp-1 rs-man-pur-1 dRatio-2 cStyleComp-2 iCompQty-2 iNumUp-2 ~
rs-man-pur-2 dRatio-3 cStyleComp-3 iCompQty-3 iNumUp-3 rs-man-pur-3 ~
dRatio-4 cStyleComp-4 iCompQty-4 iNumUp-4 rs-man-pur-4 dRatio-5 ~
cStyleComp-5 iCompQty-5 iNumUp-5 rs-man-pur-5 dRatio-6 cStyleComp-6 ~
iCompQty-6 iNumUp-6 rs-man-pur-6 dRatio-7 cStyleComp-7 iCompQty-7 iNumUp-7 ~
rs-man-pur-7 dRatio-8 cStyleComp-8 iCompQty-8 iNumUp-8 rs-man-pur-8 ~
dRatio-9 cStyleComp-9 iCompQty-9 iNumUp-9 rs-man-pur-9 dRatio-10 ~
cStyleComp-10 iCompQty-10 iNumUp-10 rs-man-pur-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCAD#Lookup 
     IMAGE-UP FILE "images/find.bmp":U
     LABEL "" 
     SIZE 4.33 BY 1.39.

DEFINE BUTTON btnCADPathLookup 
     IMAGE-UP FILE "images/find.bmp":U
     LABEL "" 
     SIZE 4.33 BY 1.39.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.72
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.72
     BGCOLOR 8 .

DEFINE VARIABLE cb-CadSeq AS CHARACTER FORMAT "X(256)":U 
     LABEL "CAD Form#" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE cadNumber AS CHARACTER FORMAT "x(200)" 
     LABEL "CAD File/Project #" 
     VIEW-AS FILL-IN 
     SIZE 77.33 BY 1.44 NO-UNDO.

DEFINE VARIABLE cadPath AS CHARACTER FORMAT "x(200)" 
     LABEL "CAD File Path" 
     VIEW-AS FILL-IN 
     SIZE 77.33 BY 1.44 NO-UNDO.

DEFINE VARIABLE cCategory AS CHARACTER FORMAT "X(5)":U 
     LABEL "Category" 
     VIEW-AS FILL-IN 
     SIZE 11.83 BY 1.39 NO-UNDO.

DEFINE VARIABLE cStyle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Style" 
     VIEW-AS FILL-IN 
     SIZE 18.83 BY 1.28 NO-UNDO.

DEFINE VARIABLE cStyleComp-1 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-10 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-2 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-3 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-4 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-5 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-6 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-7 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-8 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE cStyleComp-9 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 15.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-1 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-10 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-2 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-3 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-4 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-5 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-6 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-7 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-8 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE dRatio-9 AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-1 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-10 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-5 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-6 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-7 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-8 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iCompQty-9 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE iNumOfParts AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "# of Components" 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1.28 NO-UNDO.

DEFINE VARIABLE iNumUp-1 AS INTEGER FORMAT ">>9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-10 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-3 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-4 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-5 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-6 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-7 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-8 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iNumUp-9 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.67 BY 1 NO-UNDO.

DEFINE VARIABLE iSetQty AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Estimate Qty" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.39 NO-UNDO.

DEFINE VARIABLE vNumOfCADForm AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Manufactured", "M",
"Purchased", "P"
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-1 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-10 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-2 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-3 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-4 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-5 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-6 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-7 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-8 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE VARIABLE rs-man-pur-9 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M", "M",
"P", "P"
     SIZE 15.33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 12.78.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cadPath AT ROW 1.28 COL 18.67 COLON-ALIGNED WIDGET-ID 114
     btnCADPathLookup AT ROW 1.28 COL 98 WIDGET-ID 116
     cadNumber AT ROW 2.94 COL 18.67 COLON-ALIGNED
     btnCAD#Lookup AT ROW 2.94 COL 98.17 WIDGET-ID 4
     iSetQty AT ROW 4.61 COL 18.67 COLON-ALIGNED WIDGET-ID 42
     cCategory AT ROW 6.11 COL 18.67 COLON-ALIGNED WIDGET-ID 88
     cb-CadSeq AT ROW 8.39 COL 19 COLON-ALIGNED WIDGET-ID 84
     vNumOfCADForm AT ROW 8.39 COL 51.67 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     cStyle AT ROW 9.89 COL 19 COLON-ALIGNED WIDGET-ID 6
     rs-man-pur AT ROW 10.06 COL 42.83 NO-LABEL WIDGET-ID 118
     iNumOfParts AT ROW 11.56 COL 19 COLON-ALIGNED WIDGET-ID 8
     dRatio-1 AT ROW 14.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cStyleComp-1 AT ROW 14.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     iCompQty-1 AT ROW 14.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     iNumUp-1 AT ROW 14.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     rs-man-pur-1 AT ROW 14.67 COL 81.67 NO-LABEL WIDGET-ID 122
     dRatio-2 AT ROW 15.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     cStyleComp-2 AT ROW 15.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     iCompQty-2 AT ROW 15.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     iNumUp-2 AT ROW 15.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     rs-man-pur-2 AT ROW 15.67 COL 81.67 NO-LABEL WIDGET-ID 126
     dRatio-3 AT ROW 16.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     cStyleComp-3 AT ROW 16.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     iCompQty-3 AT ROW 16.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     iNumUp-3 AT ROW 16.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     rs-man-pur-3 AT ROW 16.67 COL 81.67 NO-LABEL WIDGET-ID 130
     dRatio-4 AT ROW 17.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     cStyleComp-4 AT ROW 17.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     iCompQty-4 AT ROW 17.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     iNumUp-4 AT ROW 17.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     rs-man-pur-4 AT ROW 17.67 COL 81.67 NO-LABEL WIDGET-ID 134
     dRatio-5 AT ROW 18.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     cStyleComp-5 AT ROW 18.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     iCompQty-5 AT ROW 18.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     iNumUp-5 AT ROW 18.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     rs-man-pur-5 AT ROW 18.67 COL 81.67 NO-LABEL WIDGET-ID 138
     dRatio-6 AT ROW 19.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     cStyleComp-6 AT ROW 19.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     iCompQty-6 AT ROW 19.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     iNumUp-6 AT ROW 19.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     rs-man-pur-6 AT ROW 19.67 COL 81.67 NO-LABEL WIDGET-ID 142
     dRatio-7 AT ROW 20.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     cStyleComp-7 AT ROW 20.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     iCompQty-7 AT ROW 20.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     iNumUp-7 AT ROW 20.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     rs-man-pur-7 AT ROW 20.67 COL 81.67 NO-LABEL WIDGET-ID 146
     dRatio-8 AT ROW 21.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 26
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     cStyleComp-8 AT ROW 21.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     iCompQty-8 AT ROW 21.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     iNumUp-8 AT ROW 21.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     rs-man-pur-8 AT ROW 21.67 COL 81.67 NO-LABEL WIDGET-ID 150
     dRatio-9 AT ROW 22.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     cStyleComp-9 AT ROW 22.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     iCompQty-9 AT ROW 22.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     iNumUp-9 AT ROW 22.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 110
     rs-man-pur-9 AT ROW 22.67 COL 81.67 NO-LABEL WIDGET-ID 154
     dRatio-10 AT ROW 23.61 COL 7.17 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     cStyleComp-10 AT ROW 23.61 COL 23.83 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     iCompQty-10 AT ROW 23.61 COL 47.17 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     iNumUp-10 AT ROW 23.61 COL 66.83 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     rs-man-pur-10 AT ROW 23.67 COL 81.67 NO-LABEL WIDGET-ID 158
     Btn_OK AT ROW 26.89 COL 26
     Btn_Cancel AT ROW 26.89 COL 63
     "Qty" VIEW-AS TEXT
          SIZE 5 BY 1.11 AT ROW 13.5 COL 53.83 WIDGET-ID 58
     "Num Up" VIEW-AS TEXT
          SIZE 8.33 BY 1.11 AT ROW 13.5 COL 69.83 WIDGET-ID 94
     "Style" VIEW-AS TEXT
          SIZE 6 BY 1.11 AT ROW 13.5 COL 31.17 WIDGET-ID 56
     "================================================================================" VIEW-AS TEXT
          SIZE 81.67 BY .61 AT ROW 7.67 COL 19 WIDGET-ID 40
     "Components %" VIEW-AS TEXT
          SIZE 16 BY 1.11 AT ROW 13.5 COL 9.17 WIDGET-ID 54
     RECT-1 AT ROW 13.17 COL 5 WIDGET-ID 82
     SPACE(2.16) SKIP(4.54)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Create Estimate from CADCAM Software"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vNumOfCADForm IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

