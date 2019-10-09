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
DEFINE VARIABLE gchCadX AS COM-HANDLE      NO-UNDO.

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

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCAD#Lookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.

DEFINE BUTTON btnCADPathLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.

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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Estimate from CADCAM Software */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCAD#Lookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCAD#Lookup D-Dialog
ON CHOOSE OF btnCAD#Lookup IN FRAME D-Dialog
DO:
    def var cCadFile as cha no-undo.
    
    def var okClicked as log no-undo.
    
    SYSTEM-DIALOG GET-FILE cCadFile 
                TITLE 'Select Artios CAD File to insert'
                FILTERS 'ARD Files    (*.ard)' '*.ard'
                INITIAL-DIR artioscad-chr
                MUST-EXIST USE-FILENAME UPDATE okClicked.
  

  IF okClicked THEN
     ASSIGN cadNumber:screen-value = cCadFile.
     
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCADPathLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCADPathLookup D-Dialog
ON CHOOSE OF btnCADPathLookup IN FRAME D-Dialog
DO:
   def var cCadFilepath as cha no-undo.
   def var okClicked as log no-undo.
    
   SYSTEM-DIALOG GET-dir cCadFilePath 
       TITLE 'Select Artios CAD File Path to insert'
       INITIAL-DIR artioscad-chr
       UPDATE okClicked.
   
   IF okClicked THEN
      cadpath:screen-value = cCadFilePath.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  opCADCAM = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  def var iExt as int no-undo. 
  def var cArtiosCadFile as cha no-undo.
  def var iRevision as int init 65 no-undo.
  def var iRevExt as int no-undo.
  
  RUN CadXConnect.
  assign iFormNumber = 0
         iBlankNumber = 0
         cadNumber cStyle iNumOfParts cCategory rs-man-pur iSetQty
             /* dRatio-1 dRatio-2
              dRatio-3 dRatio-4 dRatio-5 dRatio-6 dRatio-7 dRatio-8 dRatio-9*/
              .

  if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\".
  if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and
     substring(artioscad-chr,length(artioscad-chr),1) <> "\" then
     artioscad-chr = artioscad-chr + "\".
  
  if index(cadNumber,"\") > 0 or index(cadNumber,"/") > 0 or
     index(cadNumber,"ARD") > 0 then cArtiosCadFile = cadNumber.
  else    cArtiosCadFile = artioscad-chr + cadnumber + ".ard".
 
  session:set-wait-state("general").
  
  run AssignCADFormInfo.
  
  /* run getSubDirList.   run from leave of cadnumber */
  
  if search(cArtiosCadFile ) <> ? then do:  /* import single CAD file */
     run create-ttCad (cArtiosCadFile).
  end.
  else do iExt = 1 to iProjectCount: 
    /* import Project CAD file ###### + %%(2 digit extension) +  @ (1 character revision) */
    
     for each tt-SubDir :    
       if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ) <> ? then do:       
          
          run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + ".ARD" ).               
       end.
        /* check revision file */
       do iRevExt = 65 to 90:
     
          if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ? 
             then  run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt) + ".ARD" ).               
          else if search(tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ? 
             then  run create-ttCad (tt-SubDir.DirName + cadNumber + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ).  
       end.
     end.  /* each tt-SubDir */
                 
  end. 
  session:set-wait-state("").
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cadNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cadNumber D-Dialog
ON LEAVE OF cadNumber IN FRAME D-Dialog /* CAD File/Project # */
DO:
    def var icnt as int no-undo.
    
    ASSIGN cadNumber cadPath .
    run getNumofCADForm (output iNumofCADForm).

    /* if iNumofCADForm > 1 then do icnt = 2 to iNumofCadForm: */
/*        cb-CADSeq:add-last(string(icnt)). */
/*     end. */
    
    vNumOfCADForm:screen-value  = "Total Number of CAD File: " + string(iNumofCadForm).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cadPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cadPath D-Dialog
ON LEAVE OF cadPath IN FRAME D-Dialog /* CAD File Path */
DO:
    def var icnt as int no-undo.
    
    ASSIGN cadNumber .
    run getNumofCADForm (output iNumofCADForm).

    /* if iNumofCADForm > 1 then do icnt = 2 to iNumofCadForm: */
/*        cb-CADSeq:add-last(string(icnt)). */
/*     end. */
    
    vNumOfCADForm:screen-value  = "Total Number of CAD File: " + string(iNumofCadForm).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-CadSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-CadSeq D-Dialog
ON VALUE-CHANGED OF cb-CadSeq IN FRAME D-Dialog /* CAD Form# */
DO:
     
     run displayCadFormInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCategory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON HELP OF cCategory IN FRAME D-Dialog /* Category */
DO:
   def var char-val as cha no-undo. 

   run windows/l-fgcat.w (gcompany,self:screen-value,output char-val).
   if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         self:screen-value  = entry(1,char-val).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCategory D-Dialog
ON LEAVE OF cCategory IN FRAME D-Dialog /* Category */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-category(OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyle D-Dialog
ON HELP OF cStyle IN FRAME D-Dialog /* Style */
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyle D-Dialog
ON LEAVE OF cStyle IN FRAME D-Dialog /* Style */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT cstyle:HANDLE,
                   INPUT YES,
                   OUTPUT op-error).

   IF NOT op-error THEN
      assign cStyleComp-1:screen-value in frame {&frame-name} = cStyle:screen-value
             cStyle.
   ELSE
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-1 D-Dialog
ON HELP OF cStyleComp-1 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-1 D-Dialog
ON LEAVE OF cStyleComp-1 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-10 D-Dialog
ON HELP OF cStyleComp-10 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-10 D-Dialog
ON LEAVE OF cStyleComp-10 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-2 D-Dialog
ON HELP OF cStyleComp-2 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-2 D-Dialog
ON LEAVE OF cStyleComp-2 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-3 D-Dialog
ON HELP OF cStyleComp-3 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-3 D-Dialog
ON LEAVE OF cStyleComp-3 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-4 D-Dialog
ON HELP OF cStyleComp-4 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-4 D-Dialog
ON LEAVE OF cStyleComp-4 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-5 D-Dialog
ON HELP OF cStyleComp-5 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-5 D-Dialog
ON LEAVE OF cStyleComp-5 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-6 D-Dialog
ON HELP OF cStyleComp-6 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-6 D-Dialog
ON LEAVE OF cStyleComp-6 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-7 D-Dialog
ON HELP OF cStyleComp-7 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-7 D-Dialog
ON LEAVE OF cStyleComp-7 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-8 D-Dialog
ON HELP OF cStyleComp-8 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-8 D-Dialog
ON LEAVE OF cStyleComp-8 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStyleComp-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-9 D-Dialog
ON HELP OF cStyleComp-9 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStyleComp-9 D-Dialog
ON LEAVE OF cStyleComp-9 IN FRAME D-Dialog
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   RUN valid-style(INPUT SELF:HANDLE,
                   INPUT NO,
                   OUTPUT op-error).

   IF op-error THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-1 D-Dialog
ON HELP OF iCompQty-1 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-1 D-Dialog
ON LEAVE OF iCompQty-1 IN FRAME D-Dialog
DO:
      /* assign cStyleComp-1:screen-value in frame {&frame-name} = cStyle:screen-value */
/*                  cStyleComp-2:screen-value in frame {&frame-name} = cStyle:screen-value */
/*                 cStyleComp-3:screen-value in frame {&frame-name} = cStyle:screen-value */
/*                 cStyleComp-4:screen-value in frame {&frame-name} = cStyle:screen-value */
/*                 cStyleComp-5:screen-value in frame {&frame-name} = cStyle:screen-value */
/*                   . */
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-10 D-Dialog
ON HELP OF iCompQty-10 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-2 D-Dialog
ON HELP OF iCompQty-2 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-3 D-Dialog
ON HELP OF iCompQty-3 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-4 D-Dialog
ON HELP OF iCompQty-4 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-5 D-Dialog
ON HELP OF iCompQty-5 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-6 D-Dialog
ON HELP OF iCompQty-6 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-7 D-Dialog
ON HELP OF iCompQty-7 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-8 D-Dialog
ON HELP OF iCompQty-8 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iCompQty-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iCompQty-9 D-Dialog
ON HELP OF iCompQty-9 IN FRAME D-Dialog
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iNumOfParts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iNumOfParts D-Dialog
ON LEAVE OF iNumOfParts IN FRAME D-Dialog /* # of Components */
DO:
     def var dTmpValue as dec no-undo.
     def var dTmpQty as int no-undo.
     
     assign dRatio-1 = 100
            dRatio-2 = 0
            dRatio-3 = 0
            dRatio-4 = 0
            dRatio-5 = 0
            dRatio-6 = 0
            dRatio-7 = 0
            dRatio-8 = 0
            dRatio-9 = 0
            dRatio-10 = 0
            iCompQty-1 = 1
            iCompQty-2 = 0
            iCompQty-3 = 0
            iCompQty-4 = 0
            iCompQty-5 = 0
            iCompQty-6 = 0
            iCompQty-7 = 0
            iCompQty-8 = 0
            iCompQty-9 = 0
            iCompQty-10 = 0
            cStyleComp-1 = cStyle
            cStyleComp-2 = ""
            cStyleComp-3 = ""
            cStyleComp-4 = ""
            cStyleComp-5 = ""
            cStyleComp-6 = ""
            cStyleComp-7 = ""
            cStyleComp-8 = ""
            cStyleComp-9 = ""
            cStyleComp-10 = ""
            iNumUp-1 = 1
            iNumUp-2 = 0
            iNumUp-3 = 0
            iNumUp-4 = 0
            iNumUp-5 = 0
            iNumUp-6 = 0
            iNumUp-7 = 0
            iNumUp-8 = 0
            iNumUp-9 = 0
            iNumUp-10 = 0
            rs-man-pur
            rs-man-pur-1 = "M"
            rs-man-pur-2 = "M"
            rs-man-pur-3 = "M"
            rs-man-pur-4 = "M"
            rs-man-pur-5 = "M"
            rs-man-pur-6 = "M"
            rs-man-pur-7 = "M"
            rs-man-pur-8 = "M"
            rs-man-pur-9 = "M"
            rs-man-pur-10 = "M".
                
     assign iNumOfParts iSetQty.
     if iNumOfParts = 1 then
        assign dRatio-1 = 100
               iCompQty-1 = 1
               cStyleComp-1 = cStyle
               rs-man-pur-1 = rs-man-pur.
     
     if iNumofParts > 1 then do:
        ASSIGN
           dTmpValue = 100 / iNumOfParts
           dTmpQty = iSetQty / iNumOfParts.
        
        case iNumOfParts:
           when 1 then assign dRatio-1 = 100
                              cStyleComp-1 = cStyle
                              iCompQty-1 = 1
                              iNumUp-1 = 1
                              rs-man-pur-1 = rs-man-pur.

           when 2 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur.
           when 3 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur.
                                        
           when 4 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue                             
                              dRatio-4 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              cStyleComp-4 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iCompQty-4 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              iNumUp-4 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur
                              rs-man-pur-4 = rs-man-pur.
           when 5 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue                             
                              dRatio-4 = dTmpValue
                              dRatio-5 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              cStyleComp-4 = cStyle
                              cStyleComp-5 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iCompQty-4 = 1
                              iCompQty-5 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              iNumUp-4 = 1
                              iNumUp-5 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur
                              rs-man-pur-4 = rs-man-pur
                              rs-man-pur-5 = rs-man-pur.
           when 6 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue                             
                              dRatio-4 = dTmpValue
                              dRatio-5 = dTmpValue
                              dRatio-6 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              cStyleComp-4 = cStyle
                              cStyleComp-5 = cStyle
                              cStyleComp-6 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iCompQty-4 = 1
                              iCompQty-5 = 1
                              iCompQty-6 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              iNumUp-4 = 1
                              iNumUp-5 = 1
                              iNumUp-6 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur
                              rs-man-pur-4 = rs-man-pur
                              rs-man-pur-5 = rs-man-pur
                              rs-man-pur-6 = rs-man-pur.
           when 7 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue                             
                              dRatio-4 = dTmpValue
                              dRatio-5 = dTmpValue
                              dRatio-6 = dTmpValue
                              dRatio-7 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              cStyleComp-4 = cStyle
                              cStyleComp-5 = cStyle
                              cStyleComp-6 = cStyle
                              cStyleComp-7 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iCompQty-4 = 1
                              iCompQty-5 = 1
                              iCompQty-6 = 1
                              iCompQty-7 = 1
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              iNumUp-4 = 1
                              iNumUp-5 = 1
                              iNumUp-6 = 1
                              iNumUp-7 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur
                              rs-man-pur-4 = rs-man-pur
                              rs-man-pur-5 = rs-man-pur
                              rs-man-pur-6 = rs-man-pur
                              rs-man-pur-7 = rs-man-pur.
                                        
           when 8 then assign dRatio-1 = dTmpValue
                              dRatio-2 = dTmpValue
                              dRatio-3 = dTmpValue                             
                              dRatio-4 = dTmpValue
                              dRatio-5 = dTmpValue
                              dRatio-6 = dTmpValue
                              dRatio-7 = dTmpValue
                              dRatio-8 = dTmpValue
                              cStyleComp-1 = cStyle
                              cStyleComp-2 = cStyle
                              cStyleComp-3 = cStyle
                              cStyleComp-4 = cStyle
                              cStyleComp-5 = cStyle
                              cStyleComp-6 = cStyle
                              cStyleComp-7 = cStyle
                              cStyleComp-8 = cStyle
                              iCompQty-1 = 1
                              iCompQty-2 = 1
                              iCompQty-3 = 1
                              iCompQty-4 = 1
                              iCompQty-5 = 1
                              iCompQty-6 = 1
                              iCompQty-7 = 1
                              iCompQty-8 = 1                                        
                              iNumUp-1 = 1
                              iNumUp-2 = 1
                              iNumUp-3 = 1
                              iNumUp-4 = 1
                              iNumUp-5 = 1
                              iNumUp-6 = 1
                              iNumUp-7 = 1
                              iNumUp-8 = 1
                              rs-man-pur-1 = rs-man-pur
                              rs-man-pur-2 = rs-man-pur
                              rs-man-pur-3 = rs-man-pur
                              rs-man-pur-4 = rs-man-pur
                              rs-man-pur-5 = rs-man-pur
                              rs-man-pur-6 = rs-man-pur
                              rs-man-pur-7 = rs-man-pur
                              rs-man-pur-8 = rs-man-pur.
                                        
            when 9 then assign dRatio-1 = dTmpValue
                               dRatio-2 = dTmpValue
                               dRatio-3 = dTmpValue                             
                               dRatio-4 = dTmpValue
                               dRatio-5 = dTmpValue
                               dRatio-6 = dTmpValue
                               dRatio-7 = dTmpValue
                               dRatio-8 = dTmpValue
                               dRatio-9 = dTmpValue
                                cStyleComp-1 = cStyle
                               cStyleComp-2 = cStyle
                               cStyleComp-3 = cStyle
                               cStyleComp-4 = cStyle
                               cStyleComp-5 = cStyle
                               cStyleComp-6 = cStyle
                               cStyleComp-7 = cStyle
                               cStyleComp-8 = cStyle
                               cStyleComp-9 = cStyle
                               iCompQty-1 = 1
                               iCompQty-2 = 1
                               iCompQty-3 = 1
                               iCompQty-4 = 1
                               iCompQty-5 = 1
                               iCompQty-6 = 1
                               iCompQty-7 = 1
                               iCompQty-8 = 1
                               iCompQty-9 = 1
                               iNumUp-1 = 1
                               iNumUp-2 = 1
                               iNumUp-3 = 1
                               iNumUp-4 = 1
                               iNumUp-5 = 1
                               iNumUp-6 = 1
                               iNumUp-7 = 1
                               iNumUp-8 = 1
                               iNumUp-9 = 1
                               rs-man-pur-1 = rs-man-pur
                               rs-man-pur-2 = rs-man-pur
                               rs-man-pur-3 = rs-man-pur
                               rs-man-pur-4 = rs-man-pur
                               rs-man-pur-5 = rs-man-pur
                               rs-man-pur-6 = rs-man-pur
                               rs-man-pur-7 = rs-man-pur
                               rs-man-pur-8 = rs-man-pur
                               rs-man-pur-9 = rs-man-pur.

            when 10 then assign dRatio-1 = dTmpValue
                                dRatio-2 = dTmpValue
                                dRatio-3 = dTmpValue                             
                                dRatio-4 = dTmpValue
                                dRatio-5 = dTmpValue
                                dRatio-6 = dTmpValue
                                dRatio-7 = dTmpValue
                                dRatio-8 = dTmpValue
                                dRatio-9 = dTmpValue
                                dRatio-10 = dTmpValue
                                cStyleComp-1 = cStyle
                                cStyleComp-2 = cStyle
                                cStyleComp-3 = cStyle
                                cStyleComp-4 = cStyle
                                cStyleComp-5 = cStyle
                                cStyleComp-6 = cStyle
                                cStyleComp-7 = cStyle
                                cStyleComp-8 = cStyle
                                cStyleComp-9 = cStyle
                                cStyleComp-10 = cStyle
                                iCompQty-1 = 1
                                iCompQty-2 = 1
                                iCompQty-3 = 1
                                iCompQty-4 = 1
                                iCompQty-5 = 1
                                iCompQty-6 = 1
                                iCompQty-7 = 1
                                iCompQty-8 = 1
                                iCompQty-9 = 1
                                iCompQty-10 = 1
                                iNumUp-1 = 1
                                iNumUp-2 = 1
                                iNumUp-3 = 1
                                iNumUp-4 = 1
                                iNumUp-5 = 1
                                iNumUp-6 = 1
                                iNumUp-7 = 1
                                iNumUp-8 = 1
                                iNumUp-9 = 1
                                iNumUp-10 = 1
                                rs-man-pur-1 = rs-man-pur
                                rs-man-pur-2 = rs-man-pur
                                rs-man-pur-3 = rs-man-pur
                                rs-man-pur-4 = rs-man-pur
                                rs-man-pur-5 = rs-man-pur
                                rs-man-pur-6 = rs-man-pur
                                rs-man-pur-7 = rs-man-pur
                                rs-man-pur-8 = rs-man-pur
                                rs-man-pur-9 = rs-man-pur
                                rs-man-pur-10 = rs-man-pur.
        end.
           
           
       /*  display dRatio-1 dRatio-2 dRatio-3 dRatio-4 dRatio-5 dRatio-6 dRatio-7 dRatio-8 dRatio-9 dRatio-10 */
/*                    cStyleComp-1 cStyleComp-2 cStyleComp-3 cStyleComp-4 cStyleComp-5 */
/*                    cStyleComp-6 cStyleComp-7 cStyleComp-8 cStyleComp-9 cStyleComp-10 */
/*                    iCompQty-1 iCompQty-2 iCompQty-3 iCompQty-4 iCompQty-5 */
/*                    iCompQty-6 iCompQty-7 iCompQty-8 iCompQty-9 iCompQty-10 */
/*                 with frame {&frame-name}. */
                
/*         run createObjects.       */  
     end.
     
      display dRatio-1 dRatio-2 dRatio-3 dRatio-4 dRatio-5 dRatio-6 dRatio-7 dRatio-8 dRatio-9 dRatio-10
                   cStyleComp-1 cStyleComp-2 cStyleComp-3 cStyleComp-4 cStyleComp-5
                   cStyleComp-6 cStyleComp-7 cStyleComp-8 cStyleComp-9 cStyleComp-10
                   iCompQty-1 iCompQty-2 iCompQty-3 iCompQty-4 iCompQty-5
                   iCompQty-6 iCompQty-7 iCompQty-8 iCompQty-9 iCompQty-10
                   iNumUp-1 iNumUp-2 iNumUp-3 iNumUp-4 iNumUp-5
                   iNumUp-6 iNumUp-7 iNumUp-8 iNumUp-9 iNumUp-10
                   rs-man-pur-1 rs-man-pur-2 rs-man-pur-3 rs-man-pur-4 rs-man-pur-5
                   rs-man-pur-6 rs-man-pur-7 rs-man-pur-8 rs-man-pur-9 rs-man-pur-10
                with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iSetQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iSetQty D-Dialog
ON HELP OF iSetQty IN FRAME D-Dialog /* Estimate Qty */
DO:
     def var char-val as cha no-undo.
           run windows/l-stylec.w (gcompany,cStyle:screen-value, output char-val).
           if char-val <> "" and self:screen-value <> entry(1,char-val) then 
                         cStyle:screen-value  = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iSetQty D-Dialog
ON LEAVE OF iSetQty IN FRAME D-Dialog /* Estimate Qty */
DO:
      /* assign iCompQty-1:screen-value in frame {&frame-name} = iSetQty:screen-value */
/*                  . */
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-man-pur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-man-pur D-Dialog
ON VALUE-CHANGED OF rs-man-pur IN FRAME D-Dialog
DO:
   assign rs-man-pur-1:screen-value in frame {&frame-name} = rs-man-pur:screen-value
          rs-man-pur .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignCADFormInfo D-Dialog 
PROCEDURE assignCADFormInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-CompStyle for tt-CompStyle.

  do with frame {&frame-name}:
    find first bf-CompStyle where bf-CompStyle.form-num = int(substring(cb-CADSeq:screen-value,1,2)) no-error.
    if not avail bf-CompStyle then do:
       create bf-CompStyle.
       assign bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)).
    end.
   
    assign bf-CompStyle.Style = cStyle:screen-value in frame {&frame-name}
           bf-CompStyle.pur-man = IF rs-man-pur:SCREEN-VALUE EQ "M" THEN NO ELSE YES
           bf-CompStyle.NumOfComponents = int(iNumofParts:screen-value)
           bf-CompStyle.compRatio[1] = dec(dRatio-1:screen-value)
           bf-CompStyle.compRatio[2] = dec(dRatio-2:screen-value)
           bf-CompStyle.compRatio[3] = dec(dRatio-3:screen-value)
           bf-CompStyle.compRatio[4] = dec(dRatio-4:screen-value)
           bf-CompStyle.compRatio[5] = dec(dRatio-5:screen-value)
           bf-CompStyle.compRatio[6] = dec(dRatio-6:screen-value)
           bf-CompStyle.compRatio[7] = dec(dRatio-7:screen-value)
           bf-CompStyle.compRatio[8] = dec(dRatio-8:screen-value)
           bf-CompStyle.compRatio[9] = dec(dRatio-9:screen-value)
           bf-CompStyle.compRatio[10] = dec(dRatio-10:screen-value)
           
           bf-CompStyle.compStyle[1] = cStyleComp-1:screen-value
           bf-CompStyle.compStyle[2] = cStyleComp-2:screen-value
           bf-CompStyle.compStyle[3] = cStyleComp-3:screen-value
           bf-CompStyle.compStyle[4] = cStyleComp-4:screen-value
           bf-CompStyle.compStyle[5] = cStyleComp-5:screen-value
           bf-CompStyle.compStyle[6] = cStyleComp-6:screen-value
           bf-CompStyle.compStyle[7] = cStyleComp-7:screen-value
           bf-CompStyle.compStyle[8] = cStyleComp-8:screen-value
           bf-CompStyle.compStyle[9] = cStyleComp-9:screen-value
           bf-CompStyle.compStyle[10] = cStyleComp-10:screen-value
           
           bf-CompStyle.compQty[1] = int(iCompQty-1:screen-value)
           bf-CompStyle.compQty[2] = int(iCompQty-2:screen-value)
           bf-CompStyle.compQty[3] = int(iCompQty-3:screen-value)
           bf-CompStyle.compQty[4] = int(iCompQty-4:screen-value)
           bf-CompStyle.compQty[5] = int(iCompQty-5:screen-value)
           bf-CompStyle.compQty[6] = int(iCompQty-6:screen-value)
           bf-CompStyle.compQty[7] = int(iCompQty-7:screen-value)
           bf-CompStyle.compQty[8] = int(iCompQty-8:screen-value)
           bf-CompStyle.compQty[9] = int(iCompQty-9:screen-value)
           bf-CompStyle.compQty[10] = int(iCompQty-10:screen-value)
           
           bf-CompStyle.compNumUp[1] = int(iNumUp-1:screen-value)
           bf-CompStyle.compNumUp[2] = int(iNumUp-2:screen-value)
           bf-CompStyle.compNumUp[3] = int(iNumUp-3:screen-value)
           bf-CompStyle.compNumUp[4] = int(iNumUp-4:screen-value)
           bf-CompStyle.compNumUp[5] = int(iNumUp-5:screen-value)
           bf-CompStyle.compNumUp[6] = int(iNumUp-6:screen-value)
           bf-CompStyle.compNumUp[7] = int(iNumUp-7:screen-value)
           bf-CompStyle.compNumUp[8] = int(iNumUp-8:screen-value)
           bf-CompStyle.compNumUp[9] = int(iNumUp-9:screen-value)
           bf-CompStyle.compNumUp[10] = int(iNumUp-10:screen-value)
           bf-CompStyle.compPurMan[1] = IF rs-man-pur-1:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[2] = IF rs-man-pur-2:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[3] = IF rs-man-pur-3:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[4] = IF rs-man-pur-4:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[5] = IF rs-man-pur-5:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[6] = IF rs-man-pur-6:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[7] = IF rs-man-pur-7:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[8] = IF rs-man-pur-8:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[9] = IF rs-man-pur-9:SCREEN-VALUE = "M" THEN NO ELSE YES
           bf-CompStyle.compPurMan[10] = IF rs-man-pur-10:SCREEN-VALUE = "M" THEN NO ELSE YES.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CadXConnect D-Dialog 
PROCEDURE CadXConnect :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX CONNECT NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN
        CREATE "ArtiosCAD-X.CadXCtrl.1" gchCadX NO-ERROR.
    IF NOT VALID-HANDLE (gchCadX) THEN DO:
        MESSAGE "Unable to Connect to Artios CadX. Please make sure Artios CadX is installed."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-ttCad D-Dialog 
PROCEDURE create-ttCad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param ipFilename as cha no-undo.
def var cFilenameJPG as cha no-undo.
def var resultx as int no-undo.

def var iSeq as int no-undo.
DEF VAR cDieInch AS cha NO-UNDO.
def var cCadPath as cha no-undo.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "CADFILE"  NO-LOCK NO-ERROR.

ASSIGN
   iSeq = 1
   iFormNumber = iFormNumber + 1
   iBlankNumber = 1
   cCadPath = if avail sys-ctrl and sys-ctrl.char-fld <> "" then sys-ctrl.char-fld else "c:\tmp\"
   cFileNameJPG = cCadPath + /*gchCadX:ReturnTextCode4Param("#W$")*/
                  SUBSTRING(ipFilename, R-INDEX(ipFilename, "\") + 1) + ".jpg"
   resultx = gchCadX:OpenDesign (ipFilename,0)
   resultx = gchCadX:SetOverlayClass(3,1) /*print dimensions*/
   resultx = gchCadX:SaveAsBitmap(1,cFilenameJPG, 600, 600, 20, , , 1, 100).

  find first tt-CompStyle where tt-CompStyle.form-num = iFormNumber no-error.
  cDieInch = gchCadX:ReturnTextCode4Param("#LENRULE").
  IF INDEX(cDieInch,"+") > 0 THEN cDieInch = SUBSTRING(cDieInch,1,INDEX(cDieInch,"+") - 1).

  CREATE tt-artios.
  ASSIGN tt-artios.cadnum = /*cadNumber*/ gchCadX:ReturnTextCode4Param("#ITEM$")
         tt-artios.cadfile = ipFileName
         tt-artios.custname = gchCadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")
         tt-artios.partname = gchCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)") 
         tt-artios.partnum = gchCadX:ReturnTextCode4Param("#ITEM$")
         tt-artios.style = /*gchCadX:ReturnTextCode4Param("#CFN$")*/ 
                               if avail tt-CompStyle then tt-CompStyle.CompStyle[1] else cStyle
         tt-artios.flute = ""
         tt-artios.test = "" 
         tt-artios.board = substring(gchCadX:ReturnTextCode4Param("BRD$"),1,9)
         tt-artios.len = gchCadX:ReturnNumericCode4Param("L")
         tt-artios.wid = gchCadX:ReturnNumericCode4Param("W")
         tt-artios.dep = gchCadX:ReturnNumericCode4Param("D")
         tt-artios.t-len = gchCadX:ReturnNumericCode4Param("#MANSIZEX")
         tt-artios.t-wid = gchCadX:ReturnNumericCode4Param("#MANSIZEY")
         tt-artios.seq = iSeq
         tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[1] else dRatio-1
         tt-artios.form-num = iFormNumber
         tt-artios.blank-num = iBlankNumber
         tt-artios.die-in = DEC(cDieInch) 
         tt-artios.setQty = iSetQty
         tt-artios.CompQty = if avail tt-CompStyle then tt-CompStyle.CompQty[1] else iCompQty-1
         tt-artios.procat = cCategory
         tt-artios.cust-no = gchCadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")
         tt-artios.sman    = gchCadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")
         tt-artios.dienum =  gchCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")
         tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents
         tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[1] else 1
         tt-artios.DesignImg = cFileNameJPG
         tt-artios.pur-man = IF AVAIL tt-CompStyle THEN tt-CompStyle.CompPurMan[1] ELSE
                             IF rs-man-pur = "M" THEN NO
                             ELSE YES
         tt-artios.grain   = gchCadX:ReturnTextCode4Param("#GRAIN$")
         tt-artios.DesignerName = gchCadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")
                                 + " " +
                                 gchCadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")
                          .
           
       if avail tt-compstyle and tt-CompStyle.NumOfComponents > 1 then do while iSeq < tt-CompStyle.NumOfComponents:
  
          create tt-artios.
          assign iSeq = iSeq + 1
                 iBlankNumber = iBlankNumber + 1
                 tt-artios.cadnum = /*cadNumber*/ gchCadX:ReturnTextCode4Param("#ITEM$")
                 tt-artios.cadfile = ipFileName
                 tt-artios.custname = gchCadX:ReturnTextCode4Param("DBGET(CUST,NAME$)")
                 tt-artios.partname = gchCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC1$)") 
                 tt-artios.partnum = gchCadX:ReturnTextCode4Param("#ITEM$")
                 tt-artios.style = /*gchCadX:ReturnTextCode4Param("#CFN$")*/ 
                                     if avail tt-CompStyle then tt-CompStyle.CompStyle[iSeq] else cStyle
                 tt-artios.flute = ""
                 tt-artios.test = "" 
                 tt-artios.board = substring(gchCadX:ReturnTextCode4Param("BRD$"),1,9)
                 tt-artios.len = gchCadX:ReturnNumericCode4Param("L")
                 tt-artios.wid = gchCadX:ReturnNumericCode4Param("W")
                 tt-artios.dep = gchCadX:ReturnNumericCode4Param("D")
                 tt-artios.t-len = gchCadX:ReturnNumericCode4Param("#MANSIZEX")
                 tt-artios.t-wid = gchCadX:ReturnNumericCode4Param("#MANSIZEY")
                 tt-artios.seq = iSeq
                 tt-artios.ratio = if avail tt-CompStyle then tt-CompStyle.CompRatio[iSeq] else dRatio-2
                 tt-artios.form-num = iFormNumber
                 tt-artios.blank-num = iBlankNumber
                 tt-artios.die-in =  DEC(cDieInch)
                 tt-artios.setQty = iSetQty
                 tt-artios.CompQty =  if avail tt-CompStyle then tt-CompStyle.CompQty[iSeq] else 0
                 tt-artios.procat = cCategory                      
                 tt-artios.cust-no = gchCadX:ReturnTextCode4Param("DBGET(CUST,NUMBR$)")
                 tt-artios.sman    = gchCadX:ReturnTextCode4Param("DBGET(SLSPN,SNAME$)")
                 tt-artios.dienum =  gchCadX:ReturnTextCode4Param("DBGET(DESIGN,SDSC3$)")
                 tt-artios.NumOfComponents = tt-CompStyle.NumOfComponents
                 tt-artios.CompNumUp = if avail tt-CompStyle then tt-CompStyle.CompNumUp[iSeq] else 1
                 tt-artios.DesignImg = cFileNameJPG
                 tt-artios.pur-man = if avail tt-CompStyle then tt-CompStyle.CompPurMan[iSeq] else
                                     IF rs-man-pur EQ "M" THEN NO ELSE YES
                 tt-artios.grain   = gchCadX:ReturnTextCode4Param("#GRAIN$")
                 tt-artios.DesignerName = gchCadX:ReturnTextCode4Param("DBGET(DSGNR,FNAME$)")
                                 + " " + 
                                 gchCadX:ReturnTextCode4Param("DBGET(DSGNR,LNAME$)")
                  .
       end.

       resultx = gchCadX:CloseDesign().
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects D-Dialog 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var tmpCount as int no-undo.
def var hCompRatio as handle no-undo.
def var hStyle as handle no-undo.
def var hQty as handle no-undo.
def var iRow as int no-undo.

iRow = 14.
/* create widget-pool "wComponents". */
do tmpCount = 1 to iNumOfParts:
     create fill-in hCompRatio  /* in widget-pool "wComponents" */
               ASSIGN DATA-TYPE = "Decimal" 
                 FORMAT = ">>9.99%" 
                 COLUMN = 18.5
                 ROW = iRow 
                 /* SCREEN-VALUE = hBufField:BUFFER-VALUE */
                 frame = frame {&frame-name}:handle 
                 HIDDEN = NO
                 sensitive = yes.
     iRow = iRow + 2.            
     
     .
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayCADFormInfo D-Dialog 
PROCEDURE displayCADFormInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-compStyle for tt-CompStyle.
  
 
  find first bf-CompStyle where bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)) no-error.
  if not avail bf-CompStyle then do:
     create bf-CompStyle.
     assign bf-CompStyle.form-num = int(substring(cb-CADSeq,1,2)).
  end.
  
 assign bf-CompStyle.Style = cStyle:screen-value in frame {&frame-name}
        bf-CompStyle.pur-man = IF rs-man-pur:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "M" THEN NO ELSE YES
        bf-CompStyle.NumOfComponents = int(iNumofParts:screen-value)
        bf-CompStyle.compRatio[1] = dec(dRatio-1:screen-value)
        bf-CompStyle.compRatio[2] = dec(dRatio-2:screen-value)
        bf-CompStyle.compRatio[3] = dec(dRatio-3:screen-value)
        bf-CompStyle.compRatio[4] = dec(dRatio-4:screen-value)
        bf-CompStyle.compRatio[5] = dec(dRatio-5:screen-value)
        bf-CompStyle.compRatio[6] = dec(dRatio-6:screen-value)
        bf-CompStyle.compRatio[7] = dec(dRatio-7:screen-value)
        bf-CompStyle.compRatio[8] = dec(dRatio-8:screen-value)
        bf-CompStyle.compRatio[9] = dec(dRatio-9:screen-value)
        bf-CompStyle.compRatio[10] = dec(dRatio-10:screen-value)
        
        bf-CompStyle.compStyle[1] = cStyleComp-1:screen-value
        bf-CompStyle.compStyle[2] = cStyleComp-2:screen-value
        bf-CompStyle.compStyle[3] = cStyleComp-3:screen-value
        bf-CompStyle.compStyle[4] = cStyleComp-4:screen-value
        bf-CompStyle.compStyle[5] = cStyleComp-5:screen-value
        bf-CompStyle.compStyle[6] = cStyleComp-6:screen-value
        bf-CompStyle.compStyle[7] = cStyleComp-7:screen-value
        bf-CompStyle.compStyle[8] = cStyleComp-8:screen-value
        bf-CompStyle.compStyle[9] = cStyleComp-9:screen-value
        bf-CompStyle.compStyle[10] = cStyleComp-10:screen-value
        
        bf-CompStyle.compQty[1] = int(iCompQty-1:screen-value)
        bf-CompStyle.compQty[2] = int(iCompQty-2:screen-value)
        bf-CompStyle.compQty[3] = int(iCompQty-3:screen-value)
        bf-CompStyle.compQty[4] = int(iCompQty-4:screen-value)
        bf-CompStyle.compQty[5] = int(iCompQty-5:screen-value)
        bf-CompStyle.compQty[6] = int(iCompQty-6:screen-value)
        bf-CompStyle.compQty[7] = int(iCompQty-7:screen-value)
        bf-CompStyle.compQty[8] = int(iCompQty-8:screen-value)
        bf-CompStyle.compQty[9] = int(iCompQty-9:screen-value)
        bf-CompStyle.compQty[10] = int(iCompQty-10:screen-value)
        
        bf-CompStyle.compNumUp[1] = int(iNumUp-1:screen-value)
        bf-CompStyle.compNumUp[2] = int(iNumUp-2:screen-value)
        bf-CompStyle.compNumUp[3] = int(iNumUp-3:screen-value)
        bf-CompStyle.compNumUp[4] = int(iNumUp-4:screen-value)
        bf-CompStyle.compNumUp[5] = int(iNumUp-5:screen-value)
        bf-CompStyle.compNumUp[6] = int(iNumUp-6:screen-value)
        bf-CompStyle.compNumUp[7] = int(iNumUp-7:screen-value)
        bf-CompStyle.compNumUp[8] = int(iNumUp-8:screen-value)
        bf-CompStyle.compNumUp[9] = int(iNumUp-9:screen-value)
        bf-CompStyle.compNumUp[10] = int(iNumUp-10:screen-value)

        bf-CompStyle.compPurMan[1] = IF rs-man-pur-1:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[2] = IF rs-man-pur-2:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[3] = IF rs-man-pur-3:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[4] = IF rs-man-pur-4:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[5] = IF rs-man-pur-5:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[6] = IF rs-man-pur-6:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[7] = IF rs-man-pur-7:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[8] = IF rs-man-pur-8:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[9] = IF rs-man-pur-9:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
        bf-CompStyle.compPurMan[10] = IF rs-man-pur-10:SCREEN-VALUE EQ "M" THEN NO ELSE YES.
            
            
      find first tt-CompStyle where tt-CompStyle.form-num = int(substring(cb-CADSeq:screen-value,1,2)) no-error.
      if avail tt-CompStyle then
      assign cStyle:screen-value = tt-CompStyle.Style
               iNumofParts:screen-value = string(tt-CompStyle.NumOfComponents)
               dRatio-1:screen-value = string(tt-CompStyle.compRatio[1])
               dRatio-2:screen-value = string(tt-CompStyle.compRatio[2])
               dRatio-3:screen-value = string(tt-CompStyle.compRatio[3])
               dRatio-4:screen-value = string(tt-CompStyle.compRatio[4])
               dRatio-5:screen-value = string(tt-CompStyle.compRatio[5])
               dRatio-6:screen-value = string(tt-CompStyle.compRatio[6])
               dRatio-7:screen-value = string(tt-CompStyle.compRatio[7])
               dRatio-8:screen-value = string(tt-CompStyle.compRatio[8])
               dRatio-9:screen-value = string(tt-CompStyle.compRatio[9])
               dRatio-10:screen-value = string(tt-CompStyle.compRatio[10])
               cStyleComp-1:screen-value = tt-CompStyle.compStyle[1]
               cStyleComp-2:screen-value = tt-CompStyle.compStyle[2]
               cStyleComp-3:screen-value = tt-CompStyle.compStyle[3]
               cStyleComp-4:screen-value = tt-CompStyle.compStyle[4]
               cStyleComp-5:screen-value = tt-CompStyle.compStyle[5]
               cStyleComp-6:screen-value = tt-CompStyle.compStyle[6]
               cStyleComp-7:screen-value = tt-CompStyle.compStyle[7]
               cStyleComp-8:screen-value = tt-CompStyle.compStyle[8]
               cStyleComp-9:screen-value = tt-CompStyle.compStyle[9]
               cStyleComp-10:screen-value = tt-CompStyle.compStyle[10]
               iCompQty-1:screen-value = string(tt-CompStyle.compQty[1])
               iCompQty-2:screen-value = string(tt-CompStyle.compQty[2])
               iCompQty-3:screen-value = string(tt-CompStyle.compQty[3])
               iCompQty-4:screen-value = string(tt-CompStyle.compQty[4])
               iCompQty-5:screen-value = string(tt-CompStyle.compQty[5])
               iCompQty-6:screen-value = string(tt-CompStyle.compQty[6])
               iCompQty-7:screen-value = string(tt-CompStyle.compQty[7])
               iCompQty-8:screen-value = string(tt-CompStyle.compQty[8])
               iCompQty-9:screen-value = string(tt-CompStyle.compQty[9])
               iCompQty-10:screen-value = string(tt-CompStyle.compQty[10])
               iNumUp-1:screen-value = string(tt-CompStyle.compNumUp[1])
               iNumUp-2:screen-value = string(tt-CompStyle.compNumUp[2])
               iNumUp-3:screen-value = string(tt-CompStyle.compNumUp[3])
               iNumUp-4:screen-value = string(tt-CompStyle.compNumUp[4])
               iNumUp-5:screen-value = string(tt-CompStyle.compNumUp[5])
               iNumUp-6:screen-value = string(tt-CompStyle.compNumUp[6])
               iNumUp-7:screen-value = string(tt-CompStyle.compNumUp[7])
               iNumUp-8:screen-value = string(tt-CompStyle.compNumUp[8])
               iNumUp-9:screen-value = string(tt-CompStyle.compNumUp[9])
               iNumUp-10:screen-value = string(tt-CompStyle.compNumUp[10])
                
               rs-man-pur:SCREEN-VALUE   = IF tt-CompStyle.pur-man EQ NO THEN "M" ELSE "P"
               rs-man-pur-1:screen-value = IF tt-CompStyle.compPurMan[1] EQ NO THEN "M" ELSE "P"
               rs-man-pur-2:screen-value = IF tt-CompStyle.compPurMan[2] EQ NO THEN "M" ELSE "P"
               rs-man-pur-3:screen-value = IF tt-CompStyle.compPurMan[3] EQ NO THEN "M" ELSE "P"
               rs-man-pur-4:screen-value = IF tt-CompStyle.compPurMan[4] EQ NO THEN "M" ELSE "P"
               rs-man-pur-5:screen-value = IF tt-CompStyle.compPurMan[5] EQ NO THEN "M" ELSE "P"
               rs-man-pur-6:screen-value = IF tt-CompStyle.compPurMan[6] EQ NO THEN "M" ELSE "P"
               rs-man-pur-7:screen-value = IF tt-CompStyle.compPurMan[7] EQ NO THEN "M" ELSE "P"
               rs-man-pur-8:screen-value = IF tt-CompStyle.compPurMan[8] EQ NO THEN "M" ELSE "P"
               rs-man-pur-9:screen-value = IF tt-CompStyle.compPurMan[9] EQ NO THEN "M" ELSE "P"
               rs-man-pur-10:screen-value = IF tt-CompStyle.compPurMan[10] EQ NO THEN "M" ELSE "P".
               
        else assign
               cStyle:screen-value = ""
               rs-man-pur:SCREEN-VALUE = "M"
               iNumofParts:screen-value = "1"
               dRatio-1:screen-value = "100"
               dRatio-2:screen-value = ""
               dRatio-3:screen-value = ""
               dRatio-4:screen-value = ""
               dRatio-5:screen-value = ""
               dRatio-6:screen-value = ""
               dRatio-7:screen-value = ""
               dRatio-8:screen-value = ""
               dRatio-9:screen-value = ""
               dRatio-10:screen-value = ""
               cStyleComp-1:screen-value = ""
               cStyleComp-2:screen-value = ""
               cStyleComp-3:screen-value = ""
               cStyleComp-4:screen-value = ""
               cStyleComp-5:screen-value = ""
               cStyleComp-6:screen-value = ""
               cStyleComp-7:screen-value = ""
               cStyleComp-8:screen-value = ""
               cStyleComp-9:screen-value = ""
               cStyleComp-10:screen-value = ""
               iCompQty-1:screen-value = ""
               iCompQty-2:screen-value = ""
               iCompQty-3:screen-value = ""
               iCompQty-4:screen-value = ""
               iCompQty-5:screen-value = ""
               iCompQty-6:screen-value = ""
               iCompQty-7:screen-value = ""
               iCompQty-8:screen-value = ""
               iCompQty-9:screen-value = ""
               iCompQty-10:screen-value = ""
               iNumUp-1:screen-value = ""
               iNumUp-2:screen-value = ""
               iNumUp-3:screen-value = ""
               iNumUp-4:screen-value = ""
               iNumUp-5:screen-value = ""
               iNumUp-6:screen-value = ""
               iNumUp-7:screen-value = ""
               iNumUp-8:screen-value = ""
               iNumUp-9:screen-value = ""
               iNumUp-10:screen-value = ""
               rs-man-pur-1:SCREEN-VALUE = "M"
               rs-man-pur-2:SCREEN-VALUE = "M"
               rs-man-pur-3:SCREEN-VALUE = "M"
               rs-man-pur-4:SCREEN-VALUE = "M"
               rs-man-pur-5:SCREEN-VALUE = "M"
               rs-man-pur-6:SCREEN-VALUE = "M"
               rs-man-pur-7:SCREEN-VALUE = "M"
               rs-man-pur-8:SCREEN-VALUE = "M"
               rs-man-pur-9:SCREEN-VALUE = "M"
               rs-man-pur-10:SCREEN-VALUE = "M".
            
      assign cb-CADSeq.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY cadPath cadNumber iSetQty cCategory cb-CadSeq vNumOfCADForm cStyle 
          rs-man-pur iNumOfParts dRatio-1 cStyleComp-1 iCompQty-1 iNumUp-1 
          rs-man-pur-1 dRatio-2 cStyleComp-2 iCompQty-2 iNumUp-2 rs-man-pur-2 
          dRatio-3 cStyleComp-3 iCompQty-3 iNumUp-3 rs-man-pur-3 dRatio-4 
          cStyleComp-4 iCompQty-4 iNumUp-4 rs-man-pur-4 dRatio-5 cStyleComp-5 
          iCompQty-5 iNumUp-5 rs-man-pur-5 dRatio-6 cStyleComp-6 iCompQty-6 
          iNumUp-6 rs-man-pur-6 dRatio-7 cStyleComp-7 iCompQty-7 iNumUp-7 
          rs-man-pur-7 dRatio-8 cStyleComp-8 iCompQty-8 iNumUp-8 rs-man-pur-8 
          dRatio-9 cStyleComp-9 iCompQty-9 iNumUp-9 rs-man-pur-9 dRatio-10 
          cStyleComp-10 iCompQty-10 iNumUp-10 rs-man-pur-10 
      WITH FRAME D-Dialog.
  ENABLE cadPath btnCADPathLookup cadNumber btnCAD#Lookup iSetQty cCategory 
         cb-CadSeq cStyle rs-man-pur iNumOfParts dRatio-1 cStyleComp-1 
         iCompQty-1 iNumUp-1 rs-man-pur-1 dRatio-2 cStyleComp-2 iCompQty-2 
         iNumUp-2 rs-man-pur-2 dRatio-3 cStyleComp-3 iCompQty-3 iNumUp-3 
         rs-man-pur-3 dRatio-4 cStyleComp-4 iCompQty-4 iNumUp-4 rs-man-pur-4 
         dRatio-5 cStyleComp-5 iCompQty-5 iNumUp-5 rs-man-pur-5 dRatio-6 
         cStyleComp-6 iCompQty-6 iNumUp-6 rs-man-pur-6 dRatio-7 cStyleComp-7 
         iCompQty-7 iNumUp-7 rs-man-pur-7 dRatio-8 cStyleComp-8 iCompQty-8 
         iNumUp-8 rs-man-pur-8 dRatio-9 cStyleComp-9 iCompQty-9 iNumUp-9 
         rs-man-pur-9 dRatio-10 cStyleComp-10 iCompQty-10 iNumUp-10 
         rs-man-pur-10 Btn_OK Btn_Cancel RECT-1 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM D-Dialog 
PROCEDURE getCADCAM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  input cad# and die#
  Notes:       ttblCADCAM values extracted and used in ce/b-estitem.w
------------------------------------------------------------------------------*/
  /* DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE hRecordSet AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE hConnection AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE hCommand AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE odbcDSN AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcServer AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcUserID AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcPassword AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcQuery AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcStatus AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcRecCount AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE odbcNull AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE odbcCursor AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE i AS INTEGER NO-UNDO. */
/*    */
/*   FIND sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompany */
/*                           AND sys-ctrl.name EQ 'Artios' NO-ERROR. */
/*   IF NOT AVAILABLE sys-ctrl THEN */
/*   DO: */
/*     MESSAGE 'No System Control Record for Artios Exists!' SKIP */
/*       'Unable to connect to CADCAM Database.' VIEW-AS ALERT-BOX. */
/*     RETURN. */
/*   END. */
/*    */
/*   /* Create the connection object for the link to SQL */ */
/*   CREATE 'ADODB.Connection' hConnection. */
/*   /* Create a recordset object ready to return the data */ */
/*   CREATE 'ADODB.RecordSet' hRecordSet. */
/*   /* Create a command object for sending the SQL statement */ */
/*   CREATE 'ADODB.Command' hCommand. */
/*   /* Change the below values as necessary */ */
/*   ASSIGN */
/*     odbcDSN = ENTRY(1,sys-ctrl.char-fld) /* The ODBC DSN */ */
/*     odbcServer = ENTRY(2,sys-ctrl.char-fld) /* The name of the server hosting the SQL DB and DSN */ */
/*     odbcUserID = '' /* The user id for access to the SQL Database */ */
/*     odbcPassword = ''. /* Password required by above user-id */ */
/*   /* Open up the connection to the ODBC Layer */ */
/*   hConnection:Open ('data source=' + odbcDSN + ';server=' + */
/*                      odbcServer,odbcUserID,odbcPassword,0) NO-ERROR. */
/*    */
/*   IF ERROR-STATUS:ERROR THEN */
/*   MESSAGE 'Error:' ERROR-STATUS:NUM-MESSAGES VIEW-AS ALERT-BOX. */
/*    */
/*   /* Check for connection errors */ */
/*   IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN */
/*   DO i = 1 TO ERROR-STATUS:NUM-MESSAGES: */
/*     MESSAGE ERROR-STATUS:GET-NUMBER(i) */
/*             ERROR-STATUS:GET-MESSAGE(i) VIEW-AS ALERT-BOX. */
/*   END. */
/*   ELSE */
/*   DO: */
/*     CREATE ttblCADCAM. */
/*     ttblCADCAM.die-no = ipDieNumber. */
/*     ASSIGN */
/*       odbcQuery = 'SELECT * FROM design, board WHERE board.boardid = design.boardid' */
/*       hCommand:ActiveConnection  = hConnection */
/*       hCommand:CommandText = odbcQuery */
/*       hCommand:CommandType = 1 /* adCmdText */ */
/*       hConnection:CursorLocation = 3 /* adUseClient */ */
/*       hRecordSet:CursorType = 3 /* adOpenStatic */ */
/*       hRecordSet = hCommand:Execute (OUTPUT odbcNull,'',32) */
/*       odbcRecCount = hRecordSet:RecordCount. */
/*     /* Have we returned any rows ? */ */
/*     If odbcRecCount GT 0 AND NOT odbcRecCount EQ ? THEN */
/*     DO: */
/*       hRecordSet:MoveFirst no-error. */
/*       DO WHILE odbcCursor LT odbcRecCount: */
/*         ttblCADCAM.cad-no = hRecordSet:Fields ('DESIGNNAME'):VALUE. */
/*         IF ttblCADCAM.cad-no EQ ipCadNumber THEN */
/*         DO: */
/*           ASSIGN */
/*             ttblCADCAM.board = hRecordSet:Fields ('BOARDCODE'):VALUE */
/*             ttblCADCAM.cal = hRecordSet:Fields ('CALIPER'):VALUE */
/*             ttblCADCAM.dep = hRecordSet:Fields ('DEPTH'):VALUE */
/*             ttblCADCAM.len = hRecordSet:Fields ('LENGTH'):VALUE */
/*             ttblCADCAM.lin-in = hRecordSet:Fields ('RULELENGTH'):VALUE */
/*             ttblCADCAM.style = '' /* hRecordSet:Fields ('STYLE'):VALUE */ */
/*             ttblCADCAM.t-len = hRecordSet:Fields ('BLANKLENGTH'):VALUE */
/*             ttblCADCAM.t-sqin = hRecordSet:Fields ('AREA'):VALUE */
/*             ttblCADCAM.t-wid = hRecordSet:Fields ('BLANKHEIGHT'):VALUE */
/*             ttblCADCAM.weight = hRecordSet:Fields ('BASISWEIGHT'):VALUE */
/*             ttblCADCAM.wid = hRecordSet:Fields ('WIDTH'):VALUE. */
/*           LEAVE. */
/*         END. */
/*         odbcCursor = odbcCursor + 1. */
/*         hRecordSet:MoveNext NO-ERROR. */
/*       END. /* retrieved a single data row */ */
/*     END. /* retrieved all data rows */ */
/*     ELSE odbcStatus = 'No records found.'. */
/*    */
/*     /* Close the ADO connection */ */
/*     hConnection:Close no-error. */
/*   END. /* The connection opened correctly */ */
/*    */
/*   /* Release the memory!! */ */
/*   RELEASE OBJECT hConnection NO-ERROR. */
/*   RELEASE OBJECT hCommand NO-ERROR. */
/*   RELEASE OBJECT hRecordSet NO-ERROR. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCADCAM2 D-Dialog 
PROCEDURE getCADCAM2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipCADNumber AS CHARACTER NO-UNDO. */
/*   DEFINE INPUT PARAMETER ipDieNumber AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE i AS INTEGER NO-UNDO. */
/*   DEFINE VARIABLE hAccess AS COM-HANDLE NO-UNDO. */
/*   DEFINE VARIABLE tables AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE hFile AS CHARACTER NO-UNDO. */
/*   DEFINE VARIABLE hTable AS CHARACTER NO-UNDO. */
/*    */
/*   DEFINE VARIABLE lvBoardID AS INTEGER NO-UNDO. */
/*    */
/*   CREATE ttblCADCAM. */
/*   ASSIGN */
/*     ttblCADCAM.die-no = ipDieNumber */
/*     tables = 'Design,Board'. */
/*   CREATE 'Access.Application' hAccess CONNECT TO 'c:\fibre\dcenter.mdb'. */
/*   DO i = 1 TO NUM-ENTRIES(tables): */
/*     ASSIGN */
/*       hTable = CAPS(ENTRY(i,tables)) */
/*       hFile = 'c:\fibre\' + ENTRY(i,tables) + '.txt'. */
/*     hAccess:application:docmd:TransferText (2,,hTable,hFile,TRUE,). */
/*   END. */
/*   RELEASE OBJECT hAccess. */
/*   DO i = 1 TO NUM-ENTRIES(tables): */
/*     INPUT FROM VALUE('c:\fibre\' + ENTRY(i,tables) + '.txt') NO-ECHO. */
/*     IMPORT DELIMITER ',' ^. */
/*     REPEAT: */
/*       CASE ENTRY(i,tables): */
/*         WHEN 'Board' THEN */
/*         DO: */
/*           IMPORT DELIMITER ',' lvBoardID */
/*             ttblCADCAM.board */
/*             ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ */
/*             ttblCADCAM.weight. */
/*           IF ttblCADCAM.boardID EQ boardID THEN LEAVE. */
/*         END. */
/*         WHEN 'Design' THEN */
/*         DO: */
/*           IMPORT DELIMITER ',' */
/*             ttblCADCAM.cad-no */
/*             ^ ^ ^ ^ ^ */
/*             ttblCADCAM.boardID */
/*             ^ ^ ^ ^ ^ ^ */
/*             ttblCADCAM.len */
/*             ttblCADCAM.wid */
/*             ttblCADCAM.dep */
/*             ttblCADCAM.t-len */
/*             ttblCADCAM.t-wid */
/*             ttblCADCAM.t-sqin */
/*             ttblCADCAM.cal */
/*             ttblCADCAM.lin-in. */
/*           IF ttblCADCAM.cad-no EQ ipCadNumber THEN LEAVE. */
/*         END. */
/*       END CASE. */
/*     END. */
/*     INPUT CLOSE. */
/*   END. */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNumofCADForm D-Dialog 
PROCEDURE getNumofCADForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output param opNumofCADForm as int no-undo.
    
  def var cTMPCadFIle as cha no-undo.
  def var iExt as int no-undo.
  def var iRevExt as int no-undo.
  def var cCAD# as cha no-undo.
  
  empty temp-table tt-subdir.

  /* if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\". */
/*   if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and */
/*      substring(artioscad-chr,length(artioscad-chr),1) <> "\" then */
/*      artioscad-chr = artioscad-chr + "\". */

  if cadPath = "" then cadPath = "c:\artios\asi\".

  
  
  if substring(cadPath,length(cadPath),1) <> "/" and
     substring(cadPath,length(cadPath),1) <> "\" then
     cadPath = cadPath + "\".

  create tt-subDir.
  assign tt-SubDir.DirName = cadPath.

  run getSubDirList (cadPAth).
    
  if index(CadNumber,".ard") = 0 and
     search(cadNumber + ".ard") = ? then do:
     /*  if artioscad-chr = "" then artioscad-chr = "c:\artios\asi\". */
     /*     if substring(artioscad-chr,length(artioscad-chr),1) <> "/" and */
     /*        substring(artioscad-chr,length(artioscad-chr),1) <> "\" then  artioscad-chr = artiosc ad-chr + "\". */

     for each tt-SubDir:  
       cTMPCadFile = tt-SubDir.DirName + CadNumber.
    
       do iExt = 1 to iProjectCount:
          if search(cTmpCadFile + string(iExt,"99") + ".ARD" ) <> ? then do:
             assign opNumofCADForm = opNumofCADForm + 1
                       cCAD# = CadNumber + string(iExt,"99").       
             cb-CADSeq:add-last(string(opNumOfCadForm,"99") + " " + cCAD#) in frame {&frame-name}.
          end.
          /* check revision file existing */
         do iRevExt = 65 to 90:  /* A - Z */
     
           if search(cTMPCadFile + string(iExt,"99") + chr(iRevExt) + ".ARD" ) <> ? or
              search(cTMPCadFile + string(iExt,"99") + chr(iRevExt + 32) + ".ARD" ) <> ? 
            then do:
               assign opNumofCADForm = opNumofCADForm + 1
                          cCAD# = CadNumber + string(iExt,"99") + chr(iRevExt). 
               cb-CADSeq:add-last(string(opNumOfCadForm,"99") + " " + cCAD#).                 
            end.
         end.
       end.
     end.  /*for each */
  end.  /* if search = ? */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSubDirList D-Dialog 
PROCEDURE getSubDirList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipcCurrentDirectory AS CHARACTER.

  def var cFileName as cha no-undo.
  def var cFilePath as cha no-undo.
  def var cAttrib as cha no-undo.

/* INPUT FROM OS-DIR(artioscad-chr).                     */
/* REPEAT:                                               */
/*      IMPORT cFileName cFilePath cAttrib.              */
/*      IF INDEX(cAttrib,"D") <> 0                       */
/*              AND NOT cFileName BEGINS "." THEN        */
/*      DO:                                              */
/*          CREATE tt-SubDir.                            */
/*          ASSIGN tt-SubDir.DirName = cFilePath + "\".  */
/*      END.                                             */
/* END.                                                  */

   INPUT FROM OS-DIR (ipcCurrentDirectory).
    
   REPEAT:
        IMPORT cFileName.
        IF cFileName = '.' OR cFileName = '..' OR cFileName = ? THEN NEXT.
        FILE-INFO:FILE-NAME = ipcCurrentDirectory + cFileName.
        IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN NEXT.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
            /*PUT UNFORMATTED FILE-INFO:FULL-PATHNAME SKIP.*/
            CREATE tt-subdir.
            ASSIGN tt-subdir.dirname = FILE-INFO:FULL-PATHNAME + "\" 
                   .
            RUN getSubDirList(INPUT FILE-INFO:FULL-PATHNAME + "\").
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadCADCAM D-Dialog 
PROCEDURE loadCADCAM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* DEFINE VARIABLE i AS INTEGER NO-UNDO. */
/*    */
/*   DO i = 1 TO NUM-ENTRIES(opCADCAM) BY 2: */
/*     CASE ENTRY(i,opCADCAM): */
/*       WHEN 'board' THEN */
/*       ENTRY(i + 1,opCADCAM) = ttblCADCAM.board. */
/*       WHEN 'cad-no' THEN */
/*       ENTRY(i + 1,opCADCAM) = ttblCADCAM.cad-no. */
/*       WHEN 'cal' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.cal). */
/*       WHEN 'dep' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.dep). */
/*       WHEN 'die-no' THEN */
/*       ENTRY(i + 1,opCADCAM) = ttblCADCAM.die-no. */
/*       WHEN 'len' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.len). */
/*       WHEN 'lin-in' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.lin-in). */
/*       WHEN 'style' THEN */
/*       ENTRY(i + 1,opCADCAM) = ttblCADCAM.style. */
/*       WHEN 't-len' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-len). */
/*       WHEN 't-sqin' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-sqin). */
/*       WHEN 't-wid' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.t-wid). */
/*       WHEN 'weight' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.weight). */
/*       WHEN 'wid' THEN */
/*       ENTRY(i + 1,opCADCAM) = STRING(ttblCADCAM.wid). */
/*     END CASE. */
/*   END. /* do i */ */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-category D-Dialog 
PROCEDURE valid-category :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
   
      IF NOT CAN-FIND(FIRST fgcat WHERE
         fgcat.company EQ cocode AND
         fgcat.procat  EQ cCategory:SCREEN-VALUE) OR
         cCategory:SCREEN-VALUE EQ "" THEN DO:
         MESSAGE "Invalid Category." VIEW-AS ALERT-BOX ERROR.
         op-error = YES.
         APPLY "entry" TO cCategory.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style D-Dialog 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-handle AS WIDGET-HANDLE.
   DEFINE INPUT PARAMETER ip-check-blank AS LOG NO-UNDO.
   DEFINE OUTPUT parameter op-error AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
      IF (ip-handle:SCREEN-VALUE NE "" AND NOT CAN-FIND(FIRST style
                      WHERE style.company  EQ gcompany
                        AND style.style    EQ ip-handle:SCREEN-VALUE
                        AND style.industry EQ "2")) OR
          (ip-check-blank AND ip-handle:SCREEN-VALUE EQ "") THEN DO:
        MESSAGE "Invalid Style." VIEW-AS ALERT-BOX ERROR.
        op-error = YES.
        APPLY "entry" TO ip-handle.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

