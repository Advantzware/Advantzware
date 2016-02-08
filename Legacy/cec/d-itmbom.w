&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frmItemBOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frmItemBOM 
/*------------------------------------------------------------------------

  File: cec\d-itmbom.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input param ip-recid as recid no-undo.
DEF INPUT PARAM ip-ef-rowid AS ROWID NO-UNDO.

def buffer b-item for item.
def var v-bom-count as int no-undo.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR v-cur-lam-code AS CHAR NO-UNDO.
DEF VAR v-cur-adh-code AS CHAR NO-UNDO.
DEF VAR v-sq-in-1-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-2-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-3-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-4-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-5-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-6-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-7-dec AS DEC DECIMALS 4 NO-UNDO.
DEF VAR v-sq-in-8-dec AS DEC DECIMALS 4 NO-UNDO.
def var k_frac as dec init 6.25 no-undo.
DEF VAR v-lam-in AS DEC DECIMALS 4 NO-UNDO.
{sys/inc/f16to32.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmItemBOM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ef

/* Definitions for DIALOG-BOX frmItemBOM                                */
&Scoped-define FIELDS-IN-QUERY-frmItemBOM ef.lam-code ef.adh-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-frmItemBOM ef.lam-code ef.adh-code 
&Scoped-define ENABLED-TABLES-IN-QUERY-frmItemBOM ef
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-frmItemBOM ef
&Scoped-define QUERY-STRING-frmItemBOM FOR EACH ef SHARE-LOCK
&Scoped-define OPEN-QUERY-frmItemBOM OPEN QUERY frmItemBOM FOR EACH ef SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-frmItemBOM ef
&Scoped-define FIRST-TABLE-IN-QUERY-frmItemBOM ef


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.lam-code ef.adh-code 
&Scoped-define ENABLED-TABLES ef
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS v-bom-1 v-shrink-1 v-bom-2 v-shrink-2 ~
v-bom-3 v-shrink-3 v-bom-4 v-shrink-4 v-bom-5 v-shrink-5 v-bom-6 v-shrink-6 ~
v-bom-7 v-shrink-7 v-bom-8 v-shrink-8 btn-done btn-cancel RECT-23 
&Scoped-Define DISPLAYED-FIELDS ef.lam-code ef.adh-code 
&Scoped-define DISPLAYED-TABLES ef
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS v-bom-1 v-bom-dscr-1 v-shrink-1 v-sq-in-1 ~
v-bom-2 v-bom-dscr-2 v-shrink-2 v-sq-in-2 v-bom-3 v-bom-dscr-3 v-shrink-3 ~
v-sq-in-3 v-bom-4 v-bom-dscr-4 v-shrink-4 v-sq-in-4 v-bom-5 v-bom-dscr-5 ~
v-shrink-5 v-sq-in-5 v-bom-6 v-bom-dscr-6 v-shrink-6 v-sq-in-6 v-bom-7 ~
v-bom-dscr-7 v-shrink-7 v-sq-in-7 v-bom-8 v-bom-dscr-8 v-shrink-8 v-sq-in-8 ~
v-bom-dscr-9 v-sq-in-9 v-bom-dscr-10 v-sq-in-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-done 
     LABEL "&Done" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE v-bom-1 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 1" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-2 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 2" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-3 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 3" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-4 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 4" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-5 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 5" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-6 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 6" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-7 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 7" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-8 AS CHARACTER FORMAT "x(10)":U 
     LABEL "Paper 8" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-1 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-10 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-2 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-3 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-4 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-5 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-6 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-7 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-8 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-bom-dscr-9 AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1 NO-UNDO.

DEFINE VARIABLE v-shrink-1 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-2 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-3 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-4 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-5 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-6 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-7 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-shrink-8 AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE v-sq-in-1 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-10 AS DECIMAL FORMAT ">,>>>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-2 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-3 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-4 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-5 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-6 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-7 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-8 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-sq-in-9 AS DECIMAL FORMAT ">>,>>9.99<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 12.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY frmItemBOM FOR 
      ef SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmItemBOM
     v-bom-1 AT ROW 2.43 COL 13 COLON-ALIGNED
     v-bom-dscr-1 AT ROW 2.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-1 AT ROW 2.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-1 AT ROW 2.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     v-bom-2 AT ROW 3.43 COL 13 COLON-ALIGNED
     v-bom-dscr-2 AT ROW 3.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-2 AT ROW 3.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-2 AT ROW 3.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     v-bom-3 AT ROW 4.43 COL 13 COLON-ALIGNED
     v-bom-dscr-3 AT ROW 4.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-3 AT ROW 4.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-3 AT ROW 4.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     v-bom-4 AT ROW 5.43 COL 13 COLON-ALIGNED
     v-bom-dscr-4 AT ROW 5.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-4 AT ROW 5.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-4 AT ROW 5.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     v-bom-5 AT ROW 6.43 COL 13 COLON-ALIGNED
     v-bom-dscr-5 AT ROW 6.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-5 AT ROW 6.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-5 AT ROW 6.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     v-bom-6 AT ROW 7.43 COL 13 COLON-ALIGNED
     v-bom-dscr-6 AT ROW 7.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-6 AT ROW 7.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-6 AT ROW 7.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     v-bom-7 AT ROW 8.43 COL 13 COLON-ALIGNED
     v-bom-dscr-7 AT ROW 8.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-7 AT ROW 8.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-7 AT ROW 8.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     v-bom-8 AT ROW 9.43 COL 13 COLON-ALIGNED
     v-bom-dscr-8 AT ROW 9.43 COL 38 COLON-ALIGNED NO-LABEL
     v-shrink-8 AT ROW 9.43 COL 87 COLON-ALIGNED NO-LABEL
     v-sq-in-8 AT ROW 9.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     ef.lam-code AT ROW 10.43 COL 13 COLON-ALIGNED WIDGET-ID 6
          LABEL "Laminate"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     v-bom-dscr-9 AT ROW 10.43 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     v-sq-in-9 AT ROW 10.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     ef.adh-code AT ROW 11.43 COL 13 COLON-ALIGNED WIDGET-ID 2
          LABEL "Adhesive"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     v-bom-dscr-10 AT ROW 11.43 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     btn-done AT ROW 14 COL 31
     btn-cancel AT ROW 14 COL 66
     v-sq-in-10 AT ROW 11.43 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     "Item Description" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 1.71 COL 41
          FONT 6
     "Shrink %" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 90
          FONT 6
     "Sq. Inches" VIEW-AS TEXT
          SIZE 12.8 BY .62 AT ROW 1.71 COL 106.6 WIDGET-ID 8
          FONT 6
     RECT-23 AT ROW 1.24 COL 2
     SPACE(1.00) SKIP(2.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Item Bill of Materials"
         CANCEL-BUTTON btn-cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX frmItemBOM
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME frmItemBOM:SCROLLABLE       = FALSE
       FRAME frmItemBOM:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ef.adh-code IN FRAME frmItemBOM
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ef.lam-code IN FRAME frmItemBOM
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-1 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-10 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-2 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-3 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-4 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-5 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-6 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-7 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-8 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-bom-dscr-9 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-1 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-10 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-2 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-3 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-4 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-5 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-6 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-7 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-8 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-sq-in-9 IN FRAME frmItemBOM
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX frmItemBOM
/* Query rebuild information for DIALOG-BOX frmItemBOM
     _TblList          = "asi.ef"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX frmItemBOM */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME frmItemBOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmItemBOM frmItemBOM
ON GO OF FRAME frmItemBOM /* Item Bill of Materials */
DO:
    apply "choose" to btn-done. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmItemBOM frmItemBOM
ON HELP OF FRAME frmItemBOM /* Item Bill of Materials */
DO:
  def var char-val as cha no-undo.

  case focus:name :
      when "v-bom-1" then do:
           run cec/l-itempa.w (item.company, "P" , focus:screen-value, output char-val).

           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-1
                     v-bom-dscr-1:screen-value = entry(2,char-val)
                     v-shrink-1:screen-value = entry(3,char-val)
                     v-shrink-1
                     v-sq-in-1-dec = IF DECIMAL(v-shrink-1:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-1:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-1:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-1-dec}).
      end.
      when "v-bom-2" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-2
                     v-bom-dscr-2:screen-value = entry(2,char-val)
                     v-shrink-2:screen-value = entry(3,char-val)
                     v-shrink-2
                     v-sq-in-2-dec = IF DECIMAL(v-shrink-2:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-2:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-2:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-2-dec}).
      end.
      when "v-bom-3" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-3
                     v-bom-dscr-3:screen-value = entry(2,char-val)
                     v-shrink-3:screen-value = entry(3,char-val)
                     v-shrink-3
                     v-sq-in-3-dec = IF DECIMAL(v-shrink-3:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-3:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-3:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-3-dec}).
      end.
      when "v-bom-4" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-4
                     v-bom-dscr-4:screen-value = entry(2,char-val)
                     v-shrink-4:screen-value = entry(3,char-val)
                     v-shrink-4
                     v-sq-in-4-dec = IF DECIMAL(v-shrink-4:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-4:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-4:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-4-dec}).
      end.
      when "v-bom-5" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-5
                     v-bom-dscr-5:screen-value = entry(2,char-val)
                     v-shrink-5:screen-value = entry(3,char-val)
                     v-shrink-5
                     v-sq-in-5-dec = IF DECIMAL(v-shrink-5:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-5:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-5:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-5-dec}).
      end.
      when "v-bom-6" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-6
                     v-bom-dscr-6:screen-value = entry(2,char-val)
                     v-shrink-6:screen-value = entry(3,char-val)
                     v-shrink-6
                     v-sq-in-6-dec = IF DECIMAL(v-shrink-6:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-6:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-6:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-6-dec}).
      end.
      when "v-bom-7" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-7
                     v-bom-dscr-7:screen-value = entry(2,char-val)
                     v-shrink-7:screen-value = entry(3,char-val)
                     v-shrink-7
                     v-sq-in-7-dec = IF DECIMAL(v-shrink-7:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-7:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-7:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-7-dec}).
      end.
      when "v-bom-8" then do:
           run cec/l-itempa.w (item.company, "P" ,focus:screen-value,  output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     v-bom-8
                     v-bom-dscr-8:screen-value = entry(2,char-val)
                     v-shrink-8:screen-value = entry(3,char-val)
                     v-shrink-8
                     v-sq-in-8-dec = IF DECIMAL(v-shrink-8:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-8:screen-value) / 100)))
                                     ELSE 0
                     v-sq-in-8:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-8-dec}).
      end.
      when "lam-code" then do:
           RUN windows/l-lamin.w (ITEM.company, "2",FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN 
              ASSIGN
                 focus:screen-value in frame {&frame-name} = entry(1,char-val)
                 v-cur-lam-code = FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                 v-bom-dscr-9:screen-value = entry(3,char-val)
                 v-sq-in-9:SCREEN-VALUE = STRING({sys/inc/k16.i v-lam-in}).
      end.
      when "adh-code" then do:
           RUN windows/l-adhsve.w (ITEM.company, "2",FOCUS:SCREEN-VALUE, OUTPUT char-val).
           IF char-val <> "" THEN
              ASSIGN
                 FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                 v-cur-adh-code = FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                 v-bom-dscr-10:screen-value = entry(3,char-val).
      end.
  end.

  RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmItemBOM frmItemBOM
ON WINDOW-CLOSE OF FRAME frmItemBOM /* Item Bill of Materials */
DO:
 
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.adh-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.adh-code frmItemBOM
ON LEAVE OF ef.adh-code IN FRAME frmItemBOM /* Adhesive */
DO:
  IF LASTKEY NE -1 AND
     ef.adh-code:SCREEN-VALUE NE v-cur-adh-code THEN DO:

     IF ef.adh-code:SCREEN-VALUE NE "" THEN
     DO:
        FIND FIRST b-item
             {sys/look/itemadhW.i b-}
             AND b-item.industry EQ "2"
             AND b-item.i-no EQ ef.adh-code:SCREEN-VALUE
             NO-LOCK NO-ERROR.
       
        IF NOT AVAIL b-item THEN
        DO:
           MESSAGE "Invalid Adhesive Code."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "entry" TO ef.adh-code.
           RETURN NO-APPLY.
        END.

        v-bom-dscr-10:SCREEN-VALUE = b-item.i-name.
     END.
     ELSE
        v-bom-dscr-10:SCREEN-VALUE = "".

     v-cur-adh-code = ef.adh-code:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel frmItemBOM
ON CHOOSE OF btn-cancel IN FRAME frmItemBOM /* Cancel */
DO:
    apply "window-close" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-done frmItemBOM
ON CHOOSE OF btn-done IN FRAME frmItemBOM /* Done */
DO:  
   def var hd1 as handle no-undo.
   def var hd2 as handle no-undo.
   DEF VAR lv-bom-item AS cha EXTENT 8 NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  
   def var i as int no-undo.
    
   do with frame {&frame-name}:

      IF ef.lam-code:SCREEN-VALUE NE "" THEN
      DO:
         RUN valid-lam-code NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE.
      END.

      IF ef.adh-code:SCREEN-VALUE NE "" THEN
      DO:
         RUN valid-adh-code NO-ERROR.
         IF ERROR-STATUS:ERROR THEN LEAVE.
      END.

      assign v-bom-1 v-shrink-1
             v-bom-2 v-shrink-2
             v-bom-3 v-shrink-3
             v-bom-4 v-shrink-4
             v-bom-5 v-shrink-5
             v-bom-6 v-shrink-6
             v-bom-7 v-shrink-7
             v-bom-8 v-shrink-8.
   end.

   ASSIGN lv-bom-item[1] = v-bom-1
          lv-bom-item[2] = v-bom-2
          lv-bom-item[3] = v-bom-3
          lv-bom-item[4] = v-bom-4
          lv-bom-item[5] = v-bom-5
          lv-bom-item[6] = v-bom-6
          lv-bom-item[7] = v-bom-7
          lv-bom-item[8] = v-bom-8.

    repeat v-bom-count = 1 to 8:   
      IF lv-bom-item[v-bom-count] = "" THEN
      DO:
         find item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count no-error.
         IF AVAIL item-bom THEN
            DELETE item-bom.
           
         NEXT.
      END.

      find item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count no-error.      
      if not avail item-bom then do:
          create item-bom.
          assign item-bom.company = item.company
                 item-bom.parent-i = item.i-no
                 item-bom.line# = v-bom-count.   
      end.
      item-bom.i-no = lv-bom-item[v-bom-count].

      IF NOT CAN-FIND(FIRST b-item WHERE
         b-item.company EQ item-bom.company AND
         b-item.i-no    EQ item-bom.i-no) THEN
         do:
            message "Bill Of Material found for a Invalid Item#.  Not Creating Item Bill"
                    view-as alert-box warning.
            delete item-bom.
            NEXT.
         end.
      
      case v-bom-count :
           when 1 then assign   item-bom.i-no = v-bom-1
                                item-bom.shrink = v-shrink-1   .
           when 2 then assign   item-bom.i-no = v-bom-2
                                item-bom.shrink = v-shrink-2   .
           when 3 then assign   item-bom.i-no = v-bom-3
                                item-bom.shrink = v-shrink-3   .
           when 4 then assign   item-bom.i-no = v-bom-4
                                item-bom.shrink = v-shrink-4   .
           when 5 then assign   item-bom.i-no = v-bom-5
                                item-bom.shrink = v-shrink-5   .
           when 6 then assign   item-bom.i-no = v-bom-6
                                item-bom.shrink = v-shrink-6   .
           when 7 then assign   item-bom.i-no = v-bom-7
                                item-bom.shrink = v-shrink-7   .
           when 8 then assign   item-bom.i-no = v-bom-8
                                item-bom.shrink = v-shrink-8   .
      end.        
   end.
  /* Reset master item avg/last costs */
  assign item.avg-cost  = 0
         item.last-cost = 0.

  for each item-bom where  item-bom.company = item.company and
                           item-bom.parent-i = item.i-no AND
                           item-bom.line# LT 9 no-lock:
     find b-item where  b-item.company = item-bom.company and
                        b-item.i-no = item-bom.i-no no-lock no-error.
     assign item.avg-cost  = item.avg-cost  + b-item.avg-cost
            item.last-cost = item.last-cost + b-item.last-cost.
  end.

  FIND CURRENT ef EXCLUSIVE NO-ERROR.

  IF AVAIL ef THEN DO WITH FRAME {&FRAME-NAME}:

     ASSIGN
        ef.lam-code
        ef.adh-code
        ef.adh-sqin = v-sq-in-1-dec + v-sq-in-2-dec + v-sq-in-3-dec +
                      v-sq-in-4-dec + v-sq-in-5-dec + v-sq-in-6-dec +
                      v-sq-in-7-dec + v-sq-in-8-dec.

     FIND CURRENT ef NO-LOCK.
  END.

  apply "GO" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.lam-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.lam-code frmItemBOM
ON LEAVE OF ef.lam-code IN FRAME frmItemBOM /* Laminate */
DO:
  IF LASTKEY NE -1 AND
     ef.lam-code:SCREEN-VALUE NE v-cur-lam-code THEN DO:

     IF ef.lam-code:SCREEN-VALUE NE "" THEN
     DO:
        FIND FIRST b-item
             {sys/look/itemlamW.i b-}
             AND b-item.industry EQ "2"
             AND b-item.i-no EQ ef.lam-code:SCREEN-VALUE
            NO-LOCK NO-ERROR.
       
        IF NOT AVAIL b-item THEN
        DO:
           MESSAGE "Invalid Laminate Code."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO ef.lam-code.
           RETURN NO-APPLY.
        END.

        ASSIGN
           v-bom-dscr-9:SCREEN-VALUE = b-item.i-name
           v-sq-in-9:SCREEN-VALUE = STRING({sys/inc/k16.i v-lam-in}).
     END.
     ELSE
        ASSIGN
           v-bom-dscr-9:SCREEN-VALUE = ""
           v-sq-in-9:SCREEN-VALUE = "0".

     v-cur-lam-code = ef.lam-code:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-1 frmItemBOM
ON LEAVE OF v-bom-1 IN FRAME frmItemBOM /* Paper 1 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-1 NE v-bom-1:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-1:SCREEN-VALUE = b-item.i-name
             v-shrink-1:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-1-dec = IF DECIMAL(v-shrink-1:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-1:screen-value) / 100)))
                             ELSE 0
             v-sq-in-1:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-1-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-1 NE v-bom-1:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-1:SCREEN-VALUE = ""
           v-shrink-1:SCREEN-VALUE = "0"
           v-sq-in-1:SCREEN-VALUE = "0"
           v-sq-in-1-dec = 0.

    ASSIGN v-bom-1.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-2 frmItemBOM
ON LEAVE OF v-bom-2 IN FRAME frmItemBOM /* Paper 2 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-2 NE v-bom-2:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-2:SCREEN-VALUE = b-item.i-name
             v-shrink-2:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-2-dec = IF DECIMAL(v-shrink-2:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-2:screen-value) / 100)))
                                     ELSE 0
             v-sq-in-2:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-2-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-2 NE v-bom-2:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-2:SCREEN-VALUE = ""
           v-shrink-2:SCREEN-VALUE = "0"
           v-sq-in-2:SCREEN-VALUE = "0"
           v-sq-in-2-dec = 0.

    ASSIGN v-bom-2.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-3 frmItemBOM
ON LEAVE OF v-bom-3 IN FRAME frmItemBOM /* Paper 3 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-3 NE v-bom-3:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-3:SCREEN-VALUE = b-item.i-name
             v-shrink-3:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-3-dec = IF DECIMAL(v-shrink-3:screen-value) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(v-shrink-3:screen-value) / 100)))
                             ELSE 0
             v-sq-in-3:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-3-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-3 NE v-bom-3:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-3:SCREEN-VALUE = ""
           v-shrink-3:SCREEN-VALUE = "0"
           v-sq-in-3:SCREEN-VALUE = "0"
           v-sq-in-3-dec = 0.

    ASSIGN v-bom-3.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-4 frmItemBOM
ON LEAVE OF v-bom-4 IN FRAME frmItemBOM /* Paper 4 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-4 NE v-bom-4:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-4:SCREEN-VALUE = b-item.i-name
             v-shrink-4:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-4-dec = IF DECIMAL(v-shrink-4:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-4:screen-value) / 100)))
                             ELSE 0
             v-sq-in-4:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-4-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-4 NE v-bom-4:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-4:SCREEN-VALUE = ""
           v-shrink-4:SCREEN-VALUE = "0"
           v-sq-in-4:SCREEN-VALUE = "0"
           v-sq-in-4-dec = 0.

    ASSIGN v-bom-4.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-5 frmItemBOM
ON LEAVE OF v-bom-5 IN FRAME frmItemBOM /* Paper 5 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-5 NE v-bom-5:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-5:SCREEN-VALUE = b-item.i-name
             v-shrink-5:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-5-dec = IF DECIMAL(v-shrink-5:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-5:screen-value) / 100)))
                             ELSE 0
             v-sq-in-5:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-5-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-5 NE v-bom-5:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-5:SCREEN-VALUE = ""
           v-shrink-5:SCREEN-VALUE = "0"
           v-sq-in-5:SCREEN-VALUE = "0"
           v-sq-in-5-dec = 0.

    ASSIGN v-bom-5.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-6 frmItemBOM
ON LEAVE OF v-bom-6 IN FRAME frmItemBOM /* Paper 6 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-6 NE v-bom-6:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-6:SCREEN-VALUE = b-item.i-name
             v-shrink-6:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-6-dec = IF DECIMAL(v-shrink-6:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-6:screen-value) / 100)))
                             ELSE 0
             v-sq-in-6:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-6-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-6 NE v-bom-6:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-6:SCREEN-VALUE = ""
           v-shrink-6:SCREEN-VALUE = "0"
           v-sq-in-6:SCREEN-VALUE = "0"
           v-sq-in-6-dec = 0.

    ASSIGN v-bom-6.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-7 frmItemBOM
ON LEAVE OF v-bom-7 IN FRAME frmItemBOM /* Paper 7 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-7 NE v-bom-7:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-7:SCREEN-VALUE = b-item.i-name
             v-shrink-7:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-7-dec = IF DECIMAL(v-shrink-7:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-7:screen-value) / 100)))
                             ELSE 0
             v-sq-in-7:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-7-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-7 NE v-bom-7:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-7:SCREEN-VALUE = ""
           v-shrink-7:SCREEN-VALUE = "0"
           v-sq-in-7:SCREEN-VALUE = "0"
           v-sq-in-7-dec = 0.

    ASSIGN v-bom-7.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-bom-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-bom-8 frmItemBOM
ON LEAVE OF v-bom-8 IN FRAME frmItemBOM /* Paper 8 */
DO:
   if lastkey <> -1 and self:screen-value <> "" AND
       v-bom-8 NE v-bom-8:SCREEN-VALUE THEN
    DO:
       FIND first b-item
            where b-item.company  eq item.company
              and b-item.i-no     eq self:screen-value
              and b-item.mat-type eq "P" /* Paper */
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-item THEN
       do:
          message "You MUST Enter a Valid Paper Item or Blank " 
                 view-as alert-box error.
          return no-apply.
       END.
       ELSE
          ASSIGN
             v-bom-dscr-8:SCREEN-VALUE = b-item.i-name
             v-shrink-8:SCREEN-VALUE = STRING(b-item.shrink)
             v-sq-in-8-dec = IF DECIMAL(v-shrink-8:screen-value) NE 100 THEN
                                ef.gsh-wid *
                                (ef.gsh-len / (1 - (DECIMAL(v-shrink-8:screen-value) / 100)))
                             ELSE 0
             v-sq-in-8:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-8-dec}).
    END.
    ELSE if lastkey <> -1 and self:screen-value = "" AND
         v-bom-8 NE v-bom-8:SCREEN-VALUE THEN
         ASSIGN
           v-bom-dscr-8:SCREEN-VALUE = ""
           v-shrink-8:SCREEN-VALUE = "0"
           v-sq-in-8:SCREEN-VALUE = "0"
           v-sq-in-8-dec = 0.

    ASSIGN v-bom-8.

    RUN adh-sq-in-tot-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-1 frmItemBOM
ON LEAVE OF v-shrink-1 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-1 THEN
   DO:
      ASSIGN
         v-sq-in-1-dec = IF DECIMAL(v-shrink-1:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-1:screen-value) / 100)))
                         ELSE 0
         v-sq-in-1:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-1-dec})
         v-shrink-1.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-2 frmItemBOM
ON LEAVE OF v-shrink-2 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-2 THEN
   DO:
      ASSIGN
         v-sq-in-2-dec = IF DECIMAL(v-shrink-2:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-2:screen-value) / 100)))
                         ELSE 0
         v-sq-in-2:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-2-dec})
         v-shrink-2.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-3 frmItemBOM
ON LEAVE OF v-shrink-3 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-3 THEN
   DO:
      ASSIGN
         v-sq-in-3-dec = IF DECIMAL(v-shrink-3:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-3:screen-value) / 100)))
                         ELSE 0
         v-sq-in-3:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-3-dec})
         v-shrink-3.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-4 frmItemBOM
ON LEAVE OF v-shrink-4 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-4 THEN
   DO:
      ASSIGN
         v-sq-in-4-dec = IF DECIMAL(v-shrink-4:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-4:screen-value) / 100)))
                         ELSE 0
         v-sq-in-4:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-4-dec})
         v-shrink-4.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-5 frmItemBOM
ON LEAVE OF v-shrink-5 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-5 THEN
   DO:
      ASSIGN
         v-sq-in-5-dec = IF DECIMAL(v-shrink-5:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-5:screen-value) / 100)))
                         ELSE 0
         v-sq-in-5:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-5-dec})
         v-shrink-5.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-6 frmItemBOM
ON LEAVE OF v-shrink-6 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-6 THEN
   DO:
      ASSIGN
         v-sq-in-6-dec = IF DECIMAL(v-shrink-6:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-6:screen-value) / 100)))
                         ELSE 0
         v-sq-in-6:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-6-dec})
         v-shrink-6.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-7 frmItemBOM
ON LEAVE OF v-shrink-7 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-7 THEN
   DO:
      ASSIGN
         v-sq-in-7-dec = IF DECIMAL(v-shrink-7:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-7:screen-value) / 100)))
                         ELSE 0
         v-sq-in-7:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-7-dec})
         v-shrink-7.

       RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-shrink-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-shrink-8 frmItemBOM
ON LEAVE OF v-shrink-8 IN FRAME frmItemBOM
DO:
   if lastkey <> -1 and decimal(self:screen-value) <> v-shrink-8 THEN
   DO:
      ASSIGN
         v-sq-in-8-dec = IF DECIMAL(v-shrink-8:screen-value) NE 100 THEN
                            ef.gsh-wid *
                            (ef.gsh-len / (1 - (DECIMAL(v-shrink-8:screen-value) / 100)))
                         ELSE 0
         v-sq-in-8:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-8-dec})
         v-shrink-8.

      RUN adh-sq-in-tot-proc.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frmItemBOM 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   find item where recid(item) = ip-recid.

   cocode = ITEM.company.

   repeat v-bom-count = 1 to 8:   
      find item-bom where item-bom.company  = item.company and
                          item-bom.parent-i = item.i-no and
                          item-bom.line# = v-bom-count  no-error.
      if not avail item-bom then next. 
      find b-item where b-item.company = item-bom.company and
                        b-item.i-no    = item-bom.i-no no-lock no-error.
      
      IF AVAIL b-item THEN
         case v-bom-count :
            when 1 then assign  v-bom-1 = item-bom.i-no
                                v-bom-dscr-1 = b-item.i-name
                                v-shrink-1    = item-bom.shrink.
            when 2 then assign  v-bom-2 = item-bom.i-no
                                v-bom-dscr-2 = b-item.i-name
                                v-shrink-2    = item-bom.shrink.
            when 3 then assign  v-bom-3 = item-bom.i-no
                                v-bom-dscr-3 = b-item.i-name
                                v-shrink-3    = item-bom.shrink.
            when 4 then assign  v-bom-4 = item-bom.i-no
                                v-bom-dscr-4 = b-item.i-name
                                v-shrink-4    = item-bom.shrink.
            when 5 then assign  v-bom-5 = item-bom.i-no
                                v-bom-dscr-5 = b-item.i-name
                                v-shrink-5    = item-bom.shrink.
            when 6 then assign  v-bom-6 = item-bom.i-no
                                v-bom-dscr-6 = b-item.i-name
                                v-shrink-6    = item-bom.shrink.
            when 7 then assign  v-bom-7 = item-bom.i-no
                                v-bom-dscr-7 = b-item.i-name
                                v-shrink-7    = item-bom.shrink.
            when 8 then assign  v-bom-8 = item-bom.i-no
                                v-bom-dscr-8 = b-item.i-name
                                v-shrink-8 = item-bom.shrink.
        end.        
   end.

   FIND ef WHERE ROWID(ef) EQ ip-ef-rowid NO-LOCK.

   RUN enable_UI.

   DISPLAY ef.adh-code
           ef.lam-code WITH FRAME {&FRAME-NAME}.

   IF v-cecscrn-char EQ "Decimal" THEN
      ASSIGN
         v-sq-in-1:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-2:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-3:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-4:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-5:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-6:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-7:FORMAT = ">>,>>9.99<<<<"
         v-sq-in-8:FORMAT = ">>,>>9.99<<<<".

   ASSIGN
      v-sq-in-1-dec          = IF v-bom-1 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-1 / 100)))
                               ELSE 0
      v-sq-in-1:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-1-dec})
      
      v-sq-in-2-dec          = IF v-bom-2 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-2 / 100)))
                               ELSE 0
      v-sq-in-2:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-2-dec})
      
      v-sq-in-3-dec          = IF v-bom-3 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-3 / 100)))
                               ELSE 0
      v-sq-in-3:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-3-dec})
                               
      v-sq-in-4-dec          = IF v-bom-4 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-4 / 100)))
                               ELSE 0

      v-sq-in-4:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-4-dec})
      
      v-sq-in-5-dec          = IF v-bom-5 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-5 / 100)))
                               ELSE 0
      v-sq-in-5:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-5-dec})
      
      v-sq-in-6-dec          = IF v-bom-6 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-6 / 100)))
                               ELSE 0
      v-sq-in-6:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-6-dec})
      
      v-sq-in-7-dec          = IF v-bom-7 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-7 / 100)))
                               ELSE 0
      v-sq-in-7:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-7-dec})
      v-sq-in-8-dec          = IF v-bom-8 NE "" THEN ef.gsh-wid *
                                 (ef.gsh-len / (1 - (v-shrink-8 / 100)))
                               ELSE 0
      v-sq-in-8:SCREEN-VALUE = STRING({sys/inc/k16.i v-sq-in-8-dec})
      v-lam-in = ef.gsh-len * ef.gsh-wid.

   IF ef.lam-code NE "" THEN
      v-sq-in-9:SCREEN-VALUE = STRING({sys/inc/k16.i v-lam-in}).
                                   
   RUN adh-sq-in-tot-proc.                                      

   IF ef.lam-code NE "" THEN
   DO:
      v-cur-lam-code = ef.lam-code.

      FIND FIRST b-item
           {sys/look/itemlamW.i b-}
           AND b-item.industry EQ "2"
           AND b-item.i-no EQ ef.lam-code:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL b-item THEN
         v-bom-dscr-9:SCREEN-VALUE = b-item.i-name.
   END.

   IF ef.adh-code NE "" THEN
   DO:
      v-cur-adh-code = ef.adh-code.

      FIND FIRST b-item
           {sys/look/itemadhW.i b-}
           AND b-item.industry EQ "2"
           AND b-item.i-no EQ ef.adh-code:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL b-item THEN
         v-bom-dscr-10:SCREEN-VALUE = b-item.i-name.
   END.

   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adh-sq-in-tot-proc frmItemBOM 
PROCEDURE adh-sq-in-tot-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-total AS DEC DECIMALS 4 NO-UNDO.

   ASSIGN
      v-total = v-sq-in-1-dec + v-sq-in-2-dec + v-sq-in-3-dec + v-sq-in-4-dec +
                v-sq-in-5-dec + v-sq-in-6-dec + v-sq-in-7-dec + v-sq-in-8-dec
      v-sq-in-10:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
                                       STRING({sys/inc/k16.i v-total}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frmItemBOM  _DEFAULT-DISABLE
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
  HIDE FRAME frmItemBOM.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frmItemBOM  _DEFAULT-ENABLE
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
  DISPLAY v-bom-1 v-bom-dscr-1 v-shrink-1 v-sq-in-1 v-bom-2 v-bom-dscr-2 
          v-shrink-2 v-sq-in-2 v-bom-3 v-bom-dscr-3 v-shrink-3 v-sq-in-3 v-bom-4 
          v-bom-dscr-4 v-shrink-4 v-sq-in-4 v-bom-5 v-bom-dscr-5 v-shrink-5 
          v-sq-in-5 v-bom-6 v-bom-dscr-6 v-shrink-6 v-sq-in-6 v-bom-7 
          v-bom-dscr-7 v-shrink-7 v-sq-in-7 v-bom-8 v-bom-dscr-8 v-shrink-8 
          v-sq-in-8 v-bom-dscr-9 v-sq-in-9 v-bom-dscr-10 v-sq-in-10 
      WITH FRAME frmItemBOM.
  IF AVAILABLE ef THEN 
    DISPLAY ef.lam-code ef.adh-code 
      WITH FRAME frmItemBOM.
  ENABLE v-bom-1 v-shrink-1 v-bom-2 v-shrink-2 v-bom-3 v-shrink-3 v-bom-4 
         v-shrink-4 v-bom-5 v-shrink-5 v-bom-6 v-shrink-6 v-bom-7 v-shrink-7 
         v-bom-8 v-shrink-8 ef.lam-code ef.adh-code btn-done btn-cancel RECT-23 
      WITH FRAME frmItemBOM.
  VIEW FRAME frmItemBOM.
  {&OPEN-BROWSERS-IN-QUERY-frmItemBOM}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-adh-code frmItemBOM 
PROCEDURE valid-adh-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF ef.adh-code:SCREEN-VALUE NE "" THEN DO:
         IF NOT CAN-FIND(FIRST ITEM
                         {sys/look/itemadhW.i}
                           AND item.industry EQ "2"
                           AND item.i-no EQ ef.adh-code:SCREEN-VALUE) THEN DO:
           MESSAGE "Invalid " + TRIM(ef.adh-code:LABEL) + ", try help..."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO ef.adh-code.
           RETURN ERROR.
         END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-lam-code frmItemBOM 
PROCEDURE valid-lam-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      IF ef.lam-code:SCREEN-VALUE NE "" AND
          NOT CAN-FIND(FIRST ITEM
                      {sys/look/itemlamW.i}
                        AND item.industry EQ "2"
                        AND item.i-no EQ ef.lam-code:SCREEN-VALUE) THEN DO:
         MESSAGE "Invalid " + TRIM(ef.lam-code:LABEL) + ", try help..."
             VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO ef.lam-code.
         RETURN ERROR.
       END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

