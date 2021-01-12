&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: viewers/d-CustMark.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

  Created: 12th Jan, 2020
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

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* Parameters Definitions ---                                        */

/* PARAMs Definitions ---                                           */
DEFINE INPUT  PARAMETER iprRowID  AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER ipcType   AS CHARACTER NO-UNDO .   /* add,update,view and copy */
DEFINE OUTPUT PARAMETER oprRowID  AS ROWID     NO-UNDO.

{custom/globdefs.i}
{methods/defines/hndlset.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company
    locode = g_loc
    .

/* Local Variable Definitions ---                                       */
    DEFINE VARIABLE riRowID      AS ROWID     NO-UNDO.
    DEFINE VARIABLE cRtnValue    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecordFound AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        INPUT cocode,
        INPUT "CEMarkupMatrixLookup",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cRtnValue,
        OUTPUT lRecordFound
        ).

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
&Scoped-Define ENABLED-FIELDS cust-markup.style cust-markup.procat ~
cust-markup.lookup_reduction[1] cust-markup.markup_reduction[1] ~
cust-markup.run-qty[1] cust-markup.markup[1] ~
cust-markup.lookup_reduction[2] cust-markup.markup_reduction[2] ~
cust-markup.run-qty[2] cust-markup.markup[2] ~
cust-markup.lookup_reduction[3] cust-markup.markup_reduction[3] ~
cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.lookup_reduction[4] cust-markup.markup_reduction[4] ~
cust-markup.run-qty[4] cust-markup.markup[4] ~
cust-markup.lookup_reduction[5] cust-markup.markup_reduction[5] ~
cust-markup.run-qty[5] cust-markup.markup[5] ~
cust-markup.lookup_reduction[6] cust-markup.markup_reduction[6] ~
cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.lookup_reduction[7] cust-markup.markup_reduction[7] ~
cust-markup.run-qty[7] cust-markup.markup[7] ~
cust-markup.lookup_reduction[8] cust-markup.markup_reduction[8] ~
cust-markup.run-qty[8] cust-markup.markup[8] ~
cust-markup.lookup_reduction[9] cust-markup.markup_reduction[9] ~
cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.lookup_reduction[10] cust-markup.markup_reduction[10] ~
cust-markup.run-qty[10] cust-markup.markup[10] 
&Scoped-define ENABLED-TABLES cust-markup
&Scoped-define FIRST-ENABLED-TABLE cust-markup
&Scoped-Define ENABLED-OBJECTS btnDone btnCancel btnOk RECT-44 RECT-45 ~
cb_markup-on-01 cb_markup-on-02 cb_markup-on-03 cb_markup-on-04 ~
cb_markup-on-05 cb_markup-on-06 cb_markup-on-07 cb_markup-on-08 ~
cb_markup-on-09 cb_markup-on-10 
&Scoped-Define DISPLAYED-FIELDS cust-markup.style cust-markup.procat ~
cust-markup.lookup_reduction[1] cust-markup.markup_reduction[1] ~
cust-markup.run-qty[1] cust-markup.markup[1] ~
cust-markup.lookup_reduction[2] cust-markup.markup_reduction[2] ~
cust-markup.run-qty[2] cust-markup.markup[2] ~
cust-markup.lookup_reduction[3] cust-markup.markup_reduction[3] ~
cust-markup.run-qty[3] cust-markup.markup[3] ~
cust-markup.lookup_reduction[4] cust-markup.markup_reduction[4] ~
cust-markup.run-qty[4] cust-markup.markup[4] ~
cust-markup.lookup_reduction[5] cust-markup.markup_reduction[5] ~
cust-markup.run-qty[5] cust-markup.markup[5] ~
cust-markup.lookup_reduction[6] cust-markup.markup_reduction[6] ~
cust-markup.run-qty[6] cust-markup.markup[6] ~
cust-markup.lookup_reduction[7] cust-markup.markup_reduction[7] ~
cust-markup.run-qty[7] cust-markup.markup[7] ~
cust-markup.lookup_reduction[8] cust-markup.markup_reduction[8] ~
cust-markup.run-qty[8] cust-markup.markup[8] ~
cust-markup.lookup_reduction[9] cust-markup.markup_reduction[9] ~
cust-markup.run-qty[9] cust-markup.markup[9] ~
cust-markup.lookup_reduction[10] cust-markup.markup_reduction[10] ~
cust-markup.run-qty[10] cust-markup.markup[10] 
&Scoped-define DISPLAYED-TABLES cust-markup
&Scoped-define FIRST-DISPLAYED-TABLE cust-markup
&Scoped-Define DISPLAYED-OBJECTS fiLookUp cb_markup-on-01 cb_markup-on-02 ~
cb_markup-on-03 cb_markup-on-04 cb_markup-on-05 cb_markup-on-06 ~
cb_markup-on-07 cb_markup-on-08 cb_markup-on-09 cb_markup-on-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnDone AUTO-END-KEY DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Done" 
     SIZE 8 BY 1.91 TOOLTIP "Done".

DEFINE BUTTON btnOk 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&OK" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE VARIABLE cb_markup-on-01 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-02 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-03 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-04 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-05 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-06 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-07 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-08 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-09 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE cb_markup-on-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "(N)et","(G)ross","(B)oard" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE fiLookUp AS CHARACTER FORMAT "X(256)":U INITIAL "Square Feet" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 21 BY 2.24.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 12.14
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btnDone AT ROW 15.91 COL 62 WIDGET-ID 84
     btnCancel AT ROW 15.91 COL 66.6 WIDGET-ID 78
     btnOk AT ROW 15.91 COL 56 WIDGET-ID 76
     cust-markup.style AT ROW 1.19 COL 55.2 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 19.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.procat AT ROW 2.38 COL 55.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 19.2 BY 1
          BGCOLOR 15 FONT 4
     fiLookUp AT ROW 4.05 COL 1.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     cust-markup.lookup_reduction[1] AT ROW 5.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 130
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[1] AT ROW 5.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 150
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[1] AT ROW 5.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 110 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[1] AT ROW 5.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-01 AT ROW 5.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     cust-markup.lookup_reduction[2] AT ROW 6.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 132
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[2] AT ROW 6.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 152
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[2] AT ROW 6.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 112 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[2] AT ROW 6.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-02 AT ROW 6.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     cust-markup.lookup_reduction[3] AT ROW 7.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 134
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[3] AT ROW 7.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 154
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[3] AT ROW 7.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 114 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[3] AT ROW 7.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-03 AT ROW 7.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     cust-markup.lookup_reduction[4] AT ROW 8.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 136
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[4] AT ROW 8.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 156
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[4] AT ROW 8.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 116 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     cust-markup.markup[4] AT ROW 8.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-04 AT ROW 8.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     cust-markup.lookup_reduction[5] AT ROW 9.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 138
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[5] AT ROW 9.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 158
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[5] AT ROW 9.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 118 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[5] AT ROW 9.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-05 AT ROW 9.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     cust-markup.lookup_reduction[6] AT ROW 10.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 140
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[6] AT ROW 10.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 160
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[6] AT ROW 10.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 120 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[6] AT ROW 10.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-06 AT ROW 10.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     cust-markup.lookup_reduction[7] AT ROW 11.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 142
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[7] AT ROW 11.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 162
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[7] AT ROW 11.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 122 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[7] AT ROW 11.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-07 AT ROW 11.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     cust-markup.lookup_reduction[8] AT ROW 12.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 144
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[8] AT ROW 12.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 164
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[8] AT ROW 12.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 124 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     cust-markup.markup[8] AT ROW 12.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-08 AT ROW 12.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     cust-markup.lookup_reduction[9] AT ROW 13.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 146
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[9] AT ROW 13.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 166
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[9] AT ROW 13.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 126 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[9] AT ROW 13.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-09 AT ROW 13.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     cust-markup.lookup_reduction[10] AT ROW 14.19 COL 86 COLON-ALIGNED NO-LABEL WIDGET-ID 128
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup_reduction[10] AT ROW 14.19 COL 110 COLON-ALIGNED NO-LABEL WIDGET-ID 148
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.run-qty[10] AT ROW 14.29 COL 2.2 COLON-ALIGNED NO-LABEL WIDGET-ID 108 FORMAT ">>>,>>>,>>9.9<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     cust-markup.markup[10] AT ROW 14.29 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     cb_markup-on-10 AT ROW 14.29 COL 56 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     "Margin Reduction" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 4.05 COL 112 WIDGET-ID 170
          BGCOLOR 15 
     "Reduction Matrix" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 3.24 COL 86 WIDGET-ID 174
          BGCOLOR 15 
     "Margin On" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 4.05 COL 58.6 WIDGET-ID 74
          BGCOLOR 15 
     "Board % of Cost" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 4.1 COL 88 WIDGET-ID 176
          BGCOLOR 15 
     "Margin" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.05 COL 32 WIDGET-ID 52
          BGCOLOR 15 
     RECT-44 AT ROW 15.76 COL 54.8 WIDGET-ID 82
     RECT-45 AT ROW 3.62 COL 84 WIDGET-ID 168
     SPACE(6.39) SKIP(2.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Customer Pricing" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewrhlp.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiLookUp IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[10] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[1] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[2] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[3] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[4] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[5] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[6] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[7] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[8] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN cust-markup.run-qty[9] IN FRAME D-Dialog
   EXP-FORMAT                                                           */
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
ON END-ERROR OF FRAME D-Dialog /* Customer Pricing */
DO:
    DISABLE TRIGGERS FOR LOAD OF cust-markup.
    IF AVAILABLE cust-markup THEN
        oprRowID = ROWID(cust-markup).  
    IF riRowID NE ? THEN 
        oprRowID = ?.
/*     APPLY 'GO':U TO FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Customer Pricing */
DO:  
    DISABLE TRIGGERS FOR LOAD OF cust-markup.
    
    IF AVAILABLE cust-markup THEN
        oprRowID = ROWID(cust-markup).
    
    IF riRowID NE ? THEN 
        oprRowID = ?.
        
     APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel D-Dialog
ON CHOOSE OF btnCancel IN FRAME D-Dialog /* Cancel */
DO:
    DISABLE TRIGGERS FOR LOAD OF cust-markup.
    
    IF AVAILABLE cust-markup THEN
        oprRowID = ROWID(cust-markup).
        
    IF riRowID NE ? THEN 
        oprRowID = ?.
     APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDone D-Dialog
ON CHOOSE OF btnDone IN FRAME D-Dialog /* Done */
DO:
    IF AVAILABLE cust-markup THEN
        oprRowID = ROWID(cust-markup). 
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk D-Dialog
ON CHOOSE OF btnOk IN FRAME D-Dialog /* OK */
DO:
    IF ipcType EQ "View" THEN DO:
        APPLY "GO":U TO FRAME {&FRAME-NAME}.
        RETURN.
    END.    
        
    RUN pValidStyle NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN NO-APPLY.

    RUN pValidCategory NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        RETURN NO-APPLY.

    RUN pValidMarkup NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        RETURN NO-APPLY.
        
    FIND CURRENT cust-markup EXCLUSIVE-LOCK NO-ERROR.    
    ASSIGN
        {&ENABLED-FIELDS}
        cust-markup.markup-on[01] = SUBSTR(cb_markup-on-01:SCREEN-VALUE,2,1)
        cust-markup.markup-on[02] = SUBSTR(cb_markup-on-02:SCREEN-VALUE,2,1)
        cust-markup.markup-on[03] = SUBSTR(cb_markup-on-03:SCREEN-VALUE,2,1)
        cust-markup.markup-on[04] = SUBSTR(cb_markup-on-04:SCREEN-VALUE,2,1)
        cust-markup.markup-on[05] = SUBSTR(cb_markup-on-05:SCREEN-VALUE,2,1)
        cust-markup.markup-on[06] = SUBSTR(cb_markup-on-06:SCREEN-VALUE,2,1)
        cust-markup.markup-on[07] = SUBSTR(cb_markup-on-07:SCREEN-VALUE,2,1)
        cust-markup.markup-on[08] = SUBSTR(cb_markup-on-08:SCREEN-VALUE,2,1)
        cust-markup.markup-on[09] = SUBSTR(cb_markup-on-09:SCREEN-VALUE,2,1)
        cust-markup.markup-on[10] = SUBSTR(cb_markup-on-10:SCREEN-VALUE,2,1)
        .
    oprRowID = ROWID(cust-markup). 
    MESSAGE cust-markup.markup-on[01]
    VIEW-AS ALERT-BOX.   
    APPLY 'GO':U TO FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-markup.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.procat D-Dialog
ON LEAVE OF cust-markup.procat IN FRAME D-Dialog /* Category */
DO: 
    IF LASTKEY NE -1 THEN DO:
        RUN pValidCategory NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cust-markup.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cust-markup.style D-Dialog
ON LEAVE OF cust-markup.style IN FRAME D-Dialog /* Style Code */
DO:
    IF LASTKEY NE -1 THEN DO:
        RUN pValidStyle NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
    IF cRtnValue NE "" THEN     
        fiLookUp:SCREEN-VALUE = cRtnValue.
        
    IF ipcType EQ "Copy" OR ipcType EQ "Add" 
        THEN riRowID = iprRowID.

    FIND FIRST cust-markup NO-LOCK
         WHERE ROWID(cust-markup) EQ iprRowID
         NO-ERROR.  
    IF ipcType NE "View" THEN DO:
        RUN enable_UI.
        RUN pDisplayFields.
        btnDone:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE DO:
        RUN pDisplayFields.
        ASSIGN
            btnDone:HIDDEN IN FRAME {&FRAME-NAME} = NO
            btnDone:SENSITIVE                     = YES
            btnOk:HIDDEN                          = YES
            btnCancel:HIDDEN                      = YES
            .
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

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
  DISPLAY fiLookUp cb_markup-on-01 cb_markup-on-02 cb_markup-on-03 
          cb_markup-on-04 cb_markup-on-05 cb_markup-on-06 cb_markup-on-07 
          cb_markup-on-08 cb_markup-on-09 cb_markup-on-10 
      WITH FRAME D-Dialog.
  IF AVAILABLE cust-markup THEN 
    DISPLAY cust-markup.style cust-markup.procat cust-markup.lookup_reduction[1] 
          cust-markup.markup_reduction[1] cust-markup.run-qty[1] 
          cust-markup.markup[1] cust-markup.lookup_reduction[2] 
          cust-markup.markup_reduction[2] cust-markup.run-qty[2] 
          cust-markup.markup[2] cust-markup.lookup_reduction[3] 
          cust-markup.markup_reduction[3] cust-markup.run-qty[3] 
          cust-markup.markup[3] cust-markup.lookup_reduction[4] 
          cust-markup.markup_reduction[4] cust-markup.run-qty[4] 
          cust-markup.markup[4] cust-markup.lookup_reduction[5] 
          cust-markup.markup_reduction[5] cust-markup.run-qty[5] 
          cust-markup.markup[5] cust-markup.lookup_reduction[6] 
          cust-markup.markup_reduction[6] cust-markup.run-qty[6] 
          cust-markup.markup[6] cust-markup.lookup_reduction[7] 
          cust-markup.markup_reduction[7] cust-markup.run-qty[7] 
          cust-markup.markup[7] cust-markup.lookup_reduction[8] 
          cust-markup.markup_reduction[8] cust-markup.run-qty[8] 
          cust-markup.markup[8] cust-markup.lookup_reduction[9] 
          cust-markup.markup_reduction[9] cust-markup.run-qty[9] 
          cust-markup.markup[9] cust-markup.lookup_reduction[10] 
          cust-markup.markup_reduction[10] cust-markup.run-qty[10] 
          cust-markup.markup[10] 
      WITH FRAME D-Dialog.
  ENABLE btnDone btnCancel btnOk RECT-44 RECT-45 cust-markup.style 
         cust-markup.procat cust-markup.lookup_reduction[1] 
         cust-markup.markup_reduction[1] cust-markup.run-qty[1] 
         cust-markup.markup[1] cb_markup-on-01 cust-markup.lookup_reduction[2] 
         cust-markup.markup_reduction[2] cust-markup.run-qty[2] 
         cust-markup.markup[2] cb_markup-on-02 cust-markup.lookup_reduction[3] 
         cust-markup.markup_reduction[3] cust-markup.run-qty[3] 
         cust-markup.markup[3] cb_markup-on-03 cust-markup.lookup_reduction[4] 
         cust-markup.markup_reduction[4] cust-markup.run-qty[4] 
         cust-markup.markup[4] cb_markup-on-04 cust-markup.lookup_reduction[5] 
         cust-markup.markup_reduction[5] cust-markup.run-qty[5] 
         cust-markup.markup[5] cb_markup-on-05 cust-markup.lookup_reduction[6] 
         cust-markup.markup_reduction[6] cust-markup.run-qty[6] 
         cust-markup.markup[6] cb_markup-on-06 cust-markup.lookup_reduction[7] 
         cust-markup.markup_reduction[7] cust-markup.run-qty[7] 
         cust-markup.markup[7] cb_markup-on-07 cust-markup.lookup_reduction[8] 
         cust-markup.markup_reduction[8] cust-markup.run-qty[8] 
         cust-markup.markup[8] cb_markup-on-08 cust-markup.lookup_reduction[9] 
         cust-markup.markup_reduction[9] cust-markup.run-qty[9] 
         cust-markup.markup[9] cb_markup-on-09 cust-markup.lookup_reduction[10] 
         cust-markup.markup_reduction[10] cust-markup.run-qty[10] 
         cust-markup.markup[10] cb_markup-on-10 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayFields D-Dialog 
PROCEDURE pDisplayFields PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF AVAILABLE cust-markup THEN  DO:
    RUN pDisplayMarginOn.
    DISPLAY cust-markup.style cust-markup.procat cust-markup.lookup_reduction[1] 
          cust-markup.markup_reduction[1] cust-markup.run-qty[1] 
          cust-markup.markup[1] cust-markup.lookup_reduction[2] 
          cust-markup.markup_reduction[2] cust-markup.run-qty[2] 
          cust-markup.markup[2] cust-markup.lookup_reduction[3] 
          cust-markup.markup_reduction[3] cust-markup.run-qty[3] 
          cust-markup.markup[3] cust-markup.lookup_reduction[4] 
          cust-markup.markup_reduction[4] cust-markup.run-qty[4] 
          cust-markup.markup[4] cust-markup.lookup_reduction[5] 
          cust-markup.markup_reduction[5] cust-markup.run-qty[5] 
          cust-markup.markup[5] cust-markup.lookup_reduction[6] 
          cust-markup.markup_reduction[6] cust-markup.run-qty[6] 
          cust-markup.markup[6] cust-markup.lookup_reduction[7] 
          cust-markup.markup_reduction[7] cust-markup.run-qty[7] 
          cust-markup.markup[7] cust-markup.lookup_reduction[8] 
          cust-markup.markup_reduction[8] cust-markup.run-qty[8] 
          cust-markup.markup[8] cust-markup.lookup_reduction[9] 
          cust-markup.markup_reduction[9] cust-markup.run-qty[9] 
          cust-markup.markup[9] cust-markup.lookup_reduction[10] 
          cust-markup.markup_reduction[10] cust-markup.run-qty[10] 
          cust-markup.markup[10] fiLookUp
      WITH FRAME D-Dialog.
   END.   

   IF ipcType NE "View" THEN .
       ENABLE btnCancel btnOk WITH FRAME Dialog-Frame.
   
   IF ipcType EQ "Update" THEN 
       DISABLE cust-markup.style cust-markup.procat.    

   VIEW FRAME {&FRAME-NAME}. 
   APPLY "ENTRY" TO FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayMarginOn D-Dialog 
PROCEDURE pDisplayMarginOn PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        {viewers/custmark.i 01}
        {viewers/custmark.i 02}
        {viewers/custmark.i 03}
        {viewers/custmark.i 04}
        {viewers/custmark.i 05}
        {viewers/custmark.i 06}
        {viewers/custmark.i 07}
        {viewers/custmark.i 08}
        {viewers/custmark.i 09}
        {viewers/custmark.i 10}
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidCategory D-Dialog 
PROCEDURE pValidCategory PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        cust-markup.procat:SCREEN-VALUE =
            CAPS(cust-markup.procat:SCREEN-VALUE).

        IF cust-markup.procat:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST fgcat
                         WHERE fgcat.company EQ cocode
                           AND fgcat.procat  EQ cust-markup.procat:SCREEN-VALUE)
            THEN DO:
            MESSAGE TRIM(cust-markup.procat:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO cust-markup.procat.
            RETURN ERROR.
        END.

        RUN pValidEntry NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
            RETURN ERROR.
    END.

  {methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidEntry D-Dialog 
PROCEDURE pValidEntry PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-cust-markup FOR cust-markup.
    
    {methods/lValidateError.i YES}
      DO WITH FRAME {&FRAME-NAME}:
          IF CAN-FIND(FIRST bf-cust-markup
              WHERE bf-cust-markup.company EQ cocode
                AND bf-cust-markup.cust-no EQ cust-markup.cust-no
                AND bf-cust-markup.style   EQ cust-markup.style:SCREEN-VALUE
                AND bf-cust-markup.procat  EQ cust-markup.procat:SCREEN-VALUE
                AND ROWID(bf-cust-markup)  NE ROWID(cust-markup))
              THEN DO:
              MESSAGE "This record already exists for customer, please re-enter..."
                  VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY":U TO cust-markup.procat.
              RETURN ERROR.
          END.
      END.
    
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidMarkup D-Dialog 
PROCEDURE pValidMarkup PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:

        IF DEC(cust-markup.markup[1]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[1].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[2]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[2].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[3]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[3].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[4]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[4].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[5]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[5].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[6]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[6].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[7]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[7].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[8]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[8].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[9]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[9].
            RETURN ERROR.
        END.

        IF DEC(cust-markup.markup[10]:SCREEN-VALUE) GE 100 THEN DO:
            MESSAGE "Markup Must Be Less Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO cust-markup.markup[10].
            RETURN ERROR.
        END.
    END.

  {methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidStyle D-Dialog 
PROCEDURE pValidStyle PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        cust-markup.style:SCREEN-VALUE =
            CAPS(cust-markup.style:SCREEN-VALUE).

        IF cust-markup.style:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST style
                         WHERE style.company EQ cocode
                           AND style.style   EQ cust-markup.style:SCREEN-VALUE)
            THEN DO:
            MESSAGE TRIM(cust-markup.style:LABEL) + " is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY":U TO cust-markup.style.
            RETURN ERROR.
        END.

        RUN pValidEntry NO-ERROR.
        IF ERROR-STATUS:ERROR THEN  
            RETURN ERROR.
    END.

  {methods/lValidateError.i NO}

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

