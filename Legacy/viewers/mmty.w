&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE enable-detail enable-detail

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES mmty
&Scoped-define FIRST-EXTERNAL-TABLE mmty


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mmty.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mmty.col-value[1] mmty.col-value[2] ~
mmty.col-value[3] mmty.col-value[4] mmty.col-value[5] mmty.col-value[6] ~
mmty.col-value[7] mmty.col-value[8] mmty.col-value[9] mmty.col-value[10] ~
mmty.row-value[1] mmty.row-value[2] mmty.row-value[3] mmty.row-value[4] ~
mmty.row-value[5] mmty.row-value[6] mmty.row-value[7] mmty.row-value[8] ~
mmty.row-value[9] mmty.row-value[10] mmty.row-value[11] mmty.row-value[12] ~
mmty.row-value[13] mmty.row-value[14] mmty.row-value[15] mmty.vals[11] ~
mmty.vals[12] mmty.vals[13] mmty.vals[14] mmty.vals[15] mmty.vals[16] ~
mmty.vals[17] mmty.vals[18] mmty.vals[19] mmty.vals[20] mmty.vals[21] ~
mmty.vals[22] mmty.vals[23] mmty.vals[24] mmty.vals[25] mmty.vals[26] ~
mmty.vals[27] mmty.vals[28] mmty.vals[29] mmty.vals[30] mmty.vals[31] ~
mmty.vals[32] mmty.vals[33] mmty.vals[34] mmty.vals[35] mmty.vals[36] ~
mmty.vals[37] mmty.vals[38] mmty.vals[39] mmty.vals[40] mmty.vals[41] ~
mmty.vals[42] mmty.vals[43] mmty.vals[44] mmty.vals[45] mmty.vals[46] ~
mmty.vals[47] mmty.vals[48] mmty.vals[49] mmty.vals[50] mmty.vals[51] ~
mmty.vals[52] mmty.vals[53] mmty.vals[54] mmty.vals[55] mmty.vals[56] ~
mmty.vals[57] mmty.vals[58] mmty.vals[59] mmty.vals[60] mmty.vals[61] ~
mmty.vals[62] mmty.vals[63] mmty.vals[64] mmty.vals[65] mmty.vals[66] ~
mmty.vals[67] mmty.vals[68] mmty.vals[69] mmty.vals[70] mmty.vals[71] ~
mmty.vals[72] mmty.vals[73] mmty.vals[74] mmty.vals[75] mmty.vals[76] ~
mmty.vals[77] mmty.vals[78] mmty.vals[79] mmty.vals[80] mmty.vals[81] ~
mmty.vals[82] mmty.vals[83] mmty.vals[84] mmty.vals[85] mmty.vals[86] ~
mmty.vals[87] mmty.vals[88] mmty.vals[89] mmty.vals[90] mmty.vals[91] ~
mmty.vals[92] mmty.vals[93] mmty.vals[94] mmty.vals[95] mmty.vals[96] ~
mmty.vals[97] mmty.vals[98] mmty.vals[99] mmty.vals[100] mmty.vals[101] ~
mmty.vals[102] mmty.vals[103] mmty.vals[104] mmty.vals[105] mmty.vals[106] ~
mmty.vals[107] mmty.vals[108] mmty.vals[109] mmty.vals[110] mmty.vals[111] ~
mmty.vals[112] mmty.vals[113] mmty.vals[114] mmty.vals[115] mmty.vals[116] ~
mmty.vals[117] mmty.vals[118] mmty.vals[119] mmty.vals[120] mmty.vals[121] ~
mmty.vals[122] mmty.vals[123] mmty.vals[124] mmty.vals[125] mmty.vals[126] ~
mmty.vals[127] mmty.vals[128] mmty.vals[129] mmty.vals[130] mmty.vals[131] ~
mmty.vals[132] mmty.vals[133] mmty.vals[134] mmty.vals[135] mmty.vals[136] ~
mmty.vals[137] mmty.vals[138] mmty.vals[139] mmty.vals[140] mmty.vals[141] ~
mmty.vals[142] mmty.vals[143] mmty.vals[144] mmty.vals[145] mmty.vals[146] ~
mmty.vals[147] mmty.vals[148] mmty.vals[149] mmty.vals[150] mmty.vals[151] ~
mmty.vals[152] mmty.vals[153] mmty.vals[154] mmty.vals[155] mmty.vals[156] ~
mmty.vals[157] mmty.vals[158] mmty.vals[159] mmty.vals[160] 
&Scoped-define ENABLED-TABLES mmty
&Scoped-define FIRST-ENABLED-TABLE mmty
&Scoped-Define ENABLED-OBJECTS RECT-20 
&Scoped-Define DISPLAYED-FIELDS mmty.m-code mmty.style mmty.c-title ~
mmty.col-value[1] mmty.col-value[2] mmty.col-value[3] mmty.col-value[4] ~
mmty.col-value[5] mmty.col-value[6] mmty.col-value[7] mmty.col-value[8] ~
mmty.col-value[9] mmty.col-value[10] mmty.rtit[1] mmty.row-value[1] ~
mmty.row-value[2] mmty.row-value[3] mmty.row-value[4] mmty.row-value[5] ~
mmty.row-value[6] mmty.row-value[7] mmty.row-value[8] mmty.row-value[9] ~
mmty.row-value[10] mmty.row-value[11] mmty.row-value[12] mmty.row-value[13] ~
mmty.row-value[14] mmty.row-value[15] mmty.vals[11] mmty.vals[12] ~
mmty.vals[13] mmty.vals[14] mmty.vals[15] mmty.vals[16] mmty.vals[17] ~
mmty.vals[18] mmty.vals[19] mmty.rtit[2] mmty.vals[20] mmty.vals[21] ~
mmty.vals[22] mmty.vals[23] mmty.vals[24] mmty.vals[25] mmty.vals[26] ~
mmty.vals[27] mmty.vals[28] mmty.vals[29] mmty.vals[30] mmty.rtit[3] ~
mmty.vals[31] mmty.vals[32] mmty.vals[33] mmty.vals[34] mmty.vals[35] ~
mmty.vals[36] mmty.vals[37] mmty.vals[38] mmty.vals[39] mmty.vals[40] ~
mmty.rtit[4] mmty.vals[41] mmty.vals[42] mmty.vals[43] mmty.vals[44] ~
mmty.vals[45] mmty.vals[46] mmty.vals[47] mmty.vals[48] mmty.vals[49] ~
mmty.vals[50] mmty.rtit[5] mmty.vals[51] mmty.vals[52] mmty.vals[53] ~
mmty.vals[54] mmty.vals[55] mmty.vals[56] mmty.vals[57] mmty.vals[58] ~
mmty.vals[59] mmty.vals[60] mmty.rtit[6] mmty.vals[61] mmty.vals[62] ~
mmty.vals[63] mmty.vals[64] mmty.vals[65] mmty.vals[66] mmty.vals[67] ~
mmty.vals[68] mmty.vals[69] mmty.vals[70] mmty.rtit[7] mmty.vals[71] ~
mmty.vals[72] mmty.vals[73] mmty.vals[74] mmty.vals[75] mmty.vals[76] ~
mmty.vals[77] mmty.vals[78] mmty.vals[79] mmty.vals[80] mmty.rtit[8] ~
mmty.vals[81] mmty.vals[82] mmty.vals[83] mmty.vals[84] mmty.vals[85] ~
mmty.vals[86] mmty.vals[87] mmty.vals[88] mmty.vals[89] mmty.vals[90] ~
mmty.rtit[9] mmty.vals[91] mmty.vals[92] mmty.vals[93] mmty.vals[94] ~
mmty.vals[95] mmty.vals[96] mmty.vals[97] mmty.vals[98] mmty.vals[99] ~
mmty.vals[100] mmty.rtit[10] mmty.vals[101] mmty.vals[102] mmty.vals[103] ~
mmty.vals[104] mmty.vals[105] mmty.vals[106] mmty.vals[107] mmty.vals[108] ~
mmty.vals[109] mmty.vals[110] mmty.rtit[11] mmty.vals[111] mmty.vals[112] ~
mmty.vals[113] mmty.vals[114] mmty.vals[115] mmty.vals[116] mmty.vals[117] ~
mmty.vals[118] mmty.vals[119] mmty.vals[120] mmty.rtit[12] mmty.vals[121] ~
mmty.vals[122] mmty.vals[123] mmty.vals[124] mmty.vals[125] mmty.vals[126] ~
mmty.vals[127] mmty.vals[128] mmty.vals[129] mmty.vals[130] mmty.rtit[13] ~
mmty.vals[131] mmty.vals[132] mmty.vals[133] mmty.vals[134] mmty.vals[135] ~
mmty.vals[136] mmty.vals[137] mmty.vals[138] mmty.vals[139] mmty.vals[140] ~
mmty.rtit[14] mmty.vals[141] mmty.vals[142] mmty.vals[143] mmty.vals[144] ~
mmty.vals[145] mmty.vals[146] mmty.vals[147] mmty.vals[148] mmty.vals[149] ~
mmty.vals[150] mmty.rtit[15] mmty.vals[151] mmty.vals[152] mmty.vals[153] ~
mmty.vals[154] mmty.vals[155] mmty.vals[156] mmty.vals[157] mmty.vals[158] ~
mmty.vals[159] mmty.vals[160] 
&Scoped-define DISPLAYED-TABLES mmty
&Scoped-define FIRST-DISPLAYED-TABLE mmty
&Scoped-Define DISPLAYED-OBJECTS mcode_dscr style_dscr lv-page 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE lv-page AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1 NO-UNDO.

DEFINE VARIABLE mcode_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 125 BY 18.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mmty.m-code AT ROW 1.24 COL 18 COLON-ALIGNED
          LABEL "Machine" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mcode_dscr AT ROW 1.24 COL 31 COLON-ALIGNED NO-LABEL
     mmty.style AT ROW 1.24 COL 75 COLON-ALIGNED
          LABEL "Style" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     style_dscr AT ROW 1.24 COL 88 COLON-ALIGNED NO-LABEL
     mmty.c-title AT ROW 2.19 COL 33 COLON-ALIGNED NO-LABEL FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     lv-page AT ROW 3.38 COL 2 NO-LABEL
     mmty.col-value[1] AT ROW 3.38 COL 20 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[2] AT ROW 3.38 COL 31 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[3] AT ROW 3.38 COL 42 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[4] AT ROW 3.38 COL 52 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[5] AT ROW 3.38 COL 62 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[6] AT ROW 3.38 COL 72 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[7] AT ROW 3.38 COL 82 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[8] AT ROW 3.38 COL 92 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[9] AT ROW 3.38 COL 102 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.col-value[10] AT ROW 3.38 COL 112 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmty.rtit[1] AT ROW 4.57 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.row-value[1] AT ROW 4.57 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[2] AT ROW 5.52 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[3] AT ROW 6.48 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[4] AT ROW 7.43 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[5] AT ROW 8.43 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[6] AT ROW 9.43 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[7] AT ROW 10.29 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[8] AT ROW 11.24 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[9] AT ROW 12.19 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.row-value[10] AT ROW 13.14 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[11] AT ROW 14.1 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[12] AT ROW 15.05 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[13] AT ROW 16 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[14] AT ROW 16.95 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.row-value[15] AT ROW 17.91 COL 5 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmty.vals[11] AT ROW 4.57 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[12] AT ROW 4.57 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[13] AT ROW 4.57 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[14] AT ROW 4.57 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[15] AT ROW 4.57 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[16] AT ROW 4.57 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[17] AT ROW 4.57 COL 82 COLON-ALIGNED NO-LABEL FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[18] AT ROW 4.57 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[19] AT ROW 4.57 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[2] AT ROW 5.57 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[20] AT ROW 4.57 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[21] AT ROW 5.52 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[22] AT ROW 5.52 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[23] AT ROW 5.52 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[24] AT ROW 5.52 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[25] AT ROW 5.52 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[26] AT ROW 5.52 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[27] AT ROW 5.52 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[28] AT ROW 5.52 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[29] AT ROW 5.52 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[30] AT ROW 5.52 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[3] AT ROW 6.48 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[31] AT ROW 6.48 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[32] AT ROW 6.48 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[33] AT ROW 6.48 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[34] AT ROW 6.48 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[35] AT ROW 6.48 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[36] AT ROW 6.48 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[37] AT ROW 6.48 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[38] AT ROW 6.48 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[39] AT ROW 6.48 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[40] AT ROW 6.48 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[4] AT ROW 7.43 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[41] AT ROW 7.43 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[42] AT ROW 7.43 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[43] AT ROW 7.43 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[44] AT ROW 7.43 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[45] AT ROW 7.43 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[46] AT ROW 7.43 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[47] AT ROW 7.43 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[48] AT ROW 7.43 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[49] AT ROW 7.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[50] AT ROW 7.43 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[5] AT ROW 8.38 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[51] AT ROW 8.38 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[52] AT ROW 8.38 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[53] AT ROW 8.38 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[54] AT ROW 8.38 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[55] AT ROW 8.38 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[56] AT ROW 8.38 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[57] AT ROW 8.38 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[58] AT ROW 8.38 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[59] AT ROW 8.38 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[60] AT ROW 8.38 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[6] AT ROW 9.33 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[61] AT ROW 9.33 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[62] AT ROW 9.33 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[63] AT ROW 9.33 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[64] AT ROW 9.33 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[65] AT ROW 9.33 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[66] AT ROW 9.33 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[67] AT ROW 9.33 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[68] AT ROW 9.33 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[69] AT ROW 9.33 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[70] AT ROW 9.33 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[7] AT ROW 10.29 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[71] AT ROW 10.29 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[72] AT ROW 10.29 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[73] AT ROW 10.29 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[74] AT ROW 10.29 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[75] AT ROW 10.29 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[76] AT ROW 10.29 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[77] AT ROW 10.29 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[78] AT ROW 10.29 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[79] AT ROW 10.29 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[80] AT ROW 10.29 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[8] AT ROW 11.24 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[81] AT ROW 11.24 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[82] AT ROW 11.24 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[83] AT ROW 11.24 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[84] AT ROW 11.24 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[85] AT ROW 11.24 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[86] AT ROW 11.24 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[87] AT ROW 11.24 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[88] AT ROW 11.24 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[89] AT ROW 11.24 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[90] AT ROW 11.24 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[9] AT ROW 12.19 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[91] AT ROW 12.19 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[92] AT ROW 12.19 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[93] AT ROW 12.19 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[94] AT ROW 12.19 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[95] AT ROW 12.19 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[96] AT ROW 12.19 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[97] AT ROW 12.19 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[98] AT ROW 12.19 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[99] AT ROW 12.19 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[100] AT ROW 12.19 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[10] AT ROW 13.14 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[101] AT ROW 13.14 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[102] AT ROW 13.14 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[103] AT ROW 13.14 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[104] AT ROW 13.14 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[105] AT ROW 13.14 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[106] AT ROW 13.14 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[107] AT ROW 13.14 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[108] AT ROW 13.14 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[109] AT ROW 13.14 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[110] AT ROW 13.14 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[11] AT ROW 14.1 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[111] AT ROW 14.1 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[112] AT ROW 14.1 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[113] AT ROW 14.1 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[114] AT ROW 14.1 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[115] AT ROW 14.1 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[116] AT ROW 14.1 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[117] AT ROW 14.1 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[118] AT ROW 14.1 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[119] AT ROW 14.1 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[120] AT ROW 14.1 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[12] AT ROW 15.05 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[121] AT ROW 15.05 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[122] AT ROW 15.05 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[123] AT ROW 15.05 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[124] AT ROW 15.05 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[125] AT ROW 15.05 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[126] AT ROW 15.05 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[127] AT ROW 15.05 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[128] AT ROW 15.05 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[129] AT ROW 15.05 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[130] AT ROW 15.05 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[13] AT ROW 16 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[131] AT ROW 16 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[132] AT ROW 16 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1.05
     mmty.vals[133] AT ROW 16 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[134] AT ROW 16 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[135] AT ROW 16 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[136] AT ROW 16 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[137] AT ROW 16 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[138] AT ROW 16 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[139] AT ROW 16 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[140] AT ROW 16 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[14] AT ROW 16.95 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[141] AT ROW 16.95 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
     mmty.vals[142] AT ROW 16.95 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[143] AT ROW 16.95 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[144] AT ROW 16.95 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[145] AT ROW 16.95 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[146] AT ROW 16.95 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[147] AT ROW 16.95 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[148] AT ROW 16.95 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[149] AT ROW 16.95 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[150] AT ROW 16.95 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.rtit[15] AT ROW 17.91 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmty.vals[151] AT ROW 17.91 COL 20 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmty.vals[152] AT ROW 17.91 COL 32 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[153] AT ROW 17.91 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[154] AT ROW 17.91 COL 52 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[155] AT ROW 17.91 COL 62 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[156] AT ROW 17.91 COL 72 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[157] AT ROW 17.91 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[158] AT ROW 17.91 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[159] AT ROW 17.91 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmty.vals[160] AT ROW 17.91 COL 112 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     RECT-20 AT ROW 1 COL 1
     "MR" VIEW-AS TEXT
          SIZE 6 BY 1 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.mmty
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 27.29
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mmty.c-title IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mmty.col-value[10] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[7] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[8] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.col-value[9] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN lv-page IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.m-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN mcode_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mmty.row-value[10] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[11] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[12] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[13] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[14] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[15] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[7] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[8] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.row-value[9] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmty.rtit[10] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[11] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[12] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[13] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[14] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[15] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[1] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[2] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[3] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[4] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[5] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[6] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[7] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[8] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.rtit[9] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmty.style IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mmty.vals[17] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "mmty"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mmty"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-detail V-table-Win 
PROCEDURE enable-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DISPLAY mmty.vals[11 FOR 150] WITH FRAME {&FRAME-NAME}.
  ENABLE  mmty.vals[11 FOR 150] WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail mmty then return.
  lv-page:screen-value in frame {&frame-name} = 
      "Pg: " + trim(string(mmty.page-no + 1,">9")) + "-" +
               trim(string(mmty.across-no + 1,">9")).

  find first style where style.style = mmty.style no-lock no-error.
  style_dscr:screen-value = if avail style then style.dscr else "".
               
  DEF VAR i AS INT NO-UNDO.
  DO i = 11 TO 160 WITH FRAME {&FRAME-NAME} :
     IF mmty.vals[i] = 0 THEN
        CASE i :
            WHEN 11 THEN HIDE mmty.vals[11]. 
            WHEN 12 THEN HIDE mmty.vals[12]. 
            WHEN 13 THEN HIDE mmty.vals[13]. 
            WHEN 14 THEN HIDE mmty.vals[14]. 
            WHEN 15 THEN HIDE mmty.vals[15]. 
            WHEN 16 THEN HIDE mmty.vals[16]. 
            WHEN 17 THEN HIDE mmty.vals[17]. 
            WHEN 18 THEN HIDE mmty.vals[18]. 
            WHEN 19 THEN HIDE mmty.vals[19]. 
            WHEN 20 THEN HIDE mmty.vals[20]. 
            WHEN 21 THEN HIDE mmty.vals[21]. 
            WHEN 22 THEN HIDE mmty.vals[22]. 
            WHEN 23 THEN HIDE mmty.vals[23]. 
            WHEN 24 THEN HIDE mmty.vals[24]. 
            WHEN 25 THEN HIDE mmty.vals[25]. 
            WHEN 26 THEN HIDE mmty.vals[26]. 
            WHEN 27 THEN HIDE mmty.vals[27]. 
            WHEN 28 THEN HIDE mmty.vals[28]. 
            WHEN 29 THEN HIDE mmty.vals[29]. 
            WHEN 30 THEN HIDE mmty.vals[30]. 
            WHEN 31 THEN HIDE mmty.vals[31]. 
            WHEN 32 THEN HIDE mmty.vals[32]. 
            WHEN 33 THEN HIDE mmty.vals[33]. 
            WHEN 34 THEN HIDE mmty.vals[34]. 
            WHEN 35 THEN HIDE mmty.vals[35]. 
            WHEN 36 THEN HIDE mmty.vals[36]. 
            WHEN 37 THEN HIDE mmty.vals[37]. 
            WHEN 38 THEN HIDE mmty.vals[38]. 
            WHEN 39 THEN HIDE mmty.vals[39]. 
            WHEN 40 THEN HIDE mmty.vals[40]. 
            WHEN 41 THEN HIDE mmty.vals[41]. 
            WHEN 42 THEN HIDE mmty.vals[42]. 
            WHEN 43 THEN HIDE mmty.vals[43]. 
            WHEN 44 THEN HIDE mmty.vals[44]. 
            WHEN 45 THEN HIDE mmty.vals[45]. 
            WHEN 46 THEN HIDE mmty.vals[46]. 
            WHEN 47 THEN HIDE mmty.vals[47]. 
            WHEN 48 THEN HIDE mmty.vals[48]. 
            WHEN 49 THEN HIDE mmty.vals[49].             
            WHEN 50 THEN HIDE mmty.vals[50]. 
            WHEN 51 THEN HIDE mmty.vals[51]. 
            WHEN 52 THEN HIDE mmty.vals[52]. 
            WHEN 53 THEN HIDE mmty.vals[53]. 
            WHEN 54 THEN HIDE mmty.vals[54]. 
            WHEN 55 THEN HIDE mmty.vals[55]. 
            WHEN 56 THEN HIDE mmty.vals[56]. 
            WHEN 57 THEN HIDE mmty.vals[57]. 
            WHEN 58 THEN HIDE mmty.vals[58]. 
            WHEN 59 THEN HIDE mmty.vals[59]. 
            WHEN 60 THEN HIDE mmty.vals[60]. 
            WHEN 61 THEN HIDE mmty.vals[61]. 
            WHEN 62 THEN HIDE mmty.vals[62]. 
            WHEN 63 THEN HIDE mmty.vals[63]. 
            WHEN 64 THEN HIDE mmty.vals[64]. 
            WHEN 65 THEN HIDE mmty.vals[65]. 
            WHEN 66 THEN HIDE mmty.vals[66]. 
            WHEN 67 THEN HIDE mmty.vals[67]. 
            WHEN 68 THEN HIDE mmty.vals[68]. 
            WHEN 69 THEN HIDE mmty.vals[69]. 
            WHEN 70 THEN HIDE mmty.vals[70]. 
            WHEN 71 THEN HIDE mmty.vals[71]. 
            WHEN 72 THEN HIDE mmty.vals[72]. 
            WHEN 73 THEN HIDE mmty.vals[73]. 
            WHEN 74 THEN HIDE mmty.vals[74]. 
            WHEN 75 THEN HIDE mmty.vals[75]. 
            WHEN 76 THEN HIDE mmty.vals[76]. 
            WHEN 77 THEN HIDE mmty.vals[77]. 
            WHEN 78 THEN HIDE mmty.vals[78]. 
            WHEN 79 THEN HIDE mmty.vals[79].             
            WHEN 80 THEN HIDE mmty.vals[80]. 
            WHEN 81 THEN HIDE mmty.vals[81]. 
            WHEN 82 THEN HIDE mmty.vals[82]. 
            WHEN 83 THEN HIDE mmty.vals[83]. 
            WHEN 84 THEN HIDE mmty.vals[84]. 
            WHEN 85 THEN HIDE mmty.vals[85]. 
            WHEN 86 THEN HIDE mmty.vals[86]. 
            WHEN 87 THEN HIDE mmty.vals[87]. 
            WHEN 88 THEN HIDE mmty.vals[88]. 
            WHEN 89 THEN HIDE mmty.vals[89]. 
            WHEN 90 THEN HIDE mmty.vals[90]. 
            WHEN 91 THEN HIDE mmty.vals[91]. 
            WHEN 92 THEN HIDE mmty.vals[92]. 
            WHEN 93 THEN HIDE mmty.vals[93]. 
            WHEN 94 THEN HIDE mmty.vals[94]. 
            WHEN 95 THEN HIDE mmty.vals[95]. 
            WHEN 96 THEN HIDE mmty.vals[96]. 
            WHEN 97 THEN HIDE mmty.vals[97]. 
            WHEN 98 THEN HIDE mmty.vals[98]. 
            WHEN 99 THEN HIDE mmty.vals[99]. 
            WHEN 100 THEN HIDE mmty.vals[100]. 
            WHEN 101 THEN HIDE mmty.vals[101]. 
            WHEN 102 THEN HIDE mmty.vals[102]. 
            WHEN 103 THEN HIDE mmty.vals[103]. 
            WHEN 104 THEN HIDE mmty.vals[104]. 
            WHEN 105 THEN HIDE mmty.vals[105]. 
            WHEN 106 THEN HIDE mmty.vals[106]. 
            WHEN 107 THEN HIDE mmty.vals[107]. 
            WHEN 108 THEN HIDE mmty.vals[108]. 
            WHEN 109 THEN HIDE mmty.vals[109]. 
            WHEN 110 THEN HIDE mmty.vals[110]. 
            WHEN 111 THEN HIDE mmty.vals[111]. 
            WHEN 112 THEN HIDE mmty.vals[112]. 
            WHEN 113 THEN HIDE mmty.vals[113]. 
            WHEN 114 THEN HIDE mmty.vals[114]. 
            WHEN 115 THEN HIDE mmty.vals[115]. 
            WHEN 116 THEN HIDE mmty.vals[116]. 
            WHEN 117 THEN HIDE mmty.vals[117]. 
            WHEN 118 THEN HIDE mmty.vals[118]. 
            WHEN 119 THEN HIDE mmty.vals[119]. 
            WHEN 120 THEN HIDE mmty.vals[120]. 
            WHEN 121 THEN HIDE mmty.vals[121]. 
            WHEN 122 THEN HIDE mmty.vals[122]. 
            WHEN 123 THEN HIDE mmty.vals[123]. 
            WHEN 124 THEN HIDE mmty.vals[124]. 
            WHEN 125 THEN HIDE mmty.vals[125]. 
            WHEN 126 THEN HIDE mmty.vals[126]. 
            WHEN 127 THEN HIDE mmty.vals[127]. 
            WHEN 128 THEN HIDE mmty.vals[128]. 
            WHEN 129 THEN HIDE mmty.vals[129]. 
            WHEN 130 THEN HIDE mmty.vals[130]. 
            WHEN 131 THEN HIDE mmty.vals[131]. 
            WHEN 132 THEN HIDE mmty.vals[132]. 
            WHEN 133 THEN HIDE mmty.vals[133]. 
            WHEN 134 THEN HIDE mmty.vals[134]. 
            WHEN 135 THEN HIDE mmty.vals[135]. 
            WHEN 136 THEN HIDE mmty.vals[136]. 
            WHEN 137 THEN HIDE mmty.vals[137]. 
            WHEN 138 THEN HIDE mmty.vals[138]. 
            WHEN 139 THEN HIDE mmty.vals[139]. 
            WHEN 140 THEN HIDE mmty.vals[140]. 
            WHEN 141 THEN HIDE mmty.vals[141]. 
            WHEN 142 THEN HIDE mmty.vals[142]. 
            WHEN 143 THEN HIDE mmty.vals[143]. 
            WHEN 144 THEN HIDE mmty.vals[144]. 
            WHEN 145 THEN HIDE mmty.vals[145]. 
            WHEN 146 THEN HIDE mmty.vals[146]. 
            WHEN 147 THEN HIDE mmty.vals[147]. 
            WHEN 148 THEN HIDE mmty.vals[148]. 
            WHEN 149 THEN HIDE mmty.vals[149]. 
            WHEN 150 THEN HIDE mmty.vals[150]. 
            WHEN 151 THEN HIDE mmty.vals[151]. 
            WHEN 152 THEN HIDE mmty.vals[152]. 
            WHEN 153 THEN HIDE mmty.vals[153]. 
            WHEN 154 THEN HIDE mmty.vals[154]. 
            WHEN 155 THEN HIDE mmty.vals[155]. 
            WHEN 156 THEN HIDE mmty.vals[156]. 
            WHEN 157 THEN HIDE mmty.vals[157]. 
            WHEN 158 THEN HIDE mmty.vals[158]. 
            WHEN 159 THEN HIDE mmty.vals[159]. 
            WHEN 160 THEN HIDE mmty.vals[160]. 
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch ('display-fields').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "mmty"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

