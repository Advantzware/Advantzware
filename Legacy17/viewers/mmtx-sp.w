&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p on 04.07.2017 @  2:08:16 pm */

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
&SCOPED-DEFINE ENABLE-detail enable-detail

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
&Scoped-define EXTERNAL-TABLES mmtx
&Scoped-define FIRST-EXTERNAL-TABLE mmtx


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mmtx.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mmtx.col-value[1] mmtx.col-value[2] ~
mmtx.col-value[3] mmtx.col-value[4] mmtx.col-value[5] mmtx.col-value[6] ~
mmtx.col-value[7] mmtx.col-value[8] mmtx.col-value[9] mmtx.col-value[10] ~
mmtx.row-value[1] mmtx.row-value[2] mmtx.row-value[3] mmtx.row-value[4] ~
mmtx.row-value[5] mmtx.row-value[6] mmtx.row-value[7] mmtx.row-value[8] ~
mmtx.row-value[9] mmtx.row-value[10] mmtx.row-value[11] mmtx.row-value[12] ~
mmtx.row-value[13] mmtx.row-value[14] mmtx.row-value[15] mmtx.vals[11] ~
mmtx.vals[12] mmtx.vals[13] mmtx.vals[14] mmtx.vals[15] mmtx.vals[16] ~
mmtx.vals[17] mmtx.vals[18] mmtx.vals[19] mmtx.vals[20] mmtx.vals[21] ~
mmtx.vals[22] mmtx.vals[23] mmtx.vals[24] mmtx.vals[25] mmtx.vals[26] ~
mmtx.vals[27] mmtx.vals[28] mmtx.vals[29] mmtx.vals[30] mmtx.vals[31] ~
mmtx.vals[32] mmtx.vals[33] mmtx.vals[34] mmtx.vals[35] mmtx.vals[36] ~
mmtx.vals[37] mmtx.vals[38] mmtx.vals[39] mmtx.vals[40] mmtx.vals[41] ~
mmtx.vals[42] mmtx.vals[43] mmtx.vals[44] mmtx.vals[45] mmtx.vals[46] ~
mmtx.vals[47] mmtx.vals[48] mmtx.vals[49] mmtx.vals[50] mmtx.vals[51] ~
mmtx.vals[52] mmtx.vals[53] mmtx.vals[54] mmtx.vals[55] mmtx.vals[56] ~
mmtx.vals[57] mmtx.vals[58] mmtx.vals[59] mmtx.vals[60] mmtx.vals[61] ~
mmtx.vals[62] mmtx.vals[63] mmtx.vals[64] mmtx.vals[65] mmtx.vals[66] ~
mmtx.vals[67] mmtx.vals[68] mmtx.vals[69] mmtx.vals[70] mmtx.vals[71] ~
mmtx.vals[72] mmtx.vals[73] mmtx.vals[74] mmtx.vals[75] mmtx.vals[76] ~
mmtx.vals[77] mmtx.vals[78] mmtx.vals[79] mmtx.vals[80] mmtx.vals[81] ~
mmtx.vals[82] mmtx.vals[83] mmtx.vals[84] mmtx.vals[85] mmtx.vals[86] ~
mmtx.vals[87] mmtx.vals[88] mmtx.vals[89] mmtx.vals[90] mmtx.vals[91] ~
mmtx.vals[92] mmtx.vals[93] mmtx.vals[94] mmtx.vals[95] mmtx.vals[96] ~
mmtx.vals[97] mmtx.vals[98] mmtx.vals[99] mmtx.vals[100] mmtx.vals[101] ~
mmtx.vals[102] mmtx.vals[103] mmtx.vals[104] mmtx.vals[105] mmtx.vals[106] ~
mmtx.vals[107] mmtx.vals[108] mmtx.vals[109] mmtx.vals[110] mmtx.vals[111] ~
mmtx.vals[112] mmtx.vals[113] mmtx.vals[114] mmtx.vals[115] mmtx.vals[116] ~
mmtx.vals[117] mmtx.vals[118] mmtx.vals[119] mmtx.vals[120] mmtx.vals[121] ~
mmtx.vals[122] mmtx.vals[123] mmtx.vals[124] mmtx.vals[125] mmtx.vals[126] ~
mmtx.vals[127] mmtx.vals[128] mmtx.vals[129] mmtx.vals[130] mmtx.vals[131] ~
mmtx.vals[132] mmtx.vals[133] mmtx.vals[134] mmtx.vals[135] mmtx.vals[136] ~
mmtx.vals[137] mmtx.vals[138] mmtx.vals[139] mmtx.vals[140] mmtx.vals[141] ~
mmtx.vals[142] mmtx.vals[143] mmtx.vals[144] mmtx.vals[145] mmtx.vals[146] ~
mmtx.vals[147] mmtx.vals[148] mmtx.vals[149] mmtx.vals[150] mmtx.vals[151] ~
mmtx.vals[152] mmtx.vals[153] mmtx.vals[154] mmtx.vals[155] mmtx.vals[156] ~
mmtx.vals[157] mmtx.vals[158] mmtx.vals[159] mmtx.vals[160] 
&Scoped-define ENABLED-TABLES mmtx
&Scoped-define FIRST-ENABLED-TABLE mmtx
&Scoped-Define ENABLED-OBJECTS RECT-8 
&Scoped-Define DISPLAYED-FIELDS mmtx.m-code mmtx.style mmtx.c-title ~
mmtx.col-value[1] mmtx.col-value[2] mmtx.col-value[3] mmtx.col-value[4] ~
mmtx.col-value[5] mmtx.col-value[6] mmtx.col-value[7] mmtx.col-value[8] ~
mmtx.col-value[9] mmtx.col-value[10] mmtx.rtit[1] mmtx.row-value[1] ~
mmtx.row-value[2] mmtx.row-value[3] mmtx.row-value[4] mmtx.row-value[5] ~
mmtx.row-value[6] mmtx.row-value[7] mmtx.row-value[8] mmtx.row-value[9] ~
mmtx.row-value[10] mmtx.row-value[11] mmtx.row-value[12] mmtx.row-value[13] ~
mmtx.row-value[14] mmtx.row-value[15] mmtx.vals[11] mmtx.vals[12] ~
mmtx.vals[13] mmtx.vals[14] mmtx.vals[15] mmtx.vals[16] mmtx.vals[17] ~
mmtx.vals[18] mmtx.vals[19] mmtx.vals[20] mmtx.rtit[2] mmtx.vals[21] ~
mmtx.vals[22] mmtx.vals[23] mmtx.vals[24] mmtx.vals[25] mmtx.vals[26] ~
mmtx.vals[27] mmtx.vals[28] mmtx.vals[29] mmtx.vals[30] mmtx.rtit[3] ~
mmtx.vals[31] mmtx.vals[32] mmtx.vals[33] mmtx.vals[34] mmtx.vals[35] ~
mmtx.vals[36] mmtx.vals[37] mmtx.vals[38] mmtx.vals[39] mmtx.vals[40] ~
mmtx.rtit[4] mmtx.vals[41] mmtx.vals[42] mmtx.vals[43] mmtx.vals[44] ~
mmtx.vals[45] mmtx.vals[46] mmtx.vals[47] mmtx.vals[48] mmtx.vals[49] ~
mmtx.vals[50] mmtx.rtit[5] mmtx.vals[51] mmtx.vals[52] mmtx.vals[53] ~
mmtx.vals[54] mmtx.vals[55] mmtx.vals[56] mmtx.vals[57] mmtx.vals[58] ~
mmtx.vals[59] mmtx.vals[60] mmtx.rtit[6] mmtx.vals[61] mmtx.vals[62] ~
mmtx.vals[63] mmtx.vals[64] mmtx.vals[65] mmtx.vals[66] mmtx.vals[67] ~
mmtx.vals[68] mmtx.vals[69] mmtx.vals[70] mmtx.rtit[7] mmtx.vals[71] ~
mmtx.vals[72] mmtx.vals[73] mmtx.vals[74] mmtx.vals[75] mmtx.vals[76] ~
mmtx.vals[77] mmtx.vals[78] mmtx.vals[79] mmtx.vals[80] mmtx.rtit[8] ~
mmtx.vals[81] mmtx.vals[82] mmtx.vals[83] mmtx.vals[84] mmtx.vals[85] ~
mmtx.vals[86] mmtx.vals[87] mmtx.vals[88] mmtx.vals[89] mmtx.vals[90] ~
mmtx.rtit[9] mmtx.vals[91] mmtx.vals[92] mmtx.vals[93] mmtx.vals[94] ~
mmtx.vals[95] mmtx.vals[96] mmtx.vals[97] mmtx.vals[98] mmtx.vals[99] ~
mmtx.vals[100] mmtx.rtit[10] mmtx.vals[101] mmtx.vals[102] mmtx.vals[103] ~
mmtx.vals[104] mmtx.vals[105] mmtx.vals[106] mmtx.vals[107] mmtx.vals[108] ~
mmtx.vals[109] mmtx.vals[110] mmtx.rtit[11] mmtx.vals[111] mmtx.vals[112] ~
mmtx.vals[113] mmtx.vals[114] mmtx.vals[115] mmtx.vals[116] mmtx.vals[117] ~
mmtx.vals[118] mmtx.vals[119] mmtx.vals[120] mmtx.rtit[12] mmtx.vals[121] ~
mmtx.vals[122] mmtx.vals[123] mmtx.vals[124] mmtx.vals[125] mmtx.vals[126] ~
mmtx.vals[127] mmtx.vals[128] mmtx.vals[129] mmtx.vals[130] mmtx.rtit[13] ~
mmtx.vals[131] mmtx.vals[132] mmtx.vals[133] mmtx.vals[134] mmtx.vals[135] ~
mmtx.vals[136] mmtx.vals[137] mmtx.vals[138] mmtx.vals[139] mmtx.vals[140] ~
mmtx.rtit[14] mmtx.vals[141] mmtx.vals[142] mmtx.vals[143] mmtx.vals[144] ~
mmtx.vals[145] mmtx.vals[146] mmtx.vals[147] mmtx.vals[148] mmtx.vals[149] ~
mmtx.vals[150] mmtx.rtit[15] mmtx.vals[151] mmtx.vals[152] mmtx.vals[153] ~
mmtx.vals[154] mmtx.vals[155] mmtx.vals[156] mmtx.vals[157] mmtx.vals[158] ~
mmtx.vals[159] mmtx.vals[160] 
&Scoped-define DISPLAYED-TABLES mmtx
&Scoped-define FIRST-DISPLAYED-TABLE mmtx
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
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE style_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 124 BY 18.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     mmtx.m-code AT ROW 1.24 COL 21 COLON-ALIGNED
          LABEL "Machine"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     mcode_dscr AT ROW 1.24 COL 34 COLON-ALIGNED NO-LABEL
     mmtx.style AT ROW 1.24 COL 76 COLON-ALIGNED
          LABEL "Style"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     style_dscr AT ROW 1.24 COL 89 COLON-ALIGNED NO-LABEL
     mmtx.c-title AT ROW 2.43 COL 32 COLON-ALIGNED NO-LABEL FORMAT "x(50)"
          VIEW-AS FILL-IN 
          SIZE 56 BY 1
     lv-page AT ROW 3.38 COL 2 NO-LABEL
     mmtx.col-value[1] AT ROW 3.38 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[2] AT ROW 3.38 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[3] AT ROW 3.38 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[4] AT ROW 3.38 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[5] AT ROW 3.38 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[6] AT ROW 3.38 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[7] AT ROW 3.38 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[8] AT ROW 3.38 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[9] AT ROW 3.38 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.col-value[10] AT ROW 3.38 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     mmtx.rtit[1] AT ROW 4.57 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.row-value[1] AT ROW 4.57 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[2] AT ROW 5.52 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[3] AT ROW 6.48 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[4] AT ROW 7.43 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[5] AT ROW 8.43 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[6] AT ROW 9.43 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[7] AT ROW 10.29 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[8] AT ROW 11.24 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[9] AT ROW 12.19 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.row-value[10] AT ROW 13.14 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[11] AT ROW 14.1 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[12] AT ROW 15.05 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[13] AT ROW 16 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[14] AT ROW 16.95 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.row-value[15] AT ROW 17.91 COL 4 COLON-ALIGNED NO-LABEL FORMAT ">>>>>>>9.<<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     mmtx.vals[11] AT ROW 4.57 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[12] AT ROW 4.57 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[13] AT ROW 4.57 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[14] AT ROW 4.57 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[15] AT ROW 4.57 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[16] AT ROW 4.57 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[17] AT ROW 4.57 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[18] AT ROW 4.57 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[19] AT ROW 4.57 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[20] AT ROW 4.57 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[2] AT ROW 5.57 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[21] AT ROW 5.52 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[22] AT ROW 5.52 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[23] AT ROW 5.52 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[24] AT ROW 5.52 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[25] AT ROW 5.52 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[26] AT ROW 5.52 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[27] AT ROW 5.52 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[28] AT ROW 5.52 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[29] AT ROW 5.52 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[30] AT ROW 5.52 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[3] AT ROW 6.48 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[31] AT ROW 6.48 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[32] AT ROW 6.48 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[33] AT ROW 6.48 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[34] AT ROW 6.48 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[35] AT ROW 6.48 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[36] AT ROW 6.48 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[37] AT ROW 6.48 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[38] AT ROW 6.48 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[39] AT ROW 6.48 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[40] AT ROW 6.48 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[4] AT ROW 7.43 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[41] AT ROW 7.43 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[42] AT ROW 7.43 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[43] AT ROW 7.43 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[44] AT ROW 7.43 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[45] AT ROW 7.43 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[46] AT ROW 7.43 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[47] AT ROW 7.43 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[48] AT ROW 7.43 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[49] AT ROW 7.43 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[50] AT ROW 7.43 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[5] AT ROW 8.38 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[51] AT ROW 8.38 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[52] AT ROW 8.38 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[53] AT ROW 8.38 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[54] AT ROW 8.38 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[55] AT ROW 8.38 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[56] AT ROW 8.38 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[57] AT ROW 8.38 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[58] AT ROW 8.38 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[59] AT ROW 8.38 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[60] AT ROW 8.38 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[6] AT ROW 9.33 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[61] AT ROW 9.33 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[62] AT ROW 9.33 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[63] AT ROW 9.33 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[64] AT ROW 9.33 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[65] AT ROW 9.33 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[66] AT ROW 9.33 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[67] AT ROW 9.33 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[68] AT ROW 9.33 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[69] AT ROW 9.33 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[70] AT ROW 9.33 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[7] AT ROW 10.29 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[71] AT ROW 10.29 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[72] AT ROW 10.29 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[73] AT ROW 10.29 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[74] AT ROW 10.29 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[75] AT ROW 10.29 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[76] AT ROW 10.29 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[77] AT ROW 10.29 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[78] AT ROW 10.29 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[79] AT ROW 10.29 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[80] AT ROW 10.29 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[8] AT ROW 11.24 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[81] AT ROW 11.24 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[82] AT ROW 11.24 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[83] AT ROW 11.24 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[84] AT ROW 11.24 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[85] AT ROW 11.24 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[86] AT ROW 11.24 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[87] AT ROW 11.24 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[88] AT ROW 11.24 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[89] AT ROW 11.24 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[90] AT ROW 11.24 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[9] AT ROW 12.19 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[91] AT ROW 12.19 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[92] AT ROW 12.19 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[93] AT ROW 12.19 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[94] AT ROW 12.19 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[95] AT ROW 12.19 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[96] AT ROW 12.19 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[97] AT ROW 12.19 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[98] AT ROW 12.19 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[99] AT ROW 12.19 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[100] AT ROW 12.19 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[10] AT ROW 13.14 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[101] AT ROW 13.14 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[102] AT ROW 13.14 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[103] AT ROW 13.14 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[104] AT ROW 13.14 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[105] AT ROW 13.14 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[106] AT ROW 13.14 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[107] AT ROW 13.14 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[108] AT ROW 13.14 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[109] AT ROW 13.14 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[110] AT ROW 13.14 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[11] AT ROW 14.1 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[111] AT ROW 14.1 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[112] AT ROW 14.1 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[113] AT ROW 14.1 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[114] AT ROW 14.1 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[115] AT ROW 14.1 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[116] AT ROW 14.1 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[117] AT ROW 14.1 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[118] AT ROW 14.1 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[119] AT ROW 14.1 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[120] AT ROW 14.1 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[12] AT ROW 15.05 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[121] AT ROW 15.05 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[122] AT ROW 15.05 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[123] AT ROW 15.05 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[124] AT ROW 15.05 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[125] AT ROW 15.05 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[126] AT ROW 15.05 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[127] AT ROW 15.05 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[128] AT ROW 15.05 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[129] AT ROW 15.05 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[130] AT ROW 15.05 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[13] AT ROW 16 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[131] AT ROW 16 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[132] AT ROW 16 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1.05
     mmtx.vals[133] AT ROW 16 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[134] AT ROW 16 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[135] AT ROW 16 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[136] AT ROW 16 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[137] AT ROW 16 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[138] AT ROW 16 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[139] AT ROW 16 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[140] AT ROW 16 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[14] AT ROW 16.95 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[141] AT ROW 16.95 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[142] AT ROW 16.95 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[143] AT ROW 16.95 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[144] AT ROW 16.95 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[145] AT ROW 16.95 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[146] AT ROW 16.95 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[147] AT ROW 16.95 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[148] AT ROW 16.95 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[149] AT ROW 16.95 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[150] AT ROW 16.95 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.rtit[15] AT ROW 17.91 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.8 BY 1
     mmtx.vals[151] AT ROW 17.91 COL 19 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[152] AT ROW 17.91 COL 29 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[153] AT ROW 17.91 COL 39 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[154] AT ROW 17.91 COL 49 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[155] AT ROW 17.91 COL 59 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[156] AT ROW 17.91 COL 69 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[157] AT ROW 17.91 COL 79 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[158] AT ROW 17.91 COL 89 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     mmtx.vals[159] AT ROW 17.91 COL 99 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     mmtx.vals[160] AT ROW 17.91 COL 109 COLON-ALIGNED NO-LABEL FORMAT ">99.99"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     RECT-8 AT ROW 1 COL 1
     "SPOIL" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 1.24 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.mmtx
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

/* SETTINGS FOR FILL-IN mmtx.c-title IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN mmtx.col-value[10] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[7] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[8] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.col-value[9] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN lv-page IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.m-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN mcode_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mmtx.row-value[10] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[11] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[12] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[13] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[14] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[15] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[7] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[8] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.row-value[9] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.rtit[10] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[11] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[12] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[13] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[14] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[15] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[1] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[2] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[3] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[4] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[5] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[6] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[7] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[8] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.rtit[9] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN mmtx.style IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN style_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mmtx.vals[100] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[101] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[102] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[103] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[104] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[105] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[106] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[107] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[108] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[109] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[110] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[111] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[112] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[113] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[114] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[115] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[116] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[117] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[118] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[119] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[11] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[120] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[121] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[122] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[123] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[124] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[125] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[126] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[127] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[128] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[129] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[12] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[130] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[131] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[132] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[133] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[134] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[135] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[136] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[137] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[138] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[139] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[13] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[140] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[141] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[142] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[143] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[144] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[145] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[146] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[147] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[148] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[149] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[14] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[150] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[151] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[152] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[153] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[154] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[155] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[156] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[157] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[158] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[159] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[15] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[160] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[16] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[17] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[18] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[19] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[20] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[21] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[22] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[23] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[24] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[25] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[26] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[27] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[28] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[29] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[30] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[31] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[32] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[33] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[34] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[35] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[36] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[37] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[38] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[39] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[40] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[41] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[42] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[43] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[44] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[45] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[46] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[47] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[48] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[49] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[50] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[51] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[52] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[53] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[54] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[55] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[56] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[57] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[58] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[59] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[60] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[61] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[62] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[63] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[64] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[65] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[66] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[67] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[68] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[69] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[70] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[71] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[72] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[73] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[74] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[75] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[76] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[77] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[78] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[79] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[80] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[81] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[82] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[83] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[84] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[85] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[86] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[87] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[88] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[89] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[90] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[91] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[92] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[93] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[94] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[95] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[96] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[97] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[98] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN mmtx.vals[99] IN FRAME F-Main
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
  {src/adm/template/row-list.i "mmtx"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mmtx"}

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
  DISPLAY mmtx.vals[11 FOR 150] WITH FRAME {&FRAME-NAME}.
  ENABLE  mmtx.vals[11 FOR 150] WITH FRAME {&FRAME-NAME}.

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
 if avail mmtx then do:
    lv-page:screen-value in frame {&frame-name} = 
              "Pg: " + trim(string(mmtx.page-no + 1,">9")) + "-" +
               trim(string(mmtx.across-no + 1,">9")).
    find first style where style.style = mmtx.style no-lock no-error.
    style_dscr:screen-value = if avail style then style.dscr else "".


  DEF VAR i AS INT NO-UNDO.
  DO i = 11 TO 160 WITH FRAME {&FRAME-NAME} :
     IF mmtx.vals[i] = 0 THEN
        CASE i :
            WHEN 11 THEN HIDE mmtx.vals[11]. 
            WHEN 12 THEN HIDE mmtx.vals[12]. 
            WHEN 13 THEN HIDE mmtx.vals[13]. 
            WHEN 14 THEN HIDE mmtx.vals[14]. 
            WHEN 15 THEN HIDE mmtx.vals[15]. 
            WHEN 16 THEN HIDE mmtx.vals[16]. 
            WHEN 17 THEN HIDE mmtx.vals[17]. 
            WHEN 18 THEN HIDE mmtx.vals[18]. 
            WHEN 19 THEN HIDE mmtx.vals[19]. 
            WHEN 20 THEN HIDE mmtx.vals[20]. 
            WHEN 21 THEN HIDE mmtx.vals[21]. 
            WHEN 22 THEN HIDE mmtx.vals[22]. 
            WHEN 23 THEN HIDE mmtx.vals[23]. 
            WHEN 24 THEN HIDE mmtx.vals[24]. 
            WHEN 25 THEN HIDE mmtx.vals[25]. 
            WHEN 26 THEN HIDE mmtx.vals[26]. 
            WHEN 27 THEN HIDE mmtx.vals[27]. 
            WHEN 28 THEN HIDE mmtx.vals[28]. 
            WHEN 29 THEN HIDE mmtx.vals[29]. 
            WHEN 30 THEN HIDE mmtx.vals[30]. 
            WHEN 31 THEN HIDE mmtx.vals[31]. 
            WHEN 32 THEN HIDE mmtx.vals[32]. 
            WHEN 33 THEN HIDE mmtx.vals[33]. 
            WHEN 34 THEN HIDE mmtx.vals[34]. 
            WHEN 35 THEN HIDE mmtx.vals[35]. 
            WHEN 36 THEN HIDE mmtx.vals[36]. 
            WHEN 37 THEN HIDE mmtx.vals[37]. 
            WHEN 38 THEN HIDE mmtx.vals[38]. 
            WHEN 39 THEN HIDE mmtx.vals[39]. 
            WHEN 40 THEN HIDE mmtx.vals[40]. 
            WHEN 41 THEN HIDE mmtx.vals[41]. 
            WHEN 42 THEN HIDE mmtx.vals[42]. 
            WHEN 43 THEN HIDE mmtx.vals[43]. 
            WHEN 44 THEN HIDE mmtx.vals[44]. 
            WHEN 45 THEN HIDE mmtx.vals[45]. 
            WHEN 46 THEN HIDE mmtx.vals[46]. 
            WHEN 47 THEN HIDE mmtx.vals[47]. 
            WHEN 48 THEN HIDE mmtx.vals[48]. 
            WHEN 49 THEN HIDE mmtx.vals[49].             
            WHEN 50 THEN HIDE mmtx.vals[50]. 
            WHEN 51 THEN HIDE mmtx.vals[51]. 
            WHEN 52 THEN HIDE mmtx.vals[52]. 
            WHEN 53 THEN HIDE mmtx.vals[53]. 
            WHEN 54 THEN HIDE mmtx.vals[54]. 
            WHEN 55 THEN HIDE mmtx.vals[55]. 
            WHEN 56 THEN HIDE mmtx.vals[56]. 
            WHEN 57 THEN HIDE mmtx.vals[57]. 
            WHEN 58 THEN HIDE mmtx.vals[58]. 
            WHEN 59 THEN HIDE mmtx.vals[59]. 
            WHEN 60 THEN HIDE mmtx.vals[60]. 
            WHEN 61 THEN HIDE mmtx.vals[61]. 
            WHEN 62 THEN HIDE mmtx.vals[62]. 
            WHEN 63 THEN HIDE mmtx.vals[63]. 
            WHEN 64 THEN HIDE mmtx.vals[64]. 
            WHEN 65 THEN HIDE mmtx.vals[65]. 
            WHEN 66 THEN HIDE mmtx.vals[66]. 
            WHEN 67 THEN HIDE mmtx.vals[67]. 
            WHEN 68 THEN HIDE mmtx.vals[68]. 
            WHEN 69 THEN HIDE mmtx.vals[69]. 
            WHEN 70 THEN HIDE mmtx.vals[70]. 
            WHEN 71 THEN HIDE mmtx.vals[71]. 
            WHEN 72 THEN HIDE mmtx.vals[72]. 
            WHEN 73 THEN HIDE mmtx.vals[73]. 
            WHEN 74 THEN HIDE mmtx.vals[74]. 
            WHEN 75 THEN HIDE mmtx.vals[75]. 
            WHEN 76 THEN HIDE mmtx.vals[76]. 
            WHEN 77 THEN HIDE mmtx.vals[77]. 
            WHEN 78 THEN HIDE mmtx.vals[78]. 
            WHEN 79 THEN HIDE mmtx.vals[79].             
            WHEN 80 THEN HIDE mmtx.vals[80]. 
            WHEN 81 THEN HIDE mmtx.vals[81]. 
            WHEN 82 THEN HIDE mmtx.vals[82]. 
            WHEN 83 THEN HIDE mmtx.vals[83]. 
            WHEN 84 THEN HIDE mmtx.vals[84]. 
            WHEN 85 THEN HIDE mmtx.vals[85]. 
            WHEN 86 THEN HIDE mmtx.vals[86]. 
            WHEN 87 THEN HIDE mmtx.vals[87]. 
            WHEN 88 THEN HIDE mmtx.vals[88]. 
            WHEN 89 THEN HIDE mmtx.vals[89]. 
            WHEN 90 THEN HIDE mmtx.vals[90]. 
            WHEN 91 THEN HIDE mmtx.vals[91]. 
            WHEN 92 THEN HIDE mmtx.vals[92]. 
            WHEN 93 THEN HIDE mmtx.vals[93]. 
            WHEN 94 THEN HIDE mmtx.vals[94]. 
            WHEN 95 THEN HIDE mmtx.vals[95]. 
            WHEN 96 THEN HIDE mmtx.vals[96]. 
            WHEN 97 THEN HIDE mmtx.vals[97]. 
            WHEN 98 THEN HIDE mmtx.vals[98]. 
            WHEN 99 THEN HIDE mmtx.vals[99]. 
            WHEN 100 THEN HIDE mmtx.vals[100]. 
            WHEN 101 THEN HIDE mmtx.vals[101]. 
            WHEN 102 THEN HIDE mmtx.vals[102]. 
            WHEN 103 THEN HIDE mmtx.vals[103]. 
            WHEN 104 THEN HIDE mmtx.vals[104]. 
            WHEN 105 THEN HIDE mmtx.vals[105]. 
            WHEN 106 THEN HIDE mmtx.vals[106]. 
            WHEN 107 THEN HIDE mmtx.vals[107]. 
            WHEN 108 THEN HIDE mmtx.vals[108]. 
            WHEN 109 THEN HIDE mmtx.vals[109]. 
            WHEN 110 THEN HIDE mmtx.vals[110]. 
            WHEN 111 THEN HIDE mmtx.vals[111]. 
            WHEN 112 THEN HIDE mmtx.vals[112]. 
            WHEN 113 THEN HIDE mmtx.vals[113]. 
            WHEN 114 THEN HIDE mmtx.vals[114]. 
            WHEN 115 THEN HIDE mmtx.vals[115]. 
            WHEN 116 THEN HIDE mmtx.vals[116]. 
            WHEN 117 THEN HIDE mmtx.vals[117]. 
            WHEN 118 THEN HIDE mmtx.vals[118]. 
            WHEN 119 THEN HIDE mmtx.vals[119]. 
            WHEN 120 THEN HIDE mmtx.vals[120]. 
            WHEN 121 THEN HIDE mmtx.vals[121]. 
            WHEN 122 THEN HIDE mmtx.vals[122]. 
            WHEN 123 THEN HIDE mmtx.vals[123]. 
            WHEN 124 THEN HIDE mmtx.vals[124]. 
            WHEN 125 THEN HIDE mmtx.vals[125]. 
            WHEN 126 THEN HIDE mmtx.vals[126]. 
            WHEN 127 THEN HIDE mmtx.vals[127]. 
            WHEN 128 THEN HIDE mmtx.vals[128]. 
            WHEN 129 THEN HIDE mmtx.vals[129]. 
            WHEN 130 THEN HIDE mmtx.vals[130]. 
            WHEN 131 THEN HIDE mmtx.vals[131]. 
            WHEN 132 THEN HIDE mmtx.vals[132]. 
            WHEN 133 THEN HIDE mmtx.vals[133]. 
            WHEN 134 THEN HIDE mmtx.vals[134]. 
            WHEN 135 THEN HIDE mmtx.vals[135]. 
            WHEN 136 THEN HIDE mmtx.vals[136]. 
            WHEN 137 THEN HIDE mmtx.vals[137]. 
            WHEN 138 THEN HIDE mmtx.vals[138]. 
            WHEN 139 THEN HIDE mmtx.vals[139]. 
            WHEN 140 THEN HIDE mmtx.vals[140]. 
            WHEN 141 THEN HIDE mmtx.vals[141]. 
            WHEN 142 THEN HIDE mmtx.vals[142]. 
            WHEN 143 THEN HIDE mmtx.vals[143]. 
            WHEN 144 THEN HIDE mmtx.vals[144]. 
            WHEN 145 THEN HIDE mmtx.vals[145]. 
            WHEN 146 THEN HIDE mmtx.vals[146]. 
            WHEN 147 THEN HIDE mmtx.vals[147]. 
            WHEN 148 THEN HIDE mmtx.vals[148]. 
            WHEN 149 THEN HIDE mmtx.vals[149]. 
            WHEN 150 THEN HIDE mmtx.vals[150]. 
            WHEN 151 THEN HIDE mmtx.vals[151]. 
            WHEN 152 THEN HIDE mmtx.vals[152]. 
            WHEN 153 THEN HIDE mmtx.vals[153]. 
            WHEN 154 THEN HIDE mmtx.vals[154]. 
            WHEN 155 THEN HIDE mmtx.vals[155]. 
            WHEN 156 THEN HIDE mmtx.vals[156]. 
            WHEN 157 THEN HIDE mmtx.vals[157]. 
            WHEN 158 THEN HIDE mmtx.vals[158]. 
            WHEN 159 THEN HIDE mmtx.vals[159]. 
            WHEN 160 THEN HIDE mmtx.vals[160]. 
        END.
  END.
end.
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
  {src/adm/template/snd-list.i "mmtx"}

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

