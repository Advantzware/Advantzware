&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

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
{custom/gcompany.i}
{custom/gloc.i}

&Scoped-define stack-flute-enable enable-stack-flute

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
&Scoped-define EXTERNAL-TABLES stack-flute flute
&Scoped-define FIRST-EXTERNAL-TABLE stack-flute


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR stack-flute, flute.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS stack-flute.pallet stack-flute.row-value[1] ~
stack-flute.vals[11] stack-flute.vals[12] stack-flute.vals[13] ~
stack-flute.vals[14] stack-flute.vals[15] stack-flute.vals[16] ~
stack-flute.vals[17] stack-flute.vals[18] stack-flute.vals[19] ~
stack-flute.vals[20] stack-flute.row-value[2] stack-flute.vals[21] ~
stack-flute.vals[22] stack-flute.vals[23] stack-flute.vals[24] ~
stack-flute.vals[25] stack-flute.vals[26] stack-flute.vals[27] ~
stack-flute.vals[28] stack-flute.vals[29] stack-flute.vals[30] ~
stack-flute.row-value[3] stack-flute.vals[31] stack-flute.vals[32] ~
stack-flute.vals[33] stack-flute.vals[34] stack-flute.vals[35] ~
stack-flute.vals[36] stack-flute.vals[37] stack-flute.vals[38] ~
stack-flute.vals[39] stack-flute.vals[40] stack-flute.row-value[4] ~
stack-flute.vals[41] stack-flute.vals[42] stack-flute.vals[43] ~
stack-flute.vals[44] stack-flute.vals[45] stack-flute.vals[46] ~
stack-flute.vals[47] stack-flute.vals[48] stack-flute.vals[49] ~
stack-flute.vals[50] stack-flute.row-value[5] stack-flute.vals[51] ~
stack-flute.vals[52] stack-flute.vals[53] stack-flute.vals[54] ~
stack-flute.vals[55] stack-flute.vals[56] stack-flute.vals[57] ~
stack-flute.vals[58] stack-flute.vals[59] stack-flute.vals[60] ~
stack-flute.row-value[6] stack-flute.vals[61] stack-flute.vals[62] ~
stack-flute.vals[63] stack-flute.vals[64] stack-flute.vals[65] ~
stack-flute.vals[66] stack-flute.vals[67] stack-flute.vals[68] ~
stack-flute.vals[69] stack-flute.vals[70] stack-flute.row-value[7] ~
stack-flute.vals[71] stack-flute.vals[72] stack-flute.vals[73] ~
stack-flute.vals[74] stack-flute.vals[75] stack-flute.vals[76] ~
stack-flute.vals[77] stack-flute.vals[78] stack-flute.vals[79] ~
stack-flute.vals[80] stack-flute.row-value[8] stack-flute.vals[81] ~
stack-flute.vals[82] stack-flute.vals[83] stack-flute.vals[84] ~
stack-flute.vals[85] stack-flute.vals[86] stack-flute.vals[87] ~
stack-flute.vals[88] stack-flute.vals[89] stack-flute.vals[90] ~
stack-flute.row-value[9] stack-flute.vals[91] stack-flute.vals[92] ~
stack-flute.vals[93] stack-flute.vals[94] stack-flute.vals[95] ~
stack-flute.vals[96] stack-flute.vals[97] stack-flute.vals[98] ~
stack-flute.vals[99] stack-flute.vals[100] stack-flute.row-value[10] ~
stack-flute.vals[101] stack-flute.vals[102] stack-flute.vals[103] ~
stack-flute.vals[104] stack-flute.vals[105] stack-flute.vals[106] ~
stack-flute.vals[107] stack-flute.vals[108] stack-flute.vals[109] ~
stack-flute.vals[110] stack-flute.vals[111] stack-flute.vals[112] ~
stack-flute.vals[113] stack-flute.vals[114] stack-flute.row-value[11] ~
stack-flute.vals[115] stack-flute.vals[116] stack-flute.vals[117] ~
stack-flute.vals[118] stack-flute.vals[119] stack-flute.vals[120] ~
stack-flute.vals[121] stack-flute.vals[122] stack-flute.vals[123] ~
stack-flute.vals[124] stack-flute.vals[125] stack-flute.row-value[12] ~
stack-flute.vals[126] stack-flute.vals[127] stack-flute.vals[128] ~
stack-flute.vals[129] stack-flute.vals[130] stack-flute.vals[131] ~
stack-flute.vals[132] stack-flute.vals[133] stack-flute.row-value[13] ~
stack-flute.vals[134] stack-flute.vals[135] stack-flute.vals[136] ~
stack-flute.vals[137] stack-flute.vals[138] stack-flute.vals[139] ~
stack-flute.vals[140] stack-flute.vals[141] stack-flute.vals[142] ~
stack-flute.vals[143] stack-flute.row-value[14] stack-flute.vals[144] ~
stack-flute.vals[145] stack-flute.vals[146] stack-flute.vals[147] ~
stack-flute.vals[148] stack-flute.vals[149] stack-flute.vals[150] ~
stack-flute.row-value[15] stack-flute.vals[151] stack-flute.vals[152] ~
stack-flute.vals[153] stack-flute.vals[154] stack-flute.vals[155] ~
stack-flute.vals[156] stack-flute.vals[157] stack-flute.vals[158] ~
stack-flute.vals[159] stack-flute.vals[160] 
&Scoped-define ENABLED-TABLES stack-flute
&Scoped-define FIRST-ENABLED-TABLE stack-flute
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS stack-flute.code stack-flute.pallet ~
stack-flute.c-title stack-flute.page-no stack-flute.col-value[1] ~
stack-flute.col-value[2] stack-flute.col-value[3] stack-flute.col-value[4] ~
stack-flute.col-value[5] stack-flute.col-value[6] stack-flute.col-value[7] ~
stack-flute.col-value[8] stack-flute.col-value[9] stack-flute.col-value[10] ~
stack-flute.rtit[1] stack-flute.row-value[1] stack-flute.vals[11] ~
stack-flute.vals[12] stack-flute.vals[13] stack-flute.vals[14] ~
stack-flute.vals[15] stack-flute.vals[16] stack-flute.vals[17] ~
stack-flute.vals[18] stack-flute.vals[19] stack-flute.vals[20] ~
stack-flute.rtit[2] stack-flute.row-value[2] stack-flute.vals[21] ~
stack-flute.vals[22] stack-flute.vals[23] stack-flute.vals[24] ~
stack-flute.vals[25] stack-flute.vals[26] stack-flute.vals[27] ~
stack-flute.vals[28] stack-flute.vals[29] stack-flute.vals[30] ~
stack-flute.rtit[3] stack-flute.row-value[3] stack-flute.vals[31] ~
stack-flute.vals[32] stack-flute.vals[33] stack-flute.vals[34] ~
stack-flute.vals[35] stack-flute.vals[36] stack-flute.vals[37] ~
stack-flute.vals[38] stack-flute.vals[39] stack-flute.vals[40] ~
stack-flute.rtit[4] stack-flute.row-value[4] stack-flute.vals[41] ~
stack-flute.vals[42] stack-flute.vals[43] stack-flute.vals[44] ~
stack-flute.vals[45] stack-flute.vals[46] stack-flute.vals[47] ~
stack-flute.vals[48] stack-flute.vals[49] stack-flute.vals[50] ~
stack-flute.rtit[5] stack-flute.row-value[5] stack-flute.vals[51] ~
stack-flute.vals[52] stack-flute.vals[53] stack-flute.vals[54] ~
stack-flute.vals[55] stack-flute.vals[56] stack-flute.vals[57] ~
stack-flute.vals[58] stack-flute.vals[59] stack-flute.vals[60] ~
stack-flute.rtit[6] stack-flute.row-value[6] stack-flute.vals[61] ~
stack-flute.vals[62] stack-flute.vals[63] stack-flute.vals[64] ~
stack-flute.vals[65] stack-flute.vals[66] stack-flute.vals[67] ~
stack-flute.vals[68] stack-flute.vals[69] stack-flute.vals[70] ~
stack-flute.rtit[7] stack-flute.row-value[7] stack-flute.vals[71] ~
stack-flute.vals[72] stack-flute.vals[73] stack-flute.vals[74] ~
stack-flute.vals[75] stack-flute.vals[76] stack-flute.vals[77] ~
stack-flute.vals[78] stack-flute.vals[79] stack-flute.vals[80] ~
stack-flute.rtit[8] stack-flute.row-value[8] stack-flute.vals[81] ~
stack-flute.vals[82] stack-flute.vals[83] stack-flute.vals[84] ~
stack-flute.vals[85] stack-flute.vals[86] stack-flute.vals[87] ~
stack-flute.vals[88] stack-flute.vals[89] stack-flute.vals[90] ~
stack-flute.rtit[9] stack-flute.row-value[9] stack-flute.vals[91] ~
stack-flute.vals[92] stack-flute.vals[93] stack-flute.vals[94] ~
stack-flute.vals[95] stack-flute.vals[96] stack-flute.vals[97] ~
stack-flute.vals[98] stack-flute.vals[99] stack-flute.vals[100] ~
stack-flute.rtit[10] stack-flute.row-value[10] stack-flute.vals[101] ~
stack-flute.vals[102] stack-flute.vals[103] stack-flute.vals[104] ~
stack-flute.vals[105] stack-flute.vals[106] stack-flute.vals[107] ~
stack-flute.vals[108] stack-flute.vals[109] stack-flute.vals[110] ~
stack-flute.vals[111] stack-flute.vals[112] stack-flute.vals[113] ~
stack-flute.vals[114] stack-flute.rtit[11] stack-flute.row-value[11] ~
stack-flute.vals[115] stack-flute.vals[116] stack-flute.vals[117] ~
stack-flute.vals[118] stack-flute.vals[119] stack-flute.vals[120] ~
stack-flute.vals[121] stack-flute.vals[122] stack-flute.vals[123] ~
stack-flute.vals[124] stack-flute.vals[125] stack-flute.rtit[12] ~
stack-flute.row-value[12] stack-flute.vals[126] stack-flute.vals[127] ~
stack-flute.vals[128] stack-flute.vals[129] stack-flute.vals[130] ~
stack-flute.vals[131] stack-flute.vals[132] stack-flute.vals[133] ~
stack-flute.rtit[13] stack-flute.row-value[13] stack-flute.vals[134] ~
stack-flute.vals[135] stack-flute.vals[136] stack-flute.vals[137] ~
stack-flute.vals[138] stack-flute.vals[139] stack-flute.vals[140] ~
stack-flute.vals[141] stack-flute.vals[142] stack-flute.vals[143] ~
stack-flute.rtit[14] stack-flute.row-value[14] stack-flute.vals[144] ~
stack-flute.vals[145] stack-flute.vals[146] stack-flute.vals[147] ~
stack-flute.vals[148] stack-flute.vals[149] stack-flute.vals[150] ~
stack-flute.row-value[15] stack-flute.vals[151] stack-flute.vals[152] ~
stack-flute.vals[153] stack-flute.vals[154] stack-flute.vals[155] ~
stack-flute.vals[156] stack-flute.vals[157] stack-flute.vals[158] ~
stack-flute.vals[159] stack-flute.vals[160] stack-flute.rtit[15] 
&Scoped-define DISPLAYED-TABLES stack-flute
&Scoped-define FIRST-DISPLAYED-TABLE stack-flute


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS stack-flute.page-no 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 141 BY 17.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     stack-flute.code AT ROW 1.24 COL 18 COLON-ALIGNED
          LABEL "Flute Code"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.pallet AT ROW 1.24 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     stack-flute.c-title AT ROW 1.24 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     stack-flute.page-no AT ROW 1.24 COL 116 COLON-ALIGNED
          LABEL "Page#"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     stack-flute.col-value[1] AT ROW 3.14 COL 18 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[2] AT ROW 3.14 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[3] AT ROW 3.14 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[4] AT ROW 3.14 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[5] AT ROW 3.14 COL 66 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[6] AT ROW 3.14 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[7] AT ROW 3.14 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[8] AT ROW 3.14 COL 102 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[9] AT ROW 3.14 COL 114 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.col-value[10] AT ROW 3.14 COL 126 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.rtit[1] AT ROW 4.1 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[1] AT ROW 4.1 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[11] AT ROW 4.1 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[12] AT ROW 4.1 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[13] AT ROW 4.1 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[14] AT ROW 4.1 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[15] AT ROW 4.1 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[16] AT ROW 4.1 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[17] AT ROW 4.1 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[18] AT ROW 4.1 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[19] AT ROW 4.1 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[20] AT ROW 4.1 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[2] AT ROW 5.05 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.row-value[2] AT ROW 5.05 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[21] AT ROW 5.05 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[22] AT ROW 5.05 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[23] AT ROW 5.05 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[24] AT ROW 5.05 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[25] AT ROW 5.05 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[26] AT ROW 5.05 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[27] AT ROW 5.05 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[28] AT ROW 5.05 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[29] AT ROW 5.05 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[30] AT ROW 5.05 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[3] AT ROW 6 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[3] AT ROW 6 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[31] AT ROW 6 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[32] AT ROW 6 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[33] AT ROW 6 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[34] AT ROW 6 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[35] AT ROW 6 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[36] AT ROW 6 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[37] AT ROW 6 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[38] AT ROW 6 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[39] AT ROW 6 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[40] AT ROW 6 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[4] AT ROW 6.95 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[4] AT ROW 6.95 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[41] AT ROW 6.95 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[42] AT ROW 6.95 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[43] AT ROW 6.95 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[44] AT ROW 6.95 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[45] AT ROW 6.95 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[46] AT ROW 6.95 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[47] AT ROW 6.95 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[48] AT ROW 6.95 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[49] AT ROW 6.95 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[50] AT ROW 6.95 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[5] AT ROW 7.91 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[5] AT ROW 7.91 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[51] AT ROW 7.91 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[52] AT ROW 7.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[53] AT ROW 7.91 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[54] AT ROW 7.91 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[55] AT ROW 7.91 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[56] AT ROW 7.91 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[57] AT ROW 7.91 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[58] AT ROW 7.91 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[59] AT ROW 7.91 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[60] AT ROW 7.91 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[6] AT ROW 8.86 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[6] AT ROW 8.86 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[61] AT ROW 8.86 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[62] AT ROW 8.86 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[63] AT ROW 8.86 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[64] AT ROW 8.86 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[65] AT ROW 8.86 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[66] AT ROW 8.86 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[67] AT ROW 8.86 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[68] AT ROW 8.86 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[69] AT ROW 8.86 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[70] AT ROW 8.86 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[7] AT ROW 9.81 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[7] AT ROW 9.81 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[71] AT ROW 9.81 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[72] AT ROW 9.81 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[73] AT ROW 9.81 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[74] AT ROW 9.81 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[75] AT ROW 9.81 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[76] AT ROW 9.81 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[77] AT ROW 9.81 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[78] AT ROW 9.81 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[79] AT ROW 9.81 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[80] AT ROW 9.81 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[8] AT ROW 10.76 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[8] AT ROW 10.76 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[81] AT ROW 10.76 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[82] AT ROW 10.76 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[83] AT ROW 10.76 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[84] AT ROW 10.76 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[85] AT ROW 10.76 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[86] AT ROW 10.76 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[87] AT ROW 10.76 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[88] AT ROW 10.76 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[89] AT ROW 10.76 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[90] AT ROW 10.76 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[9] AT ROW 11.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[9] AT ROW 11.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[91] AT ROW 11.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[92] AT ROW 11.71 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[93] AT ROW 11.71 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[94] AT ROW 11.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[95] AT ROW 11.71 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[96] AT ROW 11.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[97] AT ROW 11.71 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[98] AT ROW 11.71 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[99] AT ROW 11.71 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[100] AT ROW 11.71 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[10] AT ROW 12.67 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[10] AT ROW 12.67 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[101] AT ROW 12.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[102] AT ROW 12.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[103] AT ROW 12.67 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[104] AT ROW 12.67 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[105] AT ROW 12.67 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[106] AT ROW 12.67 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[107] AT ROW 12.67 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[108] AT ROW 12.67 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[109] AT ROW 12.67 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[110] AT ROW 12.67 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    stack-flute.rtit[11] AT ROW 13.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[11] AT ROW 13.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[111] AT ROW 13.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[112] AT ROW 13.71 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[113] AT ROW 13.71 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[114] AT ROW 13.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[115] AT ROW 13.71 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[116] AT ROW 13.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[117] AT ROW 13.71 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[118] AT ROW 13.71 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[119] AT ROW 13.71 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[120] AT ROW 13.71 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    stack-flute.rtit[12] AT ROW 14.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[12] AT ROW 14.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[121] AT ROW 14.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[122] AT ROW 14.71 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[123] AT ROW 14.71 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[124] AT ROW 14.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[125] AT ROW 14.71 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1

     stack-flute.vals[126] AT ROW 14.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[127] AT ROW 14.71 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[128] AT ROW 14.71 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[129] AT ROW 14.71 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[130] AT ROW 14.71 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[13] AT ROW 15.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[13] AT ROW 15.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[131] AT ROW 15.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[132] AT ROW 15.71 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[133] AT ROW 15.71 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1

     stack-flute.vals[134] AT ROW 15.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[135] AT ROW 15.71 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[136] AT ROW 15.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[137] AT ROW 15.71 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[138] AT ROW 15.71 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[139] AT ROW 15.71 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[140] AT ROW 15.71 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    stack-flute.rtit[14] AT ROW 16.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[14] AT ROW 16.71 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[141] AT ROW 16.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[142] AT ROW 16.71 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[143] AT ROW 16.71 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1

     stack-flute.vals[144] AT ROW 16.71 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[145] AT ROW 16.71 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[146] AT ROW 16.71 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[147] AT ROW 16.71 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[148] AT ROW 16.71 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     stack-flute.vals[149] AT ROW 16.71 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[150] AT ROW 16.71 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.rtit[15] AT ROW 17.71 COL 3 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     stack-flute.row-value[15] AT ROW 17.67 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     stack-flute.vals[151] AT ROW 17.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[152] AT ROW 17.67 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[153] AT ROW 17.67 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[154] AT ROW 17.67 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[155] AT ROW 17.67 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[156] AT ROW 17.67 COL 80 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[157] AT ROW 17.67 COL 92 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[158] AT ROW 17.67 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     stack-flute.vals[159] AT ROW 17.67 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1      
     stack-flute.vals[160] AT ROW 17.67 COL 128 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1

     RECT-1 AT ROW 1 COL 1
     "TEST" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.14 COL 9
     "S t a c k i n g      P a t t e r n s" VIEW-AS TEXT
          SIZE 50 BY .62 AT ROW 2.43 COL 35
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.stack-flute,ASI.flute
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
         HEIGHT             = 24.19
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN stack-flute.c-title IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN stack-flute.col-value[10] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[5] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[6] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[7] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[8] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.col-value[9] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN stack-flute.page-no IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN stack-flute.rtit[10] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[11] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[12] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[13] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[14] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[15] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[1] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[2] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[3] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[4] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[5] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[6] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[7] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[8] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN stack-flute.rtit[9] IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    def var char-val as cha no-undo.

    case focus:name :
         when 'pallet' then do:
               run windows/l-itemp.w (gcompany, "", focus:screen-value, output char-val).
               if char-val <> "" then 
                   assign focus:screen-value = entry(1, char-val)
                          stack-flute.c-title:screen-value = entry(2,char-val).                       
         end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME stack-flute.pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stack-flute.pallet V-table-Win
ON LEAVE OF stack-flute.pallet IN FRAME F-Main /* Pallet */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-pallet NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
session:data-entry-return = true.
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
  {src/adm/template/row-list.i "stack-flute"}
  {src/adm/template/row-list.i "flute"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "stack-flute"}
  {src/adm/template/row-find.i "flute"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation V-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation V-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-stack-flute V-table-Win 
PROCEDURE enable-stack-flute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT adm-new-record THEN DISABLE stack-flute.pallet.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-s-f FOR stack-flute.

  DEF VAR lv-row-value LIKE stack-flute.row-value NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO li = 1 TO EXTENT(lv-row-value):
    lv-row-value[li] = stack-flute.row-value[li].
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* This will remove any row-value that has been removed from this record
     from the other pages of records with same pallet code */
  DISABLE TRIGGERS FOR LOAD OF b-s-f.

  DO li = 1 TO EXTENT(lv-row-value):
    IF lv-row-value[li] NE "" THEN DO:
      ll = YES.
      DO lj = 1 TO EXTENT(stack-flute.row-value):
        IF lv-row-value[li] EQ stack-flute.row-value[lj] THEN DO:
          ll = NO.
          LEAVE.
        END.
      END.

      IF ll THEN
      FOR EACH b-s-f
          WHERE b-s-f.company EQ stack-flute.company
            AND b-s-f.loc     EQ stack-flute.loc
            AND b-s-f.code    EQ stack-flute.code
            AND b-s-f.pallet  EQ stack-flute.pallet:
        DO lj = 1 TO EXTENT(b-s-f.row-value):
          IF lv-row-value[li] EQ b-s-f.row-value[lj] THEN DO:
            b-s-f.row-value[lj] = "".
          END.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:
------------------------------------------------------------------------------*/
  def buffer alt-reftable for reftable.
  def var cts-count as int no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  find ce-ctrl where ce-ctrl.company = gcompany and
                     ce-ctrl.loc = gloc
                     no-lock no-error.
  assign stack-flute.company = gcompany
         stack-flute.loc = gloc
         stack-flute.code = flute.code
         stack-flute.page-no = 0
         /*stack-flute.pallet = ce-ctrl.def-pal*/
         .

  if adm-adding-record then do:       
     display stack-flute.code /*stack-flute.pallet */ with frame {&frame-name}.

    ctsloop:
      do cts-count = 1 to extent(stack-flute.col-value):
        find next alt-reftable where  alt-reftable.company = "" /* KLUDGE stack-flute.company ***/ and
                                      alt-reftable.loc     = "" /* KLUDGE stack-flute.loc ***/ and
                                      alt-reftable.reftable = "STACK" 
                                      no-lock no-error.
        if avail(alt-reftable) then
          assign stack-flute.col-value[cts-count] = alt-reftable.code.
        else leave ctsloop.
    end.

    display stack-flute.code stack-flute.pallet 
          stack-flute.col-value[1 for 10]
          with frame {&frame-name}.
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
  DEF BUFFER b-s-f FOR stack-flute.

  DEF VAR char-hdl AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-pallet NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FIND b-s-f NO-LOCK WHERE ROWID(b-s-f) EQ ROWID(stack-flute) NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("open-query").

    FIND LAST stack-flute NO-LOCK
        WHERE stack-flute.company EQ b-s-f.company
          AND stack-flute.loc     EQ b-s-f.loc
          AND stack-flute.code    EQ b-s-f.code
          AND stack-flute.pallet  EQ b-s-f.pallet
          AND stack-flute.page-no LE b-s-f.page-no
        NO-ERROR.

    IF AVAIL stack-flute THEN
      RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(stack-flute)).
  END.

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
  {src/adm/template/snd-list.i "stack-flute"}
  {src/adm/template/snd-list.i "flute"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pallet V-table-Win 
PROCEDURE valid-pallet PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-s-f FOR stack-flute.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF adm-new-record THEN DO:
      IF lv-msg EQ ""                             AND
         NOT CAN-FIND(FIRST item
                      WHERE item.company  EQ stack-flute.company
                        AND item.i-no     EQ stack-flute.pallet:SCREEN-VALUE 
                        AND item.mat-type EQ "D") THEN
        lv-msg = "is invalid, try help".

      IF lv-msg EQ "" AND stack-flute.page-no EQ 0 THEN DO:
        FOR EACH b-s-f NO-LOCK
            WHERE b-s-f.company EQ stack-flute.company
              AND b-s-f.loc     EQ stack-flute.loc
              AND b-s-f.code    EQ stack-flute.code
              AND b-s-f.pallet  EQ stack-flute.pallet:SCREEN-VALUE
              AND ROWID(b-s-f)  NE ROWID(stack-flute)
            BY b-s-f.page-no:
          DO li = 1 TO EXTENT(b-s-f.row-value) - 1:
            IF b-s-f.row-value[li] EQ "" THEN DO:
              li = EXTENT(b-s-f.row-value) + 2.
              LEAVE.
            END.
          END.
          IF li EQ EXTENT(b-s-f.row-value) + 2 THEN DO:
            RUN dispatch ("delete-record").
            FIND stack-flute WHERE ROWID(stack-flute) EQ ROWID(b-s-f).
            RUN dispatch ("display-fields").
            li = 0.
            LEAVE.
          END.
          stack-flute.page-no:SCREEN-VALUE = STRING(b-s-f.page-no + 1).
        END.
      END.
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(stack-flute.pallet:LABEL) + " " + TRIM(lv-msg) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO stack-flute.pallet.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

