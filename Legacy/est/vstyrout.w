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
{custom/gcompany.i}
{custom/gloc.i}
{custom/globdefs.i}
assign gcompany = g_company
       gloc = g_loc.

DEF VAR lv-save-format AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES routing-mtx style
&Scoped-define FIRST-EXTERNAL-TABLE routing-mtx


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR routing-mtx, style.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS routing-mtx.msf routing-mtx.dim-type ~
routing-mtx.bl-len[1] routing-mtx.bl-len[2] routing-mtx.bl-len[3] ~
routing-mtx.bl-len[4] routing-mtx.bl-len[5] routing-mtx.bl-len[6] ~
routing-mtx.bl-len[7] routing-mtx.bl-len[8] routing-mtx.bl-len[9] ~
routing-mtx.bl-len[10] routing-mtx.bl-wid[1] routing-mtx.bl-wid[2] ~
routing-mtx.bl-wid[3] routing-mtx.bl-wid[4] routing-mtx.bl-wid[5] ~
routing-mtx.bl-wid[6] routing-mtx.bl-wid[7] routing-mtx.bl-wid[8] ~
routing-mtx.bl-wid[9] routing-mtx.bl-wid[10] routing-mtx.r-code[1] ~
routing-mtx.r-code[2] routing-mtx.r-code[3] routing-mtx.r-code[4] ~
routing-mtx.r-code[5] routing-mtx.r-code[6] routing-mtx.r-code[7] ~
routing-mtx.r-code[8] routing-mtx.r-code[9] routing-mtx.r-code[10] ~
routing-mtx.r-code[11] routing-mtx.r-code[12] routing-mtx.r-code[13] ~
routing-mtx.r-code[14] routing-mtx.r-code[15] routing-mtx.r-code[16] ~
routing-mtx.r-code[17] routing-mtx.r-code[18] routing-mtx.r-code[19] ~
routing-mtx.r-code[20] routing-mtx.r-code[21] routing-mtx.r-code[22] ~
routing-mtx.r-code[23] routing-mtx.r-code[24] routing-mtx.r-code[25] ~
routing-mtx.r-code[26] routing-mtx.r-code[27] routing-mtx.r-code[28] ~
routing-mtx.r-code[29] routing-mtx.r-code[30] routing-mtx.r-code[31] ~
routing-mtx.r-code[32] routing-mtx.r-code[33] routing-mtx.r-code[34] ~
routing-mtx.r-code[35] routing-mtx.r-code[36] routing-mtx.r-code[37] ~
routing-mtx.r-code[38] routing-mtx.r-code[39] routing-mtx.r-code[40] ~
routing-mtx.r-code[41] routing-mtx.r-code[42] routing-mtx.r-code[43] ~
routing-mtx.r-code[44] routing-mtx.r-code[45] routing-mtx.r-code[46] ~
routing-mtx.r-code[47] routing-mtx.r-code[48] routing-mtx.r-code[49] ~
routing-mtx.r-code[50] routing-mtx.r-code[51] routing-mtx.r-code[52] ~
routing-mtx.r-code[53] routing-mtx.r-code[54] routing-mtx.r-code[55] ~
routing-mtx.r-code[56] routing-mtx.r-code[57] routing-mtx.r-code[58] ~
routing-mtx.r-code[59] routing-mtx.r-code[60] routing-mtx.r-code[61] ~
routing-mtx.r-code[62] routing-mtx.r-code[63] routing-mtx.r-code[64] ~
routing-mtx.r-code[65] routing-mtx.r-code[66] routing-mtx.r-code[67] ~
routing-mtx.r-code[68] routing-mtx.r-code[69] routing-mtx.r-code[70] ~
routing-mtx.r-code[71] routing-mtx.r-code[72] routing-mtx.r-code[73] ~
routing-mtx.r-code[74] routing-mtx.r-code[75] routing-mtx.r-code[76] ~
routing-mtx.r-code[77] routing-mtx.r-code[78] routing-mtx.r-code[79] ~
routing-mtx.r-code[80] routing-mtx.r-code[81] routing-mtx.r-code[82] ~
routing-mtx.r-code[83] routing-mtx.r-code[84] routing-mtx.r-code[85] ~
routing-mtx.r-code[86] routing-mtx.r-code[87] routing-mtx.r-code[88] ~
routing-mtx.r-code[89] routing-mtx.r-code[90] routing-mtx.r-code[91] ~
routing-mtx.r-code[92] routing-mtx.r-code[93] routing-mtx.r-code[94] ~
routing-mtx.r-code[95] routing-mtx.r-code[96] routing-mtx.r-code[97] ~
routing-mtx.r-code[98] routing-mtx.r-code[99] routing-mtx.r-code[100] 
&Scoped-define ENABLED-TABLES routing-mtx
&Scoped-define FIRST-ENABLED-TABLE routing-mtx
&Scoped-Define ENABLED-OBJECTS RECT-16 
&Scoped-Define DISPLAYED-FIELDS routing-mtx.style routing-mtx.msf ~
routing-mtx.dim-type routing-mtx.bl-len[1] routing-mtx.bl-len[2] ~
routing-mtx.bl-len[3] routing-mtx.bl-len[4] routing-mtx.bl-len[5] ~
routing-mtx.bl-len[6] routing-mtx.bl-len[7] routing-mtx.bl-len[8] ~
routing-mtx.bl-len[9] routing-mtx.bl-len[10] routing-mtx.bl-wid[1] ~
routing-mtx.bl-wid[2] routing-mtx.bl-wid[3] routing-mtx.bl-wid[4] ~
routing-mtx.bl-wid[5] routing-mtx.bl-wid[6] routing-mtx.bl-wid[7] ~
routing-mtx.bl-wid[8] routing-mtx.bl-wid[9] routing-mtx.bl-wid[10] ~
routing-mtx.r-code[1] routing-mtx.r-code[2] routing-mtx.r-code[3] ~
routing-mtx.r-code[4] routing-mtx.r-code[5] routing-mtx.r-code[6] ~
routing-mtx.r-code[7] routing-mtx.r-code[8] routing-mtx.r-code[9] ~
routing-mtx.r-code[10] routing-mtx.r-code[11] routing-mtx.r-code[12] ~
routing-mtx.r-code[13] routing-mtx.r-code[14] routing-mtx.r-code[15] ~
routing-mtx.r-code[16] routing-mtx.r-code[17] routing-mtx.r-code[18] ~
routing-mtx.r-code[19] routing-mtx.r-code[20] routing-mtx.r-code[21] ~
routing-mtx.r-code[22] routing-mtx.r-code[23] routing-mtx.r-code[24] ~
routing-mtx.r-code[25] routing-mtx.r-code[26] routing-mtx.r-code[27] ~
routing-mtx.r-code[28] routing-mtx.r-code[29] routing-mtx.r-code[30] ~
routing-mtx.r-code[31] routing-mtx.r-code[32] routing-mtx.r-code[33] ~
routing-mtx.r-code[34] routing-mtx.r-code[35] routing-mtx.r-code[36] ~
routing-mtx.r-code[37] routing-mtx.r-code[38] routing-mtx.r-code[39] ~
routing-mtx.r-code[40] routing-mtx.r-code[41] routing-mtx.r-code[42] ~
routing-mtx.r-code[43] routing-mtx.r-code[44] routing-mtx.r-code[45] ~
routing-mtx.r-code[46] routing-mtx.r-code[47] routing-mtx.r-code[48] ~
routing-mtx.r-code[49] routing-mtx.r-code[50] routing-mtx.r-code[51] ~
routing-mtx.r-code[52] routing-mtx.r-code[53] routing-mtx.r-code[54] ~
routing-mtx.r-code[55] routing-mtx.r-code[56] routing-mtx.r-code[57] ~
routing-mtx.r-code[58] routing-mtx.r-code[59] routing-mtx.r-code[60] ~
routing-mtx.r-code[61] routing-mtx.r-code[62] routing-mtx.r-code[63] ~
routing-mtx.r-code[64] routing-mtx.r-code[65] routing-mtx.r-code[66] ~
routing-mtx.r-code[67] routing-mtx.r-code[68] routing-mtx.r-code[69] ~
routing-mtx.r-code[70] routing-mtx.r-code[71] routing-mtx.r-code[72] ~
routing-mtx.r-code[73] routing-mtx.r-code[74] routing-mtx.r-code[75] ~
routing-mtx.r-code[76] routing-mtx.r-code[77] routing-mtx.r-code[78] ~
routing-mtx.r-code[79] routing-mtx.r-code[80] routing-mtx.r-code[81] ~
routing-mtx.r-code[82] routing-mtx.r-code[83] routing-mtx.r-code[84] ~
routing-mtx.r-code[85] routing-mtx.r-code[86] routing-mtx.r-code[87] ~
routing-mtx.r-code[88] routing-mtx.r-code[89] routing-mtx.r-code[90] ~
routing-mtx.r-code[91] routing-mtx.r-code[92] routing-mtx.r-code[93] ~
routing-mtx.r-code[94] routing-mtx.r-code[95] routing-mtx.r-code[96] ~
routing-mtx.r-code[97] routing-mtx.r-code[98] routing-mtx.r-code[99] ~
routing-mtx.r-code[100] 
&Scoped-define DISPLAYED-TABLES routing-mtx
&Scoped-define FIRST-DISPLAYED-TABLE routing-mtx


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
DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 121 BY 15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     routing-mtx.style AT ROW 1.48 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.2 BY 1
     routing-mtx.msf AT ROW 1.48 COL 42.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     routing-mtx.dim-type AT ROW 2.91 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     routing-mtx.bl-len[1] AT ROW 3.86 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[2] AT ROW 3.86 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[3] AT ROW 3.86 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[4] AT ROW 3.86 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[5] AT ROW 3.86 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[6] AT ROW 3.86 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[7] AT ROW 3.86 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[8] AT ROW 3.86 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[9] AT ROW 3.86 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-len[10] AT ROW 3.86 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[1] AT ROW 5.76 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[2] AT ROW 6.76 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[3] AT ROW 7.67 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[4] AT ROW 8.62 COL 6.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[5] AT ROW 9.57 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[6] AT ROW 10.52 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[7] AT ROW 11.48 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[8] AT ROW 12.43 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[9] AT ROW 13.38 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.bl-wid[10] AT ROW 14.33 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     routing-mtx.r-code[1] AT ROW 5.76 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[2] AT ROW 5.76 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[3] AT ROW 5.76 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[4] AT ROW 5.76 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     routing-mtx.r-code[5] AT ROW 5.76 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[6] AT ROW 5.76 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[7] AT ROW 5.76 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[8] AT ROW 5.76 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[9] AT ROW 5.76 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[10] AT ROW 5.76 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[11] AT ROW 6.71 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[12] AT ROW 6.71 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[13] AT ROW 6.71 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[14] AT ROW 6.71 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[15] AT ROW 6.71 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[16] AT ROW 6.71 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[17] AT ROW 6.71 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[18] AT ROW 6.71 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[19] AT ROW 6.71 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[20] AT ROW 6.71 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[21] AT ROW 7.67 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[22] AT ROW 7.67 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[23] AT ROW 7.67 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[24] AT ROW 7.67 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[25] AT ROW 7.67 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[26] AT ROW 7.67 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[27] AT ROW 7.67 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[28] AT ROW 7.67 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[29] AT ROW 7.67 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     routing-mtx.r-code[30] AT ROW 7.67 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[31] AT ROW 8.62 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[32] AT ROW 8.62 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[33] AT ROW 8.62 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[34] AT ROW 8.62 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[35] AT ROW 8.62 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[36] AT ROW 8.62 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[37] AT ROW 8.62 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[38] AT ROW 8.62 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[39] AT ROW 8.62 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[40] AT ROW 8.62 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[41] AT ROW 9.57 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[42] AT ROW 9.57 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[43] AT ROW 9.57 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[44] AT ROW 9.57 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[45] AT ROW 9.57 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[46] AT ROW 9.57 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[47] AT ROW 9.57 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[48] AT ROW 9.57 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[49] AT ROW 9.57 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[50] AT ROW 9.57 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[51] AT ROW 10.52 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[52] AT ROW 10.52 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[53] AT ROW 10.52 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[54] AT ROW 10.52 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     routing-mtx.r-code[55] AT ROW 10.52 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[56] AT ROW 10.52 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[57] AT ROW 10.52 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[58] AT ROW 10.52 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[59] AT ROW 10.52 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[60] AT ROW 10.52 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[61] AT ROW 11.48 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[62] AT ROW 11.48 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[63] AT ROW 11.48 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[64] AT ROW 11.48 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[65] AT ROW 11.48 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[66] AT ROW 11.48 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[67] AT ROW 11.48 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[68] AT ROW 11.48 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[69] AT ROW 11.48 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[70] AT ROW 11.48 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[71] AT ROW 12.43 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[72] AT ROW 12.43 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[73] AT ROW 12.43 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[74] AT ROW 12.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[75] AT ROW 12.43 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[76] AT ROW 12.43 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[77] AT ROW 12.43 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[78] AT ROW 12.43 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[79] AT ROW 12.43 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     routing-mtx.r-code[80] AT ROW 12.43 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[81] AT ROW 13.38 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[82] AT ROW 13.38 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[83] AT ROW 13.38 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[84] AT ROW 13.38 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[85] AT ROW 13.38 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[86] AT ROW 13.38 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[87] AT ROW 13.38 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[88] AT ROW 13.38 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[89] AT ROW 13.38 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[90] AT ROW 13.38 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[91] AT ROW 14.33 COL 19 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[92] AT ROW 14.33 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[93] AT ROW 14.33 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[94] AT ROW 14.33 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[95] AT ROW 14.33 COL 59 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[96] AT ROW 14.33 COL 69 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[97] AT ROW 14.33 COL 79 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[98] AT ROW 14.33 COL 89 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[99] AT ROW 14.33 COL 99 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     routing-mtx.r-code[100] AT ROW 14.33 COL 109 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     "LENGTH" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 4.1 COL 8
     "WIDTH" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 5.05 COL 8
     "DIMENSIONS" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 3.14 COL 19
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.routing-mtx,ASI.style
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
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

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

/* SETTINGS FOR FILL-IN routing-mtx.style IN FRAME F-Main
   NO-ENABLE                                                            */
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
    DEF VAR lv-focus AS WIDGET NO-UNDO.


    lv-focus = FOCUS.

    case focus:name :
         when "r-code" then do:
              run windows/l-rout.w (gcompany, gloc, focus:screen-value in frame {&frame-name}, output char-val).      
              if char-val <> "" then lv-focus:screen-value = entry(1,char-val).
         end.

    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[100]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[100] V-table-Win
ON LEAVE OF routing-mtx.r-code[100] IN FRAME F-Main /* Routing Code[100] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do: 
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[10] V-table-Win
ON LEAVE OF routing-mtx.r-code[10] IN FRAME F-Main /* Routing Code[10] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[11]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[11] V-table-Win
ON LEAVE OF routing-mtx.r-code[11] IN FRAME F-Main /* Routing Code[11] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[12]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[12] V-table-Win
ON LEAVE OF routing-mtx.r-code[12] IN FRAME F-Main /* Routing Code[12] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[13]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[13] V-table-Win
ON LEAVE OF routing-mtx.r-code[13] IN FRAME F-Main /* Routing Code[13] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[14]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[14] V-table-Win
ON LEAVE OF routing-mtx.r-code[14] IN FRAME F-Main /* Routing Code[14] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[15]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[15] V-table-Win
ON LEAVE OF routing-mtx.r-code[15] IN FRAME F-Main /* Routing Code[15] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[16]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[16] V-table-Win
ON LEAVE OF routing-mtx.r-code[16] IN FRAME F-Main /* Routing Code[16] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[17]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[17] V-table-Win
ON LEAVE OF routing-mtx.r-code[17] IN FRAME F-Main /* Routing Code[17] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[18]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[18] V-table-Win
ON LEAVE OF routing-mtx.r-code[18] IN FRAME F-Main /* Routing Code[18] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[19]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[19] V-table-Win
ON LEAVE OF routing-mtx.r-code[19] IN FRAME F-Main /* Routing Code[19] */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[1] V-table-Win
ON LEAVE OF routing-mtx.r-code[1] IN FRAME F-Main /* Routing Code[1] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[20]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[20] V-table-Win
ON LEAVE OF routing-mtx.r-code[20] IN FRAME F-Main /* Routing Code[20] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[21]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[21] V-table-Win
ON LEAVE OF routing-mtx.r-code[21] IN FRAME F-Main /* Routing Code[21] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[22]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[22] V-table-Win
ON LEAVE OF routing-mtx.r-code[22] IN FRAME F-Main /* Routing Code[22] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}  
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[23]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[23] V-table-Win
ON LEAVE OF routing-mtx.r-code[23] IN FRAME F-Main /* Routing Code[23] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[24]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[24] V-table-Win
ON LEAVE OF routing-mtx.r-code[24] IN FRAME F-Main /* Routing Code[24] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[25]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[25] V-table-Win
ON LEAVE OF routing-mtx.r-code[25] IN FRAME F-Main /* Routing Code[25] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[26]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[26] V-table-Win
ON LEAVE OF routing-mtx.r-code[26] IN FRAME F-Main /* Routing Code[26] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[27]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[27] V-table-Win
ON LEAVE OF routing-mtx.r-code[27] IN FRAME F-Main /* Routing Code[27] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[28]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[28] V-table-Win
ON LEAVE OF routing-mtx.r-code[28] IN FRAME F-Main /* Routing Code[28] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[29]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[29] V-table-Win
ON LEAVE OF routing-mtx.r-code[29] IN FRAME F-Main /* Routing Code[29] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[2] V-table-Win
ON LEAVE OF routing-mtx.r-code[2] IN FRAME F-Main /* Routing Code[2] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[30]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[30] V-table-Win
ON LEAVE OF routing-mtx.r-code[30] IN FRAME F-Main /* Routing Code[30] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
     {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[31]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[31] V-table-Win
ON LEAVE OF routing-mtx.r-code[31] IN FRAME F-Main /* Routing Code[31] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[32]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[32] V-table-Win
ON LEAVE OF routing-mtx.r-code[32] IN FRAME F-Main /* Routing Code[32] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[33]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[33] V-table-Win
ON LEAVE OF routing-mtx.r-code[33] IN FRAME F-Main /* Routing Code[33] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[34]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[34] V-table-Win
ON LEAVE OF routing-mtx.r-code[34] IN FRAME F-Main /* Routing Code[34] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[35]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[35] V-table-Win
ON LEAVE OF routing-mtx.r-code[35] IN FRAME F-Main /* Routing Code[35] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[36]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[36] V-table-Win
ON LEAVE OF routing-mtx.r-code[36] IN FRAME F-Main /* Routing Code[36] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[37]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[37] V-table-Win
ON LEAVE OF routing-mtx.r-code[37] IN FRAME F-Main /* Routing Code[37] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[38]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[38] V-table-Win
ON LEAVE OF routing-mtx.r-code[38] IN FRAME F-Main /* Routing Code[38] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[39]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[39] V-table-Win
ON LEAVE OF routing-mtx.r-code[39] IN FRAME F-Main /* Routing Code[39] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[3] V-table-Win
ON LEAVE OF routing-mtx.r-code[3] IN FRAME F-Main /* Routing Code[3] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[4] V-table-Win
ON LEAVE OF routing-mtx.r-code[4] IN FRAME F-Main /* Routing Code[4] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[5] V-table-Win
ON LEAVE OF routing-mtx.r-code[5] IN FRAME F-Main /* Routing Code[5] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[6] V-table-Win
ON LEAVE OF routing-mtx.r-code[6] IN FRAME F-Main /* Routing Code[6] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[7] V-table-Win
ON LEAVE OF routing-mtx.r-code[7] IN FRAME F-Main /* Routing Code[7] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[8] V-table-Win
ON LEAVE OF routing-mtx.r-code[8] IN FRAME F-Main /* Routing Code[8] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME routing-mtx.r-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.r-code[9] V-table-Win
ON LEAVE OF routing-mtx.r-code[9] IN FRAME F-Main /* Routing Code[9] */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = self:screen-value)
    then do:
    {&methods/lValidateError.i YES}
        message "Invalid Routing Code. Try Help." view-as alert-box error.
        return no-apply.
    {&methods/lValidateError.i NO}
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME routing-mtx.msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL routing-mtx.msf V-table-Win
ON LEAVE OF routing-mtx.msf IN FRAME F-Main /* Routing Code[9] */
DO:
    if lastkey <> -1 and self:screen-value <> ""  THEN do:
      RUN valid-msf NO-ERROR. 
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    end.                 

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
/*{custom/getcmpny.i}
{custom/getloc.i}
*/
{sys/inc/f3help.i}
session:data-entry-return = yes.
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
  {src/adm/template/row-list.i "routing-mtx"}
  {src/adm/template/row-list.i "style"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "routing-mtx"}
  {src/adm/template/row-find.i "style"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
  run est/d-routmc.w (if avail routing-mtx then recid(routing-mtx) else ?, recid(style)).

  /* Dispatch standard ADM method.                             */
  /* RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ). */

  /* Code placed here will execute AFTER standard behavior.    */

  run get-link-handle in adm-broker-hdl (this-procedure,"record-source", output char-hdl).  
/*  run get-link-handle in adm-broker-hdl (widget-handle(char-hdl),"record-source", output char-hdl).  */
  RUN dispatch IN widget-handle(char-hdl) ( INPUT 'open-query':U ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-routing-mtx FOR routing-mtx .
DEFINE  VARIABLE cCheckMsf AS CHARACTER NO-UNDO .
DEFINE VARIABLE dMsf AS DECIMAL NO-UNDO .
DEFINE VARIABLE iCount AS INTEGER NO-UNDO .
  /* Code placed here will execute PRIOR to standard behavior. */

   FIND FIRST bf-routing-mtx NO-LOCK
      WHERE bf-routing-mtx.company EQ style.company
        AND bf-routing-mtx.loc EQ gloc
        AND bf-routing-mtx.style EQ style.style
        AND bf-routing-mtx.msf EQ 0 NO-ERROR .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  dMsf = 0 .

 IF AVAIL style AND AVAIL bf-routing-mtx  THEN do:
 FOR EACH bf-routing-mtx NO-LOCK 
     WHERE bf-routing-mtx.company EQ style.company
       AND bf-routing-mtx.loc EQ gloc
       AND bf-routing-mtx.style EQ style.style :
        cCheckMsf = cCheckMsf + STRING(bf-routing-mtx.msf) + "," .
 END.
 
  DO iCount = 1 TO NUM-ENTRIES(cCheckMsf):
      IF ENTRY(iCount,cCheckMsf) NE "" AND decimal(ENTRY(iCount,cCheckMsf)) EQ dMsf THEN
          dMsf = dMsf + 1 .
  END.
 END.
  
  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
  assign routing-mtx.company = style.company
         routing-mtx.loc = gloc
         routing-mtx.style = style.style
         routing-mtx.msf   = dMsf .
  END.
  disp routing-mtx.style with frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-routing-mtx for routing-mtx.

  /* Code placed here will execute PRIOR to standard behavior. */
/*  
  if not avail style then return.
message "1" routing-mtx.msf.
   find first bf-routing-mtx of style no-lock no-error.
   if not avail bf-routing-mtx then do:
     create bf-routing-mtx.
     assign bf-routing-mtx.company = style.company
         bf-routing-mtx.loc = gloc
         bf-routing-mtx.style = style.style.
     display bf-routing-mtx.style @ routingwith frame {&frame-name}.    
   end.
*/
  DO WITH FRAME {&FRAME-NAME}:
    IF lv-save-format EQ "" THEN lv-save-format = routing-mtx.bl-len[1]:FORMAT.

    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[01] EQ 0 THEN
       routing-mtx.bl-len[01]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[01]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[02] EQ 0 THEN
       routing-mtx.bl-len[02]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[02]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[03] EQ 0 THEN
       routing-mtx.bl-len[03]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[03]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[04] EQ 0 THEN
       routing-mtx.bl-len[04]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[04]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[05] EQ 0 THEN
       routing-mtx.bl-len[05]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[05]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[06] EQ 0 THEN
       routing-mtx.bl-len[06]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[06]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[07] EQ 0 THEN
       routing-mtx.bl-len[07]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[07]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[08] EQ 0 THEN
       routing-mtx.bl-len[08]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[08]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[09] EQ 0 THEN
       routing-mtx.bl-len[09]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[09]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-len[10] EQ 0 THEN
       routing-mtx.bl-len[10]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-len[10]:FORMAT = ">>9.99".

    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[01] EQ 0 THEN
       routing-mtx.bl-wid[01]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[01]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[02] EQ 0 THEN
       routing-mtx.bl-wid[02]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[02]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[03] EQ 0 THEN
       routing-mtx.bl-wid[03]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[03]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[04] EQ 0 THEN
       routing-mtx.bl-wid[04]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[04]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[05] EQ 0 THEN
       routing-mtx.bl-wid[05]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[05]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[06] EQ 0 THEN
       routing-mtx.bl-wid[06]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[06]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[07] EQ 0 THEN
       routing-mtx.bl-wid[07]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[07]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[08] EQ 0 THEN
       routing-mtx.bl-wid[08]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[08]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[09] EQ 0 THEN
       routing-mtx.bl-wid[09]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[09]:FORMAT = ">>9.99".
    IF NOT AVAIL routing-mtx       OR 
       routing-mtx.bl-wid[10] EQ 0 THEN
       routing-mtx.bl-wid[10]:FORMAT = ">>>>>>>>".
    ELSE
       routing-mtx.bl-wid[10]:FORMAT = ">>9.99".
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     routing-mtx.bl-len[01]:FORMAT = lv-save-format
     routing-mtx.bl-len[02]:FORMAT = lv-save-format
     routing-mtx.bl-len[03]:FORMAT = lv-save-format
     routing-mtx.bl-len[04]:FORMAT = lv-save-format
     routing-mtx.bl-len[05]:FORMAT = lv-save-format
     routing-mtx.bl-len[06]:FORMAT = lv-save-format
     routing-mtx.bl-len[07]:FORMAT = lv-save-format
     routing-mtx.bl-len[08]:FORMAT = lv-save-format
     routing-mtx.bl-len[09]:FORMAT = lv-save-format
     routing-mtx.bl-len[10]:FORMAT = lv-save-format

     routing-mtx.bl-wid[01]:FORMAT = lv-save-format
     routing-mtx.bl-wid[02]:FORMAT = lv-save-format
     routing-mtx.bl-wid[03]:FORMAT = lv-save-format
     routing-mtx.bl-wid[04]:FORMAT = lv-save-format
     routing-mtx.bl-wid[05]:FORMAT = lv-save-format
     routing-mtx.bl-wid[06]:FORMAT = lv-save-format
     routing-mtx.bl-wid[07]:FORMAT = lv-save-format
     routing-mtx.bl-wid[08]:FORMAT = lv-save-format
     routing-mtx.bl-wid[09]:FORMAT = lv-save-format
     routing-mtx.bl-wid[10]:FORMAT = lv-save-format.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */
  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child.
  do while valid-handle(lv-field-hdl):

     if lv-field-hdl:name = "r-code" then do:
        run validate-routing (lv-field-hdl:screen-value) no-error.
        if error-status:error then do:
        {&methods/lValidateError.i YES}
           message "Invalid Routing Code. Try Help." 
                   view-as alert-box error.
           apply "entry" to lv-field-hdl .
           return no-apply.
        {&methods/lValidateError.i NO}
        end.
     end.   
     lv-field-hdl = lv-field-hdl:next-sibling.

  end.

   RUN valid-msf NO-ERROR. 
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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
  {src/adm/template/snd-list.i "routing-mtx"}
  {src/adm/template/snd-list.i "style"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-routing V-table-Win 
PROCEDURE validate-routing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-r-code as cha no-undo.

  {methods/lValidateError.i YES}
  if  ip-r-code <> "" and
       not can-find(routing where routing.company = gcompany and routing.loc = gloc
                    and routing.r-code = ip-r-code)
    then do:
              return error.
    end.   
    {methods/lValidateError.i NO}              
    return. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-msf V-table-Win 
PROCEDURE valid-msf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER bf-routing-mtx FOR routing-mtx .

   {methods/lValidateError.i YES}
  IF integer(routing-mtx.msf:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ 0 THEN do:
    MESSAGE "MSF cannot be 0. Please enter valid MSF..." VIEW-AS ALERT-BOX INFO.
       APPLY "entry" TO routing-mtx.MSF .
       RETURN ERROR.
  END.
  
 IF AVAIL style THEN
  FIND FIRST bf-routing-mtx NO-LOCK 
     WHERE bf-routing-mtx.company EQ style.company
       AND bf-routing-mtx.loc EQ gloc
       AND bf-routing-mtx.style EQ style.style 
       AND bf-routing-mtx.msf EQ integer(routing-mtx.msf:SCREEN-VALUE IN FRAME {&FRAME-NAME})
       AND ROWID(bf-routing-mtx) NE ROWID(routing-mtx) NO-ERROR .
       
    IF  AVAIL bf-routing-mtx THEN DO:
       MESSAGE "MSF " routing-mtx.msf:SCREEN-VALUE IN FRAME {&FRAME-NAME} " already Exist. Please enter different MSF..." VIEW-AS ALERT-BOX INFO.
       APPLY "entry" TO routing-mtx.MSF .
       RETURN ERROR.
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
