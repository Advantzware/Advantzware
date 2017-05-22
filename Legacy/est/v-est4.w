&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: est\v-est4.w

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
{custom/globdefs.i}
{sys/inc/var.i new shared}
assign cocode = g_company
       locode = g_loc.

DEF VAR li-sq AS INT NO-UNDO.
DEF VAR lv-spec-qty LIKE ef.spec-qty DECIMALS 10 NO-UNDO.

{est/d-selblk.i NEW}

DO TRANSACTION:
  {sys/inc/cemisc.i}
END.

{sys/inc/ceprepprice.i}

&SCOPED-DEFINE proc-enable enable-est4

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES est ef eb
&Scoped-define FIRST-EXTERNAL-TABLE est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR est, ef, eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ef.mis-bnum[1] ef.mis-cost[1] ef.mis-matf[1] ~
ef.mis-labf[1] ef.mis-matm[1] ef.mis-labm[1] ef.mis-simon[1] ef.mis-mkup[1] ~
ef.mis-bnum[2] ef.mis-cost[2] ef.mis-matf[2] ef.mis-labf[2] ef.mis-matm[2] ~
ef.mis-labm[2] ef.mis-simon[2] ef.mis-mkup[2] ef.mis-bnum[3] ef.mis-cost[3] ~
ef.mis-matf[3] ef.mis-labf[3] ef.mis-matm[3] ef.mis-labm[3] ef.mis-simon[3] ~
ef.mis-mkup[3] ef.mis-bnum[4] ef.mis-cost[4] ef.mis-matf[4] ef.mis-labf[4] ~
ef.mis-matm[4] ef.mis-labm[4] ef.mis-simon[4] ef.mis-mkup[4] ef.mis-bnum[5] ~
ef.mis-cost[5] ef.mis-matf[5] ef.mis-labf[5] ef.mis-matm[5] ef.mis-labm[5] ~
ef.mis-simon[5] ef.mis-mkup[5] ef.mis-bnum[6] ef.mis-cost[6] ef.mis-matf[6] ~
ef.mis-labf[6] ef.mis-matm[6] ef.mis-labm[6] ef.mis-simon[6] ef.mis-mkup[6] ~
ef.spec-no[1] ef.spec-dscr[1] ef.spec-no[2] ef.spec-dscr[2] ef.spec-no[3] ~
ef.spec-dscr[3] ef.spec-no[4] ef.spec-dscr[4] ef.spec-no[5] ef.spec-dscr[5] ~
ef.spec-no[6] ef.spec-dscr[6] ef.spec-no[7] ef.spec-dscr[7] ef.spec-no[8] ~
ef.spec-dscr[8] 
&Scoped-define ENABLED-TABLES ef
&Scoped-define FIRST-ENABLED-TABLE ef
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-24 
&Scoped-Define DISPLAYED-FIELDS ef.mis-snum[1] ef.mis-bnum[1] ~
ef.mis-cost[1] ef.mis-matf[1] ef.mis-labf[1] ef.mis-matm[1] ef.mis-labm[1] ~
ef.mis-simon[1] ef.mis-mkup[1] ef.mis-snum[2] ef.mis-bnum[2] ef.mis-cost[2] ~
ef.mis-matf[2] ef.mis-labf[2] ef.mis-matm[2] ef.mis-labm[2] ef.mis-simon[2] ~
ef.mis-mkup[2] ef.mis-snum[3] ef.mis-bnum[3] ef.mis-cost[3] ef.mis-matf[3] ~
ef.mis-labf[3] ef.mis-matm[3] ef.mis-labm[3] ef.mis-simon[3] ef.mis-mkup[3] ~
ef.mis-snum[4] ef.mis-bnum[4] ef.mis-cost[4] ef.mis-matf[4] ef.mis-labf[4] ~
ef.mis-matm[4] ef.mis-labm[4] ef.mis-simon[4] ef.mis-mkup[4] ef.mis-snum[5] ~
ef.mis-bnum[5] ef.mis-cost[5] ef.mis-matf[5] ef.mis-labf[5] ef.mis-matm[5] ~
ef.mis-labm[5] ef.mis-simon[5] ef.mis-mkup[5] ef.mis-snum[6] ef.mis-bnum[6] ~
ef.mis-cost[6] ef.mis-matf[6] ef.mis-labf[6] ef.mis-matm[6] ef.mis-labm[6] ~
ef.mis-simon[6] ef.mis-mkup[6] ef.spec-no[1] ef.spec-dscr[1] ef.spec-uom[1] ~
ef.spec-no[2] ef.spec-dscr[2] ef.spec-uom[2] ef.spec-no[3] ef.spec-dscr[3] ~
ef.spec-uom[3] ef.spec-no[4] ef.spec-dscr[4] ef.spec-uom[4] ef.spec-no[5] ~
ef.spec-dscr[5] ef.spec-uom[5] ef.spec-no[6] ef.spec-dscr[6] ef.spec-uom[6] ~
ef.spec-no[7] ef.spec-dscr[7] ef.spec-uom[7] ef.spec-no[8] ef.spec-dscr[8] ~
ef.spec-uom[8] 
&Scoped-define DISPLAYED-TABLES ef
&Scoped-define FIRST-DISPLAYED-TABLE ef
&Scoped-Define DISPLAYED-OBJECTS fi_hdr fi_spec-qty-01 fi_spec-qty-02 ~
fi_spec-qty-03 fi_spec-qty-04 fi_spec-qty-05 fi_spec-qty-06 fi_spec-qty-07 ~
fi_spec-qty-08 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS ef.mis-snum[1] ef.mis-snum[2] ~
ef.mis-snum[3] ef.mis-snum[4] ef.mis-snum[5] ef.mis-snum[6] fi_spec-qty-01 ~
fi_spec-qty-02 fi_spec-qty-03 fi_spec-qty-04 fi_spec-qty-05 fi_spec-qty-06 ~
fi_spec-qty-07 fi_spec-qty-08 

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
DEFINE VARIABLE fi_hdr AS CHARACTER FORMAT "X(256)":U INITIAL "Sub-Contract, Farm-Out, and Misc Costs" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .71 NO-UNDO.

DEFINE VARIABLE fi_spec-qty-01 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-02 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-03 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-04 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-05 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-06 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-07 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_spec-qty-08 AS DECIMAL FORMAT ">>>>>>>>9.9<<<<" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 7.62.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_hdr AT ROW 1 COL 5 COLON-ALIGNED NO-LABEL
     ef.mis-snum[1] AT ROW 2.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[1] AT ROW 2.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[1] AT ROW 2.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[1] AT ROW 2.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[1] AT ROW 2.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[1] AT ROW 2.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[1] AT ROW 2.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[1] AT ROW 2.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.mis-mkup[1] AT ROW 2.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.mis-snum[2] AT ROW 3.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[2] AT ROW 3.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[2] AT ROW 3.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[2] AT ROW 3.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[2] AT ROW 3.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[2] AT ROW 3.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[2] AT ROW 3.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[2] AT ROW 3.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.mis-mkup[2] AT ROW 3.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.mis-snum[3] AT ROW 4.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[3] AT ROW 4.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[3] AT ROW 4.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[3] AT ROW 4.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[3] AT ROW 4.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[3] AT ROW 4.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[3] AT ROW 4.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[3] AT ROW 4.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.mis-mkup[3] AT ROW 4.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     ef.mis-snum[4] AT ROW 5.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[4] AT ROW 5.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[4] AT ROW 5.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[4] AT ROW 5.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[4] AT ROW 5.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[4] AT ROW 5.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[4] AT ROW 5.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[4] AT ROW 5.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.mis-mkup[4] AT ROW 5.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.mis-snum[5] AT ROW 6.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[5] AT ROW 6.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[5] AT ROW 6.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[5] AT ROW 6.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[5] AT ROW 6.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[5] AT ROW 6.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[5] AT ROW 6.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[5] AT ROW 6.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     ef.mis-mkup[5] AT ROW 6.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.mis-snum[6] AT ROW 7.43 COL 2 NO-LABEL FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     ef.mis-bnum[6] AT ROW 7.43 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     ef.mis-cost[6] AT ROW 7.43 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     ef.mis-matf[6] AT ROW 7.43 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-labf[6] AT ROW 7.43 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ef.mis-matm[6] AT ROW 7.43 COL 61 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-labm[6] AT ROW 7.43 COL 79.6 COLON-ALIGNED NO-LABEL FORMAT ">>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ef.mis-simon[6] AT ROW 7.43 COL 98.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     ef.mis-mkup[6] AT ROW 7.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     ef.spec-no[1] AT ROW 10.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[1] AT ROW 10.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-01 AT ROW 10.05 COL 69 COLON-ALIGNED HELP
          "Enter Quantity for this Item" NO-LABEL
     ef.spec-uom[1] AT ROW 10.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[2] AT ROW 11.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[2] AT ROW 11.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-02 AT ROW 11.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[2] AT ROW 11.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[3] AT ROW 12.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[3] AT ROW 12.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-03 AT ROW 12.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[3] AT ROW 12.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[4] AT ROW 13.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[4] AT ROW 13.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-04 AT ROW 13.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[4] AT ROW 13.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[5] AT ROW 14.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[5] AT ROW 14.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-05 AT ROW 14.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[5] AT ROW 14.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[6] AT ROW 15.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[6] AT ROW 15.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-06 AT ROW 15.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[6] AT ROW 15.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[7] AT ROW 16.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ef.spec-dscr[7] AT ROW 16.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-07 AT ROW 16.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[7] AT ROW 16.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     ef.spec-no[8] AT ROW 17.05 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     ef.spec-dscr[8] AT ROW 17.05 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     fi_spec-qty-08 AT ROW 17.05 COL 69 COLON-ALIGNED NO-LABEL
     ef.spec-uom[8] AT ROW 17.05 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     "Simon" VIEW-AS TEXT
          SIZE 7.8 BY .62 AT ROW 1.71 COL 97.8
     "Markup" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.71 COL 105.6
     "Qty / FG (Set)" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 9.33 COL 71
     "Labor/SU" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.71 COL 51
     "Item #" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 9.33 COL 6
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 9.33 COL 31
     "UOM" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 9.33 COL 92
     "Misc. Cost" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.71 COL 17
     "S/B" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.71 COL 5
     "Mat's/M" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.71 COL 64
     "Special Materials" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 8.62 COL 3
     "Labor /M" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.71 COL 81.8
     "Mat'l/SU" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.71 COL 40
     RECT-15 AT ROW 1 COL 1
     RECT-24 AT ROW 8.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.est,ASI.ef,ASI.eb
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
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_hdr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_spec-qty-01 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-02 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-03 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-04 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-05 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-06 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-07 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_spec-qty-08 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ef.mis-labm[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-labm[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-labm[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-labm[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-labm[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-labm[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[2] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[3] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[4] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[5] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-matm[6] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ef.mis-snum[1] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.mis-snum[2] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.mis-snum[3] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.mis-snum[4] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.mis-snum[5] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.mis-snum[6] IN FRAME F-Main
   NO-ENABLE ALIGN-L 2 EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN ef.spec-uom[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[2] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[3] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[4] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[5] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[6] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[7] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ef.spec-uom[8] IN FRAME F-Main
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
    case focus:name:
         when "spec-no" then do:
              run windows/l-item.w (ef.company, "2", "M,@", focus:screen-value, output char-val).
              if char-val <> "" then do:
                 assign focus:screen-value = entry(1,char-val).
                 case focus:index  :
                      when 1 then ef.spec-dscr[1]:screen-value = entry(2,char-val).
                      when 2 then ef.spec-dscr[2]:screen-value = entry(2,char-val).
                      when 3 then ef.spec-dscr[3]:screen-value = entry(2,char-val).
                      when 4 then ef.spec-dscr[4]:screen-value = entry(2,char-val).
                      when 5 then ef.spec-dscr[5]:screen-value = entry(2,char-val).
                      when 6 then ef.spec-dscr[6]:screen-value = entry(2,char-val).
                      when 7 then ef.spec-dscr[7]:screen-value = entry(2,char-val).
                      when 8 then ef.spec-dscr[8]:screen-value = entry(2,char-val).
                 end case.
              end. 
              return no-apply.
         end.
    end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[1] V-table-Win
ON LEAVE OF ef.mis-cost[1] IN FRAME F-Main /* Misc. Cost[1] */
DO:
  def var lv-ref-rec-qty as recid no-undo.
  def var lv-ref-rec-cst as recid no-undo.

  IF SELF:SCREEN-VALUE = "" THEN DO:

    {cec/refestg2.i "MAT" 1}
    {cec/refestg2.i "LAB" 1}
  END.
  ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[1] AND ef.mis-cost[1] = "" THEN
     ef.mis-simon[1]:SCREEN-VALUE = cemisc-cha.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[1] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[1] IN FRAME F-Main /* Misc. Cost[1] */
DO:
  RUN new-mis-cost(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[2] V-table-Win
ON LEAVE OF ef.mis-cost[2] IN FRAME F-Main /* Misc. Cost[2] */
DO:
   IF SELF:SCREEN-VALUE = "" THEN DO:
      def var lv-ref-rec-qty as recid no-undo.
      def var lv-ref-rec-cst as recid no-undo.

      {cec/refestg2.i "MAT" 2}
      {cec/refestg2.i "LAB" 2}
   END.
   ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[2] AND ef.mis-cost[2] = "" THEN
      ef.mis-simon[2]:SCREEN-VALUE = cemisc-cha.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[2] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[2] IN FRAME F-Main /* Misc. Cost[2] */
DO:
  RUN new-mis-cost(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[3] V-table-Win
ON LEAVE OF ef.mis-cost[3] IN FRAME F-Main /* Misc. Cost[3] */
DO:
   IF SELF:SCREEN-VALUE = "" THEN DO:
      def var lv-ref-rec-qty as recid no-undo.
      def var lv-ref-rec-cst as recid no-undo.

      {cec/refestg2.i "MAT" 3}
      {cec/refestg2.i "LAB" 3}
   END.
   ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[3] AND ef.mis-cost[3] = "" THEN
      ef.mis-simon[3]:SCREEN-VALUE = cemisc-cha.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[3] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[3] IN FRAME F-Main /* Misc. Cost[3] */
DO:
  RUN new-mis-cost(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[4] V-table-Win
ON LEAVE OF ef.mis-cost[4] IN FRAME F-Main /* Misc. Cost[4] */
DO:
   IF SELF:SCREEN-VALUE = "" THEN DO:
      def var lv-ref-rec-qty as recid no-undo.
      def var lv-ref-rec-cst as recid no-undo.

      {cec/refestg2.i "MAT" 4}
      {cec/refestg2.i "LAB" 4}
   END.
   ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[4] AND ef.mis-cost[4] = "" THEN
      ef.mis-simon[4]:SCREEN-VALUE = cemisc-cha.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[4] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[4] IN FRAME F-Main /* Misc. Cost[4] */
DO:
  RUN new-mis-cost(4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[5] V-table-Win
ON LEAVE OF ef.mis-cost[5] IN FRAME F-Main /* Misc. Cost[5] */
DO:
   IF SELF:SCREEN-VALUE = "" THEN DO:
      def var lv-ref-rec-qty as recid no-undo.
      def var lv-ref-rec-cst as recid no-undo.

      {cec/refestg2.i "MAT" 5}
      {cec/refestg2.i "LAB" 5}
   END.
   ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[5] AND ef.mis-cost[5] = "" THEN
      ef.mis-simon[5]:SCREEN-VALUE = cemisc-cha.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[5] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[5] IN FRAME F-Main /* Misc. Cost[5] */
DO:
  RUN new-mis-cost(5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-cost[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[6] V-table-Win
ON LEAVE OF ef.mis-cost[6] IN FRAME F-Main /* Misc. Cost[6] */
DO:
   IF SELF:SCREEN-VALUE = "" THEN DO:
      def var lv-ref-rec-qty as recid no-undo.
      def var lv-ref-rec-cst as recid no-undo.

      {cec/refestg2.i "MAT" 6}
      {cec/refestg2.i "LAB" 6}
   END.
   ELSE IF SELF:SCREEN-VALUE NE ef.mis-cost[6] AND ef.mis-cost[6] = "" THEN
      ef.mis-simon[6]:SCREEN-VALUE = cemisc-cha.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-cost[6] V-table-Win
ON VALUE-CHANGED OF ef.mis-cost[6] IN FRAME F-Main /* Misc. Cost[6] */
DO:
  RUN new-mis-cost(6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[1] V-table-Win
ON ENTRY OF ef.mis-labm[1] IN FRAME F-Main /* Lab/M[1] */
DO:
  {cec/refestg.i "LAB" 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[2] V-table-Win
ON ENTRY OF ef.mis-labm[2] IN FRAME F-Main /* Lab/M[2] */
DO:
  {cec/refestg.i "LAB" 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[3] V-table-Win
ON ENTRY OF ef.mis-labm[3] IN FRAME F-Main /* Lab/M[3] */
DO:
  {cec/refestg.i "LAB" 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[4] V-table-Win
ON ENTRY OF ef.mis-labm[4] IN FRAME F-Main /* Lab/M[4] */
DO:
  {cec/refestg.i "LAB" 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[5] V-table-Win
ON ENTRY OF ef.mis-labm[5] IN FRAME F-Main /* Lab/M[5] */
DO:
  {cec/refestg.i "LAB" 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-labm[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-labm[6] V-table-Win
ON ENTRY OF ef.mis-labm[6] IN FRAME F-Main /* Lab/M[6] */
DO:
  {cec/refestg.i "LAB" 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[1] V-table-Win
ON ENTRY OF ef.mis-matm[1] IN FRAME F-Main /* Mat'l/M[1] */
DO:
  {cec/refestg.i "MAT" 1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[2] V-table-Win
ON ENTRY OF ef.mis-matm[2] IN FRAME F-Main /* Mat'l/M[2] */
DO:
  {cec/refestg.i "MAT" 2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[3] V-table-Win
ON ENTRY OF ef.mis-matm[3] IN FRAME F-Main /* Mat'l/M[3] */
DO:
  {cec/refestg.i "MAT" 3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[4] V-table-Win
ON ENTRY OF ef.mis-matm[4] IN FRAME F-Main /* Mat'l/M[4] */
DO:
  {cec/refestg.i "MAT" 4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[5] V-table-Win
ON ENTRY OF ef.mis-matm[5] IN FRAME F-Main /* Mat'l/M[5] */
DO:
  {cec/refestg.i "MAT" 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-matm[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-matm[6] V-table-Win
ON ENTRY OF ef.mis-matm[6] IN FRAME F-Main /* Mat'l/M[6] */
DO:
  {cec/refestg.i "MAT" 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[1] V-table-Win
ON LEAVE OF ef.mis-mkup[1] IN FRAME F-Main /* Mkup[1] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (1,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[2] V-table-Win
ON LEAVE OF ef.mis-mkup[2] IN FRAME F-Main /* Mkup[2] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (2,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[3] V-table-Win
ON LEAVE OF ef.mis-mkup[3] IN FRAME F-Main /* Mkup[3] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (3,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[4] V-table-Win
ON LEAVE OF ef.mis-mkup[4] IN FRAME F-Main /* Mkup[4] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (4,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[5] V-table-Win
ON LEAVE OF ef.mis-mkup[5] IN FRAME F-Main /* Mkup[5] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (5,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-mkup[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-mkup[6] V-table-Win
ON LEAVE OF ef.mis-mkup[6] IN FRAME F-Main /* Mkup[6] */
DO:
   DEF VAR op-error AS LOG NO-UNDO.

   IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" THEN DO:
      RUN valid-markup (6,OUTPUT op-error).
      IF op-error THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[1] V-table-Win
ON LEAVE OF ef.mis-simon[1] IN FRAME F-Main /* SIMON[1] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[1] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[1] IN FRAME F-Main /* SIMON[1] */
DO:
  IF ef.mis-cost[1]:SCREEN-VALUE EQ "" THEN ef.mis-simon[1]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[1]:SCREEN-VALUE = CAPS(ef.mis-simon[1]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[2] V-table-Win
ON LEAVE OF ef.mis-simon[2] IN FRAME F-Main /* SIMON[2] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[2] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[2] IN FRAME F-Main /* SIMON[2] */
DO:
  IF ef.mis-cost[2]:SCREEN-VALUE EQ "" THEN ef.mis-simon[2]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[2]:SCREEN-VALUE = CAPS(ef.mis-simon[2]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[3] V-table-Win
ON LEAVE OF ef.mis-simon[3] IN FRAME F-Main /* SIMON[3] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[3] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[3] IN FRAME F-Main /* SIMON[3] */
DO:
  IF ef.mis-cost[3]:SCREEN-VALUE EQ "" THEN ef.mis-simon[3]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[3]:SCREEN-VALUE = CAPS(ef.mis-simon[3]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[4] V-table-Win
ON LEAVE OF ef.mis-simon[4] IN FRAME F-Main /* SIMON[4] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[4] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[4] IN FRAME F-Main /* SIMON[4] */
DO:
  IF ef.mis-cost[4]:SCREEN-VALUE EQ "" THEN ef.mis-simon[4]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[4]:SCREEN-VALUE = CAPS(ef.mis-simon[4]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[5] V-table-Win
ON LEAVE OF ef.mis-simon[5] IN FRAME F-Main /* SIMON[5] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (5) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[5] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[5] IN FRAME F-Main /* SIMON[5] */
DO:
  IF ef.mis-cost[5]:SCREEN-VALUE EQ "" THEN ef.mis-simon[5]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[5]:SCREEN-VALUE = CAPS(ef.mis-simon[5]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.mis-simon[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[6] V-table-Win
ON LEAVE OF ef.mis-simon[6] IN FRAME F-Main /* SIMON[6] */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-mis-simon (6) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.mis-simon[6] V-table-Win
ON VALUE-CHANGED OF ef.mis-simon[6] IN FRAME F-Main /* SIMON[6] */
DO:
  IF ef.mis-cost[6]:SCREEN-VALUE EQ "" THEN ef.mis-simon[6]:SCREEN-VALUE = "".
  ELSE
    ef.mis-simon[6]:SCREEN-VALUE = CAPS(ef.mis-simon[6]:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[1] V-table-Win
ON LEAVE OF ef.spec-no[1] IN FRAME F-Main /* RM Item#[1] */
DO:
    if lastkey = -1 then return.
   {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[1]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[1]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[2] V-table-Win
ON LEAVE OF ef.spec-no[2] IN FRAME F-Main /* RM Item#[2] */
DO:
    if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[2]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[2]:screen-value = item.i-name.      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[3] V-table-Win
ON LEAVE OF ef.spec-no[3] IN FRAME F-Main /* RM Item#[3] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[3]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[3]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[4] V-table-Win
ON LEAVE OF ef.spec-no[4] IN FRAME F-Main /* RM Item#[4] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[4]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[4]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[5] V-table-Win
ON LEAVE OF ef.spec-no[5] IN FRAME F-Main /* RM Item#[5] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[5]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[5]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[6] V-table-Win
ON LEAVE OF ef.spec-no[6] IN FRAME F-Main /* RM Item#[6] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[6]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[6]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[7] V-table-Win
ON LEAVE OF ef.spec-no[7] IN FRAME F-Main /* RM Item#[7] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[7]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[7]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ef.spec-no[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ef.spec-no[8] V-table-Win
ON LEAVE OF ef.spec-no[8] IN FRAME F-Main /* RM Item#[8] */
DO:
      if lastkey = -1 then return.
    {&methods/lValidateError.i YES}
    find first item where item.company = ef.company and
                          item.indus = "2" and
                          INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                          item.i-no = self:screen-value
                          no-lock no-error.
    if not avail item then do:
       if self:screen-value = "" then do:
          ef.spec-dscr[8]:screen-value = "".
          return.
       end.
       message "Invalid Item. Try Help." view-as alert-box.
       return no-apply.
    end.                          
    ef.spec-dscr[8]:screen-value = item.i-name.                      
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:data-entry-return = yes.
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
  {src/adm/template/row-list.i "est"}
  {src/adm/template/row-list.i "ef"}
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "est"}
  {src/adm/template/row-find.i "ef"}
  {src/adm/template/row-find.i "eb"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-misc V-table-Win 
PROCEDURE copy-misc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.

  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-eb2 FOR eb.
  DEF BUFFER b-ref FOR reftable.


  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/checkuse.i}

  IF AVAIL ef                                  AND
     CAN-FIND(FIRST b-ef
              WHERE b-ef.company EQ ef.company
                AND b-ef.est-no  EQ ef.est-no
                AND ROWID(b-ef)  NE ROWID(ef)) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN est/d-selblk.w (ROWID(ef), "Copy " + TRIM(fi_hdr:SCREEN-VALUE)).

    ll = NO.

    IF CAN-FIND(FIRST tt-select WHERE tt-selected) THEN
    MESSAGE "WARNING" SKIP(1)
            "Copy will clear out any " + TRIM(fi_hdr:SCREEN-VALUE) + " for " +
            "the Form#'s you are copying to.  Do you still want to perform "
            "this function?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

    IF ll THEN
    FOR EACH tt-select WHERE tt-selected,
        FIRST b-eb NO-LOCK WHERE ROWID(b-eb) EQ tt-rowid,
        FIRST b-ef
        WHERE b-ef.company EQ b-eb.company
          AND b-ef.est-no  EQ b-eb.est-no
          AND b-ef.eqty    EQ b-eb.eqty
          AND b-ef.form-no EQ b-eb.form-no:

      ASSIGN
       b-ef.mis-snum  = 0
       b-ef.mis-bnum  = 0
       b-ef.mis-cost  = ""
       b-ef.mis-matf  = 0
       b-ef.mis-labf  = 0
       b-ef.mis-matm  = 0
       b-ef.mis-labm  = 0
       b-ef.mis-simon = ""
       b-ef.mis-mkup  = 0.

      FOR EACH b-ref
          WHERE b-ref.reftable EQ "EST-MISC"
                AND b-ref.company  EQ b-ef.company
                AND b-ref.loc      EQ b-ef.loc
                AND b-ref.code     EQ TRIM(b-ef.est-no) +
                                  STRING(b-ef.form-no,"/99"):
        DELETE b-ref.
      END.

      DO li = 1 TO EXTENT(ef.mis-cost):
        IF ef.mis-cost[li] NE ""                              AND
           (ef.mis-bnum[li] EQ 0 OR
            CAN-FIND(FIRST b-eb2
                    WHERE b-eb2.company  EQ b-ef.company
                      AND b-eb2.est-no   EQ b-ef.est-no
                      AND b-eb2.eqty     EQ b-ef.eqty
                      AND b-eb2.form-no  EQ b-ef.form-no
                      AND b-eb2.blank-no EQ ef.mis-bnum[li])) THEN DO:
          ASSIGN
           b-ef.mis-snum[li]  = b-ef.form-no
           b-ef.mis-bnum[li]  = ef.mis-bnum[li]
           b-ef.mis-cost[li]  = ef.mis-cost[li]
           b-ef.mis-matf[li]  = ef.mis-matf[li]
           b-ef.mis-labf[li]  = ef.mis-labf[li]
           b-ef.mis-matm[li]  = ef.mis-matm[li]
           b-ef.mis-labm[li]  = ef.mis-labm[li]
           b-ef.mis-simon[li] = ef.mis-simon[li]
           b-ef.mis-mkup[li]  = ef.mis-mkup[li].

          FOR EACH reftable
              WHERE reftable.reftable          EQ "EST-MISC"
                    AND reftable.company           EQ ef.company
                    AND reftable.loc               EQ ef.loc
                    AND reftable.code              EQ TRIM(ef.est-no) +
                                                  STRING(ef.form-no,"/99")
                    AND SUBSTR(reftable.code2,8,2) EQ STRING(li,"99"):
            CREATE b-ref.
            BUFFER-COPY reftable EXCEPT rec_key TO b-ref
            ASSIGN
             b-ref.CODE = TRIM(b-ef.est-no) + STRING(b-ef.form-no,"/99").
          END.
        END.
      END.
    END.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-est4 V-table-Win 
PROCEDURE enable-est4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    ENABLE fi_spec-qty-01 fi_spec-qty-02 fi_spec-qty-03 fi_spec-qty-04
           fi_spec-qty-05 fi_spec-qty-06 fi_spec-qty-07 fi_spec-qty-08.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   lv-spec-qty[01] = fi_spec-qty-01
   lv-spec-qty[02] = fi_spec-qty-02
   lv-spec-qty[03] = fi_spec-qty-03
   lv-spec-qty[04] = fi_spec-qty-04
   lv-spec-qty[05] = fi_spec-qty-05
   lv-spec-qty[06] = fi_spec-qty-06
   lv-spec-qty[07] = fi_spec-qty-07
   lv-spec-qty[08] = fi_spec-qty-08.

  DO li-sq = 1 TO EXTENT(ef.spec-qty):
    RUN custom/extradec.p (10000, lv-spec-qty[li-sq],
                           OUTPUT ef.spec-qty[li-sq]).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  lv-spec-qty = 0.

  IF AVAIL ef THEN
  DO li-sq = 1 TO EXTENT(ef.spec-qty):
    RUN custom/extradec.p (.0001, ef.spec-qty[li-sq],
                           OUTPUT lv-spec-qty[li-sq]).
  END.

  ASSIGN
   fi_spec-qty-01 = lv-spec-qty[01]
   fi_spec-qty-02 = lv-spec-qty[02]
   fi_spec-qty-03 = lv-spec-qty[03]
   fi_spec-qty-04 = lv-spec-qty[04]
   fi_spec-qty-05 = lv-spec-qty[05]
   fi_spec-qty-06 = lv-spec-qty[06]
   fi_spec-qty-07 = lv-spec-qty[07]
   fi_spec-qty-08 = lv-spec-qty[08].

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

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
  DEF VAR op-error AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN new-mis-upcost.

  /* validation ==*/
  RUN valid-mis-simon (0) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF ceprepprice-chr EQ "Profit" THEN
  DO:
     RUN valid-markup(0,OUTPUT op-error).
     IF op-error THEN RETURN NO-APPLY.
  END.
{&methods/lValidateError.i YES}
  if ef.spec-no[1]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[1]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[1].
         return no-apply.
      end.                          
      ef.spec-dscr[1]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[2]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[2]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[2].
         return no-apply.
      end.                          
      ef.spec-dscr[2]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[3]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[3]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[3].
         return no-apply.
      end.                          
      ef.spec-dscr[3]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[4]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[4]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[4].
         return no-apply.
      end.                          
      ef.spec-dscr[4]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[5]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[5]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[5].
         return no-apply.
      end.                          
      ef.spec-dscr[5]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[6]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[6]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[6].
         return no-apply.
      end.                          
      ef.spec-dscr[6]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[7]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[7]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[7].
         return no-apply.
      end.                          
      ef.spec-dscr[7]:screen-value = item.i-name.              
  end.    
  if ef.spec-no[8]:screen-value in frame {&frame-name} <> "" then do:
     find first item where item.company = ef.company and
                        item.indus = "2" and
                        INDEX("MOXY789@",ITEM.mat-type) GT 0 and
                        item.i-no = ef.spec-no[8]:screen-value
                        no-lock no-error.
      if not avail item then do:
         message "Invalid Item. Try Help." view-as alert-box.
         apply "entry" to ef.spec-no[8].
         return no-apply.
      end.                          
      ef.spec-dscr[8]:screen-value = item.i-name.              
  end.
  {&methods/lValidateError.i NO}    
  /* validation ==*/

  DISABLE ALL WITH FRAME {&FRAME-NAME}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-mis-cost V-table-Win 
PROCEDURE new-mis-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETE v-int AS INT NO-UNDO .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ef.mis-snum[1]:SCREEN-VALUE =
        IF ef.mis-cost[1]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[2]:SCREEN-VALUE =
        IF ef.mis-cost[2]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[3]:SCREEN-VALUE =
        IF ef.mis-cost[3]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[4]:SCREEN-VALUE =
        IF ef.mis-cost[4]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[5]:SCREEN-VALUE =
        IF ef.mis-cost[5]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[6]:SCREEN-VALUE =
        IF ef.mis-cost[6]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE "".

    IF ef.mis-cost[1]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[1]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[1]:SCREEN-VALUE = "1".
    IF ef.mis-cost[2]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[2]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[2]:SCREEN-VALUE = "1".
    IF ef.mis-cost[3]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[3]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[3]:SCREEN-VALUE = "1".
    IF ef.mis-cost[4]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[4]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[4]:SCREEN-VALUE = "1".
    IF ef.mis-cost[5]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[5]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[5]:SCREEN-VALUE = "1".
    IF ef.mis-cost[6]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[6]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[6]:SCREEN-VALUE = "1".
    IF v-int = 1 THEN
    IF ef.mis-cost[1]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[1]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[1]:SCREEN-VALUE = STRING(cemisc-dec).
    IF v-int = 2 THEN
    IF ef.mis-cost[2]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[2]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[2]:SCREEN-VALUE = STRING(cemisc-dec).

    IF v-int = 3 THEN
    IF ef.mis-cost[3]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[3]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[3]:SCREEN-VALUE = STRING(cemisc-dec).

    IF v-int = 4 THEN
    IF ef.mis-cost[4]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[4]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[4]:SCREEN-VALUE = STRING(cemisc-dec).

    IF v-int = 5 THEN
    IF ef.mis-cost[5]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[5]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[5]:SCREEN-VALUE = STRING(cemisc-dec).

    IF v-int = 6 THEN
    IF ef.mis-cost[6]:SCREEN-VALUE NE ""     AND
       DEC(ef.mis-mkup[6]:SCREEN-VALUE) EQ 0 THEN
      ef.mis-mkup[6]:SCREEN-VALUE = STRING(cemisc-dec).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-mis-upcost V-table-Win 
PROCEDURE new-mis-upcost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ef.mis-snum[1]:SCREEN-VALUE =
        IF ef.mis-cost[1]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[2]:SCREEN-VALUE =
        IF ef.mis-cost[2]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[3]:SCREEN-VALUE =
        IF ef.mis-cost[3]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[4]:SCREEN-VALUE =
        IF ef.mis-cost[4]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[5]:SCREEN-VALUE =
        IF ef.mis-cost[5]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE ""
     ef.mis-snum[6]:SCREEN-VALUE =
        IF ef.mis-cost[6]:SCREEN-VALUE NE "" THEN STRING(ef.form-no) ELSE "".

    IF ef.mis-cost[1]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[1]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[1]:SCREEN-VALUE = "1".
    IF ef.mis-cost[2]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[2]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[2]:SCREEN-VALUE = "1".
    IF ef.mis-cost[3]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[3]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[3]:SCREEN-VALUE = "1".
    IF ef.mis-cost[4]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[4]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[4]:SCREEN-VALUE = "1".
    IF ef.mis-cost[5]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[5]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[5]:SCREEN-VALUE = "1".
    IF ef.mis-cost[6]:SCREEN-VALUE NE "" AND
       ef.mis-bnum[6]:SCREEN-VALUE EQ "" THEN ef.mis-bnum[6]:SCREEN-VALUE = "1".

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
  {src/adm/template/snd-list.i "est"}
  {src/adm/template/snd-list.i "ef"}
  {src/adm/template/snd-list.i "eb"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-markup V-table-Win 
PROCEDURE valid-markup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv AS DEC NO-UNDO.

  {methods/lValidateError.i YES}
  li = ip-int.

  IF li EQ 0 THEN
    ASSIGN
     li     = 1
     ip-int = 6.

  DO li = li TO ip-int WITH FRAME {&FRAME-NAME}:
    CASE li:
      WHEN 1 THEN lv = DEC(EF.mis-mkup[1]:SCREEN-VALUE).
      WHEN 2 THEN lv = DEC(EF.mis-mkup[2]:SCREEN-VALUE).
      WHEN 3 THEN lv = DEC(EF.mis-mkup[3]:SCREEN-VALUE).
      WHEN 4 THEN lv = DEC(EF.mis-mkup[4]:SCREEN-VALUE).
      WHEN 5 THEN lv = DEC(EF.mis-mkup[5]:SCREEN-VALUE).
      WHEN 6 THEN lv = DEC(EF.mis-mkup[6]:SCREEN-VALUE).
    END CASE.

    IF lv GE 100 THEN DO:
       MESSAGE "Invalid Markup."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       CASE li:
         WHEN 1 THEN APPLY "entry" TO ef.mis-mkup[1].
         WHEN 2 THEN APPLY "entry" TO ef.mis-mkup[2].
         WHEN 3 THEN APPLY "entry" TO ef.mis-mkup[3].
         WHEN 4 THEN APPLY "entry" TO ef.mis-mkup[4].
         WHEN 5 THEN APPLY "entry" TO ef.mis-mkup[5].
         WHEN 6 THEN APPLY "entry" TO ef.mis-mkup[6].
       END CASE.
       op-error = YES.
       RETURN.
    END.

  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-mis-simon V-table-Win 
PROCEDURE valid-mis-simon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  li = ip-int.

  IF li EQ 0 THEN
    ASSIGN
     li     = 1
     ip-int = 6.

  DO li = li TO ip-int WITH FRAME {&FRAME-NAME}:
    CASE li:
      WHEN 1 THEN lv = ef.mis-cost[1]:SCREEN-VALUE.
      WHEN 2 THEN lv = ef.mis-cost[2]:SCREEN-VALUE.
      WHEN 3 THEN lv = ef.mis-cost[3]:SCREEN-VALUE.
      WHEN 4 THEN lv = ef.mis-cost[4]:SCREEN-VALUE.
      WHEN 5 THEN lv = ef.mis-cost[5]:SCREEN-VALUE.
      WHEN 6 THEN lv = ef.mis-cost[6]:SCREEN-VALUE.
    END CASE.

    IF lv NE "" THEN DO:
      CASE li:
        WHEN 1 THEN lv = ef.mis-simon[1]:SCREEN-VALUE.
        WHEN 2 THEN lv = ef.mis-simon[2]:SCREEN-VALUE.
        WHEN 3 THEN lv = ef.mis-simon[3]:SCREEN-VALUE.
        WHEN 4 THEN lv = ef.mis-simon[4]:SCREEN-VALUE.
        WHEN 5 THEN lv = ef.mis-simon[5]:SCREEN-VALUE.
        WHEN 6 THEN lv = ef.mis-simon[6]:SCREEN-VALUE.
      END CASE.

      IF INDEX("SIMON",lv) LE 0 THEN DO:
        MESSAGE "Simon code must be 'S', 'I', 'M', 'O', or 'N'..."
            VIEW-AS ALERT-BOX ERROR.
        CASE li:
          WHEN 1 THEN APPLY "entry" TO ef.mis-simon[1].
          WHEN 2 THEN APPLY "entry" TO ef.mis-simon[2].
          WHEN 3 THEN APPLY "entry" TO ef.mis-simon[3].
          WHEN 4 THEN APPLY "entry" TO ef.mis-simon[4].
          WHEN 5 THEN APPLY "entry" TO ef.mis-simon[5].
          WHEN 6 THEN APPLY "entry" TO ef.mis-simon[6].
        END CASE.
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

