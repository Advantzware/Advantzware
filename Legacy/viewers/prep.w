&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/prep.w

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
def var hld-code as cha no-undo.   /* for new record */
def var v-code as cha no-undo.     /* for new record */
def var li-create-cnt as int no-undo.  /* new record to cancel creation */
DEF VAR k_frac as dec init 6.25 no-undo.
DEF VAR ll-corr AS LOG NO-UNDO.
DEF VAR lv-format AS CHAR NO-UNDO.
DEFINE VARIABLE glProfit AS LOGICAL NO-UNDO.

/* gdm - 01270904 */
DEF VAR v_rmcrtflg AS LOG   NO-UNDO.
DEF BUFFER bf_item FOR ITEM.

{custom/gcompany.i}
{custom/gloc.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

{custom/format.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&SCOPED-DEFINE enable-prep enable-prep


DO TRANSACTION:
  {sys/inc/addprep.i}
END.

{sys/inc/f16to32.i}

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
&Scoped-define EXTERNAL-TABLES prep
&Scoped-define FIRST-EXTERNAL-TABLE prep


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prep.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prep.dscr prep.ml prep.cost prep.mkup ~
prep.spare-dec-1 prep.amtz prep.mat-type prep.dfault prep.cost-type ~
prep.actnum prep.prep-date prep.loc prep.loc-bin prep.simon prep.fgcat ~
prep.cust-no prep.cust-name prep.owner[1] prep.owner-%[1] prep.number-up ~
prep.no-of-impressions prep.owner[2] prep.owner-%[2] prep.disposal-date ~
prep.sts prep.carton-w prep.die-w prep.received-date prep.box-style ~
prep.carton-l prep.die-l prep.last-date prep.wood-type prep.carton-d ~
prep.last-est-no prep.last-order prep.i-no prep.procat 
&Scoped-define ENABLED-TABLES prep
&Scoped-define FIRST-ENABLED-TABLE prep
&Scoped-Define ENABLED-OBJECTS fi_cad-image fi_strip-loc fi_strip-loc-bin ~
fi_blank-loc fi_blank-loc-bin fi_job-no fi_job-no2 fi_cad# fi_fil# RECT-2 ~
RECT-3 RECT-4 
&Scoped-Define DISPLAYED-FIELDS prep.code prep.dscr prep.ml prep.cost ~
prep.mkup prep.spare-dec-1 prep.amtz prep.mat-type prep.dfault ~
prep.cost-type prep.uom prep.actnum prep.prep-date prep.loc prep.loc-bin ~
prep.simon prep.fgcat prep.cust-no prep.cust-name prep.owner[1] ~
prep.owner-%[1] prep.number-up prep.no-of-impressions prep.owner[2] ~
prep.owner-%[2] prep.disposal-date prep.sts prep.carton-w prep.die-w ~
prep.received-date prep.box-style prep.carton-l prep.die-l prep.last-date ~
prep.wood-type prep.carton-d prep.last-est-no prep.last-order prep.i-no ~
prep.procat 
&Scoped-define DISPLAYED-TABLES prep
&Scoped-define FIRST-DISPLAYED-TABLE prep
&Scoped-Define DISPLAYED-OBJECTS fi_cad-image mat_dscr costtype_descr ~
uom_dscr ls-time fi_strip-loc fi_strip-loc-bin fi_blank-loc ~
fi_blank-loc-bin fi_job-no fi_job-no2 fi_cad# fi_fil# 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS prep.code 
&Scoped-define ADM-ASSIGN-FIELDS fi_job-no fi_job-no2 
&Scoped-define DISPLAY-FIELD prep.mat-type prep.cost-type prep.uom ~
fi_job-no fi_job-no2 

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
DEFINE VARIABLE costtype_descr AS CHARACTER FORMAT "x(30)" INIT ""
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE fi_blank-loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Blanker Whs" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_blank-loc-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cad# AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cad  #" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cad-image AS CHARACTER FORMAT "x(80)" 
     LABEL "Image" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE fi_fil# AS CHARACTER FORMAT "X(15)":U 
     LABEL "File  #" 
     VIEW-AS FILL-IN 
     SIZE 22.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Last Job # Used" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_strip-loc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Stripper Whs" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_strip-loc-bin AS CHARACTER FORMAT "X(8)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ls-time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE mat_dscr AS CHARACTER FORMAT "x(30)"  INITIAL ""
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE uom_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 4.05.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 3.19.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 131 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     prep.code AT ROW 1.24 COL 17 COLON-ALIGNED
          LABEL "Prep Code" FORMAT "X(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     prep.dscr AT ROW 1.24 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     prep.ml AT ROW 1.24 COL 105 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Material", yes,
"Labor", no
          SIZE 25.8 BY 1
     fi_cad-image AT ROW 17.71 COL 18 COLON-ALIGNED
     prep.cost AT ROW 2.43 COL 48.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     prep.mkup AT ROW 2.43 COL 73 COLON-ALIGNED FORMAT "->>9.99<<"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     prep.spare-dec-1 AT ROW 2.43 COL 91.8 COLON-ALIGNED WIDGET-ID 10
          LABEL "Price" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
          FONT 4
     prep.amtz AT ROW 3.62 COL 96 COLON-ALIGNED
          LABEL "Amort"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     prep.mat-type AT ROW 4.33 COL 17 COLON-ALIGNED
          LABEL "Material Type"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     mat_dscr AT ROW 4.33 COL 26 COLON-ALIGNED NO-LABEL
     prep.dfault AT ROW 3.52 COL 19
          VIEW-AS TOGGLE-BOX
          SIZE 27.4 BY .76
     prep.cost-type AT ROW 5.29 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     costtype_descr AT ROW 5.29 COL 26 COLON-ALIGNED NO-LABEL
     prep.uom AT ROW 6.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     uom_dscr AT ROW 6.24 COL 26 COLON-ALIGNED NO-LABEL
     prep.actnum AT ROW 7.19 COL 17 COLON-ALIGNED
          LABEL "Sales G/L #"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 15 FONT 4
     prep.prep-date AT ROW 2.43 COL 113 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ls-time AT ROW 3.62 COL 113 COLON-ALIGNED
     prep.loc AT ROW 5.24 COL 88 COLON-ALIGNED
          LABEL "Prep Whs"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     prep.loc-bin AT ROW 5.24 COL 113 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     fi_strip-loc AT ROW 6.24 COL 88 COLON-ALIGNED
     fi_strip-loc-bin AT ROW 6.24 COL 113 COLON-ALIGNED
     fi_blank-loc AT ROW 7.19 COL 88 COLON-ALIGNED
     fi_blank-loc-bin AT ROW 7.19 COL 113 COLON-ALIGNED
     prep.simon AT ROW 8.52 COL 6 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Integrate", "I":U,
"Markup", "M":U,
"No Charge", "N":U,
"Override", "O":U,
"Separate Bill", "S":U
          SIZE 87 BY 1.19
     prep.fgcat AT ROW 8.43 COL 113 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     prep.cust-no AT ROW 10.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     prep.cust-name AT ROW 10.14 COL 34 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     prep.owner[1] AT ROW 11.62 COL 85 COLON-ALIGNED
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     prep.owner-%[1] AT ROW 11.62 COL 117 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     prep.number-up AT ROW 11.33 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     prep.no-of-impressions AT ROW 11.33 COL 55 COLON-ALIGNED
         FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     prep.owner[2] AT ROW 12.57 COL 85 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     prep.owner-%[2] AT ROW 12.57 COL 117 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     prep.disposal-date AT ROW 13.91 COL 71 COLON-ALIGNED
          LABEL "Disposal Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     prep.sts AT ROW 13.91 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     prep.carton-w AT ROW 14.57 COL 18 COLON-ALIGNED
          LABEL "Width"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.die-w AT ROW 14.57 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.received-date AT ROW 14.86 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     prep.box-style AT ROW 14.86 COL 114 COLON-ALIGNED FORMAT "X(6)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.carton-l AT ROW 15.52 COL 18 COLON-ALIGNED
          LABEL "Length"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.die-l AT ROW 15.52 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.last-date AT ROW 15.81 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     prep.wood-type AT ROW 15.81 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.carton-d AT ROW 16.48 COL 18 COLON-ALIGNED
          LABEL "Depth"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     prep.last-est-no AT ROW 16.76 COL 71 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     prep.last-order AT ROW 16.76 COL 114 COLON-ALIGNED FORMAT ">>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     fi_job-no AT ROW 17.71 COL 71 COLON-ALIGNED
     fi_job-no2 AT ROW 17.71 COL 87 COLON-ALIGNED
     prep.i-no AT ROW 2.43 COL 17 COLON-ALIGNED HELP
          "Enter R/M Item Number"
          LABEL "RM Item #" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     fi_cad# AT ROW 12.52 COL 17 COLON-ALIGNED WIDGET-ID 2
     fi_fil# AT ROW 12.52 COL 52.2 COLON-ALIGNED WIDGET-ID 6
     prep.procat AT ROW 9.57 COL 113 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.91 COL 121
     "Owner" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 10.91 COL 95
     "Die" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 13.86 COL 36
     "Carton" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 13.86 COL 21
     RECT-2 AT ROW 13.62 COL 6
     RECT-3 AT ROW 10.67 COL 80
     RECT-4 AT ROW 1.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE NO-VALIDATE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.prep
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
         HEIGHT             = 18.05
         WIDTH              = 131.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN prep.actnum IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.amtz IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.box-style IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN prep.carton-d IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.carton-l IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.carton-w IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.code IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN prep.cost-type IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN costtype_descr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prep.disposal-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_job-no IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN fi_job-no2 IN FRAME F-Main
   2 4                                                                  */
/* SETTINGS FOR FILL-IN prep.i-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN prep.last-est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN prep.last-order IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN prep.loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ls-time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prep.mat-type IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN mat_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN prep.mkup IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN prep.owner[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.owner[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN prep.spare-dec-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN prep.uom IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN uom_dscr IN FRAME F-Main
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
  def var v-up like prep.number-up no-undo.
  DEF VAR lw-focus AS WIDGET NO-UNDO.
  DEF VAR look-recid AS RECID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    lw-focus = FOCUS.

    CASE lw-focus:NAME:

      WHEN "i-no" THEN DO:
         RUN windows/l-itmall.w (gcompany, "","", prep.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
         IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN
            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
      END.

      WHEN "mat-type" THEN DO:         
        RUN windows/l-matpr.w (gcompany, prep.mat-type:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" AND ENTRY(1,char-val) NE prep.mat-type:SCREEN-VALUE THEN
          ASSIGN
           prep.mat-type:SCREEN-VALUE = ENTRY(1,char-val)
           mat_dscr:SCREEN-VALUE      = ENTRY(2,char-val).
      END.

      WHEN "cost-type" THEN DO:         
        RUN windows/l-costtp.w (gcompany, prep.cost-type:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" AND ENTRY(1,char-val) NE prep.cost-type:SCREEN-VALUE THEN
          ASSIGN
           prep.cost-type:SCREEN-VALUE = ENTRY(1,char-val)
           costtype_descr:SCREEN-VALUE = ENTRY(2,char-val).
      END.

      WHEN "actnum" THEN DO:         
        IF prep.simon:SCREEN-VALUE EQ "S" THEN 
            RUN windows/l-acct2.w (gcompany, "R", prep.actnum:SCREEN-VALUE, OUTPUT char-val).
        ELSE 
            RUN windows/l-acct2.w (gcompany, "", prep.actnum:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" AND ENTRY(1,char-val) NE prep.actnum:SCREEN-VALUE THEN
          prep.actnum:SCREEN-VALUE = ENTRY(1,char-val).
      END.

      when "last-est-no" then do:         
        run windows/l-est.w (gcompany,gloc,lw-focus:screen-value, output char-val).
        if char-val ne "" then do:
          find eb where recid(eb) eq int(entry(1,char-val)) no-lock no-error.

          if avail eb then do:
            assign
             lw-focus:screen-value          = eb.est-no
             prep.cust-no:screen-value   = eb.cust-no
             prep.carton-l:screen-value  = string(eb.len)
             prep.carton-w:screen-value  = string(eb.wid)
             prep.carton-d:screen-value  = string(eb.dep)
             prep.box-style:screen-value = eb.style.

            find first est
                where est.company eq eb.company
                  and est.est-no  eq eb.est-no
                no-lock no-error.
            if avail est then do:
              run sys/inc/numup.p (eb.company, eb.est-no, eb.form-no, output v-up).

              assign
               prep.last-order:screen-value = string(eb.ord-no)
               prep.number-up:screen-value  = string(v-up).
            end.

            find first ef
                where ef.company eq eb.company
                  and ef.est-no  eq eb.est-no
                  and ef.form-no eq eb.form-no
                no-lock no-error.

            if avail ef then
              assign
               prep.die-w:screen-value = string(ef.trim-w)
               prep.die-l:screen-value = string(ef.trim-l).
            prep.dscr:screen-value = prep.dscr.

            find first cust
                where cust.company eq gcompany
                  and cust.cust-no eq prep.cust-no:screen-value
                no-lock no-error.
            if avail cust then
              assign
               prep.cust-name:screen-value  = cust.name
               prep.owner[1]:screen-value   = cust.name
               prep.owner-%[1]:screen-value = "100"
               prep.owner[2]:screen-value   = ""
               prep.owner-%[2]:screen-value = "0".
          end.
        end.
      end.

      when "box-style" then do:         
        run windows/l-style.w (gcompany,lw-focus:screen-value, output char-val).
        if char-val ne "" then do: 
          lw-focus:screen-value = entry(1,char-val).
        end.  
      end.

      when "cust-no" then do:
        run windows/l-cust.w (gcompany,lw-focus:screen-value, output char-val).
        if char-val ne "" then do:
          assign
           lw-focus:screen-value          = entry(1,char-val)
           prep.cust-name:screen-value = entry(2,char-val).
        end.
      end.  /* cust-no*/

      when "loc" then do:
          run rm/l-loc.w (gcompany, lw-focus:screen-value, output char-val).
          if char-val <> "" then 
              assign lw-focus:screen-value  = entry(1,char-val).  
      end.

      when "loc-bin" then do:
          run rm/l-locbin.w (gcompany,prep.loc:screen-value, output char-val).
          if char-val <> "" then assign lw-focus:screen-value  = entry(1,char-val).                  

      END.
      when "fgcat" then do:
             run windows/l-fgcat.w (gcompany,lw-focus:screen-value, output char-val).
             if char-val <> "" then lw-focus:screen-value = entry(1,char-val).
      end.
      when "procat" then do:
             run windows/l-rmcat.w (gcompany,lw-focus:screen-value, output char-val).
             if char-val <> "" then lw-focus:screen-value = entry(1,char-val).
      end.
      otherwise do:
        run applhelp.p.
        if g_lookup-var ne "" then lw-focus:screen-value = g_lookup-var.
      end.  /* otherwise */
    end case.

    apply "entry" to lw-focus.  
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.actnum V-table-Win
ON LEAVE OF prep.actnum IN FRAME F-Main /* Sales G/L # */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED THEN RUN new-actnum.

    RUN valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.carton-d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.carton-d V-table-Win
ON LEAVE OF prep.carton-d IN FRAME F-Main /* Depth */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.carton-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.carton-l V-table-Win
ON LEAVE OF prep.carton-l IN FRAME F-Main /* Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.carton-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.carton-w V-table-Win
ON LEAVE OF prep.carton-w IN FRAME F-Main /* Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.code V-table-Win
ON LEAVE OF prep.code IN FRAME F-Main /* Prep Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.cost V-table-Win
ON LEAVE OF prep.cost IN FRAME F-Main /* Cost */
DO:
    RUN UpdatePrice.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.cost-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.cost-type V-table-Win
ON LEAVE OF prep.cost-type IN FRAME F-Main /* Cost Type */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and
      not can-find(costtype where costtype.company = gcompany and costtype.loc = gloc 
                              and costtype.cost-type= self:screen-value)
   then do:
      message "Invalid Cost Type. Try Help. " view-as alert-box error.
      return no-apply.
   end.
  {methods/dispflds.i}

   FIND FIRST costtype where costtype.company = gcompany and costtype.loc = gloc 
                              and costtype.cost-type= self:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL costtype THEN do:
      ASSIGN
          costtype_descr = costtype.descr
          costtype_descr:SCREEN-VALUE = costtype.descr .
   end.
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.cust-no V-table-Win
ON LEAVE OF prep.cust-no IN FRAME F-Main /* Cust. # */
DO:
  DEF BUFFER b-cust FOR cust.
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" THEN
    DO:
      FIND FIRST b-cust where
        b-cust.company = gcompany and
        b-cust.cust-no = self:SCREEN-VALUE
        NO-LOCK NO-ERROR.

      IF NOT AVAIL b-cust THEN DO:
        message "Invalid Customer!. Try help." view-as alert-box error.
        return no-apply.
      end.
      ELSE prep.cust-name:SCREEN-VALUE = b-cust.name.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.dfault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.dfault V-table-Win
ON return OF prep.dfault IN FRAME F-Main /* Use in all Estimates */
DO:
    apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.die-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.die-l V-table-Win
ON LEAVE OF prep.die-l IN FRAME F-Main /* Die Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.die-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.die-w V-table-Win
ON LEAVE OF prep.die-w IN FRAME F-Main /* Die Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.fgcat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.fgcat V-table-Win
ON LEAVE OF prep.fgcat IN FRAME F-Main /* FG Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fgcat(INPUT YES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cad#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cad# V-table-Win
ON LEAVE OF fi_cad# IN FRAME F-Main /* Cad  # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fil#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fil# V-table-Win
ON LEAVE OF fi_fil# IN FRAME F-Main /* File  # */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no V-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main /* Last Job # Used */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no V-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main /* Last Job # Used */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no2 V-table-Win
ON LEAVE OF fi_job-no2 IN FRAME F-Main /* - */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.i-no V-table-Win
ON LEAVE OF prep.i-no IN FRAME F-Main /* RM Item # */
DO:
/* gdm - 01270904 - ADDED RM ITEM CREATION,IF SELECTED */

  IF LASTKEY <> -1 AND  
      self:SCREEN-VALUE <> "" AND 
      NOT CAN-FIND(FIRST ITEM 
                   WHERE ITEM.company = gcompany
                   AND SUBSTRING(ITEM.i-no,1,10) = self:SCREEN-VALUE)
    THEN DO:      

      MESSAGE 
          "Invalid RM Item #."  SKIP
          "Would you like to create this RM Item # ?"
         VIEW-AS ALERT-BOX MESSAGE
         BUTTON YES-NO
         UPDATE v_rmcrtflg.      

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.last-est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.last-est-no V-table-Win
ON VALUE-CHANGED OF prep.last-est-no IN FRAME F-Main /* Last Estimate */
DO:
  RUN display-dim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.mat-type V-table-Win
ON LEAVE OF prep.mat-type IN FRAME F-Main /* Material Type */
DO:
  {&methods/lValidateError.i YES}
  if lastkey <> -1 and self:screen-value <> "" and
      not can-find(matprep where matprep.company = gcompany
                             and matprep.mat = self:screen-value)
   then do:
      message "Invalid Material Type. Try Help. " view-as alert-box error.
      return no-apply.
   end.

  {methods/dispflds.i}

   FIND LAST matprep WHERE matprep.company = gcompany
      AND matprep.mat = self:screen-value 
                   NO-LOCK NO-ERROR.                       
   IF AVAIL matprep THEN DO:                               
       ASSIGN                                                  
           mat_dscr = matprep.dscr                               
           mat_dscr:SCREEN-VALUE = matprep.dscr.                 
   END.  
   {&methods/lValidateError.i NO}                                                  
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.mkup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.mkup V-table-Win
ON LEAVE OF prep.mkup IN FRAME F-Main /* Markup */
DO:
  RUN UpdatePrice.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.ml
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.ml V-table-Win
ON RETURN OF prep.ml IN FRAME F-Main /* M/L */
DO:
  apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.ml V-table-Win
ON VALUE-CHANGED OF prep.ml IN FRAME F-Main /* M/L */
DO:
    apply "tab" to self.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.procat V-table-Win
ON LEAVE OF prep.procat IN FRAME F-Main /* RM Category */
DO:
    IF LASTKEY NE -1 THEN DO:
    RUN valid-rmcat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.simon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.simon V-table-Win
ON return OF prep.simon IN FRAME F-Main /* SIMON */
DO:
    apply "tab" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.simon V-table-Win
ON VALUE-CHANGED OF prep.simon IN FRAME F-Main /* SIMON */
DO:
  IF prep.simon:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "S" THEN RUN valid-actnum.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.spare-dec-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.spare-dec-1 V-table-Win
ON LEAVE OF prep.spare-dec-1 IN FRAME F-Main /* Price */
DO:
  RUN UpdateMarkup.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prep.uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prep.uom V-table-Win
ON LEAVE OF prep.uom IN FRAME F-Main /* UOM */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and self:screen-value <> "" and
      not can-find(uom where uom.uom = self:screen-value)
   then do:
      message "Invalid UOM. " view-as alert-box error.
      return no-apply.
   end.

  {methods/dispflds.i}
   {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN dispatch ('add-record').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "prep"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prep"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-dim V-table-Win 
PROCEDURE display-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-est-no LIKE prep.last-est-no NO-UNDO.
  DEF VAR lv-crt-w LIKE prep.carton-w NO-UNDO.
  DEF VAR lv-crt-l LIKE prep.carton-l NO-UNDO.
  DEF VAR lv-crt-d LIKE prep.carton-d NO-UNDO.
  DEF VAR lv-die-w LIKE prep.die-w NO-UNDO.
  DEF VAR lv-die-l LIKE prep.die-l NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-format EQ "" THEN lv-format = prep.carton-w:FORMAT.

    ASSIGN
     lv-crt-w = DEC(prep.carton-w:SCREEN-VALUE)
     lv-crt-l = DEC(prep.carton-l:SCREEN-VALUE)
     lv-crt-d = DEC(prep.carton-d:SCREEN-VALUE)
     lv-die-w = DEC(prep.die-w:SCREEN-VALUE)
     lv-die-l = DEC(prep.die-l:SCREEN-VALUE).

    IF ll-corr THEN DO:
      {sys/inc/k16bb.i lv-crt-w}
      {sys/inc/k16bb.i lv-crt-l}
      {sys/inc/k16bb.i lv-crt-d}
      {sys/inc/k16bb.i lv-die-w}
      {sys/inc/k16bb.i lv-die-l}
    END.

    ASSIGN
     lv-est-no = prep.last-est-no:SCREEN-VALUE
     lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no)
     ll-corr   = AVAIL prep                   AND
                 TRIM(lv-est-no) NE "" AND
                 CAN-FIND(FIRST est
                          WHERE est.company  EQ prep.company
                            AND est.est-no   EQ lv-est-no
                            AND est.est-type GT 4).

    IF ll-corr THEN DO:
       ASSIGN
        lv-crt-w = {sys/inc/k16.i lv-crt-w}
        lv-crt-l = {sys/inc/k16.i lv-crt-l}
        lv-crt-d = {sys/inc/k16.i lv-crt-d}
        lv-die-w = {sys/inc/k16.i lv-die-w}
        lv-die-l = {sys/inc/k16.i lv-die-l}.

       IF v-cecscrn-char NE "Decimal" THEN
          ASSIGN
             prep.carton-w:FORMAT = "->>,>>9.99"
             prep.carton-l:FORMAT = "->>,>>9.99"
             prep.carton-d:FORMAT = "->>,>>9.99"
             prep.die-w:FORMAT    = "->>,>>9.99"
             prep.die-l:FORMAT    = "->>,>>9.99".
       ELSE
          ASSIGN
             prep.carton-w:FORMAT = "->>,>>9.999999"
             prep.carton-l:FORMAT = "->>,>>9.999999"
             prep.carton-d:FORMAT = "->>,>>9.999999"
             prep.die-w:FORMAT    = "->>,>>9.999999"
             prep.die-l:FORMAT    = "->>,>>9.999999".
    END.

    ELSE
      ASSIGN
       prep.carton-w:FORMAT = lv-format
       prep.carton-l:FORMAT = lv-format
       prep.carton-d:FORMAT = lv-format
       prep.die-w:FORMAT    = lv-format
       prep.die-l:FORMAT    = lv-format.

    ASSIGN
     prep.carton-w:SCREEN-VALUE = STRING(lv-crt-w)
     prep.carton-l:SCREEN-VALUE = STRING(lv-crt-l)
     prep.carton-d:SCREEN-VALUE = STRING(lv-crt-d)
     prep.die-w:SCREEN-VALUE    = STRING(lv-die-w)
     prep.die-l:SCREEN-VALUE    = STRING(lv-die-l).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-prep-job V-table-Win 
PROCEDURE enable-prep-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN fi_job-no:SENSITIVE = YES
            fi_job-no2:SENSITIVE = YES

            /* gdm - 12010903*/
            fi_cad#:SENSITIVE = TRUE
            fi_fil#:SENSITIVE = TRUE
            .
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win
PROCEDURE local-apply-entry:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RUN updatePrice.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */



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

  /* gdm - 12010903*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN fi_cad# fi_fil#. 
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-corr THEN DO:
   {sys/inc/k16bb.i prep.carton-w}
   {sys/inc/k16bb.i prep.carton-l}
   {sys/inc/k16bb.i prep.carton-d}
   {sys/inc/k16bb.i prep.die-w}
   {sys/inc/k16bb.i prep.die-l}
  END.

  RUN reftable-values (NO).

  /* gdm - 01270904 - IF FLAG IS YES _ CREATE RM ITEM RECORD */  
  IF v_rmcrtflg THEN RUN RM-item-create.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    if li-create-cnt <= 1 then hld-code = "".

    ASSIGN fi_job-no:SENSITIVE = NO
           fi_job-no2:SENSITIVE = NO
           /* gdm - 12010903*/
           fi_cad#:SENSITIVE = FALSE
           fi_fil#:SENSITIVE = FALSE.
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
  DEF BUFFER b-prep FOR prep.

  DEF VAR v-int AS INT NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR v-test-int AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/prep.i}
  ASSIGN
    fi_job-no = ""
    fi_job-no2 = 0
    uom_dscr = "Each"
    li-create-cnt = li-create-cnt + 1.  /* to reset hld-code in cancel-record */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'prepfly-target':U,OUTPUT char-hdl).


  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN get-defaults IN WIDGET-HANDLE(char-hdl) (INPUT ROWID(prep)).
    FIND CURRENT prep NO-ERROR.
  END.

  display prep.uom uom_dscr fi_job-no fi_job-no2 with frame {&frame-name}.

  /* auto assign */
  if hld-code EQ "" AND prep.code EQ "" THEN DO:

      repeat:
         MESSAGE TRIM(IF addprep-chr EQ "" THEN
                        "Enter 'R'otary Die, 'F'lat Die, 'P'rinting Die/Plate, 'M'=Foam, " +
                        "or leave blank"
                      ELSE addprep-chr) + ":" UPDATE v-code.
         if index("RFPM",v-code) eq 0 and v-code ne "" then undo, retry.
         v-code = caps(v-code).
         leave.
      end.
  END.
  ELSE IF prep.CODE BEGINS "!" AND LOOKUP(prep.code, "!R,!F,!P,!M,!") GT 0 THEN DO:

    v-code = substring(CAPS(prep.CODE), 2, 1). 
    IF v-code = "!" THEN
        ASSIGN v-code = ""
               prep.CODE = ""
               prep.CODE:SCREEN-VALUE = "".
  END.

  if v-code <> "" then do:
     assign  v-int         = 9999
             prep.mat-type = v-code
             prep.cost-type:SCREEN-VALUE = prep.cost-type .
     /* Note, this should be changed to use est/calcMatType.p */
     if prep.mat-type eq "F" then prep.mat-type = "D".
     IF prep.mat-type EQ "M" THEN prep.mat-type = "A".

     prep.mat-type:SCREEN-VALUE = prep.mat-type .

     if hld-code = "" then 
     for each b-prep    where b-prep.company          eq gcompany
/*           and b-prep.loc              eq gloc */
          and b-prep.code             begins v-code
          and substr(b-prep.code,2,4) ge "0001"
          and substr(b-prep.code,2,4) le "9999"
          and substr(b-prep.code,5,1) ne ""          
        NO-LOCK USE-INDEX real-index:


        v-test-int = INT(substr(b-prep.code,2,4)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        IF INDEX(b-prep.CODE, "-") GT 0 OR INDEX(b-prep.CODE, "+") GT 0 THEN
            NEXT.

         if v-int + 1 lt int(substr(b-prep.code,2,4)) then leave.
         v-int = int(substr(b-prep.code,2,4)).

     end.
     else v-int = int(substr(hld-code,2,4)).

     prep.code = v-code + if v-int ne 9999 then string(v-int + 1,"9999") else "0001".
  end.

  /* gdm - 12010903*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN fi_cad# = ""
           fi_fil# = ""
           fi_cad#:SCREEN-VALUE = "" 
           fi_fil#:SCREEN-VALUE = "". 

  END.

  disp prep.amtz prep.ml prep.simon with frame {&frame-name}.

  DO WITH FRAME {&FRAME-NAME}:

  IF  prep.mat-type <> "" THEN
      FIND FIRST matprep where matprep.company = gcompany
                             and matprep.mat = prep.mat-type:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL matprep then do:
       ASSIGN
           mat_dscr = matprep.dscr
           mat_dscr:SCREEN-VALUE = matprep.dscr  .      
   end.

   IF prep.cost-type:SCREEN-VALUE <> "" THEN
   FIND FIRST costtype where costtype.company = gcompany and costtype.loc = gloc 
                              and costtype.cost-type= prep.cost-type:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL costtype THEN do:
      ASSIGN
          costtype_descr = costtype.descr
          costtype_descr:SCREEN-VALUE = costtype.descr .
   end.
  END.

/*     FIND LAST matprep WHERE matprep.mat = prep.mat-type */
/*                   NO-LOCK NO-ERROR.                     */
/* MESSAGE "CREATE " AVAIL matprep                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
/*     IF AVAIL matprep THEN DO:                           */
/*                                                         */
/*     ASSIGN                                              */
/*      mat_dscr = matprep.dscr                            */
/*      mat_dscr:SCREEN-VALUE = matprep.dscr.              */
/*     END.                                                */
  hld-code = prep.code.

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
  RUN reftable-values(INPUT YES).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ls-time = IF AVAIL prep THEN STRING(prep.prep-time,"HH:MM:SS") ELSE "".

  disp ls-time fi_job-no fi_job-no2 fi_cad# fi_fil# with frame {&frame-name}.

  ll-corr = NO.
  RUN UpdatePrice.
  RUN display-dim.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE check-update-mode :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER op-in-update AS LOGICAL INIT NO NO-UNDO.

IF prep.dscr:SENSITIVE IN FRAME {&FRAME-NAME} EQ YES THEN do:
    op-in-update = YES .
END.  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_cad-image:SENSITIVE = NO
     fi_strip-loc:SENSITIVE = NO
     fi_strip-loc-bin:SENSITIVE = NO
     fi_blank-loc:SENSITIVE = NO
     fi_blank-loc-bin:SENSITIVE = NO
     fi_job-no:SENSITIVE = NO
     fi_job-no2:SENSITIVE = NO

     /* gdm - 12010903*/
     fi_cad#:SENSITIVE = FALSE
     fi_fil#:SENSITIVE = FALSE.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
RUN sys/ref/nk1Look.p(INPUT cocode,
                      INPUT "CEPrepPrice",
                      INPUT "C",
                      INPUT NO,
                      INPUT NO,
                      INPUT "",
                      INPUT "",
                      OUTPUT cReturn,
                      OUTPUT lFound).
IF lFound THEN
    glProfit = cReturn EQ "Profit".

RUN UpdatePrice.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF BUFFER bCust FOR cust.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  do with frame {&frame-name} :
    if prep.mat-type:screen-value <> "" and
       not can-find(matprep where matprep.company = gcompany
                              and matprep.mat = prep.mat-type:screen-value)
    then do:
      message "Invalid Material Type. Try Help. " view-as alert-box error.
      apply "entry" to prep.mat-type.
      return no-apply.
    end.
    if /*prep.cost-type:screen-value <> "" and */
       not can-find(costtype where costtype.company = gcompany and costtype.loc = gloc 
                               and costtype.cost-type= prep.cost-type:screen-value)
    then do:
      message "Invalid Cost Type. Try Help. " view-as alert-box error.
      apply "entry" to prep.cost-type.
      return no-apply.
    end.
  end.
  {&methods/lValidateError.i NO}
  RUN valid-rm-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-fgcat(INPUT NO) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-rmcat NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  if PREP.CUST-no:screen-value <> "" THEN
  DO:
    FIND FIRST bCust where
         bCust.company = gcompany and
         bCust.cust-no = prep.cust-no:screen-value
         NO-LOCK NO-ERROR.

    IF NOT AVAIL bCust THEN
    DO:
       message "Invalid Customer!. Try help." view-as alert-box error.
       apply "entry" to prep.cust-no.
       return no-apply.
    END.
    ELSE
    DO:
      prep.cust-name:SCREEN-VALUE = bCust.NAME.
      RELEASE bCust.
    END.
  END.
  {&methods/lValidateError.i NO}
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-dim (prep.carton-w:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (prep.carton-l:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (prep.carton-d:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (prep.die-w:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (prep.die-l:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'prepfly-target':U,OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN set-code IN WIDGET-HANDLE(char-hdl) (prep.code).

  ASSIGN fi_job-no:SENSITIVE = NO
         fi_job-no2:SENSITIVE = NO
         /* gdm - 12010903*/
         fi_cad#:SENSITIVE = FALSE
         fi_fil#:SENSITIVE = FALSE.


END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-actnum V-table-Win 
PROCEDURE new-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND account
        WHERE account.company EQ gcompany
          AND account.actnum  BEGINS prep.actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL account THEN prep.actnum:SCREEN-VALUE = account.actnum.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepfly V-table-Win 
PROCEDURE prepfly :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch ("add-record").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.

  /* gdm - */
  DEF BUFFER bf-reftable FOR reftable.
 IF AVAIL prep THEN DO:

    IF ip-display THEN DO:
      ASSIGN
        fi_job-no = prep.last-job-no
        fi_job-no2 = prep.last-job-no2
         .
        FIND FIRST bf-reftable NO-LOCK
               WHERE bf-reftable.reftable EQ "PREPCADFILE"
                 AND bf-reftable.rec_key  EQ prep.rec_key NO-ERROR.  
        IF AVAIL bf-reftable THEN DO:
           ASSIGN fi_cad# =  bf-reftable.CODE
                  fi_fil# = bf-reftable.code2.
        END.
        RELEASE bf-reftable.
    END.
    ELSE
    DO:
       IF fi_job-no NE prep.last-job-no OR
          fi_job-no2 NE prep.last-job-no2 THEN
          DO:
             REPEAT:
                DO:
                   ASSIGN
                      prep.last-job-no = fi_job-no
                      prep.last-job-no2 = fi_job-no2.
                   LEAVE.
                END.
             END.
          END.

       /* should be able to save blank cad# and fil# */
       /*IF fi_cad# NE "" OR fi_fil# NE "" THEN DO:*/

          FIND FIRST bf-reftable EXCLUSIVE-LOCK
               WHERE bf-reftable.reftable EQ "PREPCADFILE"
                 AND bf-reftable.rec_key  EQ prep.rec_key NO-ERROR.  
          IF NOT AVAIL bf-reftable THEN DO:
             CREATE bf-reftable.
             ASSIGN bf-reftable.reftable = "PREPCADFILE"
                    bf-reftable.rec_key  = prep.rec_key.
          END.

          ASSIGN bf-reftable.code  = fi_cad#
                 bf-reftable.code2 = fi_fil#.

          RELEASE bf-reftable.
       /*END.*/
    END. /* else */
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RM-item-create V-table-Win 
PROCEDURE RM-item-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v_industry  AS CHAR NO-UNDO.
DEF VAR v_mat-type  AS CHAR NO-UNDO.
DEF VAR v_flg       AS LOG  NO-UNDO.
DEF VAR v_i-no      AS CHAR NO-UNDO.
DEF VAR v_dscr      AS CHAR NO-UNDO.
DEF VAR v_cost-type AS CHAR NO-UNDO.
DEF VAR v_procat    AS CHAR NO-UNDO.
DEF VAR v_loc       AS CHAR NO-UNDO.
DEF VAR v_loc-bin   AS CHAR NO-UNDO.



DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
       v_i-no      = IF prep.i-no NE ""            
                       THEN prep.i-no
                       ELSE prep.i-no:SCREEN-VALUE 
       v_dscr      = IF prep.dscr NE ""            
                       THEN prep.dscr
                       ELSE prep.dscr:SCREEN-VALUE
       v_mat-type  = IF prep.mat-type NE ""            
                       THEN prep.mat-type              
                       ELSE prep.mat-type:SCREEN-VALUE
       v_cost-type = IF prep.cost-type NE ""
                       THEN prep.cost-type
                       ELSE prep.cost-type:SCREEN-VALUE
       v_procat    = IF prep.fgcat NE ""
                       THEN prep.fgcat 
                       ELSE prep.fgcat:SCREEN-VALUE
       v_loc       = IF prep.loc NE ""
                       THEN prep.loc
                       ELSE prep.loc:SCREEN-VALUE
       v_loc-bin   = IF prep.loc-bin NE ""
                       THEN prep.loc-bin
                       ELSE prep.loc-bin:SCREEN-VALUE.

END. /* DO */

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ gcompany
      AND sys-ctrl.name    EQ "CEMENU" NO-ERROR.
IF AVAIL sys-ctrl THEN DO:

    IF sys-ctrl.char-fld EQ "Foldware" 
       THEN ASSIGN v_industry =  "1".
       ELSE IF sys-ctrl.char-fld EQ "Corrware" 
         THEN ASSIGN v_industry =  "2".
         ELSE ASSIGN v_industry =  "B".
END. /* IF AVAIL sys-ctrl */

IF v_industry = "B" THEN DO:

    IF (prep.last-est-no NE "" OR 
        prep.last-est-no:SCREEN-VALUE NE "") THEN DO:

        FIND FIRST est NO-LOCK
            WHERE est.company = prep.company
              AND (est.est-no  = prep.last-est-no OR 
                   est.est-no  = prep.last-est-no:SCREEN-VALUE) NO-ERROR.
        IF AVAIL est THEN DO:

            IF est.est-type GE 1 AND  
               est.est-type LE 4 THEN ASSIGN v_industry = "1".
            IF est.est-type GE 5 AND  
               est.est-type LE 8 THEN ASSIGN v_industry = "2".

        END. /* IF AVAIL est */
    END. /* IF (prep.last-est-no */


    /* CHECK IF INDUSTRY STILL = "B */
    IF v_industry = "B" THEN DO:

        MESSAGE 
              "Is this a Folding Carton Material?"
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO
            UPDATE v_flg.

        IF v_flg THEN ASSIGN v_industry = "1".

        IF NOT v_flg THEN ASSIGN v_industry = "2".

    END.     
END. /* IF v_industry = "B" */

/* CHECK AGAIN IF INDUSTRY STILL = "B */
IF v_industry = "B" THEN DO:

    MESSAGE 
          "RAW Material Item creation FAILED. " 
          "Please create RAW Material Items manually."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    NEXT.
END.


DISABLE TRIGGERS FOR LOAD OF e-item.
DISABLE TRIGGERS FOR LOAD OF e-item-vend.

FIND FIRST item EXCLUSIVE-LOCK
    WHERE item.company  = cocode
      AND item.industry = v_industry
      AND item.i-name   = v_dscr
      AND item.i-no     = v_i-no NO-ERROR.
IF NOT AVAIL item THEN DO:
    CREATE item.
    ASSIGN
        item.company   = prep.company          
        item.industry  = v_industry            
        item.i-name    = v_dscr 
        item.i-no      = v_i-no
        item.i-code    = "R"
        item.cons-uom  = "EA"
        item.pur-uom   = "EA"
        item.cost-type = v_cost-type
        item.procat    = v_procat
        item.stocked   = TRUE
        item.loc       = v_loc    
        item.loc-bin   = v_loc-bin.

    IF v_mat-type NE "" THEN DO:
        IF v_mat-type = 'P' 
          THEN item.mat-type = '7'.
          ELSE IF v_mat-type = 'D' 
            THEN item.mat-type = '8'.
            ELSE IF v_mat-type = 'R' 
              THEN item.mat-type = 'X'.
              ELSE IF v_mat-type = 'F' 
                THEN item.mat-type = 'Y'.
                ELSE item.mat-type = 'M'.              
    END.    

    FIND FIRST e-item EXCLUSIVE-LOCK
        WHERE e-item.company EQ cocode 
          AND e-item.i-no EQ item.i-no NO-ERROR.
    IF NOT AVAIL e-item THEN DO:

        CREATE e-item.
        ASSIGN
            e-item.company = cocode 
            e-item.i-no    = item.i-no
            e-item.std-uom = "EA".
    END.

    FIND FIRST e-item-vend EXCLUSIVE-LOCK 
        WHERE e-item-vend.company EQ cocode 
          AND e-item-vend.i-no EQ item.i-no NO-ERROR.
    IF NOT AVAIL e-item-vend THEN DO:
        CREATE e-item-vend.
        ASSIGN
            e-item-vend.company     = cocode 
            e-item-vend.i-no        = item.i-no
            e-item-vend.item-type   = YES
            e-item-vend.run-cost[1] = prep.cost
            e-item-vend.run-qty[1]  = 9999999.9.
    END.
    RELEASE e-item.
    RELEASE e-item-vend.

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
  {src/adm/template/snd-list.i "prep"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateMarkup V-table-Win 
PROCEDURE UpdateMarkup :
/*------------------------------------------------------------------------------
  Purpose:    Calculates Markup
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMkup AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPrice AS DECIMAL     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
        dCost =  DEC(prep.cost:SCREEN-VALUE)
        dPrice =  DEC(prep.spare-dec-1:SCREEN-VALUE).

    IF glProfit THEN
        dMkup = (1 - dCost / dPrice) * 100. 
    ELSE
        dMkup = (dPrice / dCost - 1) * 100.

    prep.mkup:SCREEN-VALUE = STRING(dMkup).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdatePrice V-table-Win 
PROCEDURE UpdatePrice :
/*------------------------------------------------------------------------------
  Purpose:    Calculates Price 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCost AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMkup AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPrice AS DECIMAL     NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
        dCost =  DEC(prep.cost:SCREEN-VALUE)
        dMkup =  DEC(prep.mkup:SCREEN-VALUE).
    IF glProfit THEN
        dPrice = dCost / (1 - (dMkup / 100)).
    ELSE
        dPrice = dCost * (1 + (dMkup / 100)).
    prep.spare-dec-1:SCREEN-VALUE = STRING(dPrice).
END.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum V-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-account FOR account.

  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-account
        WHERE bf-account.company EQ gcompany
          AND bf-account.actnum  EQ prep.actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-account THEN DO:         /*Task# 11051312*/
        MESSAGE TRIM(prep.actnum:LABEL) +
              " is not a valid account.  Try F1 to lookup valid accounts."
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO prep.actnum.
        RETURN ERROR.
    END.
    ELSE
        IF bf-account.TYPE NE "R" AND prep.simon:SCREEN-VALUE EQ "S" THEN DO: 
            MESSAGE TRIM(prep.actnum:LABEL) +
              " is not a valid revenue account since the prep code is billable. Try F1 to lookup valid revenue accounts or change prep code to non-billable."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO prep.actnum.
            RETURN ERROR.
        END.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code V-table-Win 
PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-prep FOR prep.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF CAN-FIND(FIRST b-prep
                WHERE b-prep.company EQ gcompany
                  AND b-prep.code    EQ prep.code:SCREEN-VALUE
                  AND ROWID(b-prep)  NE ROWID(prep)) THEN DO:
      MESSAGE TRIM(prep.code:LABEL) +
              " already exists, please delete this one or the other(s)..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO prep.code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dim V-table-Win 
PROCEDURE valid-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR lv-est-no AS CHAR NO-UNDO.
  DEF VAR ll-corr2 AS LOG NO-UNDO.

  {methods/lValidateError.i YES}
  ASSIGN
     lv-est-no = prep.last-est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
     lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no)
     ll-corr2  = AVAIL prep AND
                 TRIM(lv-est-no) NE "" AND
                 CAN-FIND(FIRST est
                          WHERE est.company  EQ prep.company
                            AND est.est-no   EQ lv-est-no
                            AND est.est-type GT 4).

  IF ll-corr2 AND
     DEC(ip-focus:SCREEN-VALUE) - TRUNC(DEC(ip-focus:SCREEN-VALUE),0) GE
     v-16-or-32 THEN DO:
    MESSAGE "Cannot have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
        VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO ip-focus.
    RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgcat V-table-Win 
PROCEDURE valid-fgcat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplUpdateAcc AS LOG NO-UNDO.

  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:

    IF prep.fgcat:SCREEN-VALUE NE "" THEN DO: 
        FIND FIRST fgcat 
        WHERE fgcat.company EQ gcompany
            AND fgcat.procat  EQ prep.fgcat:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF NOT AVAIL fgcat THEN DO:
            MESSAGE "Invalid FG Category, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO prep.fgcat.
            RETURN ERROR.
        END.
        ELSE
            IF iplUpdateAcc THEN prep.actnum:SCREEN-VALUE = fgcat.glacc.
    END.

/*     IF prep.fgcat:SCREEN-VALUE EQ "" AND                                                   */
/*        prep.i-no:SCREEN-VALUE GT ""                                                        */
/*     THEN DO:                                                                               */
/*       MESSAGE "FG Category cannot be blank for a raw material..." VIEW-AS ALERT-BOX ERROR. */
/*       APPLY "entry" TO prep.fgcat.                                                         */
/*       RETURN ERROR.                                                                        */
/*     END.                                                                                   */

  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-i-no V-table-Win 
PROCEDURE valid-rm-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

      IF prep.i-no:SCREEN-VALUE NE "" AND 
         NOT v_rmcrtflg AND
         NOT CAN-FIND(FIRST item 
                      WHERE ITEM.company EQ gcompany 
                        AND SUBSTRING(ITEM.i-no,1,10) EQ prep.i-no:SCREEN-VALUE)
        THEN DO:

        MESSAGE 
          "Invalid RM Item #."  SKIP
          "Would you like to create this RM Item # ?"
         VIEW-AS ALERT-BOX MESSAGE
         BUTTON YES-NO
         UPDATE v_rmcrtflg.      

       /*MESSAGE "RM Item # is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.

      APPLY "entry" TO prep.i-no.
      RETURN ERROR.*/

    END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rmcat V-table-Win 
PROCEDURE valid-rmcat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:

    IF prep.procat:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST procat WHERE procat.company EQ gcompany
                                  AND procat.procat  EQ prep.procat:SCREEN-VALUE)                
    THEN DO:
      MESSAGE "Invalid RM Category, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO prep.procat.
      RETURN ERROR.
    END.

    IF prep.procat:SCREEN-VALUE EQ "" AND
       prep.i-no:SCREEN-VALUE GT ""
    THEN DO:
      MESSAGE "RM Category cannot be blank for a raw material..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO prep.procat.
      RETURN ERROR.
    END.

  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

