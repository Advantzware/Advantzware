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

  File: viewers\stylec.w

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
/*{methods/defines/hndldefs.i} */
DEFINE NEW SHARED VARIABLE Persistent-Handle AS HANDLE.
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
/*{methods/prgsecur.i}*/
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
/* style-maint preprocedure is for enable/disble variables */
&global-define style-maint Corr
DEF VAR ll-deleted AS LOG NO-UNDO.  /* reopen query to enable update buttons when user delete style */
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT "6.25" NO-UNDO.
DEF VAR ld-2-array LIKE eb.k-wid-array NO-UNDO.
DEF VAR ld-3-array LIKE eb.k-len-array NO-UNDO.
DEF VAR ld-4-array LIKE eb.k-len-array NO-UNDO.
DEF VAR ld-5-array LIKE eb.k-len-array NO-UNDO.
DEF VAR ld-6-array LIKE eb.k-len-array NO-UNDO.
DEF VAR ld-7-array LIKE eb.k-len-array NO-UNDO.
DEF VAR lv-2-scr-type LIKE eb.k-wid-scr-type NO-UNDO.
DEF VAR lv-3-scr-type LIKE eb.k-len-scr-type NO-UNDO.
DEF VAR lv-4-scr-type LIKE eb.k-len-scr-type NO-UNDO.
DEF VAR lv-5-scr-type LIKE eb.k-len-scr-type NO-UNDO.
DEF VAR lv-6-scr-type LIKE eb.k-len-scr-type NO-UNDO.
DEF VAR lv-7-scr-type LIKE eb.k-len-scr-type NO-UNDO.
DEF SHARED VAR flute-val AS CHAR NO-UNDO .

DEF TEMP-TABLE tt-array FIELD tt-dec AS DEC FIELD tt-type AS CHAR.
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.
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
&Scoped-define EXTERNAL-TABLES style flute
&Scoped-define FIRST-EXTERNAL-TABLE style


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR style, flute.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS style.dscr style.type style.design-no ~
style.royalty style.dim-tk style.dim-pan5 style.dim-fit style.material[1] ~
style.material[2] style.material[3] style.material[4] style.material[5] ~
style.material[6] style.material[7] style.qty-per-set style.dim-df ~
style.m-code[1] style.m-code[2] style.m-code[3] style.m-code[4] ~
style.m-code[5] style.m-code[6] style.m-code[7] style.spare-char-5 
&Scoped-define ENABLED-TABLES style
&Scoped-define FIRST-ENABLED-TABLE style
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-FIELDS style.style style.dscr style.type ~
style.design-no style.royalty flute.code style.dim-tk style.dim-pan5 ~
style.dim-fit style.material[1] style.material[2] style.material[3] ~
style.material[4] style.material[5] style.material[6] style.material[7] ~
style.qty-per-set style.dim-df style.m-code[1] style.m-code[2] ~
style.m-code[3] style.m-code[4] style.m-code[5] style.m-code[6] ~
style.m-code[7] style.m-dscr[1] style.m-dscr[2] style.m-dscr[3] ~
style.m-dscr[4] style.m-dscr[5] style.m-dscr[6] style.m-dscr[7] ~
style.spare-char-5 
&Scoped-define DISPLAYED-TABLES style flute
&Scoped-define FIRST-DISPLAYED-TABLE style
&Scoped-define SECOND-DISPLAYED-TABLE flute
&Scoped-Define DISPLAYED-OBJECTS ld-joint-tab ld-blank-width ld-glue-in ~
ld-glue-out ld-stitch-in ld-stitch-out ld-tape-score 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS style.style 
&Scoped-define ADM-ASSIGN-FIELDS ld-joint-tab style.m-dscr[1] ~
style.m-dscr[2] style.m-dscr[3] style.m-dscr[4] style.m-dscr[5] ~
style.m-dscr[6] style.m-dscr[7] 

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
DEFINE VARIABLE ld-blank-width AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Blank Width Score" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-glue-in AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Glue Tab In" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-glue-out AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Glue Tab Out" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-joint-tab AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Joint Tab Width" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-stitch-in AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Stitch Tab In" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-stitch-out AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Stitch Tab Out" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-tape-score AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Tape Score Total" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 12.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 10.48.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 9.29.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     style.style AT ROW 1.48 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 4
     style.dscr AT ROW 1.48 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.type AT ROW 1.48 COL 69 COLON-ALIGNED HELP
          "'B'ox, 'C' lear Poly Bags, 'D'ie Cut, 'F'oam, 'P'artition, 'R'oll, or 'W'ood"
          LABEL "Type" FORMAT "!"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     style.design-no AT ROW 1.48 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     style.royalty AT ROW 1.48 COL 108 COLON-ALIGNED
          LABEL "Markup$"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     flute.code AT ROW 1.48 COL 130 COLON-ALIGNED
          LABEL "Flute"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     ld-joint-tab AT ROW 3.62 COL 25 COLON-ALIGNED
     ld-blank-width AT ROW 4.57 COL 25 COLON-ALIGNED
     ld-glue-in AT ROW 5.52 COL 25 COLON-ALIGNED
     ld-glue-out AT ROW 6.48 COL 25 COLON-ALIGNED
     ld-stitch-in AT ROW 7.43 COL 25 COLON-ALIGNED
     ld-stitch-out AT ROW 8.38 COL 25 COLON-ALIGNED
     ld-tape-score AT ROW 9.33 COL 25 COLON-ALIGNED
     style.dim-tk AT ROW 10.29 COL 25 COLON-ALIGNED FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-pan5 AT ROW 11.24 COL 25 COLON-ALIGNED FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.dim-fit AT ROW 12.24 COL 25 COLON-ALIGNED
          LABEL "Lock Tab" FORMAT ">>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     style.material[1] AT ROW 3.62 COL 62 COLON-ALIGNED
          LABEL "Board"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[2] AT ROW 4.57 COL 62 COLON-ALIGNED
          LABEL "Ink"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[3] AT ROW 5.52 COL 62 COLON-ALIGNED
          LABEL "Ink Cov %"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[4] AT ROW 6.48 COL 62 COLON-ALIGNED
          LABEL "Adder"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[5] AT ROW 7.43 COL 62 COLON-ALIGNED
          LABEL "Label"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[6] AT ROW 8.38 COL 62 COLON-ALIGNED
          LABEL "Coating"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.material[7] AT ROW 9.33 COL 62 COLON-ALIGNED
          LABEL "Joint Glue"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.qty-per-set AT ROW 10.29 COL 62 COLON-ALIGNED
          LABEL "Qty/Set" FORMAT "->>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.dim-df AT ROW 12.43 COL 62 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "# Slots" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[1] AT ROW 3.62 COL 91 COLON-ALIGNED
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.m-code[2] AT ROW 4.57 COL 91 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[3] AT ROW 5.52 COL 91 COLON-ALIGNED
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[4] AT ROW 6.48 COL 91 COLON-ALIGNED
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[5] AT ROW 7.43 COL 91 COLON-ALIGNED
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[6] AT ROW 8.38 COL 91 COLON-ALIGNED
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[7] AT ROW 9.33 COL 91 COLON-ALIGNED
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[1] AT ROW 3.62 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[2] AT ROW 4.57 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[3] AT ROW 5.52 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[4] AT ROW 6.48 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[5] AT ROW 7.43 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[6] AT ROW 8.38 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[7] AT ROW 9.33 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     style.spare-char-5 AT ROW 11.24 COL 62 COLON-ALIGNED WIDGET-ID 6
          LABEL "Packing Code"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     "DEFAULT MATERIAL CODES" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 2.91 COL 47
          FGCOLOR 9 
     "DEFAULT FLUTE DIMENSIONS" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 2.91 COL 4
          FGCOLOR 9 
     "DEFAULT MACHINE ROUTING" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 2.91 COL 93
          FGCOLOR 9 
     RECT-19 AT ROW 1 COL 1
     RECT-7 AT ROW 3.14 COL 3
     RECT-8 AT ROW 3.14 COL 45
     RECT-9 AT ROW 3.14 COL 87
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.style,ASI.flute
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
         HEIGHT             = 22.33
         WIDTH              = 150.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN flute.code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN style.dim-df IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN style.dim-fit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.dim-pan5 IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN style.dim-tk IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ld-blank-width IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-glue-in IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-glue-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-joint-tab IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ld-stitch-in IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-stitch-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-tape-score IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style.m-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-dscr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.material[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.qty-per-set IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.royalty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.spare-char-5 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.style IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN style.type IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
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
   DEF VAR char-val AS cha NO-UNDO.   
   DEF VAR ls-cur-val AS cha NO-UNDO.
   DEF VAR lv-foam AS LOG NO-UNDO.
   DEF VAR lv-icode AS cha NO-UNDO.
   DEF VAR lv-mat-type AS cha NO-UNDO.
   DEF VAR lv-rowid AS ROWID NO-UNDO.


   CASE FOCUS:NAME :
        WHEN 'design-no' THEN DO:
             RUN windows/l-boxdes.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).    
        END.
        WHEN 'code' THEN DO:
           RUN windows/l-flute.w (style.company,OUTPUT char-val).
           IF char-val <> "" THEN 
              FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
           RETURN NO-APPLY.   

        END.
        WHEN "type" THEN DO:
              RUN windows/l-stytyp.w (OUTPUT char-val).
              IF char-val <> "" THEN DO:
                FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
                IF FOCUS:SCREEN-VALUE = "W" THEN
                    style.material[1]:LABEL = "Wood" .
                ELSE IF FOCUS:SCREEN-VALUE = "F" THEN
                    style.material[1]:LABEL = "Foam" .
                ELSE IF FOCUS:SCREEN-VALUE = "C" THEN
                    style.material[1]:LABEL = "PolyBag" .
                ELSE
                     style.material[1]:LABEL = "Board" .
              END.
         END.
        WHEN 'material' THEN DO:
             CASE FOCUS:INDEX :
                  WHEN 1 THEN DO:  /* board */
                         DEF VAR lv-ind LIKE style.industry NO-UNDO.
                         ls-cur-val = FOCUS:SCREEN-VALUE.
                         IF AVAIL style THEN lv-ind = style.industry.
                         ELSE lv-ind = "".  
                         IF AVAIL style AND style.type:screen-value = "f" THEN  /* foam */
                            RUN windows/l-boardf.w (style.company,lv-ind,ls-cur-val,OUTPUT char-val).
                         ELSE RUN windows/l-board1.w (cocode,lv-ind,FOCUS:SCREEN-VALUE, OUTPUT lv-rowid).
                         FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
                         IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN
                           ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = item.i-no
                            .
                        RETURN NO-APPLY.                  
                  END.
                  WHEN 4 THEN DO:  /* adder */
                    IF AVAIL style THEN ASSIGN lv-ind = style.industry
                                               lv-foam = style.type:screen-value = "F" .
                    ELSE ASSIGN lv-ind = ""
                                lv-foam = NO.
                    RUN windows/l-boarda.w (style.company,lv-ind, FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                        ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                      .
                    RETURN NO-APPLY.

                  END.
                  /*=====????
                  when 2 or when 5 or when 6 or when 7 then do:
                     if avail style then assign lv-ind = style.industry
                                                lv-foam = style.type:screen-value = "F" .
                     else assign lv-ind = ""
                                 lv-foam = no
                                 .
                     lv-mat-type = if lv-foam then "1234" else "BPR".             
                     lv-icode = "B".    
                  /*   do while true:
                       message "Please enter item code ('R'eal, 'E'st or 'B'oth) " update lv-icode .
                       if index("REB", lv-icode) > 0 then leave. 
                     end.
                  */  
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.

                  end.
                  */
                  WHEN 2 THEN DO:
                     IF AVAIL style THEN ASSIGN lv-ind = style.industry.
                     ELSE ASSIGN lv-ind = ""                     .
                     lv-mat-type =  "I".
                     RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                      OUTPUT char-val).
                     IF char-val <> "" THEN 
                         ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                         .
                     RETURN NO-APPLY.
                  END.
                  WHEN 5 THEN DO:
                     IF AVAIL style THEN ASSIGN lv-ind = style.industry.
                     ELSE ASSIGN lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "WLF".
                     RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                      OUTPUT char-val).
                     IF char-val <> "" THEN 
                         ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                         .
                     RETURN NO-APPLY.
                  END.
                  WHEN 6 THEN DO:
                     IF AVAIL style THEN ASSIGN lv-ind = style.industry.
                     ELSE ASSIGN lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "V".
                     RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                      OUTPUT char-val).
                     IF char-val <> "" THEN 
                         ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                         .
                     RETURN NO-APPLY.
                  END.
                  WHEN 7 THEN DO:
                     IF AVAIL style THEN ASSIGN lv-ind = style.industry.
                     ELSE ASSIGN lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "GTS".
                     RUN windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, FOCUS:SCREEN-VALUE,lv-foam,lv-icode,
                                      OUTPUT char-val).
                     IF char-val <> "" THEN 
                         ASSIGN FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
                         .
                     RETURN NO-APPLY.
                  END.

             END CASE.
        END.  /* material*/
        WHEN 'm-code' THEN DO:
             RUN windows/l-mach.w (style.company, gloc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
             IF char-val <> "" THEN 
                CASE FOCUS:INDEX :
                     WHEN 1 THEN ASSIGN style.m-code[1]:screen-value IN FRAME {&frame-name} = ENTRY(1,char-val)
                                        style.m-dscr[1]:screen-value = ENTRY(2,char-val).
                     WHEN 2 THEN ASSIGN style.m-code[2]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[2]:screen-value = ENTRY(2,char-val).
                     WHEN 3 THEN ASSIGN style.m-code[3]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[3]:screen-value = ENTRY(2,char-val).
                     WHEN 4 THEN ASSIGN style.m-code[4]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[4]:screen-value = ENTRY(2,char-val).
                     WHEN 5 THEN ASSIGN style.m-code[5]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[5]:screen-value = ENTRY(2,char-val).
                     WHEN 6 THEN ASSIGN style.m-code[6]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[6]:screen-value = ENTRY(2,char-val).
                     WHEN 7 THEN ASSIGN style.m-code[7]:screen-value = ENTRY(1,char-val)
                                        style.m-dscr[7]:screen-value = ENTRY(2,char-val).

                END CASE.
        END.  /* m-code */
        WHEN "spare-char-5" THEN /*Pack Code*/
           DO:
               RUN windows/l-item.w (style.company, "2", "C", FOCUS:SCREEN-VALUE, OUTPUT char-val).
               IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
               DO:
                   FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                   APPLY "value-changed" TO style.spare-char-5.
               END.
           END.        
   END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.design-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.design-no V-table-Win
ON LEAVE OF style.design-no IN FRAME F-Main /* Design # */
DO:    
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST box-design-hdr WHERE /*box-design-hdr.company = style.company */
                     box-design-hdr.design-no = int(SELF:screen-value))
     THEN DO:
        MESSAGE "Invalid Box Design Number. Try help." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.                  
     {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-blank-width
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-blank-width V-table-Win
ON ENTRY OF ld-blank-width IN FRAME F-Main /* Blank Width Score */
DO:
  {viewers/stylec1.i 2}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-glue-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-in V-table-Win
ON ENTRY OF ld-glue-in IN FRAME F-Main /* Glue Tab In */
DO:
  {viewers/stylec1.i 3} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-glue-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-out V-table-Win
ON ENTRY OF ld-glue-out IN FRAME F-Main /* Glue Tab Out */
DO:
  {viewers/stylec1.i 4}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-stitch-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-in V-table-Win
ON ENTRY OF ld-stitch-in IN FRAME F-Main /* Stitch Tab In */
DO:
  {viewers/stylec1.i 5}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-stitch-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-out V-table-Win
ON ENTRY OF ld-stitch-out IN FRAME F-Main /* Stitch Tab Out */
DO:
  {viewers/stylec1.i 6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-tape-score
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-tape-score V-table-Win
ON ENTRY OF ld-tape-score IN FRAME F-Main /* Tape Score Total */
DO:
  {viewers/stylec1.i 7}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[1] V-table-Win
ON LEAVE OF style.m-code[1] IN FRAME F-Main /* 1 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[1]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[1]:SCREEN-VALUE = "".
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[2] V-table-Win
ON LEAVE OF style.m-code[2] IN FRAME F-Main /* 2 */
DO:
      {&methods/lValidateError.i YES}
      IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[2]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[2]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[3] V-table-Win
ON LEAVE OF style.m-code[3] IN FRAME F-Main /* 3 */
DO:
      {&methods/lValidateError.i YES}
      IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[3]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[3]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[4] V-table-Win
ON LEAVE OF style.m-code[4] IN FRAME F-Main /* 4 */
DO:
      {&methods/lValidateError.i YES}
      IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    FIND FIRST mach WHERE mach.company = style.company AND 
                          mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL mach THEN style.m-dscr[4]:SCREEN-VALUE = mach.m-dscr.
    ELSE style.m-dscr[4]:SCREEN-VALUE = "".
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[5] V-table-Win
ON LEAVE OF style.m-code[5] IN FRAME F-Main /* 5 */
DO:
      {&methods/lValidateError.i YES}
      IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[5]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[5]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[6] V-table-Win
ON LEAVE OF style.m-code[6] IN FRAME F-Main /* 6 */
DO:
      {&methods/lValidateError.i YES}
      IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[6]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[6]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[7] V-table-Win
ON LEAVE OF style.m-code[7] IN FRAME F-Main /* 7 */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.

    FIND FIRST mach WHERE mach.company = style.company AND 
                         mach.m-code= SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL mach THEN style.m-dscr[7]:SCREEN-VALUE = mach.m-dscr.
   ELSE style.m-dscr[7]:SCREEN-VALUE = "".
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[1] V-table-Win
ON LEAVE OF style.material[1] IN FRAME F-Main /* Board */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                  /*item.industry = style.industry and*/
                  item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Board Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[2] V-table-Win
ON LEAVE OF style.material[2] IN FRAME F-Main /* Ink */
DO:
    {&methods/lValidateError.i YES}
    IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
             /*item.industry = style.industry and*/
             item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Ink Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[4] V-table-Win
ON LEAVE OF style.material[4] IN FRAME F-Main /* Adder */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                 /* item.industry = style.industry and */
                  item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Adder Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[5] V-table-Win
ON LEAVE OF style.material[5] IN FRAME F-Main /* Label */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                 /* item.industry = style.industry and */
                  item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Label Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[6] V-table-Win
ON LEAVE OF style.material[6] IN FRAME F-Main /* Coating */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
       NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                 /* item.industry = style.industry and */
                  item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Coating Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[7] V-table-Win
ON LEAVE OF style.material[7] IN FRAME F-Main /* Joint Glue */
DO:
   {&methods/lValidateError.i YES}
   IF LASTKEY <> -1 AND SELF:screen-value <> "" AND
      NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                 /* item.industry = style.industry and */
                  item.i-no = SELF:screen-value )
    THEN DO:
       MESSAGE "Invalid Joint Glue Code. Try help please." VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.style V-table-Win
ON LEAVE OF style.style IN FRAME F-Main /* Style No. */
DO:
    {&methods/lValidateError.i YES}
    DEF BUFFER bf-style FOR style.
    IF LASTKEY <> -1 AND 
       CAN-FIND(FIRST bf-style WHERE bf-style.company = style.company AND
                      bf-style.style = SELF:screen-value)
    THEN DO:
         MESSAGE "Style already exists. Try different style code." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.     
   {&methods/lValidateError.i NO}             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.type V-table-Win
ON LEAVE OF style.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF SELF:screen-value = "W" THEN
          style.material[1]:LABEL = "Wood" .
      ELSE IF SELF:screen-value = "F" THEN
          style.material[1]:LABEL = "Foam" .
      ELSE IF SELF:screen-value = "C" THEN
          style.material[1]:LABEL = "PolyBag" .
      ELSE
           style.material[1]:LABEL = "Board" .

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.type V-table-Win
ON VALUE-CHANGED OF style.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF SELF:screen-value = "W" THEN
          style.material[1]:LABEL = "Wood" .
      ELSE IF SELF:screen-value = "F" THEN
          style.material[1]:LABEL = "Foam" .
      ELSE IF SELF:screen-value = "C" THEN
          style.material[1]:LABEL = "PolyBag" .
      ELSE
           style.material[1]:LABEL = "Board" .

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
SESSION:DATA-ENTRY-RETURN = YES.

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
  {src/adm/template/row-list.i "style"}
  {src/adm/template/row-list.i "flute"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "style"}
  {src/adm/template/row-find.i "flute"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copy-scores V-table-Win 
PROCEDURE copy-scores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL style AND AVAIL flute THEN
    RUN windows/d-cpscor.w (style.style, flute.code).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-flute-dim V-table-Win 
PROCEDURE display-flute-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-code AS INT NO-UNDO.

  FIND FIRST reftable WHERE reftable.reftable = "STYFLU" 
                        AND reftable.company = style.style
                        AND reftable.loc = flute.code
                        AND reftable.code = string(ip-code)
                        NO-ERROR.
  IF NOT AVAIL reftable THEN DO:

     CREATE reftable.
     ASSIGN reftable.reftable = "STYFLU" 
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = STRING(ip-code)
            . 
  END.       

  CASE ip-code:
      WHEN 1 THEN DO:
             ld-joint-tab:screen-value IN FRAME {&frame-name} = string(reftable.val[13]).
      END.
      WHEN 2 THEN ld-blank-width:screen-value IN FRAME {&frame-name} = string(reftable.val[13]).
      WHEN 3 THEN ld-glue-in:screen-value = STRING(reftable.val[13]).
      WHEN 4 THEN ld-glue-out:screen-value = STRING(reftable.val[13]).
      WHEN 5 THEN ld-stitch-in:screen-value = STRING(reftable.val[13]).
      WHEN 6 THEN ld-stitch-out:screen-value = STRING(reftable.val[13]).                 
      WHEN 7 THEN ld-tape-score:screen-value = STRING(reftable.val[13]).
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-style-field V-table-Win 
PROCEDURE enable-style-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* called from methods/viewers/enable/style.i */

  ENABLE ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dim-values V-table-Win 
PROCEDURE get-dim-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-style LIKE style.style NO-UNDO.
  DEF INPUT PARAM ip-flute AS cha NO-UNDO.
  DEF INPUT PARAM ip-code AS cha NO-UNDO.
  DEF OUTPUT PARAM op-total AS DEC NO-UNDO.
  DEF VAR i AS INT NO-UNDO.


  FIND FIRST reftable WHERE reftable.reftable = "STYFLU" 
                        AND reftable.company = ip-style
                        AND reftable.loc = ip-flute
                        AND reftable.code = ip-code
                        NO-ERROR.
  IF  AVAIL reftable THEN op-total = reftable.val[13].

  /*  same val[13] is sum of [1 for 12]
  do i = 1 to 12:
          op-total = op-total + reftable.val[i] .
  end.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-flute FOR flute.
  DEF BUFFER bf-reftable FOR reftable.
  DEF BUFFER bf-routing-mtx FOR routing-mtx.
  DEF BUFFER old-style FOR style .
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-old-style AS cha NO-UNDO.
  DEF VAR j AS INT NO-UNDO.
  DEF VAR ld-joint-val AS DECIMAL NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   ls-old-style = style.style
   ld-joint-val = DEC(ld-joint-tab:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* ======== create flute for the style ==========*/
  IF adm-new-record THEN DO:
     IF adm-adding-record THEN
     FOR EACH bf-flute NO-LOCK:  /* used be reftable */
        DO j = 1 TO 7:
           IF NOT CAN-FIND(FIRST reftable WHERE reftable.reftable = "STYFLU"
                               AND reftable.company = style.style
                               AND reftable.loc = bf-flute.code
                               AND reftable.code = string(j) )
           THEN DO:
                CREATE reftable.
                ASSIGN reftable.reftable = "STYFLU"
                       reftable.company = style.style
                       reftable.loc = bf-flute.code
                       reftable.code = STRING(j)
                       .
                CREATE reftable.
                ASSIGN reftable.reftable = "STYFLU"
                       reftable.company = style.style
                       reftable.loc = bf-flute.code
                       reftable.code2 = "1"
                       reftable.code = STRING(j)
                       .

           END. 
        END.  /* do j */
     END.     /* for each bf-flute */

     ELSE DO: /* === copy style, scoring, & routing-mtx info */
         FOR EACH reftable
             WHERE reftable.reftable EQ "STYFLU"
               AND reftable.company  EQ ls-old-style
             NO-LOCK:

           FIND FIRST bf-reftable
               WHERE bf-reftable.reftable EQ "STYFLU"
                 AND bf-reftable.company  EQ style.style
                 AND bf-reftable.loc      EQ reftable.loc
                 AND bf-reftable.code     EQ reftable.code
                 AND bf-reftable.code2    EQ reftable.code2
               NO-LOCK NO-ERROR.

           IF NOT AVAIL bf-reftable THEN DO:
             CREATE bf-reftable.
             BUFFER-COPY reftable TO bf-reftable
             ASSIGN
              bf-reftable.company  = style.style.
           END.
         END.

              FIND old-style WHERE old-style.company = gcompany 
                               AND old-style.style = ls-old-style
                               NO-LOCK .
              ASSIGN style.formula[1] = old-style.formula[1]
                     style.formula[2] = old-style.formula[2]
                     style.formula[3] = old-style.formula[3]
                     style.formula[4] = old-style.formula[4]
                     style.formula[5] = old-style.formula[5]
                     style.formula[6] = old-style.formula[6]
                     style.formula[7] = old-style.formula[7]
                     style.formula[8] = old-style.formula[8]
                     style.formula[12] = old-style.formula[12]
                     style.kdf-width  = old-style.kdf-width
                     style.kdf-length = old-style.kdf-length
                     style.balecount = old-style.balecount 
                     style.use-l[2] = old-style.use-l[2]
                     style.use-l[3] = old-style.use-l[3]
                     style.use-l[4] = old-style.use-l[4]
                     style.use-l[5] = old-style.use-l[5]
                     style.use-l[6] = old-style.use-l[6]
                     style.use-l[7] = old-style.use-l[7]
                     style.use-l[8] = old-style.use-l[8]
                     style.use-l[9] = old-style.use-l[9]
                     style.use-l[10] = old-style.use-l[10]
                     style.use-l[11] = old-style.use-l[11]
                     style.use-l[12] = old-style.use-l[12]
                     style.use-l[13] = old-style.use-l[13]
                     style.use-w[2] = old-style.use-w[2]
                     style.use-w[3] = old-style.use-w[3]
                     style.use-w[4] = old-style.use-w[4]
                     style.use-w[5] = old-style.use-w[5]
                     style.use-w[6] = old-style.use-w[6]
                     style.use-w[7] = old-style.use-w[7]
                     style.use-w[8] = old-style.use-w[8]
                     style.use-w[9] = old-style.use-w[9]
                     style.use-w[10] = old-style.use-w[10]
                     style.use-w[11] = old-style.use-w[11]
                     style.use-w[12] = old-style.use-w[12]
                     style.use-w[13] = old-style.use-w[13]
                     .            
                 FOR EACH routing-mtx OF old-style NO-LOCK:
                     CREATE bf-routing-mtx.
                     BUFFER-COPY routing-mtx EXCEPT routing-mtx.style TO bf-routing-mtx.
                     bf-routing-mtx.style = style.style.
                 END.
     END.  /* copy */
  END.  /* adm-new */

  FIND FIRST reftable WHERE reftable.reftable = "STYFLU" 
                         AND reftable.company = style.style
                         AND reftable.loc = flute.code
                         AND reftable.code = "1"  /* joint tab */
                         NO-ERROR
                         .
  IF NOT AVAIL reftable THEN DO:
     CREATE reftable.
     ASSIGN reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "1".  
  END.
  reftable.val[13] = ld-joint-val.

  {viewers/stylec2.i 2}
  {viewers/stylec2.i 3}
  {viewers/stylec2.i 4}
  {viewers/stylec2.i 5}
  {viewers/stylec2.i 6}
  {viewers/stylec2.i 7}

  {sys/inc/k16bb.i style.dim-gl  } 
  {sys/inc/k16bb.i style.dim-tk  } 
  {sys/inc/k16bb.i style.dim-dkl  } 
  {sys/inc/k16bb.i style.dim-dkw  } 
  {sys/inc/k16bb.i style.dim-pan5  } 
  {sys/inc/k16bb.i style.dim-fit  }                                              

  FIND FIRST reftable WHERE reftable.reftable = "STYFLU" 
                        AND reftable.company = style.style
                        AND reftable.loc = flute.code
                        AND reftable.code = "BOARD"
                        NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
     CREATE reftable.
     ASSIGN reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "BOARD".  
  END. 
  ASSIGN
   reftable.dscr     = style.material[1]:screen-value
   style.material[1] = "".

  /* ============= end ==============*/
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"scores-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN enable-disable IN WIDGET-HANDLE(char-hdl) ("ENABLE").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN style.company = gcompany
         style.industry = "2"
         style.qty-per-set = 1. /* corrugated style */

  IF adm-adding-record THEN DO:

     ASSIGN ld-joint-tab = 0
            ld-blank-width = 0
            ld-glue-in = 0
            ld-glue-out = 0 
            ld-stitch-in = 0
            ld-stitch-out = 0
          ld-tape-score = 0.

     DISPLAY ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-flute FOR flute.
  DEF VAR j AS INT NO-UNDO.
  DEF VAR lv-rowid AS ROWID EXTENT 2 NO-UNDO.

   {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */

  FIND FIRST eb WHERE eb.company = gcompany AND
                      eb.loc = gloc AND
                      eb.style = style.style
                      NO-LOCK NO-ERROR.
  IF AVAIL eb THEN DO:
     MESSAGE "You must remove all references to this 'Style' - " style.style " in the " SKIP
             "database files (edtimates, quotes,etc.) before you delete it." SKIP
             "Delete this style anyway?"
             VIEW-AS ALERT-BOX ERROR BUTTON YES-NO UPDATE ll-ans AS LOG.
     IF NOT ll-ans THEN RETURN NO-APPLY.        
  END.

  IF NOT adm-new-record THEN DO:
    ll-ans = NO.
    MESSAGE "Are you sure you want to delete style " style.style "?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans .
    IF NOT ll-ans THEN RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  SESSION:SET-WAIT-STATE ("general").

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl), "record-source", OUTPUT char-hdl).

  lv-rowid[1] = ROWID(style).

  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("get-next").
  IF NOT AVAIL style THEN
  RUN dispatch IN WIDGET-HANDLE(char-hdl) ("get-prev").
  IF AVAIL style THEN
  lv-rowid[2] = IF AVAIL style THEN ROWID(style) ELSE lv-rowid[1].

  RUN reset-browse (lv-rowid[1]).

  FOR EACH bf-flute NO-LOCK:  /* used be reftable */
      DO j = 1 TO 7:
         FIND FIRST reftable WHERE reftable.reftable = "STYFLU"
                               AND reftable.company = style.style
                               AND reftable.loc = bf-flute.code
                               AND reftable.code = string(j) 
                               NO-ERROR.
         IF AVAIL reftable THEN DELETE reftable.
      END.
  END.   

  FOR EACH routing-mtx OF style :
      DELETE routing-mtx.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reset-browse (lv-rowid[2]).

  RUN set-attribute-list IN adm-broker-hdl ("IS-DELETED=yes").  /* to force update button to be enabled */

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  DEF BUFFER bf-reftable FOR reftable.
/*MESSAGE "start " + STRING(flute.code) VIEW-AS ALERT-BOX ERROR.*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
 RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
 IF AVAIL flute THEN
 ASSIGN flute-val = flute.CODE .

  /* Code placed here will execute AFTER standard behavior.    */

   IF NOT AVAIL style THEN RETURN.

   DO WITH FRAME {&FRAME-NAME}:
      IF v-cecscrn-char EQ "Decimal" THEN
         ASSIGN
            ld-joint-tab:FORMAT = "->>,>>9.999999"
            ld-blank-width:FORMAT = "->>,>>9.999999"
            ld-glue-in:FORMAT = "->>,>>9.999999"
            ld-glue-out:FORMAT = "->>,>>9.999999"
            ld-stitch-in:FORMAT = "->>,>>9.999999"
            ld-stitch-out:FORMAT = "->>,>>9.999999"
            ld-tape-score:FORMAT = "->>,>>9.999999"
            style.dim-tk:FORMAT = "->>,>>9.999999"
            style.dim-pan5:FORMAT = "->>,>>9.999999"
            style.dim-fit:FORMAT = "->>,>>9.999999".
      IF style.TYPE = "W" THEN
          style.material[1]:LABEL = "Wood" .
      ELSE IF style.TYPE = "F" THEN
          style.material[1]:LABEL = "Foam" .
      ELSE IF style.TYPE = "C" THEN
          style.material[1]:LABEL = "PolyBag" .
      ELSE
           style.material[1]:LABEL = "Board" .


   END.


   {viewers/stylec3.i 2}
   {viewers/stylec3.i 3}
   {viewers/stylec3.i 4}
   {viewers/stylec3.i 5}
   {viewers/stylec3.i 6}
   {viewers/stylec3.i 7}

   DEF VAR ld-total AS DEC DECIMALS 6 NO-UNDO.
   RUN get-dim-values (style.style,flute.code, "1", OUTPUT ld-total).                                 
   ld-joint-tab = ld-total.
   RUN get-dim-values (    style.style,flute.code, "2", OUTPUT ld-total).                                 
   ld-blank-width = (ld-total).
   RUN get-dim-values (    style.style,flute.code, "3", OUTPUT ld-total).                                 
   ld-glue-in = (ld-total).
   RUN get-dim-values (    style.style,flute.code, "4", OUTPUT ld-total).                                 
   ld-glue-out = (ld-total).
   RUN get-dim-values (    style.style,flute.code, "5", OUTPUT ld-total).                                 
   ld-stitch-in = (ld-total).
   RUN get-dim-values (    style.style,flute.code, "6", OUTPUT ld-total).                                 
   ld-stitch-out = (ld-total).
   RUN get-dim-values (    style.style,flute.code, "7", OUTPUT ld-total).                                 
   ld-tape-score = (ld-total).

  DISPLAY ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

  DISABLE ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

  FIND FIRST reftable WHERE reftable.reftable = "STYFLU" AND reftable.company = style.style
                        AND reftable.loc = flute.code
                        AND reftable.code = "BOARD"
                        NO-LOCK NO-ERROR.
  style.material[1]:screen-value = IF AVAIL reftable THEN reftable.dscr ELSE style.material[1].        

  ASSIGN
   style.dim-tk:screen-value = STRING( {sys/inc/k16.i style.dim-tk } )
   /*style.dim-dkl:screen-value = string( {sys/inc/k16.i style.dim-dkl } ) 
   style.dim-dkw:screen-value = string( {sys/inc/k16.i style.dim-dkw } )*/
   style.dim-pan5:screen-value = STRING( {sys/inc/k16.i style.dim-pan5 } ) 
   style.dim-fit:screen-value = string( {sys/inc/k16.i style.dim-fit } ).
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN enable-style-field.
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"scores-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN enable-disable IN WIDGET-HANDLE(char-hdl) ("DISABLE").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record V-table-Win 
PROCEDURE local-reset-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   ENABLE ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.
  DEF VAR ls-m-value AS cha NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* validation check */
  RUN valid-type NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   {&methods/lValidateError.i YES}
  DO WITH FRAME {&frame-name}:
     IF NOT CAN-FIND(FIRST box-design-hdr WHERE /*box-design-hdr.company = style.company 
                       and*/  box-design-hdr.design-no = int(style.design-no:screen-value))
     THEN DO:
        MESSAGE "Invalid Box Design Number. Try help." VIEW-AS ALERT-BOX.
        APPLY "entry" TO style.design-no .
        RETURN NO-APPLY.
     END.                  
     /* style.material validation */
     ls-m-value = "".   
     DO i = 1 TO 7:
        CASE i:
           WHEN 1 THEN ls-m-value = style.material[1]:screen-value.
           WHEN 2 THEN ls-m-value = style.material[2]:screen-value.
           WHEN 3 THEN NEXT.
           WHEN 4 THEN ls-m-value = style.material[4]:screen-value.
           WHEN 5 THEN ls-m-value = style.material[5]:screen-value.
           WHEN 6 THEN ls-m-value = style.material[6]:screen-value.
           WHEN 7 THEN ls-m-value = style.material[7]:screen-value.           
        END CASE.

        IF NOT CAN-FIND(FIRST item WHERE item.company = style.company AND 
                 /* item.industry = style.industry and */
                  item.i-no = ls-m-value )
           AND ls-m-value <> ""       
        THEN DO:
          MESSAGE "Invalid Material Code. Try help please." ls-m-value VIEW-AS ALERT-BOX.
          CASE i:
             WHEN 1 THEN APPLY "entry" TO style.material[1] IN FRAME {&frame-name}.
             WHEN 2 THEN APPLY "entry" TO style.material[2] IN FRAME {&frame-name}.
             WHEN 4 THEN APPLY "entry" TO style.material[4] IN FRAME {&frame-name}.
             WHEN 5 THEN APPLY "entry" TO style.material[5] IN FRAME {&frame-name}.
             WHEN 6 THEN APPLY "entry" TO style.material[6] IN FRAME {&frame-name}.
             WHEN 7 THEN APPLY "entry" TO style.material[7] IN FRAME {&frame-name}.
          END.
          RETURN NO-APPLY.
        END.
    END.
    /* style.m-code validation */
    ls-m-value = "".
    DO i = 1 TO 7:
       CASE i:
          WHEN 1 THEN ls-m-value = style.m-code[1]:screen-value.
          WHEN 2 THEN ls-m-value = style.m-code[2]:screen-value.
          WHEN 3 THEN ls-m-value = style.m-code[3]:screen-value.
          WHEN 4 THEN ls-m-value = style.m-code[4]:screen-value.
          WHEN 5 THEN ls-m-value = style.m-code[5]:screen-value.
          WHEN 6 THEN ls-m-value = style.m-code[6]:screen-value.
          WHEN 7 THEN ls-m-value = style.m-code[7]:screen-value.          
       END CASE.
       IF ls-m-value <> "" AND
       NOT CAN-FIND(FIRST mach WHERE mach.company = style.company AND 
                  mach.m-code= ls-m-value )
       THEN DO:
           MESSAGE "Invalid Machine Code. Try help please." VIEW-AS ALERT-BOX.
           CASE i :
              WHEN 1 THEN APPLY "entry" TO style.m-code[1].
              WHEN 2 THEN APPLY "entry" TO style.m-code[2]. 
              WHEN 3 THEN APPLY "entry" TO style.m-code[3]. 
              WHEN 4 THEN APPLY "entry" TO style.m-code[4].
              WHEN 5 THEN APPLY "entry" TO style.m-code[5].              
              WHEN 6 THEN APPLY "entry" TO style.m-code[6].
              WHEN 7 THEN APPLY "entry" TO style.m-code[7].
           END CASE.
           RETURN NO-APPLY.
       END.
    END.
  END.   /* do with frame */
   {&methods/lValidateError.i NO}
  /* end of validation */
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN RUN reset-browse (ROWID(style)).

  DISABLE ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score WITH FRAME {&frame-name}.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
  DO i = 1 TO NUM-ENTRIES(char-hdl):
     RUN reopen-query IN WIDGET-HANDLE(ENTRY(i,char-hdl)).
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"scores-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN enable-disable IN WIDGET-HANDLE(char-hdl) ("ENABLE") .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-browse V-table-Win 
PROCEDURE reset-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl), "record-source", OUTPUT char-hdl).

  RUN refresh-query IN WIDGET-HANDLE(char-hdl) (ip-rowid).

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
  {src/adm/template/snd-list.i "style"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-DO("B,C,D,F,P,R,W",style.type:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid Type. Type must be 'B'ox, 'D'ie Cut, 'F'oam, 'P'artition, 'R'oll Partition, or 'W'ood..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

