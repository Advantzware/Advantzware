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

  File: oe\v-retlin.w

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

DEF BUFFER bf-retl FOR oe-retl.
DEF VAR rOeReth AS ROWID NO-UNDO.
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.


&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

DEF VAR ll-item-valid AS LOG NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES oe-retl oe-reth
&Scoped-define FIRST-EXTERNAL-TABLE oe-retl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-retl, oe-reth.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-retl.i-no oe-retl.tag oe-retl.loc ~
oe-retl.loc-bin oe-retl.tot-qty-return oe-retl.qty-return-inv 
&Scoped-define ENABLED-TABLES oe-retl
&Scoped-define FIRST-ENABLED-TABLE oe-retl
&Scoped-Define ENABLED-OBJECTS RECT-14 RECT-2 RECT-3 RECT-4 RECT-6 
&Scoped-Define DISPLAYED-FIELDS oe-retl.i-no oe-retl.ord-no oe-retl.po-no ~
oe-retl.job-no oe-retl.job-no2 oe-retl.est-no oe-retl.tag oe-retl.part-no ~
oe-retl.i-name oe-retl.loc oe-retl.loc-bin oe-retl.i-dscr ~
oe-retl.tot-qty-return oe-retl.qty-return-inv oe-retl.unit-pr oe-retl.uom ~
oe-retl.cost 
&Scoped-define DISPLAYED-TABLES oe-retl
&Scoped-define FIRST-DISPLAYED-TABLE oe-retl
&Scoped-Define DISPLAYED-OBJECTS fi_cost-uom lv_sman1 lv_s-comm1 lv_sman2 ~
lv_s-comm2 lv_cas-cnt lv_sman3 lv_s-comm3 lv_disc lv_tax 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS oe-retl.ord-no oe-retl.po-no ~
oe-retl.job-no oe-retl.job-no2 oe-retl.est-no oe-retl.part-no ~
oe-retl.i-name oe-retl.i-dscr oe-retl.unit-pr oe-retl.uom oe-retl.cost 

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
DEFINE VARIABLE fi_cost-uom AS CHARACTER FORMAT "X(4)":U 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE lv_cas-cnt AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Case" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE lv_disc AS DECIMAL FORMAT "(>>>,>>9.99)" INITIAL 0 
     LABEL "Discount" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE lv_s-comm1 AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1.

DEFINE VARIABLE lv_s-comm2 AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1.

DEFINE VARIABLE lv_s-comm3 AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1.

DEFINE VARIABLE lv_sman1 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.

DEFINE VARIABLE lv_sman2 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.

DEFINE VARIABLE lv_sman3 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 3.33.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 4.76.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 4.76.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 4.76.

DEFINE VARIABLE lv_tax AS LOGICAL INITIAL no 
     LABEL "Tax" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_cost-uom AT ROW 11.24 COL 48 COLON-ALIGNED
     oe-retl.i-no AT ROW 3.14 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-retl.ord-no AT ROW 1.48 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-retl.po-no AT ROW 1.48 COL 36 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     oe-retl.job-no AT ROW 1.48 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     oe-retl.job-no2 AT ROW 1.48 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     oe-retl.est-no AT ROW 1.48 COL 103 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-retl.tag AT ROW 3.62 COL 78 COLON-ALIGNED
          LABEL "Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     oe-retl.part-no AT ROW 4.1 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-retl.i-name AT ROW 5.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-retl.loc AT ROW 5.05 COL 78 COLON-ALIGNED
          LABEL "Whse"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     oe-retl.loc-bin AT ROW 5.05 COL 97 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     oe-retl.i-dscr AT ROW 6 COL 17 COLON-ALIGNED
          LABEL "Item Desc"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-retl.tot-qty-return AT ROW 7.67 COL 35 COLON-ALIGNED
          LABEL "Qty to Credit Invoice"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     oe-retl.qty-return-inv AT ROW 8.62 COL 35 COLON-ALIGNED
          LABEL "Qty Returned to Inventory"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     lv_sman1 AT ROW 10.05 COL 77 COLON-ALIGNED NO-LABEL
     lv_s-comm1 AT ROW 10.05 COL 92 COLON-ALIGNED NO-LABEL
     oe-retl.unit-pr AT ROW 10.29 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-retl.uom AT ROW 10.29 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     lv_sman2 AT ROW 11 COL 77 COLON-ALIGNED NO-LABEL
     lv_s-comm2 AT ROW 11 COL 92 COLON-ALIGNED NO-LABEL
     oe-retl.cost AT ROW 11.24 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     lv_cas-cnt AT ROW 11.24 COL 63.4 COLON-ALIGNED
     lv_sman3 AT ROW 11.95 COL 77 COLON-ALIGNED NO-LABEL
     lv_s-comm3 AT ROW 11.95 COL 92 COLON-ALIGNED NO-LABEL
     lv_disc AT ROW 12.19 COL 17 COLON-ALIGNED
     lv_tax AT ROW 12.19 COL 50
     "SalesRep" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 8.86 COL 77
          FGCOLOR 9 
     "Commission" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 8.86 COL 92
          FGCOLOR 9 
     RECT-14 AT ROW 1 COL 1
     RECT-2 AT ROW 10.05 COL 3
     RECT-3 AT ROW 2.67 COL 69
     RECT-4 AT ROW 8.62 COL 75
     RECT-6 AT ROW 2.67 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-retl,ASI.oe-reth
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
         HEIGHT             = 16.43
         WIDTH              = 132.8.
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

/* SETTINGS FOR FILL-IN oe-retl.cost IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.est-no IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN fi_cost-uom IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-retl.i-dscr IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-retl.i-name IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.job-no IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.job-no2 IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-retl.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lv_cas-cnt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_disc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_s-comm1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_s-comm2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_s-comm3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_sman1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_sman2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_sman3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX lv_tax IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-retl.ord-no IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.part-no IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.po-no IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.qty-return-inv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-retl.tag IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-retl.tot-qty-return IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-retl.unit-pr IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-retl.uom IN FRAME F-Main
   NO-ENABLE 2                                                          */
ASSIGN 
       oe-retl.uom:DROP-TARGET IN FRAME F-Main      = TRUE.

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
   DEF VAR rec-val AS cha NO-UNDO.
   DEF VAR lv-handle AS HANDLE NO-UNDO.
   DEF VAR lw-focus AS HANDLE NO-UNDO.
   DEF VAR char-hdl-window AS CHAR NO-UNDO.
   DEF VAR cnt AS INT NO-UNDO.
   DEF VAR lrCurRow AS ROWID NO-UNDO.
   DEF VAR cSaveFirst AS CHAR NO-UNDO.
   DEF VAR v-line AS INT NO-UNDO.
   DEF BUFFER bf-oe-boll FOR oe-boll.
   DEF BUFFER bf-oe-retl FOR oe-retl.
   DEF VAR char-hdl AS CHAR NO-UNDO.

  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.

    lw-focus = FOCUS.

    CASE lw-focus:NAME:
        WHEN "i-no" THEN DO:
            RUN windows/l-arinvl.w (g_company,oe-reth.cust-no,oe-reth.inv-no,lw-focus:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
            IF char-val NE "" AND ENTRY(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
               ASSIGN
                lw-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                oe-retl.ord-no:SCREEN-VALUE = ENTRY(2,char-val).
               RUN new-inv.
            END.
        END.
        WHEN "ord-no" THEN DO:
            RUN windows/l-arinvl.w (g_company,oe-reth.cust-no,oe-reth.inv-no,oe-retl.i-no:SCREEN-VALUE,OUTPUT char-val, OUTPUT rec-val).
            IF char-val NE "" AND INT(ENTRY(2,char-val)) NE INT(lw-focus) THEN DO:
               ASSIGN
                oe-retl.i-no:SCREEN-VALUE = ENTRY(1,char-val)
                lw-focus:SCREEN-VALUE     = ENTRY(2,char-val).
               RUN new-inv.
            END.
        END.
        WHEN "tag" THEN DO:

            /* Change from single to multiple to select multiple tags */
          IF adm-new-record THEN
            RUN windows/l-fgtg3.w (g_company,oe-retl.i-no:screen-value,
                                   lw-focus:SCREEN-VALUE,string(oe-reth.inv-no),
                                   "MULTIPLE",
                                   OUTPUT char-val).
          ELSE
            RUN windows/l-fgtg3.w (g_company,oe-retl.i-no:screen-value,
                                   lw-focus:SCREEN-VALUE,string(oe-reth.inv-no),
                                   "SINGLE",
                                   OUTPUT char-val).


            IF char-val <> "" THEN do:

               IF NUM-ENTRIES(char-val) GT 1 THEN DO:

                 run get-link-handle in adm-broker-hdl (this-procedure,"detail-browse-source",output char-hdl).             
                 char-hdl-window = char-hdl.
                 IF VALID-HANDLE(widget-handle(char-hdl)) THEN
                 RUN tags-from-list IN widget-handle(char-hdl) (INPUT char-val).

                 /* Select Cancel in Panel */
                 run get-link-handle in adm-broker-hdl (this-procedure,"tableio-source",output char-hdl).
                 IF VALID-HANDLE(widget-handle(char-hdl)) THEN
                   RUN auto-cancel IN widget-handle(char-hdl).
                 IF VALID-HANDLE(widget-handle(char-hdl-window)) THEN
                   RUN local-exit IN widget-handle(char-hdl-window).

               END.

               /* Process the first one selected */                 
               lrCurRow = TO-ROWID(char-val).
               FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ lrCurRow
                  NO-LOCK NO-ERROR.
               IF AVAIL bf-oe-boll THEN
                 ASSIGN lw-focus:SCREEN-VALUE = bf-oe-boll.tag
                        oe-retl.loc:SCREEN-VALUE = bf-oe-boll.loc
                        oe-retl.loc-bin:SCREEN-VALUE = bf-oe-boll.loc-bin
                        oe-retl.i-no:SCREEN-VALUE = bf-oe-boll.i-no.

               run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
               IF VALID-HANDLE(widget-handle(char-hdl)) THEN DO:
                 RUN refresh-detail-browse IN widget-handle(char-hdl).
                 RUN window-to-front IN widget-handle(char-hdl) NO-ERROR.
               END.

               APPLY 'value-changed' TO asi.oe-retl.tag.

            END. /* if char-val <> "" */
        END.
        WHEN "loc" THEN DO:
            RUN windows/l-loc.w (g_company,lw-focus:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN do:
               ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                     .
            END.
        END.
        WHEN "loc-bin" THEN DO:
            RUN windows/l-fgbin.w (g_company,oe-retl.loc:screen-value,lw-focus:SCREEN-VALUE,OUTPUT char-val).
            IF char-val <> "" THEN do:
               ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                      .
            END.
        END.
        otherwise do:
             lv-handle = lw-focus:handle.
             run applhelp.p.

             if g_lookup-var <> "" then do:
                lv-handle:screen-value = g_lookup-var.

             end.   /* g_lookup-var <> "" */
             apply "entry" to lv-handle.
             return no-apply.

       end.  /* otherwise */
    END CASE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.i-no V-table-Win
ON LEAVE OF oe-retl.i-no IN FRAME F-Main /* Item # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.i-no V-table-Win
ON VALUE-CHANGED OF oe-retl.i-no IN FRAME F-Main /* Item # */
DO:
  RUN new-inv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.loc V-table-Win
ON LEAVE OF oe-retl.loc IN FRAME F-Main /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.loc-bin V-table-Win
ON LEAVE OF oe-retl.loc-bin IN FRAME F-Main /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.ord-no V-table-Win
ON LEAVE OF oe-retl.ord-no IN FRAME F-Main /* Order # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.ord-no V-table-Win
ON VALUE-CHANGED OF oe-retl.ord-no IN FRAME F-Main /* Order # */
DO:
  RUN new-inv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.po-no V-table-Win
ON LEAVE OF oe-retl.po-no IN FRAME F-Main /* PO # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-po-no (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.po-no V-table-Win
ON VALUE-CHANGED OF oe-retl.po-no IN FRAME F-Main /* PO # */
DO:
  RUN new-inv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.qty-return-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.qty-return-inv V-table-Win
ON LEAVE OF oe-retl.qty-return-inv IN FRAME F-Main /* Qty Returned to Inventory */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-qty-return-inv (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.tag V-table-Win
ON LEAVE OF oe-retl.tag IN FRAME F-Main /* Tag */
DO:

    IF LASTKEY NE -1 THEN DO:
    RUN valid-tag (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.tag V-table-Win
ON VALUE-CHANGED OF oe-retl.tag IN FRAME F-Main /* Tag */
DO:
    DO WITH FRAME {&FRAME-NAME}:  
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ oe-reth.company
            AND ar-inv.cust-no EQ oe-reth.cust-no
            AND ar-inv.inv-no  EQ oe-reth.inv-no
          USE-INDEX ar-inv NO-ERROR.
      RELEASE ar-invl.
      IF AVAIL ar-inv THEN
      FIND FIRST ar-invl NO-LOCK
          WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
            AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
            AND ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE
          USE-INDEX x-no NO-ERROR.
      IF AVAIL ar-invl THEN
        FIND FIRST oe-boll 
          WHERE oe-boll.company EQ ar-invl.company 
            AND oe-boll.b-no EQ ar-invl.b-no 
            AND oe-boll.tag  EQ oe-retl.tag:SCREEN-VALUE
          NO-LOCK NO-ERROR.

      IF AVAIL(oe-boll) THEN     
        ASSIGN 
           oe-retl.qty-return-inv:SCREEN-VALUE = STRING(oe-boll.qty)
           oe-retl.tot-qty-return:SCREEN-VALUE = STRING(oe-boll.qty).         
      ELSE
        IF AVAIL ar-invl THEN
          ASSIGN 
             oe-retl.qty-return-inv:SCREEN-VALUE = STRING(ar-invl.ship-qty)
             oe-retl.tot-qty-return:SCREEN-VALUE = STRING(ar-invl.ship-qty)
             .
      IF AVAIL ar-invl THEN
        ASSIGN
          oe-retl.uom:SCREEN-VALUE            = ar-invl.pr-uom
          oe-retl.unit-pr:SCREEN-VALUE        = STRING(ar-invl.unit-pr).

  END. /* do with frame */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-retl.tot-qty-return
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-retl.tot-qty-return V-table-Win
ON LEAVE OF oe-retl.tot-qty-return IN FRAME F-Main /* Qty to Credit Invoice */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tot-qty-return (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SUBSCRIBE TO "oeretl-add-record" ANYWHERE.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.

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
  {src/adm/template/row-list.i "oe-retl"}
  {src/adm/template/row-list.i "oe-reth"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-retl"}
  {src/adm/template/row-find.i "oe-reth"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE oe-retl.ord-no oe-retl.po-no.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR CHAR-hdl AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run oe/oe-retup.p (recid(oe-reth)).
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).

  IF VALID-HANDLE(widget-handle(char-hdl)) THEN
  RUN refresh-detail-browse IN widget-handle(char-hdl).

    /* RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. */
  IF adm-new-record THEN DO:
    /* Only do this in add mode */
    run get-link-handle in adm-broker-hdl (this-procedure,"line-browse-target",output char-hdl).
    IF VALID-HANDLE(widget-handle(char-hdl)) THEN
    RUN reopen-query IN widget-handle(char-hdl) (INPUT RECID(oe-reth)).
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
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF VAR lv-hd-recid AS RECID NO-UNDO.
  DEF BUFFER bf-reth FOR oe-reth.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.
  lv-hd-recid = RECID(oe-reth).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/* when browser is in ascending order 
  IF ll-new-record THEN DO:  /* reposition to right header 
                    when first line item deleted or add canceled */

     FIND bf-reth WHERE RECID(bf-reth) = lv-hd-recid NO-LOCK NO-ERROR.
     IF AVAIL bf-reth THEN
        FIND FIRST bf-retl WHERE bf-retl.company = bf-reth.company AND
                                 bf-retl.r-no = bf-reth.r-no NO-LOCK NO-ERROR.
     IF NOT AVAIL bf-retl THEN DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"line-browse-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-hd-recid).
     END.
  END.
*/

  RUN disable-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR v-line AS INT NO-UNDO.
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find last bf-retl where bf-retl.company = cocode and
                              bf-retl.r-no    = oe-reth.r-no
                                use-index r-no no-lock no-error.
   if available bf-retl THEN assign v-line = bf-retl.line + 1.
   ELSE assign v-line = 1.
   FIND CURRENT oe-reth EXCLUSIVE-LOCK.
   assign oe-retl.company = oe-reth.company
              oe-retl.ra-no   = oe-reth.ra-no
              oe-retl.r-no    = oe-reth.r-no
              oe-retl.line    = v-line
              /*oel_id          = recid(bf-retl)
              nufile          = yes */
              oe-reth.applied = yes.
   FIND CURRENT oe-reth no-LOCK. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-hd-recid AS RECID NO-UNDO.
  DEF BUFFER bf-reth FOR oe-reth.
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  /* Code placed here will execute PRIOR to standard behavior. */
  lv-hd-recid = RECID(oe-reth).
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 /* when browser is ascending order
  FIND bf-reth WHERE RECID(bf-reth) = lv-hd-recid NO-LOCK NO-ERROR.
  IF AVAIL bf-reth THEN
        FIND FIRST bf-retl WHERE bf-retl.company = bf-reth.company AND
                                 bf-retl.r-no = bf-reth.r-no NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-retl THEN DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"line-browse-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN reopen-query IN WIDGET-HANDLE(char-hdl) (lv-hd-recid).
  END.
  ELSE DO:
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"line-browse-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?).
  END.
  */

      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"line-browse-target",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
           RUN reopen-query IN WIDGET-HANDLE(char-hdl) (?).


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
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  find first ar-inv
      where ar-inv.company eq oe-reth.company
        and ar-inv.cust-no eq oe-reth.cust-no
        and ar-inv.inv-no  eq oe-reth.inv-no
      use-index ar-inv no-lock no-error.

  IF AVAIL ar-inv THEN 
    find first ar-invl no-lock
        where ar-invl.x-no   eq ar-inv.x-no
          AND ar-invl.i-no   eq oe-retl.i-no
          AND ar-invl.ord-no eq oe-retl.ord-no
          AND ar-invl.po-no  eq oe-retl.po-no
        use-index x-no no-error.
  IF AVAIL ar-invl THEN 
     ASSIGN /*lv_unit-pr = ar-invl.unit-pr
            lv_pr-uom = ar-invl.pr-uom */
            lv_sman1 = ar-invl.sman[1]
            lv_s-comm1 = ar-invl.s-comm[1]            
            lv_cas-cnt = ar-invl.cas-cnt
            lv_sman2 = ar-invl.sman[2]
            lv_s-comm2 = ar-invl.s-comm[2]
            lv_disc = ar-invl.disc
            lv_tax = ar-invl.tax
            lv_sman3 = ar-invl.sman[3] 
            lv_s-comm3 = ar-invl.s-comm[3].

 DISPLAY   lv_sman1 lv_s-comm1           
           lv_cas-cnt lv_sman2
            lv_s-comm2 lv_disc 
            lv_tax  lv_sman3
            lv_s-comm3
            WITH FRAME {&FRAME-NAME}.  


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
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL oe-retl AND NOT adm-new-record THEN
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
DEF VAR char-hdl AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  IF NOT AVAIL(oe-reth) THEN DO:
     run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
     /* Process entries 2..N */
     RUN get-oe-reth IN widget-handle(char-hdl) (OUTPUT rOeReth) NO-ERROR.
     FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth
        NO-LOCK NO-ERROR.

  END.
  ELSE
    rOeReth = ROWID(oe-reth).
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

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
  DEF VAR char-hdl AS cha NO-UNDO.
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-i-no (oe-retl.i-no:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

    RUN valid-ord-no (oe-retl.ord-no:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

    RUN valid-po-no (oe-retl.po-no:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-tot-qty-return (oe-retl.tot-qty-return:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-qty-return-inv (oe-retl.qty-return-inv:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-loc (oe-retl.loc:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-loc-bin (oe-retl.loc-bin:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-tag (oe-retl.tag:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-fields.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"line-browse-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN reopen-query IN WIDGET-HANDLE(char-hdl) (RECID(oe-reth)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-inv V-table-Win 
PROCEDURE new-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  FIND FIRST ar-inv NO-LOCK
      WHERE ar-inv.company EQ oe-reth.company
        AND ar-inv.cust-no EQ oe-reth.cust-no
        AND ar-inv.inv-no  EQ oe-reth.inv-no
      USE-INDEX ar-inv NO-ERROR.

  IF AVAIL ar-inv THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND ar-invl NO-LOCK
        WHERE ar-invl.x-no EQ ar-inv.x-no
          AND ar-invl.i-no EQ oe-retl.i-no:SCREEN-VALUE
        USE-INDEX x-no NO-ERROR.

    IF AVAIL ar-invl THEN oe-retl.ord-no:SENSITIVE = NO.
    ELSE DO:
      FIND ar-invl NO-LOCK
          WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
            AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
          USE-INDEX x-no NO-ERROR.
      oe-retl.ord-no:SENSITIVE = YES.
    END.

    IF AVAIL ar-invl THEN oe-retl.po-no:SENSITIVE = NO.
    ELSE DO:
      FIND ar-invl NO-LOCK
          WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
            AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
            AND ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE
          USE-INDEX x-no NO-ERROR.
      oe-retl.po-no:SENSITIVE = YES.
    END.

    IF NOT AVAIL ar-invl THEN
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.x-no   EQ ar-inv.x-no
          AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
          AND (ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE) OR
               INT(oe-retl.ord-no:SCREEN-VALUE) EQ 0)
          AND (ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE OR
               oe-retl.po-no:SCREEN-VALUE EQ "")
        USE-INDEX x-no NO-ERROR.

    IF AVAIL ar-invl THEN
    DO:
      ASSIGN
       oe-retl.i-name:SCREEN-VALUE         = ar-invl.i-name         
       oe-retl.i-dscr:SCREEN-VALUE         = ar-invl.i-dscr
       oe-retl.job-no:SCREEN-VALUE         = ar-invl.job-no
       oe-retl.job-no2:SCREEN-VALUE        = STRING(ar-invl.job-no2)
       oe-retl.qty-return-inv:SCREEN-VALUE = STRING(ar-invl.ship-qty)
       oe-retl.tot-qty-return:SCREEN-VALUE = STRING(ar-invl.ship-qty)
       oe-retl.cost:SCREEN-VALUE           = STRING(ar-invl.cost)
       oe-retl.ord-no:SCREEN-VALUE         = STRING(ar-invl.ord-no)
       oe-retl.po-no:SCREEN-VALUE          = ar-invl.po-no
       oe-retl.uom:SCREEN-VALUE            = ar-invl.pr-uom
       oe-retl.loc:SCREEN-VALUE            = ar-invl.loc
       oe-retl.est-no:SCREEN-VALUE         = ar-invl.est-no
       oe-retl.part-no:SCREEN-VALUE        = ar-invl.part-no
       oe-retl.unit-pr:SCREEN-VALUE        = STRING(ar-invl.unit-pr).

      IF ar-invl.dscr[1] EQ "" THEN
         fi_cost-uom:SCREEN-VALUE = "M".
      ELSE
         fi_cost-uom:SCREEN-VALUE = ar-invl.dscr[1].
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oeretl-add-record V-table-Win 
PROCEDURE oeretl-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS CHAR NO-UNDO.
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"detail-browse-source",OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
      RUN oe/w-oe-retl.w  (INPUT cocode, INPUT ROWID(oe-reth), WIDGET-HANDLE(char-hdl)).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE return-fields-enabled V-table-Win 
PROCEDURE return-fields-enabled :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oplFieldsEnabled AS LOG NO-UNDO.
DEF VAR lFieldsEnabled AS LOG NO-UNDO.
  RUN get-attribute ("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "NO" THEN 
    lFieldsEnabled = FALSE.
  ELSE
    lFieldsEnabled = TRUE.
oplFieldsEnabled = lFieldsEnabled.

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
  {src/adm/template/snd-list.i "oe-retl"}
  {src/adm/template/snd-list.i "oe-reth"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF BUFFER b-oe-retl FOR oe-retl.

  DEF VAR lv-msg AS CHAR NO-UNDO.

  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  FIND FIRST ar-inv NO-LOCK
      WHERE ar-inv.company EQ oe-reth.company
        AND ar-inv.cust-no EQ oe-reth.cust-no
        AND ar-inv.inv-no  EQ oe-reth.inv-no
      USE-INDEX ar-inv NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF lv-msg EQ ""                   AND
       (NOT AVAIL ar-inv OR
        NOT CAN-FIND(FIRST ar-invl
                     WHERE ar-invl.x-no EQ ar-inv.x-no
                       AND ar-invl.i-no EQ ip-focus:SCREEN-VALUE
                     USE-INDEX x-no)) THEN
      lv-msg = TRIM(ip-focus:LABEL) + " does not exist on invoice".

    IF lv-msg EQ ""                                        AND
       CAN-FIND(FIRST b-oe-retl NO-LOCK
                WHERE b-oe-retl.company EQ oe-reth.company
                  AND b-oe-retl.r-no    EQ oe-reth.r-no
                  AND b-oe-retl.i-no    EQ ip-focus:SCREEN-VALUE
                  AND b-oe-retl.tag     EQ oe-retl.tag:SCREEN-VALUE IN FRAME {&FRAME-NAME}                                              
                  AND ROWID(b-oe-retl)  NE ROWID(oe-retl)) THEN
      lv-msg = TRIM(ip-focus:LABEL) + " already exists on this return".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      IF oe-retl.tag:SCREEN-VALUE NE "" THEN
        APPLY "entry" TO oe-retl.tag.
      ELSE
        APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ oe-reth.company
                      AND loc.loc     EQ ip-focus:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin V-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.
  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    ip-focus:SCREEN-VALUE = CAPS(ip-focus:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ oe-reth.company
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ oe-retl.loc:SCREEN-VALUE
                      AND fg-bin.loc-bin EQ ip-focus:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no V-table-Win 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  FIND FIRST ar-inv NO-LOCK
      WHERE ar-inv.company EQ oe-reth.company
        AND ar-inv.cust-no EQ oe-reth.cust-no
        AND ar-inv.inv-no  EQ oe-reth.inv-no
      USE-INDEX ar-inv NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SENSITIVE             AND
       (NOT AVAIL ar-inv OR
        NOT CAN-FIND(FIRST ar-invl
                     WHERE ar-invl.x-no   EQ ar-inv.x-no
                       AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
                       AND ar-invl.ord-no EQ INT(ip-focus:SCREEN-VALUE)
                     USE-INDEX x-no)) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " does not exist on invoice..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no V-table-Win 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  FIND FIRST ar-inv NO-LOCK
      WHERE ar-inv.company EQ oe-reth.company
        AND ar-inv.cust-no EQ oe-reth.cust-no
        AND ar-inv.inv-no  EQ oe-reth.inv-no
      USE-INDEX ar-inv NO-ERROR.

  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SENSITIVE             AND
       (NOT AVAIL ar-inv OR
        NOT CAN-FIND(FIRST ar-invl
                     WHERE ar-invl.x-no   EQ ar-inv.x-no
                       AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
                       AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
                       AND ar-invl.po-no  EQ ip-focus:SCREEN-VALUE
                     USE-INDEX x-no)) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " does not exist on invoice..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty-return-inv V-table-Win 
PROCEDURE valid-qty-return-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ip-focus:SCREEN-VALUE) GT
       DEC(oe-retl.tot-qty-return:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(ip-focus:LABEL)               +
              " may not be more than "           +
              TRIM(oe-retl.tot-qty-return:LABEL) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.      
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag V-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  /* Blank tag is valid */
  IF ip-focus:SCREEN-VALUE EQ "" THEN
    RETURN.
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:        
    FIND FIRST loadtag WHERE loadtag.company EQ cocode
      AND loadtag.item-type EQ NO
      AND loadtag.tag-no EQ ip-focus:SCREEN-VALUE
      AND loadtag.i-no EQ asi.oe-retl.i-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
    IF NOT AVAIL loadtag THEN
      FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
         AND fg-bin.i-no EQ asi.oe-retl.i-no:SCREEN-VALUE
         AND fg-bin.tag  EQ ip-focus:SCREEN-VALUE
      NO-LOCK NO-ERROR.

    IF ip-focus:SENSITIVE             AND
        NOT AVAIL loadtag AND
        NOT AVAIL fg-bin

        THEN DO:
          MESSAGE TRIM(ip-focus:LABEL) + " not a valid tag..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.
  DO WITH FRAME {&FRAME-NAME}:  
    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ oe-reth.company
          AND ar-inv.cust-no EQ oe-reth.cust-no
          AND ar-inv.inv-no  EQ oe-reth.inv-no
        USE-INDEX ar-inv NO-ERROR.
    RELEASE ar-invl.
    IF AVAIL ar-inv THEN
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.x-no   EQ ar-inv.x-no
          AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
          AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
          AND ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE
        USE-INDEX x-no NO-ERROR.
    IF AVAIL ar-invl THEN
      FIND FIRST oe-boll 
        WHERE oe-boll.company EQ ar-invl.company 
          AND oe-boll.b-no EQ ar-invl.b-no 
          AND oe-boll.tag  EQ ip-focus:SCREEN-VALUE
        NO-LOCK NO-ERROR.
      IF ip-focus:SENSITIVE             AND
        NOT AVAIL oe-boll
        THEN DO:        
        MESSAGE TRIM(ip-focus:LABEL) + " not a valid tag..."
                VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ip-focus.
        RETURN ERROR.
      END. /* if record not found */
  END. /* do with frame */
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tot-qty-return V-table-Win 
PROCEDURE valid-tot-qty-return :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  DEF VAR lv-msg AS CHAR NO-UNDO.
  {methods/lValidateError.i YES}
  IF NOT AVAIL oe-reth THEN
    FIND oe-reth WHERE ROWID(oe-reth) EQ rOeReth NO-LOCK NO-ERROR.      

  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ ""                    AND
       DEC(ip-focus:SCREEN-VALUE) EQ 0 THEN
      lv-msg = TRIM(ip-focus:LABEL) + " may not be 0...".

    IF lv-msg EQ "" THEN DO:
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ oe-reth.company
            AND ar-inv.cust-no EQ oe-reth.cust-no
            AND ar-inv.inv-no  EQ oe-reth.inv-no
          USE-INDEX ar-inv NO-ERROR.
      RELEASE ar-invl.
      IF AVAIL ar-inv THEN
      FIND FIRST ar-invl NO-LOCK
          WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
            AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
            AND ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE
          USE-INDEX x-no NO-ERROR.
      IF NOT AVAIL ar-invl                              OR
         DEC(ip-focus:SCREEN-VALUE) GT ar-invl.ship-qty THEN
        lv-msg = TRIM(ip-focus:LABEL) + " may not be more than qty shipped...".
    END.

    IF lv-msg EQ "" AND oe-retl.tag:SCREEN-VALUE GT "" THEN DO:
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ oe-reth.company
            AND ar-inv.cust-no EQ oe-reth.cust-no
            AND ar-inv.inv-no  EQ oe-reth.inv-no
          USE-INDEX ar-inv NO-ERROR.
      RELEASE ar-invl.
      IF AVAIL ar-inv THEN
      FIND FIRST ar-invl NO-LOCK
          WHERE ar-invl.x-no   EQ ar-inv.x-no
            AND ar-invl.i-no   EQ oe-retl.i-no:SCREEN-VALUE
            AND ar-invl.ord-no EQ INT(oe-retl.ord-no:SCREEN-VALUE)
            AND ar-invl.po-no  EQ oe-retl.po-no:SCREEN-VALUE
          USE-INDEX x-no NO-ERROR.
      IF AVAIL ar-invl THEN
      FIND FIRST oe-boll 
        WHERE oe-boll.company EQ ar-invl.company 
          AND oe-boll.b-no EQ ar-invl.b-no 
          AND oe-boll.tag  EQ oe-retl.tag:SCREEN-VALUE
        NO-LOCK NO-ERROR.
      IF AVAIL oe-boll AND DEC(ip-focus:SCREEN-VALUE) GT oe-boll.qty THEN
         lv-msg = TRIM(ip-focus:LABEL) + " may not be more than qty shipped for tag...".
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

