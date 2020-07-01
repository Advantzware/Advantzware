&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/vendcostmtx.w

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
{custom/globdefs.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/*&scoped-def vendItemCost-maint vendItemCost*/

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE BUFFER bf-vendItemCost FOR vendItemCost .
DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
 RUN util/Validate.p PERSISTENT SET hdValidator.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdValidator).

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
&Scoped-define EXTERNAL-TABLES vendItemCost
&Scoped-define FIRST-EXTERNAL-TABLE vendItemCost


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vendItemCost.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vendItemCost.dimWidthMinimum ~
vendItemCost.dimWidthMaximum vendItemCost.dimLengthMinimum ~
vendItemCost.dimLengthMaximum vendItemCost.dimWidthUnder ~
vendItemCost.dimWidthUnderCharge vendItemCost.dimLengthUnder ~
vendItemCost.dimLengthUnderCharge vendItemCost.quantityMinimumOrder ~
vendItemCost.quantityMaximumOrder vendItemCost.validWidth[1] ~
vendItemCost.validWidth[2] vendItemCost.validWidth[3] ~
vendItemCost.validWidth[4] vendItemCost.validWidth[5] ~
vendItemCost.validWidth[6] vendItemCost.validWidth[7] ~
vendItemCost.validWidth[8] vendItemCost.validWidth[9] ~
vendItemCost.validWidth[10] vendItemCost.validWidth[11] ~
vendItemCost.validWidth[12] vendItemCost.validWidth[13] ~
vendItemCost.validWidth[14] vendItemCost.validWidth[15] ~
vendItemCost.validWidth[16] vendItemCost.validWidth[17] ~
vendItemCost.validWidth[18] vendItemCost.validWidth[19] ~
vendItemCost.validWidth[20] 
&Scoped-define ENABLED-TABLES vendItemCost
&Scoped-define FIRST-ENABLED-TABLE vendItemCost
&Scoped-Define DISPLAYED-FIELDS vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo ~
vendItemCost.formNo vendItemCost.blankNo vendItemCost.vendorItemID ~
vendItemCost.effectiveDate vendItemCost.expirationDate ~
vendItemCost.createdDate vendItemCost.createdID vendItemCost.updatedDate ~
vendItemCost.updatedID vendItemCost.vendorUOM vendItemCost.dimWidthMinimum ~
vendItemCost.dimWidthMaximum vendItemCost.dimLengthMinimum ~
vendItemCost.dimLengthMaximum vendItemCost.dimWidthUnder ~
vendItemCost.dimWidthUnderCharge vendItemCost.dimLengthUnder ~
vendItemCost.dimLengthUnderCharge vendItemCost.quantityMinimumOrder ~
vendItemCost.quantityMaximumOrder vendItemCost.validWidth[1] ~
vendItemCost.validWidth[2] vendItemCost.validWidth[3] ~
vendItemCost.validWidth[4] vendItemCost.validWidth[5] ~
vendItemCost.validWidth[6] vendItemCost.validWidth[7] ~
vendItemCost.validWidth[8] vendItemCost.validWidth[9] ~
vendItemCost.validWidth[10] vendItemCost.validWidth[11] ~
vendItemCost.validWidth[12] vendItemCost.validWidth[13] ~
vendItemCost.validWidth[14] vendItemCost.validWidth[15] ~
vendItemCost.validWidth[16] vendItemCost.validWidth[17] ~
vendItemCost.validWidth[18] vendItemCost.validWidth[19] ~
vendItemCost.validWidth[20] 
&Scoped-define DISPLAYED-TABLES vendItemCost
&Scoped-define FIRST-DISPLAYED-TABLE vendItemCost


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS vendItemCost.itemID vendItemCost.vendorID ~
vendItemCost.customerID vendItemCost.estimateNo 
&Scoped-define ADM-ASSIGN-FIELDS vendItemCost.itemID vendItemCost.vendorID ~
vendItemCost.customerID vendItemCost.estimateNo vendItemCost.formNo ~
vendItemCost.blankNo vendItemCost.vendorItemID vendItemCost.effectiveDate ~
vendItemCost.expirationDate vendItemCost.createdDate vendItemCost.createdID ~
vendItemCost.updatedDate vendItemCost.updatedID vendItemCost.vendorUOM ~
vendItemCost.quantityMaximumOrder 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.vendItemCost.company
itemID||y|ASI.vendItemCost.itemID
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,itemID"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143 BY 16.43.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 141.2 BY 15.62.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 93 BY 14.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vendItemCost.itemType AT ROW 2.29 COL 14.2 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "FG","FG",
                     "RM","RM"
          DROP-DOWN-LIST
          SIZE 9 BY 1
     vendItemCost.itemID AT ROW 3.43 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 
     vendItemCost.vendorID AT ROW 4.57 COL 14.2 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.customerID AT ROW 5.71 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.estimateNo AT ROW 6.95 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.formNo AT ROW 6.95 COL 31.4 COLON-ALIGNED
          LABEL "F"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 
     vendItemCost.blankNo AT ROW 6.95 COL 40.2 COLON-ALIGNED
          LABEL "B"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 
     vendItemCost.vendorItemID AT ROW 8.14 COL 14.2 COLON-ALIGNED
          LABEL "vend Item"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 15 
     vendItemCost.effectiveDate AT ROW 9.33 COL 14.2 COLON-ALIGNED
          LABEL "Effective"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 15 
     vendItemCost.expirationDate AT ROW 10.57 COL 14.2 COLON-ALIGNED
          LABEL "Expires"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 15 
     vendItemCost.createdDate AT ROW 11.81 COL 14.2 COLON-ALIGNED
          LABEL "Created"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.createdID AT ROW 11.81 COL 35 COLON-ALIGNED
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.updatedDate AT ROW 13.14 COL 14.2 COLON-ALIGNED
          LABEL "Updated"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.updatedID AT ROW 13.1 COL 35 COLON-ALIGNED
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.vendorUOM AT ROW 2.24 COL 95.2 COLON-ALIGNED
          LABEL "Cost and Quantity UOM"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     vendItemCost.dimWidthMinimum AT ROW 4.19 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 38 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimWidthMaximum AT ROW 4.19 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 36 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimLengthMinimum AT ROW 5.38 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 30 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimLengthMaximum AT ROW 5.38 COL 69.2 COLON-ALIGNED NO-LABEL WIDGET-ID 28 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimWidthUnder AT ROW 8.43 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 42 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vendItemCost.dimWidthUnderCharge AT ROW 8.38 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 54 FORMAT "->>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimLengthUnder AT ROW 9.52 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 34 FORMAT ">>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.dimLengthUnderCharge AT ROW 9.52 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 52 FORMAT "->>,>>9.9999"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.quantityMinimumOrder AT ROW 11.29 COL 68 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     vendItemCost.quantityMaximumOrder AT ROW 12.71 COL 68 COLON-ALIGNED WIDGET-ID 44
          LABEL "Max Order Qty" FORMAT ">>>>>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[1] AT ROW 4.33 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[2] AT ROW 5.38 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[3] AT ROW 6.43 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[4] AT ROW 7.48 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[5] AT ROW 8.52 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[6] AT ROW 9.57 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[7] AT ROW 10.62 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[8] AT ROW 11.67 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[9] AT ROW 12.71 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[10] AT ROW 13.76 COL 84.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[11] AT ROW 4.33 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[12] AT ROW 5.38 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[13] AT ROW 6.43 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[14] AT ROW 7.48 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vendItemCost.validWidth[15] AT ROW 8.52 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[16] AT ROW 9.57 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[17] AT ROW 10.62 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[18] AT ROW 11.67 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[19] AT ROW 12.71 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validWidth[20] AT ROW 13.76 COL 98.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[1] AT ROW 4.33 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[2] AT ROW 5.38 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[3] AT ROW 6.43 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[4] AT ROW 7.48 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[5] AT ROW 8.52 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[6] AT ROW 9.57 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[7] AT ROW 10.62 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[8] AT ROW 11.67 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[9] AT ROW 12.71 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[10] AT ROW 13.76 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[11] AT ROW 4.29 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[12] AT ROW 5.38 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[13] AT ROW 6.43 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[14] AT ROW 7.48 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     vendItemCost.validLength[15] AT ROW 8.52 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[16] AT ROW 9.57 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[17] AT ROW 10.62 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[18] AT ROW 11.67 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[19] AT ROW 12.71 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.validLength[20] AT ROW 13.76 COL 126.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     "Upcharge" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 7.62 COL 71.2 WIDGET-ID 14
     "L:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 5.48 COL 53 WIDGET-ID 22
     "Under" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 7.62 COL 58.2 WIDGET-ID 12
     "Valid Roll Widths" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 3.57 COL 90 WIDGET-ID 50
     " Restrictions" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.71 COL 51 WIDGET-ID 8
     " Vendor Item Cost Details" VIEW-AS TEXT
          SIZE 29.8 BY .62 AT ROW 1.33 COL 4.2
     "W:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 8.57 COL 53 WIDGET-ID 16
     "L:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 9.52 COL 53 WIDGET-ID 18
     "Min" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.57 COL 58.2 WIDGET-ID 24
     "Max" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.57 COL 72 WIDGET-ID 26
     "W:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 4.52 COL 52.8 WIDGET-ID 20
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 1.48 COL 2
     RECT-6 AT ROW 2.05 COL 49.2 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.vendItemCost
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
         HEIGHT             = 17.86
         WIDTH              = 158.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer4.i}

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

/* SETTINGS FOR FILL-IN vendItemCost.blankNo IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.createdDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.createdID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.customerID IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN vendItemCost.dimLengthMaximum IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimLengthMinimum IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimLengthUnder IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimLengthUnderCharge IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vendItemCost.dimWidthMaximum IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimWidthMinimum IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimWidthUnder IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vendItemCost.dimWidthUnderCharge IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vendItemCost.effectiveDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.estimateNo IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN vendItemCost.expirationDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.formNo IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.itemID IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR COMBO-BOX vendItemCost.itemType IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vendItemCost.quantityMaximumOrder IN FRAME F-Main
   2 EXP-LABEL EXP-FORMAT                                               */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vendItemCost.updatedDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.updatedID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.validLength[10] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[10]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[11] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[11]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[12] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[12]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[13] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[13]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[14] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[14]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[15] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[15]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[16] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[16]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[17] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[17]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[18] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[18]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[19] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[19]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[1] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[1]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[20] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[20]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[2] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[2]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[3] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[3]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[4] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[4]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[5] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[5]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[6] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[6]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[7] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[7]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[8] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[8]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.validLength[9] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       vendItemCost.validLength[9]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN vendItemCost.vendorID IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN vendItemCost.vendorItemID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.vendorUOM IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
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
  DEFINE VARIABLE riLookup AS RECID NO-UNDO.
  def var lv-handle as handle no-undo.
  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

  {&methods/lValidateError.i YES}
  case FOCUS:NAME :
    when "vendorUOM" then do:
        IF vendItemCost.itemType:SCREEN-VALUE EQ "RM" THEN do:
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ cocode
                AND ITEM.i-no EQ  vendItemCost.itemID:SCREEN-VALUE NO-ERROR.
            IF AVAIL ITEM THEN
                RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
        END.
        ELSE DO:
            RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
        END.

        run windows/l-stduom.w (cocode, uom-list, focus:screen-value, output char-val).
        if char-val ne "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
    end.
    WHEN "customerID" THEN DO:
        
         RUN system/openlookup.p (g_company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN vendItemCost.customerID:SCREEN-VALUE = cMainField.        
    END.
    WHEN "vendorID" THEN DO:
        
         RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN vendItemCost.vendorID:SCREEN-VALUE = cMainField.        
    END.
    WHEN "itemID" THEN DO:
        
        IF vendItemCost.itemType:SCREEN-VALUE = "RM" THEN DO: 
            RUN system/openlookup.p (g_company, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
             IF cMainField <> "" THEN vendItemCost.itemID:SCREEN-VALUE = cMainField. 
        END.
        ELSE DO:  /* finished good */
            RUN system/openlookup.p (g_company, "i-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
             IF cMainField <> "" THEN vendItemCost.itemID:SCREEN-VALUE = cMainField. 
        END.
    END.
    WHEN "estimateNo" THEN DO:
        
         RUN windows/l-est.w (g_company,g_loc,vendItemCost.estimateNo:screen-value, OUTPUT char-val).
         IF char-val <> "" THEN DO:
             FIND FIRST eb NO-LOCK WHERE RECID(eb) = INT(char-val) NO-ERROR.
             IF AVAIL eb THEN 
                 vendItemCost.estimateNo:screen-value = eb.est-no.
         END.
    END.

    otherwise do:
      lv-handle = focus:handle.
      run applhelp.p.

      if g_lookup-var ne "" then lv-handle:screen-value = g_lookup-var. 

      apply "entry" to lv-handle.
      return no-apply.
    end.
  end case.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = vendItemCost
           &WHERE = "WHERE vendItemCost.company eq key-value"
       }
  END CASE.

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
  {src/adm/template/row-list.i "vendItemCost"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vendItemCost"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-vendItemCost-field V-table-Win 
PROCEDURE enable-vendItemCost-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE ALL.
  END.
  RUN set-panel (0).

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
  disable all with frame {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-panel (1).

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
  vendItemCost.company = cocode.
 

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost)).

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

  /*RUN pDisplayValue .*/

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
    DEFINE VARIABLE lNewRecord AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
   
  END.
 
  disable all with frame {&frame-name}.

  IF adm-adding-record THEN lNewRecord = YES .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*RUN set-panel (0).*/

  
  adm-adding-record = NO .
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "vendItemCost" "company"}
  {src/adm/template/sndkycas.i "itemID" "vendItemCost" "itemID"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "vendItemCost"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-panel V-table-Win 
PROCEDURE set-panel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-switch AS INT NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

  END.

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

