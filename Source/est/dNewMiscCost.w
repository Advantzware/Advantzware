&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/d-estrel.w
  
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


DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
DEFINE VARIABLE opCADCAM AS CHARACTER NO-UNDO.
DEFINE VARIABLE lButtonLabel AS LOGICAL NO-UNDO .
IF PROGRAM-NAME(2) MATCHES "*est/v-est4.*"  THEN
    ASSIGN lButtonLabel = YES .

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE v-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE k_frac     AS DECIMAL INIT 6.25 NO-UNDO.

DEFINE BUFFER bff-e-itemfg-vend FOR e-itemfg-vend .

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


/*{est\ttInputEst.i}*/
{sys/inc/var.i shared}
{custom/gcompany.i}  
{sys/inc/venditemcost.i}
gcompany = cocode.


{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
    DEFINE TEMP-TABLE tt-64-dec NO-UNDO
        FIELD DEC AS DECIMAL DECIMALS 6.

    DO v-count = 0 TO 63:
        CREATE tt-64-dec.
        tt-64-dec.DEC = v-count / 64.0.
        RELEASE tt-64-dec.
    END.
END.

DEFINE VARIABLE iFormNumber      AS INTEGER NO-UNDO.
DEFINE VARIABLE iBlankNumber     AS INTEGER NO-UNDO.
DEFINE VARIABLE iNumofCADForm    AS INTEGER NO-UNDO.
DEFINE VARIABLE iProjectCount    AS INTEGER INIT 50 NO-UNDO.
DEFINE VARIABLE lv-copy-qty      AS INTEGER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-copy-rel      AS INTEGER EXTENT 20 NO-UNDO.
DEFINE VARIABLE lv-crt-est-rowid AS ROWID   NO-UNDO.
DEFINE VARIABLE uom-list         AS cha     INIT "C,CS,EA,L,M," NO-UNDO.
DEFINE VARIABLE ilogic           AS LOG     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS quantity cCustNo ship-to cVendor cVendorItem ~
cCostUom cItemDscr1 dSuCost1 dEaCost1 iQtyPer1 cCostType1 cMatLab1 ~
cItemDscr2 dSuCost2 dEaCost2 iQtyPer2 cCostType2 cMatLab2 cItemDscr3 ~
dSuCost3 dEaCost3 iQtyPer3 cCostType3 cMatLab3 cItemDscr4 dSuCost4 dEaCost4 ~
iQtyPer4 cCostType4 cMatLab4 cItemDscr5 dSuCost5 dEaCost5 iQtyPer5 ~
cCostType5 cMatLab5 Btn_OK Btn_Cancel RECT-1 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS quantity cCustNo ship-to cVendor ~
cVendorItem cCostUom cItemDscr1 dSuCost1 dEaCost1 iQtyPer1 cCostType1 ~
cMatLab1 cItemDscr2 dSuCost2 dEaCost2 iQtyPer2 cCostType2 cMatLab2 ~
cItemDscr3 dSuCost3 dEaCost3 iQtyPer3 cCostType3 cMatLab3 cItemDscr4 ~
dSuCost4 dEaCost4 iQtyPer4 cCostType4 cMatLab4 cItemDscr5 dSuCost5 dEaCost5 ~
iQtyPer5 cCostType5 cMatLab5 iForm iBlank est-no cust-name ship-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Next" 
     SIZE 15 BY 1.29
     BGCOLOR 8 .

DEFINE VARIABLE cCostType1 AS CHARACTER FORMAT "X(25)":U INITIAL "Include w/Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Include w/Vendor","N",
                     "Separate Bill","S"
     DROP-DOWN-LIST
     SIZE 24.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCostType2 AS CHARACTER FORMAT "X(25)":U INITIAL "Include w/Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Include w/Vendor","N",
                     "Separate Bill","S"
     DROP-DOWN-LIST
     SIZE 24.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCostType3 AS CHARACTER FORMAT "X(25)":U INITIAL "Include w/Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Include w/Vendor","N",
                     "Separate Bill","S"
     DROP-DOWN-LIST
     SIZE 24.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCostType4 AS CHARACTER FORMAT "X(25)":U INITIAL "Include w/Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Include w/Vendor","N",
                     "Separate Bill","S"
     DROP-DOWN-LIST
     SIZE 24.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCostType5 AS CHARACTER FORMAT "X(25)":U INITIAL "Include w/Vendor" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Include w/Vendor","N",
                     "Separate Bill","S"
     DROP-DOWN-LIST
     SIZE 24.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCostUom AS CHARACTER FORMAT "X(5)":U 
     LABEL "Cost/Uom" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 10.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMatLab1 AS CHARACTER FORMAT "X(5)":U INITIAL "Mat" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Mat","M",
                     "Lab","L"
     DROP-DOWN-LIST
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMatLab2 AS CHARACTER FORMAT "X(5)":U INITIAL "Mat" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Mat","M",
                     "Lab","L"
     DROP-DOWN-LIST
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMatLab3 AS CHARACTER FORMAT "X(5)":U INITIAL "Mat" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Mat","M",
                     "Lab","L"
     DROP-DOWN-LIST
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMatLab4 AS CHARACTER FORMAT "X(5)":U INITIAL "Mat" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Mat","M",
                     "Lab","L"
     DROP-DOWN-LIST
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cMatLab5 AS CHARACTER FORMAT "X(5)":U INITIAL "Mat" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Mat","M",
                     "Lab","L"
     DROP-DOWN-LIST
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustNo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cust#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemDscr1 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemDscr2 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemDscr3 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemDscr4 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemDscr5 AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cust-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cVendor AS CHARACTER FORMAT "X(8)":U 
     LABEL "Vendor" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cVendorItem AS CHARACTER FORMAT "X(16)":U 
     LABEL "Vendor Item#" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost1 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost2 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost3 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost4 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost5 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSuCost1 AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSuCost2 AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSuCost3 AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSuCost4 AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSuCost5 AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iBlank AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Blank #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iForm AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Form #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQtyPer1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQtyPer2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQtyPer3 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQtyPer4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iQtyPer5 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE quantity AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-name AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE ship-to AS CHARACTER FORMAT "X(8)":U 
     LABEL "Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 122.2 BY 3
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 122.2 BY 14.91
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     quantity AT ROW 2.14 COL 46.2 COLON-ALIGNED WIDGET-ID 198
     cCustNo AT ROW 3.33 COL 13.4 COLON-ALIGNED WIDGET-ID 176
     ship-to AT ROW 3.33 COL 71 COLON-ALIGNED WIDGET-ID 178
     cVendor AT ROW 5.19 COL 17 COLON-ALIGNED WIDGET-ID 88
     cVendorItem AT ROW 5.19 COL 54 COLON-ALIGNED WIDGET-ID 238
     cCostUom AT ROW 5.19 COL 106.8 COLON-ALIGNED WIDGET-ID 240
     cItemDscr1 AT ROW 7.38 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     dSuCost1 AT ROW 7.38 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 254
     dEaCost1 AT ROW 7.38 COL 50.4 COLON-ALIGNED NO-LABEL WIDGET-ID 266
     iQtyPer1 AT ROW 7.38 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 276
     cCostType1 AT ROW 7.38 COL 77.8 COLON-ALIGNED NO-LABEL WIDGET-ID 290
     cMatLab1 AT ROW 7.38 COL 104.6 COLON-ALIGNED NO-LABEL WIDGET-ID 302
     cItemDscr2 AT ROW 8.48 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 244
     dSuCost2 AT ROW 8.48 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     dEaCost2 AT ROW 8.48 COL 50.4 COLON-ALIGNED NO-LABEL WIDGET-ID 268
     iQtyPer2 AT ROW 8.48 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 280
     cCostType2 AT ROW 8.48 COL 77.8 COLON-ALIGNED NO-LABEL WIDGET-ID 292
     cMatLab2 AT ROW 8.48 COL 104.6 COLON-ALIGNED NO-LABEL WIDGET-ID 304
     cItemDscr3 AT ROW 9.57 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 246
     dSuCost3 AT ROW 9.57 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 258
     dEaCost3 AT ROW 9.57 COL 50.4 COLON-ALIGNED NO-LABEL WIDGET-ID 270
     iQtyPer3 AT ROW 9.57 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 282
     cCostType3 AT ROW 9.57 COL 77.8 COLON-ALIGNED NO-LABEL WIDGET-ID 294
     cMatLab3 AT ROW 9.57 COL 104.6 COLON-ALIGNED NO-LABEL WIDGET-ID 306
     cItemDscr4 AT ROW 10.71 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 248
     dSuCost4 AT ROW 10.71 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 260
     dEaCost4 AT ROW 10.71 COL 50.4 COLON-ALIGNED NO-LABEL WIDGET-ID 272
     iQtyPer4 AT ROW 10.71 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 284
     cCostType4 AT ROW 10.71 COL 77.8 COLON-ALIGNED NO-LABEL WIDGET-ID 296
     cMatLab4 AT ROW 10.71 COL 104.6 COLON-ALIGNED NO-LABEL WIDGET-ID 308
     cItemDscr5 AT ROW 11.86 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 250
     dSuCost5 AT ROW 11.86 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     dEaCost5 AT ROW 11.86 COL 50.4 COLON-ALIGNED NO-LABEL WIDGET-ID 274
     iQtyPer5 AT ROW 11.86 COL 65.8 COLON-ALIGNED NO-LABEL WIDGET-ID 286
     cCostType5 AT ROW 11.86 COL 77.8 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     cMatLab5 AT ROW 11.86 COL 104.6 COLON-ALIGNED NO-LABEL WIDGET-ID 310
     iForm AT ROW 2.14 COL 93.4 COLON-ALIGNED WIDGET-ID 314
     iBlank AT ROW 2.14 COL 111.2 COLON-ALIGNED WIDGET-ID 316
     Btn_OK AT ROW 13.86 COL 43.2
     Btn_Cancel AT ROW 13.86 COL 64.6
     est-no AT ROW 2.14 COL 13.4 COLON-ALIGNED WIDGET-ID 200
     cust-name AT ROW 3.33 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 202
     ship-name AT ROW 3.33 COL 89.4 COLON-ALIGNED NO-LABEL WIDGET-ID 204
     "Main Input" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
     "Item Description" VIEW-AS TEXT
          SIZE 22 BY 1 AT ROW 6.38 COL 6 WIDGET-ID 242
     "SU Cost" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 6.38 COL 37.2 WIDGET-ID 252
     "EA Cost" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 6.38 COL 52.4 WIDGET-ID 264
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     "Qty Per" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 6.38 COL 67.8 WIDGET-ID 278
     "Cost Type" VIEW-AS TEXT
          SIZE 12.8 BY 1 AT ROW 6.38 COL 80.2 WIDGET-ID 288
     "Mat/Lab" VIEW-AS TEXT
          SIZE 12.8 BY 1 AT ROW 6.38 COL 106.4 WIDGET-ID 300
     RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
     RECT-5 AT ROW 1.1 COL 1.8 WIDGET-ID 312
     SPACE(0.39) SKIP(0.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Miscellaneous Product Estimate - Cost Details "
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cust-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN est-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBlank IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship-name IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Miscellaneous Product Estimate - Cost Details  */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:     
        APPLY "go" TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Next */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN valid-vend-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .
         
        SESSION:SET-WAIT-STATE("general").
  
        RUN pAssignValues .
        
        SESSION:SET-WAIT-STATE("").
  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCostUom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCostUom D-Dialog
ON HELP OF cCostUom IN FRAME D-Dialog /* Cost/Uom */
DO:
    /*DEFINE VARIABLE char-val   AS cha   NO-UNDO.
    DEFINE VARIABLE look-recid AS RECID NO-UNDO.

    RUN windows/l-cstprt.w (gcompany, "", FOCUS:SCREEN-VALUE, "", OUTPUT char-val, OUTPUT look-recid).
    IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
        ASSIGN
            SELF:screen-value      = ENTRY(1,char-val)
             .*/

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCostUom D-Dialog
ON LEAVE OF cCostUom IN FRAME D-Dialog /* Cost/Uom */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemDscr1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemDscr1 D-Dialog
ON LEAVE OF cItemDscr1 IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
          IF cItemDscr1:SCREEN-VALUE NE "" THEN
              ASSIGN iQtyPer1:SCREEN-VALUE = "1" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemDscr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemDscr2 D-Dialog
ON LEAVE OF cItemDscr2 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr2:SCREEN-VALUE NE "" THEN
              ASSIGN iQtyPer2:SCREEN-VALUE = "1" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemDscr3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemDscr3 D-Dialog
ON LEAVE OF cItemDscr3 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr3:SCREEN-VALUE NE "" THEN
              ASSIGN iQtyPer3:SCREEN-VALUE = "1" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemDscr4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemDscr4 D-Dialog
ON LEAVE OF cItemDscr4 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr4:SCREEN-VALUE NE "" THEN
              ASSIGN iQtyPer4:SCREEN-VALUE = "1" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cItemDscr5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cItemDscr5 D-Dialog
ON LEAVE OF cItemDscr5 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr5:SCREEN-VALUE NE "" THEN
              ASSIGN iQtyPer5:SCREEN-VALUE = "1" .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cVendor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cVendor D-Dialog
ON HELP OF cVendor IN FRAME D-Dialog /* Vendor */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        
        RUN windows/l-vendno.w (gcompany, "", cVendor:screen-value, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cVendor D-Dialog
ON LEAVE OF cVendor IN FRAME D-Dialog /* Vendor */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            RUN valid-vend-no(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cVendorItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cVendorItem D-Dialog
ON LEAVE OF cVendorItem IN FRAME D-Dialog /* Vendor Item# */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQtyPer1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQtyPer1 D-Dialog
ON LEAVE OF iQtyPer1 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr1:SCREEN-VALUE NE "" THEN do:
                IF INTEGER(iQtyPer1:SCREEN-VALUE) LE 0 THEN
                    ASSIGN iQtyPer1:SCREEN-VALUE = "1" .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQtyPer2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQtyPer2 D-Dialog
ON LEAVE OF iQtyPer2 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr2:SCREEN-VALUE NE "" THEN do:
                IF INTEGER(iQtyPer2:SCREEN-VALUE) LE 0 THEN
                    ASSIGN iQtyPer2:SCREEN-VALUE = "1" .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQtyPer3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQtyPer3 D-Dialog
ON LEAVE OF iQtyPer3 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr3:SCREEN-VALUE NE "" THEN do:
                IF INTEGER(iQtyPer3:SCREEN-VALUE) LE 0 THEN
                    ASSIGN iQtyPer3:SCREEN-VALUE = "1" .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQtyPer4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQtyPer4 D-Dialog
ON LEAVE OF iQtyPer4 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr4:SCREEN-VALUE NE "" THEN do:
                IF INTEGER(iQtyPer4:SCREEN-VALUE) LE 0 THEN
                    ASSIGN iQtyPer4:SCREEN-VALUE = "1" .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iQtyPer5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iQtyPer5 D-Dialog
ON LEAVE OF iQtyPer5 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            IF cItemDscr5:SCREEN-VALUE NE "" THEN do:
                IF INTEGER(iQtyPer5:SCREEN-VALUE) LE 0 THEN
                    ASSIGN iQtyPer5:SCREEN-VALUE = "1" .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /*{src/adm/template/dialogmn.i}*/

    RUN enable_UI.

    {methods/nowait.i}
     
    RUN pDisplayValue .

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY quantity cCustNo ship-to cVendor cVendorItem cCostUom cItemDscr1 
          dSuCost1 dEaCost1 iQtyPer1 cCostType1 cMatLab1 cItemDscr2 dSuCost2 
          dEaCost2 iQtyPer2 cCostType2 cMatLab2 cItemDscr3 dSuCost3 dEaCost3 
          iQtyPer3 cCostType3 cMatLab3 cItemDscr4 dSuCost4 dEaCost4 iQtyPer4 
          cCostType4 cMatLab4 cItemDscr5 dSuCost5 dEaCost5 iQtyPer5 cCostType5 
          cMatLab5 iForm iBlank est-no cust-name ship-name 
      WITH FRAME D-Dialog.
  ENABLE quantity cCustNo ship-to cVendor cVendorItem cCostUom cItemDscr1 
         dSuCost1 dEaCost1 iQtyPer1 cCostType1 cMatLab1 cItemDscr2 dSuCost2 
         dEaCost2 iQtyPer2 cCostType2 cMatLab2 cItemDscr3 dSuCost3 dEaCost3 
         iQtyPer3 cCostType3 cMatLab3 cItemDscr4 dSuCost4 dEaCost4 iQtyPer4 
         cCostType4 cMatLab4 cItemDscr5 dSuCost5 dEaCost5 iQtyPer5 cCostType5 
         cMatLab5 Btn_OK Btn_Cancel RECT-1 RECT-5 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignValues D-Dialog 
PROCEDURE pAssignValues :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-ref-rec-qty AS RECID NO-UNDO.
    DEFINE VARIABLE lv-ref-rec-cst AS RECID NO-UNDO.
    DEFINE VARIABLE dCalValueSetup AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dCalValueCost AS DECIMAL NO-UNDO .
    
    FIND FIRST eb WHERE ROWID(eb) EQ iprRowid  EXCLUSIVE-LOCK NO-ERROR.
        
    IF AVAILABLE eb THEN 
    DO:
        FIND FIRST ef EXCLUSIVE-LOCK
            WHERE ef.company EQ eb.company
            AND ef.est-no EQ eb.est-no
            AND ef.form-no EQ eb.form-no NO-ERROR .
        IF AVAILABLE ef THEN 
        DO:

            ASSIGN
                ef.mis-cost[1] = cItemDscr1 
                ef.mis-cost[2] = cItemDscr2
                ef.mis-cost[3] = cItemDscr3
                ef.mis-cost[4] = cItemDscr4
                ef.mis-cost[5] = cItemDscr5  .

            IF ef.mis-cost[1] NE "" THEN
                ASSIGN
                    ef.mis-snum[1] = eb.form-no   
                    ef.mis-bnum[1] = eb.blank-no.
            IF ef.mis-cost[2] NE "" THEN
                ASSIGN
                    ef.mis-snum[2] = eb.form-no   
                    ef.mis-bnum[2] = eb.blank-no.
            IF ef.mis-cost[3] NE "" THEN
                ASSIGN
                    ef.mis-snum[3] = eb.form-no   
                    ef.mis-bnum[3] = eb.blank-no.
            IF ef.mis-cost[4] NE "" THEN
                ASSIGN
                    ef.mis-snum[4] = eb.form-no   
                    ef.mis-bnum[4] = eb.blank-no.
            IF ef.mis-cost[5] NE "" THEN
                ASSIGN
                    ef.mis-snum[5] = eb.form-no   
                    ef.mis-bnum[5] = eb.blank-no.

            IF cMatLab1 EQ "M" THEN 
            DO:
                ASSIGN
                    ef.mis-matf[1] = dSuCost1 
                    ef.mis-matm[1] = dEaCost1 * iQtyPer1 * 1000 
                    ef.mis-labf[1] = 0 
                    ef.mis-labm[1] = 0.
            END.
            ELSE 
            DO:
                ASSIGN
                    ef.mis-labf[1] = dSuCost1 
                    ef.mis-labm[1] = dEaCost1 * iQtyPer1 * 1000 
                    ef.mis-matf[1] = 0
                    ef.mis-matm[1] = 0
                    .
            END.

            IF cMatLab2 EQ "M" THEN 
            DO:
                ASSIGN
                    ef.mis-matf[2] = dSuCost2 
                    ef.mis-matm[2] = dEaCost2 * iQtyPer2 * 1000  
                    ef.mis-labf[2] = 0
                    ef.mis-labm[2] = 0.
            END.
            ELSE 
            DO:
                ASSIGN
                    ef.mis-labf[2] = dSuCost2 
                    ef.mis-labm[2] = dEaCost2 * iQtyPer2 * 1000
                    ef.mis-matf[2] = 0
                    ef.mis-matm[2] = 0 .
            END.

            IF cMatLab3 EQ "M" THEN 
            DO:
                ASSIGN
                    ef.mis-matf[3] = dSuCost3 
                    ef.mis-matm[3] = dEaCost3 * iQtyPer3 * 1000 
                    ef.mis-labf[3] = 0 
                    ef.mis-labm[3] = 0.
            END.
            ELSE 
            DO:
                ASSIGN
                    ef.mis-labf[3] = dSuCost3 
                    ef.mis-labm[3] = dEaCost3 * iQtyPer3 * 1000 
                    ef.mis-matf[3] = 0
                    ef.mis-matm[3] = 0.
            END.

            IF cMatLab4 EQ "M" THEN 
            DO:
                ASSIGN
                    ef.mis-matf[4] = dSuCost4 
                    ef.mis-matm[4] = dEaCost4 * iQtyPer4 * 1000
                    ef.mis-labf[4] = 0 
                    ef.mis-labm[4] = 0 .
            END.
            ELSE 
            DO:
                ASSIGN
                    ef.mis-labf[4] = dSuCost4 
                    ef.mis-labm[4] = dEaCost4 * iQtyPer4 * 1000
                    ef.mis-matf[4] = 0 
                    ef.mis-matm[4] = 0.
            END.

            IF cMatLab5 EQ "M" THEN 
            DO:
                ASSIGN
                    ef.mis-matf[5] = dSuCost5 
                    ef.mis-matm[5] = dEaCost5 * iQtyPer5 * 1000  
                    ef.mis-labf[5] = 0
                    ef.mis-labm[5] = 0.
            END.
            ELSE 
            DO:
                ASSIGN
                    ef.mis-labf[5] = dSuCost5 
                    ef.mis-labm[5] = dEaCost5 * iQtyPer5 * 1000 
                    ef.mis-matf[5] = 0
                    ef.mis-matm[5] = 0.
            END.

            ASSIGN
                ef.mis-simon[1] = cCostType1
                ef.mis-simon[2] = cCostType2
                ef.mis-simon[3] = cCostType3
                ef.mis-simon[4] = cCostType4
                ef.mis-simon[5] = cCostType5 
                ef.misQtyPer[1] = iQtyPer1 
                ef.misQtyPer[2] = iQtyPer2
                ef.misQtyPer[3] = iQtyPer3
                ef.misQtyPer[4] = iQtyPer4
                ef.misQtyPer[5] = iQtyPer5 . 
 
            IF ef.mis-cost[1] NE "" THEN 
            DO:

                IF cMatLab1 EQ "M" THEN 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Mat", 1,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost1 * iQtyPer1 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .
                END.
                ELSE 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Lab", 1,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost1 * iQtyPer1 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .    
                END.
            END.

            IF ef.mis-cost[2] NE "" THEN 
            DO:

                IF cMatLab2 EQ "M" THEN 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Mat", 2,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost2 * iQtyPer2 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .
                END.
                ELSE 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Lab", 2,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost2 * iQtyPer2 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .    
                END.
            END.

            IF ef.mis-cost[3] NE "" THEN 
            DO:

                IF cMatLab3 EQ "M" THEN 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Mat", 3,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost3 * iQtyPer3 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .
                END.
                ELSE 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Lab", 3,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost3 * iQtyPer3 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .    
                END.
            END.

            IF ef.mis-cost[4] NE "" THEN 
            DO:

                IF cMatLab4 EQ "M" THEN 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Mat", 4,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost4 * iQtyPer4 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .
                END.
                ELSE 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Lab", 4,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost4 * iQtyPer4 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .    
                END.
            END.


            IF ef.mis-cost[5] NE "" THEN 
            DO:

                IF cMatLab5 EQ "M" THEN 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Mat", 5,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost5 * iQtyPer5 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .
                END.
                ELSE 
                DO:
                    RUN cec/refestg1.p (ROWID(eb), "Lab", 5,
                        OUTPUT lv-ref-rec-qty,
                        OUTPUT lv-ref-rec-cst). 

                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-cst .
                    ASSIGN 
                        reftable.val[1] = (dEaCost5 * iQtyPer5 * 1000 ) .
             
                    FIND reftable WHERE RECID(reftable) = lv-ref-rec-qty .
                    ASSIGN 
                        reftable.val[1] = 99999999.00 .    
                END.
            END.
            IF cCostType1 EQ "N" THEN
                ASSIGN 
                    dCalValueSetup = dCalValueSetup + dSuCost1
                    dCalValueCost = dCalValueCost + dEaCost1 * iQtyPer1 * ( IF cCostUom EQ "M" THEN 1000  ELSE 1).
            IF cCostType2 EQ "N" THEN
                ASSIGN 
                    dCalValueSetup = dCalValueSetup + dSuCost2
                    dCalValueCost = dCalValueCost + dEaCost2 * iQtyPer2 * ( IF cCostUom EQ "M" THEN 1000  ELSE 1).
            IF cCostType3 EQ "N" THEN
                ASSIGN 
                    dCalValueSetup = dCalValueSetup + dSuCost3
                    dCalValueCost = dCalValueCost + dEaCost3 * iQtyPer3 * ( IF cCostUom EQ "M" THEN 1000  ELSE 1).                
            IF cCostType4 EQ "N" THEN
                ASSIGN 
                    dCalValueSetup = dCalValueSetup + dSuCost4
                    dCalValueCost = dCalValueCost + dEaCost4 * iQtyPer4 * ( IF cCostUom EQ "M" THEN 1000  ELSE 1).
            IF cCostType5 EQ "N" THEN
                ASSIGN 
                    dCalValueSetup = dCalValueSetup + dSuCost5
                    dCalValueCost = dCalValueCost + dEaCost5 * iQtyPer5 * ( IF cCostUom EQ "M" THEN 1000  ELSE 1).            
           
            IF lNewVendorItemCost THEN RUN pAssignValuestToNewVendorCost (dCalValueCost, dCalValueSetup  ).
            ELSE DO:
                FIND FIRST bff-e-itemfg-vend EXCLUSIVE-LOCK
                    WHERE bff-e-itemfg-vend.company = eb.company 
                     AND bff-e-itemfg-vend.est-no = eb.est-no 
                     AND bff-e-itemfg-vend.eqty = eb.eqty 
                     AND bff-e-itemfg-vend.form-no = eb.form-no 
                     AND bff-e-itemfg-vend.blank-no = eb.blank-no
                     AND bff-e-itemfg-vend.vend-no NE ""  NO-ERROR .
                IF NOT AVAIL bff-e-itemfg-vend  THEN
                    FIND FIRST bff-e-itemfg-vend EXCLUSIVE-LOCK
                    WHERE bff-e-itemfg-vend.company = eb.company 
                     AND bff-e-itemfg-vend.est-no = eb.est-no 
                     AND bff-e-itemfg-vend.eqty = eb.eqty 
                     AND bff-e-itemfg-vend.form-no = eb.form-no 
                     AND bff-e-itemfg-vend.blank-no = eb.blank-no
                     AND bff-e-itemfg-vend.vend-no EQ ""  NO-ERROR.
                
                IF AVAIL bff-e-itemfg-vend THEN do:
     
                   ASSIGN
                       bff-e-itemfg-vend.vend-no   = cVendor 
                       bff-e-itemfg-vend.vend-item = cVendorItem 
                       bff-e-itemfg-vend.std-uom   = cCostUom 
                       bff-e-itemfg-vend.run-qty[1]  = 9999999
                       bff-e-itemfg-vend.run-cost[1] = dCalValueCost
                       bff-e-itemfg-vend.setups[1]   = dCalValueSetup .  
                END.
    
                IF NOT AVAILABLE bff-e-itemfg-vend THEN DO:
    
                    DO i = 1 TO 2:
                        IF cVendor EQ "" THEN
                            ASSIGN i = 2 .
                        CREATE e-itemfg-vend .
                        ASSIGN 
                            e-itemfg-vend.company   = cocode
                            e-itemfg-vend.item-type = NO   /* for finished good */
                            e-itemfg-vend.est-no    = eb.est-no
                            e-itemfg-vend.eqty      = eb.eqty
                            e-itemfg-vend.form-no   = eb.form-no
                            e-itemfg-vend.blank-no  = eb.blank-no
                            e-itemfg-vend.i-no      = eb.stock-no .
                            
                        IF i EQ 1 THEN
                            ASSIGN
                            e-itemfg-vend.vend-no   = cVendor .
                        ASSIGN
                            e-itemfg-vend.vend-item = cVendorItem 
                            e-itemfg-vend.std-uom   = cCostUom .
                        ASSIGN
                            e-itemfg-vend.run-qty[1]  = 9999999
                            e-itemfg-vend.run-cost[1] = dCalValueCost 
                            e-itemfg-vend.setups[1]   = dCalValueSetup
                             .  
        
                        IF NOT CAN-FIND(FIRST e-itemfg OF e-itemfg-vend) THEN 
                        DO:
                            CREATE e-itemfg.
                            ASSIGN 
                                e-itemfg.company = e-itemfg-vend.company
                                e-itemfg.i-no    = e-itemfg-vend.i-no.
                        END.
                    END.
                END.
            END.  


        END. /* avail ef */
    END. /* avail eb*/
    

    RELEASE eb.
    RELEASE ef.
    RELEASE bff-e-itemfg-vend.
    RELEASE e-itemfg-vend .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignValuestToNewVendorCost D-Dialog
PROCEDURE pAssignValuestToNewVendorCost:
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/
  DEF INPUT parameter ipdCalValueCost AS DECIMAL no-undo.
  DEF INPUT PARAMETER ipdCalValueSetup AS DECIMAL NO-UNDO.
  
  FIND FIRST venditemcost exclusive-lock
                    WHERE venditemcost.company = eb.company
                      AND venditemcost.estimateNo = eb.est-no 
                      AND venditemcost.formNo = eb.form-no
                      AND venditemcost.blankNo = eb.blank-no
                      AND venditemcost.itemID = eb.stock-no
                      AND venditemcost.itemType = "FG"
                      AND venditemcost.vendorID NE ""
                      NO-ERROR.
  IF NOT AVAIL venditemcost then
     FIND FIRST venditemcost exclusive-lock
            WHERE venditemcost.company = eb.company
            AND venditemcost.estimateNo = eb.est-no 
            AND venditemcost.formNo = eb.form-no
            AND venditemcost.blankNo = eb.blank-no
            AND venditemcost.itemID = eb.stock-no
            AND venditemcost.itemType = "FG"
            AND venditemcost.vendorID eq ""
            NO-ERROR.            
    IF AVAIL venditemcost THEN 
    do:                                    
       ASSIGN venditemcost.vendorID = cVendor
              venditemcost.vendorItemID = cVendorItem
              venditemcost.vendorUOM = cCostUOM
              .                            
       FIND FIRST venditemcostLevel exclusive-lock 
            WHERE vendItemCostLevel.vendItemCostID = venditemcost.venditemcostID
            NO-ERROR.            
       IF AVAILABLE venditemcostLevel then
          ASSIGN vendItemCostLevel.quantityBase = 9999999
                 vendItemCostLevel.costPerUOM   = ipdCalValueCost
                 vendItemCostLevel.costSetup    = ipdCalValueSetup
                 .                                        
    END.
    ELSE DO:
      CREATE venditemcost.
      ASSIGN vendItemCost.Company = cocode
             vendItemCost.ItemID = eb.stock-no
             vendItemCost.itemType = "FG"                   
             venditemcost.estimateNO = eb.est-no
             venditemcost.formNo = eb.form-no
             venditemcost.blankNo = eb.blank-no
             venditemcost.vendorID = cVendor
             vendItemCost.VendorUOM = cCostUOM
             venditemcost.vendorItemID = cVendorItem
             venditemcost.effectiveDate = today
             venditemcost.expirationDate = 12/31/2099
             .
       CREATE venditemcostLevel.
       ASSIGN venditemcostLevel.venditemcostID = venditemcost.venditemcostID
              vendItemCostLevel.quantityBase = 9999999
              vendItemCostLevel.quantityTo = 9999999
              vendItemCostLevel.costPerUOM = ipdCalValueCost
              vendItemCostLevel.costSetup    = ipdCalValueSetup
              .       
    END.
            
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue D-Dialog 
PROCEDURE pDisplayValue :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
   DO WITH FRAME {&frame-name}:
      
        APPLY "entry" TO cVendor IN FRAME {&FRAME-NAME}.

        IF lButtonLabel THEN
            ASSIGN Btn_OK:LABEL = "Save" .
        quantity:VISIBLE  = NO .

        FIND FIRST eb WHERE ROWID(eb) EQ iprRowid  NO-LOCK NO-ERROR.
        IF AVAILABLE eb THEN 
            ASSIGN
                est-no:SCREEN-VALUE   = eb.est-no 
                quantity:SCREEN-VALUE = STRING(eb.eqty)
                cCustNo:SCREEN-VALUE  = eb.cust-no
                ship-to:SCREEN-VALUE  = eb.ship-id
                iForm:SCREEN-VALUE    = STRING(eb.form-no)
                iBlank:SCREEN-VALUE   = STRING(eb.blank-no)
                .
        ASSIGN
            cMatLab1:SCREEN-VALUE   = "M" 
            cMatLab2:SCREEN-VALUE   = "M" 
            cMatLab3:SCREEN-VALUE   = "M" 
            cMatLab4:SCREEN-VALUE   = "M" 
            cMatLab5:SCREEN-VALUE   = "M" 
            cCostType1:SCREEN-VALUE = "N" 
            cCostType2:SCREEN-VALUE = "N" 
            cCostType3:SCREEN-VALUE = "N" 
            cCostType4:SCREEN-VALUE = "N" 
            cCostType5:SCREEN-VALUE = "N" .

        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ eb.company
            AND cust.cust-no EQ eb.cust-no NO-ERROR .
        IF AVAILABLE cust THEN 
        DO:
            cust-name:SCREEN-VALUE = cust.NAME .
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company = eb.company
                AND shipto.cust-no EQ eb.cust-no
                AND shipto.ship-id EQ eb.ship-id  NO-ERROR .
       
            IF AVAILABLE shipto THEN
                ASSIGN ship-name:SCREEN-VALUE = shipto.ship-name .
        END.

        
        cCostUom:LIST-ITEMS IN FRAME {&frame-name} = "".
        
        RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
        ASSIGN uom-list = "EA,M" .
        DO i = 1 TO NUM-ENTRIES(uom-list):
            ilogic = cCostUom:ADD-LAST (ENTRY(i,uom-list)) IN FRAME {&frame-name}.
        END.
        cCostUom:SCREEN-VALUE = "EA"  NO-ERROR.    

        FIND FIRST ef EXCLUSIVE-LOCK
            WHERE ef.company EQ eb.company
            AND ef.est-no EQ eb.est-no
            AND ef.form-no EQ eb.form-no NO-ERROR .
        IF AVAILABLE ef THEN 
        DO:

            ASSIGN
                cItemDscr1:SCREEN-VALUE = ef.mis-cost[1] 
                cItemDscr2:SCREEN-VALUE = ef.mis-cost[2] 
                cItemDscr3:SCREEN-VALUE = ef.mis-cost[3] 
                cItemDscr4:SCREEN-VALUE = ef.mis-cost[4] 
                cItemDscr5:SCREEN-VALUE = ef.mis-cost[5]  .
            ASSIGN
               cCostType1:SCREEN-VALUE =  ef.mis-simon[1]
               cCostType2:SCREEN-VALUE =  ef.mis-simon[2]
               cCostType3:SCREEN-VALUE =  ef.mis-simon[3]
               cCostType4:SCREEN-VALUE =  ef.mis-simon[4]
               cCostType5:SCREEN-VALUE =  ef.mis-simon[5] 
               iQtyPer1:SCREEN-VALUE   = STRING(ef.misQtyPer[1])
               iQtyPer2:SCREEN-VALUE   = STRING(ef.misQtyPer[2])
               iQtyPer3:SCREEN-VALUE   = STRING(ef.misQtyPer[3])
               iQtyPer4:SCREEN-VALUE   = STRING(ef.misQtyPer[4])
               iQtyPer5:SCREEN-VALUE   = STRING(ef.misQtyPer[5]) .

           IF ef.mis-matf[1] NE 0 OR ef.mis-matm[1] NE 0 THEN DO:
              dSuCost1:SCREEN-VALUE = string(ef.mis-matf[1]).
              dEaCost1:SCREEN-VALUE = STRING( (ef.mis-matm[1] / ef.misQtyPer[1] ) / 1000) .
              cMatLab1:SCREEN-VALUE   = "M" .
           END.
           ELSE IF ef.mis-labf[1] GT 0 OR ef.mis-labm[1] NE 0 THEN DO:
              dSuCost1:SCREEN-VALUE = string(ef.mis-labf[1]).
              dEaCost1:SCREEN-VALUE = STRING( (ef.mis-labm[1] / ef.misQtyPer[1] ) / 1000) .
              cMatLab1:SCREEN-VALUE   = "L" .
           END.

           IF ef.mis-matf[2] NE 0 OR ef.mis-matm[2] NE 0 THEN DO:
              dSuCost2:SCREEN-VALUE = string(ef.mis-matf[2]).
              dEaCost2:SCREEN-VALUE = STRING( (ef.mis-matm[2] / ef.misQtyPer[2] ) / 1000) .
              cMatLab2:SCREEN-VALUE   = "M" .
           END.
           ELSE IF ef.mis-labf[2] GT 0 OR ef.mis-labm[2] NE 0 THEN DO:
              dSuCost2:SCREEN-VALUE = string(ef.mis-labf[2]).
              dEaCost2:SCREEN-VALUE = STRING( (ef.mis-labm[2] / ef.misQtyPer[2] ) / 1000) .
              cMatLab2:SCREEN-VALUE   = "L" .
           END.

           IF ef.mis-matf[3] NE 0 OR ef.mis-matm[3] NE 0 THEN DO:
              dSuCost3:SCREEN-VALUE = string(ef.mis-matf[3]).
              dEaCost3:SCREEN-VALUE = STRING( (ef.mis-matm[3] / ef.misQtyPer[3] ) / 1000) .
              cMatLab3:SCREEN-VALUE   = "M" .
           END.
           ELSE IF ef.mis-labf[3] GT 0 OR ef.mis-labm[3] NE 0 THEN DO:
              dSuCost3:SCREEN-VALUE = string(ef.mis-labf[3]).
              dEaCost3:SCREEN-VALUE = STRING( (ef.mis-labm[3] / ef.misQtyPer[3] ) / 1000) .
              cMatLab3:SCREEN-VALUE   = "L" .
           END.

           IF ef.mis-matf[4] NE 0 OR ef.mis-matm[4] NE 0 THEN DO:
              dSuCost4:SCREEN-VALUE = string(ef.mis-matf[4]).
              dEaCost4:SCREEN-VALUE = STRING( (ef.mis-matm[4] / ef.misQtyPer[4] ) / 1000) .
              cMatLab4:SCREEN-VALUE   = "M" .
           END.
           ELSE IF ef.mis-labf[4] GT 0 OR ef.mis-labm[4] NE 0 THEN DO:
              dSuCost4:SCREEN-VALUE = string(ef.mis-labf[4]).
              dEaCost4:SCREEN-VALUE = STRING( (ef.mis-labm[4] / ef.misQtyPer[4] ) / 1000) .
              cMatLab4:SCREEN-VALUE   = "L" .
           END.

           IF ef.mis-matf[5] NE 0 OR ef.mis-matm[5] NE 0 THEN DO:
              dSuCost5:SCREEN-VALUE = string(ef.mis-matf[5]).
              dEaCost5:SCREEN-VALUE = STRING( (ef.mis-matm[5] / ef.misQtyPer[5] ) / 1000) .
              cMatLab5:SCREEN-VALUE   = "M" .
           END.
           ELSE IF ef.mis-labf[5] GT 0 OR ef.mis-labm[5] NE 0 THEN DO:
              dSuCost5:SCREEN-VALUE = string(ef.mis-labf[5]).
              dEaCost5:SCREEN-VALUE = STRING( (ef.mis-labm[5] / ef.misQtyPer[5] ) / 1000) .
              cMatLab5:SCREEN-VALUE   = "L" .
           END.

            
            
           FIND FIRST bff-e-itemfg-vend NO-LOCK
                WHERE bff-e-itemfg-vend.company = eb.company 
                 AND bff-e-itemfg-vend.est-no = eb.est-no 
                 AND bff-e-itemfg-vend.eqty = eb.eqty 
                 AND bff-e-itemfg-vend.form-no = eb.form-no 
                 AND bff-e-itemfg-vend.blank-no = eb.blank-no
                 AND bff-e-itemfg-vend.vend-no NE ""  NO-ERROR .
            IF NOT AVAIL bff-e-itemfg-vend  THEN
                FIND FIRST bff-e-itemfg-vend NO-LOCK
                WHERE bff-e-itemfg-vend.company = eb.company 
                 AND bff-e-itemfg-vend.est-no = eb.est-no 
                 AND bff-e-itemfg-vend.eqty = eb.eqty 
                 AND bff-e-itemfg-vend.form-no = eb.form-no 
                 AND bff-e-itemfg-vend.blank-no = eb.blank-no
                 AND bff-e-itemfg-vend.vend-no EQ ""  NO-ERROR.

            IF AVAIL bff-e-itemfg-vend THEN
                ASSIGN
                cVendor:SCREEN-VALUE     = bff-e-itemfg-vend.vend-no   
                cVendorItem:SCREEN-VALUE = bff-e-itemfg-vend.vend-item 
                cCostUom:SCREEN-VALUE    = bff-e-itemfg-vend.std-uom   .

        END.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no D-Dialog 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF cVendor:SCREEN-VALUE NE "" AND
            NOT CAN-FIND(FIRST vend
            WHERE vend.company  EQ gcompany
            AND vend.vend-no    EQ cVendor:SCREEN-VALUE )  THEN 
        DO:
            MESSAGE "Invalid Vendor, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cVendor .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

