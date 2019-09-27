&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: viewers/dVendCostLevel.w
  
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
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO .

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

DEFINE VARIABLE uom-list         AS CHARACTER     INIT "C,CS,EA,L,M," NO-UNDO.


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
&Scoped-Define ENABLED-OBJECTS dFrom1 dToQty1 ~
dEaCost1 dSetup1 dFrom2 dToQty2 dEaCost2 dSetup2 dFrom3 dToQty3 dEaCost3 ~
dSetup3 dFrom4 dToQty4 dEaCost4 dSetup4 dFrom5 dToQty5 dEaCost5 dSetup5 ~
Btn_OK Btn_Cancel RECT-1 RECT-5 dFrom6 dFrom7 dFrom8 dFrom9 dFrom10 ~
dToQty6 dToQty7 dToQty8 dToQty9 dToQty10 dEaCost6 dEaCost7 dEaCost8 ~
dEaCost9 dEaCost10 dSetup6 dSetup7 dSetup8 dSetup9 dSetup10 dDev1 dDev2 ~
dDev3 dDev4 dDev5 dDev6 dDev7 dDev8 dDev9 dDev10 RECT-21 
&Scoped-Define DISPLAYED-OBJECTS cVendor cVendorItem cCostUom dFrom1 ~
dToQty1 dEaCost1 dSetup1 dFrom2 dToQty2 dEaCost2 dSetup2 dFrom3 dToQty3 ~
dEaCost3 dSetup3 dFrom4 dToQty4 dEaCost4 dSetup4 dFrom5 dToQty5 dEaCost5 ~
dSetup5 iForm iBlank est-no cItemNo dFrom6 dFrom7 dFrom8 dFrom9 dFrom10 ~
dToQty6 dToQty7 dToQty8 dToQty9 dToQty10 dEaCost6 dEaCost7 dEaCost8 ~
dEaCost9 dEaCost10 dSetup6 dSetup7 dSetup8 dSetup9 dSetup10 dDev1 dDev2 ~
dDev3 dDev4 dDev5 dDev6 dDev7 dDev8 dDev9 dDev10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 10 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO                                             
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 10 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cCostUom AS CHARACTER FORMAT "X(5)":U 
     LABEL "Cost/Uom" 
     VIEW-AS FILL-IN
     SIZE 10.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cItemNo AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item#" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
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

DEFINE VARIABLE dDev1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev10 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev3 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev5 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev6 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev7 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev8 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev9 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost1 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost10 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
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

DEFINE VARIABLE dEaCost6 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost7 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost8 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost9 AS DECIMAL FORMAT ">>,>>9.99<<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom10 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom5 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom6 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom7 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom8 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom9 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup10 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup4 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup5 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup6 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup7 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup8 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup9 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty1 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty10 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty4 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty5 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty6 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty7 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty8 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty9 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY 1
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 115.2 BY 3
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 115.2 BY 16.10
     BGCOLOR 15 .
DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL ROUNDED  
    SIZE 25.8 BY 2.4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cVendor AT ROW 3.38 COL 11 COLON-ALIGNED WIDGET-ID 88
     cVendorItem AT ROW 3.38 COL 48 COLON-ALIGNED WIDGET-ID 238
     cCostUom AT ROW 3.38 COL 100.8 COLON-ALIGNED WIDGET-ID 240
     dFrom1 AT ROW 5.86 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     dToQty1 AT ROW 5.86 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 254
     dEaCost1 AT ROW 5.86 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 266
     dSetup1 AT ROW 5.86 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 276
     dDev1 AT ROW 5.86 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     dFrom2 AT ROW 6.95 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 244
     dToQty2 AT ROW 6.95 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     dEaCost2 AT ROW 6.95 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 268
     dSetup2 AT ROW 6.95 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 280
     dDev2 AT ROW 6.95 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 364
     dFrom3 AT ROW 8.05 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 246
     dToQty3 AT ROW 8.05 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 258
     dEaCost3 AT ROW 8.05 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 270
     dSetup3 AT ROW 8.05 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 282
     dDev3 AT ROW 8.05 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 366
     dFrom4 AT ROW 9.19 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 248
     dToQty4 AT ROW 9.19 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 260
     dEaCost4 AT ROW 9.19 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 272
     dSetup4 AT ROW 9.19 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 284
     dDev4 AT ROW 9.19 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 368
     dFrom5 AT ROW 10.33 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 250
     dToQty5 AT ROW 10.33 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     dEaCost5 AT ROW 10.33 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 274
     dSetup5 AT ROW 10.33 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 286
     dDev5 AT ROW 10.33 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 370
     iForm AT ROW 2.14 COL 74.2 COLON-ALIGNED WIDGET-ID 314
     iBlank AT ROW 2.14 COL 91.6 COLON-ALIGNED WIDGET-ID 316
     est-no AT ROW 2.14 COL 48 COLON-ALIGNED WIDGET-ID 200
     cItemNo AT ROW 2.14 COL 11 COLON-ALIGNED WIDGET-ID 318
     dFrom6 AT ROW 11.43 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 320
     dToQty6 AT ROW 11.48 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 330
     dEaCost6 AT ROW 11.48 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     dSetup6 AT ROW 11.48 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 350
     dDev6 AT ROW 11.48 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 372

     dFrom7 AT ROW 12.52 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 322
     dToQty7 AT ROW 12.57 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 332
     dEaCost7 AT ROW 12.57 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 342
     dSetup7 AT ROW 12.57 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 352
     dDev7 AT ROW 12.57 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 374

     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     dFrom8 AT ROW 13.62 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 324
     dToQty8 AT ROW 13.67 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 334
     dEaCost8 AT ROW 13.67 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 344
     dSetup8 AT ROW 13.67 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 354
     dDev8 AT ROW 13.67 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 376
     dFrom9 AT ROW 14.76 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 326
     dToQty9 AT ROW 14.81 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 336
     dEaCost9 AT ROW 14.81 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 346
     dSetup9 AT ROW 14.81 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 356
     dDev9 AT ROW 14.81 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 378
     dFrom10 AT ROW 15.91 COL 17.2 COLON-ALIGNED NO-LABEL WIDGET-ID 328
     dToQty10 AT ROW 15.95 COL 34.8 COLON-ALIGNED NO-LABEL WIDGET-ID 338
     dEaCost10 AT ROW 15.95 COL 50.8 COLON-ALIGNED NO-LABEL WIDGET-ID 348
     dSetup10 AT ROW 15.95 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 358                
     dDev10 AT ROW 15.95 COL 83 COLON-ALIGNED NO-LABEL WIDGET-ID 362
     Btn_OK AT ROW 17.59 COL 76.2
     Btn_Cancel AT ROW 17.59 COL 86.6
     "Main Input" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 1.14 COL 6 WIDGET-ID 206
     "From" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 4.86 COL 19.2 WIDGET-ID 242
     "To" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.86 COL 36.8 WIDGET-ID 252
     "Cost" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.86 COL 52.4 WIDGET-ID 264
     "Setup" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.86 COL 69.2 WIDGET-ID 278
     "Dev" VIEW-AS TEXT
          SIZE 12.8 BY 1 AT ROW 4.86 COL 85.2 WIDGET-ID 288
     RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
     RECT-5 AT ROW 1.1 COL 1.8 WIDGET-ID 312
     RECT-21 AT ROW 17.4 COL 74
     SPACE(1.39) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Vendor Item Cost - Level "
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

/* SETTINGS FOR FILL-IN est-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBlank IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cVendor IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cVendorItem IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCostUom IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cItemNo IN FRAME D-Dialog
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
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Ok */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        /*RUN valid-vend-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY .*/
         
        SESSION:SET-WAIT-STATE("general").
  
        IF ipcType EQ "Create" THEN
            RUN pCreateValues .
        ELSE RUN pAssignValues .
        
        SESSION:SET-WAIT-STATE("").
  
        APPLY "close" TO THIS-PROCEDURE.
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCostUom
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


&Scoped-define SELF-NAME dDev1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev1 D-Dialog
ON LEAVE OF dDev1 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev10 D-Dialog
ON LEAVE OF dDev10 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev2 D-Dialog
ON LEAVE OF dDev2 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev3 D-Dialog
ON LEAVE OF dDev3 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev4 D-Dialog
ON LEAVE OF dDev4 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev5 D-Dialog
ON LEAVE OF dDev5 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev6 D-Dialog
ON LEAVE OF dDev6 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev7 D-Dialog
ON LEAVE OF dDev7 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev8 D-Dialog
ON LEAVE OF dDev8 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev9 D-Dialog
ON LEAVE OF dDev9 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom1 D-Dialog
ON LEAVE OF dFrom1 IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
         
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom10 D-Dialog
ON LEAVE OF dFrom10 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom2 D-Dialog
ON LEAVE OF dFrom2 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom3 D-Dialog
ON LEAVE OF dFrom3 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom4 D-Dialog
ON LEAVE OF dFrom4 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom5 D-Dialog
ON LEAVE OF dFrom5 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom6 D-Dialog
ON LEAVE OF dFrom6 IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
          
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom7 D-Dialog
ON LEAVE OF dFrom7 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom8 D-Dialog
ON LEAVE OF dFrom8 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom9 D-Dialog
ON LEAVE OF dFrom9 IN FRAME D-Dialog
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup1 D-Dialog
ON LEAVE OF dSetup1 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup10 D-Dialog
ON LEAVE OF dSetup10 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup2 D-Dialog
ON LEAVE OF dSetup2 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup3 D-Dialog
ON LEAVE OF dSetup3 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup4 D-Dialog
ON LEAVE OF dSetup4 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup5 D-Dialog
ON LEAVE OF dSetup5 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup6 D-Dialog
ON LEAVE OF dSetup6 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup7 D-Dialog
ON LEAVE OF dSetup7 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup8 D-Dialog
ON LEAVE OF dSetup8 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup9 D-Dialog
ON LEAVE OF dSetup9 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
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
  DISPLAY cVendor cVendorItem cCostUom dFrom1 dToQty1 dEaCost1 dSetup1 dFrom2 
          dToQty2 dEaCost2 dSetup2 dFrom3 dToQty3 dEaCost3 dSetup3 dFrom4 
          dToQty4 dEaCost4 dSetup4 dFrom5 dToQty5 dEaCost5 dSetup5 iForm 
          iBlank est-no dFrom6 dFrom7 dFrom8 dFrom9 dFrom10 dToQty6 cItemNo
          dToQty7 dToQty8 dToQty9 dToQty10 dEaCost6 dEaCost7 dEaCost8 
          dEaCost9 dEaCost10 dSetup6 dSetup7 dSetup8 dSetup9 dSetup10 dDev1 
          dDev2 dDev3 dDev4 dDev5 dDev6 dDev7 dDev8 dDev9 dDev10 
      WITH FRAME D-Dialog.
  ENABLE  dFrom1 dToQty1 dEaCost1 dSetup1 dFrom2 
         dToQty2 dEaCost2 dSetup2 dFrom3 dToQty3 dEaCost3 dSetup3 dFrom4 
         dToQty4 dEaCost4 dSetup4 dFrom5 dToQty5 dEaCost5 dSetup5 Btn_OK 
         Btn_Cancel RECT-1 RECT-5  dFrom6 dFrom7 dFrom8 dFrom9 dFrom10 
         dToQty6 dToQty7 dToQty8 dToQty9 dToQty10 dEaCost6 dEaCost7 
         dEaCost8 dEaCost9 dEaCost10 dSetup6 dSetup7 dSetup8 dSetup9 dSetup10 
         dDev1 dDev2 dDev3 dDev4 dDev5 dDev6 dDev7 dDev8 dDev9 dDev10 RECT-21
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateValues D-Dialog 
PROCEDURE pCreateValues :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-ref-rec-qty AS RECID NO-UNDO.
    DEFINE VARIABLE lv-ref-rec-cst AS RECID NO-UNDO.
    DEFINE VARIABLE dCalValueSetup AS DECIMAL NO-UNDO .
    DEFINE VARIABLE dCalValueCost AS DECIMAL NO-UNDO .
    
    FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.

    

DO WITH FRAME {&frame-name}:        
   DO i = 1 TO 10:
       
       IF i EQ 1 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom1:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty1:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost1:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup1:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev1:SCREEN-VALUE) . 
       END.
       ELSE IF i EQ 2 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom2:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty2:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost2:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup2:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev2:SCREEN-VALUE) . 
       END.
       ELSE IF i EQ 3 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom3:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty3:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost3:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup3:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev3:SCREEN-VALUE) .
       END.
       ELSE IF i EQ 4 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom4:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty4:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost4:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup4:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev4:SCREEN-VALUE) .   
       END.
       ELSE IF i EQ 5 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom5:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty5:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost5:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup5:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev5:SCREEN-VALUE) .    
       END.
       ELSE IF i EQ 6 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom6:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty6:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost6:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup6:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev6:SCREEN-VALUE) .  
       END.
       ELSE IF i EQ 7 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom7:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty7:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost7:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup7:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev7:SCREEN-VALUE) .    
       END.
       ELSE IF i EQ 8 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom8:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty8:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost8:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup8:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev8:SCREEN-VALUE) .  
       END.
       ELSE IF i EQ 9 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom9:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty9:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost9:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup9:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev9:SCREEN-VALUE) .   
       END.
       ELSE IF i EQ 10 THEN do:
           CREATE vendItemCostLevel .
           ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom10:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty10:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost10:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup10:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev10:SCREEN-VALUE) .    
       END.

   END.
        
END.

    RELEASE vendItemCostLevel.
    
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
    
    FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
DO WITH FRAME {&frame-name}:   
    i = 1 .
    FOR EACH vendItemCostLevel EXCLUSIVE-LOCK
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
            BY vendItemCostLevel.vendItemCostLevelID :
  
       IF i EQ 1 THEN do:

           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom1:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty1:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost1:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup1:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev1:SCREEN-VALUE) . 
       END.
       ELSE IF i EQ 2 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom2:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty2:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost2:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup2:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev2:SCREEN-VALUE) . 
       END.
       ELSE IF  i EQ 3 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom3:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty3:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost3:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup3:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev3:SCREEN-VALUE) .
       END.
       ELSE IF  i EQ 4 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom4:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty4:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost4:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup4:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev4:SCREEN-VALUE) .   
       END.
       ELSE IF  i EQ 5 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom5:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty5:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost5:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup5:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev5:SCREEN-VALUE) .    
       END.
       ELSE IF  i EQ 6 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom6:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty6:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost6:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup6:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev6:SCREEN-VALUE) .  
       END.
       ELSE IF  i EQ 7 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom7:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty7:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost7:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup7:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev7:SCREEN-VALUE) .    
       END.
       ELSE IF  i EQ 8 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom8:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty8:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost8:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup8:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev8:SCREEN-VALUE) .  
       END.
       ELSE IF  i EQ 9 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom9:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty9:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost9:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup9:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev9:SCREEN-VALUE) .   
       END.
       ELSE IF  i EQ 10 THEN do:
           ASSIGN 
           vendItemCostLevel.quantityFrom  = decimal(dFrom10:SCREEN-VALUE)   
           vendItemCostLevel.quantityTo    = decimal(dToQty10:SCREEN-VALUE)  
           vendItemCostLevel.costPerUOM    = decimal(dEaCost10:SCREEN-VALUE) 
           vendItemCostLevel.costSetup     = decimal(dSetup10:SCREEN-VALUE)
           vendItemCostLevel.costDeviation = decimal(dDev10:SCREEN-VALUE) .    
       END.
       i = i + 1 .

   END.
END.

    RELEASE vendItemCostLevel.
    
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
      
        APPLY "entry" TO dFrom1 IN FRAME {&FRAME-NAME}.

        
            ASSIGN Btn_OK:LABEL = "Save" .
        

        FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
        IF AVAILABLE vendItemCost THEN 
            ASSIGN
                est-no:SCREEN-VALUE   = vendItemCost.estimateNo 
                cItemNo:SCREEN-VALUE = STRING(vendItemCost.itemID)
                iForm:SCREEN-VALUE    = STRING(vendItemCost.formNo)
                iBlank:SCREEN-VALUE   = STRING(vendItemCost.blankNo)
                cVendor:SCREEN-VALUE     = vendItemCost.vendorID
                cVendorItem:SCREEN-VALUE = vendItemCost.vendorItemID
                cCostUom:SCREEN-VALUE    = vendItemCost.vendorUOM .
         
        i = 1 .
        FOR EACH vendItemCostLevel NO-LOCK
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
            BY vendItemCostLevel.vendItemCostLevelID:

           IF i EQ 1 THEN
                ASSIGN 
                dFrom1:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)
                dToQty1:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)
                dEaCost1:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)
                dSetup1:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)
                dDev1:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) .
            ELSE IF i EQ 2 THEN
                ASSIGN 
                dFrom2:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty2:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost2:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup2:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev2:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 3 THEN
                ASSIGN 
                dFrom3:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty3:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost3:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup3:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev3:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 4 THEN
                ASSIGN 
                dFrom4:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty4:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost4:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup4:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev4:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 5 THEN
                ASSIGN 
                dFrom5:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty5:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost5:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup5:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev5:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF  i EQ 6 THEN
                ASSIGN 
                dFrom6:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty6:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost6:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup6:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev6:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 7 THEN
                ASSIGN 
                dFrom7:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty7:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost7:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup7:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev7:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 8 THEN
                ASSIGN 
                dFrom8:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty8:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost8:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup8:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev8:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 9 THEN
                ASSIGN 
                dFrom9:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty9:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost9:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup9:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev9:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            ELSE IF i EQ 10 THEN
                ASSIGN 
                dFrom10:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)    
                dToQty10:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)      
                dEaCost10:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)      
                dSetup10:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)       
                dDev10:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) . 
            i = i + 1 .
            IF i GT 10 THEN LEAVE .
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

