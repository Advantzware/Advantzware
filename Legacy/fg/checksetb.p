&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT        PARAM ip-rowid1    AS   ROWID              NO-UNDO.
DEF INPUT        PARAM ip-rowid2    AS   ROWID              NO-UNDO.
DEF INPUT        PARAM ip-job-no    LIKE fg-rctd.job-no     NO-UNDO.
DEF INPUT        PARAM ip-job-no2   LIKE fg-rctd.job-no2    NO-UNDO.
DEF INPUT        PARAM ipcLoc       LIKE itemfg.loc         NO-UNDO.
DEF INPUT-OUTPUT PARAM io-set-qty   AS   INT                NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-fg-rctd FOR fg-rctd.
DEF BUFFER b2-fg-rctd FOR fg-rctd.
DEF BUFFER use-job FOR reftable.

DEF VAR v-set           LIKE itemfg.i-no NO-UNDO.
DEF VAR v-comp          LIKE itemfg.i-no NO-UNDO.
DEF VAR lv-q-onh        LIKE itemfg.q-onh NO-UNDO.
DEF VAR lv-q-alloc      LIKE itemfg.q-alloc NO-UNDO.
DEF VAR lv-partset      AS   INT NO-UNDO.
DEF VAR v-set-use       AS   INT NO-UNDO.
DEF VAR v-max-qty       AS   INT NO-UNDO.

{fg/fullset.i NEW}

DEF TEMP-TABLE tt-set NO-UNDO
                      FIELD comp LIKE itemfg.i-no
                      FIELD alloc AS INT 
                      FIELD onhand AS INT
                      FIELD setqty AS INT.

DO TRANSACTION:
  {sys/inc/fgsetrec.i}
END.

DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lGetBin AS LOGICAL     NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "L",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "FGSetAssembly",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT cFGSetAssembly,
                       OUTPUT lFound).
DEF VAR tb_use-job AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN main-procedure.
  io-set-qty = v-max-qty.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-checkset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkset Procedure 
PROCEDURE checkset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-int AS INT NO-UNDO.

  v-max-qty = io-set-qty.

  RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH tt-fg-set,
      FIRST b-itemfg
      WHERE b-itemfg.company EQ itemfg.company
        AND b-itemfg.i-no    EQ tt-fg-set.part-no
        AND b-itemfg.i-no    NE itemfg.i-no
      NO-LOCK:  

    ASSIGN
     lv-q-onh   = 0
     lv-q-alloc = 0.
    FOR EACH fg-bin FIELDS(qty)
        WHERE fg-bin.company   EQ b-itemfg.company
          AND fg-bin.i-no      EQ b-itemfg.i-no
/*           AND ((fg-bin.job-no  EQ ip-job-no AND  */
/*                 fg-bin.job-no2 EQ ip-job-no2) OR */
/*                NOT tb_use-job)                   */
          AND (IF lFGSetAssembly  THEN fg-bin.loc EQ ipcLoc ELSE TRUE)
          AND (IF lFGSetAssembly  THEN fg-bin.loc-bin EQ cFGSetAssembly ELSE TRUE) 
        NO-LOCK:
      
      lv-q-onh = lv-q-onh + fg-bin.qty.
    END.

    IF itemfg.alloc NE YES THEN
      lv-q-alloc = b-itemfg.q-alloc + (IF PROGRAM-NAME(2) BEGINS "oe/oe-ordlu." THEN
                                         (io-set-qty * tt-fg-set.part-qty-dec)
                                       ELSE 0).

    IF ip-rowid1 NE ? THEN
    FOR EACH b-fg-rctd FIELDS(t-qty r-no)
        WHERE b-fg-rctd.company   EQ cocode   
          AND b-fg-rctd.i-no      EQ b-itemfg.i-no
          AND b-fg-rctd.rita-code EQ "R"
          AND ((b-fg-rctd.job-no  EQ ip-job-no AND
                b-fg-rctd.job-no2 EQ ip-job-no2) OR
               NOT tb_use-job)
          AND (IF lFGSetAssembly THEN b-fg-rctd.loc EQ ipcLoc ELSE TRUE)
          AND (IF lFGSetAssembly THEN b-fg-rctd.loc-bin EQ cFGSetAssembly ELSE TRUE)
          NO-LOCK:



          IF fg-rctd.created-by NE "" THEN
          DO:
           v-int = b-fg-rctd.r-no.
           IF NOT CAN-FIND(FIRST b2-fg-rctd WHERE
             b2-fg-rctd.r-no EQ v-int AND
             b2-fg-rctd.rita-code NE "P"
             AND ROWID(b2-fg-rctd)    NE ip-rowid2) THEN
             NEXT.
          END.   
          
      /* v-set-use already includes this number, unless there is another */
      /* positive receipt for the component separate from the set header */
      IF b-fg-rctd.t-qty GT 0 THEN
        lv-q-onh = lv-q-onh + b-fg-rctd.t-qty.

    END.
    ASSIGN
     lv-q-onh   = lv-q-onh - (v-set-use * tt-fg-set.part-qty-dec)
     lv-partset = TRUNC(lv-q-onh / tt-fg-set.part-qty-dec,0).
     
    IF (io-set-qty * tt-fg-set.part-qty-dec) GT lv-q-onh THEN DO:
      IF ip-rowid1 NE ? THEN DO:
        CREATE tt-set.
        ASSIGN
         tt-set.comp   = b-itemfg.i-no
         tt-set.onhand = lv-q-onh
         tt-set.alloc  = lv-q-alloc
         tt-set.setqty = lv-partset.
      END.
      
      IF lv-partset LT v-max-qty THEN v-max-qty = lv-partset.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-main-procedure) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main-procedure Procedure 
PROCEDURE main-procedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid2 EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL fg-rctd THEN DO:
    ASSIGN fg-rctd.use-job = fgsetrec-log.

    IF ip-rowid1 EQ ? THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company EQ fg-rctd.company
            AND itemfg.i-no    EQ fg-rctd.i-no
          NO-LOCK NO-ERROR.
      tb_use-job = fgsetrec-log.
    END.

    ELSE DO:
      FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid1 NO-LOCK NO-ERROR.
      tb_use-job = fgsetrec-log.
    END.
    RELEASE fg-rctd.
  END.

  IF AVAIL itemfg THEN DO:
    v-set = itemfg.i-no.
   
    FOR EACH b-fg-rctd FIELDS(t-qty)
        WHERE b-fg-rctd.company   EQ cocode   
          AND b-fg-rctd.i-no      EQ itemfg.i-no
          AND b-fg-rctd.rita-code EQ "R"
          AND ROWID(b-fg-rctd)    NE ip-rowid2
        NO-LOCK:
      v-set-use = v-set-use + b-fg-rctd.t-qty.
    END.

    IF itemfg.alloc EQ YES THEN
    FOR EACH fg-bin FIELDS(qty)
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.job-no  EQ ip-job-no
          AND fg-bin.job-no2 EQ ip-job-no2
        NO-LOCK:
      v-set-use = v-set-use + fg-bin.qty.
    END.

    /* IF itemfg.alloc EQ YES THEN tb_use-job = YES. */

    RUN checkset.

/*     IF CAN-FIND(FIRST tt-set) THEN DO:                                                              */
/*       FRAME {&FRAME-NAME}:TITLE = "Set: " + TRIM(CAPS(v-set)) +                                     */
/*                                   " / Components" + " " +                                           */
/*                                   (IF ip-job-no NE "" THEN                                          */
/*                                      "For Job#: " + TRIM(ip-job-no) + "-" + STRING(ip-job-no2,"99") */
/*                                    ELSE "").                                                        */
/*       RUN enable_UI.                                                                                */
/*                                                                                                     */
/*       IF itemfg.alloc EQ YES OR fgsetrec-log EQ NO THEN                                             */
/*       DO WITH FRAME {&FRAME-NAME}:                                                                  */
/*         tb_use-job:HIDDEN = YES.                                                                    */
/*       END.                                                                                          */
/*                                                                                                     */
/*       WAIT-FOR GO OF FRAME {&FRAME-NAME}.                                                           */
/*     END. */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recheckset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recheckset Procedure 
PROCEDURE recheckset :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE tt-set.

  RUN checkset.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

