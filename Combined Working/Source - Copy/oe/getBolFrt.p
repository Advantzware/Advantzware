&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe\getBolFrt.p
    Purpose     : Get or set BOL freight per line or entire BOL

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM ip-rowid       AS   ROWID                  NO-UNDO.
DEF INPUT  PARAM ip-cust-no     LIKE oe-bolh.cust-no        NO-UNDO.
DEF INPUT  PARAM ip-ship-id     LIKE oe-bolh.ship-id        NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE oe-bolh.carrier        NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.

DEF BUFFER b-oe-boll FOR oe-boll.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.

DEF TEMP-TABLE tt-bollfreight NO-UNDO
FIELD freight-basis AS DEC DECIMALS 10
FIELD boll-row AS ROWID .

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
    
RUN main-process.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getBolhFreight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBolhFreight Procedure 
PROCEDURE getBolhFreight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opdFreight AS DEC DECIMALS 10.
DEF OUTPUT PARAMETER opdMinRate AS DEC DECIMALS 10.

DEF VAR dLineFreight AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dLineMin     AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBOLFreight  AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBOLMin      AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBasis       AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTotalBasis  AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTotFreight  AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dFreight     AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTotBasis    AS DEC DECIMALS 10 NO-UNDO.
DEF VAR ldMinRate AS DEC DECIMALS 10.
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-oe-bolh FOR oe-bolh.
DEF VAR dAccumFreight AS DECIMAL DECIMALS 10 NO-UNDO.

/* dAccumFreight = 0.                                                                                   */
/* FIND FIRST bf-oe-bolh WHERE ROWID(bf-oe-bolh) EQ iprRowid NO-LOCK NO-ERROR.                          */
/* FOR EACH bf-oe-boll                                                                                  */
/*     WHERE bf-oe-boll.company EQ bf-oe-bolh.company                                                   */
/*       AND bf-oe-boll.b-no EQ bf-oe-bolh.b-no                                                         */
/*     NO-LOCK:                                                                                         */
/*   RUN getBollFreight (INPUT ROWID(bf-oe-boll), OUTPUT dLineFreight, OUTPUT dLineMin, OUTPUT dBasis). */
/*                                                                                                      */
/*                                                                                                      */
/*   dAccumFreight = dAccumFreight + dLineFreight.                                                      */
/* END.                                                                                                 */
/* IF dLineMin GT dAccumFreight THEN                                                                    */
/*   dAccumFreight = dLineMin.                                                                          */
/*                                                                                                      */
/* opdFreight = dAccumFreight.                                                                          */
/* opdMinRate = dLineMin.                                                                               */

dTotFreight = 0.        
tot-other-freight = 0.

FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
   NO-LOCK:
  RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
              INPUT oe-bolh.cust-no,
              INPUT oe-bolh.ship-id,
              INPUT oe-bolh.carrier,
              INPUT rowid(oe-boll), 
              OUTPUT v-other-freight). 
  tot-other-freight = tot-other-freight + v-other-freight.
END.

FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no
    NO-LOCK:

  FIND FIRST shipto
        WHERE shipto.company EQ oe-bolh.company
          AND shipto.cust-no EQ oe-bolh.cust-no
          AND shipto.ship-id EQ oe-bolh.ship-id
        NO-LOCK NO-ERROR.
  IF NOT AVAIL shipto THEN
    NEXT.

  RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
              INPUT oe-bolh.cust-no,
              INPUT oe-bolh.ship-id,
              INPUT oe-bolh.carrier,
              INPUT rowid(oe-boll), 
              OUTPUT v-other-freight). 

  RUN oe/getLineFrt.p (oe-bolh.company, 
                       shipto.loc, 
                       oe-bolh.carrier, 
                       shipto.dest-code,
                       shipto.ship-zip, 
                       v-other-freight, 
                       tot-other-freight, 
                       1, 
                       OUTPUT dFreight, 
                       OUTPUT ldMinRate).
 
  dTotFreight = dTotFreight + dFreight.
  dTotBasis = dTotBasis + v-other-freight.
 
END. /* each oe-boll */        

IF dTotFreight LT ldMinRate THEN DO:
  dTotFreight = ldMinRate.
END.
opdFreight = dTotFreight.
opdMinRate = ldMinRate.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBollFreight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBollFreight Procedure 
PROCEDURE getBollFreight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opdFreight AS DEC DECIMALS 10.
DEF OUTPUT PARAMETER opdMinRate AS DEC DECIMALS 10.
DEF OUTPUT PARAMETER opdBasis   AS DEC DECIMALS 10.
DEF VAR dFreight AS DECIMAL DECIMALS 10 NO-UNDO.
DEF VAR dTotFreight AS DECIMAL DECIMALS 10 NO-UNDO.
DEF VAR ldMinRate AS DEC DECIMALS 10.
DEF BUFFER bf-oe-boll FOR oe-boll.

tot-other-freight = 0.
FIND FIRST bf-oe-boll WHERE ROWID(bf-oe-boll) EQ iprRowid NO-LOCK NO-ERROR.

/* wfk- This loop originally only considered line with the same i-no, */
/* but I believe the weight of the entire BOL should be considered    */
/* since they are all on the same BOL                                 */
IF AVAIL bf-oe-boll THEN DO:
  FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ bf-oe-boll.b-no NO-LOCK NO-ERROR.


  /* Obtain full basis weight (or pallets) to calculate freight */
  tot-other-freight = 0.
  FOR EACH b-oe-boll WHERE b-oe-boll.company = bf-oe-boll.company
                       AND b-oe-boll.bol-no  = bf-oe-boll.bol-no
                      /* AND b-oe-boll.i-no    = bf-oe-boll.i-no */
                     NO-LOCK.
     RUN other-lines (INPUT rowid(b-oe-boll), OUTPUT v-other-freight).
     /* total weight of all lines on BOL for this item */
     tot-other-freight = tot-other-freight + v-other-freight.
  END.


  IF AVAIL oe-bolh THEN
  FIND FIRST shipto
        WHERE shipto.company EQ oe-bolh.company
          AND shipto.cust-no EQ ip-cust-no
          AND shipto.ship-id EQ ip-ship-id
        NO-LOCK NO-ERROR.
        
  IF AVAIL shipto THEN
  FIND FIRST carrier
      WHERE carrier.company EQ shipto.company
        AND carrier.loc     EQ shipto.loc
        AND carrier.carrier EQ ip-carrier
      NO-LOCK NO-ERROR.

  v-del-zone = shipto.dest-code.


  /* Obtain freight for entire bol to compare with minimum */
  FOR EACH b-oe-boll WHERE b-oe-boll.company = bf-oe-boll.company
                       AND b-oe-boll.bol-no  = bf-oe-boll.bol-no
                      /* AND b-oe-boll.i-no    = bf-oe-boll.i-no */
                     NO-LOCK.
    /* Indicates to always copy BOL freight class from item */
    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "BOLFreight" 
                        no-lock no-error.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
      FIND FIRST itemfg WHERE itemfg.company EQ b-oe-boll.company
          AND itemfg.i-no EQ b-oe-boll.i-no 
        NO-LOCK NO-ERROR.
      IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                              AND carr-mtx.loc      EQ shipto.loc
                              AND carr-mtx.carrier  EQ ip-carrier
                              AND carr-mtx.del-zone EQ itemfg.frt-class
                            NO-LOCK NO-ERROR.
        IF AVAIL carr-mtx THEN
          ASSIGN v-del-zone = itemfg.frt-class.    
      END.
    END. /* if fgfreightclass */

    RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                INPUT oe-bolh.cust-no,
                INPUT oe-bolh.ship-id,
                INPUT oe-bolh.carrier,
                INPUT rowid(b-oe-boll), 
                OUTPUT v-other-freight). 

    RUN oe/getLineFrt.p (oe-bolh.company, 
                         shipto.loc, 
                         oe-bolh.carrier, 
                         v-del-zone,
                         shipto.ship-zip, 
                         v-other-freight, 
                         tot-other-freight, 
                         1, 
                         OUTPUT dFreight, 
                         OUTPUT ldMinRate).

 
    dTotFreight = dTotFreight + dFreight.
  END. /* each b-oe-boll */

 
  FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ bf-oe-boll.b-no NO-LOCK NO-ERROR.
                    
  IF AVAIL oe-bolh THEN
  FIND FIRST shipto
        WHERE shipto.company EQ oe-bolh.company
          AND shipto.cust-no EQ ip-cust-no
          AND shipto.ship-id EQ ip-ship-id
        NO-LOCK NO-ERROR.
        
  IF AVAIL shipto THEN
  FIND FIRST carrier
      WHERE carrier.company EQ shipto.company
        AND carrier.loc     EQ shipto.loc
        AND carrier.carrier EQ ip-carrier
      NO-LOCK NO-ERROR.
  
  IF AVAIL carrier THEN
  FOR EACH itemfg
     WHERE itemfg.company EQ bf-oe-boll.company
       AND itemfg.i-no    EQ bf-oe-boll.i-no
     NO-LOCK:
  
    RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                INPUT oe-bolh.cust-no,
                INPUT oe-bolh.ship-id,
                INPUT oe-bolh.carrier,
                INPUT rowid(bf-oe-boll), 
                OUTPUT v-other-freight). 
  
    v-del-zone = shipto.dest-code.
  
    /* Indicates to always copy BOL freight class from item */
    find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "BOLFreight" 
                        no-lock no-error.
    IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
      IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                              AND carr-mtx.loc      EQ shipto.loc
                              AND carr-mtx.carrier  EQ ip-carrier
                              AND carr-mtx.del-zone EQ itemfg.frt-class
                            NO-LOCK NO-ERROR.
        IF AVAIL carr-mtx THEN
          ASSIGN v-del-zone = itemfg.frt-class.    
      END.
    END. /* if fgfreightclass */
    
    opdBasis = v-frt-chg.
  
    /* Get freight for current BOL line */
    RUN getfrate (shipto.loc, ip-carrier, v-del-zone,
                  shipto.ship-zip, v-other-freight, tot-other-freight, 1, 
                  OUTPUT v-frt-chg, OUTPUT ldMinRate).

    IF dTotFreight LT ldMinRate THEN DO:
              dTotFreight = ldMinRate.
              RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                        INPUT oe-bolh.cust-no,
                        INPUT oe-bolh.ship-id,
                        INPUT oe-bolh.carrier,
                        INPUT rowid(bf-oe-boll), 
                        OUTPUT v-other-freight).
              /* Freight for this oe-boll is the total freight * (basis-weight / total basis weight) */
              v-frt-chg = dTotFreight * v-other-freight / tot-other-freight.
    END.
    op-freight = v-frt-chg.
  
  
  
    LEAVE.
  END. /* each itemfg */
  
  opdFreight = op-freight.
  opdMinRate = ldMinRate.
END. /* if avail bf-oe-boll */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getfrate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getfrate Procedure 
PROCEDURE getfrate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       If there is a minimum, returns it in op-min so the caller
               can deal with it 
------------------------------------------------------------------------------*/

DEF INPUT  PARAM ip-loc         LIKE loc.loc                NO-UNDO.
DEF INPUT  PARAM ip-carrier     LIKE carrier.carrier        NO-UNDO.
DEF INPUT  PARAM ip-del-zone    LIKE carr-mtx.del-zone      NO-UNDO.
DEF INPUT  PARAM ip-zip-code    LIKE shipto.ship-zip        NO-UNDO.
DEF INPUT  PARAM ip-qty         AS   DEC DECIMALS 10        NO-UNDO.
DEF INPUT  PARAM ip-tot-qty         AS   DEC DECIMALS 10        NO-UNDO.
DEF INPUT  PARAM ip-rels        AS   INT                    NO-UNDO.
DEF OUTPUT PARAM op-freight     AS   DEC DECIMALS 10        NO-UNDO.
DEF OUTPUT PARAM op-min         AS   DEC DECIMALS 10        NO-UNDO.
DEF VAR v-zip-code AS CHAR NO-UNDO.

/* {custom/globdefs.i}        */
/*                            */
/* {sys/inc/var.i NEW SHARED} */

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR li AS INT NO-UNDO.
FIND FIRST carr-mtx
    WHERE carr-mtx.company  EQ cocode
      AND carr-mtx.loc      EQ ip-loc
      AND carr-mtx.carrier  EQ ip-carrier
      AND carr-mtx.del-zone EQ ip-del-zone
      AND carr-mtx.del-zip  EQ ip-zip-code
    NO-LOCK NO-ERROR.
IF AVAIL carr-mtx THEN
  v-zip-code = ip-zip-code.
ELSE
  v-zip-code = "".

FOR FIRST carr-mtx
    WHERE carr-mtx.company  EQ cocode
      AND carr-mtx.loc      EQ ip-loc
      AND carr-mtx.carrier  EQ ip-carrier
      AND carr-mtx.del-zone EQ ip-del-zone
      AND (IF v-zip-code GT "" THEN carr-mtx.del-zip = v-zip-code
                               ELSE TRUE)
    NO-LOCK,

    FIRST carrier OF carr-mtx NO-LOCK:

  DO li = 1 TO 10:
    op-freight = carr-mtx.rate[li] * ip-qty.
    IF carr-mtx.weight[li] GE ip-tot-qty THEN LEAVE.
  END.

  IF carrier.chg-method EQ "W" THEN op-freight = op-freight / 100.

  IF op-freight LT carr-mtx.min-rate THEN op-min = carr-mtx.min-rate.
      
  /* op-freight = op-freight + (carr-mtx.min-rate * (ip-rels - 1)). */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-main-process) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main-process Procedure 
PROCEDURE main-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR dLineFreight AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTLineFreight AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dLineMin     AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBOLFreight  AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBOLMin      AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTbolMin     AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dBasis       AS DEC DECIMALS 10 NO-UNDO.
DEF VAR dTotalBasis  AS DEC DECIMALS 10 NO-UNDO.

FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL oe-boll THEN
    FIND FIRST oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL oe-boll THEN DO:
    RUN getBollFreight (INPUT ROWID(oe-boll), OUTPUT dLineFreight, OUTPUT dLineMin, OUTPUT dBasis).
/*     IF dLineMin GT 0 THEN DO:                                                                                   */
/*         IF AVAIL oe-boll THEN                                                                                   */
/*           FIND FIRST oe-bolh WHERE                                                                              */
/*               oe-bolh.b-no EQ oe-boll.b-no                                                                      */
/*               NO-LOCK NO-ERROR.                                                                                 */
/*         IF AVAIL oe-bolh THEN DO:                                                                               */
/*             /* Create a tt-bollfreight for each line of PO so can be */                                         */
/*             /* assigned to the lines in the right proportion         */                                         */
/*             dTotalBasis = 0.                                                                                    */
/*             FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company                                           */
/*                 AND oe-boll.b-no EQ oe-bolh.b-no                                                                */
/*                 NO-LOCK.                                                                                        */
/*                RUN getBollFreight (INPUT ROWID(oe-boll), OUTPUT dTLineFreight, OUTPUT dLineMin, OUTPUT dBasis). */
/*                dTotalBasis = dTotalBasis + dTLineFreight.                                                       */
/*                                                                                                                 */
/*             END.                                                                                                */
/*                                                                                                                 */
/*             /* If line item total is more than the minimum, can disregard the minimum */                        */
/*             IF dTotalBasis GT dLineMin THEN                                                                     */
/*               dLineMin = 0.                                                                                     */
/*         END.                                                                                                    */
/*     END.                                                                                                        */
/*                                                                                                                 */
/*     IF dLineMin GT 0 THEN DO:                                                                                   */
/*                                                                                                                 */
/*         /* If the minimum is used, can't just take the value for the line */                                    */
/*         /* have to get the value for entire BOL and prorate per line */                                         */
/*         /* First store the value for each line so we can get right proportion */                                */
/*         /* for the given line                                                 */                                */
/*         IF AVAIL oe-boll THEN                                                                                   */
/*           FIND FIRST oe-bolh WHERE                                                                              */
/*               oe-bolh.b-no EQ oe-boll.b-no                                                                      */
/*               NO-LOCK NO-ERROR.                                                                                 */
/*         IF AVAIL oe-bolh THEN DO:                                                                               */
/*             /* Create a tt-bollfreight for each line of PO so can be */                                         */
/*             /* assigned to the lines in the right proportion         */                                         */
/*             dTotalBasis = 0.                                                                                    */
/*             FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company                                           */
/*                 AND oe-boll.b-no EQ oe-bolh.b-no                                                                */
/*                 NO-LOCK.                                                                                        */
/*                RUN getBollFreight (INPUT ROWID(oe-boll), OUTPUT dLineFreight, OUTPUT dLineMin, OUTPUT dBasis).  */
/*                dTotalBasis = dTotalBasis + dBasis.                                                              */
/*                CREATE tt-bollfreight.                                                                           */
/*                ASSIGN tt-bollfreight.freight-basis = dBasis                                                     */
/*                       tt-bollfreight.boll-row = ROWID(oe-boll).                                                 */
/*             END.                                                                                                */
/*                                                                                                                 */
/*             /* Now get the total freight for the BOL */                                                         */
/*             RUN getBolhFreight (INPUT ROWID(oe-bolh), OUTPUT dBOLFreight, OUTPUT dBOLMin).                      */
/*                                                                                                                 */
/*             /* Now divide by the total to get the right proportion */                                           */
/*             FIND FIRST tt-bollfreight WHERE tt-bollfreight.boll-row = ip-rowid                                  */
/*                 NO-ERROR.                                                                                       */
/*             IF AVAIL tt-bollfreight THEN                                                                        */
/*               op-freight = dBolFreight * (tt-bollfreight.freight-basis / dTotalBasis).                          */
/*         END. /* if avail oe-bolh */                                                                             */
/*     END. /* If minimum rate is used */                                                                          */
/*     ELSE                                                                                                        */
       op-freight = dLineFreight.                                                                              
END. /* If rowid of oe-boll was given */                                                                        
ELSE IF AVAIL oe-bolh THEN DO:   
    RUN getBolhFreight (INPUT ROWID(oe-bolh), OUTPUT dBOLFreight, OUTPUT dBOLMin).
    op-freight = dBolFreight.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-other-lines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE other-lines Procedure 
PROCEDURE other-lines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER v-oe-boll AS ROWID.
DEF OUTPUT PARAMETER v-out-frt AS DEC NO-UNDO.
DEF BUFFER bf-oe-boll FOR oe-boll.
FIND bf-oe-boll WHERE ROWID(bf-oe-boll) = v-oe-boll NO-LOCK.
FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ bf-oe-boll.b-no NO-LOCK NO-ERROR.
                  
IF AVAIL oe-bolh THEN
FIND FIRST shipto
      WHERE shipto.company EQ oe-bolh.company
        AND shipto.cust-no EQ ip-cust-no
        AND shipto.ship-id EQ ip-ship-id
      NO-LOCK NO-ERROR.
      
IF AVAIL shipto THEN
FIND FIRST carrier
    WHERE carrier.company EQ shipto.company
      AND carrier.loc     EQ shipto.loc
      AND carrier.carrier EQ ip-carrier
    NO-LOCK NO-ERROR.

IF AVAIL carrier THEN
FOR EACH itemfg
   WHERE itemfg.company EQ bf-oe-boll.company
     AND itemfg.i-no    EQ bf-oe-boll.i-no
   NO-LOCK:

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ bf-oe-boll.i-no
        AND fg-bin.tag     EQ bf-oe-boll.tag
        AND fg-bin.loc     EQ bf-oe-boll.loc
        AND fg-bin.loc-bin EQ bf-oe-boll.loc-bin
        AND fg-bin.job-no  EQ bf-oe-boll.job-no
        AND fg-bin.job-no2 EQ bf-oe-boll.job-no2
      NO-LOCK NO-ERROR.

  CASE carrier.chg-method:
    WHEN "W" THEN DO:                                     /* Weight in Lbs */
      v-out-frt = IF bf-oe-boll.weight NE 0 THEN bf-oe-boll.weight
                  ELSE (itemfg.weight-100 * bf-oe-boll.qty / 100).
    END.

    WHEN "P" THEN DO:                                     /* # of Pallets */
      RUN oe/pallcalc.p (ROWID(bf-oe-boll), OUTPUT v-pallets).
      v-out-frt = v-pallets.                        
    END.

    OTHERWISE DO:                                         /* MSF */
      v-out-frt = itemfg.t-sqft * bf-oe-boll.qty / 1000.
    END.
  END CASE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

