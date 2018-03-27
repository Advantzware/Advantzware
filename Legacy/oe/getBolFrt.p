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
DEFINE INPUT  PARAMETER ip-rowid       AS   ROWID                  NO-UNDO.
DEFINE INPUT  PARAMETER ip-cust-no     LIKE oe-bolh.cust-no        NO-UNDO.
DEFINE INPUT  PARAMETER ip-ship-id     LIKE oe-bolh.ship-id        NO-UNDO.
DEFINE INPUT  PARAMETER ip-carrier     LIKE oe-bolh.carrier        NO-UNDO.
DEFINE OUTPUT PARAMETER op-freight     AS   DECIMAL DECIMALS 10        NO-UNDO.

DEFINE BUFFER b-oe-boll FOR oe-boll.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE v-pallets         AS INTEGER NO-UNDO.
DEFINE VARIABLE v-qty-pal         AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-frt-chg         AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-del-zone        LIKE oe-ordl.del-zone NO-UNDO.
DEFINE VARIABLE v-other-freight   AS DECIMAL NO-UNDO DECIMALS 10.
DEFINE VARIABLE tot-other-freight AS DECIMAL NO-UNDO DECIMALS 10.

DEFINE TEMP-TABLE tt-bollfreight NO-UNDO
    FIELD freight-basis AS DECIMAL   DECIMALS 10
    FIELD boll-row      AS ROWID .

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
    DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreight AS DECIMAL DECIMALS 10.
    DEFINE OUTPUT PARAMETER opdMinRate AS DECIMAL DECIMALS 10.

    DEFINE VARIABLE dLineFreight AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dLineMin     AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBOLFreight  AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBOLMin      AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBasis       AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTotalBasis  AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTotFreight  AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dFreight     AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTotBasis    AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE cCustID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ldMinRate    AS DECIMAL DECIMALS 10.
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    DEFINE VARIABLE dAccumFreight AS DECIMAL DECIMALS 10 NO-UNDO.


    dTotFreight = 0.        
    tot-other-freight = 0.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        NO-LOCK:
        cCustID = oe-bolh.cust-no.
        RUN pGetCustID(BUFFER oe-boll, INPUT-OUTPUT cCustID).
        RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
            INPUT cCustID,
            INPUT oe-bolh.ship-id,
            INPUT oe-bolh.carrier,
            INPUT ROWID(oe-boll), 
            OUTPUT v-other-freight). 
        tot-other-freight = tot-other-freight + v-other-freight.
    END.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        NO-LOCK:
        cCustID = oe-bolh.cust-no.
        RUN pGetCustID(BUFFER oe-boll, INPUT-OUTPUT cCustID).
               
        FIND FIRST shipto
            WHERE shipto.company EQ oe-bolh.company
            AND shipto.cust-no EQ cCustID
            AND shipto.ship-id EQ oe-bolh.ship-id
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE shipto THEN
            NEXT.

        RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
            INPUT cCustID,
            INPUT oe-bolh.ship-id,
            INPUT oe-bolh.carrier,
            INPUT ROWID(oe-boll), 
            OUTPUT v-other-freight). 

        RUN oe/getLineFrt.p (oe-bolh.company, 
            oe-boll.loc, 
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

    IF dTotFreight LT ldMinRate THEN 
    DO:
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
    DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreight AS DECIMAL DECIMALS 10.
    DEFINE OUTPUT PARAMETER opdMinRate AS DECIMAL DECIMALS 10.
    DEFINE OUTPUT PARAMETER opdBasis   AS DECIMAL DECIMALS 10.

    DEFINE    VARIABLE      dFreight    AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE    VARIABLE      dTotFreight AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE cCustID AS CHARACTER NO-UNDO.
    DEFINE    VARIABLE      ldMinRate   AS DECIMAL     DECIMALS 10.
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    DEFINE BUFFER bf-shipto FOR shipto.
    

    tot-other-freight = 0.
    FIND FIRST bf-oe-boll WHERE ROWID(bf-oe-boll) EQ iprRowid NO-LOCK NO-ERROR.
    
    /* wfk- This loop originally only considered line with the same i-no, */
    /* but I believe the weight of the entire BOL should be considered    */
    /* since they are all on the same BOL                                 */
    IF AVAILABLE bf-oe-boll THEN 
    DO:
        cCustID = ip-cust-no.
        RUN pGetCustID(BUFFER bf-oe-boll, INPUT-OUTPUT cCustID).
           
        FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ bf-oe-boll.b-no NO-LOCK NO-ERROR.
         IF AVAILABLE oe-bolh THEN 
        DO:
            
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ oe-bolh.company
                AND shipto.cust-no EQ cCustID
                AND shipto.ship-id EQ ip-ship-id
                NO-ERROR.
        END.        
        IF AVAILABLE shipto THEN
            FIND FIRST carrier
                WHERE carrier.company EQ shipto.company
                AND carrier.loc     EQ bf-oe-boll.loc
                AND carrier.carrier EQ ip-carrier
                NO-LOCK NO-ERROR.
        ELSE 
            RETURN. /*Failure - should not reach this point*/

        /* Obtain full basis weight (or pallets) to calculate freight */
        tot-other-freight = 0.
        FOR EACH b-oe-boll WHERE b-oe-boll.company = bf-oe-boll.company
            AND b-oe-boll.bol-no  = bf-oe-boll.bol-no
            /* AND b-oe-boll.i-no    = bf-oe-boll.i-no */
            NO-LOCK.
            RUN other-lines (BUFFER b-oe-boll, BUFFER carrier, OUTPUT v-other-freight).
            /* total weight of all lines on BOL for this item */
            tot-other-freight = tot-other-freight + v-other-freight.
        END.

        
        v-del-zone = shipto.dest-code.

        /* Obtain freight for entire bol to compare with minimum */
        FOR EACH b-oe-boll WHERE b-oe-boll.company = bf-oe-boll.company
            AND b-oe-boll.bol-no  = bf-oe-boll.bol-no
            /* AND b-oe-boll.i-no    = bf-oe-boll.i-no */
            NO-LOCK.
            
            /* Indicates to always copy BOL freight class from item */
            FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "BOLFreight" 
                NO-LOCK NO-ERROR.
            IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN 
            DO:
                FIND FIRST itemfg WHERE itemfg.company EQ b-oe-boll.company
                    AND itemfg.i-no EQ b-oe-boll.i-no 
                    NO-LOCK NO-ERROR.
                IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN 
                DO:
                    FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                        AND carr-mtx.loc      EQ b-oe-boll.loc
                        AND carr-mtx.carrier  EQ ip-carrier
                        AND carr-mtx.del-zone EQ itemfg.frt-class
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE carr-mtx THEN
                        ASSIGN v-del-zone = itemfg.frt-class.    
                END.
            END. /* if fgfreightclass */

            RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                INPUT cCustID,
                INPUT oe-bolh.ship-id,
                INPUT oe-bolh.carrier,
                INPUT ROWID(b-oe-boll), 
                OUTPUT v-other-freight). 

            RUN oe/getLineFrt.p (oe-bolh.company, 
                b-oe-boll.loc, 
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
   
        IF AVAILABLE carrier THEN
            FOR EACH itemfg
                WHERE itemfg.company EQ bf-oe-boll.company
                AND itemfg.i-no    EQ bf-oe-boll.i-no
                NO-LOCK:
                
                RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                    INPUT cCustID,
                    INPUT oe-bolh.ship-id,
                    INPUT oe-bolh.carrier,
                    INPUT ROWID(bf-oe-boll), 
                    OUTPUT v-other-freight). 
  
                v-del-zone = shipto.dest-code.
  
                /* Indicates to always copy BOL freight class from item */
                FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name    EQ "BOLFreight" 
                    NO-LOCK NO-ERROR.
                IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN 
                DO:
                    IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN 
                    DO:
                        FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                            AND carr-mtx.loc      EQ shipto.loc
                            AND carr-mtx.carrier  EQ ip-carrier
                            AND carr-mtx.del-zone EQ itemfg.frt-class
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE carr-mtx THEN
                            ASSIGN v-del-zone = itemfg.frt-class.    
                    END.
                END. /* if fgfreightclass */
    
                opdBasis = v-frt-chg.
  
                /* Get freight for current BOL line */
                RUN getfrate (bf-oe-boll.loc, ip-carrier, v-del-zone,
                    shipto.ship-zip, v-other-freight, tot-other-freight, 1, 
                    OUTPUT v-frt-chg, OUTPUT ldMinRate).

                IF dTotFreight LT ldMinRate THEN 
                DO:
                    dTotFreight = ldMinRate.
                    RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
                        INPUT cCustID,
                        INPUT oe-bolh.ship-id,
                        INPUT oe-bolh.carrier,
                        INPUT ROWID(bf-oe-boll), 
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

    DEFINE INPUT  PARAMETER ip-loc         LIKE loc.loc                NO-UNDO.
    DEFINE INPUT  PARAMETER ip-carrier     LIKE carrier.carrier        NO-UNDO.
    DEFINE INPUT  PARAMETER ip-del-zone    LIKE carr-mtx.del-zone      NO-UNDO.
    DEFINE INPUT  PARAMETER ip-zip-code    LIKE shipto.ship-zip        NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty         AS   DECIMAL DECIMALS 10        NO-UNDO.
    DEFINE INPUT  PARAMETER ip-tot-qty         AS   DECIMAL DECIMALS 10        NO-UNDO.
    DEFINE INPUT  PARAMETER ip-rels        AS   INTEGER                    NO-UNDO.
    DEFINE OUTPUT PARAMETER op-freight     AS   DECIMAL DECIMALS 10        NO-UNDO.
    DEFINE OUTPUT PARAMETER op-min         AS   DECIMAL DECIMALS 10        NO-UNDO.
    DEFINE VARIABLE v-zip-code AS CHARACTER NO-UNDO.

    /* {custom/globdefs.i}        */
    /*                            */
    /* {sys/inc/var.i NEW SHARED} */

    ASSIGN
        cocode = g_company
        locode = g_loc.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.
    FIND FIRST carr-mtx
        WHERE carr-mtx.company  EQ cocode
        AND carr-mtx.loc      EQ ip-loc
        AND carr-mtx.carrier  EQ ip-carrier
        AND carr-mtx.del-zone EQ ip-del-zone
        AND carr-mtx.del-zip  EQ ip-zip-code
        NO-LOCK NO-ERROR.
    IF AVAILABLE carr-mtx THEN
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
    DEFINE VARIABLE dLineFreight  AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTLineFreight AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dLineMin      AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBOLFreight   AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBOLMin       AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTbolMin      AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dBasis        AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE dTotalBasis   AS DECIMAL DECIMALS 10 NO-UNDO.

    FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE oe-boll THEN
        FIND FIRST oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAILABLE oe-boll THEN 
    DO:
        RUN getBollFreight (INPUT ROWID(oe-boll), OUTPUT op-Freight, OUTPUT dLineMin, OUTPUT dBasis).                       
    END. /* If rowid of oe-boll was given */                                                                        
    ELSE IF AVAILABLE oe-bolh THEN 
        DO:   
            RUN getBolhFreight (INPUT ROWID(oe-bolh), OUTPUT op-Freight, OUTPUT dBOLMin).
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
    DEFINE PARAMETER BUFFER ipbf-oe-boll FOR oe-boll.
    DEFINE PARAMETER BUFFER ipbf-carrier FOR carrier.
    DEFINE OUTPUT PARAMETER v-out-frt AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
    
    IF AVAILABLE ipbf-carrier THEN
        FOR EACH itemfg
            WHERE itemfg.company EQ ipbf-oe-boll.company
            AND itemfg.i-no    EQ ipbf-oe-boll.i-no
            NO-LOCK:

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ ipbf-oe-boll.i-no
                AND fg-bin.tag     EQ ipbf-oe-boll.tag
                AND fg-bin.loc     EQ ipbf-oe-boll.loc
                AND fg-bin.loc-bin EQ ipbf-oe-boll.loc-bin
                AND fg-bin.job-no  EQ ipbf-oe-boll.job-no
                AND fg-bin.job-no2 EQ ipbf-oe-boll.job-no2
                NO-LOCK NO-ERROR.

            CASE ipbf-carrier.chg-method:
                WHEN "W" THEN 
                    DO:                                     /* Weight in Lbs */
                        v-out-frt = IF ipbf-oe-boll.weight NE 0 THEN ipbf-oe-boll.weight
                        ELSE (itemfg.weight-100 * ipbf-oe-boll.qty / 100).
                    END.

                WHEN "P" THEN 
                    DO:                                     /* # of Pallets */
                        RUN oe/pallcalc.p (ROWID(ipbf-oe-boll), OUTPUT v-pallets).
                        v-out-frt = v-pallets.                        
                    END.

                OTHERWISE 
                DO:                                         /* MSF */
                    v-out-frt = itemfg.t-sqft * ipbf-oe-boll.qty / 1000.
                END.
            END CASE.
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetCustID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCustID Procedure
PROCEDURE pGetCustID:
/*------------------------------------------------------------------------------
 Purpose: Returns the appropriate cust ID given an oe-boll buffer
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-boll FOR oe-boll.
DEFINE INPUT-OUTPUT PARAMETER iopcCustID AS CHARACTER NO-UNDO.

IF ipbf-oe-boll.s-code EQ 'T' THEN DO:
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-oe-boll.company
        AND cust.active EQ "X"
        NO-ERROR.
    IF AVAILABLE cust 
        THEN iopcCustID = cust.cust-no.
 
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

