
/*------------------------------------------------------------------------
    File        : cXmlBOL.p
    Purpose     : 

    Syntax      :

    Description : cXML BOL Generation   

    Author(s)   : WFK
    Created     : Wed Aug 21 17:08:12 EDT 2019
    Notes       : Code originally in oe/rep/bolprem.i 
                : used by bolpremcx.p, bolprempx.p and bolpremx.p 

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

      
/*used by bolpremcx.p, bolprempx.p and bolpremx.p */
DEFINE  INPUT  PARAMETER  ipcCompany   AS CHARACTER NO-UNDO.
DEFINE  INPUT  PARAMETER  ipiBolNo     AS INTEGER  NO-UNDO.
DEFINE  INPUT  PARAMETER  ipiOrdNo     AS INTEGER NO-UNDO.
DEFINE new shared VARIABLE cocode       AS CHARACTER NO-UNDO.

cocode = ipcCompany.
 
{oe/bolSign.i} 
{oe/rep/oe-lad.i}
DEF BUFFER bf-ttboll FOR tt-boll. 


{XMLOutput/XMLOutput.i  &cXMLSysCtrl=cXMLASN &cXMLOutput=cXMLASN &Company=cocode &c=c}


ASSIGN      
    clXMLOutput = YES 
    .

&SCOPED-DEFINE sysCtrlcXML cXMLASN
DEFINE VARIABLE j               AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ship-name     LIKE shipto.ship-name.
DEFINE VARIABLE v-ship-addr     LIKE shipto.ship-addr.
DEFINE VARIABLE v-ship-city     LIKE shipto.ship-city.
DEFINE VARIABLE v-ship-state    LIKE shipto.ship-state.
DEFINE VARIABLE v-ship-zip      LIKE shipto.ship-zip.

DEFINE VARIABLE cPoNum          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lGeneratecXML   AS LOG       NO-UNDO.
DEFINE VARIABLE iReprint        AS INTEGER   NO-UNDO.

DEFINE VARIABLE ll-consol-bolls AS LOG       NO-UNDO.

/* Needed for Cxml */
DEFINE VARIABLE cOrderDate      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dOrigQty        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cOrigUom        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLShipTo      AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCaseUOMList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.



RUN sys/ref/nk1look.p (INPUT cocode, "CaseUOMList", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
cCaseUomList = cRtnChar.  
  
/* Needed for cxml ???? */
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BOLFMT" NO-LOCK NO-ERROR.
ASSIGN
    ll-consol-bolls = AVAILABLE sys-ctrl AND sys-ctrl.int-fld NE 0.


FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.

/* Sets v-term value */
{sa/sa-sls01.i}
 
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.

FOR EACH oe-bolh NO-LOCK
  WHERE oe-bolh.company EQ cocode
    AND oe-bolh.bol-no = ipiBolNo
    ,
    FIRST cust NO-LOCK
    WHERE cust.company EQ cocode
    AND cust.cust-no EQ oe-bolh.cust-no    
    BREAK BY oe-bolh.bol-no:

    /* rstark 05291402 */
    FIND FIRST sys-ctrl NO-LOCK
        WHERE sys-ctrl.company EQ  cocode
        AND sys-ctrl.name    EQ 'cXMLASN' NO-ERROR.
    IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
    DO:
        FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
            WHERE sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no
            AND sys-ctrl-shipto.log-fld EQ YES
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto THEN 
            ASSIGN 
                cXMLIdentity  = sys-ctrl-shipto.char-fld
                cXMLDTD       = 'http://xml.cxml.org/schemas/cXML/1.2.025/Fulfill.dtd'
                iReprint      = sys-ctrl-shipto.int-fld
                lGeneratecXML = YES.
    END. /* avail sys-ctrl */
    
    
                
     {XMLOutput/cXMLCust.i
        &cXMLSysCtrl={&sysCtrlcXML}
        &Company=oe-bolh.company
        &Customer=oe-bolh.cust-no}
     
    
    
    IF FIRST-OF(oe-bolh.bol-no) THEN 
    DO:
        
        FIND FIRST carrier
            WHERE carrier.company EQ oe-bolh.company
            AND carrier.carrier EQ oe-bolh.carrier
            NO-LOCK NO-ERROR.

        RUN oe/custxship.p (oe-bolh.company,
            oe-bolh.cust-no,
            oe-bolh.ship-id,
            BUFFER shipto).

        ASSIGN
            v-ship-name  = shipto.ship-name
            v-ship-city  = shipto.ship-city
            v-ship-state = shipto.ship-state
            v-ship-zip   = shipto.ship-zip       
            v-ship-addr[1] = shipto.ship-addr[1]
            v-ship-addr[2] = shipto.ship-addr[2]    
            .            
       
        FOR EACH tt-boll:
            DELETE tt-boll.
        END.
    END. /* first-of(oe-bolh.bol-no) */

    
    /* Create tt-boll */
    FOR EACH oe-boll 
        WHERE oe-boll.company EQ oe-bolh.company 
          AND oe-boll.b-no EQ oe-bolh.b-no
          AND oe-boll.ord-no EQ ipiOrdNo:
        IF ll-consol-bolls THEN 
        DO:
            IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
                RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

            IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
                RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
        END.

        ELSE 
        DO:
            CREATE tt-boll.
            BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
        END.

    END.
    
    j = 0.
   
    IF LAST-OF(oe-bolh.bol-no)  THEN 
    DO:
        
        /* Needed for cxml */
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ oe-bolh.company
            AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
            AND sys-ctrl-shipto.cust-vend EQ YES
            AND sys-ctrl-shipto.cust-vend-no EQ oe-bolh.cust-no
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN 
            cXMLShipTo = TRIM(sys-ctrl-shipto.char-fld) + oe-bolh.ship-id.
        ELSE 
            cXMLShipTo = oe-bolh.ship-id.  
                     

        IF lGeneratecXML THEN 
        DO:
            /* Output to the stream cXmlOutput If lXMLOutput = no, procedure cXmloutput just returns */
            RUN cXMLOutput (clXMLOutput,'Request deploymentMode="' + cXMLProduction + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'ShipNoticeRequest','','Row').
            RUN cXMLOutput (clXMLOutput,'ShipNoticeHeader shipmentType="actual" shipmentDate="' + getFormattedDate(oe-bolh.bol-date, TIME)
                + '" deliveryDate="' + getFormattedDate(oe-bolh.bol-date + 2, TIME)
                + '" noticeDate="' + cXMLTimeStamp
                + '" operation="new" shipmentID="' +
                STRING(oe-bolh.bol-no) + '"','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact role="shipFrom"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en"','','Row').
            RUN cXMLOutput (clXMLOutput,'','Premier Packaging','Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street','3900 Produce Road','Col').
            RUN cXMLOutput (clXMLOutput,'City','Louisville','Col').
            RUN cXMLOutput (clXMLOutput,'State','KY','Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode','40218','Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','United States','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'Contact addressID="' + cXMLShipTo + '" role="shipTo"','','Row').
            RUN cXMLOutput (clXMLOutput,'Name xml:lang="en"','','Row').
            RUN cXMLOutput (clXMLOutput,'',v-ship-name,'Col').
            RUN cXMLOutput (clXMLOutput,'/Name','','Row').
            RUN cXMLOutput (clXMLOutput,'PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'Street',v-ship-addr[1],'Col').
            IF shipto.ship-addr[2] NE "" AND shipto.ship-addr[2] NE '345 Court Street' THEN 
                RUN cXMLOutput (clXMLOutput,'Street',shipto.ship-addr[2],'Col').
            RUN cXMLOutput (clXMLOutput,'City',v-ship-city,'Col').
            RUN cXMLOutput (clXMLOutput,'State',v-ship-state,'Col').
            RUN cXMLOutput (clXMLOutput,'PostalCode',v-ship-zip,'Col').
            RUN cXMLOutput (clXMLOutput,'Country isoCountryCode="US"','','Row').
            RUN cXMLOutput (clXMLOutput,'','United States','Col').
            RUN cXMLOutput (clXMLOutput,'/Country','','Row').
            RUN cXMLOutput (clXMLOutput,'/PostalAddress','','Row').
            RUN cXMLOutput (clXMLOutput,'/Contact','','Row').
            RUN cXMLOutput (clXMLOutput,'/ShipNoticeHeader','','Row').
            RUN cXMLOutput (clXMLOutput,'ShipControl','','Row').
            RUN cXMLOutput (clXMLOutput,'CarrierIdentifier domain="companyName"','','Row').
            RUN cXMLOutput (clXMLOutput,'',oe-bolh.carrier,'Col').
            RUN cXMLOutput (clXMLOutput,'/CarrierIdentifier','','Row').
            RUN cXMLOutput (clXMLOutput,'ShipmentIdentifier',oe-bolh.trailer,'Col').
            RUN cXMLOutput (clXMLOutput,'/ShipControl','','Row').
            RUN cXMLOutput (clXMLOutput,'ShipNoticePortion','','Row').
//            RUN cXMLOutput (clXMLOutput,'OrderReference orderDate="' + cOrderDate + '"' + ' orderID="' + cPoNum + '"','','Row').
//            RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + cXMLPayloadID + '" /','','Row').
//            RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
            ciXMLOutput = 0.
        
        END. /* If cGeneratecXML */


        FOR EACH tt-boll,
            FIRST itemfg WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ tt-boll.i-no NO-LOCK
            BREAK BY tt-boll.po-no /* po-no first like premier */            
            BY tt-boll.ord-no
            BY tt-boll.i-no
            BY tt-boll.line
            BY tt-boll.cases DESCENDING:
            
            IF FIRST-OF(tt-boll.ord-no) THEN DO: 
                FIND FIRST oe-ord no-lock
                    WHERE oe-ord.company EQ cocode
                      AND oe-ord.ord-no EQ tt-boll.ord-no
                    NO-ERROR. 

                IF AVAILABLE oe-ord THEN DO:
                    ASSIGN
                        cXMLPayloadID = oe-ord.spare-char-3
                        cXMLProcessID = STRING(oe-ord.ord-no)
                        cOrderDate    = STRING(YEAR(oe-ord.ord-date),'9999')
                                     + '-'
                                     + STRING(MONTH(oe-ord.ord-date),'99')
                                     + '-'
                                     + STRING(DAY(oe-ord.ord-date),'99')
                                     + 'T'
                                     + STRING(0,'hh:mm:ss')
                                     + '-05:00'
                        cPoNum        = tt-boll.po-no
                        .                
                      
                    RUN cXMLOutput (clXMLOutput,'OrderReference orderDate="' + cOrderDate + '"' + ' orderID="' + cPoNum + '"','','Row').
                    RUN cXMLOutput (clXMLOutput,'DocumentReference payloadID="' + cXMLPayloadID + '" /','','Row').
                    RUN cXMLOutput (clXMLOutput,'/OrderReference','','Row').
              END.
            END.
           
            IF ll-consol-bolls THEN 
            DO:
                
                IF FIRST-OF(tt-boll.LINE) THEN DO:

                    FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                                           AND bf-ttboll.po-no = tt-boll.po-no
                                           AND bf-ttboll.ord-no = tt-boll.ord-no
                                           AND bf-ttboll.LINE = tt-boll.LINE
                                    BREAK BY bf-ttboll.cases DESC.
                                    
                          find first oe-ordl where oe-ordl.company eq cocode
                             and oe-ordl.ord-no  eq tt-boll.ord-no
                             and oe-ordl.i-no    eq tt-boll.i-no
                             and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
                    
                          find first oe-ord where oe-ord.company eq cocode
                             and oe-ord.ord-no  eq tt-boll.ord-no no-lock no-error.
                           IF lGenerateCXML  THEN DO:
                                IF AVAIL oe-ordl AND oe-ordl.spare-char-2 NE '' THEN DO:
                                    ASSIGN 
                                        dOrigQty = oe-ordl.spare-dec-1
                                        cOrigUom = oe-ordl.spare-char-2
                                        .
                                    IF (cOrigUom EQ 'CS' OR LOOKUP(cOrigUom, cCaseUOMList) GT 0)
                                        AND dOrigQty NE tt-boll.qty 
                                        AND oe-ordl.cas-cnt NE 0 THEN DO:
                                        dOrigQty = tt-boll.qty / oe-ordl.cas-cnt.
                                    END.
                                    ELSE dOrigQty = tt-boll.qty.
                                END.
                                IF dOrigQty EQ 0 THEN dOrigQty = tt-boll.qty.
                                IF cOrigUom EQ "" THEN cOrigUom = "EA".
                                ciXMLOutput = ciXMLOutput + 1.
                                RUN cXMLOutput (clXMLOutput,'ShipNoticeItem lineNumber="' + STRING(tt-boll.LINE) + '" quantity="' + STRING(dOrigQty) + '"','','Row').
                                RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',cOrigUom,'Col').
                                RUN cXMLOutput (clXMLOutput,'/ShipNoticeItem','','Row').
                           /* rstark 05291402 */
                           END. /* If generate cxml */
                    END. /* Each bf-ttbol */  
                END. /* If first-of line */

            END.  /* If consolidating BOLs */
            ELSE 
            DO:
                FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.ord-no  EQ tt-boll.ord-no
                    AND oe-ordl.i-no    EQ tt-boll.i-no
                    AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

                FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.
                
                IF lGenerateCXML  THEN 
                DO:
                    IF AVAILABLE oe-ordl AND oe-ordl.spare-char-2 NE '' THEN 
                    DO:
                        ASSIGN 
                            dOrigQty = oe-ordl.spare-dec-1
                            cOrigUom = oe-ordl.spare-char-2
                            .

                        IF (cOrigUom EQ 'CS' OR LOOKUP(cOrigUom, cCaseUOMList) GT 0)
                            AND dOrigQty NE tt-boll.qty 
                            AND oe-ordl.cas-cnt NE 0 THEN 
                        DO:
                            dOrigQty = tt-boll.qty / oe-ordl.cas-cnt.
                        END.
                        ELSE dOrigQty = tt-boll.qty.
                    END.
                    IF dOrigQty EQ 0 THEN dOrigQty = tt-boll.qty.
                    IF cOrigUom EQ "" THEN cOrigUom = "EA".
                    ciXMLOutput = ciXMLOutput + 1.
                    RUN cXMLOutput (clXMLOutput,'ShipNoticeItem lineNumber="' + STRING(tt-boll.LINE) + '" quantity="' + STRING(dOrigQty) + '"','','Row').
                    RUN cXMLOutput (clXMLOutput,'UnitOfMeasure',cOrigUom,'Col').
                    RUN cXMLOutput (clXMLOutput,'/ShipNoticeItem','','Row').
                
                END. /* If lGeneeratecxml */

            END. /* else...not consolidate */
                       
        END. /* for each tt-boll */
 
        IF lGeneratecXML THEN 
        DO: 
            
            RUN cXMLOutput (clXMLOutput,'/ShipNoticePortion','','Row').
            RUN cXMLOutput (clXMLOutput,'/ShipNoticeRequest','','Row').
            RUN cXMLOutput (clXMLOutput,'/Request','','Row').
        
        END.
         
    IF lGeneratecXML THEN 
    DO:
        {XMLOutput/XMLOutput.i &c=c &XMLClose} /* rstark 05291402 */
    END. 
    END. /* last of bol-no */
  


  
END. /* for each oe-bolh */


/* Clean up in main program, not needed here */

PROCEDURE create-tt-boll.
    DEFINE INPUT PARAMETER ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
    DEFINE INPUT PARAMETER ip-cases    LIKE oe-boll.cases NO-UNDO.

    IF ip-qty-case LT 0 THEN
        ASSIGN
            ip-qty-case = ip-qty-case * -1
            ip-cases    = ip-cases * -1.

    FIND FIRST tt-boll
        WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
        AND tt-boll.qty-case EQ ip-qty-case
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE tt-boll THEN 
    DO:
        CREATE tt-boll.
        BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
            ASSIGN
            tt-boll.qty-case = ip-qty-case
            tt-boll.cases    = 0
            tt-boll.qty      = 0
            tt-boll.weight   = 0
            tt-boll.partial  = 0.
    END.

    ASSIGN
        tt-boll.cases  = tt-boll.cases + ip-cases
        tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
        tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight).

    IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.
 

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
