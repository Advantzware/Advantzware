
                                 
/*------------------------------------------------------------------------
    File        : RfqShipping.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


{RfqShip.i}
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo     AS INT  NO-UNDO.


DEFINE INPUT PARAMETER RfqSeq        AS integer   NO-UNDO.
DEFINE INPUT PARAMETER prmShipid        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmName        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmCarrier      AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmdscr        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmCasno        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmTrno         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmWeight       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmCasCost      AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmTrCost       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmCascnt       AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER prmTrcnt        AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER prmCaslen       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmTrlen        AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmCasWid       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmTrwid        AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmCasdep       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prTrdep        AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmCaspal       AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER prmTrcas        AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER prmCaswt        AS DECIMAL NO-UNDO. 

                                    
DEFINE INPUT PARAMETER RfqSRowid     AS RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqShipping.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
DEFINE BUFFER buff-rfqitem FOR rfqitem.
DEFINE BUFFER buff-shipto FOR rfqitem.
DEFINE BUFFER buff-carrier FOR rfqitem.


IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ?   THEN ASSIGN prmAction = "".
IF PrmRfqNo  = ?   THEN ASSIGN prmRfqNo = 0.

IF RfqSeq  = ?  THEN ASSIGN RfqSeq = 0.
IF prmShipid   = ? THEN ASSIGN   prmShipid     = "".
IF prmCarrier  = ? THEN ASSIGN   prmCarrier    = "".
IF prmCasno    = ? THEN ASSIGN   prmCasno      = "".
IF prmTrno     = ? THEN ASSIGN   prmTrno       = "".
IF prmWeight   = ? THEN ASSIGN   prmWeight     = 0.
IF prmCasCost  = ? THEN ASSIGN   prmCasCost    = 0.
IF prmTrCost   = ? THEN ASSIGN   prmTrCost     = 0.
IF prmCascnt   = ? THEN ASSIGN   prmCascnt     = 0.
IF prmTrcnt    = ? THEN ASSIGN   prmTrcnt      = 0.
IF prmCaslen   = ? THEN ASSIGN   prmCaslen     = 0.
IF prmTrlen    = ? THEN ASSIGN   prmTrlen      = 0. 
IF prmCasWid   = ? THEN ASSIGN   prmCasWid     = 0. 
IF prmTrwid    = ? THEN ASSIGN   prmTrwid      = 0. 
IF prmCasdep   = ? THEN ASSIGN   prmCasdep     = 0. 
IF prTrdep     = ? THEN ASSIGN   prTrdep       = 0. 
IF prmCaspal   = ? THEN ASSIGN   prmCaspal     = 0. 
IF prmTrcas    = ? THEN ASSIGN   prmTrcas      = 0.   
IF prmCaswt    = ? THEN ASSIGN   prmCaswt      = 0.
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".

DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

IF prmAction = "UpdateRfqShipping" THEN DO:
    FIND FIRST shipto WHERE shipto.ship-id = prmShipid NO-LOCK NO-ERROR.
    IF NOT AVAILABLE shipto THEN DO:
        ASSIGN cError  = "Invalid ShipId".
        ASSIGN prmAction = "Select". 
    END.
    FIND FIRST carrier WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
    IF NOT AVAILABLE carrier THEN DO:
        ASSIGN cError  = "Invalid carrier".
        ASSIGN prmAction = "Select". 
    END.
    FIND FIRST ITEM WHERE ITEM.i-no = prmCasno NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ITEM THEN DO:
        ASSIGN cError  = "Invalid Packing Code".
        ASSIGN prmAction = "Select". 
    END.
    FIND FIRST ITEM WHERE ITEM.i-no = prmTrno NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ITEM THEN DO:
        ASSIGN cError  = "Invalid Unit Code".
        ASSIGN prmAction = "Select". 
    END.
END.

/*************************************/
IF prmAction = "UpdateRfqShipping" THEN DO:
      FIND FIRST buff-rfqitem WHERE
         buff-rfqitem.company EQ prmComp AND
         buff-rfqitem.loc EQ prmLoc AND
         buff-rfqitem.rfq-no = prmRfqNo AND
         buff-rfqitem.seq = RfqSeq
         EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL buff-rfqitem THEN DO:

        assign
             buff-rfqitem.ship-id       =  prmShipid  
             buff-rfqitem.carrier       = prmCarrier
             buff-rfqitem.cas-no        =  prmCasno 
             buff-rfqitem.tr-no         =  prmTrno 
             buff-rfqitem.cas-cnt       = prmCascnt
             buff-rfqitem.cas-len       = prmCaslen
             buff-rfqitem.cas-wid       = prmCasWid
             buff-rfqitem.cas-dep       = prmCasdep
             buff-rfqitem.cas-pal       = prmCaspal
             buff-rfqitem.cas-wt        = prmCaswt         
             buff-rfqitem.tr-len        = prmTrlen
             buff-rfqitem.tr-wid        = prmTrwid
             buff-rfqitem.tr-dep        = prTrdep.

        RELEASE buff-rfqitem.                                                                                    
         
        END.   /*if avail buff-rfqitem*/                                                                               
        ASSIGN prmAction = "Select". 
     END.  /*IF prmAction = "UpdateRfqPrinting" THEN DO:*/


 IF prmAction = "Select" THEN DO:
     FIND FIRST rfq WHERE rfq.rfq-no = PrmRfqNo NO-LOCK NO-ERROR.
   FIND FIRST rfqitem WHERE rfqitem.company = prmComp
                        AND rfqitem.loc = prmLoc
                        AND rfqitem.rfq-no = rfq.rfq-no 
                        AND rfqitem.seq = RfqSeq 
                        AND rfqitem.seq < 999  NO-LOCK NO-ERROR.
   FIND FIRST item WHERE  item.company = rfqitem.company 
                    AND item.i-no = rfqitem.tr-no NO-LOCK NO-ERROR.
   find first shipto where shipto.cust-no = rfq.cust-no no-lock no-error.
   find carrier where carrier.company = rfqitem.company and 
                     carrier.loc     = rfqitem.loc and
                     carrier.carrier = rfqitem.carrier
                                 no-lock no-error.
  
          MESSAGE "ship"  rfqitem.ship-id  shipto.ship-id.     

       RUN CreateShipping.
   
END. /*IF prmAction = "Select" */

PROCEDURE CreateShipping:
    CREATE ttRfqShipping.
    
        ASSIGN 
            ttRfqShipping.vShipid    = rfqitem.ship-id
            ttRfqShipping.VShipname  = if avail shipto then shipto.ship-name else ""
            ttRfqShipping.vCarrier   = rfqitem.carrier
            ttRfqShipping.Vcarrdscr  = if avail carrier then carrier.dscr else ""
            ttRfqShipping.vCasno     = rfqitem.cas-no
            ttRfqShipping.VTrno      = rfqitem.tr-no
            ttRfqShipping.vWeight    = rfqitem.weight-m
            ttRfqShipping.vCasCost   = rfqitem.cas-cost
            ttRfqShipping.vTrCost    = rfqitem.tr-cost
            ttRfqShipping.vCascnt    = rfqitem.cas-cnt
            ttRfqShipping.vTrcnt     = rfqitem.tr-cnt
            ttRfqShipping.vCaslen    = rfqitem.cas-len
            ttRfqShipping.vTrlen     = rfqitem.tr-len
            ttRfqShipping.vCasWid    = rfqitem.cas-wid
            ttRfqShipping.vTrwid     = rfqitem.tr-wid   
            ttRfqShipping.vCasdep    = rfqitem.cas-dep
            ttRfqShipping.vTrdep     = rfqitem.tr-dep
            ttRfqShipping.vCaspal    = rfqitem.cas-pal
            ttRfqShipping.vTrcas     = rfqitem.tr-cas
            ttRfqShipping.vCaswt     = rfqitem.cas-wt
            ttRfqShipping.RfqSRowid  = recid(rfqitem)  
            ttRfqShipping.vPallet    = item.i-name 
            ttRfqShipping.vShipAddr  = shipto.ship-addr[1]
            ttRfqShipping.vShipAddr2 = shipto.ship-addr[2]
            ttRfqShipping.vShipCity  = shipto.ship-city
            ttRfqShipping.vShipState = shipto.ship-state
            ttRfqShipping.vShipZip   = shipto.ship-zip
                
            .
        
               
        
END PROCEDURE.  /*PROCEDURE CreateShipping:*/

/*****************************************PROCEDURE assign-RfqShipping******************************/



