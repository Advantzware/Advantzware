

/*------------------------------------------------------------------------
    File        : ViewRfq.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa Singh
    Created     :  Feb 18 2008
    Notes       :
  ----------------------------------------------------------------------*/
  

/* ***************************  Definitions  ************************** */

{ViewRfq.i}

DEFINE INPUT PARAMETER prmUser            AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmExt             AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER PrmRfqNo           AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmReqdate         LIKE rfq.req-date   NO-UNDO.      
DEFINE INPUT PARAMETER prmDuedate         LIKE rfq.due-date   NO-UNDO.     
DEFINE INPUT PARAMETER prmCustno          LIKE rfq.cust-no    NO-UNDO.     
DEFINE INPUT PARAMETER prmShipname        LIKE rfq.ship-name  NO-UNDO.  
DEFINE INPUT PARAMETER prmShipAddr        LIKE rfq.ship-addr[1] NO-UNDO.
DEFINE INPUT PARAMETER prmShipAddr2       LIKE rfq.ship-addr[2] NO-UNDO.
DEFINE INPUT PARAMETER prmShipcity        LIKE rfq.ship-city   NO-UNDO. 
DEFINE INPUT PARAMETER prmShipstate       LIKE rfq.ship-state  NO-UNDO. 
DEFINE INPUT PARAMETER prmShipzip         LIKE rfq.ship-zip    NO-UNDO.   
DEFINE INPUT PARAMETER prmSman            LIKE rfq.sman        NO-UNDO.       
DEFINE INPUT PARAMETER prmSmanName        LIKE sman.sname      NO-UNDO.   
DEFINE INPUT PARAMETER prmComm            LIKE rfq.comm        NO-UNDO.       
DEFINE INPUT PARAMETER prmFobcode         LIKE rfq.fob-code    NO-UNDO.   
DEFINE INPUT PARAMETER prmChgmethod       LIKE rfq.chg-method  NO-UNDO. 
DEFINE INPUT PARAMETER prmWhmonth        LIKE rfq.wh-month    NO-UNDO.  
DEFINE INPUT PARAMETER prmInst            LIKE rfq.inst        NO-UNDO.
DEFINE INPUT PARAMETER VRowid            AS RECID       NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewRfq.
    DEFINE OUTPUT PARAMETER cError            AS CHAR       NO-UNDO.
DEFINE BUFFER bf-rfq FOR rfq.
    DEFINE BUFFER buff-rfq FOR rfq.
DEFINE BUFFER bf-rfqitem FOR rfqitem.
DEFINE BUFFER buff-rfqitem FOR rfqitem.
    DEFINE VAR VAction      AS CHARACTER  NO-UNDO.    
    def var li-rfq as int no-undo.
    def var rfqnum as int no-undo.
    DEF VAR prmLoc AS CHAR NO-UNDO.
    DEF VAR prmComp AS CHAR NO-UNDO.
    
    def var li-seq as int no-undo.
    
IF prmExt       = ? THEN    prmExt         = "".
IF prmAction    = ? THEN    prmAction     = "Select".
IF PrmRfqNo     = ? THEN    PrmRfqNo      = 0.  
IF prmCustno    = ? THEN    prmCustno     = "" .
IF prmShipname  = ? THEN    prmShipname   = ""  .
IF prmShipAddr  = ? THEN    prmShipAddr   = ""  .
IF prmShipAddr2 = ? THEN    prmShipAddr2  = ""  .
IF prmShipcity  = ? THEN    prmShipcity   = ""  .
IF prmShipstate = ? THEN    prmShipstate  = ""  .
IF prmShipzip   = ? THEN    prmShipzip    = ""  .
IF prmSman      = ? THEN    prmSman       = ""  .
IF prmSmanName  = ? THEN    prmSmanName   = ""  .
IF prmComm      = ? THEN    prmComm       = 0  .
IF prmFobcode   = ? THEN    prmFobcode    = ""  .
IF prmChgmethod = ? THEN    prmChgmethod  = ""  .
IF prmWhmonth   = ? THEN    prmWhmonth    = 0  .
IF prmInst      = ? THEN    prmInst       = ""  .
    FOR EACH ttViewRfq NO-LOCK :
        DELETE ttViewRfq.
    END.
   
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.company = prmComp AND
        usercomp.loc NE "" AND
        usercomp.loc_default = yes
        NO-LOCK NO-ERROR.

   prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".


IF prmExt = "ExtView"  THEN DO:
    FOR EACH  usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            CREATE ttViewRfq.
            ASSIGN
                ttViewRfq.cust_no   = cust.cust-no 
                ttViewRfq.ship_name = cust.name
                ttViewRfq.shipAddr  = cust.addr[1]
                ttViewRfq.shipAddr2 = cust.addr[2] 
                ttViewRfq.ship_city = cust.city
                ttViewRfq.ship_state = cust.state
                ttViewRfq.ship_zip   = cust.zip
                ttViewRfq.sman       = cust.sman
                ttViewRfq.fob_code  = cust.fob-code
                .

    find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.            
                ASSIGN 
                    ttViewRfq.comm = sman.scomm
                    ttViewRfq.smanName = if avail sman then sman.sname else "".
        END.
END.
END. /* IF prmAction = "ExtView"  THEN DO:*/


  

IF prmAction = "DeleteView"  THEN DO:
    
    FIND FIRST bf-rfq WHERE
        bf-rfq.company EQ prmComp AND
        bf-rfq.loc EQ prmLoc AND
        bf-rfq.rfq-no = PrmRfqNo NO-ERROR.
    IF AVAIL bf-rfq  THEN DO:
        FOR EACH rfqitem OF bf-rfq :
            DELETE rfqitem.
        END.
        DELETE bf-rfq.
    END.  /*IF AVAIL b_rfqitem */
    FOR EACH  usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK:
        FIND LAST rfq WHERE rfq.company = prmComp AND
            (rfq.cust-no = usercust.cust-no) NO-LOCK NO-ERROR.
        ASSIGN PrmRfqNo = rfq.rfq-no 
            prmAction = "Select".
    END.
END. /*IF prmAction = "delete"*/
/***********************************************************************************************************************/

/* Validation for Rfq Item*/
IF prmAction = "ValidateRfq" THEN DO:
    FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = prmCustno NO-LOCK   NO-ERROR.
    IF NOT AVAIL cust THEN DO:
        ASSIGN cError = "Invalid Customer!!".
        RETURN.
    END.  
    
    FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = prmSman NO-LOCK   NO-ERROR.
    IF NOT AVAIL sman THEN DO:
        ASSIGN cError = "Invalid SalesMan!!".
        RETURN.
    END.   
    
END.
/********************************************************************************/

IF prmAction = "getRfqNo" THEN DO:
    def var new-rfq# like rfq.rfq-no no-undo.
  
    find first rfq-ctrl no-error.
    if avail rfq-ctrl then do:
        new-rfq# = rfq-ctrl.rfq-num.
        rfq-ctrl.rfq-num = rfq-ctrl.rfq-num + 1.   
        release rfq-ctrl. 
        
        CREATE ttViewRfq.
        ASSIGN
            ttViewRfq.rfq_no = new-rfq# .
        
        /*return string(new-rfq#).*/
    end.
END.


/*******************************************************************************/

IF prmAction = "AddRfqView" THEN DO:

 FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = prmSman NO-LOCK   NO-ERROR.
 IF AVAIL sman THEN DO:   
     ASSIGN prmSmanName = sman.sname.
 END.

    IF prmFobcode = "" THEN DO:
        FIND FIRST cust WHERE cust.cust-no = prmCustno NO-LOCK NO-ERROR.
        IF AVAIL cust THEN ASSIGN prmFobcode = cust.fob-code.
    END.
    ASSIGN li-rfq =  1 .
    find LAST bf-rfq WHERE bf-rfq.company = prmComp AND bf-rfq.loc = prmLoc  USE-INDEX rfq no-lock NO-ERROR.
    ASSIGN    rfqnum = bf-rfq.rfq-no + li-rfq.
    CREATE bf-rfq.
    
    ASSIGN
        bf-rfq.rfq-no           = PrmRfqNo
        bf-rfq.company          = prmComp
        bf-rfq.loc              = prmLoc 
        bf-rfq.req-date         = prmReqdate  
        bf-rfq.due-date         = prmDuedate  
        bf-rfq.cust-no          = prmCustno   
        bf-rfq.ship-name        = prmShipname 
        bf-rfq.ship-addr[1]     = prmShipAddr 
        bf-rfq.ship-addr[2]     = prmShipAddr2
        bf-rfq.ship-city        = prmShipcity 
        bf-rfq.ship-state       = prmShipstate
        bf-rfq.ship-zip         = prmShipzip  
        bf-rfq.sman             = prmSman    
        bf-rfq.comm             = prmComm             
        bf-rfq.fob-code         = prmFobcode   
        bf-rfq.chg-method       = prmChgmethod 
        bf-rfq.wh-month         = prmWhmonth    
        bf-rfq.inst             = prmInst      
            
        .
       
    ASSIGN prmAction = "Select".
    RELEASE bf-rfq.
    
    /*RELEASE buff-rfqitem.*/

END.  /*IF prmAction = "AddRfqView" THEN DO:*/
IF prmAction = "CopyRfqView" THEN DO:
   
    FIND buff-rfq WHERE
         buff-rfq.company EQ prmComp AND
         buff-rfq.loc EQ prmLoc AND
         buff-rfq.rfq-no = prmRfqNo  
         NO-LOCK NO-ERROR.
        
    ASSIGN li-rfq =  1 .
    find LAST bf-rfq WHERE bf-rfq.company = prmComp AND bf-rfq.loc = prmLoc  USE-INDEX rfq no-lock NO-ERROR.
    
    ASSIGN    rfqnum = bf-rfq.rfq-no + li-rfq.
    
    CREATE bf-rfq.
    ASSIGN
        bf-rfq.rfq-no           = rfqnum
        bf-rfq.req-date         = TODAY  
        bf-rfq.due-date         = TODAY
        bf-rfq.company          = prmComp
        bf-rfq.loc              = prmLoc  
        bf-rfq.cust-no          = buff-rfq.cust-no
        bf-rfq.ship-name        = buff-rfq.ship-name
        bf-rfq.ship-addr[1]     = buff-rfq.ship-addr[1]
        bf-rfq.ship-addr[2]     = buff-rfq.ship-addr[2]     
        bf-rfq.ship-city        = buff-rfq.ship-city 
        bf-rfq.ship-state       = buff-rfq.ship-state
        bf-rfq.ship-zip         = buff-rfq.ship-zip
        bf-rfq.sman             = buff-rfq.sman 
        bf-rfq.comm             = buff-rfq.comm 
        bf-rfq.fob-code         = buff-rfq.fob-code 
        bf-rfq.chg-method       = buff-rfq.chg-method 
        bf-rfq.wh-month         = buff-rfq.wh-month
        bf-rfq.inst             = buff-rfq.inst
        bf-rfq.sman             = prmSman           .

    FIND FIRST sman WHERE sman.sman = buff-rfq.sman NO-LOCK NO-ERROR.
    IF AVAIL sman THEN
        ASSIGN  sman.sname =  prmSmanName .
    
    ASSIGN PrmRfqNo = rfqnum 
           prmAction = "Select".
        RELEASE bf-rfq.
    /*RELEASE buff-rfqitem.*/
   
END. /*IF prmAction = "CopyRfqView"*/
IF prmAction = "UpdateRfqView" THEN DO:  
    FIND FIRST sman WHERE sman.company = prmComp AND sman.sman = prmSman NO-LOCK   NO-ERROR.
    IF AVAIL sman THEN DO:  
        ASSIGN prmSmanName = sman.sname.
    END.        

   FIND bf-rfq WHERE
         bf-rfq.company EQ prmComp AND
         bf-rfq.loc EQ prmLoc AND
         bf-rfq.rfq-no = prmRfqNo  
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL bf-rfq THEN DO:
        assign
             bf-rfq.req-date         = prmReqdate  
             bf-rfq.due-date         = prmDuedate  
             bf-rfq.cust-no          = prmCustno   
             bf-rfq.ship-name        = prmShipname 
             bf-rfq.ship-addr[1]     = prmShipAddr 
             bf-rfq.ship-addr[2]     = prmShipAddr2
             bf-rfq.ship-city        = prmShipcity 
             bf-rfq.ship-state       = prmShipstate
             bf-rfq.ship-zip         = prmShipzip  
             bf-rfq.sman             = prmSman    
             bf-rfq.comm             = prmComm             
             bf-rfq.fob-code         = prmFobcode   
             bf-rfq.chg-method       = prmChgmethod 
             bf-rfq.wh-month         = prmWhmonth    
             bf-rfq.inst             = prmInst      
              bf-rfq.sman            = prmSman           
        .
    FIND FIRST sman WHERE sman.sman = bf-rfq.sman EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL sman THEN
        ASSIGN sman.sname = prmSmanName.
        RELEASE bf-rfq.                                                                                    
    END.   /*if avail bf-rfq*/                                                                               
    ASSIGN prmAction = "Select". 
                

    END.  /*IF prmAction = "UpdateRfqPrinting" THEN DO:*/
   
    

    
    IF prmAction = "Select" THEN DO:
        FOR FIRST rfq WHERE
            rfq.company EQ prmComp AND
            rfq.loc EQ prmLoc AND
            rfq.rfq-no = PrmRfqNo
             NO-LOCK:
            
            find sman where
                 sman.company = rfq.company AND 
                 sman.sman = rfq.sman
                 no-lock no-error.
            RUN CreateRfqView.
         END.
    END.

PROCEDURE CreateRfqView:
    
    CREATE ttViewRfq.
    ASSIGN
        ttViewRfq.rfq_no = rfq.rfq-no
        ttViewRfq.req_date = rfq.req-date
        ttViewRfq.due_date = rfq.due-date
        ttViewRfq.cust_no = rfq.cust-no
        ttViewRfq.ship_name = rfq.ship-name
        ttViewRfq.shipAddr = rfq.ship-addr[1]
        ttViewRfq.shipAddr2 = rfq.ship-addr[2]
        ttViewRfq.ship_city = rfq.ship-city
        ttViewRfq.ship_state = rfq.ship-state
        ttViewRfq.ship_zip = rfq.ship-zip
        ttViewRfq.sman = rfq.sman
        ttViewRfq.comm = rfq.comm
        ttViewRfq.fob_code = rfq.fob-code
        ttViewRfq.chg_method = rfq.chg-method
        ttViewRfq.wh_month = rfq.wh-month
        ttViewRfq.inst = rfq.inst
        ttViewRfq.sman = rfq.sma
        ttViewRfq.smanName = if avail sman then sman.sname else "".
             /*ttViewRfq.VRowid = RECID(rfq).*/
            /*if cust.frt-pay = "P" then 
                ASSIGN  ttViewRfq.chg_method =   "Prepaid".
            else if cust.frt-pay = "C" then ASSIGN  ttViewRfq.chg_method = "Collect".
            else if cust.frt-pay = "B" THEN ASSIGN  ttViewRfq.chg_method =  "Bill".
            else if cust.frt-pay = "T" then ASSIGN  ttViewRfq.chg_method = "Third Party".
            else "".*/
           IF VAction = "AddRfqView" OR VAction = "CopyRfqView"  THEN DO:
                ASSIGN 
                     ttViewRfq.VRowid = RECID(rfq). 
            END.
    END PROCEDURE.






