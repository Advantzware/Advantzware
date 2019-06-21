
/*------------------------------------------------------------------------
    File        :OrdEntryUpdate.p
    Purpose     : ViewOrderEntry

    Syntax      :

    Description : Return a Dataset of all order Entry

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrdEntryUpdate.i}
 
MESSAGE "entry"  .
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUserid      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmStat        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSold        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmOrdate      AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldName    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueCode     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueDate     AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmCustAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLastDate    AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmcustAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProdDate    AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmCity        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmState       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmZip         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldCity    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldState   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldZip     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPonum       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmContact     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOverpct     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmUnderpct    AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmTerms       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmTermdscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProd        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmTaxgr       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFob         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSname       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman2       AS CHAR NO-UNDO.                   
DEFINE INPUT PARAMETER prmSname2      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSman3       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSname3       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCtype       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmcExp        AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER prmCnum        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCauth       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustName       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmType       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmLine       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmWhis     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER VRowid            AS RECID       NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateOrdEntry.
    DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
DEFINE BUFFER bf-ord FOR oe-ord.
    DEFINE BUFFER buff-ord FOR oe-ord.
DEFINE BUFFER buff-ordl FOR oe-ordl.
def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE Ordernum AS INTEGER.
DEFINE VAR VAction      AS CHARACTER  NO-UNDO.    
IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = 0.
     
IF prmAction      = ?  THEN ASSIGN    prmAction      = "".
IF prmAction      = ""  THEN ASSIGN    prmAction      = "Select".

IF prmContact     = ?  THEN ASSIGN    prmContact     = "".      
IF prmCustomer    = ?  THEN ASSIGN    prmCustomer    = "".
IF prmUserid      = ?  THEN ASSIGN    prmUserid      = "".  
IF prmStat        = ?  THEN ASSIGN    prmStat        = "".  
IF prmSold        = ?  THEN ASSIGN    prmSold        = "".  
IF prmSoldName    = ?  THEN ASSIGN    prmSoldName    = "".  
IF prmDueCode     = ?  THEN ASSIGN    prmDueCode     = "".             
IF prmCustAddr    = ?  THEN ASSIGN    prmCustAddr    = "".     
IF prmSoldAddr    = ?  THEN ASSIGN    prmSoldAddr    = "". 
IF prmcustAddr2   = ?  THEN ASSIGN    prmcustAddr2   = "".              
IF prmSoldAddr2   = ?  THEN ASSIGN    prmSoldAddr2   = "".     
IF prmCity        = ?  THEN ASSIGN    prmCity        = "". 
IF prmState       = ?  THEN ASSIGN    prmState       = "". 
IF prmZip         = ?  THEN ASSIGN    prmZip         = "".    
IF prmSoldCity    = ?  THEN ASSIGN    prmSoldCity    = "".  
IF prmSoldState   = ?  THEN ASSIGN    prmSoldState   = "".  
IF prmSoldZip     = ?  THEN ASSIGN    prmSoldZip     = "".  
IF prmPonum       = ?  THEN ASSIGN    prmPonum       = "".  
IF prmOverpct     = ?  THEN ASSIGN    prmOverpct     = 0.  
IF prmUnderpct    = ?  THEN ASSIGN    prmUnderpct    = 0.  
IF prmTerms       = ?  THEN ASSIGN    prmTerms       = "".  
IF prmTermdscr    = ?  THEN ASSIGN    prmTermdscr    = "".       
IF prmProd        = ?  THEN ASSIGN    prmProd        = 0.  
IF prmTaxgr       = ?  THEN ASSIGN    prmTaxgr       = "".  
IF prmFreight     = ?  THEN ASSIGN    prmFreight     = "".  
IF prmCarrier     = ?  THEN ASSIGN    prmCarrier     = "".  
IF prmFob         = ?  THEN ASSIGN    prmFob         = "".  
IF prmSman        = ?  THEN ASSIGN    prmSman        = "".  
IF prmSname       = ?  THEN ASSIGN    prmSname       = "".  
IF prmSman2       = ?  THEN ASSIGN    prmSman2       = "".  
IF prmSname2      = ?  THEN ASSIGN    prmSname2      = "".  
IF prmSman3       = ?  THEN ASSIGN    prmSman3       = "".  
IF prmSname3      = ?  THEN ASSIGN    prmSname3      = "".  
IF prmCtype       = ?  THEN ASSIGN    prmCtype       = "".
IF prmCnum        = ?  THEN ASSIGN    prmCnum        = "". 
IF prmCauth       = ?  THEN ASSIGN    prmCauth       = "". 
IF prmCustName       = ?  THEN ASSIGN    prmCustName       = "". 
IF prmType       = ?  THEN ASSIGN    prmType       = "". 
IF prmLine      = ?  THEN ASSIGN    prmLine      = 0. 

DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
IF prmAction = "Add" OR  prmAction = "Copy" THEN DO:
   
       ASSIGN VAction =  prmAction.
END.


 
/* ********************  Preprocessor Definitions  ******************** */
IF prmAction = "Update" THEN DO:
   FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum AND oe-ord.company = prmComp EXCLUSIVE-LOCK NO-ERROR. 
    FIND FIRST carrier WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
    IF NOT AVAILABLE carrier THEN DO:
        ASSIGN cError  = "Invalid carrier".
        RETURN.
        
    END.

    FIND first terms where terms.t-code = prmTerms  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terms  then do:
      ASSIGN cError  = "Invalid Terms Code. Try help. ".
      RETURN.   

    end.

    FIND first stax where stax.tax-group = prmTaxgr  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE stax  then do:
     ASSIGN cError  = "Invalid Tax Code. Try help. ".
     RETURN.

   end.
    IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman AND sman.company = PrmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:

     ASSIGN cError = "Invalid Salesman".
     RETURN.
                
    END.
    END.
    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2 AND sman.company = PrmComp  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:

    ASSIGN cError = "Invalid Salesman".
    RETURN.

   END.
   END.
IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3 AND sman.company = PrmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:

       ASSIGN cError = "Invalid Salesman".
       RETURN.

      END.

      END.

    IF prmDueCode <> "" AND
       lookup(prmDueCode,v-duelist) = 0 then 
    DO:
        ASSIGN cError  = "Invalid Due Code. Try help. ".
        RETURN.
        
    END.
    IF date(prmDueDate) < date(oe-ord.ord-date) then do:
        ASSIGN cError  = "Due Date can not be earlier than Order Date".
        RETURN.
        
    END.
  IF DATE(prmDueDate) > DATE(oe-ord.last-date) 
  THEN oe-ord.last-date = prmDueDate.
  /*IF (prmProdDate < today )then do:
    ASSIGN cError  = "Prod. Date can not be earlier than TODAY.".
        ASSIGN prmAction = "Select". 
  END.*/
find first soldto where soldto.company = oe-ord.company and trim(soldto.sold-id) = trim(prmSold)
                        NO-LOCK no-error.
    if avail soldto THEN DO:
          assign 
             prmSold        = soldto.sold-id 
             prmSoldName    = soldto.sold-name
             prmSoldAddr    = soldto.sold-addr[1]
             prmSoldAddr2   = soldto.sold-addr[2]
             prmSoldCity    = soldto.sold-city
             prmSoldState   = soldto.sold-state
             prmSoldZip     = soldto.sold-zip
             .
    END.
    else if prmSold <> "" then do:
         
         ASSIGN cError  = "Invalid Sold To. Try help.". 
         RETURN.
        
    end.  
 
END.
    
IF prmAction = "Delete"  THEN DO:
    FIND FIRST bf-ord WHERE
        bf-ord.company EQ prmComp AND
        bf-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-ord  THEN DO:
        FOR EACH buff-ordl WHERE buff-ordl.company EQ bf-ord.company AND
            buff-ordl.ord-no = bf-ord.ord-no EXCLUSIVE-LOCK:
            DELETE buff-ordl.
        END.
        DELETE bf-ord.
   END.  /*IF AVAIL bf-ordl */
   MESSAGE "Test" prmComp prmCustomer.
    FIND LAST oe-ord WHERE   oe-ord.cust-no = prmCustomer NO-LOCK NO-ERROR.
    MESSAGE "Test2" oe-ord.ord-no .
    ASSIGN prmOrderNum = oe-ord.ord-no 
          prmAction = "Select".
    MESSAGE "Test3" prmAction.
END. /*IF prmAction = "delete"*/
/*******************************************************************************/
IF prmAction = "Add" THEN DO:
     FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum AND oe-ord.company = prmComp EXCLUSIVE-LOCK NO-ERROR. 
    FIND FIRST carrier WHERE carrier.carrier = prmCarrier NO-LOCK NO-ERROR.
    IF NOT AVAILABLE carrier THEN DO:
        ASSIGN cError  = "Invalid carrier".
        RETURN.
        
    END.

    FIND first terms where terms.t-code = prmTerms  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terms  then do:
      ASSIGN cError  = "Invalid Terms Code. Try help. ".
      RETURN.

    end.

    FIND first stax where stax.tax-group = prmTaxgr  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE stax  then do:
     ASSIGN cError  = "Invalid Tax Code. Try help. ".
     RETURN.

   end.
    IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman AND sman.company = PrmComp  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:

     ASSIGN cError = "Invalid Salesman".
     RETURN.
                
    END.
    END.
    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2 AND sman.company = PrmComp  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:

    ASSIGN cError = "Invalid Salesman".
    RETURN.

   END.
   END.
IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3 AND sman.company = PrmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:

       ASSIGN cError = "Invalid Salesman".
       RETURN.

      END.

      END.

    IF prmDueCode <> "" AND
       lookup(prmDueCode,v-duelist) = 0 then 
    DO:
        ASSIGN cError  = "Invalid Due Code. Try help. ".
        RETURN.
        
    END.
    IF date(prmDueDate) < date(oe-ord.ord-date) then do:
        ASSIGN cError  = "Due Date can not be earlier than Order Date".
        RETURN.
        
    END.
  IF DATE(prmDueDate) > DATE(oe-ord.last-date) 
  THEN oe-ord.last-date = prmDueDate.
  /*IF (prmProdDate < today )then do:
    ASSIGN cError  = "Prod. Date can not be earlier than TODAY.".
        ASSIGN prmAction = "Select". 
  END.*/
find first soldto where soldto.company = oe-ord.company and trim(soldto.sold-id) = trim(prmSold)
                        NO-LOCK no-error.
    if avail soldto THEN DO:
          assign 
             prmSold        = soldto.sold-id 
             prmSoldName    = soldto.sold-name
             prmSoldAddr    = soldto.sold-addr[1]
             prmSoldAddr2   = soldto.sold-addr[2]
             prmSoldCity    = soldto.sold-city
             prmSoldState   = soldto.sold-state
             prmSoldZip     = soldto.sold-zip
             .
    END.
    else if prmSold <> "" then do:
         
         ASSIGN cError  = "Invalid Sold To. Try help.". 
         RETURN.
        
    end.  
END.



  MESSAGE "add order  " prmAction .
IF prmAction = "Add" THEN DO:
    MESSAGE "enteradd "  .
    FIND LAST bf-ord WHERE bf-ord.company = prmComp  NO-LOCK NO-ERROR.
    IF AVAIL bf-ord THEN DO:
        ASSIGN    Ordernum = bf-ord.ord-no + 1.
    END.
    ELSE DO:
        ASSIGN  Ordernum = 1.
    END.
    MESSAGE "addtest" Ordernum prmAction prmComp prmCustomer .
    CREATE oe-ord.
    MESSAGE "test" Ordernum.
        ASSIGN
            oe-ord.company           = prmComp
            oe-ord.ord-no            = Ordernum
            oe-ord.cust-no           = prmCustomer  
            oe-ord.user-id           = "asi" 
            oe-ord.stat              = "O"
            oe-ord.sold-id           = prmSold 
            oe-ord.type              = prmType
            oe-ord.ord-date          = TODAY
            oe-ord.sold-name         = prmSoldName 
            oe-ord.due-code          = prmDueCode    
            oe-ord.due-date          = prmDueDate             
            oe-ord.addr[1]           = prmCustAddr   
            oe-ord.sold-addr[1]      = prmSoldAddr 
            oe-ord.last-date         = prmLastDate    
            oe-ord.addr[2]           = prmcustAddr2     
            oe-ord.sold-addr[2]      = prmSoldAddr2 
            oe-ord.prod-date         = prmProdDate 
            oe-ord.city              = prmCity
            oe-ord.state             = prmState
            /*oe-ord.zip               = prmZip*/
            oe-ord.sold-city         = prmSoldCity
            oe-ord.sold-state        = prmSoldState
            oe-ord.sold-zip          = prmSoldZip
            oe-ord.po-no             = prmPonum
            oe-ord.contact           = prmContact
            oe-ord.over-pct          = prmOverpct
            oe-ord.under-pct         = prmUnderpct
            oe-ord.terms             = prmTerms
            oe-ord.terms-d           = prmTermdscr   
            oe-ord.pord-no           = prmProd    
            oe-ord.tax-gr            = prmTaxgr    
            oe-ord.frt-pay           = prmFreight    
            oe-ord.carrier           = prmCarrier    
            oe-ord.fob-code          = prmFob    
            oe-ord.sman[1]           = prmSman    
            oe-ord.sname[1]          = prmSname    
            oe-ord.sman[2]           = prmSman2    
            oe-ord.sname[2]          = prmSname2    
            oe-ord.sman[3]           = prmSman3  
            oe-ord.sname[3]          = prmSname3  
            oe-ord.cc-type           = prmCtype  
            oe-ord.cc-expiration     = prmcExp  
            oe-ord.cc-num            = prmCnum  
            oe-ord.cc-auth           = prmCauth
            oe-ord.cust-name         = prmCustName
            oe-ord.whsed             = IF prmWhis = "yes" THEN TRUE ELSE FALSE.
            oe-ord.s-pct[1]          = 100.00.
            FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
            find sman where sman.company = oe-ord.company
                        and sman.sman = cust.sman
                        no-lock no-error.
            if avail sman then assign oe-ord.sname[1] = sman.sname
                                      oe-ord.s-comm[1]= sman.scomm.
           
       /*  FIND cust WHERE cust.company = prmComp AND cust.cust-no = prmCustomer NO-LOCK NO-ERROR.
         CREATE buff-ordl.
         assign
             buff-ordl.ord-no            = Ordernum
             buff-ordl.type-code         = oe-ord.type
             buff-ordl.company           = prmComp
             buff-ordl.LINE              = 1
             buff-ordl.cust-no           = prmCustomer  
             buff-ordl.po-no             = prmPonum
             buff-ordl.over-pct          = prmOverpct
             buff-ordl.under-pct         = prmUnderpct
             buff-ordl.req-code          = prmDueCode
             buff-ordl.req-date          = prmDueDate
             buff-ordl.prom-code         = prmDueCode
             buff-ordl.prom-date         = prmDueDate
             buff-ordl.disc              = cust.disc
             buff-ordl.tax               = cust.sort 
             buff-ordl.s-man[1]          = prmSman
             buff-ordl.s-man[2]          = prmSman2
             buff-ordl.s-man[3]          = prmSman3
             buff-ordl.s-pct[1]          = 100.
         find sman where sman.company = prmComp
                     and sman.sman = cust.sman
                     no-lock no-error.
         if avail sman then assign oe-ord.sname[1] = sman.sname
                        oe-ord.s-comm[1] = string(sman.scomm)
             .
         buff-ordl.s-comm[1] = oe-ord.s-comm[1].*/
                 
      ASSIGN prmOrderNum = Ordernum 
          prmAction = "Select".
         MESSAGE "addtest2" Ordernum prmAction.
END.

/***************************************************************************************************************/


IF prmAction = "Update" THEN DO:
    FIND FIRST oe-ord WHERE
         oe-ord.company = prmComp AND
         oe-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN DO:
        ASSIGN
            oe-ord.cust-no           = prmCustomer  
            oe-ord.user-id           = prmUserid 
            oe-ord.stat              = prmStat
            oe-ord.sold-id           = prmSold 
            oe-ord.type              = prmType
            oe-ord.ord-date          = prmOrdate
            oe-ord.sold-name         = prmSoldName 
            oe-ord.due-code          = prmDueCode    
            oe-ord.due-date          = prmDueDate             
            oe-ord.addr[1]           = prmCustAddr   
            oe-ord.sold-addr[1]      = prmSoldAddr 
            oe-ord.last-date         = prmLastDate    
            oe-ord.addr[2]           = prmCustAddr2     
            oe-ord.sold-addr[2]      = prmSoldAddr2 
            oe-ord.prod-date         = prmProdDate 
            oe-ord.city              = prmCity
            oe-ord.state             = prmState
            oe-ord.zip               = prmZip
            oe-ord.sold-city         = prmSoldCity
            oe-ord.sold-state        = prmSoldState
            oe-ord.sold-zip          = prmSoldZip
            oe-ord.po-no             = prmPonum
            oe-ord.contact           = prmContact
            oe-ord.over-pct          = prmOverpct
            oe-ord.under-pct         = prmUnderpct
            oe-ord.terms             = prmTerms
            oe-ord.terms-d           = prmTermdscr   
            oe-ord.pord-no           = prmProd    
            oe-ord.tax-gr            = prmTaxgr    
            oe-ord.frt-pay           = prmFreight    
            oe-ord.carrier           = prmCarrier    
            oe-ord.fob-code          = prmFob    
            oe-ord.sman[1]           = prmSman    
            oe-ord.sname[1]          = prmSname    
            oe-ord.sman[2]           = prmSman2    
            oe-ord.sname[2]          = prmSname2    
            oe-ord.sman[3]           = prmSman3  
            oe-ord.sname[3]          = prmSname3  
            oe-ord.cc-type           = prmCtype  
            oe-ord.cc-expiration     = prmcExp  
            oe-ord.cc-num            = prmCnum  
            oe-ord.cc-auth           = prmCauth
            oe-ord.cust-name         = prmCustName
            oe-ord.whsed             = IF prmWhis = "yes" THEN TRUE ELSE FALSE
            oe-ord.s-pct[1]          = 100.00.
            FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
            find sman where sman.company = oe-ord.company
                        and sman.sman = cust.sman
                        no-lock no-error.
            if avail sman then assign oe-ord.sname[1] = sman.sname
                                      oe-ord.s-comm[1]= sman.scomm.

        /* FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND 
                             oe-ordl.ord-no = prmOrderNum AND 
                             oe-ordl.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO: 
             assign
                 buff-ordl.cust-no           = prmCustomer  
                 buff-ordl.po-no             = prmPonum
                 buff-ordl.over-pct          = prmOverpct
                 buff-ordl.under-pct         = prmUnderpct
                 buff-ordl.frt-pay           = prmFreight    
                 /*buff-ordl.carrier           = prmCarrier    */
                 . 
           END.   /*if prmCauth    avail oe-ord*/ */
        END.  /*if  avail oe-ord*/ 
    ASSIGN prmAction = "Select".
END.

/* ***************************  Procedures  *************************** */
IF prmAction = "Copy" THEN DO:
    FIND buff-ord WHERE
         buff-ord.company EQ prmComp AND
         buff-ord.ord-no = prmOrderNum  
      NO-LOCK NO-ERROR.

    FIND LAST bf-ord WHERE bf-ord.company = prmComp  EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-ord THEN DO:
        ASSIGN    Ordernum = bf-ord.ord-no + 1.
    END.
    ELSE DO:
        ASSIGN  Ordernum = 1.
    END.
    CREATE oe-ord.
    ASSIGN
            bf-ord.company           = buff-ord.company
            bf-ord.ord-no            = Ordernum
            bf-ord.TYPE              = "O"
            bf-ord.cust-no           = buff-ord.cust-no
            bf-ord.user-id           = prmUserid 
            bf-ord.sold-id           = buff-ord.sold-id
            bf-ord.ord-date          = TODAY
            bf-ord.sold-name         = buff-ord.sold-name
            bf-ord.due-code          = buff-ord.due-code    
            bf-ord.addr[1]           = buff-ord.addr[1]       
            bf-ord.sold-addr[1]      = buff-ord.sold-addr[1]  
            bf-ord.addr[2]           = buff-ord.addr[2]       
            bf-ord.sold-addr[2]      = buff-ord.sold-addr[2]  
            bf-ord.city              = buff-ord.city          
            bf-ord.state             = buff-ord.state         
            bf-ord.zip               = buff-ord.zip           
            bf-ord.sold-city         = buff-ord.sold-city     
            bf-ord.sold-state        = buff-ord.sold-state    
            bf-ord.sold-zip          = buff-ord.sold-zip      
            bf-ord.po-no             = buff-ord.po-no         
            bf-ord.contact           = buff-ord.contact       
            bf-ord.over-pct          = buff-ord.over-pct      
            bf-ord.under-pct         = buff-ord.under-pct     
            bf-ord.terms             = buff-ord.terms         
            bf-ord.terms-d           = buff-ord.terms-d       
            bf-ord.pord-no           = buff-ord.pord-no       
            bf-ord.tax-gr            = buff-ord.tax-gr        
            bf-ord.frt-pay           = buff-ord.frt-pay       
            bf-ord.carrier           = buff-ord.carrier       
            bf-ord.fob-code          = buff-ord.fob-code      
            bf-ord.sman[1]           = buff-ord.sman[1]       
            bf-ord.sname[1]          = buff-ord.sname[1]      
            bf-ord.sman[2]           = buff-ord.sman[2]       
            bf-ord.sname[2]          = buff-ord.sname[2]      
            bf-ord.sman[3]           = buff-ord.sman[3]       
            bf-ord.sname[3]          = buff-ord.sname[3]      
            bf-ord.cc-type           = buff-ord.cc-type       
            bf-ord.cc-expiration     = buff-ord.cc-expiration 
            bf-ord.cc-num            = buff-ord.cc-num        
            bf-ord.cc-auth           = buff-ord.cc-auth       
            bf-ord.cust-name         = buff-ord.cust-name     
            . 
          CREATE buff-ordl.
             assign
                 buff-ordl.ord-no            = Ordernum
                 buff-ordl.company           = prmComp
                 buff-ordl.LINE              = 1.
             RELEASE bf-ord.
             RELEASE buff-ord.
             RELEASE buff-ordl.
ASSIGN prmOrderNum = Ordernum 
            prmAction = "Select".
       END.

/*************************************************************************************************/
IF prmAction = "Select" THEN DO:
    FIND FIRST oe-ord WHERE oe-ord.company = prmComp 
                  AND oe-ord.ord-no =  prmOrderNum  NO-LOCK NO-ERROR.
         IF AVAIL oe-ord THEN DO:
             CREATE ttUpdateOrdEntry.
             ASSIGN
              ttUpdateOrdEntry.VOrderNum      = oe-ord.ord-no
              ttUpdateOrdEntry.VEstimate      = oe-ord.est-no
              ttUpdateOrdEntry.VJob           = oe-ord.job-no 
              ttUpdateOrdEntry.VJob2          = oe-ord.job-no2
              ttUpdateOrdEntry.VCustomer      = oe-ord.cust-no
              ttUpdateOrdEntry.VUserid        = oe-ord.user-id
              ttUpdateOrdEntry.VStat          = oe-ord.stat
              ttUpdateOrdEntry.VSold          = oe-ord.sold-id
              ttUpdateOrdEntry.VOrdate        = oe-ord.ord-date
              ttUpdateOrdEntry.VCustName      = oe-ord.cust-name
              ttUpdateOrdEntry.VSoldName      = oe-ord.sold-name   
              ttUpdateOrdEntry.VDueCode       = oe-ord.due-code    
              ttUpdateOrdEntry.VDueDate       = oe-ord.due-date    
              ttUpdateOrdEntry.VCustAddr      = oe-ord.addr[1]     
              ttUpdateOrdEntry.VSoldAddr      = oe-ord.sold-addr[1]
              ttUpdateOrdEntry.VLastDate      = oe-ord.last-date   
              ttUpdateOrdEntry.VcustAddr2     = oe-ord.addr[2]     
              ttUpdateOrdEntry.VSoldAddr2     = oe-ord.sold-addr[2]
              ttUpdateOrdEntry.VProdDate      = oe-ord.prod-date   
              ttUpdateOrdEntry.VCity          = oe-ord.city        
              ttUpdateOrdEntry.VState         = oe-ord.state       
              ttUpdateOrdEntry.VZip           = oe-ord.zip         
              ttUpdateOrdEntry.VSoldCity      = oe-ord.sold-city   
              ttUpdateOrdEntry.VSoldState     = oe-ord.sold-state  
              ttUpdateOrdEntry.VSoldZip       = oe-ord.sold-zip    
              ttUpdateOrdEntry.VPonum         = oe-ord.po-no       
              ttUpdateOrdEntry.VContact       = oe-ord.contact     
              ttUpdateOrdEntry.VOverpct       = oe-ord.over-pct    
              ttUpdateOrdEntry.VUnderpct      = oe-ord.under-pct   
              ttUpdateOrdEntry.VTerms         = oe-ord.terms       
              ttUpdateOrdEntry.VTermdscr      = oe-ord.terms-d     
              ttUpdateOrdEntry.VProd          = oe-ord.pord-no     
              ttUpdateOrdEntry.VTaxgr         = oe-ord.tax-gr      
              ttUpdateOrdEntry.VFreight       = oe-ord.frt-pay     
              ttUpdateOrdEntry.VCarrier       = oe-ord.carrier     
              ttUpdateOrdEntry.VFob           = oe-ord.fob-code    
              ttUpdateOrdEntry.VSman          = oe-ord.sman[1]   
              ttUpdateOrdEntry.VSname          = oe-ord.sname[1] 
              ttUpdateOrdEntry.VSpct          = oe-ord.s-pct[1]          
              ttUpdateOrdEntry.VScomm         = oe-ord.s-comm[1]         
              ttUpdateOrdEntry.VSman2         = oe-ord.sman[2]           
              ttUpdateOrdEntry.VSname2        = oe-ord.sname[2]          
              ttUpdateOrdEntry.VSpct2         = oe-ord.s-pct[2]         
              ttUpdateOrdEntry.VScomm2        = oe-ord.s-comm[2]         
              ttUpdateOrdEntry.VSman3         = oe-ord.sman[3]          
              ttUpdateOrdEntry.VSname3        = oe-ord.sname[3]         
              ttUpdateOrdEntry.VSpct3         = oe-ord.s-pct[3]         
              ttUpdateOrdEntry.VScomm3        = oe-ord.s-comm[3]        
              ttUpdateOrdEntry.VCtype         = oe-ord.cc-type         
              ttUpdateOrdEntry.VcExp          = oe-ord.cc-expiration    
              ttUpdateOrdEntry.VCnum          = oe-ord.cc-num
              ttUpdateOrdEntry.VCauth         = oe-ord.cc-auth
              ttUpdateOrdEntry.ordtype           = oe-ord.type
             ttUpdateOrdEntry.VWhis           = oe-ord.whsed
             
             .
             

IF  oe-ord.frt-pay  = "P" THEN
         ttUpdateOrdEntry.VFreightdscr        = "Prepaid".
         ELSE IF oe-ord.frt-pay  = "C" THEN 
             ttUpdateOrdEntry.VFreightdscr        = "Collect".
             ELSE IF oe-ord.frt-pay  = "B" THEN
                 ttUpdateOrdEntry.VFreightdscr       = "Bill".
                 ELSE IF oe-ord.frt-pay  = "T" THEN
                     ttUpdateOrdEntry.VFreightdscr        = "3rd Party".
     IF  oe-ord.fob-code  = "DEST" THEN
         ttUpdateOrdEntry.VFob        = "Destination".
         ELSE IF oe-ord.fob-code  = "ORIG" THEN 
             ttUpdateOrdEntry.VFob       = "Origin".
         
         /*FIND FIRST carrier WHERE carrier.company = oe-ord.company AND carrier.loc = oe-ord.loc
             AND carrier.carrier = oe-ord.carrier NO-LOCK NO-ERROR.
             assign                                     
                  ttUpdateOrdEntry.Carrdscr       = carrier.dscr. */
             
         END.
         IF VAction = "Add" OR VAction = "Add" THEN DO:
               ASSIGN 
                    ttUpdateOrdEntry.VRowid = RECID(oe-ord). 
         END.


END.  /*if prmAction*/
/* ***************************  Main Block  *************************** */
                           


