


/*------------------------------------------------------------------------
    File        : CreditstatusInv.p
    Purpose     : Credit Status Invoice

    Syntax      :

    Description : Return a Dataset of all Order Inquiry invoice

    Author(s)   : Kuldeep
    Created     : DEC 20 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{CreditStatusInv.i}

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCreditStatusInv.

IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".

DEF VAR prmComp AS CHAR no-undo.
DEF VAR fi_last-ord-date AS DATE NO-UNDO.
DEF VAR fi_frst-ord-date AS DATE NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */
     
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.


prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST cust WHERE
    cust.company EQ prmComp AND
    cust.cust-no = prmCust  NO-LOCK NO-ERROR.
    
    /*first two for each loops are wrong I think, need leave statements, have to be in each ar-inv loop*/
        FOR EACH ar-inv OF cust WHERE ar-inv.due > 0 NO-LOCK :
        create ttCreditStatusInv.
        ASSIGN
            ttCreditStatusInv.vCustNo           = cust.cust-no
            ttCreditStatusInv.vCustName         = cust.NAME
            ttCreditStatusInv.vCustHibal        = cust.hibal
            ttCreditStatusInv.vCustHibalDate    = cust.hibal-date
            ttCreditStatusInv.vCustCrHold       = cust.cr-hold
            ttCreditStatusInv.vCustLpay         = cust.lpay
            ttCreditStatusInv.vCustLpayDate     = cust.lpay-date
            ttCreditStatusInv.vCustCrLim        = cust.cr-lim
            ttCreditStatusInv.vAccBal           = cust.acc-bal
            ttCreditStatusInv.vCustOrdlim       = cust.ord-lim
            ttCreditStatusInv.vCustOrdBal       = cust.ord-bal
            ttCreditStatusInv.vOnAccount        = cust.on-account  
            ttCreditStatusInv.bInvDate       = ar-inv.inv-date
            ttCreditStatusInv.bInvNo         = ar-inv.inv-no
            ttCreditStatusInv.ld-inv-amt     = ar-inv.paid + ar-inv.due
            ttCreditStatusInv.bBalDue        = ar-inv.due 
            ttCreditStatusInv.li-days-old    = TODAY - ar-inv.inv-date
            .
    
     for each oe-ord where oe-ord.company eq cust.company
                      and oe-ord.cust-no eq cust.cust-no
                      use-index cust no-lock by oe-ord.ord-date:
        ASSIGN ttCreditStatusInv.filastorddate = oe-ord.ord-date.
    end.
    for each oe-ord where oe-ord.company eq cust.company
                      and oe-ord.cust-no eq cust.cust-no
                      use-index cust no-lock by oe-ord.ord-date desc:
        ASSIGN ttCreditStatusInv.fifrstorddate = oe-ord.ord-date.
    end.
    END. /*if avail ar-inv*/

