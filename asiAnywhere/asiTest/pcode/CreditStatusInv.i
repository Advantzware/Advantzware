
DEFINE TEMP-TABLE ttCreditStatusInv NO-UNDO
BEFORE-TABLE beforeCreditStatusInv
     FIELD vCustNo         LIKE cust.cust-no
     FIELD vCustName       LIKE cust.NAME
     FIELD vCustHibal      LIKE cust.hibal
     FIELD vCustHibalDate  LIKE cust.hibal-date
     FIELD vCustCrHold     LIKE cust.cr-hold
     FIELD vCustLpay       LIKE cust.lpay
     FIELD vCustLpayDate   LIKE cust.lpay-date
     FIELD vCustCrLim      LIKE cust.cr-lim
     FIELD vAccBal         LIKE cust.acc-bal
     FIELD vCustOrdlim     LIKE cust.ord-lim
     FIELD vCustOrdBal     LIKE cust.ord-bal
     FIELD vOnAccount      LIKE cust.on-account 
     FIELD fifrstorddate      AS DATE FORMAT "99/99/9999" 
     FIELD filastorddate      AS DATE FORMAT "99/99/9999"
     FIELD bInvDate        LIKE ar-inv.inv-date FORMAT "99/99/9999"
     FIELD bInvNo          LIKE  ar-inv.inv-no FORMAT ">>>>>>>>"
     FIELD ld-inv-amt      AS DECIMAL FORMAT "->>,>>>,>>9.99"
     FIELD bBalDue         LIKE ar-inv.due FORMAT "->>,>>>,>>9.99"
     FIELD li-days-old     AS INTEGER FORMAT "->>,>>>"
      
      
        
        . 
DEFINE DATASET dsCreditStatusInv FOR ttCreditStatusInv .
DEFINE QUERY q-CreditStatusInvQuery FOR ttCreditStatusInv.
DEFINE DATA-SOURCE src-CreditStatusInv  FOR QUERY q-CreditStatusInvQuery.
BUFFER ttCreditStatusInv :ATTACH-DATA-SOURCE(DATA-SOURCE src-CreditStatusInv  :HANDLE).







