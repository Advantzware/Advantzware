
/*------------------------------------------------------------------------
    File        : fixNoCust.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Apr 23 10:28:07 EDT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF cust.
DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

DEF BUFFER bf-cust FOR cust.

MESSAGE "Enter Company #" UPDATE cCompany
.
MESSAGE "Enter Cust#" UPDATE cCustNo
.
MESSAGE "Enter Customer Name" UPDATE cName 
.

FIND FIRST bf-cust WHERE bf-cust.company = cCompany AND bf-cust.active = "X" NO-LOCK NO-ERROR.

FIND FIRST cust WHERE cust.company EQ cCompany 
  AND cust.cust-no = cCustNo
  NO-LOCK NO-ERROR.
IF AVAIL cust AND cust.active EQ "A" THEN DO:
    MESSAGE "Customer already exists."
    VIEW-AS ALERT-BOX.
    RETURN.
END.

IF AVAIL cust AND cust.active NE "A" THEN DO:
message "Activate this customer??" view-as alert-box question
          button yes-no update ll-ans as log.    
  IF ll-ans THEN DO:  
    FIND FIRST cust WHERE cust.company EQ cCompany 
      AND cust.cust-no = cCustNo
      EXCLUSIVE-LOCK NO-ERROR.
    cust.active = "A".
  END.
  RETURN.
END.
CREATE cust.
ASSIGN cust.company = cCompany
       cust.cust-no = cCustNo
       cust.name = cName
       cust.active = "A".
IF AVAIL bf-cust THEN 
  ASSIGN cust.ch-type = bf-cust.ch-type
         cust.sell-by = bf-cust.sell-by
         cust.frt-pay = bf-cust.frt-pay
         cust.stat-grp = bf-cust.stat-grp.
ELSE
  ASSIGN cust.ch-type = "O"
         cust.sell-by = "N"
         cust.frt-pay = "P"
         cust.stat-grp = "W".

cust.rec_key = DYNAMIC-FUNCTION("sfGetNextRecKey").

CREATE rec_key.
ASSIGN
 rec_key.rec_key    = cust.rec_key
 rec_key.table_name = "cust".
 MESSAGE "Customer record has been created."
