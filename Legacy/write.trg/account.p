&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME account

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

  ASSIGN account.n1 = 0
         account.n2 = 0
         account.n3 = 0
         account.n4 = 0
         account.n5 = 0.

  FIND FIRST company WHERE company.company = account.company NO-LOCK NO-ERROR.
  IF AVAILABLE company THEN DO:
    ASSIGN account.n1 = int(SUBSTRING(account.actnum,1,company.acc-dig[1])).
    IF company.acc-level > 1 THEN
        account.n2 = int(SUBSTRING(account.actnum,company.acc-dig[1] + 2, company.acc-dig[2])).
    IF company.acc-level > 2 THEN
        account.n3 = int(SUBSTRING(account.actnum,company.acc-dig[1] + company.acc-dig[2] + 3,company.acc-dig[3])).
    IF company.acc-level > 3 THEN
        account.n4 = int(SUBSTRING(account.actnum,company.acc-dig[1] + company.acc-dig[2] + company.acc-dig[3] + 4,company.acc-dig[4])).
    IF company.acc-level > 4 THEN
        account.n5 = int(SUBSTRING(account.actnum,company.acc-dig[1] + company.acc-dig[2] + company.acc-dig[3] + company.acc-dig[4] + 5,company.acc-dig[5])).
  END.

