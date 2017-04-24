&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME vend

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

IF vend.acc-bal <> 0 THEN DO:
   MESSAGE "Vendor's Balance is not 0. Can't delete."
       VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.
FIND FIRST ap-inv WHERE ap-inv.company = vend.company 
                    AND ap-inv.vend-no = vend.vend-no
                    AND ap-inv.due <> 0 NO-LOCK NO-ERROR.
IF AVAIL ap-inv THEN do:
   MESSAGE "Vendor has outstanding invoices. Can't delete vendor."
        VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

FIND FIRST ap-inv WHERE ap-inv.company = vend.company 
                    AND ap-inv.vend-no = vend.vend-no
                    AND ap-inv.posted NO-LOCK NO-ERROR.

IF AVAIL ap-inv THEN do:
   MESSAGE "Vendor has posted invoices. Can't delete vendor."
        VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "vend.poexport"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {&TABLENAME}.vend-no:
  DELETE reftable.
END.
