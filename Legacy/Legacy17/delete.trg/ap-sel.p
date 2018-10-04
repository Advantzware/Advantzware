&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME ap-sel

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}
/*
 FIND FIRST ap-inv WHERE ap-inv.company = ap-sel.company
                        AND ap-inv.vend-no = ap-sel.vend-no
                        AND ap-inv.inv-no = ap-sel.inv-no
                        AND ap-inv.posted = YES
                        NO-ERROR.
IF AVAIL ap-inv THEN ASSIGN ap-inv.due = ap-inv.due + ap-sel.amt-paid + ap-sel.disc-amt
                            ap-inv.paid = ap-inv.paid - ap-sel.amt-paid.

*/
