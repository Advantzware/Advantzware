&Scoped-define ACTION DELETE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME e-itemfg-vend

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
{methods/triggers/delete.i}

FIND FIRST reftable WHERE
     reftable.reftable EQ 'e-itemfg-vend.markup' AND
     reftable.company EQ e-itemfg-vend.company AND
     reftable.loc EQ e-itemfg-vend.i-no AND
     reftable.code = e-itemfg-vend.vend-no
     EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.

IF NOT CAN-FIND(FIRST bf-e-itemfg-vend WHERE bf-e-itemfg-vend.company = e-itemfg-vend.company
                 AND bf-e-itemfg-vend.est-no = e-itemfg-vend.est-no
                 AND bf-e-itemfg-vend.eqty = e-itemfg-vend.eqty
                 AND bf-e-itemfg-vend.form-no = e-itemfg-vend.form-no
                 AND bf-e-itemfg-vend.blank-no = e-itemfg-vend.blank-no
                 AND RECID(bf-e-itemfg-vend) <> RECID(e-itemfg-vend))
THEN DO:

  FIND FIRST reftable WHERE
     reftable.reftable EQ "e-itemfg-vend.std-uom" AND
     reftable.company  EQ e-itemfg-vend.company AND
     reftable.loc      EQ "" AND
     reftable.code     EQ e-itemfg-vend.est-no AND
     reftable.val[1]   EQ e-itemfg-vend.form-no AND
     reftable.val[2]   EQ e-itemfg-vend.blank-no
     EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL reftable THEN
     DELETE reftable.

END.
