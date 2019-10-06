/* terms.i */

FIND FIRST reftable EXCLUSIVE-LOCK
     WHERE reftable.reftable EQ 'terms.cod'
       AND reftable.company EQ terms.company
       AND reftable.loc EQ ''
       AND reftable.code EQ terms.t-code NO-ERROR.
IF AVAILABLE reftable THEN DELETE reftable.
