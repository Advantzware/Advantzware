/* wiptag.i */

FIND FIRST reftable WHERE
     reftable.reftable = "WIPLEN" AND
     reftable.company = wiptag.company AND
     reftable.CODE = wiptag.tag-no
     USE-INDEX CODE
     NO-ERROR.

IF AVAIL reftable THEN
   DELETE reftable.
