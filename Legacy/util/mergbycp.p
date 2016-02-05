
{sys/inc/var.i NEW SHARED}

DEF VAR v-new-item LIKE itemfg.i-no.


SESSION:SET-WAIT-STATE ("general").

FOR EACH itemfg 
   WHERE cust-no NE ""
     AND part-no NE ""
     AND isaset  EQ NO
   NO-LOCK 
   BREAK BY company
         BY cust-no
         BY part-no
         BY i-no:

  IF FIRST-OF(part-no) THEN v-new-item = i-no.

  ELSE DO:
    STATUS DEFAULT "Cust#/Part#/OldFG#/NewFG#: " +
                   TRIM(cust-no) + "/" + TRIM(part-no) +
                   TRIM(i-no) + "/" + TRIM(v-new-item).
    cocode = company.
    RUN fg/updfgitm.p (RECID(itemfg), v-new-item).
    STATUS DEFAULT "".
  END.
END.

SESSION:SET-WAIT-STATE ("").
