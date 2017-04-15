/* Add scores data automatically when a PO line is added */  
DEF INPUT PARAMETER ipr-po-ordl AS ROWID.
DEF BUFFER b-Ref1 FOR ref.
DEF BUFFER b-Ref2 FOR ref.
DEF VAR cocode AS CHAR.

FIND po-ordl WHERE ROWID(po-ordl) = ipr-po-ordl EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL po-ordl THEN DO:

    /* Add Scores Data - copied from po/d-scores.w */
    cocode = po-ordl.company. 
    {po/po-ordls.i}
  
    {po/poordls2W.i}

    IF AVAIL b-ref1 THEN DELETE b-ref1.
    IF AVAIL b-ref2 THEN DELETE b-ref2.
    /* Note: po-ordlw.p was created because Joe insisted on not touching the code
             in po/po-ordls.p */
    IF po-ordl.spare-char-1 = "Length" THEN
      RUN po/po-ordlw.p (RECID(po-ordl), "Length").
    ELSE
      RUN po/po-ordls.p (RECID(po-ordl)).

    {po/po-ordls.i}  
    {po/poordls2W.i}
END.

