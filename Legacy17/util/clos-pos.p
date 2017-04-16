
SESSION:SET-WAIT-STATE ("general").

FOR EACH po-ord WHERE opened EQ YES NO-LOCK,
    FIRST po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no   EQ po-ord.po-no NO-LOCK:

  RUN po/closechk.p (ROWID(po-ordl)).
END.

SESSION:SET-WAIT-STATE ("").
