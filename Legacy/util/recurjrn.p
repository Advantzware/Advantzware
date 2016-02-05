
DEF BUFFER b-gl1 FOR gl-jrn.
DEF BUFFER b-gl2 FOR gl-jrnl.

FOR EACH company,
    EACH gl-jrn
    WHERE gl-jrn.company EQ company.company
      AND posted         EQ NO
      AND tr-date        EQ 12/31/9999
      AND journal        EQ 0
      AND period         EQ 0
      AND recur          EQ NO:

  CREATE b-gl1.
  BUFFER-COPY gl-jrn EXCEPT j-no journal rec_key TO b-gl1
  ASSIGN
   b-gl1.recur = YES.

  FOR EACH gl-jrnl OF gl-jrn:
    CREATE b-gl2.
    BUFFER-COPY gl-jrnl EXCEPT rec_key TO b-gl2
    ASSIGN
     b-gl2.j-no = b-gl1.j-no.
    DELETE gl-jrnl.
  END.

  DELETE gl-jrn.
END.

MESSAGE "Utility Completed..." VIEW-AS ALERT-BOX.
