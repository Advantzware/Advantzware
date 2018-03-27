&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME itemfg

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF BUFFER set-{&TABLENAME} FOR {&TABLENAME}.
    
DEF VAR v-msg AS CHAR INIT "" NO-UNDO.
DEF VAR choice AS LOG INIT NO NO-UNDO.

IF {&TABLENAME}.i-no NE "" THEN DO:

  {delete.trg\{&TABLENAME}.i &fil=eb        &fld=stock-no   &msg="Estimate"}
  
  {delete.trg\{&TABLENAME}.i &fil=job-hdr   &fld=i-no       &msg="Job"}

  {delete.trg\{&TABLENAME}.i &fil=oe-ordl   &fld=i-no       &msg="Order"}

/* wfk - 05161319 - unindexed search, if oe-ordl is not there, oe-rel should not */
/*  {delete.trg\{&TABLENAME}.i &fil=oe-rel    &fld=i-no       &msg="Scheduled Release"} */

  {delete.trg\{&TABLENAME}.i &fil=oe-rell   &fld=i-no       &msg="Actual Release"}

  {delete.trg\{&TABLENAME}.i &fil=oe-boll   &fld=i-no       &msg="BOL"}

  {delete.trg\{&TABLENAME}.i &fil=inv-line  &fld=i-no       &msg="Unposted Invoice"}

  {delete.trg\{&TABLENAME}.i &fil=ar-invl   &fld=i-no       &msg="Posted Invoice"}
END.

IF v-msg NE "" THEN DO:
  MESSAGE v-msg VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

delete-children: DO TRANSACTION.
  IF {&TABLENAME}.i-no NE "" THEN
  FOR EACH fg-set
      WHERE fg-set.company EQ {&TABLENAME}.company
        AND fg-set.part-no EQ {&TABLENAME}.i-no
      BREAK BY fg-set.part-no:

    IF FIRST(fg-set.part-no) THEN DO:
      choice = NO.
      MESSAGE "This item is part of a set, delete anyway?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE choice.
      IF NOT choice THEN UNDO delete-children, RETURN ERROR.
    END.

    DELETE fg-set.
  END.

  IF {&TABLENAME}.i-no NE "" THEN
  FOR EACH fg-set
      WHERE fg-set.company EQ {&TABLENAME}.company
        AND fg-set.set-no  EQ {&TABLENAME}.i-no
      BREAK BY fg-set.set-no:

    IF FIRST(fg-set.set-no) THEN DO:
      choice = NO.
      MESSAGE "This item is a set, delete all components?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE choice.
    END.

    IF choice THEN DO:
      FIND FIRST set-{&TABLENAME}
          WHERE set-{&TABLENAME}.company EQ {&TABLENAME}.company
            AND set-{&TABLENAME}.i-no    EQ fg-set.part-no  
          NO-ERROR.
      IF AVAIL set-{&TABLENAME} THEN DO:
        DISABLE TRIGGERS FOR LOAD OF set-{&TABLENAME}.
        RUN fg/delkids.p (BUFFER set-{&TABLENAME}).
        DELETE set-{&TABLENAME}.
      END.
    END.

    DELETE fg-set.
  END.  /* for each fg-set */
  RUN fg/delkids.p (BUFFER {&TABLENAME}).
END.  /* delete-children */
