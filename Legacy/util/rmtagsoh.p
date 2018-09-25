
{sys/inc/var.i NEW SHARED}

DEF TEMP-TABLE tt-bin NO-UNDO FIELD tt-rowid AS ROWID
                              FIELD tt-company LIKE rm-bin.company
                              FIELD tt-i-no LIKE rm-bin.i-no
                              INDEX tt-bin tt-company tt-i-no.

DEF VAR ll AS LOG NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH company NO-LOCK,
    EACH rm-bin NO-LOCK
    WHERE rm-bin.company EQ company.company
      AND rm-bin.tag     GT ""
      AND rm-bin.i-no    NE ""
      AND rm-bin.qty     NE 0
      AND CAN-FIND(FIRST rm-rdtlh
                   WHERE rm-rdtlh.company   EQ rm-bin.company
                     AND rm-rdtlh.tag       EQ rm-bin.tag
                     AND rm-rdtlh.rita-code EQ "I"
                   USE-INDEX tag)
    WITH FRAME f1:

  DISPLAY rm-bin.company    FORMAT "x(5)"       COLUMN-LABEL "Company"
          rm-bin.i-no       FORMAT "x(13)"      COLUMN-LABEL "RM Item#"
          rm-bin.tag        FORMAT "x(20)"      COLUMN-LABEL "Tag"
          rm-bin.qty        FORMAT "->>>,>>>,>>9.9<<<<"
                                                COLUMN-LABEL "Qty".

  CREATE tt-bin.
  ASSIGN
   tt-rowid   = ROWID(rm-bin)
   tt-company = rm-bin.company
   tt-i-no    = rm-bin.i-no.
END.

SESSION:SET-WAIT-STATE ("").

IF CAN-FIND(FIRST tt-bin) THEN
  MESSAGE "Listing complete, would you like to set these bins to 0 Qty On-Hand?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.

SESSION:SET-WAIT-STATE ("general").

IF ll THEN
FOR EACH tt-bin,
    FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid
    BREAK BY tt-company
          BY tt-i-no:

  cocode = rm-bin.company.

  RUN rm/cre-tran.p (tt-rowid, "C", 0,"").

  IF LAST-OF(tt-i-no) THEN DO:
    FIND FIRST item NO-LOCK
        WHERE item.company EQ tt-company
          AND item.i-no    EQ tt-i-no
        NO-ERROR.
    IF AVAIL item THEN DO:
      RUN rm/rm-mkbin.p (RECID(item)).
      RUN rm/rm-reset.p (RECID(item)).
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").

MESSAGE "Program complete..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

HIDE FRAME f1 NO-PAUSE.
