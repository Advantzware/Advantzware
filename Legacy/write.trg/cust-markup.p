&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME cust-markup

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE w-{&TABLENAME} FIELD run-qty   AS DEC
                              FIELD markup-on AS CHAR
                              FIELD markup    AS DEC.

DO li = 1 TO EXTENT({&TABLENAME}.run-qty):
  IF {&TABLENAME}.run-qty[li] NE 0 THEN DO:
    CREATE w-{&TABLENAME}.
    ASSIGN
     w-{&TABLENAME}.run-qty   = {&TABLENAME}.run-qty[li]
     w-{&TABLENAME}.markup-on = {&TABLENAME}.markup-on[li]
     w-{&TABLENAME}.markup    = {&TABLENAME}.markup[li].
  END.
END.

ASSIGN
 {&TABLENAME}.run-qty   = 0
 {&TABLENAME}.markup-on = ""
 {&TABLENAME}.markup    = 0
 li                     = 0.

FOR EACH w-{&TABLENAME}
    WHERE w-{&TABLENAME}.run-qty GT 0
    BREAK BY w-{&TABLENAME}.run-qty: 

  li = li + 1.

  IF li LE EXTENT({&TABLENAME}.run-qty) THEN
    ASSIGN                                                              
     {&TABLENAME}.run-qty[li]   = IF LAST(w-{&TABLENAME}.run-qty) THEN 999999999.9
                                  ELSE w-{&TABLENAME}.run-qty
     {&TABLENAME}.markup-on[li] = w-{&TABLENAME}.markup-on
     {&TABLENAME}.markup[li]    = w-{&TABLENAME}.markup.
END.

ASSIGN
 {&TABLENAME}.upd-date = TODAY
 {&TABLENAME}.upd-time = TIME.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
