
{sys/inc/var.i shared}
{sys/form/s-top.f}

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef FOR ef.
DEF NEW SHARED BUFFER xeb FOR eb.

DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

{ce/print4.i "new shared" "new shared"}
{ce/print42.i "new shared"}

DEFINE INPUT PARAMETER ip-est-qty AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-est-no AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-all-forms-ink.

EMPTY TEMP-TABLE tt-all-forms-ink.

qty = ip-est-qty.

FIND FIRST xest WHERE
     xest.company EQ cocode AND
     xest.est-no EQ ip-est-no
     NO-LOCK.

find first xef
      where xef.company eq cocode
        and xef.est-no  eq xest.est-no
      no-lock.
  
  find first xeb
      where xeb.company eq cocode
        and xeb.est-no  eq xest.est-no
        and xeb.form-no eq 1
      no-lock.

RUN cec/box/print42.p.

FOR EACH xef WHERE
    xef.company EQ xest.company AND
    xef.est-no EQ xest.est-no
    NO-LOCK,
    each xeb where
        xeb.company = xest.company AND
        xeb.est-no = xest.est-no AND
        xeb.form-no = xef.form-no
        NO-LOCK:
    
        do i = 1 to 10:
           if xeb.i-code[i] ne "" and
              can-find(first ITEM WHERE
              item.company eq xeb.company AND
              item.i-no    eq xeb.i-code[i]) THEN
              DO:
                 FIND FIRST tt-all-forms-ink WHERE
                      tt-all-forms-ink.i-code eq xeb.i-code[i]
                      NO-ERROR.
                   
                 IF NOT AVAIL tt-all-forms-ink THEN
                 do:
                    create tt-all-forms-ink.
                    tt-all-forms-ink.i-code = xeb.i-code[i].
                 END.
           END.
        END.
END.

FOR EACH tt-all-forms-ink,
    EACH brd WHERE
         brd.i-no EQ tt-all-forms-ink.i-code:

    tt-all-forms-ink.i-qty = tt-all-forms-ink.i-qty + brd.qty.
END.
