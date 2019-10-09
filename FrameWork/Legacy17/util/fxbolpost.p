{custom/gcompany.i}    
{custom/gloc.i}
{custom/globdefs.i NEW}

DEF VAR cocode AS CHAR.
DEF VAR locode AS CHAR.

RUN custom/getcomp.p.
assign cocode = g_company
       locode = g_loc.


DEF VAR vBolNo LIKE oe-bolh.bol-no NO-UNDO.

UPDATE SKIP(1) SPACE(4) vBolNo LABEL "BOL #" space(4) SKIP(1)
    WITH FRAME X CENTERED TITLE "Fix BOL Incorrectly Set to Posted" 
    SIDE-LABELS WIDTH 50 ROW 5.

FIND FIRST oe-bolh WHERE oe-bolh.company = cocode
                     AND oe-bolh.bol-no  = vBolNo
                   EXCLUSIVE-LOCK NO-ERROR. 
IF AVAIL oe-bolh AND oe-bolh.posted = YES THEN DO:
  oe-bolh.posted = NO.
  MESSAGE "BOL Status was reset to not posted"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.
ELSE DO:

    MESSAGE "Posted BOL not found!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

HIDE ALL NO-PAUSE.
