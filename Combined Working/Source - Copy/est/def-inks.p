
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.   
DEF INPUT PARAM ip-ink AS DEC NO-UNDO.
DEF INPUT PARAM ip-ips AS DEC NO-UNDO.
DEF INPUT PARAM ip-vrn AS DEC NO-UNDO.
DEF INPUT PARAM ip-vps AS DEC NO-UNDO.
    
{sys/inc/var.i NEW SHARED}

DEF BUFFER alt-item FOR item.

DEF VAR li1 AS INT NO-UNDO.
DEF VAR li2 AS DEC NO-UNDO.
DEF VAR li3 AS INT NO-UNDO.
DEF VAR li4 AS INT NO-UNDO.

{est/inksvarn.i}


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef NO-LOCK
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-ERROR.

IF AVAIL ef THEN DO:
  ASSIGN
   cocode = ef.company
   locode = ef.loc.

  FOR EACH inks:
    FIND FIRST item NO-LOCK
        WHERE item.company EQ eb.company
          AND item.i-no    EQ inks.cd[1]
          AND inks.cd[1]   NE ""
        NO-ERROR.
    IF AVAIL item THEN inks.iv = item.mat-type.
    ELSE DELETE inks.
  END.

  FIND FIRST style
      {sys/ref/styleW.i}
        AND style.style EQ eb.style
      NO-LOCK NO-ERROR.
  IF AVAIL style THEN DO:
    IF li1 EQ 0 THEN li1 = INT(style.material[3]).

    IF style.material[2] NE "" THEN
    FIND FIRST item
        {sys/look/itemiW.i}
          AND item.i-no EQ style.material[2]
        NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN li1 = INT(style.material[3]).


    IF style.material[6] NE "" THEN
    FIND FIRST alt-item
        WHERE alt-item.company  EQ cocode
          AND alt-item.mat-type EQ "V"
          AND alt-item.i-no     EQ style.material[6]
        NO-LOCK NO-ERROR.
  END.

  IF NOT AVAIL item OR NOT AVAIL alt-item OR (li1 EQ 0) THEN DO:
    FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

    IF li1 EQ 0 THEN li1 = ce-ctrl.def-inkcov.

    IF NOT AVAIL item THEN
    FIND FIRST ITEM
        {sys/look/itemiW.i}
          AND item.i-no EQ ce-ctrl.def-ink
        NO-LOCK NO-ERROR.

    IF NOT AVAIL alt-item THEN
    FIND FIRST alt-item
        WHERE alt-item.company  EQ cocode
          AND alt-item.mat-type EQ "V"
          AND alt-item.i-no     EQ ce-ctrl.def-coat
        NO-LOCK NO-ERROR.
  END.

  IF NOT AVAIL item AND ef.m-code NE "" THEN DO:
    FIND FIRST mach
        WHERE mach.company  EQ cocode
          AND mach.m-code   EQ ef.m-code
          AND mach.dept[1]  EQ "PR"
        NO-LOCK NO-ERROR.
    IF AVAIL mach THEN
    FIND FIRST item
        {sys/look/itemivW.i}
          AND item.press-type EQ mach.pr-type
        NO-LOCK NO-ERROR.
  END.

  IF NOT AVAIL item THEN
  FIND FIRST item {sys/look/itemiW.i} NO-LOCK NO-ERROR.

  IF NOT AVAIL alt-item THEN
  FIND FIRST alt-item
      WHERE alt-item.company  EQ cocode
        AND alt-item.mat-type EQ "V"
      NO-LOCK NO-ERROR.

  ASSIGN
   li2 = ip-ink / ip-ips
   li3 = 1.

  {sys/inc/roundup.i li2}

  li4 = 0.
  FOR EACH inks WHERE inks.iv EQ "I":
    li4 = li4 + 1.
    IF li4 GT ip-ink THEN DELETE inks.
    ELSE DO:
      inks.ps[1] = li3.

      IF li4 MODULO li2 EQ 0 THEN li3 = li3 + 1.
      IF li3 GT ip-ips THEN li3 = ip-ips.
    END.
  END.

  IF AVAIL item THEN
  DO li4 = li4 + 1 TO ip-ink:
    CREATE inks.
    ASSIGN
     inks.ps[1] = li3
     inks.cd[1] = item.i-no
     inks.ds[1] = item.i-name
     inks.pc[1] = li1
     inks.side  = "F"
     inks.iv    = item.mat-type.

    IF li4 MODULO li2 EQ 0 THEN li3 = li3 + 1.
    IF li3 GT ip-ips THEN li3 = ip-ips.
  END.

  ASSIGN
   li2 = ip-vrn / ip-vps
   li3 = 1.

  {sys/inc/roundup.i li2}

  li4 = 0.
  FOR EACH inks WHERE inks.iv EQ "V":
    li4 = li4 + 1.
    IF li4 GT ip-vrn THEN DELETE inks.
    ELSE DO:
      inks.ps[1] = li3.

      IF li4 MODULO li2 EQ 0 THEN li3 = li3 + 1.
      IF li3 GT ip-vps THEN li3 = ip-vps.
    END.
  END.

  IF AVAIL alt-item THEN
  DO li4 = li4 + 1 TO ip-vrn:
    CREATE inks.
    ASSIGN
     inks.ps[1] = li3
     inks.cd[1] = alt-item.i-no
     inks.ds[1] = alt-item.i-name
     inks.pc[1] = 100
     inks.side  = "F"
     inks.iv    = alt-item.mat-type.

    IF li4 MODULO li2 EQ 0 THEN li3 = li3 + 1.
    IF li3 GT ip-vps THEN li3 = ip-vps.
  END.
END.
