
DEF BUFFER alt-item FOR item.

DEF VAR li1 AS INT NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.
DEF VAR li3 AS INT NO-UNDO.
DEF VAR li4 AS INT NO-UNDO.
DEF VAR lv-rowid1 AS ROWID NO-UNDO.
DEF VAR lv-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-i-ps LIKE eb.i-ps NO-UNDO.
DEF VAR lv-i-code LIKE eb.i-code NO-UNDO.
DEF VAR lv-i-dscr LIKE eb.i-dscr NO-UNDO.
DEF VAR lv-i-% LIKE eb.i-% NO-UNDO.


   IF DEC(eb.i-col:SCREEN-VALUE)  GT 0 OR
      DEC(eb.i-coat:SCREEN-VALUE) GT 0 THEN DO:

     ASSIGN
      lv-i-ps[01]   = DEC(eb.i-ps[01]:SCREEN-VALUE)
      lv-i-code[01] = eb.i-code[01]:SCREEN-VALUE
      lv-i-dscr[01] = eb.i-dscr[01]:SCREEN-VALUE
      lv-i-%[01]    = DEC(eb.i-%[01]:SCREEN-VALUE)
      lv-i-ps[02]   = DEC(eb.i-ps[02]:SCREEN-VALUE)
      lv-i-code[02] = eb.i-code[02]:SCREEN-VALUE
      lv-i-dscr[02] = eb.i-dscr[02]:SCREEN-VALUE
      lv-i-%[02]    = DEC(eb.i-%[02]:SCREEN-VALUE)
      lv-i-ps[03]   = DEC(eb.i-ps[03]:SCREEN-VALUE)
      lv-i-code[03] = eb.i-code[03]:SCREEN-VALUE
      lv-i-dscr[03] = eb.i-dscr[03]:SCREEN-VALUE
      lv-i-%[03]    = DEC(eb.i-%[03]:SCREEN-VALUE)
      lv-i-ps[04]   = DEC(eb.i-ps[04]:SCREEN-VALUE)
      lv-i-code[04] = eb.i-code[04]:SCREEN-VALUE
      lv-i-dscr[04] = eb.i-dscr[04]:SCREEN-VALUE
      lv-i-%[04]    = DEC(eb.i-%[04]:SCREEN-VALUE)
      lv-i-ps[05]   = DEC(eb.i-ps[05]:SCREEN-VALUE)
      lv-i-code[05] = eb.i-code[05]:SCREEN-VALUE
      lv-i-dscr[05] = eb.i-dscr[05]:SCREEN-VALUE
      lv-i-%[05]    = DEC(eb.i-%[05]:SCREEN-VALUE)
      lv-i-ps[06]   = DEC(eb.i-ps[06]:SCREEN-VALUE)
      lv-i-code[06] = eb.i-code[06]:SCREEN-VALUE
      lv-i-dscr[06] = eb.i-dscr[06]:SCREEN-VALUE
      lv-i-%[06]    = DEC(eb.i-%[06]:SCREEN-VALUE)
      lv-i-ps[07]   = DEC(eb.i-ps[07]:SCREEN-VALUE)
      lv-i-code[07] = eb.i-code[07]:SCREEN-VALUE
      lv-i-dscr[07] = eb.i-dscr[07]:SCREEN-VALUE
      lv-i-%[07]    = DEC(eb.i-%[07]:SCREEN-VALUE)
      lv-i-ps[08]   = DEC(eb.i-ps[08]:SCREEN-VALUE)
      lv-i-code[08] = eb.i-code[08]:SCREEN-VALUE
      lv-i-dscr[08] = eb.i-dscr[08]:SCREEN-VALUE
      lv-i-%[08]    = DEC(eb.i-%[08]:SCREEN-VALUE)
      lv-i-ps[09]   = DEC(eb.i-ps[09]:SCREEN-VALUE)
      lv-i-code[09] = eb.i-code[09]:SCREEN-VALUE
      lv-i-dscr[09] = eb.i-dscr[09]:SCREEN-VALUE
      lv-i-%[09]    = DEC(eb.i-%[09]:SCREEN-VALUE)
      lv-i-ps[10]   = DEC(eb.i-ps[10]:SCREEN-VALUE)
      lv-i-code[10] = eb.i-code[10]:SCREEN-VALUE
      lv-i-dscr[10] = eb.i-dscr[10]:SCREEN-VALUE
      lv-i-%[10]    = DEC(eb.i-%[10]:SCREEN-VALUE).

     FIND FIRST style
         {sys/ref/styleW.i}
           AND style.style EQ eb.style
         NO-LOCK NO-ERROR.
     IF AVAIL style THEN DO:
       IF li1 EQ 0 THEN li1 = INT(style.material[3]).

       RELEASE ITEM.

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
             AND mach.loc      EQ locode
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
      lv-rowid1 = IF AVAIL(item) THEN ROWID(item) ELSE ?
      lv-rowid2 = IF AVAIL(alt-item) THEN ROWID(alt-item) ELSE ?
      li2       = (DEC(eb.i-col:SCREEN-VALUE) + DEC(eb.i-coat:SCREEN-VALUE)) /
                  DEC(eb.i-pass:SCREEN-VALUE).

     {sys/inc/roundup.i li2}

     li3 = 1.
     DO li4 = 1 TO 10:
       IF lv-i-code[li4] NE "" THEN DO:
         IF li4 GT DEC(eb.i-col:SCREEN-VALUE) + DEC(eb.i-coat:SCREEN-VALUE) THEN
           ASSIGN
            lv-i-ps[li4]   = 0
            lv-i-code[li4] = ""
            lv-i-dscr[li4] = ""
            lv-i-%[li4]    = 0.

         NEXT.
       END.

       ELSE
       IF li4 LE DEC(eb.i-col:SCREEN-VALUE) THEN DO:
         FIND item WHERE ROWID(item) EQ lv-rowid1 NO-LOCK NO-ERROR.
         IF AVAIL item THEN
           ASSIGN
            lv-i-ps[li4]   = li3
            lv-i-code[li4] = item.i-no
            lv-i-dscr[li4] = item.i-name
            lv-i-%[li4]    = li1.
       END.

       ELSE
       IF li4 GT DEC(eb.i-col:SCREEN-VALUE)                               AND
          li4 LE DEC(eb.i-col:SCREEN-VALUE) + DEC(eb.i-coat:SCREEN-VALUE) THEN DO:
         FIND alt-item WHERE ROWID(alt-item) eq lv-rowid2 NO-LOCK NO-ERROR.
         IF AVAIL alt-item THEN
           ASSIGN
            lv-i-ps[li4]   = li3
            lv-i-code[li4] = alt-item.i-no
            lv-i-dscr[li4] = alt-item.i-name
            lv-i-%[li4]    = 100.
       END.

       IF li4 MODULO li2 EQ 0 THEN li3 = li3 + 1.

       IF li3 GT DEC(eb.i-pass:SCREEN-VALUE) THEN li3 = DEC(eb.i-pass:SCREEN-VALUE).
     END.

     ASSIGN
      eb.i-ps[01]:SCREEN-VALUE   = STRING(lv-i-ps[01])
      eb.i-code[01]:SCREEN-VALUE = lv-i-code[01]
      eb.i-dscr[01]:SCREEN-VALUE = lv-i-dscr[01]
      eb.i-%[01]:SCREEN-VALUE    = STRING(lv-i-%[01])
      eb.i-ps[02]:SCREEN-VALUE   = STRING(lv-i-ps[02])
      eb.i-code[02]:SCREEN-VALUE = lv-i-code[02]
      eb.i-dscr[02]:SCREEN-VALUE = lv-i-dscr[02]
      eb.i-%[02]:SCREEN-VALUE    = STRING(lv-i-%[02])
      eb.i-ps[03]:SCREEN-VALUE   = STRING(lv-i-ps[03])
      eb.i-code[03]:SCREEN-VALUE = lv-i-code[03]
      eb.i-dscr[03]:SCREEN-VALUE = lv-i-dscr[03]
      eb.i-%[03]:SCREEN-VALUE    = STRING(lv-i-%[03])
      eb.i-ps[04]:SCREEN-VALUE   = STRING(lv-i-ps[04])
      eb.i-code[04]:SCREEN-VALUE = lv-i-code[04]
      eb.i-dscr[04]:SCREEN-VALUE = lv-i-dscr[04]
      eb.i-%[04]:SCREEN-VALUE    = STRING(lv-i-%[04])
      eb.i-ps[05]:SCREEN-VALUE   = STRING(lv-i-ps[05])
      eb.i-code[05]:SCREEN-VALUE = lv-i-code[05]
      eb.i-dscr[05]:SCREEN-VALUE = lv-i-dscr[05]
      eb.i-%[05]:SCREEN-VALUE    = STRING(lv-i-%[05])
      eb.i-ps[06]:SCREEN-VALUE   = STRING(lv-i-ps[06])
      eb.i-code[06]:SCREEN-VALUE = lv-i-code[06]
      eb.i-dscr[06]:SCREEN-VALUE = lv-i-dscr[06]
      eb.i-%[06]:SCREEN-VALUE    = STRING(lv-i-%[06])
      eb.i-ps[07]:SCREEN-VALUE   = STRING(lv-i-ps[07])
      eb.i-code[07]:SCREEN-VALUE = lv-i-code[07]
      eb.i-dscr[07]:SCREEN-VALUE = lv-i-dscr[07]
      eb.i-%[07]:SCREEN-VALUE    = STRING(lv-i-%[07])
      eb.i-ps[08]:SCREEN-VALUE   = STRING(lv-i-ps[08])
      eb.i-code[08]:SCREEN-VALUE = lv-i-code[08]
      eb.i-dscr[08]:SCREEN-VALUE = lv-i-dscr[08]
      eb.i-%[08]:SCREEN-VALUE    = STRING(lv-i-%[08])
      eb.i-ps[09]:SCREEN-VALUE   = STRING(lv-i-ps[09])
      eb.i-code[09]:SCREEN-VALUE = lv-i-code[09]
      eb.i-dscr[09]:SCREEN-VALUE = lv-i-dscr[09]
      eb.i-%[09]:SCREEN-VALUE    = STRING(lv-i-%[09])
      eb.i-ps[10]:SCREEN-VALUE   = STRING(lv-i-ps[10])
      eb.i-code[10]:SCREEN-VALUE = lv-i-code[10]
      eb.i-dscr[10]:SCREEN-VALUE = lv-i-dscr[10]
      eb.i-%[10]:SCREEN-VALUE    = STRING(lv-i-%[10]).
   END.
