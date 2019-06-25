
DEF {1} SHARED WORKFILE w-ink
  FIELD form-no  LIKE eb.form-no
  FIELD blank-no LIKE eb.blank-no
  FIELD i-no     LIKE item.i-no
  FIELD pass     AS   INT
  FIELD inks     AS   INT               FORMAT ">>9"
  FIELD varn     AS   INT               FORMAT ">>9"
  FIELD coat     LIKE mach.coater
  FIELD press    LIKE item.press-type.

DEF BUFFER b-ink FOR w-ink.
