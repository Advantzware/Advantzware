DEFINE  VARIABLE   prt_pos         AS INTEGER NO-UNDO.
DEFINE  VARIABLE   prt_pot         AS INTEGER NO-UNDO.
DEFINE  VARIABLE   prt_ch          AS CHARACTER NO-UNDO.
DEFINE  VARIABLE   prt_hex         AS INTEGER NO-UNDO.
DEFINE  VARIABLE   prt_var         AS INTEGER NO-UNDO.
DEFINE  VARIABLE   prt_len         AS INTEGER NO-UNDO.

prt_pos = INDEX({1},"^[").
DO WHILE prt_pos <> 0:
  SUBSTR({1},prt_pos,2) = CHR(27).
  prt_pos = INDEX({1},"^[").
END.

prt_pos = INDEX({1},"~\E").
DO WHILE prt_pos <> 0:
  SUBSTR({1},prt_pos,2) = CHR(27).
  prt_pos = INDEX({1},"~\E").
END.

prt_pos = INDEX({1},"^").
DO WHILE prt_pos <> 0:
  prt_ch = SUBSTR({1},prt_pos + 1,1).
  SUBSTR({1},prt_pos,2) = CHR(ASC(CAPS(prt_ch)) MODULO 32).
  prt_pos = INDEX({1},"^").
END.

prt_pos = INDEX({1},"~~").
DO WHILE prt_pos <> 0:
  prt_hex = (INDEX("0123456789ABCDEF",SUBSTR({1},prt_pos + 1,1)) - 1) * 16
    + (INDEX("0123456789ABCDEF",SUBSTR({1},prt_pos + 2,1)) - 1).
  SUBSTR({1},prt_pos,3) = CHR(prt_hex).
  prt_pos = INDEX({1},"~~").
END.

IF "{3}" = "LPI" OR "{3}" = "LPP" THEN
DO:
  prt_pos = INDEX({1},"~{").
  prt_pot = INDEX({1},"~}").
  DO WHILE prt_pos <> 0:
    prt_len = prt_pot - prt_pos + 1.
    prt_var = INTEGER(SUBSTRING({1},prt_pos + 1,
      prt_pot - (prt_pos + 1))).
    IF "{3}" = "LPP" THEN
    DO:
      prt_var = prt_var * (IF {2} = 0 THEN
      66 ELSE
      INTEGER({2})).
    END.
    ELSE
    DO:
      prt_var = prt_var / (IF {2} = 0 THEN
      6 ELSE
      INTEGER({2})).
    END.
    SUBSTR({1},prt_pos,prt_len) = STRING(prt_var).
    prt_pos = INDEX({1},"~{").
    prt_pot = INDEX({1},"~}").
  END.
END.
