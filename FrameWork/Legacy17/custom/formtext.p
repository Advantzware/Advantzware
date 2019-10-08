
DEF INPUT PARAM ip-text AS CHAR NO-UNDO.

{custom/formtext.i}

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR ls AS CHAR NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.


DO li = 1 TO LENGTH(ip-text):
  ASSIGN
   ls = SUBSTR(ip-text,li,1)
   ll = ls EQ CHR(10) OR ls EQ CHR(13).

  IF ls NE " " OR ll OR
     (ls EQ " " AND LENGTH(lv-text) GT 0 AND
      SUBSTR(lv-text,LENGTH(lv-text),1) NE " ") THEN
    lv-text = lv-text + ls.

END.

FIND NEXT tt-formtext NO-ERROR.

DO li = 1 TO LENGTH(lv-text):  /* length(trim(lv-text)) - cutting length YSK */
  ASSIGN
   ls = SUBSTR(lv-text,li,1)
   ll = ls EQ CHR(10) OR ls EQ CHR(13).

  IF ll THEN ls = " ".

  IF AVAIL tt-formtext THEN DO:
    IF LENGTH(tt-text) + 1 GT tt-length OR ll THEN DO:
      IF NOT ll AND ls NE " " THEN
      DO lj = LENGTH(tt-text) TO 1 BY -1:
        IF SUBSTR(tt-text,lj,1) NE " " THEN
          ASSIGN
           ls = SUBSTR(tt-text,lj,1) + ls
           SUBSTR(tt-text,lj,1) = "".
        ELSE LEAVE.
      END.    
       
      IF LENGTH(ls) < 42 THEN
      FIND NEXT tt-formtext NO-ERROR.
    END.
   
    IF AVAIL tt-formtext                   AND
       NOT ll                              AND
       (ls NE " " OR LENGTH(tt-text) GT 0 ) THEN 
      tt-text = tt-text + ls.
  END.
END.
