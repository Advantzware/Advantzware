/***************************************************************************\
*****************************************************************************
**  Program: C:\RPatch\RC\DEPUNCT.P
**       By: Chris Heins    
** Descript: Consolidates functionality of various depunct include files.
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_from AS char NO-UNDO.
DEF INPUT PARAM p_opt    AS char NO-UNDO.
DEF OUTPUT PARAM p_to    AS char NO-UNDO.
{rc/stringv.i}
DEF var NUM_CHARS       AS char NO-UNDO initial "0123456789".
DEF VAR CHARSET         AS CHAR NO-UNDO.
DEF VAR DEC_CHARS       AS CHAR NO-UNDO INITIAL ".+-".
DEF var alpha_chars     AS char NO-UNDO INITIAL "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
DEF VAR C               AS CHAR NO-UNDO.
DEF var i               AS int  NO-UNDO.
DEF var token           AS char NO-UNDO.
IF p_OPT <= " " OR p_opt = ? THEN
charset = NUM_CHARS.
ELSE
DO i = 1 TO NUM-ENTRIES(p_opt):
  token = ENTRY(i, p_OPT).
  IF token BEGINS "NUM" THEN
  ASSIGN CHARSET = CHARSET + NUM_CHARS.
  ELSE
  IF token BEGINS "DEC" THEN
  ASSIGN CHARSET = CHARSET + NUM_CHARS
    + DEC_CHARS.
  ELSE
  IF token BEGINS "ALPHA" THEN
  ASSIGN CHARSET = CHARSET + ALPHA_CHARS.
  ELSE
  do:   /* warning but assume these are additional characters */
    RUN rc/debugmsg.p ("Unrecognized option: " + token).
    assign charset = charset + token.
  end.
END.
p_to = "".
i = 1.
DO i = 1 TO length(p_from):
  C = substring(p_from,i,1).
  IF INDEX(CHARSET, C) > 0
    THEN
  p_to = p_to + c.
END.
