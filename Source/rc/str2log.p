/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\rc\str2log.p
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_char AS char NO-UNDO.
DEF OUTPUT PARAM p_log AS logical NO-UNDO initial ?.
IF p_char BEGINS "T" OR p_char BEGINS "Y" THEN
p_log = TRUE.
ELSE
IF p_char BEGINS "F" OR p_char BEGINS "N" THEN
p_log = FALSE.
