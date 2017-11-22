/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\rc\filearch.
**       By: Chris Heins
** Descript: copy a file to file.MDD for archiving.
Usage: run rc/filearch.p (filename, archive path, options).
**
*****************************************************************************
\***************************************************************************/
DEF INPUT PARAM p_file AS char NO-UNDO.
DEF INPUT PARAM p_archive AS char NO-UNDO.
DEF INPUT PARAM p_options AS char NO-UNDO.
DEF OUTPUT PARAM p_dest AS char NO-UNDO.
DEF VAR ws_fext AS CHAR NO-UNDO.
DEF VAR archive_fid AS CHAR NO-UNDO.
DEF var ws_char AS char NO-UNDO.
DEF var ws_int AS integer NO-UNDO.
RUN rc/dt2fext.p (TODAY, OUTPUT ws_fext).
archive_fid = p_file.
ws_int = R-INDEX(archive_fid, ".").
IF ws_int > 0          /* strip off extension */
  THEN
archive_fid = SUBSTRING(archive_fid, 1, ws_int - 1).
archive_fid = archive_fid + ws_fext.  /* add MDD extension */
OS-COPY VALUE(p_file) VALUE(archive_fid).  /* save the input file as .MDD */
IF OS-ERROR = 0 THEN
DO:
  OS-DELETE VALUE(p_file).
  p_dest = archive_fid.
END.
ELSE
DO:
  IF NOT CAN-DO (p_options, "SILENT")
    THEN
  MESSAGE "Error during file archiving: " OS-ERROR
    VIEW-AS alert-box.
  p_dest = "ERROR: " + string(OS-ERROR).
END.
