/* { prodict/dictrace.i {0} {*} } */
/* This routine runs quoter on <infile>, producing <outfile>. */
DEFINE INPUT PARAMETER infile  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delim   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER colum   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER outfile AS CHARACTER NO-UNDO.
IF delim = ?          THEN delim = "".
ELSE IF OPSYS = "VMS" THEN delim = "/DELIMITER=~"" + delim + "~"".
ELSE                       delim = "-d ~"" + delim + "~" ".
IF colum = ?          THEN colum = "".
ELSE IF OPSYS = "VMS" THEN colum = "/COLUMN=~"" + colum + "~"".
ELSE                       colum = "-c ~"" + colum + "~" ".
IF OPSYS = "MSDOS" OR OPSYS BEGINS "WIN" THEN
  DOS SILENT quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).
ELSE IF OPSYS = "OS2" THEN
  OS2 SILENT quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).
ELSE IF OPSYS = "UNIX" THEN
  UNIX SILENT quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).
ELSE IF OPSYS = "VMS" THEN
  VMS SILENT PROGRESS/TOOLS=QUOTER
    VALUE(delim)
    VALUE(colum)
    VALUE("/OUTPUT=" + outfile)
    VALUE(infile).
ELSE IF OPSYS = "BTOS" THEN
  BTOS SILENT VALUE(SEARCH("Quoter.Run")) Quoter
    VALUE(delim)
    VALUE(colum)
    VALUE(infile)
    VALUE(">" + outfile).
ELSE MESSAGE "osquoter.p: Unknown Operating System -" OPSYS.
if (opsys = "msdos" OR OPSYS BEGINS "WIN")
and terminal = "ms-win"
then pause 2 no-message.
RETURN.
