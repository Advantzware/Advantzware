/* { prodict/dictrace.i {0} {*} } */
/* osdelete.p - This routine removes the files named in its parameter. */
DEFINE INPUT PARAMETER filenames AS CHARACTER.
DEFINE VARIABLE loop AS INTEGER.
DEFINE VARIABLE filen AS CHARACTER.
filenames = filenames + ",".
DO loop = 1 TO NUM-ENTRIES(filenames):
  filen = ENTRY(loop,filenames).
  IF SEARCH(filen) = ? THEN NEXT.
  IF      OPSYS = "MSDOS" THEN DOS  SILENT del       VALUE(filen).
  ELSE IF OPSYS BEGINS "WIN" THEN DOS SILENT DEL     VALUE(filen). /*9903 CAH*/
  ELSE IF OPSYS = "OS2"   THEN OS2  SILENT del       VALUE(filen).
  ELSE IF OPSYS = "UNIX"  THEN UNIX SILENT rm -f     VALUE(filen).
  ELSE IF OPSYS = "VMS"   THEN VMS  SILENT delete    VALUE(filen + CHR(59)).
  ELSE IF OPSYS = "BTOS"  THEN BTOS SILENT OS-DELETE VALUE(filen).
END.
RETURN.
 
