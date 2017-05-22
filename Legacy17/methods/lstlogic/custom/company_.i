/* company_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'company_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="company" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="company" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="company" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="company" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  company.company
  company.name
  company.fid LABEL 'Federal/StateID'
  company.addr[1] LABEL 'Address'
  company.num-per LABEL '#Periods'
  company.yend-off LABEL 'Yr End Mth'
.
IF company.sid NE '' THEN
DO:
  DOWN.
  DISPLAY company.sid @ company.fid.
END.
IF company.addr[2] NE '' THEN
DO:
  IF company.sid = '' THEN
  DOWN.
  DISPLAY company.addr[2] @ company.addr[1].
END.
IF company.city NE '' OR
   company.state NE '' OR
   company.zip NE '' THEN
DO:
  IF company.sid = '' AND company.addr[2] = '' THEN
  DOWN.
  DISPLAY company.city + ', ' + company.state + ' ' + company.zip @
          company.addr[1].
END.
IF company.co-acc NE '' THEN
DO:
  DOWN.
  DISPLAY 'Acct Co:' + company.co-acc @ company.fid.
END.
IF company.acc-level NE 0 THEN
DO:
  DOWN.
  PUT UNFORMATTED '# Levels: ' AT 40 company.acc-level ' --> '.
END.
IF company.acc-level GE 1 THEN
PUT UNFORMATTED 'Digits 1: ' company.acc-dig[1].
IF company.acc-level GE 2 THEN
PUT UNFORMATTED '  2: ' company.acc-dig[2].
IF company.acc-level GE 3 THEN
PUT UNFORMATTED '  3: ' company.acc-dig[3].
IF company.acc-level GE 4 THEN
PUT UNFORMATTED '  4: ' company.acc-dig[4].
IF company.acc-level GE 5 THEN
PUT UNFORMATTED '  5: ' company.acc-dig[5].
PUT UNFORMATTED SKIP.
    /*
    WITH FRAME acc-level STREAM-IO NO-BOX SIDE-LABELS COLUMN 9.
    */
IF show-open-periods THEN
FOR EACH period OF company NO-LOCK BREAK BY period.yr
    WITH FRAME period STREAM-IO TITLE '----- OPEN PERIODS -----':
  IF FIRST-OF(period.yr) THEN
  DISPLAY period.yr.
  DISPLAY period.pnum period.pst period.pend period.pstat.
END.
IF show-open-periods THEN
PUT UNFORMATTED SKIP(1).

{methods/lstlogic/shownote.i &db_table="company" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="company" &col="5" &frame-name="f-miscflds"}
