/* custom/usrprtb.p  Set Batch procedure */

DEF INPUT PARAM ip-program-id AS CHAR NO-UNDO.
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.
DEF INPUT PARAM ip-prt-name AS CHAR NO-UNDO.
DEF INPUT PARAM ip-prt-port AS CHAR NO-UNDO.
DEF INPUT PARAM ip-copies AS INT NO-UNDO.
DEF INPUT PARAM ip-StartDate AS DATE NO-UNDO.
DEF INPUT PARAM ip-StartTime AS INT NO-UNDO.
DEF INPUT PARAM ip-EndDate AS DATE NO-UNDO.
DEF INPUT PARAM ip-EndTime AS INT NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-1 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-2 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-3 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-4 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-5 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-6 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-DayOfWeek-7 AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-RepeatWeekly AS LOGICAL NO-UNDO.
DEF INPUT PARAM ip-prog-title AS CHAR NO-UNDO.

DEF BUFFER bf-prt FOR user-print.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.  

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-batch-seq AS INT NO-UNDO.

&SCOPED-DEFINE where-phrase                        ~
    WHERE user-print.company    EQ cocode          ~
      AND user-print.program-id EQ ip-program-id ~
      AND user-print.batch      <> ""

DEF TEMP-TABLE tt-user-print LIKE user-print.

FOR EACH bf-prt WHERE bf-prt.company = g_company NO-LOCK
             BY bf-prt.BATCH-seq DESC.
    lv-batch-seq = bf-prt.BATCH-seq.
    LEAVE.
END.

IF g_batch-rowid <> ? THEN 
   FIND FIRST user-print WHERE ROWID(user-print) = g_batch-rowid NO-ERROR.
ELSE FIND FIRST user-print
    {&where-phrase}
      AND user-print.user-id EQ USERID("nosweat") 
    NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print
    {&where-phrase}
      AND user-print.user-id EQ ""
    NO-ERROR.

CREATE tt-user-print.

IF AVAIL user-print THEN BUFFER-COPY user-print TO tt-user-print.

ASSIGN
 tt-user-print.company = cocode
 tt-user-print.program-id = ip-program-id
 tt-user-print.user-id = USERID("nosweat")
 tt-user-print.batch = "Batch"
 tt-user-print.field-label = ""
 tt-user-print.field-name  = ""
 tt-user-print.field-value = ""
 tt-user-print.printer-name = IF ip-prt-name BEGINS "\\" THEN ip-prt-name ELSE ip-prt-port
 tt-user-print.batch-seq = IF g_batch-rowid = ? THEN lv-batch-seq + 1 ELSE user-print.batch-seq
 tt-user-print.prog-title = ip-prog-title
 tt-user-print.frequency = string(ip-copies)
 tt-user-print.next-date = ip-StartDate
 tt-user-print.next-time = ip-StartTime
 tt-user-print.last-date = ?
 tt-user-print.last-time = 0
 lv-group-hdl = ip-frame-hdl:FIRST-CHILD
 lv-field-hdl = lv-group-hdl:FIRST-CHILD

 li = 0.

DO WHILE TRUE:
  li = li + 1.

  IF li GT EXTENT(tt-user-print.field-name) OR
     NOT VALID-HANDLE(lv-field-hdl)         THEN LEAVE.

  ASSIGN
   tt-user-print.field-label[li] = lv-field-hdl:LABEL
   tt-user-print.field-name[li]  = lv-field-hdl:NAME
   tt-user-print.field-value[li] = lv-field-hdl:SCREEN-VALUE NO-ERROR.
 
  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.

IF g_batch-rowid = ? THEN CREATE user-print.

BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print.

IF NOT CAN-FIND(FIRST user-batch
                WHERE user-batch.company EQ user-print.company
                  AND user-batch.batch-seq EQ user-print.batch-seq
                  AND user-batch.prog-seq EQ user-print.prog-seq) THEN DO:
  CREATE user-batch.
  ASSIGN
    user-batch.company = user-print.company
    user-batch.batch-seq = user-print.batch-seq
    user-batch.prog-seq = user-print.prog-seq
    user-batch.startDate = ip-StartDate
    user-batch.startTime = ip-StartTime
    user-batch.endDate = ip-EndDate
    user-batch.endTime = ip-EndTime
    user-batch.dayOfWeek[1] = ip-DayOfWeek-1
    user-batch.dayOfWeek[2] = ip-DayOfWeek-2
    user-batch.dayOfWeek[3] = ip-DayOfWeek-3
    user-batch.dayOfWeek[4] = ip-DayOfWeek-4
    user-batch.dayOfWeek[5] = ip-DayOfWeek-5
    user-batch.dayOfWeek[6] = ip-DayOfWeek-6
    user-batch.dayOfWeek[7] = ip-DayOfWeek-7
    user-batch.repeatWeekly = ip-RepeatWeekly.
END. /* not can-find user-batch */
