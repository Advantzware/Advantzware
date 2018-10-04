DEF INPUT PARAM ip-program-id AS cha  NO-UNDO.
DEF INPUT PARAM ip-frame-hdl AS HANDLE NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE where-phrase                      ~
    WHERE user-print.company    EQ cocode        ~
      AND user-print.program-id EQ ip-program-id ~
      AND user-print.batch      EQ ""

DEF TEMP-TABLE tt-user-print LIKE user-print.
DEF TEMP-TABLE tt-date FIELD tt-name AS cha
                       FIELD tt-value AS cha.
DEF BUFFER bf-user-print FOR user-print.
DEF VAR lv-update-all-batch AS LOG NO-UNDO.

FOR EACH tt-date:
    DELETE tt-date.
END.

IF g_batch THEN
FIND FIRST user-print NO-LOCK WHERE ROWID(user-print) EQ g_batch-rowid NO-ERROR.
ELSE 
FIND FIRST user-print NO-LOCK
     {&where-phrase} AND user-print.user-id EQ USERID("nosweat") NO-ERROR.

IF NOT AVAIL user-print THEN
FIND FIRST user-print NO-LOCK {&where-phrase} AND user-print.user-id EQ "" NO-ERROR.

CREATE tt-user-print.

IF AVAIL user-print THEN BUFFER-COPY user-print TO tt-user-print.

ASSIGN
 tt-user-print.company     = cocode
 tt-user-print.program-id  = ip-program-id
 tt-user-print.user-id     = USERID("nosweat")
 tt-user-print.batch       = ""
 tt-user-print.field-label = ""
 tt-user-print.field-name  = ""
 tt-user-print.field-value = ""

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
   tt-user-print.field-value[li] = IF lv-field-hdl:NAME BEGINS "sl" THEN lv-field-hdl:LIST-ITEMS
                                   ELSE lv-field-hdl:SCREEN-VALUE
   NO-ERROR.

  IF g_batch THEN DO:
     IF lv-field-hdl:TYPE <> "Browse" AND lv-field-hdl:DATA-TYPE = "Date" THEN DO:
        CREATE tt-date.
        ASSIGN tt-date.tt-name = lv-field-hdl:NAME
               tt-date.tt-value = lv-field-hdl:SCREEN-VALUE.
        IF NOT lv-update-all-batch AND AVAIL user-print THEN
           lv-update-all-batch = tt-date.tt-value <> user-print.field-value[li].
     END.
  END.

  lv-field-hdl = lv-field-hdl:NEXT-SIBLING.
END.

IF g_batch THEN do:
  FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL user-print THEN DO:
    user-print.batch = "Batch".
    IF lv-update-all-batch THEN 
       MESSAGE "Transfer New Date To All Batches With Same Program Name?" 
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:
      FIND FIRST reftable WHERE reftable.reftable = "Batchrpt"
                          AND reftable.code2 = ip-program-id NO-LOCK NO-ERROR.
      IF AVAIL reftable THEN DO:
         FOR EACH bf-user-print WHERE bf-user-print.company EQ cocode         
                                  AND bf-user-print.program-id EQ reftable.code 
                                  AND bf-user-print.BATCH <> ""
                                  AND ROWID(bf-user-print) <> ROWID(user-print):
            FOR EACH tt-date:
              DO li = 1 TO EXTENT(bf-user-print.field-name):
                 IF TRIM(bf-user-print.field-name[li]) NE ""  AND 
                    bf-user-print.field-name[li] EQ tt-date.tt-name 
                 THEN  bf-user-print.field-value[li] = tt-date.tt-value.
              END.
            END. /* each tt-date*/
         END.  /* for each bf-user */
      END. /*reftable*/
    END. /* ll-ans*/
  END.
END.

ELSE DO:
  FIND CURRENT user-print EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAIL user-print THEN CREATE user-print.

  BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print.

  IF user-print.batch EQ "Batch" THEN
  RUN userBatch.

  RELEASE user-print.

  FOR EACH user-print NO-LOCK
      {&where-phrase}
        AND user-print.user-id EQ "":
    FIND bf-user-print WHERE ROWID(bf-user-print) EQ ROWID(user-print)
        EXCLUSIVE NO-ERROR NO-WAIT.
    IF AVAIL bf-user-print THEN DELETE bf-user-print.
  END.

  CREATE user-print.

  BUFFER-COPY tt-user-print EXCEPT rec_key TO user-print
  ASSIGN user-print.user-id = "".

  IF user-print.batch EQ "Batch" THEN
  RUN userBatch.

END.

PROCEDURE userBatch:
  IF NOT CAN-FIND(FIRST user-batch
                  WHERE user-batch.company EQ user-print.company
                    AND user-batch.batch-seq EQ user-print.batch-seq
                    AND user-batch.prog-seq EQ user-print.prog-seq) THEN DO:
    CREATE user-batch.
    ASSIGN
      user-batch.company = user-print.company
      user-batch.batch-seq = user-print.batch-seq
      user-batch.prog-seq = user-print.prog-seq.
  END. /* not can-find user-batch */
END PROCEDURE.
