&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job-mch

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEFINE BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEFINE BUFFER b-job          FOR job.

DEFINE VARIABLE ld                AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ld-tot            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cocode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-format-f       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-error          AS LOGICAL   NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF job-mat.

cocode = {&TABLENAME}.company.
IF cocode NE "" THEN DO:
  {sys/inc/jobcard.i "F"}
  lv-format-f = sys-ctrl.char-fld.
END.

/* rtark - 10.28.2016 */
IF {&TABLENAME}.spare-char-1 EQ "" THEN
{&TABLENAME}.spare-char-1 = {&TABLENAME}.m-code.
IF {&TABLENAME}.start-date GT TODAY + 180 THEN
{&TABLENAME}.start-date = TODAY + 180.
IF {&TABLENAME}.end-date GT TODAY + 180 THEN
{&TABLENAME}.end-date = TODAY + 180.
IF {&TABLENAME}.start-date-su GT TODAY + 180 THEN
{&TABLENAME}.start-date-su = TODAY + 180.
IF {&TABLENAME}.end-date-su GT TODAY + 180 THEN
{&TABLENAME}.end-date-su = TODAY + 180.
IF {&TABLENAME}.start-date-su EQ ? AND
   {&TABLENAME}.start-date    NE ? THEN
{&TABLENAME}.start-date-su = {&TABLENAME}.start-date.
IF {&TABLENAME}.start-date-su EQ {&TABLENAME}.start-date AND
   ({&TABLENAME}.start-time LT {&TABLENAME}.start-time-su OR
    {&TABLENAME}.start-time-su EQ 0) THEN
{&TABLENAME}.start-time-su = {&TABLENAME}.start-time.
IF {&TABLENAME}.start-date-su NE old-{&TABLENAME}.start-date-su OR
   {&TABLENAME}.start-date    NE old-{&TABLENAME}.start-date    THEN
FOR EACH job
    WHERE job.company EQ {&TABLENAME}.company
      AND job.job     EQ {&TABLENAME}.job
      AND job.job-no  EQ {&TABLENAME}.job-no
      AND job.job-no2 EQ {&TABLENAME}.job-no2
    NO-LOCK
    :
  RUN jc/startdat.p (ROWID(job)).
  LEAVE.
END.
IF lv-format-f NE "Frankstn" AND
   old-{&TABLENAME}.company NE "" AND
   {&TABLENAME}.run-qty NE old-{&TABLENAME}.run-qty AND
   PROGRAM-NAME(2) NE "jc/shtcalc.p" AND
   NOT CAN-FIND(FIRST b-{&TABLENAME}
                WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                  AND b-{&TABLENAME}.job     EQ {&TABLENAME}.job
                  AND b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no
                  AND b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2
                  AND b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm
                  AND b-{&TABLENAME}.line    LT {&TABLENAME}.line
                  AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})) THEN DO:
  ld-tot = 0.
  FOR EACH job-mat FIELDS(company rm-i-no qty n-up)
      WHERE job-mat.company EQ {&TABLENAME}.company
        AND job-mat.job     EQ {&TABLENAME}.job
        AND job-mat.job-no  EQ {&TABLENAME}.job-no
        AND job-mat.job-no2 EQ {&TABLENAME}.job-no2
        AND job-mat.frm     EQ {&TABLENAME}.frm
        NO-LOCK,
      FIRST item NO-LOCK
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
        AND INDEX("1234BPR",item.mat-type) GT 0
        AND item.i-code  EQ "R"
      :
    ld-tot = ld-tot + (job-mat.qty * job-mat.n-up).
  END.

  RELEASE job-mat.

  FOR EACH job-mat
      WHERE job-mat.company EQ {&TABLENAME}.company
        AND job-mat.job     EQ {&TABLENAME}.job
        AND job-mat.job-no  EQ {&TABLENAME}.job-no
        AND job-mat.job-no2 EQ {&TABLENAME}.job-no2
        AND job-mat.frm     EQ {&TABLENAME}.frm,
      FIRST item NO-LOCK
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.rm-i-no
        AND INDEX("1234BPR",item.mat-type) GT 0
      :
    ld = {&TABLENAME}.run-qty * (IF item.i-code EQ "R" THEN
                                   ((job-mat.qty * job-mat.n-up) / ld-tot)
                                 ELSE 1).
    IF job-mat.qty-uom EQ "EA" THEN
      {sys/inc/roundup.i ld}
    ELSE
      RUN sys/ref/convquom.p("EA", job-mat.qty-uom,
                             job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                             {&TABLENAME}.run-qty, OUTPUT ld).
    
    ASSIGN 
     job-mat.std-cost = job-mat.std-cost * (job-mat.qty / ld).
     job-mat.qty      = ld.
  END.
END.

/* check if run-complete changed from no to yes */
IF old-{&TABLENAME}.run-complete EQ NO  AND
       {&TABLENAME}.run-complete EQ YES THEN
RUN pLastRoutingEmail.

FIND FIRST b-job EXCLUSIVE-LOCK
     WHERE b-job.company EQ {&TABLENAME}.company
       AND b-job.job     EQ {&TABLENAME}.job
     NO-ERROR.
IF AVAILABLE b-job THEN
b-job.user-id = USERID("NOSWEAT").

/* Clear out any error-status from find with no-error that is false */
ll-error = YES NO-ERROR.

PROCEDURE pLastRoutingEmail:
    DEFINE VARIABLE cBody             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGItem           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobCompleteEmail AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMachine          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iConfigID         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lJobCompleteEmail AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bJobMch FOR job-mch.

    /* get last routing record */
    FIND LAST bJobMch NO-LOCK USE-INDEX line-idx
        WHERE bJobMch.company EQ {&TABLENAME}.company
          AND bJobMch.job     EQ {&TABLENAME}.job
        NO-ERROR.
    /* only applies if last routing record */
    IF AVAILABLE bJobMch AND
       ROWID(bJobMch) EQ ROWID({&TABLENAME}) THEN DO:
        RUN sys/ref/nk1look.p (
            cocode,"JobCompleteEmail","L",NO,NO,"","",
            OUTPUT cJobCompleteEmail,OUTPUT lJobCompleteEmail
            ).
        IF lJobCompleteEmail AND cJobCompleteEmail EQ "Yes" THEN DO:
            RUN sys/ref/nk1look.p (
                cocode,"JobCompleteEmail","I",NO,NO,"","",
                OUTPUT cJobCompleteEmail,OUTPUT lJobCompleteEmail
                ).
            iConfigID = INTEGER(cJobCompleteEmail).
            FIND FIRST mach NO-LOCK
                 WHERE mach.company EQ {&TABLENAME}.company
                   AND mach.m-code  EQ {&TABLENAME}.m-code
                 NO-ERROR.
            IF AVAILABLE mach THEN
            cMachine = mach.m-dscr.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ {&TABLENAME}.company
                   AND itemfg.i-no    EQ {&TABLENAME}.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg THEN
            cFGItem = itemfg.i-name.
            cBody = "Job: " + {&TABLENAME}.job-no + "-"
                  + STRING({&TABLENAME}.job-no2)
                  + " - Routing: " + {&TABLENAME}.m-code + " ("
                  + cMachine + ")"
                  + " - FG Item: " + {&TABLENAME}.i-no + " ("
                  + cFGItem + ")"
                  .
            RUN spSendEmail (
                INPUT iConfigID, /* emailConfig.ConfigID */
                INPUT "",        /* Override for Email RecipientsinTo */
                INPUT "",        /* Override for Email RecipientsinReplyTo */
                INPUT "",        /* Override for Email RecipientsinCC */
                INPUT "",        /* Override for Email RecipientsinBCC */
                INPUT "",        /* Override for Email Subject */
                INPUT cBody,     /* Override for Email Body */
                INPUT ""         /* Email Attachment */
                ).
        END. /* if ljobcompleteemail */
    END. /* if avail */

END PROCEDURE.
