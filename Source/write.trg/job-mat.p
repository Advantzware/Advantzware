&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME job-mat

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-job FOR job.

DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR v-comm AS DEC NO-UNDO.
DEF VAR v-bwt AS DEC NO-UNDO.
DEF VAR v-len AS DEC NO-UNDO.
DEF VAR v-wid AS DEC NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF job-mch.

cocode = {&TABLENAME}.company.

IF cocode NE "" THEN DO:
    {sys/inc/jobcard.i "F"}
    lv-format-f = sys-ctrl.char-fld.
END.

IF {&TABLENAME}.qty-all EQ 0 
AND NOT {&TABLENAME}.all-flg  THEN
    {&TABLENAME}.qty-all = {&TABLENAME}.qty - {&TABLENAME}.qty-iss.

IF {&TABLENAME}.qty-all LT 0 THEN {&TABLENAME}.qty-all = 0.

IF {&TABLENAME}.qty-all NE old-{&TABLENAME}.qty-all THEN DO:
    FIND FIRST item EXCLUSIVE-LOCK WHERE 
        item.company EQ {&TABLENAME}.company AND 
        item.i-no    EQ {&TABLENAME}.rm-i-no AND 
        item.i-code  EQ "R"
        NO-ERROR NO-WAIT.
    /*  5/5/20 - MYT - Ticket 64405
        The purpose here is to update the item.q-comm value with the change in job-mat.qty-all for THIS change.
        The current use of rm/calcqcom.p recalculates the total item.q-comm value starting from zero, using
        ALL job-mat and mat-act records.  Obviously, this is inefficient, as the total rebuild is performed
        on every transaction that writes to the job-mat file.  In a typical session of receiving job tags, for
        example, there are over 600,000 reads of the job-mat and mat-act tables.  Since every read passes data
        over the network, the traffic and the workstation RAM are overwhelmed.  The symptom here is that running
        SS receipts on the server can have acceptable performance, but running the same process on a workstation
        will take more time, up to a factor of several HUNDRED times.  To resolve this, let's just use the 
        delta of old versus new qty-all for THIS job-mat, and apply the resulting delta qty to the item. 
    */  
    /*  IF AVAIL item THEN RUN rm/calcqcom.p (ROWID(item), OUTPUT item.q-comm). */

    IF AVAIL ITEM AND {&TABLENAME}.all-flg THEN DO:
        ASSIGN 
            v-comm = {&TABLENAME}.qty-all - old-{&TABLENAME}.qty-all.

        IF job-mat.qty-uom NE item.cons-uom THEN DO:
            ASSIGN 
                v-bwt = job-mat.basis-w
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-len = IF v-len EQ 0 THEN ITEM.s-len ELSE v-len
                v-wid = IF v-wid EQ 0 THEN (IF ITEM.r-wid NE 0 THEN ITEM.r-wid ELSE ITEM.s-wid) ELSE v-wid
                v-bwt = IF v-bwt EQ 0 THEN ITEM.basis-w ELSE v-bwt.

            RUN sys/ref/convquom.p (job-mat.qty-uom, 
                                    item.cons-uom,
                                    v-bwt, 
                                    v-len, 
                                    v-wid, 
                                    item.s-dep,
                                    v-comm, 
                                    OUTPUT v-comm).
        END.
        
        ASSIGN 
            ITEM.q-comm = ITEM.q-comm + v-comm.
        
    END.
END.

IF old-{&TABLENAME}.company NE ""
AND {&TABLENAME}.qty NE old-{&TABLENAME}.qty 
AND PROGRAM-NAME(2) NE "jc/shtcalc.p" THEN DO:

    ASSIGN 
        ll = lv-format-f EQ "Frankstn".

    IF NOT ll THEN FOR EACH b-{&TABLENAME} NO-LOCK WHERE 
        b-{&TABLENAME}.company EQ {&TABLENAME}.company AND 
        b-{&TABLENAME}.job     EQ {&TABLENAME}.job AND 
        b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no AND 
        b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2 AND 
        b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm AND 
        CAN-FIND(FIRST item WHERE 
                item.company EQ b-{&TABLENAME}.company AND 
                item.i-no    EQ b-{&TABLENAME}.rm-i-no AND 
                INDEX("1234BPR",item.mat-type) GT 0 AND 
                item.i-code  EQ "R"):
        ll = YES.
        LEAVE.
    END.

    IF ll THEN DO:
        ld = 0.

        IF lv-format-f EQ "Frankstn" OR
        CAN-FIND(FIRST item WHERE 
                item.company EQ {&TABLENAME}.company AND 
                item.i-no    EQ {&TABLENAME}.rm-i-no AND 
                INDEX("1234BPR",item.mat-type) GT 0 AND 
                item.i-code  EQ "R") THEN
            ld = {&TABLENAME}.qty * (IF {&TABLENAME}.n-up EQ 0 THEN 1 ELSE {&TABLENAME}.n-up).

        FOR EACH b-{&TABLENAME} NO-LOCK WHERE 
            b-{&TABLENAME}.company EQ {&TABLENAME}.company AND 
            b-{&TABLENAME}.job     EQ {&TABLENAME}.job AND 
            b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no AND 
            b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2 AND 
            b-{&TABLENAME}.frm     EQ {&TABLENAME}.frm AND 
            ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME}) AND 
            CAN-FIND(FIRST item WHERE 
                item.company EQ b-{&TABLENAME}.company AND 
                item.i-no    EQ b-{&TABLENAME}.rm-i-no AND 
                CAN-DO("1,2,3,4,B,P,R",item.mat-type) AND 
                (item.i-code  EQ "R" OR lv-format-f EQ "Frankstn")):
            ld = ld + (b-{&TABLENAME}.qty * (IF b-{&TABLENAME}.n-up EQ 0 THEN 1 ELSE b-{&TABLENAME}.n-up)).
        END.

        RUN jc/machshts.p (ROWID({&TABLENAME}), ld, 0).
    END.
    ELSE FOR EACH ITEM NO-LOCK WHERE 
        item.company EQ {&TABLENAME}.company AND 
        item.i-no    EQ {&TABLENAME}.rm-i-no AND 
        INDEX("1234BPR",item.mat-type) GT 0,
        EACH job-mch WHERE 
            job-mch.company EQ {&TABLENAME}.company AND 
            job-mch.job     EQ {&TABLENAME}.job AND 
            job-mch.job-no  EQ {&TABLENAME}.job-no AND 
            job-mch.job-no2 EQ {&TABLENAME}.job-no2 AND 
            job-mch.frm     EQ {&TABLENAME}.frm
            BREAK BY job-mch.line:

        IF FIRST(job-mch.line) THEN DO:
            IF job-mat.qty-uom EQ "EA" THEN
                job-mch.run-qty = {&TABLENAME}.qty.
            ELSE
                RUN sys/ref/convquom.p(job-mat.qty-uom, 
                                       "EA",
                                       job-mat.basis-w, 
                                       job-mat.len, 
                                       job-mat.wid, 
                                       item.s-dep,
                                       {&TABLENAME}.qty, 
                                       OUTPUT job-mch.run-qty).

            LEAVE.
        END.
    END.
END.

FIND FIRST b-job WHERE
    b-job.company EQ {&TABLENAME}.company AND
    b-job.job     EQ {&TABLENAME}.job
    EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL b-job THEN
   b-job.user-id = USERID("NOSWEAT").

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
    {custom/fibreaud.i}
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
