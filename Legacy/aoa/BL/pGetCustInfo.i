/* pGetCustInfo.i */

PROCEDURE pGetCustInfo:
    DEFINE PARAMETER BUFFER machtran FOR machtran.

    DEFINE OUTPUT PARAMETER opcCustPartNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustName   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemFG     AS CHARACTER NO-UNDO.

    FIND FIRST job-mch NO-LOCK
         WHERE job-mch.company  EQ machtran.company
           AND job-mch.m-code   EQ machtran.machine
           AND job-mch.job-no   EQ machtran.job_number
           AND job-mch.job-no2  EQ machtran.job_sub
           AND job-mch.frm      EQ machtran.form_number
           AND job-mch.blank-no EQ machtran.blank_number
           AND job-mch.pass     EQ machtran.pass_sequence
         NO-ERROR.
    IF AVAILABLE job-mch AND job-mch.i-no NE "" THEN DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ machtran.company
               AND itemfg.i-no    EQ job-mch.i-no
             NO-ERROR.
        IF AVAILABLE itemfg THEN
        opcCustPartNo = itemfg.part-no.
    END. /* avail job-mch */
    RELEASE cust.
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company  EQ machtran.company
           AND job-hdr.job-no   EQ machtran.job_number
           AND job-hdr.job-no2  EQ machtran.job_sub
           AND job-hdr.frm      EQ machtran.form_number
           AND job-hdr.blank-no EQ machtran.blank_number
         NO-ERROR.
    IF NOT AVAILABLE job-hdr THEN
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company  EQ machtran.company
           AND job-hdr.job-no   EQ machtran.job_number
           AND job-hdr.job-no2  EQ machtran.job_sub
           AND job-hdr.frm      EQ machtran.form_number
         NO-ERROR.
    IF NOT AVAILABLE job-hdr THEN
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company  EQ machtran.company
           AND job-hdr.job-no   EQ machtran.job_number
           AND job-hdr.job-no2  EQ machtran.job_sub
         NO-ERROR.
    IF AVAILABLE job-hdr THEN DO:
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ job-hdr.company
               AND cust.cust-no EQ job-hdr.cust-no
             NO-ERROR.
        IF opcCustPartNo EQ "" THEN DO:
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ machtran.company
                   AND itemfg.i-no    EQ job-hdr.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg THEN
            opcCustPartNo = itemfg.part-no.
        END. /* if cust part blank */
    END. /* avail job-mch */
    IF AVAILABLE cust   THEN opcCustName = cust.name.
    IF AVAILABLE itemfg THEN opcItemFG   = itemfg.i-no.

END PROCEDURE.
