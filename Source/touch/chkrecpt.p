/*addon/touch/chkrecpt.p */
DEF INPUT PARAM ip-company_code AS cha NO-UNDO.
DEF INPUT PARAM ip-machine_code AS cha NO-UNDO.
DEF INPUT PARAM ip-job_number AS cha NO-UNDO.
DEF INPUT PARAM ip-job_sub AS cha NO-UNDO.
DEF INPUT PARAM ip-charge_code AS cha NO-UNDO.
DEF input PARAM ip-run-qty AS INT NO-UNDO.

DEF VAR v-rcv-qty AS INT NO-UNDO.
DEF VAR v-set-qty AS INT NO-UNDO.
DEF VAR v-rcv-qty-tot AS INT NO-UNDO.

DEF SHARED TEMP-TABLE tt-comp FIELD i-no AS cha 
                              FIELD rcv-qty AS INT
                              FIELD est-no AS cha 
                              FIELD form-no AS INT 
                              FIELD set-qty AS INT .

FOR EACH tt-comp:
    DELETE tt-comp.
END.

FOR EACH job-hdr NO-LOCK WHERE job-hdr.company = ip-company_code
                           AND job-hdr.job-no = ip-job_number
                           AND job-hdr.job-no2 = INT(ip-job_sub),
    EACH itemfg NO-LOCK WHERE itemfg.company = job-hdr.company
                          AND itemfg.i-no = job-hdr.i-no:
    
    IF itemfg.isaset THEN 
    FOR EACH ASI.fg-set NO-LOCK WHERE fg-set.company = itemfg.company
                                  AND fg-set.set-no = itemfg.i-no :
       CREATE tt-comp.
       ASSIGN tt-comp.i-no = itemfg.i-no
              tt-comp.est-no = job-hdr.est-no
              tt-comp.form-no = job-hdr.frm.
    END.
END.

v-rcv-qty = 0.
FOR EACH tt-comp:    
    FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = ip-company_code                            
                                AND fg-rcpth.job-no = ip-job_number
                                AND fg-rcpth.job-no2 = int(ip-job_sub)
                                AND fg-rcpth.i-no = tt-comp.i-no
                                AND fg-rcpth.rita-code = "R",
        EACH ASI.fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code :
        v-rcv-qty = v-rcv-qty + fg-rdtlh.qty.
        v-rcv-qty-tot = v-rcv-qty-tot + fg-rdtlh.qty.
    END.
    FOR EACH fg-rctd NO-LOCK WHERE fg-rctd.company eq ip-company_code 
                               AND fg-rctd.rita-code eq "R" :
        v-rcv-qty = v-rcv-qty + fg-rctd.t-qty.
        v-rcv-qty-tot = v-rcv-qty-tot + fg-rctd.t-qty.
    END.
    tt-comp.rcv-qty = v-rcv-qty.
    FIND FIRST eb NO-LOCK WHERE eb.company = ip-company_code
                            AND eb.est-no = tt-comp.est-no
                            AND eb.form-no = tt-comp.form-no 
                            AND eb.stock-no = tt-comp.i-no NO-ERROR.
    tt-comp.set-qty = v-rcv-qty / IF AVAIL eb THEN eb.quantityPerSet ELSE 1.  
    v-set-qty = tt-comp.set-qty.
END.


IF ip-charge_code = "MR" AND v-rcv-qty-tot <= 0 THEN DO:
   MESSAGE "No component hasn't received!" VIEW-AS ALERT-BOX WARNING.
END.
ELSE IF ip-charge_code = "RUN" AND v-set-qty < ip-run-qty THEN DO:
   MESSAGE "All components haven't received." SKIP
           "Run cannot be completed on assembly machine due to insufficient component parts on machine"
            ip-machine_code
           "for Job# " ip-job_number + "-" + ip-job_sub
       VIEW-AS ALERT-BOX WARNING.

END.
