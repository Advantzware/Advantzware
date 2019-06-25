
/*------------------------------------------------------------------------
    File        : ItemJob.p
    Purpose     : ItemJob

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemJob.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOnOrder.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE Prodqty        AS Int.
  DEF VAR li AS INT NO-UNDO.
DEFINE VAR cocode AS CHARACTER.
IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust = ? THEN ASSIGN prmCust = "".
IF prmUser = ? THEN ASSIGN prmUser = "".

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FOR EACH oe-ordl WHERE oe-ordl.company EQ prmComp NO-LOCK,
    FIRST itemfg where
          itemfg.company EQ oe-ordl.company AND
          itemfg.i-no = oe-ordl.i-no  no-lock:     
       
    find job-hdr where job-hdr.company eq oe-ordl.company
                   and job-hdr.i-no    eq oe-ordl.i-no
                   and job-hdr.job-no  eq oe-ordl.job-no
                   no-lock no-error.
    If avail job-hdr then
    DO:
       create ttOnOrder.
       assign   
          ttOnOrder.cust-part = oe-ordl.part-no
          ttOnOrder.Ord-Qty = oe-ordl.qty
          ttOnOrder.Ship-Qty = oe-ordl.ship-qty
          ttOnOrder.InvQty = oe-ordl.inv-qty
          ttOnOrder.job-no = job-hdr.job-no
          ttOnOrder.job-no2 = job-hdr.job-no2
          ttOnOrder.i-no  = job-hdr.i-no
          ttOnOrder.cust-no = job-hdr.cust-no
          ttOnOrder.est-no  = job-hdr.est-no
          ttOnOrder.ord-no = job-hdr.ord-no.    
    END.
                            
    FOR EACH job WHERE job.company EQ prmComp no-lock :
          create ttOnOrder.
          assign
            ttOnOrder.Startdate = job.start-date
            ttOnOrder.Enddate  = job.complete-date. 
                            .
    END. /*FOR EACH job no-lock*/
    li = 0.                      
  IF AVAIL oe-ordl THEN
  DO:
     IF oe-ordl.job-no NE "" THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ oe-ordl.job-no
             AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
              IF AVAILABLE fg-rdtlh THEN
              ASSIGN
                 li = li + fg-rdtlh.qty
                 ttOnOrder.Prod = li.
        END.
     ELSE
     DO:
        FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
            job-hdr.company EQ cocode AND
            job-hdr.ord-no EQ oe-ordl.ord-no AND
            job-hdr.i-no EQ oe-ordl.i-no
            USE-INDEX ord-no
            NO-LOCK,
            EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.job-no    EQ job-hdr.job-no
             AND fg-rcpth.job-no2   EQ job-hdr.job-no2
             AND fg-rcpth.i-no      EQ oe-ordl.i-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX job,
           EACH fg-rdtlh FIELDS(qty) NO-LOCK
           WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
             AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
              IF AVAILABLE fg-rdtlh THEN
              ASSIGN
                 li = li + fg-rdtlh.qty
                 ttOnOrder.Prod = li.
        END.
     END.
  END.
     FOR EACH fg-bin WHERE fg-bin.company = oe-ordl.company 
            AND fg-bin.i-no = Oe-ordl.i-no  
            AND fg-bin.job-no = string(oe-ordl.job-no)  NO-LOCK:
            ASSIGN ttOnOrder.onhandqty = ttOnOrder.onhandqty + fg-bin.qty.
                   
     END.  /*FOR EACH fg-bin WHERE fg-bin.company*/ 

END.

