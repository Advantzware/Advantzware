
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : OrderOnHand

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrderOnHand.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmOrderNum as character no-undo.
DEFINE INPUT PARAMETER prmItemNum as character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderFg.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.

FOR EACH oe-ordl NO-Lock:
FIND FIRST itemfg where itemfg.i-no = oe-ordl.i-no and itemfg.cust-no = oe-ordl.cust NO-LOCK NO-ERROR.
      
     assign 
          prmItemNum = string(itemfg.i-no)
          prmOrderNum = string(oe-ordl.ord-no)
          
         .
     
   

 

FIND FIRST fg-bin
      where fg-bin.company eq itemfg.company
        and fg-bin.i-no    eq itemfg.i-no
     
      no-lock no-error.
IF available fg-bin then do:

      create ttOrderFg.
      assign   ttOrderFg.job-no = fg-bin.job-no
               ttOrderFg.job-no2 = String(fg-bin.job-no2)
               ttOrderFg.i-no  = itemfg.i-no
               ttOrderFg.loc  = fg-bin.loc
               ttOrderFg.loc-bin = fg-bin.loc-bin
               ttOrderFg.tag = fg-bin.tag
               ttOrderFg.cust-no = fg-bin.cust-no
               
               ttOrderFg.cases = string(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
               ttOrderFg.case-count = string(fg-bin.case-count)
               ttOrderFg.cases-unit = string(fg-bin.cases-unit)
               ttOrderFg.partial-count = string(fg-bin.partial-count)
               ttOrderFg.qty = fg-bin.qty
               ttOrderFg.std-tot-cost = fg-bin.std-tot-cost
               ttOrderFg.std-mat-cost = fg-bin.std-mat-cost
               ttOrderFg.std-lab-cost = fg-bin.std-lab-cost
               ttOrderFg.std-var-cost = fg-bin.std-var-cost
               ttOrderFg.std-fix-cost = fg-bin.std-fix-cost
               ttOrderFg.last-cost = fg-bin.last-cost
               ttOrderFg.sell-uom = fg-bin.pur-uom.
     END. /*IF available fg-bin*/
       find first job-hdr where job-hdr.company eq fg-bin.company
                              and job-hdr.i-no    eq fg-bin.i-no
                              and job-hdr.job-no  eq fg-bin.job-no
                              and job-hdr.job-no2 eq fg-bin.job-no2
                            use-index i-no no-lock no-error.
       if avail job-hdr then
        assign 
        ttOrderFg.j-no = string(job-hdr.j-no).

    
 End. /*FOR EACH oe-ordl*/


