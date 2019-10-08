
def {1} shared TEMP-TABLE work-item no-undo
   field i-no like itemfg.i-no
   field cust-no like job-hdr.cust-no
   field form-no like job-hdr.frm
   field ord-no like job-hdr.ord-no
   field price like itemfg.sell-price
   field uom like itemfg.sell-uom
   field qty-job as int
   field qty-ord as int
   field press-1 as int
   field qty-prod as int
   field qty-all as int
   field act-spo as dec
   field est-spo as dec
   FIELD avg-qty AS INT
   FIELD sales-rep AS CHARACTER
   FIELD sq-ft AS DECIMAL
   INDEX cust cust-no i-no
   INDEX form-no form-no qty-prod
   INDEX i-no i-no form-no.

def {1} shared TEMP-TABLE work-mat no-undo
   field i-no like item.i-no
   field form-no like job-mat.frm
   field est-qty as dec
   field est-qty-uom like mat-act.qty-uom
   field est-cost as dec
   field act-qty as dec
   field act-qty-uom like mat-act.qty-uom
   field act-cost as dec
   field prd-qty as dec
   field sc-uom like job-mat.sc-uom
   field basis-w like job-mat.basis-w
   field len like job-mat.len
   field wid like job-mat.wid
   field n-up like job-mat.n-up
   FIELD board AS LOG
   FIELD mat-type AS CHAR
   INDEX i-no i-no form-no
   INDEX form-no form-no
   INDEX mat-type mat-type i-no.

def {1} shared buffer xwork-mat for work-mat.

def {1} shared TEMP-TABLE work-mch no-undo
   field m-code like job-mch.m-code
   FIELD frm LIKE job-mch.frm
   FIELD blk LIKE job-mch.blank-no
   FIELD pass LIKE job-mch.pass
   field d-seq like mach.d-seq
   field run-qty as dec
   field run-feet as dec
   field est-qty as dec
   field est-feet as dec
   field wst-qty as dec
   field mr-qty as dec
   field mr-waste as dec
   field est-mr-hr as dec
   field est-run-hr as dec
   field est-speed as dec
   field est-mr-cost as dec
   field est-run-cost as dec
   field est-mr-cost1 as dec
   field est-mr-cost2 as dec
   field est-mr-cost3 as dec
   field est-run-cost1 as dec
   field est-run-cost2 as dec
   field est-run-cost3 as dec
   field mr-hr as dec
   field run-hr as dec
   field mr-cost1 as dec
   field mr-cost2 as dec
   field mr-cost3 as dec
   field run-cost1 as dec
   field run-cost2 as dec
   field run-cost3 as dec
   field run-waste as dec
   field run-speed as dec
   INDEX mch m-code frm blk pass
   INDEX frm frm.
