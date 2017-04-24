
def {1} var fcust as   ch init " ".
def {1} var tcust like fcust init "zzzzzzzz".
def {1} var fitem like itemfg.i-no init " ".
def {1} var titem like fitem init "zzzzzzzzzzzzzzzzzzz".
def {1} var fpcat like itemfg.procat init " ".
def {1} var tpcat like fpcat init "zzzzzzzzzzzzzzzzzzz".
def {1} var fdate as   date format "99/99/9999".
def {1} var tdate like fdate.
def {1} var sort-by-cust as log format "Customer/ProductCategory" init yes.
def {1} var v-inc-fc as log init no.

def var v-date      AS CHAR format "x(10)" NO-UNDO.
def var v-pric      like ar-invl.unit-pr.
def var v-uom       like ar-invl.pr-uom.
def var v-brdc      as   dec.
def var v-ordc      as   dec.
def var v-invc      as   dec.
def var v-marg      as   dec.
def var v-brdp      as   dec.
def var v-$msf      as   dec.

def {1} var v-qty   as   int extent 4.
def {1} var v-msf   as   dec extent 4.
def {1} var v-cst   as   dec extent 4.
def {1} var v-cst1   as  dec extent 4.
def {1} var v-cst2   as  dec extent 4.
def {1} var v-amt   as   dec extent 4.

def var v-cust-no   like cust.cust-no.
def var v-order-date  AS CHAR format "x(10)" NO-UNDO.
def var v-job-no    like job.job-no.
def var v-job-no2   like job.job-no2.
def var v-po-no-po  like ar-invl.po-no-po.
def var v-fac       as   int.
def var v-cost      as   dec.
def var v-job-qty   as   int.
def var v-exc       as   log.

def TEMP-TABLE w-data NO-UNDO
  field sorter    as   char
  field i-no      like ar-invl.i-no column-label "FG Item"
  field inv-no    like ar-invl.inv-no column-label "Invoice!Number"
  field rec-id    as   recid.

form v-cust-no            column-label "Customer"
     w-data.inv-no
     space(2)
     w-data.i-no
     itemfg.procat        column-label "Prod!Categ"
     v-qty[1]             format "->>>,>>>,>>>"
     itemfg.t-sqft        column-label "Item!SqFt"
     v-msf[1]             format "->>9.99"
     v-$msf               format "->>>9"
     v-pric               format "->>>,>>>,>>9.99<<"
     v-uom
     v-brdc               format "->>>,>>9.99"
     v-marg               format "->>>,>>9.99"
     v-brdp               format "->,>>9.99"

    with no-box no-labels frame itemx down stream-io width 132.
