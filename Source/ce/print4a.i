
def {1} workfile xjob
   field form-no like ef.form-no
   field blank-no like eb.blank-no
   field cust-no like eb.cust-no
   field i-no as char
   field qty  as int
   field pct  as dec
   field mat  as dec
   field lab  as dec
   field foh  as dec
   field voh  as dec
   field stock-no as char
   field pur-man like eb.pur-man.

def {1} workfile blk no-undo
   field kli  as char
   field id   as char
   field snum as int
   field bnum as int
   field qreq as int
   field qyld as int
   field yr$  as logical
   field pct  as dec
   field dscr like item.est-dscr
   field fg-wt as dec
   field fg-wt$ as dec
   field lab  as dec format ">>>>9.99"
   field sell as dec format ">>>9.99"
   field fact as dec format ">>>>>9.99"
   field cost as dec format ">>>>>9.99"
   field stock-no as char
   field pur-man like eb.pur-man
   FIELD freight AS DECIMAL .
