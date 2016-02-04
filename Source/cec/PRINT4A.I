def {1} workfile xjob
   field form-no like ef.form-no
   field blank-no like eb.blank-no
   field cust-no like eb.cust-no
   field i-no as ch
   field qty  as int
   field pct  as de
   field mat  as de
   field lab  as de
   field foh  as de
   field voh  as de
   field stock-no as ch.

def {1} workfile blk
   field kli  as ch
   field id   as ch
   field snum as int
   field bnum as int
   field qreq as int
   field qyld as int
   field yr$  as logical
   field pct  as de
   field dscr like item.est-dscr
   field fg-wt  as de
   field fg-wt$ as de
   field lab  as de format ">>>>9.99"
   field sell as de format ">>>9.99"
   field fact as de format ">>>>>9.99"
   field cost as de format ">>>>>9.99".
