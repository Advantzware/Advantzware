/* -------------------------------------------------oe/rep/invchk.i 7/93 rd */
/* PRINT INVOICE ALIGNMENT CHECK - O/E MODULE                                */
/* -------------------------------------------------------------------------- */

/* JLF rmv'd 02/21/96
   disp
       v-invhead when oe-ctrl.prcom
       today @ v-inv-date
       company.name when oe-ctrl.prcom
       company.addr[1] when oe-ctrl.prcom
       "999999" @ inv-head.inv-no
       company.addr[2] when oe-ctrl.prcom
       company.city when oe-ctrl.prcom
       company.state when oe-ctrl.prcom
       company.zip when oe-ctrl.prcom
       "XXXXXXXXXXX" @ v-salesman
       "XXXXXXXXXXXXXXXXXXXXXXXXXX" @ v-fob
       "XXXXXXXXXXXXXXXXXXXXXXX" @ inv-head.terms-d
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ inv-head.cust-name
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ v-shipto-name
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ inv-head.addr[1]
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ v-shipto-addr[1]
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ inv-head.addr[2]
       "XXXXXXXXXXXXXXXXXXXXXXXX" @ v-shipto-addr[2]
       "XXXXXXXX, XX 99999-99999" @ v-addr3
       "XXXXXXXX, XX 99999-99999" @ v-sold-addr3
       today @ v-date-ship
       "XXXXXXXXXXXXXXXXXXXXXXXXX" @ v-shipvia
       "999999" @ v-del-no
       "999999" @ inv-head.bol-no
       "99999.99" @ inv-head.t-inv-weight
       "99999" @ v-tot-pallets
   with frame invhead no-labels no-box width 90.
   JLF rmv'd 02/21/96 */

/* JLF added 02/21/96 */
   page.
/* JLF added 02/21/96 */

do i = 1 to 10:
   display
       "XXXXXXXXXXXXXXX" @ inv-line.po-no
       "XXXXXXXXXXXXXXX" @ inv-line.i-no
       "-9,999,999" @ inv-line.inv-qty
       "-99,999.99" @ inv-line.price
       "XXXX" @ inv-line.pr-uom
       "-999,999.99" @ inv-line.t-price
       "XXXXXXXXXXXXXXXXXXXXXXX" @ inv-line.i-name
       "-9,999,999" @ inv-line.ship-qty
   with frame detail.
   down with frame detail.
   put skip(1).
end. /* 1 to 10 */
do:  /* T O T A L S */

	  display
	  "TAX1"  @ v-tax-code[1]
	  "-99,999.99" @ net1
	  "TAX2" @ v-tax-code[2]
	  "-99,999.99" @ net2
	  "TAX3" @ v-tax-code[3]
	  "-99,999.99" @ net3
	  "-99,999.99" @ inv-head.t-inv-tax
	  with frame tax.

	  display
	      "-9,999.99"    @ inv-head.t-inv-freight
	      "-9,999.99"    @ inv-head.t-inv-tax
	      "-9,999.99"    @ v-net
	      "-9,999.99" @ tmp1
	      "MM/DD/YY" @ tmp2
	      "-9,999,999.99" @ inv-head.t-inv-rev
	  with frame totals.

end.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
