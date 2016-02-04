/*po/po-fibx1.i*/
PUT {&STREAM} SKIP(1)  
         v-image SKIP
         /*v-change-ord
         "P U R C H A S E  O R D E R"       to 80
         "**************************"       to 80*/
         "<C50><B><P14>Purchase Order</B><P12><C69>Page#:" 
                STRING(TRIM(STRING(PAGE-NUMBER {&PAGE} - v-last-page,">9")) /*+
                " OF " + TRIM(STRING(v-page-tot,">9"))*/) FORMAT "x(10)" 
         "<P10>" SKIP(5)
         v-comp-add1  AT 3 
         /*v-comp-add2  AT 8  SKIP*/
         "<R+1><C3>" v-comp-add3  
         "<R+1><C3>" v-comp-add4  
         "<R+1><C3>" v-comp-add5  
         "<R+1><C3>" lv-email  "<P12>" SKIP
         
         "<R-1>" "DATE:"                            TO 55
         po-ord.po-date                           FORMAT "99/99/99"
         SKIP(2)
         "TO:"                              AT 11
         "SHIP TO:"                         AT 50
         vend.name                          AT 11
         v-sname                            AT 50
         vend.add1                          AT 11
         v-saddr[1]                         AT 50
         vend.add2                          AT 11
         v-saddr[2]                         AT 50
         STRING(TRIM(vend.city)  +
                ", "             +
                TRIM(vend.state) +
                "  "             +
                TRIM(vend.zip))             AT 11   FORMAT "x(30)"
         STRING(TRIM(v-scity)  +
                ", "           +
                TRIM(v-sstate) +
                "  "           +
                TRIM(v-szip))               AT 50   FORMAT "x(30)"
         SKIP
         "Phone:" + STRING(vend.area-code) + "-" + STRING(vend.phone) + " Fax:" +
         STRING(vend.fax-area) + "-" + STRING(vend.fax) FORM "x(50)" AT 11
         SKIP(1)
         "ATTN:"                            AT 7
         v-contact
         "PO#:"                             TO 55
         po-ord.po-no SKIP
         /*fill("-",80)                               FORMAT "x(80)"*/
         "<C1><R+.5><FROM><C82><LINE><||3>" SKIP
         "QTY"                              TO 6
         "DESC/NOTES"                       AT 8
         "ADDER"                            AT 37
         "JOB #"                            AT 49
         "COST/DUE"                         AT 61
         "TOTALS/MSF"                       TO 80 SKIP
         /*fill("-",80)                               FORMAT "x(80)"*/
         "<C1><R+.5><FROM><C82><LINE><||3>" SKIP
         .

