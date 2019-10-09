
         v-image SKIP
         /*v-change-ord
         "P U R C H A S E  O R D E R"       to 80
         "**************************"       to 80*/
         "<C50><B><P14>Purchase Order</B><P12><C69>Page#:" 
                TRIM(STRING(li-page - v-last-page,">9") /*+
                " OF " + TRIM(STRING(v-page-tot,">9"))*/) FORMAT "x(10)" 
         "<P10>" skip(5)
         /*v-comp-add1  AT 3 
         v-comp-add2  AT 8  SKIP*/
         /*"<R+1><C3>" v-comp-add3  
         "<R+1><C3>" v-comp-add4  
         "<R+1><C3>" v-comp-add5  
         "<R+1><C3>" lv-email  "<P12>"*/ SKIP
         
         "<P12><R-1>" "DATE:"                            to 55
         po-ord.po-date                           FORMAT "99/99/99"
         skip(2)
         "TO:"                              at 11
         "SHIP TO:"                         at 50
         vend.name                          at 11
         v-sname                            at 50
         vend.add1                          at 11
         v-saddr[1]                         at 50
         vend.add2                          at 11
         v-saddr[2]                         at 50
         string(trim(vend.city)  +
                ", "             +
                trim(vend.state) +
                "  "             +
                trim(vend.zip))             at 11   format "x(30)"
         string(trim(v-scity)  +
                ", "           +
                trim(v-sstate) +
                "  "           +
                trim(v-szip))               at 50   format "x(30)"
         SKIP
         "Phone:" + STRING(vend.area-code) + "-" + STRING(vend.phone) + " Fax:" +
         STRING(vend.fax-area) + "-" + STRING(vend.fax) FORM "x(50)" AT 11
         skip(1)
         "ATTN:"                            at 7
         v-contact
         "PO#:"                             to 55
         po-ord.po-no SKIP
         /*fill("-",80)                               format "x(80)"*/
         "<C1><R+.5><FROM><C82><LINE><||3>" skip
         "Qte"                              to 6
         "DESC/NOTES"                       at 8
         "ADDER"                            at 37
         "JOB #"                            at 49
         "Coût/DUE"                         at 61
         "MSF/TOTALS"                       to 80 SKIP
         /*fill("-",80)                               format "x(80)"*/
         "<C1><R+.5><FROM><C82><LINE><||3>" skip
         .
