
         v-image SKIP
         /*v-change-ord
         "P U R C H A S E  O R D E R"       to 80
         "**************************"       to 80*/
         "<C50><B><P14>Purchase Order</B><P12><C69>Page#:" 
                TRIM(STRING(li-page - v-last-page,">9") /*+
                " OF " + TRIM(STRING(v-page-tot,">9"))*/) FORMAT "x(10)"                  
        "<R5.5><C70><#5>" SKIP
        "<P12><=#5><P10>" SKIP
        IF lPoBarCode THEN       
        "<AT=,6.4><FROM><AT=+.5,+1.7><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" +  
                        string(po-ord.po-no) + ">" + "<AT=,7.9>" ELSE "<R+2.5>" format "x(100)" SKIP        
         SKIP         
         "<P12><R-1>" "DATE:"                            to 65
         po-ord.po-date                           FORMAT "99/99/99"
         skip(2)
         "TO:"                              at 11
         "SHIP TO:"                      at 50
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
         "<C1><R+.5><FROM><C82><LINE><||3>" SKIP
         "<R-.5>Count/"                           AT 61 
         "Qty"                              to 6
         "DESC/NOTES"                       at 8
         "ADDER"                            at 37
         "JOB #"                            at 49
         "Due Date"                         at 61
         "MSF/TOTALS"                       to 80 SKIP
         /*fill("-",80)                               format "x(80)"*/
         "<C1><R+.5><FROM><C82><LINE><||3>" skip
         .
