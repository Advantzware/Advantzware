/* --------------------------------------------------- gl/gl-rpth.f  3/94 RM  */
/* g/l report maintenance - form statement                                    */
/* -------------------------------------------------------------------------- */

form
     "Report Code:"      to 20 gl-rpt.rpt
     "Report Name:"      to 20 v-rpt-name
     "Header 1:"         to 20 v-hdr[1]
     "       2:"         to 20 v-hdr[2]
     "       3:"         to 20 v-hdr[3]
     "       4:"         to 20 v-hdr[4]
     "       5:"         to 20 v-hdr[5]
     "1 Col Balance sheet:" at  2 v-c-bs
     " Page Width:"         at 32 v-p-w
     "Columns Used:"        at 60 v-col-used
     "  Number of Columns:"   at  2 v-no-col at 23
     "Page Length:"         at 32 v-page-length
     "Description Width:"   at 55 v-d-wid at 74
     skip(1)
     "COLUMN HEADERS:"      at  2
     "1" space(0)                v-ch[1]
     "2" space(0)                v-ch[2]
     "3" space(0)                v-ch[3]
     "4" space(0)                v-ch[4] skip
     space(2)
     "5" space(0)                v-ch[5]
     "6" space(0)                v-ch[6]
     "7" space(0)                v-ch[7]
     "8" space(0)                v-ch[8]
     "9" space(0)                v-ch[9] skip
     "COLUMN TYPE:   "      at  2
     "1" space(0)                v-ct[1]
     "2" space(0)                v-ct[2]
     "3" space(0)                v-ct[3]
     "4" space(0)                v-ct[4] skip
     space(2)
     "5" space(0)                v-ct[5]
     "6" space(0)                v-ct[6]
     "7" space(0)                v-ct[7]
     "8" space(0)                v-ct[8]
     "9" space(0)                v-ct[9] skip(1)
     "VAR. Col.:"                 at  2
     v-onevar format "9" "= Col" v-vcol[1]
     "+ Col"          v-vcol[2]
     "- Col"          v-vcol[3]
     space(4)
     v-twovar format "9" "= Col"       v-vcol[4]
     "+ Col"          v-vcol[5]
     "- Col"          v-vcol[6] skip
     "Prt % for Col: "            at  2
     "1" space(1)                v-per[1]
     space(2)
     "2" space(1)                v-per[2]
     space(2)
     "3" space(1)                v-per[3]
     space(2)
     "4" space(1)                v-per[4]
     space(2)
     "5" space(1)                v-per[5]
     space(2)
     "6" space(1)                v-per[6]
     space(2)
     "7" space(1)                v-per[7]
     space(2)
     "8" space(1)                v-per[8]
     space(2)
     "9" space(1)                v-per[9] skip

     with frame gl-rpth row 2 overlay width 80 no-labels
	color value(col-bg) prompt value(col-input)
	title color value(col-warn) "                 G / L    R E P O R T    M A I N T E N A N C E                  ".

/* end ---------------------------------- copr. 1992  advanced software, inc. */
