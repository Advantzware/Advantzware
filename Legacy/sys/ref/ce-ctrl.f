/* ----------------------------------------------- sys/ref/ce-ctrl.f 1/92 cd  */
/* Cost Estimating - Control File Maintenance    FORM                         */
/* -------------------------------------------------------------------------- */

def var v-spec as char format "x" extent 3.

form header
     "           C O S T   E S T I M A T I N G    C O N T R O L   F I L E"
     with frame top2 overlay no-labels no-box no-underline row 2
          stream-io width 81.

form
  " Last Estimate Number:" ce-ctrl.e-num
    "Range:" at 45 ce-ctrl.e-range[1] "  To  " ce-ctrl.e-range[2] skip
  "    Last Quote Number:" ce-ctrl.q-num
    "Range:" at 45 ce-ctrl.q-range[1] "  To  " ce-ctrl.q-range[2] skip
  head[1]format "x(40)"  head[4] format "x(33)" at 46
  skip

  " Layout Press L:" ce-ctrl.ls-length "Trim:" ce-ctrl.ls-triml
        " FG Handling Rate/CWT:" to 67 space(4)
                ce-ctrl.fg-rate space(0) "$" skip
  " Layout Press W:" ce-ctrl.ls-width  "Trim:" ce-ctrl.ls-trimw
        " RM Handling Rate/CWT:" to 67 space(4)
                ce-ctrl.rm-rate space(0) "$" skip
  " Press FeedType:" choice format "Roll  Fed/Sheet Fed"
        " Handling Charge as %:" to 67 space(4)
                ce-ctrl.hand-pct space(0) "%" skip
  "    Ink#:" ce-ctrl.def-ink "Coverage:" to 31 ce-ctrl.def-inkcov space(0) "%"
        "Warehousing Mark Up :" to 67 space(4)
                ce-ctrl.whse-mrkup space(0) "%" skip
  "   Case#:" ce-ctrl.def-case "Pallet#:" to 31 ce-ctrl.def-pal
  /* " Lbs/Pallet"  to 30 */
  /* ce-ctrl.avg-palwt format ">>>9.9<" */
        ce-ctrl.spec-l[1] to 66 space(0) ":"
           ce-ctrl.spec-%[1] format ">>,>>9.99<<" space(0) v-spec[1] skip
  " Coating:" ce-ctrl.def-coat "Comm.Rate:" to 31 ce-ctrl.comm-mrkup
        space(0) "%"
        ce-ctrl.spec-l[2] to 66 space(0) ":"
           ce-ctrl.spec-%[2] format ">>,>>9.99<<" space(0) v-spec[2] skip
  " For REAL Items Cost, use :" ce-ctrl.r-cost
        ce-ctrl.spec-l[3] to 66 space(0) ":"
           ce-ctrl.spec-%[3] format ">>,>>9.99<<" space(0) v-spec[3] skip

  head[2] format "x(37)"
        head[3] format "x(41)" at 38 skip
               head[5] format "x(31)"
        "What If Net%   Heading" to 67 ce-ctrl.hd-net skip
  space(1) ce-ctrl.mat-cost[1] ce-ctrl.mat-pct[1]
     space(2) ce-ctrl.lab-cost[1] ce-ctrl.lab-pct[1]
        "What If Gross% Heading" to 67 ce-ctrl.hd-gross skip
  space(1) ce-ctrl.mat-cost[2] ce-ctrl.mat-pct[2]
     space(2) ce-ctrl.lab-cost[2] ce-ctrl.lab-pct[2]
        "Calculate Sell Price on N/G?"  to 67 ce-ctrl.sell-by skip
  space(1) ce-ctrl.mat-cost[3] ce-ctrl.mat-pct[3]
     space(2) ce-ctrl.lab-cost[3] ce-ctrl.lab-pct[3]
      "Profit Mark Up:" to 67 ce-ctrl.prof-mrkup space(0) "%" skip
  space(1) ce-ctrl.mat-cost[4] ce-ctrl.mat-pct[4]
     space(2) ce-ctrl.lab-cost[4] ce-ctrl.lab-pct[4]
      "Show Commissions?" at 39 ce-ctrl.comm-add
           "Show labor Rates?" ce-ctrl.sho-labor skip
  space(1) ce-ctrl.mat-cost[5] ce-ctrl.mat-pct[5]
     space(2) ce-ctrl.lab-cost[5] ce-ctrl.lab-pct[5] space(7)
      "Add to Fact. Costs? Frt:" space(0) ce-ctrl.shp-add
                "S1:" space(0) ce-ctrl.spec-add[1]
                "S2:" space(0) ce-ctrl.spec-add[2]
                "S3:" space(0) ce-ctrl.spec-add[3] skip
  space(1) ce-ctrl.mat-cost[6] ce-ctrl.mat-pct[6]
     space(2) ce-ctrl.lab-cost[6] ce-ctrl.lab-pct[6] space(7)
        space(20) "GSA:"  space(0) ce-ctrl.spec-add[6]
                  "Comm:" space(0) ce-ctrl.spec-add[7]
                  "Royal:" space(0) ce-ctrl.spec-add[8]
     /*
     "Round Dollars on Estimates?" to 67 ce-ctrl.trunc-99 skip
     */
  with frame ce-ctrl overlay no-labels no-box no-underline row 3
       stream-io width 81.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
