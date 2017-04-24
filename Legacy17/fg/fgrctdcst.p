DEFINE INPUT  PARAMETER ipr-fg-rctd-row AS ROWID      NO-UNDO.
DEFINE INPUT  PARAMETER ipi-t-qty       AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER opd-cost        AS DECIMAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opc-uom         AS CHAR       NO-UNDO.
DEFINE OUTPUT PARAMETER opd-ext-cost    AS DECIMAL    NO-UNDO.
DEF VAR vdCost AS DECIMAL NO-UNDO.
DEF VAR vcUOM  AS CHAR    NO-UNDO.
DEF VAR vcPo-no AS CHAR NO-UNDO.
DEF VAR vcJob-no AS CHAR NO-UNDO.
DEF VAR viJob-no2 AS INT NO-UNDO.

FIND fg-rctd WHERE ROWID(fg-rctd) EQ ipr-fg-rctd-row NO-LOCK NO-ERROR.
IF NOT AVAIL fg-rctd THEN
    RETURN.
vcPo-no = fg-rctd.po-no.
vcJob-no = fg-rctd.job-no.
viJob-no2 = fg-rctd.job-no2.

IF fg-rctd.tag GT "" THEN DO:
    FIND FIRST loadtag WHERE loadtag.company = fg-rctd.company
                AND loadtag.ITEM-type = NO
                AND loadtag.tag-no = fg-rctd.tag
    NO-LOCK NO-ERROR.
    IF AVAIL loadtag THEN DO:
        IF vcPo-no EQ "" THEN
            vcPo-no = string(loadtag.po-no).
        IF vcJob-no EQ "" THEN
            ASSIGN vcJob-no = loadtag.job-no
                   viJob-no2 = loadtag.job-no2.
    END.
END.

IF vcJob-no GT "" THEN DO:
  FIND FIRST job-hdr 
    WHERE job-hdr.company EQ fg-rctd.company
      AND job-hdr.job-no EQ vcJob-no
      AND job-hdr.i-no   EQ fg-rctd.i-no
      AND job-hdr.job-no2 EQ vijob-no2
    NO-LOCK NO-ERROR.
  IF AVAIL job-hdr THEN DO:
     vdCost = job-hdr.std-tot-cost.
  END.
                   
END.

IF vdCost EQ 0 AND vcPo-no GT "" THEN DO:
  FIND FIRST po-ordl 
    WHERE po-ordl.company EQ fg-rctd.company
      AND po-ordl.po-no   EQ INTEGER(vcPo-no)
      AND po-ordl.i-no    EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.
  IF AVAIL po-ordl THEN
      ASSIGN vdCost = po-ordl.cons-cost
             vcUOM  = po-ordl.cons-uom.
       
END.

IF vdCost GT 0 THEN
    ASSIGN opd-ext-cost = ipi-t-qty * vdCost / IF vcUOM EQ "M" THEN 1000
        ELSE 1
           opd-cost     = vdCost
           opc-uom      = IF vcUom GT "" THEN vcUOM ELSE "EA".
ELSE
    ASSIGN opd-ext-cost = 0
       opd-cost         = 0
       opc-uom          = "M".
