/*------------------------------------------------------------------------
    File        : oe856asn.p
    Purpose     : 

    Syntax      :

    Description : Create edi asn records (edshtran ...)

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/***************************************************************************/

/* ***************************  Definitions  ************************** */

{edi/sharedv.i}
{edi/edivars.i}

DEFINE /*shared*/ VARIABLE v-emp-id      LIKE soldto.sold-id NO-UNDO.
DEFINE /*shared*/ VARIABLE v-emp-recid   AS RECID   NO-UNDO.
DEFINE /*SHARED*/ VARIABLE v-mast-bol-no LIKE oe-bolh.bol-no FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE local-debug   AS LOGICAL NO-UNDO INITIAL FALSE.
DEFINE VARIABLE i             AS INTEGER NO-UNDO.
DEFINE BUFFER dc FOR edshipto.
DEFINE VARIABLE ws_bol_chardate AS CHARACTER NO-UNDO FORMAT "x(08)".
DEFINE VARIABLE ws_userref      LIKE eddoc.userref NO-UNDO.
DEFINE VARIABLE ws_pallet-mark  LIKE edshline.pallet-mark NO-UNDO.
DEFINE VARIABLE ws_carton-mark  LIKE edshline.carton-mark NO-UNDO.

/*/*/*DEFINE VARIABLE ws_company AS CHARACTER NO-UNDO.*/*/   */
/*/*DEFINE VARIABLE ws_partner AS CHARACTER NO-UNDO.    */   */
/*DEFINE VARIABLE bill_of_lading_number AS CHARACTER NO-UNDO.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND oe-bolh WHERE RECID(oe-bolh) = ws_process_rec NO-LOCK NO-ERROR.
IF NOT AVAILABLE oe-bolh THEN RETURN.

FIND edmast WHERE RECID(edmast) = ws_edmast_rec
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE edmast THEN RETURN.

FIND edcode WHERE /*RECID(edcode) = ws_edcode_rec*/
    EDCode.Partner = EDMast.Partner 
    AND EDCode.SetID = "856"
    AND EDCode.Direction = "o"
    AND EDCode.Version = 1
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE edcode THEN 
DO: 
    CREATE EDCode.
    ASSIGN 
        EDCode.Partner   = EDMast.partner
        EDCode.SetID     = "856"
        EDCode.Direction = "O" /* for Outbound */
        EDCode.Version   = 1.
END.

ASSIGN
    ws_company            = oe-bolh.company
    /*  ws_partner = edmast.partner*/
    ws_bol_chardate       = STRING(YEAR (oe-bolh.bol-date),"9999")
	+ string(MONTH(oe-bolh.bol-date),"99")
	+ string(DAY  (oe-bolh.bol-date),"99")
    bill_of_lading_number = STRING(oe-bolh.bol-no)  /*string(v-mast-bol-no)*/
    ws_userref            = ws_bol_chardate + '-' + oe-bolh.trailer
    .
    

FOR EACH oe-boll
    WHERE oe-boll.b-no = oe-bolh.b-no
    AND can-do("S,B", oe-boll.s-code)   /* not invoice only */
    :
        
    /*  FIND carrier OF oe-bolh NO-LOCK NO-ERROR. */
    FIND carrier WHERE carrier.company EQ oe-bolh.company
        AND carrier.carrier EQ oe-bolh.carrier
        AND carrier.loc     EQ oe-bolh.loc
        USE-INDEX carrier NO-LOCK NO-ERROR.
    IF AVAILABLE carrier THEN
        FIND edshipvia
            WHERE edshipvia.carrier = carrier.carrier NO-LOCK NO-ERROR.
    FIND oe-ord OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-boll AND AVAILABLE oe-ord
        THEN
        FIND oe-ordl OF oe-ord
            WHERE oe-ordl.line = oe-boll.line NO-LOCK NO-ERROR.
    ELSE
        FIND FIRST oe-ordl
            WHERE oe-ordl.company = oe-boll.company
            AND oe-ordl.ord-no = oe-boll.ord-no
            AND oe-ordl.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
    /* 9706 CAH: v-emp-id is set by caller, contains the DC ship code.  If blank
      we can fallback to sold-id in order */
    IF v-emp-id = "" AND AVAILABLE oe-ord
        THEN v-emp-id = oe-ord.sold-id.
    IF AVAILABLE oe-ord THEN 
    DO:
        /* asi does not have these fields */
        DEFINE VARIABLE p1 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE p2 AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i1 AS INTEGER   NO-UNDO.
        DEFINE VARIABLE i2 AS INTEGER   NO-UNDO.
        p1 = oe-ord.po-no.
        p2 = "".
        i1 = 1.
        i2 = 0.
        /* create a string with only numeric digits from the po number */
  
        DO i1 = 1 TO LENGTH(p1):
            IF  SUBSTRING(p1,i1,1) >= "0"
                AND substring(p1,i1,1) <= "9"
                THEN ASSIGN i2                 = i2 + 1
                    SUBSTRING(p2,i2,1) = SUBSTRING(p1,i1,1).
        END.
        ws_pallet-mark = INTEGER(p2).
        ws_carton-mark = oe-ord.po-no.
    END.  /* if avail oe-ord */
    ELSE ASSIGN ws_pallet-mark = oe-boll.ord-no
            ws_carton-mark = STRING(oe-boll.line).
    FIND FIRST inv-line
        WHERE inv-line.company = oe-bolh.company
        AND   inv-line.ord-no = oe-boll.ord-no
        AND   inv-line.line   = oe-boll.line
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE inv-line THEN
        FIND FIRST inv-line
            WHERE inv-line.company = oe-bolh.company
            AND   inv-line.ord-no = oe-boll.ord-no
            AND   inv-line.i-no   = oe-boll.i-no
            NO-LOCK NO-ERROR.
    IF AVAILABLE inv-line THEN
    DO:
        RUN edi/ed/asi/lkupstby.p
            (INPUT RECID(inv-line),
            OUTPUT location_number,
            OUTPUT ordering_store_number).
        FIND inv-head OF inv-line NO-LOCK NO-ERROR.
    END.
    ELSE
        ASSIGN location_number       = "" ordering_store_number = "".
    FIND FIRST edshipto
        WHERE edshipto.partner = edmast.partner
        AND edshipto.ref-type = "by"
        AND edshipto.cust = edmast.cust
        AND edshipto.ship-to = oe-bolh.ship-id NO-LOCK NO-ERROR.
    IF AVAILABLE edshipto THEN ordering_store_number = edshipto.by-code.
  
    IF local-debug AND NOT AVAILABLE edshipto THEN
    DO:
        BELL.
        MESSAGE "Could not locate BY via;" edmast.partner "by" edmast.cust
            oe-bolh.ship-id.
        PAUSE.
    END.
    /* 9706 CAH: v-emp-id overrides st code */
    IF v-emp-id > "" THEN location_number = v-emp-id.
  
    FIND FIRST edpotran
        WHERE edpotran.partner      = edmast.partner
        AND edpotran.cust-po        = oe-ord.po-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE edpotran
        THEN
    DO:
        FIND FIRST edpoline OF edpotran
            WHERE edpoline.line = oe-boll.line NO-LOCK NO-ERROR.
        IF NOT AVAILABLE edpoline
            THEN
            FIND FIRST edpoline OF edpotran
                WHERE edpoline.item-no = oe-boll.i-no
                AND edpoline.by-code = ordering_store_number NO-LOCK NO-ERROR.
        IF NOT AVAILABLE edpoline
            THEN
            FIND FIRST edpoline OF edpotran
                WHERE edpoline.item-no = oe-boll.i-no
                AND edpoline.st-code = ordering_store_number NO-LOCK NO-ERROR.
    END.
    IF AVAILABLE oe-ordl THEN
        FIND FIRST uom WHERE uom.uom = oe-ordl.pr-uom.
    FIND FIRST dc
        WHERE dc.partner = edmast.partner
        AND dc.ref-type = "st"
        AND dc.cust = edmast.cust
        AND dc.ship-to = v-emp-id /* oe-ord.sold-id 9706 CAH */ NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dc THEN
        FIND FIRST dc
            WHERE dc.partner = edmast.partner
            AND dc.ref-type = "by"
            AND dc.cust = edmast.cust
            AND dc.ship-to = v-emp-id /* oe-ord.sold-id 9706 CAH */ NO-LOCK NO-ERROR.
    IF local-debug AND NOT AVAILABLE dc THEN
    DO:
        BELL.
        MESSAGE "Could not locate ST via:" edmast.partner "by" edmast.cust
            "st" oe-ord.sold-id "emp" v-emp-id.
        PAUSE.
    END.
    FIND FIRST edshtran
        WHERE edshtran.bol-no = bill_of_lading_number
        AND edshtran.st-code = v-emp-id
    EXCLUSIVE NO-ERROR.
    IF NOT AVAILABLE edshtran THEN
    DO:
      
        RUN edi/ed/gendoc.p (RECID(edcode), bill_of_lading_number, OUTPUT ws_eddoc_rec).
        FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec exclusive.
        IF AVAILABLE EDDoc THEN
            ASSIGN
                eddoc.docseq  = ws_eddoc_rec /*INTEGER(location_number)*/
                eddoc.st-code = location_number
                eddoc.userref = ws_userref
                /* "B-NO: " + STRING(oe-bolh.b-no) */
                .

        CREATE edshtran.
        ASSIGN
            edshtran.partner          = eddoc.partner
            edshtran.seq              = eddoc.seq
            edshtran.bol-no           = bill_of_lading_number
            /* STRING(oe-bolh.bol-no) */
            edshtran.st-code          = location_number /* 970610 CAH: dc.st-code */
            edshtran.ship-name        = oe-ord.sold-name
            edshtran.ship-address[1]  = oe-ord.sold-addr[1]
            edshtran.ship-address[2]  = oe-ord.sold-addr[2]
            edshtran.ship-city        = oe-ord.sold-city
            edshtran.ship-st          = oe-ord.sold-state
            edshtran.ship-zip         = oe-ord.sold-zip
            edshtran.ship-date-code   = "011"   /* shipped */
            edshtran.ship-date        = oe-bolh.bol-date
            edshtran.ship-time        = TIME
            edshtran.ship-method-code = "T"   /* best way */
            edshtran.carrier          = oe-bolh.carrier
            edshtran.carrier-code     = (IF AVAILABLE edshipvia AND edshipvia.carrier-code > ""
      THEN edshipvia.carrier-code ELSE "")
            edshtran.trailer          = oe-bolh.trailer
            edshtran.sf-code          = oe-bolh.loc
            edshtran.vn-code          = edmast.we-vend-no
            edshtran.bol-adddate      = TODAY
            edshtran.bol-addtime      = TIME
            edshtran.package-code     = (IF oe-bolh.tot-pallets > 0 THEN "PLT" ELSE "CTN")
            + "25" /* corrugated cartons */.
        .
        DO i = 1 TO 4:
            edshtran.routing[i] = oe-bolh.ship-i[i].
        END.
    END.

    FIND FIRST edshline
        WHERE edshline.partner = edshtran.partner
        AND edshline.seq = edshtran.seq
        AND edshline.bol-no = edshtran.bol-no
        AND edshline.st-code = edshtran.st-code
        AND edshline.cust-po = oe-ord.po-no
        AND edshline.cust-po-line = STRING(oe-ordl.line)
        AND edshline.pallet-mark = ws_pallet-mark /* edivline.pallet-mark */
        AND edshline.carton-mark = ws_carton-mark /* edivline.carton-mark */
        AND edshline.by-code = ordering_store_number /* edshipto.by-code */
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edshline THEN
    DO:
        FIND itemfg
            WHERE itemfg.company = oe-boll.company
            AND itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
        CREATE edshline.
        ASSIGN
            edshline.partner      = edshtran.partner
            edshline.seq          = edshtran.seq
            edshline.invoice-no   = STRING(oe-boll.inv-no)
            edshline.uom-code     = oe-ordl.pr-uom
            edshline.bol-no       = edshtran.bol-no
            edshline.st-code      = edshtran.st-code
            edshline.cust-po      = oe-ord.po-no
            edshline.cust-po-line = STRING(oe-ordl.line)
            edshline.cust-po-date = oe-ord.ord-date
            edshline.cust-item    = oe-ordl.part-no
            edshline.qty-orig-ord = oe-ordl.qty
            edshline.order-no     = STRING(oe-boll.ord-no)
            edshline.upc          = IF AVAILABLE itemfg THEN itemfg.upc ELSE ""
            edshline.by-code      = ordering_store_number /* 9706: edshipto.by-code */
            edshline.pallet-mark  = ws_pallet-mark /* edivline.pallet-mark */
            edshline.carton-mark  = ws_carton-mark
            edshline.unit-price   = oe-ordl.price
            edshline.pack-size    = oe-boll.qty-case
            edshline.item-no      = oe-boll.i-no
            edshtran.lines        = edshtran.lines + 1
            .
    END.

    ASSIGN
    {edi/accum.i edshline.qty-shipped "oe-boll.qty" }
    {edi/accum.i edshline.tot-cartons oe-boll.cases}
    {edi/accum.i edshline.tot-volume  0}
    {edi/accum.i edshline.tot-wght    oe-boll.weight}
        edshline.case-wght = IF edshline.tot-cartons > 0 THEN
	ROUND(edshline.tot-wght / edshline.tot-cartons, 0)
	ELSE 0
        {edi/accum.i edshtran.tot-cartons oe-boll.cases}
        {edi/accum.i edshtran.tot-volume  0}
        {edi/accum.i edshtran.tot-wght    oe-boll.weight}
        .
    FIND FIRST edshord OF edshtran
        WHERE   edshord.bol-no = edshline.bol-no
        AND     edshord.cust-po = edshline.cust-po
        AND     edshord.order-no = edshline.order-no
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edshord THEN
    DO:
        CREATE edshord.
        ASSIGN
            edshord.partner      = edshtran.partner
            edshord.seq          = edshtran.seq
            edshord.bol-no       = edshline.bol-no
            edshord.cust-po      = edshline.cust-po
            edshord.order-no     = edshline.order-no
            edshord.by-code      = edshline.by-code
            edshord.st-code      = edshline.st-code
            edshord.cust-po-date = edshline.cust-po-date
            edshord.invoice-no   = edshline.invoice-no
            edshord.invoice-date = IF AVAILABLE inv-head THEN inv-head.inv-date ELSE ?
            edshord.cust-dept    = IF AVAILABLE edpotran THEN edpotran.cust-dept ELSE ""
            edshord.cust-div     = IF AVAILABLE edpotran THEN edpotran.cust-div ELSE
	IF AVAILABLE itemfg   THEN itemfg.procat ELSE ""
            .
    END.
  
  ASSIGN
    {edi/incr.i edshord.lines}
    edshord.last-line = MAX(edshord.last-line,
    INTEGER(edshline.cust-po-line))
    {edi/accum.i edshord.tot-cartons edshline.tot-cartons}
    {edi/accum.i edshord.tot-volume  edshline.tot-volume}
    {edi/accum.i edshord.tot-wght    edshline.tot-wght}
    .
    FIND FIRST edshtare OF edshtran
        WHERE edshtare.bol-no = edshtran.bol-no
        AND edshtare.pallet-mark = edshline.pallet-mark
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edshtare THEN
    DO:
        CREATE edshtare.
        ASSIGN
            edshtare.partner      = edshtran.partner
            edshtare.seq          = edshtran.seq
            edshtare.bol-no       = edshline.bol-no
            edshtare.pallet-mark  = edshline.pallet-mark
            edshtare.cust-po      = edshline.cust-po
            edshtare.package-code = ""
            .
    END.
  
  ASSIGN
    {edi/accum.i edshtare.tot-cartons edshline.tot-cartons}
    {edi/accum.i edshtare.tot-volume  edshline.tot-volume}
    {edi/accum.i edshtare.tot-wght    edshline.tot-wght}
    .
    FIND FIRST edshpack OF edshtran
        WHERE edshpack.bol-no = edshline.bol-no
        AND edshpack.pallet-mark = edshline.pallet-mark
        AND edshpack.carton-mark = edshline.carton-mark
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edshpack THEN
    DO:
        CREATE edshpack.
        ASSIGN
            edshpack.partner      = edshtran.partner
            edshpack.seq          = edshtran.seq
            edshpack.bol-no       = edshline.bol-no
            edshpack.pallet-mark  = edshline.pallet-mark
            edshpack.carton-mark  = edshline.carton-mark
            edshpack.cust-po      = edshline.cust-po
            edshpack.package-code = ""
            .
    END.
  ASSIGN
    {edi/accum.i edshpack.tot-cartons edshline.tot-cartons}
    {edi/accum.i edshpack.tot-volume  edshline.tot-volume}
    {edi/accum.i edshpack.tot-wght    edshline.tot-wght}
    .
    
END.    /* each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no */
FIND FIRST edmast NO-LOCK WHERE edmast.partner EQ edshtran.partner NO-ERROR.
IF AVAILABLE edmast AND edmast.partnerGrp EQ "GE" THEN 
  RUN edi/ge856xml.p  (oe-bolh.bol-no, EDSHTran.partner, EDSHTran.seq).
ELSE 
  RUN edi/sp856xml.p (oe-bolh.bol-no, EDSHTran.partner, EDSHTran.seq).

