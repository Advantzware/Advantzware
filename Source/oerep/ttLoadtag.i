DEFINE {1} TEMP-TABLE ttLoadTag
    FIELD   company         AS      CHARACTER       
    FIELD   orderID         LIKE    oe-ord.ord-no       
    FIELD   jobID           LIKE    oe-ordl.job-no      
    FIELD   jobID2          LIKE    oe-ordl.job-no2     
    FIELD   custID          LIKE    oe-ord.cust-no      
    FIELD   custName        LIKE    oe-ord.cust-name        
    FIELD   custAddrees1    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   custAddress2    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   custCity        LIKE    cust.city       
    FIELD   custState       LIKE    cust.state      
    FIELD   custZip         LIKE    cust.zip        
    FIELD   custCountry     LIKE    cust.country        
    FIELD   itemID          LIKE    oe-ordl.i-no        
    FIELD   custPartNo      LIKE    oe-ordl.part-no     
    FIELD   custPONO        LIKE    oe-ordl.po-no       
    FIELD   ordQuantity     LIKE    oe-ordl.qty     
    FIELD   pcs             LIKE    eb.cas-cnt      
    FIELD   bundle          LIKE    eb.cas-pal  FORMAT  ">>>9"
    FIELD   totalUnit       LIKE    eb.tr-cnt       
    FIELD   totalTags       AS      INTEGER     
    FIELD   shipCountry     LIKE    shipto.country      
    FIELD   soldID          LIKE    soldto.sold-id      
    FIELD   soldName        LIKE    soldto.sold-name        
    FIELD   soldAddress1    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   soldAddress2    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   soldCity        LIKE    soldto.sold-city        
    FIELD   soldState       LIKE    soldto.sold-state       
    FIELD   soldZip         LIKE    soldto.sold-zip     
    FIELD   soldCountry     LIKE    soldto.country      
    FIELD   shipID          LIKE    shipto.ship-id      
    FIELD   shipName        LIKE    shipto.ship-name        
    FIELD   shipAddress1    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   shipAddress2    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   shipCity        LIKE    shipto.ship-city        
    FIELD   shipState       LIKE    shipto.ship-state       
    FIELD   shipZip         LIKE    shipto.ship-zip     
    FIELD   itemName        LIKE    oe-ordl.i-name      
    FIELD   partDscr1       LIKE    oe-ordl.part-dscr1      
    FIELD   partDscr2       LIKE    oe-ordl.part-dscr2      
    FIELD   partDscr3       LIKE    oe-ordl.part-dscr3      
    FIELD   dueDate         LIKE    oe-ord.due-date     
    FIELD   relDate         LIKE    oe-rel.rel-date     
    FIELD   upcNo           LIKE    eb.upc-no       
    FIELD   mult            AS      INTEGER     
    FIELD   estID           LIKE    oe-ordl.est-no      
    FIELD   formNo          LIKE    oe-ordl.form-no     
    FIELD   boxLen          AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   boxWid          AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   boxDep          AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   flute           AS      CHARACTER       
    FIELD   test            AS      CHARACTER       
    FIELD   vendor          LIKE    company.name        
    FIELD   grossWeight     AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   tareWeight      AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   netWeight       AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   sheetWieght     AS      DECIMAL FORMAT  ">>>9.99"
    FIELD   uom             LIKE    oe-ordl.pr-uom      
    FIELD   partial         AS      INTEGER FORMAT  ">>>,>>9"
    FIELD   dontrunset      AS      LOGICAL
    FIELD   caseNo          LIKE    eb.cas-no       
    FIELD   prodNotes       LIKE    itemfg.prod-notes       
    FIELD   poID            LIKE    po-ord.po-no        
    FIELD   lcode           AS      CHARACTER   FORMAT  "X"
    FIELD   caseWeight      AS      INTEGER     FORMAT  ">>>>9"
    FIELD   lotID           AS      CHARACTER   FORMAT  "X(15)"
    FIELD   rellotID        AS      CHARACTER   FORMAT  "X(15)"
    FIELD   drawID          AS      CHARACTER   FORMAT  "X(15)"
    FIELD   overPct         LIKE    oe-ordl.over-pct        
    FIELD   qtyBefore       LIKE    oe-ordl.qty     
    FIELD   style           AS      CHARACTER       
    FIELD   styleDesc       AS      CHARACTER   FORMAT  "X(30)"
    FIELD   orddeSC1        AS      CHARACTER   FORMAT  "X(30)"
    FIELD   orddeSC2        AS      CHARACTER   FORMAT  "X(30)"
    FIELD   dueDateJob      AS      CHARACTER    
    FIELD   dueDateJobHdr   AS      CHARACTER   
    FIELD   isComponent     AS      LOGICAL     
    FIELD   lineID          LIKE    oe-ordl.e-num       
    FIELD   unitWeight      LIKE    loadtag.misc-dec[1]     
    FIELD   palletWeight    LIKE    loadtag.misc-dec[2]     
    FIELD   palletID        LIKE    eb.tr-no        
    FIELD   shipNotes       LIKE    oe-rel.ship-i       
    FIELD   SSCC            AS      CHARACTER   FORMAT  "X(20)"
    FIELD   jobQuantity     AS      INTEGER     
    FIELD   runShip         AS      LOGICAL     
    FIELD   relQuantity     LIKE    oe-ordl.qty     
    FIELD   relID           AS      INTEGER     
    FIELD   poLineID        AS      INTEGER     
    FIELD   zoneID          AS      CHARACTER       
    FIELD   broker          AS      LOGICAL     
    FIELD   ipReturn        AS      LOGICAL     
    .                   
