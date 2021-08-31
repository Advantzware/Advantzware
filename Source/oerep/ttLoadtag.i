DEFINE TEMP-TABLE ttLoadTag
    FIELD   company                      AS      CHARACTER     
    FIELD   warehouseID                  AS      CHARACTER 
    FIELD   locationID                   AS      CHARACTER  
    FIELD   tag                          AS      CHARACTER FORMAT "X(30)"
    FIELD   rfidTag                      AS      CHARACTER
    FIELD   orderID                      LIKE    oe-ord.ord-no       
    FIELD   jobID                        LIKE    oe-ordl.job-no      
    FIELD   jobID2                       LIKE    oe-ordl.job-no2
    FIELD   bolID                        LIKE    oe-bolh.bol-no     
    FIELD   custID                       LIKE    oe-ord.cust-no      
    FIELD   custName                     LIKE    oe-ord.cust-name        
    FIELD   custAddress1                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   custAddress2                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   custCity                     LIKE    cust.city       
    FIELD   custState                    LIKE    cust.state      
    FIELD   custZip                      LIKE    cust.zip        
    FIELD   custCountry                  LIKE    cust.country 
    FIELD   custAreaCode                 LIKE    cust.area-code
    FIELD   custPhone                    LIKE    cust.phone
    FIELD   custEmail                    LIKE    cust.email
    FIELD   custFax                      LIKE    cust.fax       
    FIELD   itemID                       LIKE    oe-ordl.i-no        
    FIELD   custPartNo                   LIKE    oe-ordl.part-no     
    FIELD   custPONO                     LIKE    oe-ordl.po-no   
    FIELD   quantity                     AS      DECIMAL
    FIELD   quantityTotal                AS      DECIMAL   
    FIELD   ordQuantity                  LIKE    oe-ordl.qty     
    FIELD   quantityInSubUnit            LIKE    eb.cas-cnt 
    FIELD   quantityOfSubUnits           AS      INTEGER     
    FIELD   subUnitsPerUnit              LIKE    eb.cas-pal  FORMAT  ">>>9"
    FIELD   quantityInUnit               AS      INTEGER
    FIELD   quantityOfUnits              AS      INTEGER 
    FIELD   totalTags                    AS      INTEGER     
    FIELD   shipCountry                  LIKE    shipto.country      
    FIELD   soldID                       LIKE    soldto.sold-id      
    FIELD   soldName                     LIKE    soldto.sold-name        
    FIELD   soldAddress1                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   soldAddress2                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   soldCity                     LIKE    soldto.sold-city        
    FIELD   soldState                    LIKE    soldto.sold-state       
    FIELD   soldZip                      LIKE    soldto.sold-zip     
    FIELD   soldCountry                  LIKE    soldto.country      
    FIELD   shipID                       LIKE    shipto.ship-id      
    FIELD   shipName                     LIKE    shipto.ship-name        
    FIELD   shipAddress1                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   shipAddress2                 AS      CHARACTER   FORMAT  "X(30)"
    FIELD   shipCity                     LIKE    shipto.ship-city        
    FIELD   shipState                    LIKE    shipto.ship-state       
    FIELD   shipZip                      LIKE    shipto.ship-zip     
    FIELD   itemName                     LIKE    oe-ordl.i-name      
    FIELD   partDscr1                    LIKE    oe-ordl.part-dscr1      
    FIELD   partDscr2                    LIKE    oe-ordl.part-dscr2      
    FIELD   partDscr3                    LIKE    oe-ordl.part-dscr3      
    FIELD   dueDate                      LIKE    oe-ord.due-date     
    FIELD   relDate                      LIKE    oe-rel.rel-date     
    FIELD   upcNo                        LIKE    eb.upc-no       
    FIELD   printCopies                  AS      INTEGER     
    FIELD   estID                        LIKE    oe-ordl.est-no      
    FIELD   formNo                       LIKE    oe-ordl.form-no    
    FIELD   blankNo                      LIKE    job-hdr.blank-no 
    FIELD   boxLen                       AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   boxWid                       AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   boxDep                       AS      DECIMAL FORMAT  ">>>9.99<<<"
    FIELD   flute                        AS      CHARACTER       
    FIELD   test                         AS      CHARACTER       
    FIELD   vendorID                     AS      CHARACTER
    FIELD   vendorName                   AS      CHARACTER        
    FIELD   grossWeight                  AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   tareWeight                   AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   netWeight                    AS      DECIMAL FORMAT  ">>>>9.99"
    FIELD   sheetWeight                  AS      DECIMAL FORMAT  ">>>9.99"
    FIELD   uom                          LIKE    oe-ordl.pr-uom      
    FIELD   partial                      AS      INTEGER FORMAT  ">>>,>>9"
    FIELD   dontRunSet                   AS      LOGICAL
    FIELD   caseNo                       LIKE    eb.cas-no       
    FIELD   prodNotes                    LIKE    itemfg.prod-notes 
    FIELD   deptNotes                    AS      CHARACTER   FORMAT "X(80)" EXTENT 18      
    FIELD   poID                         LIKE    po-ord.po-no        
    FIELD   lcode                        AS      CHARACTER   FORMAT  "X"
    FIELD   caseWeight                   AS      INTEGER     FORMAT  ">>>>9"
    FIELD   lotID                        AS      CHARACTER   FORMAT  "X(15)"
    FIELD   rellotID                     AS      CHARACTER   FORMAT  "X(15)"
    FIELD   drawID                       AS      CHARACTER   FORMAT  "X(15)"
    FIELD   overPct                      LIKE    oe-ordl.over-pct        
    FIELD   style                        AS      CHARACTER       
    FIELD   styleDesc                    AS      CHARACTER   FORMAT  "X(30)"
    FIELD   orderDesc1                   AS      CHARACTER   FORMAT  "X(30)"
    FIELD   orderDesc2                   AS      CHARACTER   FORMAT  "X(30)"
    FIELD   dueDateJob                   AS      CHARACTER    
    FIELD   dueDateJobHdr                AS      CHARACTER   
    FIELD   isComponent                  AS      LOGICAL     
    FIELD   lineID                       LIKE    oe-ordl.e-num       
    FIELD   unitWeight                   LIKE    loadtag.misc-dec[1]     
    FIELD   palletWeight                 LIKE    loadtag.misc-dec[2]     
    FIELD   palletID                     LIKE    eb.tr-no     
    FIELD   itemPalletID                 LIKE    itemfg.trNo   
    FIELD   shipNotes                    LIKE    oe-rel.ship-i       
    FIELD   SSCC                         AS      CHARACTER   FORMAT  "X(20)"
    FIELD   jobQuantity                  AS      INTEGER     
    FIELD   runShip                      AS      LOGICAL     
    FIELD   relQuantity                  LIKE    oe-ordl.qty     
    FIELD   relID                        AS      INTEGER     
    FIELD   poLineID                     AS      INTEGER     
    FIELD   zoneID                       AS      CHARACTER       
    FIELD   broker                       AS      LOGICAL     
    FIELD   ipReturn                     AS      LOGICAL
    FIELD   extCost                      AS      DECIMAL
    FIELD   tagStatus                    AS      CHARACTER
    FIELD   isChild                      AS      LOGICAL
    FIELD   recordSource                 AS      CHARACTER
    FIELD   recordID                     AS      INTEGER
    FIELD   createdUser                  AS      CHARACTER
    FIELD   createdTime                  LIKE    loadtag.tag-time
    FIELD   createdDate                  LIKE    loadtag.tag-date
    FIELD   tagCounter                   AS      INTEGER
    FIELD   palletCounter                AS      INTEGER
    FIELD   scannedDateTime              AS      DATETIME
    FIELD   isSelected                   AS      LOGICAL
    FIELD   exportFile                   AS      CHARACTER
    FIELD   exportTemplateFile           AS      CHARACTER
    FIELD   exportTemplate               AS      CHARACTER
    FIELD   exportFileType               AS      CHARACTER
    FIELD   errorMessage                 AS      CHARACTER
    FIELD   isError                      AS      LOGICAL
    INDEX   recordID IS PRIMARY UNIQUE recordID
    .                   
