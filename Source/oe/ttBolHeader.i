DEFINE TEMP-TABLE ttBolHeader NO-UNDO
    FIELD riBol         AS ROWID
    FIELD company       AS CHARACTER
    FIELD locationID    AS CHARACTER
    FIELD bolID         AS INTEGER     
    FIELD customerID    AS CHARACTER    
    FIELD carrier       AS CHARACTER    
    FIELD salesMan      AS CHARACTER
    FIELD terms         AS CHARACTER
    FIELD frtTerms      AS CHARACTER
    FIELD poNo          AS CHARACTER
    FIELD JobNo         AS CHARACTER
    FIELD fob           AS CHARACTER
    FIELD shipToBroker  AS LOGICAL
    FIELD ShipName      AS CHARACTER
    FIELD ShipAddr      AS CHARACTER EXTENT 2
    FIELD ShipAddr3     AS CHARACTER
    FIELD CustPhoneNum  AS CHARACTER
    FIELD shipPhone     AS CHARACTER
    FIELD bolPhone      AS CHARACTER
    FIELD shiptoContact AS CHARACTER    
    FIELD companyAdd1   AS CHARACTER
    FIELD companyAdd2   AS CHARACTER
    FIELD companyAdd3   AS CHARACTER 
    FIELD companyAdd4   AS CHARACTER 
    FIELD companyAdd5   AS CHARACTER
    FIELD companyEmail  AS CHARACTER  
    FIELD companyName   AS CHARACTER
    FIELD soldtoName    AS CHARACTER  
    FIELD soldtoAddr    AS CHARACTER EXTENT 2     
    FIELD soldtoAddr3   AS CHARACTER 
    FIELD totalWeight   AS DECIMAL 
    FIELD totalCases    AS INTEGER 

    .
