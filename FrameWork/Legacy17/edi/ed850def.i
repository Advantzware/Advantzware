/* Simplied version of Orders.i for ASi xml files */

DEFINE TEMP-TABLE Order NO-UNDO
	FIELD Order_field AS INTEGER 
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Meta NO-UNDO
/*	FIELD Version AS CHARACTER*/
	FIELD Order_id AS RECID
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Header1 NO-UNDO
	XML-NODE-NAME "Header" 
	FIELD Order_id AS RECID 
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE OrderHeader NO-UNDO	 
	FIELD TradingPartnerId AS CHARACTER 
	FIELD PurchaseOrderNumber AS CHARACTER 
	FIELD TsetPurposeCode AS CHARACTER 
	FIELD PrimaryPOTypeCode AS CHARACTER 
	FIELD PurchaseOrderDate AS DATE 
	FIELD BuyersCurrency AS CHARACTER 
	FIELD Vendor AS CHARACTER 
	FIELD Header1_id AS RECID 
	FIELD TotalAmount AS DECIMAL 
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE Dates NO-UNDO
    FIELD DateTimeQualifier AS CHARACTER 
    FIELD Date AS DATE 
    FIELD Header1_id AS RECID 
        XML-NODE-TYPE "HIDDEN" 
    FIELD LineItem_id AS RECID 
        XML-NODE-TYPE "HIDDEN" 
    .
        
DEFINE TEMP-TABLE Contacts NO-UNDO
	FIELD ContactTypeCode AS CHARACTER 
	FIELD ContactName AS CHARACTER 
	FIELD PrimaryPhone AS CHARACTER 
/*	FIELD PrimaryFax AS CHARACTER      */
/*	FIELD PrimaryEmail AS CHARACTER    */
/*	FIELD ContactReference AS CHARACTER*/
	FIELD Header1_id AS RECID
		XML-NODE-TYPE "HIDDEN"
	FIELD Address_id AS RECID
		XML-NODE-TYPE "HIDDEN"
	FIELD LineItem_id AS RECID
		XML-NODE-TYPE "HIDDEN" .
    .

DEFINE TEMP-TABLE Address NO-UNDO
	FIELD AddressTypeCode AS CHARACTER 
	FIELD LocationCodeQualifier AS CHARACTER
	FIELD AddressLocationNumber AS CHARACTER
	FIELD AddressName AS CHARACTER 
	FIELD Address1 AS CHARACTER 
	FIELD Address2 AS CHARACTER
	FIELD City AS CHARACTER 
	FIELD State AS CHARACTER 
	FIELD PostalCode AS CHARACTER 
	FIELD Country AS CHARACTER 
	FIELD Header1_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
	FIELD LineItem_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
    .
    
DEFINE TEMP-TABLE References NO-UNDO
	FIELD ReferenceQual AS CHARACTER 
	FIELD ReferenceID AS CHARACTER 
	FIELD Address_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
	FIELD Header1_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
	FIELD LineItem_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
    .

DEFINE TEMP-TABLE Notes NO-UNDO     
    FIELD NoteCode AS CHARACTER 
    FIELD Note AS CHARACTER 
    FIELD Header1_id AS RECID 
        XML-NODE-TYPE "HIDDEN" 
    FIELD LineItem_id AS RECID 
        XML-NODE-TYPE "HIDDEN" 
    .
        
DEFINE TEMP-TABLE LineItem NO-UNDO	 
	FIELD Order_id AS RECID 
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE OrderLine NO-UNDO	 
	FIELD LineSequenceNumber AS CHARACTER 
	FIELD BuyerPartNumber AS CHARACTER 
	FIELD VendorPartNumber AS CHARACTER 
	FIELD OrderQty AS DECIMAL 
	FIELD OrderQtyUOM AS CHARACTER 
	FIELD PurchasePrice AS DECIMAL 
	FIELD PurchasePriceBasis AS CHARACTER 
	FIELD LineItem_id AS RECID 
		XML-NODE-TYPE "HIDDEN" .

DEFINE TEMP-TABLE ProductOrItemDescription NO-UNDO	 
	FIELD ProductCharacteristicCode AS CHARACTER 
	FIELD ProductDescription AS CHARACTER 
	FIELD LineItem_id AS RECID 
		XML-NODE-TYPE "HIDDEN" 
    .


DEFINE TEMP-TABLE QuantitiesSchedulesLocations NO-UNDO
/*	FIELD QuantityQualifier AS CHARACTER*/
	FIELD TotalQty AS DECIMAL 
	FIELD TotalQtyUOM AS CHARACTER 
	FIELD LineItem_id AS RECID 
		XML-NODE-TYPE "HIDDEN" .


DEFINE TEMP-TABLE Summary NO-UNDO	 
	FIELD TotalAmount AS DECIMAL 
	FIELD TotalLineItemNumber AS INTEGER 
/*	FIELD Description AS CHARACTER*/
	FIELD Order_id AS RECID 
		XML-NODE-TYPE "HIDDEN" .

DEFINE DATASET Orders  
	FOR Order, Meta, Header1, OrderHeader, 
	/* AdditionalPOTypeCodes, PaymentTerms,*/ Dates, Contacts, 
	/* AdditionalContactDetails,*/ Address, References, 
	/* ReferenceIDs, FOBRelatedInstruction, Commodity, Measurements, Paperwork, Packaging, QuantityAndWeight,
       CarrierInformation, ServiceLevelCodes, SealNumbers, CarrierSpecialHandlingDetail, MarksAndNumbersCollection, RestrictionsOrConditions, LeadTime,*/
        Notes, /*Taxes, ChargesAllowances, MonetaryAmounts, QuantityTotals, RegulatoryCompliances,*/
       LineItem, OrderLine,
    /* ProductID, NRFStandardColorAndSize, PriceInformation,*/
       ProductOrItemDescription, 
    /* MasterItemAttribute, ItemAttribute, PhysicalDetails, PalletInformation, FloorReady, Subline, SublineItemDetail,*/
       QuantitiesSchedulesLocations, /* LocationQuantity, ConditionOfSale,*/ Summary
	PARENT-ID-RELATION RELATION1 FOR Order, Meta
		PARENT-ID-FIELD Order_id
/*	PARENT-ID-RELATION RELATION2 FOR OrderHeader, AdditionalPOTypeCodes                                                                                                                                                                                                                                                                              */
/*		PARENT-ID-FIELD OrderHeader_id                                                                                                                                                                                                                                                                                                                  */
/*		PARENT-FIELDS-BEFORE (TradingPartnerId,PurchaseOrderNumber,DepositorOrderNumber,TsetPurposeCode,PrimaryPOTypeCode,PrimaryPOTypeDescription)                                                                                                                                                                                                     */
/*		PARENT-FIELDS-AFTER (ReleaseNumber,PurchaseOrderDate,PurchaseOrderTime,ContractType,SalesRequirementCode,AcknowledgementType,InvoiceTypeCode,ShipCompleteCode,BuyersCurrency,SellersCurrency,ExchangeRate,Department,DepartmentDescription,Vendor,JobNumber,Division,CustomerAccountNumber,CustomerOrderNumber,DocumentVersion,DocumentRevision)*/
	PARENT-ID-RELATION RELATION3 FOR Header1, OrderHeader
		PARENT-ID-FIELD Header1_id
/*	PARENT-ID-RELATION RELATION4 FOR Header1, PaymentTerms*/
/*		PARENT-ID-FIELD Header1_id                           */
	PARENT-ID-RELATION RELATION5 FOR Header1, Dates 
		PARENT-ID-FIELD Header1_id                           
/*	PARENT-ID-RELATION RELATION6 FOR Contacts, AdditionalContactDetails                     */
/*		PARENT-ID-FIELD Contacts_id                                                            */
/*		PARENT-FIELDS-BEFORE (ContactTypeCode,ContactName,PrimaryPhone,PrimaryFax,PrimaryEmail)*/
/*		PARENT-FIELDS-AFTER (ContactReference)                                                 */
	PARENT-ID-RELATION RELATION7 FOR Header1, Contacts
		PARENT-ID-FIELD Header1_id
/*	PARENT-ID-RELATION RELATION8 FOR References, ReferenceIDs*/
/*		PARENT-ID-FIELD References_id                           */
/*	PARENT-ID-RELATION RELATION9 FOR Address, References*/
/*		PARENT-ID-FIELD Address_id                         */
/*	PARENT-ID-RELATION RELATION10 FOR Address, Contacts*/
/*		PARENT-ID-FIELD Address_id                        */
/*	PARENT-ID-RELATION RELATION11 FOR Address, Dates*/
/*		PARENT-ID-FIELD Address_id                     */
	PARENT-ID-RELATION RELATION12 FOR Header1, Address
		PARENT-ID-FIELD Header1_id
/*	PARENT-ID-RELATION RELATION13 FOR Header1, FOBRelatedInstruction       */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION14 FOR Header1, Commodity                   */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION15 FOR Header1, Measurements                */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION16 FOR Header1, Paperwork                   */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION17 FOR Packaging, Measurements              */
/*		PARENT-ID-FIELD Packaging_id                                          */
/*	PARENT-ID-RELATION RELATION18 FOR Header1, Packaging                   */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION19 FOR Header1, QuantityAndWeight           */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION20 FOR CarrierInformation, ServiceLevelCodes*/
/*		PARENT-ID-FIELD CarrierInformation_id                                 */
/*	PARENT-ID-RELATION RELATION21 FOR CarrierInformation, Address          */
/*		PARENT-ID-FIELD CarrierInformation_id                                 */
/*	PARENT-ID-RELATION RELATION22 FOR CarrierInformation, SealNumbers      */
/*		PARENT-ID-FIELD CarrierInformation_id                                 */
/*	PARENT-ID-RELATION RELATION23 FOR Header1, CarrierInformation          */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION24 FOR Header1, CarrierSpecialHandlingDetail*/
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION25 FOR Header1, MarksAndNumbersCollection   */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION26 FOR Header1, RestrictionsOrConditions    */
/*		PARENT-ID-FIELD Header1_id                                            */
/*	PARENT-ID-RELATION RELATION27 FOR LeadTime, References                 */
/*		PARENT-ID-FIELD LeadTime_id                                           */
/*	PARENT-ID-RELATION RELATION28 FOR LeadTime, Notes                      */
/*		PARENT-ID-FIELD LeadTime_id                                           */
/*	PARENT-ID-RELATION RELATION29 FOR Header1, LeadTime                    */
/*		PARENT-ID-FIELD Header1_id                                            */
	PARENT-ID-RELATION RELATION30 FOR Header1, References
		PARENT-ID-FIELD Header1_id
	PARENT-ID-RELATION RELATION31 FOR Header1, Notes
		PARENT-ID-FIELD Header1_id
/*	PARENT-ID-RELATION RELATION32 FOR Header1, Taxes                */
/*		PARENT-ID-FIELD Header1_id                                     */
/*	PARENT-ID-RELATION RELATION33 FOR ChargesAllowances, Taxes      */
/*		PARENT-ID-FIELD ChargesAllowances_id                           */
/*	PARENT-ID-RELATION RELATION34 FOR Header1, ChargesAllowances    */
/*		PARENT-ID-FIELD Header1_id                                     */
/*	PARENT-ID-RELATION RELATION35 FOR Header1, MonetaryAmounts      */
/*		PARENT-ID-FIELD Header1_id                                     */
/*	PARENT-ID-RELATION RELATION36 FOR Header1, QuantityTotals       */
/*		PARENT-ID-FIELD Header1_id                                     */
/*	PARENT-ID-RELATION RELATION37 FOR Header1, RegulatoryCompliances*/
/*		PARENT-ID-FIELD Header1_id                                     */
	PARENT-ID-RELATION RELATION38 FOR Order, Header1
		PARENT-ID-FIELD Order_id
/*	PARENT-ID-RELATION RELATION39 FOR OrderLine, ProductID                                                                                                                                                                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD OrderLine_id                                                                                                                                                                                                                                                                                                                                                                                 */
/*		PARENT-FIELDS-BEFORE (LineSequenceNumber,ApplicationId,BuyerPartNumber,VendorPartNumber,ConsumerPackageCode,EAN,GTIN,UPCCaseCode,NatlDrugCode,InternationalStandardBookNumber)                                                                                                                                                                                                                               */
/*		PARENT-FIELDS-AFTER (OrderQty,OrderQtyUOM,PurchasePriceType,PurchasePrice,PurchasePriceBasis,BuyersCurrency,SellersCurrency,ExchangeRate,ShipDate,ExtendedItemTotal,ProductSizeCode,ProductSizeDescription,ProductColorCode,ProductColorDescription,ProductMaterialCode,ProductMaterialDescription,ProductProcessCode,ProductProcessDescription,Department,DepartmentDescription,Class,Gender,SellerDateCode)*/
/*	PARENT-ID-RELATION RELATION40 FOR OrderLine, NRFStandardColorAndSize                                                                                                                                                                                                                                                                                                                                          */
/*		PARENT-ID-FIELD OrderLine_id                                                                                                                                                                                                                                                                                                                                                                                 */
	PARENT-ID-RELATION RELATION41 FOR LineItem, OrderLine
		PARENT-ID-FIELD LineItem_id
/*	PARENT-ID-RELATION RELATION42 FOR LineItem, Contacts                */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION43 FOR LineItem, Dates                   */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION44 FOR LineItem, Measurements            */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION45 FOR LineItem, PriceInformation        */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION46 FOR LineItem, ProductOrItemDescription*/
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION47 FOR ItemAttribute, Measurements       */
/*		PARENT-ID-FIELD ItemAttribute_id                                   */
/*	PARENT-ID-RELATION RELATION48 FOR MasterItemAttribute, ItemAttribute*/
/*		PARENT-ID-FIELD MasterItemAttribute_id                             */
/*	PARENT-ID-RELATION RELATION49 FOR LineItem, MasterItemAttribute     */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION50 FOR LineItem, Paperwork               */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION51 FOR LineItem, PhysicalDetails         */
/*		PARENT-ID-FIELD LineItem_id                                        */
/*	PARENT-ID-RELATION RELATION52 FOR LineItem, PalletInformation       */
/*		PARENT-ID-FIELD LineItem_id                                        */
	PARENT-ID-RELATION RELATION53 FOR LineItem, References
		PARENT-ID-FIELD LineItem_id
/*	PARENT-ID-RELATION RELATION54 FOR LineItem, Notes                                                                                                                                                                                                                            */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION55 FOR LineItem, FloorReady                                                                                                                                                                                                                       */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION56 FOR LineItem, Address                                                                                                                                                                                                                          */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION57 FOR SublineItemDetail, ProductID                                                                                                                                                                                                               */
/*		PARENT-ID-FIELD SublineItemDetail_id                                                                                                                                                                                                                                        */
/*		PARENT-FIELDS-BEFORE (LineSequenceNumber,ApplicationId,BuyerPartNumber,VendorPartNumber,ConsumerPackageCode,EAN,GTIN,UPCCaseCode,NatlDrugCode,InternationalStandardBookNumber)                                                                                              */
/*		PARENT-FIELDS-AFTER (ProductSizeCode,ProductSizeDescription,ProductColorCode,ProductColorDescription,ProductMaterialCode,ProductMaterialDescription,ProductProcessCode,ProductProcessDescription,QtyPer,QtyPerUOM,PurchasePriceType,PurchasePrice,PurchasePriceBasis,Gender)*/
/*	PARENT-ID-RELATION RELATION58 FOR SublineItemDetail, NRFStandardColorAndSize                                                                                                                                                                                                 */
/*		PARENT-ID-FIELD SublineItemDetail_id                                                                                                                                                                                                                                        */
/*	PARENT-ID-RELATION RELATION59 FOR Subline, SublineItemDetail                                                                                                                                                                                                                 */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION60 FOR Subline, Dates                                                                                                                                                                                                                             */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION61 FOR Subline, PriceInformation                                                                                                                                                                                                                  */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION62 FOR Subline, ProductOrItemDescription                                                                                                                                                                                                          */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION63 FOR Subline, PhysicalDetails                                                                                                                                                                                                                   */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION64 FOR Subline, References                                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION65 FOR Subline, Notes                                                                                                                                                                                                                             */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION66 FOR Subline, FloorReady                                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION67 FOR Subline, Taxes                                                                                                                                                                                                                             */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION68 FOR Subline, ChargesAllowances                                                                                                                                                                                                                 */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION69 FOR Subline, Address                                                                                                                                                                                                                           */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION70 FOR Subline, Commodity                                                                                                                                                                                                                         */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION71 FOR Subline, RegulatoryCompliances                                                                                                                                                                                                             */
/*		PARENT-ID-FIELD Subline_id                                                                                                                                                                                                                                                  */
/*	PARENT-ID-RELATION RELATION72 FOR LineItem, Subline                                                                                                                                                                                                                          */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION73 FOR QuantitiesSchedulesLocations, LocationQuantity                                                                                                                                                                                             */
/*		PARENT-ID-FIELD QuantitiesSchedulesLocations_id                                                                                                                                                                                                                             */
/*		PARENT-FIELDS-BEFORE (QuantityQualifier,TotalQty,TotalQtyUOM,QuantityDescription,LocationCodeQualifier,LocationDescription)                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION74 FOR QuantitiesSchedulesLocations, Dates                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD QuantitiesSchedulesLocations_id                                                                                                                                                                                                                             */
/*		PARENT-FIELDS-AFTER (AssignedID,LeadTimeCode,LeadTimeQuantity,LeadTimePeriodInterval,LeadTimeDate)                                                                                                                                                                          */
/*	PARENT-ID-RELATION RELATION75 FOR LineItem, QuantitiesSchedulesLocations                                                                                                                                                                                                     */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION76 FOR LineItem, Taxes                                                                                                                                                                                                                            */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION77 FOR LineItem, ChargesAllowances                                                                                                                                                                                                                */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION78 FOR LineItem, PaymentTerms                                                                                                                                                                                                                     */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION79 FOR ConditionOfSale, ProductID                                                                                                                                                                                                                 */
/*		PARENT-ID-FIELD ConditionOfSale_id                                                                                                                                                                                                                                          */
/*	PARENT-ID-RELATION RELATION80 FOR LineItem, ConditionOfSale                                                                                                                                                                                                                  */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION81 FOR LineItem, FOBRelatedInstruction                                                                                                                                                                                                            */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION82 FOR LineItem, Commodity                                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION83 FOR LineItem, CarrierInformation                                                                                                                                                                                                               */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION84 FOR LineItem, CarrierSpecialHandlingDetail                                                                                                                                                                                                     */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION85 FOR LineItem, MarksAndNumbersCollection                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION86 FOR LineItem, RestrictionsOrConditions                                                                                                                                                                                                         */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION87 FOR LineItem, Packaging                                                                                                                                                                                                                        */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION88 FOR LineItem, MonetaryAmounts                                                                                                                                                                                                                  */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
/*	PARENT-ID-RELATION RELATION89 FOR LineItem, RegulatoryCompliances                                                                                                                                                                                                            */
/*		PARENT-ID-FIELD LineItem_id                                                                                                                                                                                                                                                 */
	PARENT-ID-RELATION RELATION90 FOR Order, LineItem
		PARENT-ID-FIELD Order_id
	PARENT-ID-RELATION RELATION91 FOR Order, Summary
		PARENT-ID-FIELD Order_id.
