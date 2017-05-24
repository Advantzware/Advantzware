
/*------------------------------------------------------------------------
    File        : custom.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Apr 18 20:25:17 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

def temp-table SupplierOrdersLink
  XML-NODE-NAME "SupplierOrders"
  field supplierOrderId as char xml-node-type "hidden"
    index idx is primary unique supplierOrderId.
  
def temp-table ItemLink
  XML-NODE-NAME "Items"
  field supplierOrderId as char xml-node-type "hidden"
    index idx is primary unique supplierOrderId.

def temp-table ShipmentLink
  XML-NODE-NAME "Shipments"
  field supplierOrderId as char xml-node-type "hidden"
    index idx is primary unique SupplierOrderId.

def temp-table SupplierOrders no-undo
  XML-NODE-NAME "SupplierOrder"
  field dealerPONumber  as char
  field supplierOrderId as char
  field supplierCustom1 as char
  field orderStatusCode as char
  field orderMessage    as char
  field discounts       as deci
  field OtherCharges    as deci
  field Total           as deci
    index idx is primary unique supplierOrderId.    
  
def temp-table OrderItems no-undo
  XML-NODE-NAME "Item"
  field supplierOrderId   as char xml-node-type "hidden"
  field supplierSKU       as char
  field productName       as char
  field quantity          as inte
  field cost              as deci
  field ItemStatusCode    as char
  field ItemStatusMessage as char
    index idx is primary unique supplierOrderId supplierSKU.

def temp-table ShipItems no-undo
  XML-NODE-NAME "Items"
  field shipId            as char xml-node-type "hidden"
  field supplierOrderId   as char xml-node-type "hidden"
  field supplierSKU       as char
  field productName       as char
  field quantity          as inte
  field cost              as deci
  field ItemStatusCode    as char
    index idx is primary unique ShipId supplierSKU.

def temp-table Shipments no-undo
  XML-NODE-NAME "Shipment"    
  field supplierOrderId as char xml-node-type "hidden"
  field shipId          as char
  field shipper         as char
  field trackingNum     as char
  field shipDate        as date
  field shippingCost    as deci
  field shippingTotal   as deci
    index idx is primary unique SupplierOrderId ShipId.
  
  
def DATASET OrderUpdate
  for orderId
      ,SupplierOrdersLink  
      ,SupplierOrders
      ,ItemLink
      ,OrderItems
      ,Shipments
      ,ShipItems
      ,ShipmentLink
      
      DATA-RELATION drSuppLink FOR SupplierOrdersLink, SupplierOrders
      RELATION-FIELDS(SupplierOrderId, SupplierOrderId) NESTED

      DATA-RELATION drItemLink FOR ItemLink, OrderItems
      RELATION-FIELDS(supplierOrderId, supplierOrderId) NESTED

      DATA-RELATION drSuppItem FOR SupplierOrders, ItemLink
      RELATION-FIELDS(SupplierOrderId, SupplierOrderId) NESTED
      
      DATA-RELATION drShipLink FOR ShipmentLink, Shipments
      RELATION-FIELDS(supplierOrderId, supplierOrderId) NESTED
      
      DATA-RELATION drSuppShip FOR SupplierOrders, ShipmentLink
      RELATION-FIELDS(SupplierOrderId, SupplierOrderId) NESTED
      
      DATA-RELATION drShipItems FOR Shipments, ShipItems
      RELATION-FIELDS(ShipId, ShipId) NESTED
     

- See more at: https://community.progress.com/community_groups/openedge_development/f/19/t/1803#sthash.bnNFz7M0.dpuf