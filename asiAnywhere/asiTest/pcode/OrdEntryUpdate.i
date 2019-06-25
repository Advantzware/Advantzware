
/*------------------------------------------------------------------------
    File        : UpdateOrdEntry.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for View Order

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttUpdateOrdEntry NO-UNDO
    FIELD  VOrderNum    AS INT     
    FIELD  VEstimate    AS CHAR     
    FIELD  VJob         AS CHAR     
    FIELD  VJob2        AS INT      
    FIELD  VCustomer    AS CHAR     
    FIELD  VUserid      AS CHAR     
    FIELD  VStat        AS CHAR     
    FIELD  VSold        AS CHAR     
    FIELD  VOrdate      AS DATE     
    FIELD  VSoldName    AS CHAR     
    FIELD  VDueCode     AS CHAR     
    FIELD  VDueDate     AS DATE     
    FIELD  VCustAddr    AS CHAR     
    FIELD  VSoldAddr    AS CHAR     
    FIELD  VLastDate    AS DATE     
    FIELD  VcustAddr2   AS CHAR     
    FIELD  VSoldAddr2   AS CHAR     
    FIELD  VProdDate    AS DATE     
    FIELD  VCity        AS CHAR     
    FIELD  VState       AS CHAR     
    FIELD  VZip         AS CHAR     
    FIELD  VSoldCity    AS CHAR     
    FIELD  VSoldState   AS CHAR     
    FIELD  VSoldZip     AS CHAR     
    FIELD  VPonum       AS CHAR     
    FIELD  VContact     AS CHAR     
    FIELD  VOverpct     AS DECIMAL 
    FIELD  VUnderpct    AS DECIMAL 
    FIELD  VTerms       AS CHAR 
    FIELD  VTermdscr    AS CHAR   
    FIELD  VProd        AS INT   
    FIELD  VTaxgr       AS CHAR   
    FIELD  VFreight     AS CHAR   
    FIELD  VCarrier     AS CHAR   
    FIELD  VFob         AS CHAR   
    FIELD  VSman        AS CHAR   
    FIELD  VSname       AS CHAR   
    FIELD  VSpct        AS DECIMAL   
    FIELD  VScomm       AS DECIMAL   
    FIELD  VSman2       AS CHAR    
    FIELD  VSname2      AS CHAR    
    FIELD VSpct2       AS DECIMAL 
    FIELD VScomm2      AS DECIMAL 
    FIELD VSman3       AS CHAR 
    FIELD VSname3       AS CHAR  
    FIELD VSpct3       AS DECIMAL 
    FIELD VScomm3      AS DECIMAL 
    FIELD VCtype       AS CHAR  
    FIELD VcExp        AS DATE  
    FIELD VCnum        AS CHAR  
    FIELD VCauth       AS CHAR  
    FIELD VWhis       AS LOGICAL
    FIELD Carrdscr       AS CHAR  
    FIELD company       AS CHAR  
    FIELD VCustName       AS CHAR
    FIELD VFreightdscr       AS CHAR
    FIELD mand       AS CHAR
    FIELD ordtype       AS CHAR
    FIELD VRowid AS RECID.

DEFINE DATASET dsUpdateOrdEntry FOR ttUpdateOrdEntry.






