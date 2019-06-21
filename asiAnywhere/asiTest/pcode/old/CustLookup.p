
/*------------------------------------------------------------------------
    File         : CustomerLookup
    Purpose     : customer lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{CustLookup.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER Customer as character no-undo.
DEFINE INPUT PARAMETER Name as character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustomer.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

ASSIGN prmAction = "search".

IF Customer = ? THEN ASSIGN Customer = "".
IF Name = ? THEN ASSIGN Name = "".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "search" THEN DO:        
        RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).        
        q-CustomerQuery:QUERY-PREPARE(v-qry-string).        
        DATASET dsCustomer:FILL().

        FOR EACH cust where cust.cust-no = Customer and cust.name = Name no-lock.
            IF available cust then do:
            create ttCustomer.
            assign 
                ttCustomer.Customer = string(cust.cust-no)
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr.
            END.  /*IF available cust */
        END.  /*FOR EACH cust*/
    END. /*WHEN "search" THEN DO: */
END CASE.
/* ***************************  Procedures  *************************** */

PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    ASSIGN prm-query = " PRESELECT EACH oe-ordl NO-LOCK WHERE oe-ordl.company = '" + prmComp + "' and oe-ordl.cust-no = '" + prmCust + "' ".
    
    IF Customer <> "" THEN ASSIGN prm-query = prm-query + " and cust.cust-no begins '" + Customer  + "'".
    IF Name <> "" THEN ASSIGN prm-query = prm-query + " and cust.name begins '" + Name + "'".
    
END PROCEDURE.
              







