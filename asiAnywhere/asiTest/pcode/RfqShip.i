


/*------------------------------------------------------------------------
    File        : RfqShip.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : jyoti bajaj
    Created     : Mon March 10, 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRfqShipping NO-UNDO 
    FIELD vShipid    AS CHARACTER FORMAT "x(8)"
    FIELD VShipname AS CHARACTER 
    FIELD vCarrier   AS CHARACTER FORMAT "x(5)" 
    FIELD Vcarrdscr  AS CHARACTER
    FIELD vCasno     AS CHARACTER FORMAT "x(10)" 
    FIELD VTrno      AS CHARACTER FORMAT "x(10)"
    FIELD vWeight    AS DECIMAL FORMAT ">>9.99"  
    FIELD vCasCost   AS DECIMAL FORMAT ">9.99"  
    FIELD vTrCost    AS DECIMAL FORMAT ">9.99"  
    FIELD vCascnt    AS INTEGER FORMAT ">>>>9"
    FIELD vTrcnt     AS INTEGER FORMAT ">>>>9"
    FIELD vCaslen    AS DECIMAL FORMAT ">9.9999"  
    FIELD vTrlen     AS DECIMAL FORMAT ">9.9999"  
    FIELD vTrwid     AS DECIMAL FORMAT ">9.9999"
    FIELD vCasWid    AS DECIMAL FORMAT ">9.9999"
    FIELD vCasdep    AS DECIMAL FORMAT ">9.9999"  
    FIELD vTrdep     AS DECIMAL FORMAT ">9.9999"  
    FIELD vCaspal    AS INTEGER FORMAT ">>9"
    FIELD vTrcas     AS INTEGER FORMAT ">>9"
    FIELD vCaswt     AS DECIMAL FORMAT ">>9.99"  
    FIELD RfqSRowid  AS RECID
    FIELD vPallet    AS CHARACTER 
    FIELD vShipAddr  AS CHARACTER FORMAT "x(30)" 
    FIELD vShipAddr2 AS CHARACTER FORMAT "x(15)" 
    FIELD vShipCity  AS CHARACTER FORMAT "x(15)" 
    FIELD vShipState AS CHARACTER FORMAT "x(2)" 
    FIELD vShipZip   AS CHARACTER FORMAT "x(10)"  
               .
    

DEFINE DATASET dsRfqShipping FOR ttRfqShipping.
DEFINE QUERY q-RfqShippingQuery FOR ttRfqShipping.
DEFINE DATA-SOURCE src-RfqShipping  FOR QUERY q-RfqShippingQuery.
BUFFER ttRfqShipping :ATTACH-DATA-SOURCE(DATA-SOURCE src-RfqShipping  :HANDLE).


