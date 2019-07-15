

/*------------------------------------------------------------------------
    File        : RfqPrinting.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : jyoti bajaj
    Created     : Sat March 03, 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRfqPrinting NO-UNDO 
    FIELD vPcol    AS integer FORMAT "99" 
    FIELD vPass    AS integer FORMAT "99"
    FIELD vCoat    AS integer FORMAT "99"
    FIELD vColdscr AS CHAR FORMAT "x(40)"
    FIELD vIps1    AS integer FORMAT "99"  
    FIELD vIps2    AS integer FORMAT "99"
    FIELD vIps3    AS integer FORMAT "99"
    FIELD vIps4    AS integer FORMAT "99"
    FIELD vIps5    AS integer FORMAT "99"
    FIELD vIps6    AS integer FORMAT "99"
    FIELD vIps7    AS integer FORMAT "99"
    FIELD vIps8    AS integer FORMAT "99"
    FIELD vIps9    AS integer FORMAT "99"
    FIELD vIps10   AS integer FORMAT "99"
    FIELD vIcode1  AS CHAR FORMAT "x(40)"
    FIELD vIcode2  AS CHAR FORMAT "x(40)"
    FIELD vIcode3  AS CHAR FORMAT "x(40)"
    FIELD vIcode4  AS CHAR FORMAT "x(40)"
    FIELD vIcode5  AS CHAR FORMAT "x(40)"
    FIELD vIcode6  AS CHAR FORMAT "x(40)"
    FIELD vIcode7  AS CHAR FORMAT "x(40)"
    FIELD vIcode8  AS CHAR FORMAT "x(40)"
    FIELD vIcode9  AS CHAR FORMAT "x(40)"
    FIELD vIcode10 AS CHAR FORMAT "x(40)"
    FIELD vCdscr1  AS CHAR FORMAT "x(19)"
    FIELD vCdscr2  AS CHAR FORMAT "x(19)"
    FIELD vCdscr3  AS CHAR FORMAT "x(19)"
    FIELD vCdscr4  AS CHAR FORMAT "x(19)"
    FIELD vCdscr5  AS CHAR FORMAT "x(19)"
    FIELD vCdscr6  AS CHAR FORMAT "x(19)"
    FIELD vCdscr7  AS CHAR FORMAT "x(19)"
    FIELD vCdscr8  AS CHAR FORMAT "x(19)"
    FIELD vCdscr9  AS CHAR FORMAT "x(19)"
    FIELD vCdscr10 AS CHAR FORMAT "x(19)"
    FIELD vIper1   AS integer FORMAT "999"
    FIELD vIper2   AS integer FORMAT "999"
    FIELD vIper3   AS integer FORMAT "999"
    FIELD vIper4   AS integer FORMAT "999" 
    FIELD vIper5   AS integer FORMAT "999" 
    FIELD vIper6   AS integer FORMAT "999" 
    FIELD vIper7   AS integer FORMAT "999"                       
    FIELD vIper8   AS integer FORMAT "999" 
    FIELD vIper9   AS integer FORMAT "999" 
    FIELD vIper10  AS integer FORMAT "999"
    FIELD RfqPRowid AS RECID .
    

DEFINE DATASET dsRfqPrinting FOR ttRfqPrinting.
DEFINE QUERY q-RfqPrintingQuery FOR ttRfqPrinting.
DEFINE DATA-SOURCE src-RfqPrinting  FOR QUERY q-RfqPrintingQuery.
BUFFER ttRfqPrinting :ATTACH-DATA-SOURCE(DATA-SOURCE src-RfqPrinting  :HANDLE).

