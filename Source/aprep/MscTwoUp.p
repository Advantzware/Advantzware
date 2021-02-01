/* ---------------------------------------------- aprep/fibremsc.p   */
/* PRINT 2Up1099 1099-MISC                                             */
/* ------------------------------------------------------------------*/

{sys/inc/var.i shared}

{aprep\r-1099m.i}

DEFINE VARIABLE v-comp-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-addr1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-city-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-phone AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-taxid AS CHARACTER NO-UNDO.
DEFINE VARIABLE top-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE copy-count AS INTEGER NO-UNDO.

FIND FIRST company WHERE
     company.company EQ cocode
     NO-LOCK NO-ERROR.

FIND FIRST cust WHERE
     cust.company = cocode AND
     cust.active = "X"
     NO-LOCK NO-ERROR.

IF AVAIL company AND AVAIL cust THEN
DO:
   ASSIGN v-comp-name = cust.NAME
          v-addr1 = cust.addr[1]
          v-city-line = cust.city + "," + " " + cust.state + " " + cust.zip
          v-phone = cust.area-code + cust.phone
          v-taxid = company.fid.
   RELEASE company.
   RELEASE cust.
END.

PUT "<FCourier New><P10>".

DO copy-count = 1 TO s-copies:

   for each tt-1099-m NO-LOCK BREAK BY tt-1099-m.vend-no:
      
          PUT "<R5><C5.5>" v-comp-name format "X(30)" 
              "<R6><C5.5>" v-addr1 format "X(30)" 
              "<R7><C5.5>" v-city-line format "X(30)" 
              "<R8><C5.5>" v-phone format "(999)999-9999"
              "<R13><C5.5>" v-taxid FORMAT "X(15)"
              "<C22.5>" tt-1099-m.vend-tax-id FORMAT "X(15)"
              "<R16><C5.5>" tt-1099-m.vend-name FORMAT "X(30)"
              "<R20><C5.5>" tt-1099-m.vend-add1 FORMAT "X(30)"
              "<R21><C5.5>" tt-1099-m.vend-add2 FORMAT "X(30)"
              "<R23><C5.5>" tt-1099-m.vend-city-line FORMAT "X(30)".

          IF tt-1099-m.vend-box EQ "3" OR tt-1099-m.vend-box EQ "Y" THEN
              PUT "<R11><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" .
          ELSE IF tt-1099-m.vend-box EQ "1" THEN
              PUT "<R5.6><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" .
          ELSE IF tt-1099-m.vend-box EQ "2" THEN
              PUT "<R8.5><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" . 
          ELSE IF tt-1099-m.vend-box EQ "7" THEN
              PUT "<R19><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" . 

            
          PUT "<R39><C5.5>" v-comp-name format "X(30)" 
              "<R40><C5.5>" v-addr1 format "X(30)" 
              "<R41><C5.5>" v-city-line format "X(30)" 
              "<R42><C5.5>" v-phone format "(999)999-9999"
              "<R46><C5.5>" v-taxid FORMAT "X(15)"
              "<C22.5>" tt-1099-m.vend-tax-id FORMAT "X(15)"
              "<R49><C5.5>" tt-1099-m.vend-name FORMAT "X(30)"
              "<R53><C5.5>" tt-1099-m.vend-add1 FORMAT "X(30)"
              "<R54><C5.5>" tt-1099-m.vend-add2 FORMAT "X(30)" 
              "<R56><C5.5>" tt-1099-m.vend-city-line FORMAT "X(30)".

          IF tt-1099-m.vend-box EQ "3" OR tt-1099-m.vend-box EQ "Y" THEN
              PUT "<R44><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" .
          ELSE IF tt-1099-m.vend-box EQ "1" THEN
              PUT "<R38.6><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" . 
          ELSE IF tt-1099-m.vend-box EQ "2" THEN
              PUT "<R41.5><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" . 
          ELSE IF tt-1099-m.vend-box EQ "7" THEN
              PUT "<R51><C38>" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" . 
          PAGE.       
   END.  
 
END.
