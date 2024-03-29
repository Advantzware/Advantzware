/* ---------------------------------------------- aprep/fibremsc.p   */
/* PRINT Fibre 1099-MISC                                             */
/* ------------------------------------------------------------------*/

{sys/inc/var.i shared}

{aprep\r-1099m.i}

DEF VAR v-comp-name AS CHAR NO-UNDO.
DEF VAR v-addr1 AS CHAR NO-UNDO.
DEF VAR v-city-line AS CHAR NO-UNDO.
DEF VAR v-phone AS CHAR NO-UNDO.
DEF VAR v-taxid AS CHAR NO-UNDO.
DEF VAR top-flag AS LOG NO-UNDO.
DEF VAR copy-count AS INT NO-UNDO.
DEFINE VARIABLE dAmountRow AS DECIMAL NO-UNDO.
DEFINE VARIABLE dAmountColumn AS DECIMAL NO-UNDO.

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
       top-flag = NOT top-flag.
       
       FIND FIRST formLayouts NO-LOCK
            WHERE formLayouts.formType EQ "xPrint"
              AND formLayouts.formGroup EQ "US Tax Forms" 
              AND formLayouts.formID EQ "1099-MISC"
              AND formLayouts.formLine EQ integer(tt-1099-m.vend-box) NO-ERROR.
       
       dAmountRow =  IF AVAILABLE formLayouts THEN formLayouts.formRow ELSE 11.  
       dAmountColumn =  IF AVAILABLE formLayouts THEN formLayouts.formColumn ELSE 38.
      
       IF top-flag THEN DO:
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
              
            PUT "<R" trim(STRING(dAmountRow)) "><C" + trim(STRING(dAmountColumn)) ">" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" .
            
             IF NOT LAST(tt-1099-m.vend-no) THEN DO:
                 IF last-of(tt-1099-m.vend-no) THEN DO:
                     top-flag = NO.
                     PAGE.
                 END.
             END.
       END.
       ELSE
       DO:
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
              
           PUT "<R" trim(STRING(dAmountRow + 33)) "><C" trim(STRING(dAmountColumn)) ">" tt-1099-m.vend-total FORMAT "->>,>>>,>>9.99" .  
          
          PAGE.
       END.
   END.

   IF top-flag THEN
   DO:
      PAGE.
      top-flag = NO.
   END.
END.
