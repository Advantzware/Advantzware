/* pCheckRelease.i - rstark - 10.24.2021 */

PROCEDURE pCheckRelease PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipLocation       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplReturnRelease AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER oe-ordl FOR oe-ordl.
    DEFINE BUFFER oe-ord  FOR oe-ord.
    DEFINE BUFFER oe-rel  FOR oe-rel.

    MAIN-LOOP:
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode          
          AND oe-ordl.opened  EQ YES 
          AND oe-ordl.i-no    EQ itemfg.i-no 
          AND (oe-ordl.stat   NE "C"
           OR  oe-ordl.stat   EQ ""), 
        FIRST oe-ord NO-LOCK                             
        WHERE oe-ord.company EQ oe-ordl.company     
          AND oe-ord.ord-no  EQ oe-ordl.ord-no     
          AND oe-ord.opened  EQ YES,             
        EACH oe-rel NO-LOCK
        WHERE oe-rel.company       EQ oe-ordl.company   
          AND oe-rel.ord-no        EQ oe-ordl.ord-no                 
          AND oe-rel.i-no          EQ oe-ordl.i-no                     
          AND oe-rel.line          EQ oe-ordl.line                     
          AND (oe-rel.spare-char-1 EQ ipLocation
           OR  ipLocation          EQ "*All" ) 
          AND LOOKUP(oe-rel.s-code,"B,S") NE 0
          AND LOOKUP(oe-rel.stat,"S,A,L,B,Z") NE 0
        :
        oplReturnRelease = YES.
        LEAVE MAIN-LOOP.
    END.    

END PROCEDURE.
