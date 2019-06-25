
/*------------------------------------------------------------------------
    File        : itemDetail.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Item Detail

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttitem NO-UNDO LIKE itemfg
BEFORE-TABLE beforeitem
        FIELD descriptiond AS CHARACTER
    FIELD lengthd      AS CHARACTER
    FIELD widthd       AS CHARACTER
    FIELD depthd       AS CHARACTER
    FIELD StyleDscr       AS CHARACTER
    FIELD CatDscr         as character.
    
DEFINE DATASET dsItemDetail FOR ttitem.

DEFINE QUERY q-itemfgQuery FOR itemfg.

DEFINE DATA-SOURCE src-itemfg FOR QUERY q-itemfgQuery.

BUFFER ttitem:ATTACH-DATA-SOURCE(DATA-SOURCE src-itemfg:HANDLE).

DEFINE VARIABLE v-itemfg AS HANDLE     NO-UNDO. 
 v-itemfg = DATASET dsItemDetail:HANDLE.   
v-itemfg:GET-BUFFER-HANDLE("ttitem"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "get-quoted", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
 

PROCEDURE get-quoted:
    DEFINE INPUT PARAMETER DATASET FOR dsItemDetail.
    DEFINE VAR quotehdrowid AS ROWID.

    IF AVAILABLE ttitem THEN 
    DO:
        ASSIGN
            ttitem.descriptiond = ttitem.part-dscr1 + " " + ttitem.part-dscr2 + ", " + ttitem.part-dscr3
            ttitem.lengthd      = IF ttitem.l-score[50] > 0 THEN string(ttitem.l-score[50]) ELSE ""     
            ttitem.widthd       = IF ttitem.w-score[50] > 0 THEN string(ttitem.w-score[50]) ELSE ""     
            ttitem.depthd       = IF ttitem.d-score[50] > 0 THEN string(ttitem.d-score[50]) ELSE ""     
            . 
            FIND FIRST style WHERE style.style = ttitem.style NO-LOCK NO-ERROR.
            IF AVAILABLE style THEN DO:
                        assign ttitem.StyleDscr = ttitem.style + "-" + style.dscr.
            END.
            else do:
                        assign ttitem.StyleDscr = ttitem.style .
            end.
            FIND FIRST fgcat WHERE fgcat.procat = ttitem.procat NO-LOCK NO-ERROR.
            IF AVAILABLE fgcat  THEN DO:
                        assign ttitem.CatDscr      = ttitem.CatDscr + " - " + fgcat.dscr.
            END.
            ELSE DO:
                        assign ttitem.CatDscr      = ttitem.CatDscr .
            END.            
            
  


            
  

            
        
            
    END.
END PROCEDURE. 
