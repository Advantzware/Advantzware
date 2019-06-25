
/*------------------------------------------------------------------------
    File        : ordrel.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Catalog Maintenance

    Author(s)   : Jyoti Bajaj
    Created     : Sep 22 ,2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemfg NO-UNDO LIKE itemfg
BEFORE-TABLE beforeItemfg
    FIELD item_image AS CHARACTER.
    
DEFINE DATASET dsItemfg FOR ttItemfg.

DEFINE QUERY q-ItemfgQuery FOR itemfg.

DEFINE DATA-SOURCE src-itemfg FOR QUERY q-ItemfgQuery.

BUFFER ttItemfg:ATTACH-DATA-SOURCE(DATA-SOURCE src-itemfg:HANDLE).

DEFINE VARIABLE v-Itemfg AS HANDLE     NO-UNDO. 
v-Itemfg = DATASET dsItemfg:HANDLE.   
v-Itemfg:GET-BUFFER-HANDLE("ttItemfg"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-image", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-image:
    DEFINE INPUT PARAMETER DATASET FOR dsItemfg.
    
    ASSIGN ttItemfg.item_image = "images/cat" + STRING(ttItemfg.i-no, "99999") + ".jpg".
END PROCEDURE. 
