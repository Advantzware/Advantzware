
/*------------------------------------------------------------------------
    File        : Colors.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Item Color Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttcolor NO-UNDO
BEFORE-TABLE beforeColor
        FIELD press-type AS char
        FIELD rm-i-no AS char
        FIELD Dscr AS char
        FIELD pass AS integer
        FIELD in-out AS CHAR
        FIELD cover AS integer
        FIELD Occurs AS INTEGER FORMAT ">>":U INITIAL 0 
        /*FIELD unit2 AS Integer*/
        FIELD i-ps2 like eb.i-ps2
        FIELD i-code2 like eb.i-code2
        FIELD i-dscr2 Like eb.i-dscr2
        FIELD iper2 Like eb.i-%2
        FIELD unit like eb.i-col
        FIELD i-ps like eb.i-ps
        FIELD i-code like eb.i-code
        FIELD i-dscr like eb.i-dscr
        FIELD i-per like eb.i-%   
        
   . 
DEFINE DATASET dsColor FOR ttcolor .

DEFINE QUERY q-ColorQuery FOR ttcolor.

DEFINE DATA-SOURCE src-Color  FOR QUERY q-ColorQuery.

BUFFER ttcolor :ATTACH-DATA-SOURCE(DATA-SOURCE src-Color  :HANDLE).
