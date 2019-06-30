
/*------------------------------------------------------------------------
    File        : InvItemLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for ItemLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInvItemLook NO-UNDO 
          FIELD InvName AS CHARACTER 
          FIELD InvItem AS CHARACTER
          FIELD InvDscr AS CHARACTER
          FIELD Invcust-no AS CHAR
          .

DEFINE DATASET dsInvItemLook FOR ttInvItemLook .
DEFINE VARIABLE q-InvItemLookQuery AS HANDLE.
DEFINE VARIABLE src-InvItemLook AS HANDLE.

DEFINE QUERY q-InvItemLookQuery FOR ttInvItemLook .

DEFINE DATA-SOURCE src-InvItemLook  FOR QUERY q-InvItemLookQuery.


BUFFER ttInvItemLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-InvItemLook  :HANDLE).

