
/*------------------------------------------------------------------------
    File        : RfqsLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfqs

    Author(s)   : Kuldeep
    Created     : feb 13 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRfqsLook NO-UNDO 
        FIELD kRqfsNo AS INT
        FIELD kCust AS CHARACTER
        
          .
                                           
    
DEFINE DATASET dsRfqsLook FOR ttRfqsLook .
DEFINE VARIABLE q-RfqsLookQuery AS HANDLE.
DEFINE VARIABLE src-RfqsLook AS HANDLE.

DEFINE QUERY q-RfqsLookQuery FOR ttRfqsLook .

DEFINE DATA-SOURCE src-RfqsLook  FOR QUERY q-RfqsLookQuery.

BUFFER ttRfqsLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-RfqsLook  :HANDLE).






