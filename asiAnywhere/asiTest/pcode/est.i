
/*------------------------------------------------------------------------
    File        : est.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Estimate

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttest NO-UNDO LIKE est.

DEFINE DATASET dsest FOR ttest.

DEFINE QUERY q-estQuery FOR est.

DEFINE DATA-SOURCE src-est FOR QUERY q-estQuery.

BUFFER ttest:ATTACH-DATA-SOURCE(DATA-SOURCE src-est:HANDLE).

