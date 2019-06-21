
/*------------------------------------------------------------------------
    File        :job.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Job

    Author(s)   : Jyoti
    Created     : SEP 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttjob NO-UNDO LIKE job.

DEFINE DATASET dsjob FOR ttjob.

DEFINE QUERY q-jobQuery FOR job.

DEFINE DATA-SOURCE src-job FOR QUERY q-jobQuery.

BUFFER ttjob:ATTACH-DATA-SOURCE(DATA-SOURCE src-job:HANDLE).

