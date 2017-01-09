/*------------------------------------------------------------------------
    File        : dsTasks.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jun 04 12:22:33 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTasks NO-UNDO 
    FIELD TaskNum AS CHARACTER 
    FIELD TaskIcon AS BLOB 
    FIELD TaskDescription AS CHARACTER 
    FIELD TaskDetails AS CHARACTER 
    INDEX TaskNum IS UNIQUE TaskNum .
   
DEFINE DATASET dsTasks FOR ttTasks .  