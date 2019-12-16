
/*------------------------------------------------------------------------
    File        : asiUpdateHist.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 06 14:49:22 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE ttUpdateHist
    FIELD fromVersion AS CHAR 
    FIELD toVersion AS CHAR 
    FIELD applyDate AS DATE 
    FIELD startTimeInt AS INT
    FIELD startTime AS CHAR 
    FIELD endTimeInt AS INT 
    FIELD endTime AS CHAR 
    FIELD user_id AS CHAR 
    FIELD success AS LOG INITIAL NO 
    FIELD updLog AS CHAR.     

DEFINE INPUT PARAMETER TABLE FOR ttUpdateHist.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST ttUpdateHist NO-ERROR.
IF AVAIL ttUpdateHist THEN DO:
    CREATE updateHist.
    BUFFER-COPY ttUpdateHist TO updateHist.
END.
ELSE MESSAGE 
    "Failure TO acquire TEMP-TABLE IN asiUpdateHist.p"
    VIEW-AS ALERT-BOX ERROR.
    