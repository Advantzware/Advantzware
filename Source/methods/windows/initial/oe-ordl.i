
/*------------------------------------------------------------------------
    File        : methods/windows/initial/oe-ordl.i
    Purpose     : 

    Syntax      :

    Description : Custom local initialize include for oe-ordl


    Author(s)   : Rahul Rawat
    Created     : Fri Feb 12 04:20:35 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

&IF DEFINED(LocalInit) NE 0 &THEN
    IF ipcScreen NE "OU1" THEN DO:
        RUN DisableAddButton IN h_options3.
        RUN pChangeWindowTitle(
            INPUT ipcScreen
            ). 
    END.           
&ENDIF
