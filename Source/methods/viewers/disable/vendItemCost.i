
/*------------------------------------------------------------------------
    File        : vendItemCost.i
    Purpose     : 

    Syntax      :

    Description : Customer include to disable vendItemCost viewer

    Author(s)   : Rahul Rawat
    Created     : Wed Jan 27 00:55:52 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ***************************  Main Block  *************************** */
    &IF DEFINED(procDisbale) NE 0 &THEN 
        RUN pDisableButtons.
    &ENDIF.
