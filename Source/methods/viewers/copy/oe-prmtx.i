
/*------------------------------------------------------------------------
    File        : oe-prmtx.i
    Purpose     : 

    Syntax      :

    Description : Customer Copy include for oe-prmtx (O-F-3)

    Author(s)   : Rahul Rawat
    Created     : Mon Mar 08 08:10:04 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DO WITH FRAME {&FRAME-NAME}:
  END.  
&IF DEFINED(OePrmtxCopy) &THEN
oe-prmtx.eff-date:SCREEN-VALUE = STRING(TODAY,"99/99/9999").
&ENDIF