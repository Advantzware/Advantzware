
/*------------------------------------------------------------------------
    File        : Lookup1D.p
    Purpose     : 		

    Syntax      :

    Description : Accepts 2 arrays, 1 lookup value array and 1 return value array 
    and returns the discrete LT index, discrete GT index, discrete LT value, discrete GT value, and interpolated value.

    Author(s)   : BPV
    Created     : Wed Nov 09 22:07:31 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipdValueToLookup AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipdArrayToLookup AS DECIMAL EXTENT NO-UNDO.
DEFINE INPUT PARAMETER ipdArrayToReturn AS DECIMAL EXTENT NO-UNDO.
DEFINE OUTPUT PARAMETER opdValueToReturnDiscreteLT AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdValueToReturnDiscreteGE AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opdValueToReturnInterpolated AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER opiValueIndexLT AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiValueIndexGE AS INTEGER NO-UNDO.

DEFINE VARIABLE iExtent           AS INTEGER NO-UNDO.
DEFINE VARIABLE dLookupLowerBound AS DECIMAL NO-UNDO.
DEFINE VARIABLE dLookupUpperBound AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*Ensure array sizes are not mismatched*/
RUN pSizeArrays(ipdArrayToLookup,
    ipdArrayToReturn, 
    OUTPUT iExtent).

/*Lookup the value to get an index*/
RUN pGetIndex(ipdValueToLookup, 
    ipdArrayToLookup, 
    OUTPUT opiValueIndexGE).

IF opiValueIndexGE GT 0 THEN 
DO:
    /*Set return values and values needed for interpolation*/
    ASSIGN 
        opiValueIndexLT            = IF opiValueIndexGE GT 0 THEN opiValueIndexGE - 1 ELSE 0
        opdValueToReturnDiscreteLT = IF opiValueIndexLT GT 0 THEN ipdArrayToReturn[opiValueIndexLT] ELSE 0
        opdValueToReturnDiscreteGE = IF opiValueIndexGE LE iExtent THEN ipdArrayToReturn[opiValueIndexGE] ELSE ipdArrayToReturn[iExtent]
        dLookupLowerBound          = IF opiValueIndexLT GT 0 THEN ipdArrayToLookup[opiValueIndexLT] ELSE 0
        dLookupUpperBound          = IF opiValueIndexGE LE iExtent THEN ipdArrayToLookup[opiValueIndexGE] ELSE ipdArrayToLookup[iExtent]
        .

    /*Calculate interpolation*/    
    RUN pInterpolate(ipdValueToLookup, 
        dLookupLowerBound, 
        dLookupUpperBound, 
        opdValueToReturnDiscreteLT, 
        opdValueToReturnDiscreteGE, 
        OUTPUT opdValueToReturnInterpolated).
END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetIndex:
    /*------------------------------------------------------------------------------
     Purpose: Returns the index values for lookup array values less than and greater than the lookup value
     Notes: Requires Lookup Array to be in ascending order
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdLookup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdArray AS DECIMAL EXTENT NO-UNDO.
    DEFINE OUTPUT PARAMETER opiIndex AS INTEGER NO-UNDO.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
      
    DO iIndex = 1 TO EXTENT(ipdArray):
        IF ipdArray[iIndex] GE ipdLookup THEN 
        DO:
            opiIndex = iIndex.
            LEAVE.
        END.
  
    END.
END PROCEDURE.

PROCEDURE pInterpolate:
    /*------------------------------------------------------------------------------
     Purpose: Linear Interpolation between 2 values, based on % between 2 other values
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdLookupValue AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLookupLower AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLookupUpper AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueLower AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdValueUpper AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdInterpolatedValue AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dFactor AS DECIMAL NO-UNDO.

    /*the factor of the lookup value in relation to the lookup step range*/
    IF ipdLookupUpper NE ipdLookupLower THEN  
        dFactor = (ipdLookupValue - ipdLookupLower) / (ipdLookupUpper - ipdLookupLower).  
    
    IF dFactor = 0 THEN  
        opdInterpolatedValue = ipdValueLower. 
    ELSE
        /*apply the same factor to the return value step range to get the interpolated value*/     
        opdInterpolatedValue = ((ipdValueUpper - ipdValueLower) * dFactor ) + ipdValueLower. 

END PROCEDURE.

PROCEDURE pSizeArrays PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Returns the lesser extent of the passed arrays.
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdArray1 AS DECIMAL EXTENT NO-UNDO.
    DEFINE INPUT PARAMETER ipdArray2 AS DECIMAL EXTENT NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSize AS INTEGER NO-UNDO.

    DEFINE VARIABLE iSizeTemp AS INTEGER NO-UNDO.

    opdSize = EXTENT(ipdArray1).
    iSizeTemp = EXTENT(ipdArray2).
    IF iSizeTemp LT opdSize THEN
        opdSize = iSizeTemp.
     
END PROCEDURE.


