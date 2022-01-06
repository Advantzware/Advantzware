 
 /*------------------------------------------------------------------------
    File        : VendorTagParseUT.p 
    Syntax      : 
    Author(s)   : a.pandianraj
    Created     : Thu Jan 06 07:38:57 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.Assert FROM PROPATH.

BLOCK-LEVEL ON ERROR UNDO, THROW.



/* **********************  Internal Procedures  *********************** */


@Setup.
PROCEDURE setUp:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONNECT D:\asigui\Databases\Test\TESTDEVELd.db -H localhost -S 2826 -ld asi NO-ERROR.
    CONNECT D:\asigui\Databases\Test\TESTDEVELa.db -H localhost -S 2836 -ld audit NO-ERROR.

END PROCEDURE.  

@TearDown.
PROCEDURE tearDown:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE. 

@Test.
PROCEDURE TestTagParse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cParseTag AS CHARACTER NO-UNDO INIT "102345001100001234".
    
    DEFINE VARIABLE iPOnumber AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iPOline   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iQuantity AS INTEGER  NO-UNDO.
    DEFINE VARIABLE cErrorStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorType AS CHARACTER NO-UNDO.
    
    RUN addon\rm\vendorTagParse.p(cParseTag,
                       OUTPUT iPOnumber,
                       OUTPUT iPOline,
                       OUTPUT iQuantity,
                       OUTPUT cErrorStr,
                       OUTPUT cErrorType).
    /*Each assert can be added as an individual test*/                   
    assert:Equals(102345,iPOnumber).
    assert:Equals(001,iPOline).
    assert:Equals(10000,iQuantity).
    assert:Equals("",cErrorStr).
    assert:Equals("I",cErrorType).

END PROCEDURE.

@Test.
PROCEDURE TestTagParseFail:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cParseTag AS CHARACTER NO-UNDO INIT "102345001100001234".
    
    DEFINE VARIABLE iPOnumber AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iPOline   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iQuantity AS INTEGER  NO-UNDO.
    DEFINE VARIABLE cErrorStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cErrorType AS CHARACTER NO-UNDO.
    
    RUN addon\rm\vendorTagParse.p(cParseTag,
        OUTPUT iPOnumber,
        OUTPUT iPOline,
        OUTPUT iQuantity,
        OUTPUT cErrorStr,
        OUTPUT cErrorType).
    
    /*Each assert can be added as an individual test*/                   
    assert:NotEqual(02345,iPOnumber).
    assert:NotEqual(002,iPOline).
    assert:NotEqual(10001,iQuantity).
    assert:NotEqual("Error",cErrorStr).
    assert:NotEqual("S",cErrorType).

END PROCEDURE.
