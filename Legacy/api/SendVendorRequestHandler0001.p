/*------------------------------------------------------------------------
    File        : api/SendVendorRequestHandler0001.p
    Purpose     : Custom request handler for SendVendor API

    Syntax      :

    Description : Custom request handler for SendVendor API

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    
    DEFINE INPUT        PARAMETER TABLE            FOR ttArgs.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage       AS CHARACTER NO-UNDO.

    /* This is custom request data for clients which expect different request
       data from the SendVendor configuration. Replace ioplcRequestData
       assignment with custom code */
    ioplcRequestData = '~{"Request":"This is a custom request data for SendVendor API"}'.
        
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .
