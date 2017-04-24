/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : ProductDataAccess
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner
    Created     : 06.04.2014 11:15:34
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*          FROM PROPATH .
USING Consultingwerk.SmartFramework.Menu.* FROM PROPATH .
USING Consultingwerk.SmartFramework.*      FROM PROPATH .
USING Consultingwerk.OERA.*                FROM PROPATH .
USING Consultingwerk.Util.*                FROM PROPATH .
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Menu.ProductDataAccess INHERITS SmartDataAccess:

    { Consultingwerk/SmartFramework/Menu/dsProduct.i &ACCESS="PRIVATE" &REFERENCE-ONLY="REFERENCE-ONLY"}

    @DefineDataSourcesStart.
    DEFINE PRIVATE DATA-SOURCE src_SmartProduct FOR SmartProduct .
    @DefineDataSourcesEnd.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProductDataAccess class
        Notes:   Used by the BusinessEntity, will receive the handle of the dataset
                 instance form the BusinessEntity as a parameter. Permanently BIND's
                 to that dataset using the PRIVATE BindDataset Method.
        @param phDataset The handle of the Business Entity dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProductDataAccess (phDataset AS HANDLE):
        SUPER (INPUT phDataset).

        BindDataset (DATASET-HANDLE phDataset BIND) .

        /* Fields that are not assigned to the database record when a new record is created. */
        /* Mike Fechner, Consultingwerk Ltd. 24.06.2014
           Do not honor AssignSkipList when importing records during SmartDB Migration */
        IF Consultingwerk.SmartFramework.Tools.Import.GenericDataImporter:Importing = FALSE THEN
             THIS-OBJECT:AssignSkipList = "ProductGuid":U.

        THIS-OBJECT:TrackDeletions = TRUE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Attaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID AttachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsProduct:HANDLE, FALSE) .

        @AttachDataSourcesStart.
        BUFFER eSmartProduct:ATTACH-DATA-SOURCE (DATA-SOURCE src_SmartProduct:HANDLE, "ProductGuid,SmartProduct.ProductGuid,ProductCode,SmartProduct.ProductCode,ProductName,SmartProduct.ProductName,ProductDescription,SmartProduct.ProductDescription,ProductInstalled,SmartProduct.ProductInstalled":U) .
        @AttachDataSourcesEnd.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Do nothing, just receive the parameter and bind to the received
                 Dataset reference
        Notes:   Invoked from the class constructor
        @param dsProduct DATASET to bind to
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID BindDataset (DATASET dsProduct BIND):
        /* NOOP */
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: TO-DO: Subscribe to ProDataset Event Handlers using SET-CALLBACK
                 as needed
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData, note that the callback methods need to be
                 PUBLIC and need to have the Dataset defined as an INPUT Parameter
        Sample:  BUFFER eCustomer:SET-CALLBACK (Consultingwerk.DatasetBufferCallbackEnum:AfterFill,
                                                "eCustomerAfterRowFill":U) .
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DefineReadEvents ():

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Detaches the DATA-SOURCE objects to the ProDataset Buffers
        Notes:   Overrides ABSTRACT method in Consultingwerk.OERA.DataAccess,
                 Invoked in FetchData and SaveChanges
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID DetachDataSources ():

        Consultingwerk.Util.DatasetHelper:SetTrackingChanges (DATASET dsProduct:HANDLE, FALSE) .

        @DetachDataSourcesStart.
        BUFFER eSmartProduct:DETACH-DATA-SOURCE () .
        @DetachDataSourcesEnd.

    END METHOD.

&IF PROVERSION EQ "10.2B":U &THEN
    /*------------------------------------------------------------------------------
        Purpose: Overridable method that executes SAVE-ROW-CHANGES on the before buffer
        Notes:   Override for OpenEdge 10.2B to use NO-LOBS parameter, SCL-358
        @param phBeforeBuffer The handle of the before buffer
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID SaveRowChanges (phBeforeBuffer AS HANDLE):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.

        ASSIGN cTableName = phBeforeBuffer:AFTER-BUFFER:NAME .

        DO ON ERROR UNDO, THROW
           ON STOP UNDO, RETURN ERROR NEW StopConditionException
            ("A stop condition has been raised while saving the changes."{&TRAN}, 0):
            /* Mike Fechner, Consultingwerk Ltd. 26.04.2011
               Skip-List for SAVE-ROW-CHANGES */
            IF THIS-OBJECT:AssignSkipDictionary:ContainsKey (cTableName) THEN
                phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipDictionary:GetValue (cTableName), TRUE) .
            ELSE
                IF THIS-OBJECT:AssignSkipList > "":U THEN
                    phBeforeBuffer:SAVE-ROW-CHANGES (1, THIS-OBJECT:AssignSkipList, TRUE) .
            ELSE
                phBeforeBuffer:SAVE-ROW-CHANGES (1, "":U, TRUE) .
        END.

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Returns the database field name matching a temp-table field name
                 contained in a consumers query string (query string vs. temp-table
                 definition)
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code for alternative mapping
        @param pcTable The temp table name to return the source (database table) field name for
        @param pcColumn The temp table column name to return the source (database table) field name for
        @return The database field name mapped to the passed in temp-table field
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceColumn (pcTable AS CHARACTER, pcColumn AS CHARACTER):

        DEFINE VARIABLE cMap  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO .

        @SourceColumnCaseBlock.
        CASE pcTable:
            WHEN "eSmartProduct":U THEN
                cMap = DATA-SOURCE src_SmartProduct:GET-DATASET-BUFFER:DATA-SOURCE-COMPLETE-MAP .

        END CASE .

        ASSIGN iPos = LOOKUP(pcColumn, cMap) .

        IF iPos MODULO 2 = 1 THEN
            RETURN ENTRY(LOOKUP(pcColumn,cMap) + 1,cMap).
        ELSE
            RETURN "":U .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the base query string used to retrieve data for the temp
                 tables. This query string will be appended by the query provided by
                 the consumer (FetchDataRequest object).
        Notes:   Call-back used by Consultingwerk.OERA.Query.DSQueryString (part of
                 DataAccess:FetchData () query preparation
                 TO-DO: Provide code to return the DATA-SOURCE root query string
        @param pcTable The temp table name to return the source default (database) query string for
        @return The base query string used to retrieve data for the given temp-table
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC CHARACTER SourceDefaultQuery (pcTable AS CHARACTER):

        @SourceDefaultQueryCaseBlock.
        CASE pcTable:
            WHEN "eSmartProduct":U THEN
                RETURN "FOR EACH SmartProduct INDEXED-REPOSITION":U.

        END CASE .

    END METHOD.

END CLASS.
