/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : LibraryProcedureParser
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 06 14:08:02 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING com.joanju.proparse.NodeTypes                                 FROM ASSEMBLY .
USING Consultingwerk.Assertion.*                                    FROM PROPATH.
USING Consultingwerk.Studio.Proparse.NodeTypes.*                    FROM PROPATH.
USING Consultingwerk.Studio.Proparse.*                              FROM PROPATH.
USING Consultingwerk.Studio.LegacyCodeMigration.LibraryProcedures.* FROM PROPATH .
USING Consultingwerk.Util.*                                         FROM PROPATH.
USING org.prorefactor.refactor.*                                    FROM ASSEMBLY .
USING org.prorefactor.treeparser.*                                  FROM ASSEMBLY .
USING org.prorefactor.core.*                                        FROM ASSEMBLY .
USING Progress.Lang.*                                               FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Studio.LegacyCodeMigration.LibraryProcedures.LibraryProcedureParser:

    {Consultingwerk/Studio/LegacyCodeMigration/LibraryProcedures/ttEntries.i}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LibraryProcedureParser class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LibraryProcedureParser():

        ProparseHelper:SetProparseEnvironment() .
        ProparseHelper:ExportDatabaseSchema () .
        ProparseHelper:Initialize() .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generates the Wrapper class
        Notes:
        @param pcClassPackage The package for the class file
        @param pcClassName The name of the class type
        @param pcAuthorName The Author name
        @param pcInterfacePackage The package of the interface file
        @param pcInterfaceName The name of the Interface type
        @param pcTemplateFileName The name of the template file (e.g. Consultingwerk/Studio/LegacyCodeMigration/LibraryProcedures/Templates/Class.template)
        @param ttEntries The temp-table with the entries to be generated into the Interface
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateClass (pcClassPackage AS CHARACTER,
                                      pcClassName AS CHARACTER,
                                      pcInterfacePackage AS CHARACTER,
                                      pcInterfaceName AS CHARACTER,
                                      pcAuthorName AS CHARACTER,
                                      pcTemplateFileName AS CHARACTER,
                                      TABLE ttEntries):

        DEFINE VARIABLE lcMethods       AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE lcCode          AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cParameters     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cReturn         AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cReturnName     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i               AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cEntryName      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cImplementation AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cDefine         AS CHARACTER NO-UNDO .

        FileAssert:Exists (pcTemplateFileName) .

        FOR EACH ttEntries ON ERROR UNDO, THROW:

            ASSIGN cParameters     = ttEntries.EntryParameters
                   cEntryName      = ttEntries.EntryName
                   cImplementation = "":U.

            /* Functions and procedures may start with a number, methods can't */
            IF ASC (SUBSTRING (cEntryName, 1, 1)) >= 48 AND ASC (SUBSTRING (cEntryName, 1, 1)) <= 57 THEN
                ASSIGN cEntryName = "a":U + cEntryName .

            DO i = 1 TO NUM-ENTRIES (cParameters):

                IF LOOKUP (ENTRY (2, ENTRY (i, cParameters), " ":U), "TABLE,TABLE-HANDLE,DATASET,DATASET-HANDLE":U) = 0 THEN

                    ASSIGN ENTRY (i, cParameters) = ENTRY (1, ENTRY (i, cParameters), " ":U) + " ":U +
                                                    ENTRY (3, ENTRY (i, cParameters), " ":U) + " AS ":U +
                                                    ENTRY (2, ENTRY (i, cParameters), " ":U)

                           cImplementation = cImplementation +
                                             ENTRY (1, ENTRY (i, cParameters), " ":U) + " ":U +
                                             ENTRY (2, ENTRY (i, cParameters), " ":U) + ", ":U  .
                ELSE
                    ASSIGN cImplementation = cImplementation +
                                             ENTRY (1, ENTRY (i, cParameters), " ":U) + " ":U +
                                             ENTRY (2, ENTRY (i, cParameters), " ":U) + " ":U +
                                             ENTRY (3, ENTRY (i, cParameters), " ":U) + ", ":U  .
            END.

            ASSIGN cImplementation = TRIM (TRIM (cImplementation), ",":U) .

            IF ttEntries.EntryType = "FUNCTION":U THEN

                ASSIGN cImplementation = SUBSTITUTE ("RETURN DYNAMIC-FUNCTION (~"&1~" IN THIS-OBJECT:LibraryHandle&2&3) .":U,
                                                     ttEntries.EntryName,
                                                     IF cImplementation > "":U THEN ", ":U ELSE "":U,
                                                     cImplementation)

                       lcMethods       = lcMethods + SUBSTITUTE ("    METHOD PUBLIC &1 &2 (&3):~n~n":U +
                                                                 "         &4~n~n    END METHOD .~n~n":U,
                                                                 ttEntries.EntryReturn,
                                                                 cEntryName,
                                                                 REPLACE (cParameters, ",":U, ", ":U),
                                                                 cImplementation) .
            ELSE DO:

                IF ttEntries.EntryReturn > "":U THEN DO:
                    ASSIGN cReturn     = ttEntries.EntryReturn
                           cReturnName = "":U.

                    outputLoop: DO i = 1 TO NUM-ENTRIES (ttEntries.EntryParameters):
                        IF ENTRY (i, ttEntries.EntryParameters) BEGINS "OUTPUT ":U THEN DO:
                            ASSIGN cReturnName = ENTRY (3, ENTRY (i, ttEntries.EntryParameters), " ":U)
                                   cParameters = ListHelper:RemoveEntry(cParameters, i) .
                            LEAVE outputLoop .
                        END.
                    END.
                END.
                ELSE
                    ASSIGN cReturn     = "VOID":U
                           cReturnName = "":U.

                IF cReturnName > "":U THEN
                    ASSIGN cDefine = SUBSTITUTE ("        DEFINE VARIABLE &1 AS &2 NO-UNDO .~n~n":U,
                                                cReturnName, cReturn) .
                ELSE
                    ASSIGN cDefine = "":U .

                ASSIGN cImplementation = SUBSTITUTE ("RUN &1 IN THIS-OBJECT:LibraryHandle &3&2&4 .":U,
                                                     ttEntries.EntryName,
                                                     cImplementation,
                                                     IF cImplementation > "":U THEN "(":U ELSE "":U,
                                                     IF cImplementation > "":U THEN ")":U ELSE "":U) .

                IF cReturnName > "":U THEN
                    ASSIGN cImplementation = cImplementation + SUBSTITUTE ("~n~n        RETURN &1 . ", cReturnName) .

                ASSIGN lcMethods       = lcMethods + SUBSTITUTE ("    METHOD PUBLIC &1 &2 (&3):~n~n":U +
                                                                 "&5         &4~n~n    END METHOD .~n~n":U,
                                                                 cReturn,
                                                                 cEntryName,
                                                                 REPLACE (cParameters, ",":U, ", ":U),
                                                                 cImplementation,
                                                                 cDefine) .
            END.
        END.

        COPY-LOB FROM FILE FileHelper:FindFile (pcTemplateFileName) TO lcCode .

        ASSIGN lcCode = SUBSTITUTE (lcCode,
                                    pcClassPackage,
                                    pcClassName,
                                    pcAuthorName,
                                    STRING (NOW),
                                    lcMethods,
                                    pcInterfacePackage,
                                    pcInterfaceName).

        COPY-LOB FROM lcCode TO FILE ClassHelper:ClassNameToFile (SUBSTITUTE ("&1.&2":U,
                                                                 pcClassPackage,
                                                                 pcClassName)) .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW LibraryProcedureParserException (err,
                                                             SUBSTITUTE ("Error generating Class file &1.&2":U, pcClassPackage, pcClassName),
                                                             0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Interface for the Wrapper class
        Notes:
        @param pcInterfacePackage The package for the interface file
        @param pcInterfaceName The name of the Interface type
        @param pcAuthorName The Author name
        @param pcTemplateFileName The name of the template file (e.g. Consultingwerk/Studio/LegacyCodeMigration/LibraryProcedures/Templates/Interface.template)
        @param ttEntries The temp-table with the entries to be generated into the Interface
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GenerateInterface (pcInterfacePackage AS CHARACTER,
                                          pcInterfaceName AS CHARACTER,
                                          pcAuthorName AS CHARACTER,
                                          pcTemplateFileName AS CHARACTER,
                                          TABLE ttEntries):

        DEFINE VARIABLE lcMethods   AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE lcCode      AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE cParameters AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cEntryName  AS CHARACTER NO-UNDO .

        FileAssert:Exists (pcTemplateFileName) .

        FOR EACH ttEntries ON ERROR UNDO, THROW:

            ASSIGN cParameters = ttEntries.EntryParameters
                   cEntryName  = ttEntries.EntryName .

            IF ASC (SUBSTRING (cEntryName, 1, 1)) >= 48 AND ASC (SUBSTRING (cEntryName, 1, 1)) <= 57 THEN
                ASSIGN cEntryName = "a":U + cEntryName .

            DO i = 1 TO NUM-ENTRIES (cParameters):

                IF LOOKUP (ENTRY (2, ENTRY (i, cParameters), " ":U), "TABLE,TABLE-HANDLE,DATASET,DATASET-HANDLE":U) = 0 THEN

                    ASSIGN ENTRY (i, cParameters) = ENTRY (1, ENTRY (i, cParameters), " ":U) + " ":U +
                                                    ENTRY (3, ENTRY (i, cParameters), " ":U) + " AS ":U +
                                                    ENTRY (2, ENTRY (i, cParameters), " ":U) .
            END.

            IF ttEntries.EntryType = "FUNCTION":U THEN

                ASSIGN lcMethods = lcMethods + SUBSTITUTE ("    METHOD PUBLIC &1 &2 (&3) .~n~n":U,
                                                           ttEntries.EntryReturn,
                                                           cEntryName,
                                                           REPLACE (cParameters, ",":U, ", ":U)) .
            ELSE DO:
                IF ttEntries.EntryReturn > "":U THEN DO:
                    ASSIGN cReturn     = ttEntries.EntryReturn.

                    outputLoop: DO i = 1 TO NUM-ENTRIES (ttEntries.EntryParameters):
                        IF ENTRY (i, ttEntries.EntryParameters) BEGINS "OUTPUT ":U THEN DO:
                            ASSIGN cParameters = ListHelper:RemoveEntry(cParameters, i) .
                            LEAVE outputLoop .
                        END.
                    END.
                END.
                ELSE
                    ASSIGN cReturn = "VOID":U .

               lcMethods = lcMethods + SUBSTITUTE ("    METHOD PUBLIC &1 &2 (&3) .~n~n":U,
                                                   cReturn,
                                                   cEntryName,
                                                   REPLACE (cParameters, ",":U, ", ":U)) .
            END.
        END.

        COPY-LOB FROM FILE FileHelper:FindFile (pcTemplateFileName) TO lcCode .

        ASSIGN lcCode = SUBSTITUTE (lcCode,
                                    pcInterfacePackage,
                                    pcInterfaceName,
                                    pcAuthorName,
                                    STRING (NOW),
                                    lcMethods) .

        COPY-LOB FROM lcCode TO FILE ClassHelper:ClassNameToFile (SUBSTITUTE ("&1.&2":U,
                                                                 pcInterfacePackage,
                                                                 pcInterfaceName)) .

        CATCH err AS Progress.Lang.Error:
            UNDO, THROW NEW LibraryProcedureParserException (err,
                                                             SUBSTITUTE ("Error generating Interface file &1.&2":U, pcInterfacePackage, pcInterfaceName),
                                                             0) .
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Annotations of the Class Member
        Notes:
        @param pcProcedureName The name of the procedure to parse
        @param ttEntries The temp-table with the resulting entries
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ParseLibraryProcedure (pcProcedureName AS CHARACTER,
                                              OUTPUT TABLE ttEntries):

        DEFINE VARIABLE oParseUnit  AS org.prorefactor.treeparser.ParseUnit NO-UNDO .
        DEFINE VARIABLE oWrapper    AS JPNodesWrapper                       NO-UNDO .
        DEFINE VARIABLE oReturns    AS JPNode                               NO-UNDO .
        DEFINE VARIABLE cDataType   AS CHARACTER                            NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE iEntry      AS INTEGER                              NO-UNDO .
        DEFINE VARIABLE iOutput     AS INTEGER                              NO-UNDO .

        EMPTY TEMP-TABLE ttEntries .

        DO ON ERROR UNDO, THROW:
            oParseUnit = ProparseHelper:ParseFile (pcProcedureName) .

            CATCH err AS Progress.Lang.Error:
                UNDO, THROW NEW LibraryProcedureParserException (err, pcProcedureName) .
            END CATCH.
        END .

        IF NodeTypes:getTypeName(oParseUnit:getTopNode():getType()) <> "Program_root":U THEN
            UNDO, THROW NEW LibraryProcedureParserException("Top Node must be Program_root."{&TRAN}, 0, pcProcedureName) .

        oWrapper = NEW JPNodesWrapper (oParseUnit:getTopNode()) .
        oWrapper:NodeTypeFilter = "PROCEDURE":U .

        {Consultingwerk/foreachABL.i JPNode oProcedureName IN oWrapper}

            /* Skip private procedures */
            IF ProparseHelper:HasChildNodeOfNodeType(oProcedureName, ProparseNodeTypes:N_PRIVATE) THEN
                NEXT .

            /* Skip external procedures */
            IF ProparseHelper:HasChildNodeOfNodeType(oProcedureName, ProparseNodeTypes:N_EXTERNAL) THEN
                NEXT .

            CREATE ttEntries .
            ASSIGN ttEntries.EntryType       = "PROCEDURE":U
                   ttEntries.EntryName       = ProparseHelper:FindChildNodeOfNodeType (oProcedureName, "ID":U):getText()
                   ttEntries.EntryParameters = ProparseHelper:ParseProcedureParameterList (oProcedureName, TRUE)
                   ttEntries.EntryReturn     = ?
                   iOutput                   = 0
                   iEntry                    = ? .

            outputParameterLoop: DO i = 1 TO NUM-ENTRIES (ttEntries.EntryParameters):
                IF ENTRY (i, ttEntries.EntryParameters) BEGINS "OUTPUT ":U THEN DO:

                    IF iOutput > 0 THEN DO:
                        ASSIGN iEntry = ? .
                        LEAVE outputParameterLoop .
                    END.
                    ELSE
                        ASSIGN iEntry  = i
                               iOutput = iOutput + 1 .
                END.
            END.

            IF iOutput = 1 THEN
                ASSIGN ttEntries.EntryReturn = ENTRY (2, ENTRY (iEntry, ttEntries.EntryParameters), " ":U) .
        END.

        oWrapper:NodeTypeFilter = "FUNCTION":U .

        {Consultingwerk/foreachABL.i JPNode oFunctionName IN oWrapper}

            /* Skip private functions */
            IF ProparseHelper:HasChildNodeOfNodeType(oFunctionName, ProparseNodeTypes:N_PRIVATE) THEN
                NEXT .

            /* Skip forward declaration */
            IF ProparseHelper:HasChildNodeOfNodeType(oFunctionName, ProparseNodeTypes:N_FORWARDS) THEN
                NEXT .

            IF NOT ProparseHelper:HasChildNodeOfNodeType(oFunctionName, "Code_block":U) THEN
                NEXT .

            CREATE ttEntries .
            ASSIGN ttEntries.EntryType       = "FUNCTION":U
                   ttEntries.EntryName       = ProparseHelper:FindChildNodeOfNodeType (oFunctionName, "ID":U):getText()
                   ttEntries.EntryParameters = ProparseHelper:ParseParameterList (oFunctionName, TRUE)
                   ttEntries.EntryReturn     = ? .

            ASSIGN oReturns = ProparseHelper:FindChildNodeOfNodeType(oFunctionName, "RETURNS":U) .

            /* Returns may be abbreviated with return */
            IF NOT VALID-OBJECT (oReturns) THEN
                ASSIGN oReturns = ProparseHelper:FindChildNodeOfNodeType(oFunctionName, "RETURN":U) .

            IF VALID-OBJECT (oReturns) THEN DO:
                ASSIGN cDataType = DataTypeHelper:UnabbreviateDataTypeName (oReturns:nextSibling():getText ()) .

                IF cDataType > "":U THEN
                    ASSIGN ttEntries.EntryReturn = cDataType .
                ELSE
                    ASSIGN ttEntries.EntryReturn = oReturns:nextSibling():getText () .
            END.
        END.

    END METHOD.

END CLASS.
