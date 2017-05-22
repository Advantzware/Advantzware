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
    File        : AblReflectionHelper
    Purpose     : Helper methods for use with ABL reflection (OpenEdge 11.6)
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Jan 16 11:52:33 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH.
USING Consultingwerk.*           FROM PROPATH .
USING Consultingwerk.Util.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Util.AblReflectionHelper:

    /**
     * Purpose: Protected constructor for the AblReflectionHelper class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PROTECTED AblReflectionHelper ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (AblReflection) NE 0 &THEN

    /**
     * Purpose: Returns the (first) invokable method of the class with the given name
     * Notes:   Invokable methods of a Business Entity have an INPUT-OUTPUT Parameter
     *          DATASET/DATASET-HANDLE and an INPUT parameter for an object
     * @param poClass The reference to the class
     * @param pcMethodName The name of the method(s) to return
     * @return The Progress.Reflect.Method with the given name or ?
     */
    METHOD PUBLIC STATIC Progress.Reflect.Method GetInvokableMethod (poClass AS Progress.Lang.Class,
                                                                     pcMethodName AS CHARACTER):

        RETURN AblReflectionHelper:GetInvokableMethod (poClass, pcMethodName, 1) .

    END METHOD .

    /**
     * Purpose: Returns the (first) invokable method of the class with the given name
     * Notes:   Invokable methods of a Business Entity have an INPUT-OUTPUT Parameter
     *          DATASET/DATASET-HANDLE and an INPUT parameter for an object
     * @param poClass The reference to the class
     * @param pcMethodName The name of the method(s) to return
     * @param piNumDatasets The number of dataset parameters (Business Tasks, 1 for Business Entities)
     * @return The Progress.Reflect.Method with the given name or ?
     */
    METHOD PUBLIC STATIC Progress.Reflect.Method GetInvokableMethod (poClass AS Progress.Lang.Class,
                                                                     pcMethodName AS CHARACTER,
                                                                     piNumDatasets AS INTEGER):

        DEFINE VARIABLE oList       AS ListMethod                 NO-UNDO .
        DEFINE VARIABLE oParameters AS Progress.Reflect.Parameter NO-UNDO EXTENT .
        DEFINE VARIABLE oMethod     AS Progress.Reflect.Method    NO-UNDO .
        DEFINE VARIABLE i           AS INTEGER                    NO-UNDO .
        DEFINE VARIABLE j           AS INTEGER                    NO-UNDO .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass """poClass"":U"}
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcMethodName "'pcMethodName':U"}

        oList = AblReflectionHelper:GetMethodsByName (poClass, pcMethodName, Progress.Reflect.Flags:Instance OR Progress.Reflect.Flags:Public) .

        methodLoop:
        DO i = 1 TO oList:Count:
            oMethod = oList:GetItem (i) .

            IF oMethod:NumParameters = piNumDatasets + 1 THEN DO:
                oParameters = oMethod:GetParameters() .

                /* Last Parameter is expected to be INPUT object */
                IF NOT (oParameters[piNumDatasets + 1]:Mode     = Progress.Reflect.ParameterMode:Input AND
                        oParameters[piNumDatasets + 1]:DataType = Progress.Reflect.DataType:Object) THEN
                    NEXT methodLoop .

                DO j = 1 TO piNumDatasets:
                    IF NOT (oParameters[j]:Mode     = Progress.Reflect.ParameterMode:InputOutput AND
                            oParameters[j]:DataType = Progress.Reflect.DataType:DataSet OR oParameters[j]:DataType = Progress.Reflect.DataType:DataSetHandle) THEN
                        NEXT methodLoop .
                END.

                RETURN oMethod .
            END.
        END.

        RETURN ? .

    END METHOD .

    /**
     * Purpose: Returns all methods of the given class with the given name
     * Notes:
     * @param poClass The reference to the class
     * @param pcMethodName The name of the method(s) to return
     * @return The ListMethod with the list of method
     */
    METHOD PUBLIC STATIC ListMethod GetMethodsByName (poClass AS Progress.Lang.Class,
                                                      pcMethodName AS CHARACTER):

        RETURN GetMethodsByName (poClass, pcMethodName, ?) .

    END METHOD .

    /**
     * Purpose: Returns all methods of the given class with the given name
     * Notes:
     * @param poClass The reference to the class
     * @param pcMethodName The name of the method(s) to return
     * @param poFlags The method flags to filter methods
     * @return The ListMethod with the list of method
     */
    METHOD PUBLIC STATIC ListMethod GetMethodsByName (poClass AS Progress.Lang.Class,
                                                      pcMethodName AS CHARACTER,
                                                      poFlags AS Progress.Reflect.Flags):

        DEFINE VARIABLE oList    AS ListMethod              NO-UNDO .
        DEFINE VARIABLE oMethods AS Progress.Reflect.Method NO-UNDO EXTENT .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass "'poClass':U"}
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcMethodName "'pcMethodName':U"}

        oList = NEW ListMethod () .

        IF VALID-OBJECT (poFlags) THEN
            oMethods = poClass:GetMethods (poFlags) .
        ELSE
            oMethods = poClass:GetMethods () .

        DO i = 1 TO EXTENT (oMethods):

            IF oMethods[i]:Name = pcMethodName THEN
                oList:Add (oMethods[i]) .

        END.

        RETURN oList .

    END METHOD .

    /**
     * Purpose: Returns the list of Property Names of the given object
     *          except the given property names
     * Notes:
     * @param poObject The object to return the property names from
     * @param pcExceptPropertyNames The property names to exclude from the result
     * @return The list of Property Names
     */
    METHOD PUBLIC STATIC CHARACTER PropertyNamesExcept (poObject AS Progress.Lang.Object,
                                                        pcExceptPropertyNames AS CHARACTER):

        DEFINE VARIABLE oClass      AS Progress.Lang.Class       NO-UNDO .
        DEFINE VARIABLE oProperties AS Progress.Reflect.Property NO-UNDO EXTENT .
        DEFINE VARIABLE i           AS INTEGER                   NO-UNDO .

        DEFINE VARIABLE cList       AS CHARACTER                 NO-UNDO .

        oClass = poObject:GetClass() .

        oProperties = oClass:GetProperties() .

        propertyLoop:
        DO i = 1 TO EXTENT (oProperties):

            IF INDEX (pcExceptPropertyNames, oProperties[i]:Name) > 0 THEN
                NEXT propertyLoop .

            ASSIGN cList = cList + oProperties[i]:Name + ",":U .

        END.

        RETURN TRIM (cList, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns a comma delimited list of all method names with the given
     *          number of parameters
     * Notes:   Returns only PUBLIC methods
     * @param poClass The class to return the method names from
     * @param piNumParameters The number of parameters to return
     * @return The comma delimited list of method names with the given number of parameters
     */
    METHOD PUBLIC STATIC CHARACTER GetMethodNamesByParameterCount (poClass AS Progress.Lang.Class,
                                                                   piNumParameters AS INTEGER):

        DEFINE VARIABLE cMethodNames AS CHARACTER               NO-UNDO.

        DEFINE VARIABLE oMethods     AS Progress.Reflect.Method NO-UNDO EXTENT .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass "'poClass':U"}
        Assert:GE (piNumParameters, 0) .

        oMethods = poClass:GetMethods () .

        DO i = 1 TO EXTENT (oMethods):

            IF oMethods[i]:NumParameters = piNumParameters THEN
                cMethodNames = ListHelper:AddEntry (cMethodNames, oMethods[i]:Name) .

        END.

        RETURN cMethodNames .

    END METHOD .

    /**
     * Purpose: Returns a comma delimited list of all method names with the given
     *          number of parameters and flags
     * Notes:   Used by the DataAccess class to retrieve the names of vlaidation methods
     * @param poClass The class to return the method names from
     * @param piNumParameters The number of parameters to return
     * @param poFlags The method flags to filter methods
     * @return The comma delimited list of method names with the given number of parameters
     */
    METHOD PUBLIC STATIC CHARACTER GetMethodNamesByParameterCount (poClass AS Progress.Lang.Class,
                                                                   piNumParameters AS INTEGER,
                                                                   poFlags AS Progress.Reflect.Flags):

        DEFINE VARIABLE cMethodNames AS CHARACTER               NO-UNDO.

        DEFINE VARIABLE oMethods     AS Progress.Reflect.Method NO-UNDO EXTENT .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poClass "'poClass':U"}
        Assert:GE (piNumParameters, 0) .

        oMethods = poClass:GetMethods (poFlags) .

        DO i = 1 TO EXTENT (oMethods):

            IF oMethods[i]:NumParameters = piNumParameters THEN
                cMethodNames = ListHelper:AddEntry (cMethodNames, oMethods[i]:Name) .

        END.

        RETURN cMethodNames .

    END METHOD .

&ENDIF

END CLASS.
