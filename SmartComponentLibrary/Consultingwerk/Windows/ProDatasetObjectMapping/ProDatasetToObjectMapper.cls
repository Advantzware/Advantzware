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
    File        : ProDatasetToObjectMapper
    Purpose     : O/R Mapper between ProDataset records and .NET value objects
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Feb 09 10:42:47 CET 2015
    Notes       : Extension to the TempTableToObjectMapper
                  Due to dependency on the System.Type:GenericTypeArguments
                  property this class depends on .NET Framework version 4.5
                  or above. See https://msdn.microsoft.com/en-us//library/system.type.generictypearguments(v=vs.110).aspx
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                                 FROM PROPATH .
USING Consultingwerk.Assertion.*                       FROM PROPATH .
USING Consultingwerk.Util.*                            FROM PROPATH .
USING Consultingwerk.Windows.ProDatasetObjectMapping.* FROM PROPATH .
USING Consultingwerk.Windows.TempTableObjectMapping.*  FROM PROPATH .
USING Progress.Lang.*                                  FROM PROPATH .
USING System.Reflection.*                              FROM ASSEMBLY.

CLASS Consultingwerk.Windows.ProDatasetObjectMapping.ProDatasetToObjectMapper:

    /*------------------------------------------------------------------------------
        Purpose: Copies the field values from the buffer to the value object
        Notes:   Recursively processes related records and creates object instances

        @param phBuffer The handle of the source buffer
        @param poObject The reference to the target object
        @param poMapping The field mapping specification
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID CopyFromBuffer (phBuffer AS HANDLE,
                                       poObject AS System.Object,
                                       poMapping AS TableMapping):

        DEFINE VARIABLE oTempTableToObjectMapper AS TempTableToObjectMapper  NO-UNDO .
        DEFINE VARIABLE hChildBuffer             AS HANDLE                   NO-UNDO .
        DEFINE VARIABLE hChildQuery              AS HANDLE                   NO-UNDO .
        DEFINE VARIABLE oProperty                AS PropertyInfo             NO-UNDO .
        DEFINE VARIABLE oPropertyType            AS System.Type              NO-UNDO .
        DEFINE VARIABLE oItemType                AS System.Type              NO-UNDO .
        DEFINE VARIABLE oChildObject             AS System.Object            NO-UNDO .
        DEFINE VARIABLE oList                    AS System.Collections.IList NO-UNDO .

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phBuffer WidgetTypeEnum:Buffer} .
        {Consultingwerk/Assertion/BufferAssert/IsAvailable.i phBuffer} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poMapping """poMapping"":U"} .

        oTempTableToObjectMapper = NEW TempTableToObjectMapper () .
        oTempTableToObjectMapper:CopyFromBuffer (phBuffer,
                                                 poObject,
                                                 poMapping:FieldMapping) .

        {Consultingwerk/foreachABL.i ChildTableMapping oChildTableMapping in poMapping:ChildTableMappings}

            ASSIGN oProperty     = poObject:GetType():GetProperty (oChildTableMapping:PropertyName) .

            IF NOT VALID-OBJECT (oProperty) THEN
                UNDO, THROW NEW InvalidPropertyNameException (SUBSTITUTE ("The property name &1 for relation &2 is not available in the type &3 (.NET property names are case-sensitive)."{&TRAN},
                                                                          oChildTableMapping:PropertyName,
                                                                          oChildTableMapping:ChildRelationName,
                                                                          poObject:GetType():FullName),

                                                              oChildTableMapping:PropertyName,
                                                              poObject:GetType():FullName,
                                                              oChildTableMapping:ChildRelationName) .


            ASSIGN oPropertyType = oProperty:PropertyType
                   hChildBuffer  = phBuffer:DATASET:GET-RELATION (oChildTableMapping:ChildRelationName):CHILD-BUFFER .

            IF oPropertyType:IsGenericType THEN
                oItemType = CAST (oPropertyType:GenericTypeArguments:GetValue (0), System.Type) .
            ELSE
                oItemType = oPropertyType .

            hChildQuery = QueryHelper:CreatePreparedQuery (hChildBuffer, "FOR ":U + DatasetHelper:ChildRelationQueryStringWithValues (hChildBuffer)) .

            DO WHILE NOT hChildQuery:QUERY-OFF-END:

                oChildObject = oItemType:GetConstructor(System.Type:EmptyTypes):Invoke (?) .

                IF Progress.Util.TypeHelper:GetType("System.Collections.IList":U):IsAssignableFrom (oPropertyType) THEN DO:

                    oList = CAST (oProperty:GetValue (poObject), System.Collections.IList) .
                    oList:Add (oChildObject) .

                END.
                ELSE
                    oProperty:SetValue (poObject, oChildObject) .

                THIS-OBJECT:CopyFromBuffer (hChildBuffer, oChildObject, oChildTableMapping) .

                hChildQuery:GET-NEXT () .
            END.

            FINALLY:
                GarbageCollectorHelper:DeleteObject (hChildQuery) .
            END FINALLY.

        END.

   END METHOD .

END CLASS.
