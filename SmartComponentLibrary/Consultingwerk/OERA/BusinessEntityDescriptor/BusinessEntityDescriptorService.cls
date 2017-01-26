/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BusinessEntityDescriptorService
    Purpose     : Provides Business Entity Descriptor information for a
                  Business Entity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 20:30:29 CEST 2015
    Notes       : This Service is used by the Business Entities to return
                  the Busines Entity Descriptor. This service should not
                  be called from consumers
                  Relies on the annotation based TypeDescriptor
                  http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                                FROM PROPATH .
USING Consultingwerk.Framework.TypeDescriptor.*                 FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.*            FROM PROPATH .
USING Consultingwerk.OERA.BusinessEntityDescriptor.Exceptions.* FROM PROPATH .
USING Consultingwerk.Util.*                                     FROM PROPATH .
USING Progress.Lang.*                                           FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.OERA.BusinessEntityDescriptor.BusinessEntityDescriptorService
    IMPLEMENTS IBusinessEntityDescriptorService:

    DEFINE TEMP-TABLE ttBusinessEntityDescriptor NO-UNDO
        FIELD BusinessEntityName AS CHARACTER
        FIELD BusinessEntityDescriptor AS Progress.Lang.Object
        INDEX BusinessEntityName IS UNIQUE PRIMARY BusinessEntityName .

    DEFINE VARIABLE dtCacheAge   AS DATETIME-TZ NO-UNDO INITIAL ? .
    DEFINE VARIABLE iMaxCacheAge AS INTEGER     NO-UNDO INITIAL 0 .
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BusinessEntityDescriptorService class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BusinessEntityDescriptorService ():
        SUPER ().

        DEFINE VARIABLE oConfigurationProvider AS IConfigurationProvider NO-UNDO .

        oConfigurationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.IConfigurationProvider} .

        IF VALID-OBJECT (oConfigurationProvider) THEN DO:

            iMaxCacheAge = DataTypeHelper:ToInteger (oConfigurationProvider:GetValue ("BusinessEntityDescriptorCacheMaxAge":U)) .

            IF iMaxCacheAge = ? THEN
                ASSIGN iMaxCacheAge = 0 .
        END.

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Clears the cache
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ClearCache ():

        EMPTY TEMP-TABLE ttBusinessEntityDescriptor .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Business Entity Descriptor of a Business Entity
        Notes:
        @param pcBusinessEntityName The name of the Business Entity to return the Business Entity Descriptor for
        @return The Business Entity Descriptor of the given Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IBusinessEntityDescriptor GetBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER):

        DEFINE VARIABLE oBusinessEntityDescriptor AS IBusinessEntityDescriptor NO-UNDO .

        /* Check cache age */
        IF iMaxCacheAge > 0 AND dtCacheAge <> ? THEN DO:

            IF INTERVAL (NOW, dtCacheAge, "seconds":U) > iMaxCacheAge THEN DO:
                THIS-OBJECT:ClearCache() .
                ASSIGN dtCacheAge = ? .
            END.
        END.

        FIND ttBusinessEntityDescriptor WHERE ttBusinessEntityDescriptor.BusinessEntityName = pcBusinessEntityName
            NO-ERROR .

        IF AVAILABLE ttBusinessEntityDescriptor THEN DO:
            IF VALID-OBJECT (ttBusinessEntityDescriptor.BusinessEntityDescriptor) THEN
                RETURN CAST (ttBusinessEntityDescriptor.BusinessEntityDescriptor, IBusinessEntityDescriptor) .
        END.

        ASSIGN oBusinessEntityDescriptor = THIS-OBJECT:ImportBusinessEntityDescriptor (pcBusinessEntityName) .

        IF NOT AVAILABLE ttBusinessEntityDescriptor THEN
            CREATE ttBusinessEntityDescriptor .

        ASSIGN ttBusinessEntityDescriptor.BusinessEntityName       = pcBusinessEntityName
               ttBusinessEntityDescriptor.BusinessEntityDescriptor = oBusinessEntityDescriptor
               dtCacheAge                                          = NOW .

        RETURN oBusinessEntityDescriptor .

        FINALLY:
            ErrorHelper:ResetErrorStatus() .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Imports the Business Entity Descriptor of a Business Entity
        Notes:
        @param pcBusinessEntityName The name of the Business Entity to import
        @return The Business Entity Descriptor of the given Business Entity
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IBusinessEntityDescriptor ImportBusinessEntityDescriptor (pcBusinessEntityName AS CHARACTER):

        DEFINE VARIABLE oBusinessEntityDescriptor AS BusinessEntityDescriptor NO-UNDO .
        DEFINE VARIABLE oAnnotations              AS ClassAnnotationContainer NO-UNDO .
        DEFINE VARIABLE oAnnotationProvider       AS IClassAnnotationProvider NO-UNDO .

        oBusinessEntityDescriptor = NEW BusinessEntityDescriptor (pcBusinessEntityName) .

        oAnnotationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.TypeDescriptor.IClassAnnotationProvider
                                                            "NEW Consultingwerk.Framework.TypeDescriptor.ClassAnnotationProvider ()"} .

        oAnnotations = oAnnotationProvider:GetClassAnnotations (Progress.Lang.Class:GetClass (pcBusinessEntityName)) .

        IF VALID-OBJECT (oAnnotations) THEN
            THIS-OBJECT:ParseAnnotations (oAnnotations, oBusinessEntityDescriptor) .

        RETURN oBusinessEntityDescriptor .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses the Business Entity Annotations into the Business Entity
                 Descriptor
        Notes:
        @param poAnnotations The contained with the class annotations
        @param poBusinessEntityDescriptor The BusinessEntityDescriptor to parse the annotations into
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseAnnotations (poAnnotations AS ClassAnnotationContainer,
                                            poBusinessEntityDescriptor AS BusinessEntityDescriptor):

        DEFINE VARIABLE oMethodDescriptor AS InvokableMethodDescriptor     NO-UNDO .
        DEFINE VARIABLE oTableDescriptor  AS BusinessEntityTableDescriptor NO-UNDO .
        DEFINE VARIABLE oFieldDescriptor  AS BusinessEntityFieldDescriptor NO-UNDO .
        DEFINE VARIABLE oViewDescriptor   AS BusinessEntityViewDescriptor  NO-UNDO .

        {Consultingwerk/foreachABL.i SerializableAnnotation oAnnotation in poAnnotations:Annotations}

            CASE oAnnotation:Name:
                WHEN "@BusinessEntityTable":U THEN DO:
                    oTableDescriptor = NEW BusinessEntityTableDescriptor () .

                    poBusinessEntityDescriptor:Tables:Add (oTableDescriptor) .

                    IF NOT oAnnotation:Parameters:ContainsName ("Name":U) THEN
                       UNDO, THROW NEW MissingAnnotationAttributeException ("Unable to import @BusinessEntityTable annotation. ~"Name~" attribute is missing"{&TRAN},
                                                                            0,
                                                                            poBusinessEntityDescriptor:BusinessEntityName,
                                                                            "Name":U) .

                    oTableDescriptor:Name = oAnnotation:Parameters:FindByName ("Name":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("ReadOnly":U) THEN
                        oTableDescriptor:ReadOnly = DataTypeHelper:ToLogical (oAnnotation:Parameters:FindByName ("ReadOnly":U):Value) .

                    IF oAnnotation:Parameters:ContainsName ("MandatoryColumns":U) THEN
                        oTableDescriptor:MandatoryColumns = oAnnotation:Parameters:FindByName ("MandatoryColumns":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("ReadOnlyColumns":U) THEN
                        oTableDescriptor:ReadOnlyColumns = oAnnotation:Parameters:FindByName ("ReadOnlyColumns":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("DefaultSort":U) THEN
                        oTableDescriptor:DefaultSort = oAnnotation:Parameters:FindByName ("DefaultSort":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("NonFilterableColumns":U) THEN
                        oTableDescriptor:NonFilterableColumns = oAnnotation:Parameters:FindByName ("NonFilterableColumns":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("NonSortableColumns":U) THEN
                        oTableDescriptor:NonSortableColumns = oAnnotation:Parameters:FindByName ("NonSortableColumns":U):Value .
                END.

                WHEN "@BusinessEntityView":U THEN DO:
                    oViewDescriptor = NEW BusinessEntityViewDescriptor () .

                    poBusinessEntityDescriptor:Views:Add (oViewDescriptor) .

                    IF NOT oAnnotation:Parameters:ContainsName ("Name":U) THEN
                       UNDO, THROW NEW MissingAnnotationAttributeException ("Unable to import @BusinessEntityView annotation. ~"Name~" attribute is missing"{&TRAN},
                                                                            0,
                                                                            poBusinessEntityDescriptor:BusinessEntityName,
                                                                            "Name":U) .

                    IF NOT oAnnotation:Parameters:ContainsName ("EntityTable":U) THEN
                       UNDO, THROW NEW MissingAnnotationAttributeException ("Unable to import @BusinessEntityView annotation. ~"EntityTable~" attribute is missing"{&TRAN},
                                                                            0,
                                                                            poBusinessEntityDescriptor:BusinessEntityName,
                                                                            "EntityTable":U) .

                    oViewDescriptor:Name = oAnnotation:Parameters:FindByName ("Name":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("isdefault":U) AND
                        DataTypeHelper:ToLogical (oAnnotation:Parameters:FindByName ("isdefault":U):Value) = TRUE THEN
                            poBusinessEntityDescriptor:DefaultViewName = oViewDescriptor:Name .

                    IF oAnnotation:Parameters:ContainsName ("EntityTable":U) THEN
                        oViewDescriptor:EntityTable = oAnnotation:Parameters:FindByName ("EntityTable":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("EntityView":U) THEN
                        oViewDescriptor:EntityView = oAnnotation:Parameters:FindByName ("EntityView":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("ListColumns":U) THEN
                        oViewDescriptor:ListColumns = oAnnotation:Parameters:FindByName ("ListColumns":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("QueryString":U) THEN
                        oViewDescriptor:QueryString = oAnnotation:Parameters:FindByName ("QueryString":U):Value .

                    IF oAnnotation:Parameters:ContainsName ("ViewerColumns":U) THEN
                        oViewDescriptor:ViewerColumns = oAnnotation:Parameters:FindByName ("ViewerColumns":U):Value .
                END.

            END CASE .
        END.

        {Consultingwerk/foreachABL.i SerializableClassMemberAnnotation oMethod in poAnnotations:Methods}

            oAnnotation = oMethod:Annotations:FindByName ("@InvokeMethod":U) .

            IF VALID-OBJECT (oAnnotation) THEN DO:

                oMethodDescriptor = NEW InvokableMethodDescriptor () .

                poBusinessEntityDescriptor:InvokableMethods:Add (oMethodDescriptor) .

                oMethodDescriptor:Name = oMethod:Name .

                IF oAnnotation:Parameters:ContainsName ("ParameterClassName":U) THEN
                    oMethodDescriptor:ParameterClassName = oAnnotation:Parameters:FindByName ("ParameterClassName":U):Value .

                IF oAnnotation:Parameters:ContainsName ("DatasetInput":U) THEN
                    oMethodDescriptor:DatasetInput = DataTypeHelper:ToLogical(oAnnotation:Parameters:FindByName ("DatasetInput":U):Value) .

                IF oAnnotation:Parameters:ContainsName ("DatasetOutput":U) THEN
                    oMethodDescriptor:DatasetOutput = DataTypeHelper:ToLogical(oAnnotation:Parameters:FindByName ("DatasetOutput":U):Value) .
            END.
        END.

        {Consultingwerk/foreachABL.i SerializableClassMemberAnnotation oTempTable in poAnnotations:TempTables}

            IF oTempTable:Annotations:Count > 0 THEN DO:

                IF poBusinessEntityDescriptor:Tables:ContainsName (oTempTable:Name) THEN
                    oTableDescriptor = CAST (poBusinessEntityDescriptor:Tables:GetItem (oTempTable:Name), BusinessEntityTableDescriptor) .
                ELSE DO:
                    oTableDescriptor = NEW BusinessEntityTableDescriptor () .
                    oTableDescriptor:Name = oTempTable:Name .
                    poBusinessEntityDescriptor:Tables:Add (oTableDescriptor) .
                END.

                {Consultingwerk/foreachABL.i SerializableAnnotation oTempTableAnnotation in oTempTable:Annotations}

                    CASE oTempTableAnnotation:Name:
                        WHEN "@openapi.openedge.entity.primarykey":U THEN DO:
                            IF oTempTableAnnotation:Parameters:ContainsName ("fields":U) THEN
                                oTableDescriptor:PrimaryKey = oTempTableAnnotation:Parameters:GetValueByPair ("fields":U) .
                        END.

                        WHEN "@openapi.openedge.entity.field.property":U THEN DO:

                            IF oTempTableAnnotation:Parameters:ContainsName ("field":U) AND
                               oTempTableAnnotation:Parameters:ContainsName ("name":U) AND
                               oTempTableAnnotation:Parameters:GetValueByPair ("name":U) = "semanticType":U AND
                               oTempTableAnnotation:Parameters:ContainsName ("value":U) THEN DO:

                                ASSIGN oFieldDescriptor              = NEW BusinessEntityFieldDescriptor()
                                       oFieldDescriptor:Name         = oTempTableAnnotation:Parameters:GetValueByPair ("field":U)
                                       oFieldDescriptor:SemanticType = oTempTableAnnotation:Parameters:GetValueByPair ("value":U).

                                oTableDescriptor:Fields:Add (oFieldDescriptor) .
                            END.
                        END.

                    END CASE .
                END.
            END.
        END.

    END METHOD .

END CLASS.
