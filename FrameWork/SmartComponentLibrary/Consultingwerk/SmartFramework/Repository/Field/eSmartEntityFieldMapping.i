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
    File        : eSmartEntityFieldMapping.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 12.11.2016 13:43:05
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Field.EntityFieldMappingBusinessEntity", type="TempTable") .
@openapi.openedge.entity.primarykey (fields="EntityFieldMappingGuid").

DEFINE {&ACCESS} TEMP-TABLE eSmartEntityFieldMapping{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartEntityFieldMappingBefore{&SUFFIX} &ENDIF
    FIELD EntityFieldMappingGuid AS CHARACTER FORMAT "x(36)":U LABEL "EntityFieldMappingGuid":T
    FIELD EntityName AS CHARACTER FORMAT "x(40)":U INIT "*":U LABEL "Entity Name":T
    FIELD EntityTable AS CHARACTER FORMAT "x(40)":U INIT "*":U LABEL "Table Name":T
    FIELD FieldName AS CHARACTER FORMAT "x(40)":U LABEL "Field Name":T
    FIELD Environments AS CHARACTER FORMAT "x(40)":U INIT "GUI,WEB":U LABEL "Environments":T
    FIELD FieldObjectName AS CHARACTER FORMAT "x(40)":U LABEL "Object Name":T

    INDEX EntityFieldMappingGuid AS UNIQUE PRIMARY EntityFieldMappingGuid ASCENDING
    INDEX FieldName FieldName ASCENDING EntityName ASCENDING EntityTable ASCENDING
    INDEX FieldObjectName FieldObjectName ASCENDING

    .

