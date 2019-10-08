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
    File        : eSmartAttribute.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 20.10.2015 20:22:29
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.Repository.Class.AttributeValueBusinessEntity", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSmartAttribute{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartAttributeBefore{&SUFFIX} &ENDIF
    FIELD AttributeGuid AS CHARACTER FORMAT "x(36)":U LABEL "AttributeGuid":T
    FIELD AttributeGroupGuid AS CHARACTER FORMAT "x(36)":U LABEL "AttributeGroupGuid":T
    FIELD AttributeLabel AS CHARACTER FORMAT "x(40)":U LABEL "Attribute Label":T
    FIELD TechnicalName AS CHARACTER FORMAT "x(40)":U LABEL "Technical Name":T
    FIELD AttributeDesription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD RuntimeOnly AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Runtime Only":T
    FIELD VirtualProperty AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Virtual":T
    FIELD ConstantLevel AS CHARACTER FORMAT "x(10)":U LABEL "Constant Level":T
    FIELD LookupType AS CHARACTER FORMAT "x(12)":U LABEL "Lookup Type":T
    FIELD LookupValues AS CHARACTER FORMAT "x(400)":U LABEL "Lookup Values":T
    FIELD PropertyType AS CHARACTER FORMAT "x(80)":U LABEL "Property Type":T
    FIELD RepositoryType AS CHARACTER FORMAT "x(12)":U LABEL "Repository Type":T
    FIELD SetServiceType AS CHARACTER FORMAT "x(80)":U LABEL "SET Service Type Name":T
    FIELD PropertyOrEvent AS LOGICAL FORMAT "Property/Event":U INIT "yes":U LABEL "PropertyOrEvent":T
    FIELD AttributeGroupName AS CHARACTER FORMAT "x(80)":U LABEL "Attribute Group":T

    INDEX AttributeGroup AttributeGroupGuid ASCENDING AttributeLabel ASCENDING
    INDEX AttributeGuid AS UNIQUE PRIMARY AttributeGuid ASCENDING
    INDEX AttributeLabel AS UNIQUE AttributeLabel ASCENDING

    .

    