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
    File        : dsDesignAttributeValue.i
    Purpose     : Dataset with Attribute Value Temp-Table for Design

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 20 20:33:31 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF "{&DATASET-NAME}" <> "dsAttributeValue" &THEN
{ Consultingwerk/SmartFramework/Repository/Class/eSmartAttributeValue.i }
&ENDIF

DEFINE {&ACCESS} TEMP-TABLE eDesignAttributeValue{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} 
    LIKE eSmartAttributeValue 
    &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eDesignAttributeValueBefore{&SUFFIX} &ENDIF

    FIELD IsInherited AS LOGICAL INIT TRUE 
    FIELD InheritedFrom AS CHARACTER 

    FIELD TechnicalName AS CHARACTER FORMAT "x(40)":U LABEL "Technical Name":T
    FIELD AttributeDesription AS CHARACTER FORMAT "x(400)":U LABEL "Description":T
    FIELD LookupType AS CHARACTER FORMAT "x(12)":U LABEL "Lookup Type":T
    FIELD LookupValues AS CHARACTER FORMAT "x(400)":U LABEL "Lookup Values":T
    FIELD PropertyType AS CHARACTER FORMAT "x(80)":U LABEL "Property Type":T
    FIELD RepositoryType AS CHARACTER FORMAT "x(12)":U LABEL "Repository Type":T
    FIELD PropertyOrEvent AS LOGICAL FORMAT "Property/Event":U INIT "yes":U LABEL "PropertyOrEvent":T
    . 
    
DEFINE DATASET dsDesignAttributeValue FOR eDesignAttributeValue .    
    
    