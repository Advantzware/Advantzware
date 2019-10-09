/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsObjectDef.i
    Purpose     : Business Entity for ObjectDef

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.09.2014 11:58:32
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsObjectDef

{ Consultingwerk/RollbaseAdapter/MetaData/DataObjectDef.i }
{ Consultingwerk/RollbaseAdapter/MetaData/DataFieldDefs.i }
{ Consultingwerk/RollbaseAdapter/MetaData/DataFieldDef.i }
{ Consultingwerk/RollbaseAdapter/MetaData/ListItems.i }
{ Consultingwerk/RollbaseAdapter/MetaData/ListItem.i }
{ Consultingwerk/RollbaseAdapter/MetaData/RelationshipDefs.i }
{ Consultingwerk/RollbaseAdapter/MetaData/RelationshipDef.i }


DEFINE {&ACCESS} DATASET dsObjectDef {&REFERENCE-ONLY} FOR DataObjectDef, DataFieldDefs, DataFieldDef, ListItems, ListItem, RelationshipDefs, RelationshipDef 
    DATA-RELATION DataFieldDefListItemsRelation FOR DataFieldDef, ListItems 
        RELATION-FIELDS (origid,parentid)
    DATA-RELATION DataFieldDefsDataFieldDefRelatio FOR DataFieldDefs, DataFieldDef 
        RELATION-FIELDS (parentid,objectid)
    DATA-RELATION DataObjectDefDataFieldDefsRelati FOR DataObjectDef, DataFieldDefs 
        RELATION-FIELDS (id,parentid)
    DATA-RELATION DataObjectDefRelationshipDefsRel FOR DataObjectDef, RelationshipDefs 
        RELATION-FIELDS (id,parentid)
    DATA-RELATION ListItemsListItemRelation FOR ListItems, ListItem 
        RELATION-FIELDS (parentid,fieldid)
    DATA-RELATION RelationshipDefsRelationshipDefR FOR RelationshipDefs, RelationshipDef 
        RELATION-FIELDS (parentid,objectid)

    .    
