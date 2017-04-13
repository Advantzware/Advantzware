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
    File        : PropertiesSmartBusinessEntityAdapter
    Purpose     : Provides the Property Descriptor for the SmartBusinessEntityAdapter class
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 15:33:55 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.TypeDescriptor.*            FROM PROPATH .
USING Consultingwerk.SmartComponents.TypeDescriptor.Components.* FROM PROPATH .
USING Progress.Lang.*                                            FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.SmartComponents.TypeDescriptor.Components.PropertiesSmartBusinessEntityAdapter
    INHERITS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptorCollection:

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertiesSmartBusinessEntityAdapter class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertiesSmartBusinessEntityAdapter ():

        DEFINE VARIABLE oProperty         AS IPropertyDescriptor NO-UNDO .
        DEFINE VARIABLE oInvalidReference AS System.Object       NO-UNDO .

        SUPER ().

        THIS-OBJECT:TypeName = "Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter":U .

        oProperty = THIS-OBJECT:Add ("AppServerPartition":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("AutoCommit":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (TRUE))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Controls the AutoCommit functionality of the SmartBusinessEntityAdapter.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BatchSize":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (0))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the BatchSize used when reading data from the Business Entity. Batching is used for the EntityTable, not the view tables! Set to 0 to fetch all data and disable Batching":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BindingSourceExcludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("BindingSourceIncludeFields":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDatasetChildAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("ContextDataset":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.BrowsableAttribute (FALSE)).
        oProperty:Attributes:Add (System.ComponentModel.DesignerSerializationVisibilityAttribute:Hidden).

        oProperty = THIS-OBJECT:Add ("ContextID":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the ContextID to recognize the instance of the Business Entity on the Backend.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("ContextString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the ContextString used for the communication with the Business Entity.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("CustomContext":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Gets/Sets the CustomContext used for the communication with the Business Entity.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("DatasetControllerType":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Get's/set's Columns that will not get updated by UpdateRow (TRUE).":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("LinkCommitSource":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartDataAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("A reference to the SmartCommitSource of this SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (oInvalidReference)) .


        oProperty = THIS-OBJECT:Add ("QueryString":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX ("":U))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("The typename of the DatasetController that is is owning the Dataset on which this SmartBusinessEntityAdapter is operating.":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("FetchLastOnServer":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (FALSE))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Controls if the FetchLast method will fetch all remaining records (behaviour until July 2011) or directly fetch the last batch using the current batch size and the ability to perform backwards batching for retrieving the records before the last batch":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("PrefetchOnFindRowWhere":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (10))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Specifies the number of records the a FindRowWhere (FindRowWhereModeEnum FindOnServer) should read from the backend":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("RetrieveDataOnChangeSortOrder":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (TRUE))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("This property allows to subspend the call to RetrieveData in the setter of QuerySort (SetQuerySort method)":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

        oProperty = THIS-OBJECT:Add ("StopAfter":U) .
        oProperty:Attributes:Add (NEW System.ComponentModel.CategoryAttribute ("SmartBusinessEntityAdapter":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DefaultValueAttribute (BOX (0))) .
        oProperty:Attributes:Add (NEW System.ComponentModel.DescriptionAttribute ("Allows limiting the execution time of queries executed on the backend":U)) .
        oProperty:Attributes:Add (NEW System.ComponentModel.LocalizableAttribute (FALSE)) .

    END CONSTRUCTOR.

END CLASS.
