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
    File        : ISmartRepositoryService
    Purpose     : Interface for services providing access to repository
                  data on the client and appserver
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun May 15 22:07:54 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*     FROM PROPATH .
USING Consultingwerk.SmartFramework.*            FROM PROPATH .
USING Consultingwerk.SmartFramework.Repository.* FROM PROPATH.
USING Consultingwerk.Web2.Services.SmartViews.*  FROM PROPATH.
USING Progress.Lang.*                            FROM PROPATH .

INTERFACE Consultingwerk.SmartFramework.Repository.ISmartRepositoryService:

    /**
     * Purpose: Returns if the child class is a child of the given parent class
     * Notes:
     * @param pcChildClassName The name of the child class to verify
     * @param pcParentClassName The name of the parent class to test if the given child class is a child of
     * @return Logical value indicating if the child class is child of the given parent class
     */
    METHOD PUBLIC LOGICAL ClassIsA (pcChildClassName AS CHARACTER,
                                    pcParentClassName AS CHARACTER).

    /**
     * Purpose: Clears all caches of this class
     * Notes:
     */
    METHOD PUBLIC VOID ClearCaches ().

    /**
     * Purpose: Creates MetaEntityTable, MetaDataField and MetaCalculatedField
     *          object masters for the given Business Entity Model
     * Notes:
     * @param phBusinessEntityDataset The ProDataset with the Business Entity Model to import
     * @param pcModuleName The name of the Product Module to import the entity objects into
     * @param poOverwritePromptCallback The callback reference to prompt the user if an existing object should be overwritten
     * @return The CharacterList with the GUID's of the created MetaEntityTable objects
     */
    METHOD PUBLIC CharacterList CreateDataFields (phBusinessEntityDataset AS HANDLE,
                                                  pcModuleName AS CHARACTER,
                                                  poOverwritePromptCallback AS IOverwritePromptCallback) .

    /**
     * Purpose: Creates MetaEntityTable, MetaDataField and MetaCalculatedField
     *          object masters for the given Business Entity Model
     * Notes:   When the poOverwritePromptCallback reference is not valid, we'll
     *          overwrite existing objects silently
     * @param phBusinessEntityDataset The ProDataset with the Business Entity Model to import
     * @param pcModuleName The name of the Product Module to import the entity objects into
     * @param poOverwritePromptCallback The callback reference to prompt the user if an existing object should be overwritten
     * @param pcEntityTableType The type to use for table objects
     * @param pcDataFieldType The type to use for data field objects
     * @param pcCalculatedFieldType The type to use for calculated field objects
     * @return The CharacterList with the GUID's of the created MetaEntityTable objects
     */
    METHOD PUBLIC CharacterList CreateDataFields (phBusinessEntityDataset AS HANDLE,
                                                  pcModuleName AS CHARACTER,
                                                  poOverwritePromptCallback AS IOverwritePromptCallback,
                                                  pcEntityTableType AS CHARACTER,
                                                  pcDataFieldType AS CHARACTER,
                                                  pcCalculatedFieldType AS CHARACTER) .

    /**
     * Purpose: Creates an Object Instance
     * Notes:
     * @param pcContainerObject The Container Object Master GUID or Container Object Name
     * @param pcInstanceObject The Object Master GUID or Object Name of the object to create an instance of
     * @param pcInstanceName The name of the object instance (or ? to use the object name)
     * @param pcInstanceDescription The description of the instance
     * @param piPageSequence The sequence number of the page to create the instance on, zero for container page
     * @param piObjectSequence The sequence of the object instance on the given page
     * @return The Object Instance Guid
     */
    METHOD PUBLIC CHARACTER CreateInstance (pcContainerObject AS CHARACTER,
                                            pcInstanceObject AS CHARACTER,
                                            pcInstanceName AS CHARACTER,
                                            pcInstanceDescription AS CHARACTER,
                                            piPageSequence AS INTEGER,
                                            piObjectSequence AS INTEGER) .
    /**
     * Purpose: Creates an Object Instance
     * Notes:
     * @param pcContainerObject The Container Object Master GUID or Container Object Name
     * @param pcInstanceObject The Object Master GUID or Object Name of the object to create an instance of
     * @param pcInstanceName The name of the object instance (or ? to use the object name)
     * @param pcInstanceDescription The description of the instance
     * @param pcPageGuid The GUID of the page to create the instance on
     * @param piObjectSequence The sequence of the object instance on the given page
     * @return The Object Instance Guid
     */
    METHOD PUBLIC CHARACTER CreateInstance (pcContainerObject AS CHARACTER,
                                            pcInstanceObject AS CHARACTER,
                                            pcInstanceName AS CHARACTER,
                                            pcInstanceDescription AS CHARACTER,
                                            pcPageGuid AS CHARACTER,
                                            piObjectSequence AS INTEGER).

    /**
     * Purpose: Ensures that the list of classes contains GUIDs and not class names
     * Notes:   Turns Class names into guids
     * @param pcClassNames The comma delimited list of class names or class guids
     * @return The comma delimited list of class guids
     */
    METHOD PUBLIC CHARACTER EnsureGuids (pcClassNames AS CHARACTER) .

    /**
     * Purpose: Fetches an object from the repository
     * Notes:
     * @param poParameter The FetchRepositoryObjectParameter with the parameter for this call
     * @return The handle of the dataset with the repository data
     */
    METHOD PUBLIC HANDLE FetchRepositoryObject (poParameter AS FetchRepositoryObjectParameter).

    /**
     * Purpose: Returns a buffer to the attribute of a given object master (when
     *          pcInstanceName is empty) or named object instance
     * Notes:   This instance creates a new (dynamic) buffer. The caller is
     *          responsible for deleting the buffer
     * @param pcInstanceName The name of the instance to return the attribute buffer for, or "" or ? to return the attributes of the master
     * @param phRepositoryData The handle of the repository data ProDataset
     * @return The handle of the attribute buffer
     */
    METHOD PUBLIC HANDLE GetAttributeBuffer (pcInstanceName AS CHARACTER,
                                             phRepositoryData AS HANDLE).

    /**
     * Purpose: Returns the comma delimited list of child class guids of the provided
     *          parent classes
     * Notes:
     * @param pcObjectTypeGuids The list of parent classes
     * @return The list of child class guids (including the parent classes)
     */
    METHOD PUBLIC CHARACTER GetChildClasses (pcObjectTypeGuids AS CHARACTER) .

    /**
     * Purpose: Returns the name fo the field object used to render
     *          a given Business Entity Field
     * Notes:
     * @param pcEntityName The name of the Business Entity
     * @param pcEntityTable The name of the Business Entity Temp-Table
     * @param pcFieldName The name of the Business Entity Field
     * @param pcEnvironment The type of the rendering environment (GUI or WEB)
     * @return The name of the field object used to render the business entity field
     */
    METHOD PUBLIC CHARACTER GetEntityFieldMapping (pcEntityName AS CHARACTER,
                                                   pcEntityTable AS CHARACTER,
                                                   pcFieldName AS CHARACTER,
                                                   pcEnvironment AS CHARACTER) .

    /**
     * Purpose: Renders a repository field as html
     * Notes:   Fetches the Object Master Attributes from the respository
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR GetHtmlField (pcObjectName AS CHARACTER,
                                         pcScope AS CHARACTER,
                                         pcFieldModel AS CHARACTER,
                                         pcFieldId AS CHARACTER,
                                         plMandatory AS LOGICAL,
                                         plReadOnly AS LOGICAL,
                                         phDataset AS HANDLE,
                                         pcTables AS CHARACTER,
                                         poBindingStyle AS ViewerBindingStyleEnum) .

    /**
     * Purpose: Renders a repository field as html
     * Notes:   Uses an externally provided attribute buffer (which can be an object master or an object instance)
     * @param pcObjectName The name of the repository field
     * @param pcScope The scope to bind the field to (for additional properties)
     * @param pcFieldModel The model to bind the field to
     * @param pcFieldId  The Id of the field (JQuery)
     * @param plMandatory Logical value indicating if the field is mandatory
     * @param plReadOnly Logical value indicating if the field is mandatory
     * @param phDataset The handle of the Business Entity Dataset
     * @param pcTables The list of tables (entity table + entity view)
     * @param poBindingStyle The style of data bindings (AngularJS or Angular2)
     * @param phAttributesBuffer The buffer with the attributes of the object
     * @return The longchar representation (hmtl code) of the field
     */
    METHOD PUBLIC LONGCHAR GetHtmlField (pcObjectName AS CHARACTER,
                                         pcScope AS CHARACTER,
                                         pcFieldModel AS CHARACTER,
                                         pcFieldId AS CHARACTER,
                                         plMandatory AS LOGICAL,
                                         plReadOnly AS LOGICAL,
                                         phDataset AS HANDLE,
                                         pcTables AS CHARACTER,
                                         poBindingStyle AS ViewerBindingStyleEnum,
                                         phAttributesBuffer AS HANDLE) .

    /**
     * Purpose: Returns the Product Module Guid based on an Product Module Name
     *          or an product module guid
     * Notes:
     * @param pcProductModule The Product Module Name or Product Module Guid
     * @return The Product Module Guid
     */
    METHOD PUBLIC CHARACTER GetProductModuleGuid (pcProductModule AS CHARACTER).

    /**
     * Purpose: Returns the Object Master Guid based on an Object Master Name
     *          or an object master guid
     * Notes:
     * @param pcObjectMaster The Object Master Name or Object Master Guid
     * @return The Object Master Guid
     */
    METHOD PUBLIC CHARACTER GetObjectMasterGuid (pcObjectMaster AS CHARACTER) .

    /**
     * Purpose: Returns the Object Type Guid based on an Object Type Name
     *          or an object type guid
     * Notes:
     * @param pcObjectType The Object Type Name or Object Type Guid
     * @return The Object Type Guid
     */
    METHOD PUBLIC CHARACTER GetObjectTypeGuid (pcObjectType AS CHARACTER).

    /**
     * Purpose: Returns the Object Type Guid of the given object master
     * Notes:   Supports object master guid and object master name
     * @param pcObjectMaster The obejct master guid or name
     * @return The object type guid of the given object master
     */
    METHOD PUBLIC CHARACTER GetObjectTypeGuidOfMaster (pcObjectMaster AS CHARACTER).

    /**
     * Purpose: Returns the Data Folder of the given Product Module
     * Notes:   Throws an exception, when the module does not have a data folder
     * @param pcModuleGuid The guid of the Product Module
     * @return The data folder of the given product module
     */
    METHOD PUBLIC CHARACTER GetProductModuleDataFolder (pcModuleGuid AS CHARACTER).

    /**
     * Purpose: Returns if the object is of the given class
     * Notes:
     * @param pcObjectName The name of the object to verify
     * @param pcClassName The name of the class to test if the object is a child of
     * @return Logical value indicating if the object is child of the given class
     */
    METHOD PUBLIC LOGICAL ObjectIsA (pcObjectName AS CHARACTER,
                                     pcClassName AS CHARACTER).

END INTERFACE.
