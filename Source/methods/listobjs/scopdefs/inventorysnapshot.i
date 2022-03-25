
/*------------------------------------------------------------------------
    File        : inventorysnapshot.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Mar 08 04:25:42 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* inventorysnapshot.i
"inventorysnapshot"
"ASI"
"inventorysnapshot"
"inventorySnapshotID"
"inventorySnapshotID"
"begin_inventorysnapshot"
"yes"
"end_inventorysnapshot"
"yes"
"itemType"
"itemType"
"begin_inventorysnapshot_itemType"
"yes"
"end_inventorysnapshot_itemType"
"yes"
?
""
""
"no"
""
"no"
?
""
""
"no"
""
"no"
?
""
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
""
"no"
"yes"
"yes"
"no"
"no"
"yes"
*/

&{&DEFINETYPE}-define TABLENAME inventorySnapshot
&{&DEFINETYPE}-define DBFIELD1 inventorysnapshot.inventorySnapshotID
&{&DEFINETYPE}-define BEGINFLD1 begin_inventorysnapshot
&{&DEFINETYPE}-define BEGIN-DEFAULT-1 yes
&{&DEFINETYPE}-define ENDFLD1 end_inventorysnapshot
&{&DEFINETYPE}-define END-DEFAULT-1 yes
&{&DEFINETYPE}-define DBFIELD2 inventorysnapshot.snapshotDesc
&{&DEFINETYPE}-define BEGINFLD2 begin_inventorysnapshot_snapshotDesc
&{&DEFINETYPE}-define BEGIN-DEFAULT-2 yes
&{&DEFINETYPE}-define ENDFLD2 end_inventorysnapshot_snapshotDesc
&{&DEFINETYPE}-define END-DEFAULT-2 yes
&{&DEFINETYPE}-define DBFIELD3 inventorysnapshot.snapshotType
&{&DEFINETYPE}-define BEGINFLD3 begin_inventorysnapshot_snapshotType
&{&DEFINETYPE}-define BEGIN-DEFAULT-3 yes
&{&DEFINETYPE}-define ENDFLD3 end_inventorysnapshot_snapshotType
&{&DEFINETYPE}-define END-DEFAULT-3 yes
&{&DEFINETYPE}-define DBFIELD4 inventorysnapshot.ItemType
&{&DEFINETYPE}-define BEGINFLD4 begin_inventorysnapshot_ItemType
&{&DEFINETYPE}-define BEGIN-DEFAULT-4 yes
&{&DEFINETYPE}-define ENDFLD4 end_inventorysnapshot_ItemType
&{&DEFINETYPE}-define END-DEFAULT-4 yes
&{&DEFINETYPE}-define DBFIELD5 inventorysnapshot.warehouseID
&{&DEFINETYPE}-define BEGINFLD5 begin_inventorysnapshot_warehouseID
&{&DEFINETYPE}-define BEGIN-DEFAULT-5 yes
&{&DEFINETYPE}-define ENDFLD5 end_inventorysnapshot_warehouseID
&{&DEFINETYPE}-define END-DEFAULT-5 yes
&{&DEFINETYPE}-define DBFIELD6 inventorysnapshot.LocationId
&{&DEFINETYPE}-define BEGINFLD6 begin_inventorysnapshot_LocationId
&{&DEFINETYPE}-define BEGIN-DEFAULT-6 yes
&{&DEFINETYPE}-define ENDFLD6 end_inventorysnapshot_LocationId
&{&DEFINETYPE}-define END-DEFAULT-6 yes
&{&DEFINETYPE}-define DBFIELD7 inventorysnapshot.inventoryStockStatus
&{&DEFINETYPE}-define BEGINFLD7 begin_inventorysnapshot_inventoryStockStatus
&{&DEFINETYPE}-define BEGIN-DEFAULT-7 yes
&{&DEFINETYPE}-define ENDFLD7 end_inventorysnapshot_inventoryStockStatus
&{&DEFINETYPE}-define END-DEFAULT-7 yes
&{&DEFINETYPE}-define DBFIELD8 inventorysnapshot.snapshotUser
&{&DEFINETYPE}-define BEGINFLD8 begin_inventorysnapshot_snapshotUser
&{&DEFINETYPE}-define BEGIN-DEFAULT-8 yes
&{&DEFINETYPE}-define ENDFLD8 end_inventorysnapshot_snapshotUser
&{&DEFINETYPE}-define END-DEFAULT-8 yes
&{&DEFINETYPE}-define DBFIELD9 inventorysnapshot.snapshotUser
&{&DEFINETYPE}-define BEGINFLD9 begin_inventorysnapshot_snapshotUser
&{&DEFINETYPE}-define BEGIN-DEFAULT-9 yes
&{&DEFINETYPE}-define ENDFLD9 end_inventorysnapshot_snapshotUser
&{&DEFINETYPE}-define END-DEFAULT-9 yes
&{&DEFINETYPE}-define LISTORDER inventorySnapshotID,snapshotDesc,snapshotType,itemType,warehouseID,LocationId,inventoryStockStatus,snapshotUser,snapshotTime
&{&DEFINETYPE}-define ADDFLD-1 
&{&DEFINETYPE}-define ADD-DEFAULT-1 no
&{&DEFINETYPE}-define ADDFLD-2 
&{&DEFINETYPE}-define ADD-DEFAULT-2 no
&{&DEFINETYPE}-define ADDFLD-3 
&{&DEFINETYPE}-define ADD-DEFAULT-3 no
&{&DEFINETYPE}-define ADDFLD-4 
&{&DEFINETYPE}-define ADD-DEFAULT-4 no
&{&DEFINETYPE}-define ADDFLD-5 
&{&DEFINETYPE}-define ADD-DEFAULT-5 no
&{&DEFINETYPE}-define ADDFLD-6 
&{&DEFINETYPE}-define ADD-DEFAULT-6 no
&{&DEFINETYPE}-define ADDFLD-7 
&{&DEFINETYPE}-define ADD-DEFAULT-7 no
&{&DEFINETYPE}-define ADDFLD-8 
&{&DEFINETYPE}-define ADD-DEFAULT-8 no
&{&DEFINETYPE}-define ADDFLD-9 
&{&DEFINETYPE}-define ADD-DEFAULT-9 no
&{&DEFINETYPE}-define DISPLAYFLDS {&ADDFLD-1} {&ADDFLD-2} {&ADDFLD-3} {&ADDFLD-4} {&ADDFLD-5}  {&ADDFLD-6} {&ADDFLD-7} {&ADDFLD-8} {&ADDFLD-9}
&{&DEFINETYPE}-define SHOWNOTES yes
&{&DEFINETYPE}-define SHOWMISCFLDS yes
&{&DEFINETYPE}-define SHOWADDRESSES no
&{&DEFINETYPE}-define SHOWPHONES no
&{&DEFINETYPE}-define SAVENAME inventorysnapshot
&{&DEFINETYPE}-define QUERYDEFAULT yes