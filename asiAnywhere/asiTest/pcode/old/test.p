

DEFINE TEMP-TABLE ttShipLook NO-UNDO 
    FIELD cust-no AS CHARACTER
    FIELD ship-id AS CHARACTER
    FIELD SName AS CHARACTER
    FIELD Addr AS CHARACTER
    FIELD City AS CHARACTER
    FIELD State AS CHARACTER
    FIELD Zip AS CHARACTER
    FIELD carrier AS CHARACTER
    FIELD Loc AS CHARACTER .
   
. 
DEFINE DATASET dsShipLook FOR ttShipLook .

run ShipLook.p(INPUT "search", INPUT "", INPUT "" ,INPUT "", INPUT "IBM-1000"INPUT "",INPUT " ", INPUT "", input-output dataset dsShipLook).

for each ttShipLook no-lock:
display ttShipLook.cust-no ttShipLook.ship-id ttShipLook.SName ttShipLook.City ttShipLook.State             
 .
end.
