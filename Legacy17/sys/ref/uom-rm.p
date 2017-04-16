
def input  parameter v-mat-type like item.mat-type.
def output parameter v-uom-list as   char.


v-uom-list = if v-mat-type ge "1" and
                v-mat-type le "4" then "BF,EA"                  else
                
             if v-mat-type eq "P" then "TON,LB,MSF,LF,EA,ROLL"  else
             
             if v-mat-type eq "J" then "MLI"                    else
             
             if INDEX("DTOXY789",v-mat-type) GT 0 then "EA"  else

             if INDEX("MC@",v-mat-type) GT 0 THEN "EA,M"       else
                
             if v-mat-type eq "R" then "REM,MSH"                else
             
             if v-mat-type eq "F" or
                v-mat-type eq "W" then "MSI,LB"                 else
            /* gdm - 03090902 */
             if v-mat-type eq "G" then "LB" 
             else
                "MSF,EA,TON,MSH,LB,LF,ROLL".
                                       
