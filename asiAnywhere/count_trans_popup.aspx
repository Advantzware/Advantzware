<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="count_trans_popup" Codebehind="count_trans_popup.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>FG Physical Count Transfer</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmPost() {
        var retVal = makeMsgBox("Confirmation", "Are you ready to transfer all physical count?", 48, 4, 256, 4096);
        if (retVal == 6) {            
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
            return;
        }
    }
</script>
    
    <script language="javascript">

        

var whlook = "";

function locationlook(var2) {
    whlook = var2;
    var NewWindow = window.open("location_lookup.aspx", "LocationLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2) {
    if (whlook == "1") {
        document.forms[0].TextBox1.value = ReturnObj1;
        document.forms[0].TextBox1.focus();
    }
    else {
        document.forms[0].TextBox3.value = ReturnObj1;
        document.forms[0].TextBox3.focus();
    }

}

var binlook2 = "";


function binlook(var1) {
    binlook2 = var1;
    var loc1 = "";
     if (binlook2 == "1")
          loc1 = document.forms[0].TextBox1.value;
     else loc1 = document.forms[0].TextBox3.value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc=" + loc1 + "", "BinLookUpWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1, ReturnObj2) {
    if (binlook2 == "1") {
        document.forms[0].TextBox2.value = ReturnObj1;
        document.forms[0].TextBox2.focus();
    }
    else {
        document.forms[0].TextBox4.value = ReturnObj1;
        document.forms[0].TextBox4.focus();
    }
}


 </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
    <asp:HiddenField ID="HiddenFieldPost" runat="server" />
      <div>
                          
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
              
      <table class="shade" style="width: 400px">                    
         <tr><td valign="top">          
        <fieldset>        
      <table class="shade" width="400px">       
       
      <tr><td align="right" style="padding-right: 5px"><b>From Warehouse:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()"  width="100px" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="locationlook(1); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Bin:</b></td>
      <td><asp:TextBox ID="TextBox2"  width="100px" MaxLength="8" runat="server"></asp:TextBox>        
      <a href="#" tabindex="1" onClick="binlook(1); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        <tr><td align="right" style="padding-right:5px"><b>To Warehouse:</b></td>
          <td>
              <asp:TextBox ID="TextBox3" Width="100px" MaxLength="8" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="locationlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a> </td>
                            
          <td align="right" style="padding-right:5px"><b>Bin:</b></td>
          <td>
              <asp:TextBox ID="TextBox4" Width="100px" MaxLength="8" runat="server"></asp:TextBox>
              <a href="#" tabindex="1" onClick="binlook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>              
              </td></tr> 
        </table>
          </fieldset></td>
          </tr>
          
          <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" Width="50px" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Ok" />
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:Button ID="Button1"  OnClientClick="window.close()" runat="server" class="buttonM" Text="Cancel" />
             &nbsp;&nbsp;&nbsp;&nbsp;         
             </td>
          </tr>   
          
          </table>
          
          <div style="display:none">
          <asp:FormView ID="FormView1"  runat="server"         DataSourceID="ObjectDataSource1">
                
              <ItemTemplate>
                  vRno:
                  <asp:Label ID="vRnoLabel" runat="server" Text='<%# Bind("vRno") %>' />
                  <br />
                  vcntDate:
                  <asp:Label ID="vcntDateLabel" runat="server" Text='<%# Bind("vcntDate") %>' />
                  <br />
                  vcntTime:
                  <asp:Label ID="vcntTimeLabel" runat="server" Text='<%# Bind("vcntTime") %>' />
                  <br />
                  vTag:
                  <asp:Label ID="vTagLabel" runat="server" Text='<%# Bind("vTag") %>' />
                  <br />
                  vPono:
                  <asp:Label ID="vPonoLabel" runat="server" Text='<%# Bind("vPono") %>' />
                  <br />
                  vJobno:
                  <asp:Label ID="vJobnoLabel" runat="server" Text='<%# Bind("vJobno") %>' />
                  <br />
                  vJobno2:
                  <asp:Label ID="vJobno2Label" runat="server" Text='<%# Bind("vJobno2") %>' />
                  <br />
                  vItem:
                  <asp:Label ID="vItemLabel" runat="server" Text='<%# Bind("vItem") %>' />
                  <br />
                  vItemName:
                  <asp:Label ID="vItemNameLabel" runat="server" Text='<%# Bind("vItemName") %>' />
                  <br />
                  vLoc:
                  <asp:Label ID="vLocLabel" runat="server" Text='<%# Bind("vLoc") %>' />
                  <br />
                  vLocBin:
                  <asp:Label ID="vLocBinLabel" runat="server" Text='<%# Bind("vLocBin") %>' />
                  <br />
                  vCases:
                  <asp:Label ID="vCasesLabel" runat="server" Text='<%# Bind("vCases") %>' />
                  <br />
                  vQtyCas:
                  <asp:Label ID="vQtyCasLabel" runat="server" Text='<%# Bind("vQtyCas") %>' />
                  <br />
                  vCasUnit:
                  <asp:Label ID="vCasUnitLabel" runat="server" Text='<%# Bind("vCasUnit") %>' />
                  <br />
                  vPartial:
                  <asp:Label ID="vPartialLabel" runat="server" Text='<%# Bind("vPartial") %>' />
                  <br />
                  vTQty:
                  <asp:Label ID="vTQtyLabel" runat="server" Text='<%# Bind("vTQty") %>' />
                  <br />
                  vCreatedBy:
                  <asp:Label ID="vCreatedByLabel" runat="server" 
                      Text='<%# Bind("vCreatedBy") %>' />
                  <br />
                  vCreate2:
                  <asp:Label ID="vCreate2Label" runat="server" Text='<%# Bind("vCreate2") %>' />
                  <br />
                  vRecKey:
                  <asp:Label ID="vRecKeyLabel" runat="server" Text='<%# Bind("vRecKey") %>' />
                  <br />
              </ItemTemplate>
              
              
          </asp:FormView> </div>  
           <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="selectcountrcpt" TypeName="itemhistory">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmFgItem" Type="String" />
                <asp:Parameter Name="prmName" Type="String" />
                <asp:Parameter Name="prmJobno" Type="String" />
                <asp:Parameter Name="prmJobno2" Type="Int32" />
                <asp:Parameter Name="prmPono" Type="String" />
                <asp:SessionParameter Name="prmSeqno" SessionField="count_recept_list_seq" Type="String" />
                <asp:Parameter Name="prmRcptDate" Type="String" />
                <asp:Parameter Name="prmTagno" Type="String" />
                <asp:Parameter Name="prmLoc" Type="String" />
                <asp:Parameter Name="prmLocBin" Type="String" />
                <asp:Parameter Name="prmTqty" Type="Decimal" />
                <asp:Parameter Name="prmCases" Type="Int32" />
                <asp:Parameter Name="prmQty_Cas" Type="Int32" />
                <asp:Parameter Name="prmCasUnit" Type="Int32" />
                <asp:Parameter Name="prmPartial" Type="Int32" />
                <asp:Parameter Name="prmRecKey" type="String" />                   
                <asp:Parameter Name="prmllSetParts" Type="String" />
                <asp:Parameter Name="prmTransTime" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
          
     
    </div>
    
    </form>
  </body>
</HTML>

