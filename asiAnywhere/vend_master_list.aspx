<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="vend_master_list" Codebehind="vend_master_list.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Vendor Master List</title>
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
    
</script>
    
    <script language = JavaScript>    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }

    
    


function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "JobLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].TextBox7.value = ReturnObj1;
}
function jobReplook() {
    var NewWindow = window.open("jobRep_lookup.aspx", "JobLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function JobRepLookup(ReturnObj1) {
    document.forms[0].TextBox8.value = ReturnObj1;
}


function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){ 
  document.forms[0].TextBox9.value = ReturnObj1;
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){ 
  document.forms[0].TextBox10.value = ReturnObj1;
}
var vend;
function vendorlook(obj) {
    vend = obj;
    var NewWindow = window.open("corvend_lookup.aspx", "UomLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendLookup(ReturnObj1) {
    if (vend == "1") {
        document.forms[0].begvendTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endvendTextBox.value = ReturnObj1;
    }

}

var buy;
function buyerlookup(obj2) {
    buy = obj2;
    var NewWindow = window.open("buyerlook.aspx", "buyerLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function buyerlook(ReturnObj1) {
    if (buy == "1") {
        document.forms[0].begbuyTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endbuyTextBox.value = ReturnObj1;
    }

}
var vtyp;
function vendortypelook(obj4) {
    vtyp = obj4;
    var NewWindow = window.open("vendtype_lookup.aspx", "vendtypeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function VendTypeLookup(ReturnObj1) {
    if (vtyp == "1") {
        document.forms[0].begvendtyTextBox.value = ReturnObj1;
    }
    else {
        document.forms[0].endvendtyTextBox.value = ReturnObj1;
    }
}

 </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='begvendTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
         <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8" runat="server" />
         <asp:HiddenField ID="HiddenField9" runat="server" />
         <asp:HiddenField ID="HiddenField10" runat="server" />
         <asp:HiddenField ID="HiddenField11" runat="server" />
         <asp:HiddenField ID="HiddenField12" runat="server" />         
         <asp:HiddenField ID="HiddenField13" runat="server" />         
         <asp:HiddenField ID="HiddenField14" runat="server" />         
         <asp:HiddenField ID="HiddenField15" runat="server" />         
         <asp:HiddenField ID="HiddenField16" runat="server" /> 
         <asp:HiddenField ID="HiddenField17" runat="server" /> 
         <asp:HiddenField ID="HiddenFieldPost" runat="server" /> 
                 
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
           <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>
                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
         
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Vendor Master List &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      <table class="shade" style="width: 530px">                    
         <tr><td valign="top">                                     
                
      <table class="shade" width="530px">      
      
      <tr><td align="right" style="padding-right: 5px"><b>Begining Vendor#:</b></td><td>
          <asp:TextBox ID="begvendTextBox"   width="100px" MaxLength="8" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="vendorlook(1); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td>
      <td><asp:TextBox ID="endvendTextBox"  width="100px" MaxLength="8" runat="server"></asp:TextBox> 
      <a href="#" tabindex="1" onclick="vendorlook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>       
      </td>
        </tr>   
        
        <tr><td align="right" style="padding-right: 5px"><b>Begining Vendor Type:</b></td><td>
          <asp:TextBox ID="begvendtyTextBox"  width="100px" MaxLength="8" runat="server"></asp:TextBox>  
          <a href="#" tabindex="1" onClick="vendortypelook(1); return false"><asp:Image ID="vendtype" runat="server" ImageUrl="images/lookup_icon.gif" /></a>        
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Vendor Type:</b></td>
      <td><asp:TextBox ID="endvendtyTextBox"  width="100px" MaxLength="8" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="vendortypelook(2); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>        
      </td>
        </tr>    
        
        <tr><td align="right" style="padding-right: 5px"><b>Begining Buyer:</b></td><td>
          <asp:TextBox ID="begbuyTextBox"  width="100px" MaxLength="3" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="buyerlookup(1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Buyer:</b></td>
      <td><asp:TextBox ID="endbuyTextBox"  width="100px" MaxLength="3" runat="server"></asp:TextBox>   
      <a href="#" tabindex="1" onClick="buyerlookup(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>     
      </td>
        </tr>   
        </table>
        
              
                  
        
        
          
        
        <table class="shade" style="width: 530px">                    
         <tr><td valign="top" align="center">                  
        <table>        
         <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Detailed?" runat="server"></asp:CheckBox></b></td></tr>                         
         </table>     
         </td></tr></table>
         
         </td>
          </tr> 
         
         
          
          <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td  colspan="3">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
             </td>
          </tr>
           
         </table>     
          
         
          <asp:FormView ID="FormView1" Visible="False"  runat="server" DataSourceID="ObjectDataSource1">
             
              
                           
              <ItemTemplate>
                  venmstr:
                  <asp:Label ID="venmstrLabel" runat="server" Text='<%# Bind("venmstr") %>'></asp:Label><br />
                  extra:
                  <asp:Label ID="extraLabel" runat="server" Text='<%# Bind("extra") %>' />
                  <br />
              </ItemTemplate>
              
              
          </asp:FormView>   
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="VendorMasterList" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmvenmstr" Type="String" />
                  <asp:Parameter Name="prmbegvend" Type="String" />
                  <asp:Parameter Name="prmbegtyp" Type="String" />
                  <asp:Parameter Name="prmbegbuy" Type="String" />
                  <asp:Parameter Name="prmendvend" Type="String" />
                  <asp:Parameter Name="prmendtyp" Type="String" />
                  <asp:Parameter Name="prmendbuy" Type="String" />
                  <asp:Parameter Name="prmdetail" Type="String" />                               
                  <asp:Parameter Name="prmOut" Type="String" />
                                                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

