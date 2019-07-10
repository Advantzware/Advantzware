<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="rep_custag" Codebehind="rep_by_custag.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Sales Value By Customer By Tag</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
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
            theForm.SubmitButton.click();              
        }
    }
    function samevalue()
    {
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    endc.value=beginc.value;
    }        
    
    function contactcustomerlook(){ 
        var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
        document.forms[0].begincust_TextBox.value = ReturnObj1;
        document.forms[0].endcust_TextBox.value = ReturnObj1;
        document.forms[0].endcust_TextBox.focus();
    }

    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].endcust_TextBox.value = ReturnObj1;
        document.forms[0].endcust_TextBox.focus();
    }

function customerpolook(){ 
  var NewWindow = window.open("customerpo_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1){ 
  document.forms[0].custpo_TextBox.value = ReturnObj1;
  document.forms[0].custpo_TextBox.focus();
}

function customerpo2look() {
    var NewWindow = window.open("customerpo2_lookup.aspx", "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPO2Lookup(ReturnObj1){
    document.forms[0].endcustpo_TextBox.value = ReturnObj1;
    document.forms[0].endcustpo_TextBox.focus();
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].besmanTextBox.value = ReturnObj1;
  document.forms[0].besmanTextBox.focus();
  }


function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].endsmanTextBox.value = ReturnObj1;
  document.forms[0].endsmanTextBox.focus();
 }
    
    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server" defaultfocus ='begincust_TextBox' >   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
                      
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Finished Goods Sales Value By Customer By Tag&nbsp;</b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center"><b>Users</b>&nbsp;&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          </TD>
          
                   
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
          <asp:HiddenField ID="HiddenField5" runat="server" />
          <asp:HiddenField ID="HiddenField6" runat="server" />
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
          
      <table class="shade"><tr><td align="right" style="padding-right:5px"><b>Beginning Customer#:</b></td>
      <td nowrap>
          <asp:TextBox ID="begincust_TextBox" MaxLength="8" Width="100px" onkeyup="samevalue();" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Customer:</b></td>
      <td>
          <asp:TextBox ID="endcust_TextBox" MaxLength="8" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    
      </td></tr>
      
      <tr><td align="right" style="padding-right:5px"><b>Beginning Customer Po#:</b></td>
      <td>
          <asp:TextBox ID="custpo_TextBox" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="customerpolook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right:5px"><b>Ending Customer Po#:</b></td>
      <td>
          <asp:TextBox ID="endcustpo_TextBox" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="customerpo2look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
          <tr><td align="right" style="padding-right: 5px"><b>Begining Salesman:</b></td>
          <td nowrap><asp:TextBox ID="besmanTextBox" MaxLength="3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Salesman:</b></td>
          <td nowrap><asp:TextBox ID="endsmanTextBox" MaxLength="3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onclick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>          
          
         <tr><td></td><td>
              <b><asp:CheckBox ID="checkbox1"  Text="Include Zero Quantity on Hand?" runat="server" /></b></td></tr>
          <tr><td></td><td><b><asp:CheckBox ID="CheckBox2" Text="Include Customer Owned Warehouse" runat="server" /></b></td></tr>
                  
         
         <tr><td align="right" style="padding-right: 5px">
           <b>Output To:</b></td><td><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Text File" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
          <tr><td colspan="4"><asp:Button ID="SubmitButton" runat="server" CssClass="button" Text="Submit" OnClick="SubmitButton_Click"></asp:Button>
          &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
                  
                              </td></tr></table>
                              
                              
         <asp:FormView ID="FormView1" Visible="False"   runat="server" DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
                        
            <ItemTemplate>
                vFile:
                <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
                jklovgf:
                <asp:Label ID="jklovgfLabel" runat="server" Text='<%# Bind("jklovgf") %>'></asp:Label><br />
            </ItemTemplate>
             
             
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="Selectcustag" TypeName="reports">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String"  />
                <asp:Parameter Name="prmAction" Type="String"  />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmBeginCust" Type="String" />
                <asp:Parameter Name="prmEndCust" Type="String" />
                <asp:Parameter Name="prmBeginPo" Type="String" />
                <asp:Parameter Name="prmEndPo" Type="String" />
                <asp:Parameter Name="prmBeSaleman" Type="String" />
                <asp:Parameter Name="prmEndSalesman" Type="String" />
                <asp:Parameter Name="prmZeroQty" Type="String" />
                <asp:Parameter Name="prmInvWarehouse" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
       
    </form>
</body>
</html>
