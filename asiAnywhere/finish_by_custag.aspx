<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="finish_by_custag" Codebehind="finish_by_custag.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Sales Value By Customer By Tag</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    
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
    function samevalue() {
        var beginc = document.getElementById("BegCustTextBox");
        var endc = document.getElementById("EndCustTextBox");
        endc.value = beginc.value;
    }
    

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].BegCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();

    }
    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }
    
    
    function fglook() {
        var NewWindow = window.open("fgitem_lookup.aspx", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function FGLookup(ReturnObj1) {
        document.forms[0].BeItemTextBox.value = ReturnObj1;
        document.forms[0].BeItemTextBox.focus();

    }
    function fg2look() {
        var NewWindow = window.open("fgitem_translookup.aspx", "FGLookupWindow", "width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function FG2Lookup(ReturnObj1) {
        document.forms[0].EndItemTextBox.value = ReturnObj1;
        document.forms[0].EndItemTextBox.focus();
    }
    function LocationLook() {
        var NewWindow = window.open("location_lookup.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function LocationLookUp(ReturnObj1) {
        document.forms[0].BeWareTextBox.value = ReturnObj1;
        document.forms[0].BeWareTextBox.focus();
    }
    function Location2Look() {
        var NewWindow = window.open("location_lookup2.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function Location2LookUp(ReturnObj1) {
        document.forms[0].EndWareTextBox.value = ReturnObj1;
        document.forms[0].EndWareTextBox.focus();
    }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='BeWareTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Finished Goods Sales Value By Customer By Tag&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>                  
                  <asp:Label ID="CustLabel"  runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
       
      <table class="shade" width="520px">
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Warehouse:</b></td>
        <td nowrap><asp:TextBox ID="BeWareTextBox" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Warehouse:</b></td>
        <td nowrap><asp:TextBox ID="EndWareTextBox" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr>   
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="BegCustTextBox"  onkeyup="samevalue()" MaxLength="8" width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
            <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
            <td><asp:TextBox ID="EndCustTextBox" MaxLength="8" width="105px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
      </tr>      
          
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Item#:</b></td>
        <td nowrap><asp:TextBox ID="BeItemTextBox" MaxLength="15" Width="105px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
        <td nowrap><asp:TextBox ID="EndItemTextBox" MaxLength="15" Width="105px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="fg2look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr> 
        
        <tr>
            
            <td align="right" style="padding-left:10px">
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                
            </td>    
            <td colspan="3">
                   <b><asp:CheckBox ID="CheckBox1" Text="Include Cutomer Owned Warehouse?" runat="server"></asp:CheckBox></b>
                                 
            </td>            
        </tr> 
        <tr>
        <td style="background:#00FFFF" colspan="4"><br />
        <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Please Note: &nbsp;&nbsp; Cost show is Average of Last. <br /> 
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Inventory Valuation Method is set in the <br />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Finished Goods Control File.</b>
        
        </td>
        </tr>                   
          <tr><td colspan="4" align="left" style="padding-left:10px">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Output to?  
                    <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem   Value="No"   Text="Text File" />
                        <asp:ListItem  Value="Yes"  Text="Excel" />                 
                    </asp:RadioButtonList>
                </b>
         </td></tr>                    
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                  
             
              <ItemTemplate>
                  ytdcst:
                  <asp:Label ID="ytdcstLabel" runat="server" 
                      Text='<%# Bind("ytdcst") %>'></asp:Label><br />                 
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectYtdcstRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmBeWare" Type="String" />
                  <asp:Parameter Name="prmEndWare" Type="String" />
                  <asp:Parameter Name="prmBeCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmCustOwned" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


