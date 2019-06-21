<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="custim_report" Codebehind="custim_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Finished Goods Sales Value By Customer By Receipt Date</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
        
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
    
    
    
    function salesreplook() {
        var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function SalesRepLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].TextBox3.value = ReturnObj1;
        document.forms[0].TextBox3.focus();
    }
    function smancopylook1() {
        var NewWindow = window.open("sman_copylookup.aspx", "smancopyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function smancopyLookup(ReturnObj1, ReturnObj2) {
        document.forms[0].TextBox4.value = ReturnObj1;
        document.forms[0].TextBox4.focus();
    }
    function customerpolook() {
        var NewWindow = window.open("customerpo_lookup.aspx", "EstimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CustomerPOLookup(ReturnObj1) {
        document.forms[0].BePoTextBox.value = ReturnObj1;
        document.forms[0].BePoTextBox.focus();

    }
    function customerpo2look() {
        var NewWindow = window.open("customerpo2_lookup.aspx", "CustomerPoLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustomerPO2Lookup(ReturnObj1) {
        document.forms[0].EndPoTextBox.value = ReturnObj1;
        document.forms[0].EndPoTextBox.focus();
    }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Finished Goods Sales Value By Customer By Receipt Date&nbsp;</b></font></TD>
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
       
      <table class="shade" >
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>As of:</b></td>
          <td>
            <asp:TextBox ID="TextBox1" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="100px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('TextBox1').focus()"  tabindex="1" onClick="showCalendarControl(TextBox1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>          
         </td>      
      </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="BegCustTextBox"  onkeyup="samevalue()" MaxLength="8" width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
            <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td>
            <td><asp:TextBox ID="EndCustTextBox" MaxLength="8" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
      </tr>      
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Customer Po#:</b></td>
        <td nowrap><asp:TextBox ID="BePoTextBox" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="customerpolook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Customer Po#:</b></td>
        <td nowrap><asp:TextBox ID="EndPoTextBox" MaxLength="15" Width="100px" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="customerpo2look(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
        </td>
      </tr>  
          
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Sales Rep#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" MaxLength="3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px" nowrap><b>Ending Sales Rep#:</b></td>
          <td nowrap ><asp:TextBox ID="TextBox4" MaxLength="3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
        
        <tr>                         
            <td colspan="4" align="center"><table><tr>
            <td nowrap><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:CheckBox ID="CheckBox1" Text="Include Zero Quantity On Hand?" runat="server"></asp:CheckBox></b><br />
                   <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:CheckBox ID="CheckBox2" Text="Include Cutomer Owned Warehouse?" runat="server"></asp:CheckBox></b><br />
                   <b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<asp:CheckBox ID="CheckBox3" Text="Include Cutomer Part#?" runat="server"></asp:CheckBox></b></td>
                   <td nowrap> <fieldset>
                    <b>Only Show QOH That is...</b><br />
                    <b>Older Than: &nbsp;<asp:TextBox ID="OlderTextBox" Width="70px" MaxLength="7" runat="server"></asp:TextBox> </b>
                       <asp:CompareValidator ID="CompareValidator1" ControlToValidate="OlderTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true" runat="server" ErrorMessage="Enter only Integer Value "></asp:CompareValidator>
                   </fieldset>           
            </td></tr></table>                                 
            </td>            
        </tr> 
         <tr><td nowrap colspan="3" align="center" >&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem  Value="Order Date"    Text="Order Date" />
                  <asp:ListItem  Value="Due Date"    Text="Due Date" />                                
         </asp:RadioButtonList> </td></tr>
          <tr><td nowrap colspan="3" align="center" ><b>Sort By Receipt Date?</b>
             <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem   Value="A"    Text="Ascending" />
                  <asp:ListItem   Value="D"   Text="Descending" />                                  
         </asp:RadioButtonList> </td></tr>
        
                        
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
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                  
                                       
              <ItemTemplate>
                  CustimReceipt:
                  <asp:Label ID="CustimReceiptLabel" runat="server" 
                      Text='<%# Bind("CustimReceipt") %>'></asp:Label><br />                 
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectCustimReceiptDateRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmAsof" Type="String" />
                  <asp:Parameter Name="prmBeCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBePo" Type="String" />
                  <asp:Parameter Name="prmEndPo" Type="String" />
                  <asp:Parameter Name="prmBeSman" Type="String" />
                  <asp:Parameter Name="prmEndSman" Type="String" />
                  <asp:Parameter Name="prmOlder" Type="Int32" />
                  <asp:Parameter Name="prmQtyonHand" Type="String" />
                  <asp:Parameter Name="prmWarehouse" Type="String" />
                  <asp:Parameter Name="prmCustPart" Type="String" />
                  <asp:Parameter Name="prmOrderDue" Type="String" />
                  <asp:Parameter Name="prmReceipt" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


