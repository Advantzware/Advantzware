<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="viewitem_cust" Codebehind="viewitem_cust_inv.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>File Maintenance Item</title>
    <link href="include/style2.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
    <script>
    
    function contactcustomerlook()
    { 
        var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1,ReturnObj2)
    { 
        document.forms[0].FormView1_vCustNumTextBox.value = ReturnObj1;    
    }

    function fgitemlook()
    { 
        var NewWindow = window.open("fgitemlook.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function FGitemLookup(ReturnObj1,ReturnObj2)
    { 
        document.forms[0].FormView1_vFgItmNumTextBox.value = ReturnObj1;
        document.forms[0].FormView1_vCustPartNumTextBox.value = ReturnObj2;
        document.getElementById("FormView1_vFgItmNumTextBox").onchange();  
    }
        
    function crossreflook()
    { 
        var NewWindow = window.open("crossref_lookup.aspx","CrossReferenceLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function CrossRefLookup(ReturnObj1,ReturnObj2,ReturnObj3)
    { 
        document.forms[0].FormView1$vCustNumTextBox.value = ReturnObj1;
        document.forms[0].FormView1$vCustVenCodeTextBox.value = ReturnObj2;
        document.getElementById("FormView1_vCustNumTextBox").onchange();
    }
        
    function custplantlook()
    { 
        var custNum = document.getElementById("FormView1_vCustNumTextBox").value;        
        var NewWindow = window.open("cust_plant_lookup.aspx?custNum="+ custNum +"","CustomerPoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function CustPlantLookup(obj1,obj2,obj3,obj4,obj5,obj6)
    {     
        document.forms[0].FormView1$vVendPlantIdTextBox.value = obj2;
        document.forms[0].FormView1$vVendorDeptCodeTextBox.value = obj3;
        document.getElementById("FormView1_vVendPlantIdTextBox").onchange();     
    }
    
    </script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>File Maintenance Item&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            <b>Company:</b>&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li  >
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click">List Item</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click">View Item</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    <br />
        <asp:Button ID="addnewbutton" CssClass = "button" OnClick="newbutton_click" runat="server" Text="Add" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="Formview1_DataBound" OnPreRender="FormView1_PreRender">
            <EditItemTemplate>
                <table class="shade">                
                    <tr><td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>
                        <td><asp:TextBox ID="vCustNumTextBox" runat="server" AutoPostBack="true" OnTextChanged="CustNum_TextChange" Text='<%# Bind("vCustNum") %>'>
                            </asp:TextBox><a href="#" onClick="crossreflook(); return false"><asp:Image ID="CRefLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                        <td align="right" style="padding-right:5px"><b>REV:</b></td>
                        <td><asp:TextBox ID="vRevisionTextBox" runat="server" Text='<%# Bind("vRevision") %>'></asp:TextBox></td>
                    </tr>
                    <tr>    
                        <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                        <td><asp:TextBox ID="vFgItmNumTextBox" MaxLength="10" runat="server" AutoPostBack="true" OnTextChanged="FgItmNum_TextChange"  Text='<%# Bind("vFgItmNum") %>'>
                            </asp:TextBox><a href="#" onClick="fgitemlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                        <td align="right" style="padding-right:5px"><b>Customer Part:</b></td>
                        <td><asp:TextBox ID="vCustPartNumTextBox" MaxLength="2" runat="server" Text='<%# Bind("vCustPartNum") %>'></asp:TextBox></td>
                    </tr>   
                    <tr>  
                        <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                        <td><asp:TextBox ID="vCustVenCodeTextBox" runat="server" Text='<%# Bind("vCustVenCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Customers Plant Id:</b></td>
                        <td><asp:TextBox ID="vVendPlantIdTextBox" runat="server" AutoPostBack="true" OnTextChanged="PlantId_TextChange" Text='<%# Bind("vVendPlantId") %>'></asp:TextBox>
                            <a href="#" onClick="custplantlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                    </tr>                         
                    <tr>  
                        <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                        <td><asp:TextBox ID="vVendorDeptCodeTextBox" runat="server" Text='<%# Bind("vVendorDeptCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Obsolete Date:</b></td>
                        <td><asp:TextBox ID="vOsoleteDateTextBox" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Text='<%# Bind("vOsoleteDate") %>'></asp:TextBox>
                            <a href="#" onclick="showCalendarControl(FormView1_vOsoleteDateTextBox); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                        </td>
                    </tr>                       
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Est. Annual Usage:</b></td>
                        <td><asp:TextBox ID="vAnnUsageTextBox" runat="server" Text='<%# Bind("vAnnUsage") %>'>
                            </asp:TextBox><asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vAnnUsageTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                        </td>
                        <td align="right" style="padding-right:5px"><b>Customers On Hand Qty:</b></td>
                        <td><asp:TextBox ID="vCustHandQtyTextBox" runat="server" Text='<%# Bind("vCustHandQty") %>'>
                            </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustHandQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                        </td>
                    </tr>   
                    <tr>
                        <td>
                        <asp:Label ID="updateLabel1" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>'></asp:Label>
                        </td>
                    </tr>
                <tr><td colspan="2">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Update_Button_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table>
            </EditItemTemplate>
            <InsertItemTemplate>
                <table class="shade">                
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>
                        <td><asp:TextBox ID="vCustNumTextBox" AutoPostBack="true" OnTextChanged="CustNum_TextChange" runat="server" Text='<%# Bind("vCustNum") %>'>
                            </asp:TextBox><a href="#" onClick="crossreflook(); return false"><asp:Image ID="CRefLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                        <td align="right" style="padding-right:5px"><b>REV:</b></td>
                        <td><asp:TextBox ID="vRevisionTextBox" runat="server" Text='<%# Bind("vRevision") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                        <td><asp:TextBox ID="vFgItmNumTextBox" MaxLength="10" runat="server" AutoPostBack="true" OnTextChanged="FgItmNum_TextChange" Text='<%# Bind("vFgItmNum") %>'>
                            </asp:TextBox><a href="#" onClick="fgitemlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                        <td align="right" style="padding-right:5px"><b>Customer Part:</b></td>
                        <td><asp:TextBox ID="vCustPartNumTextBox" MaxLength="2" runat="server" Text='<%# Bind("vCustPartNum") %>'></asp:TextBox></td>
                    </tr>    
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                        <td><asp:TextBox ID="vCustVenCodeTextBox" runat="server" Text='<%# Bind("vCustVenCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Customers Plant Id:</b></td>
                        <td><asp:TextBox ID="vVendPlantIdTextBox" runat="server" AutoPostBack="true" OnTextChanged="PlantId_TextChange" Text='<%# Bind("vVendPlantId") %>'></asp:TextBox>
                        <a href="#" onClick="custplantlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        </td>
                    </tr>                      
                    <tr>
                        <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                        <td><asp:TextBox ID="vVendorDeptCodeTextBox" runat="server" Text='<%# Bind("vVendorDeptCode") %>'></asp:TextBox></td>
                        <td align="right" style="padding-right:5px"><b>Obsolete Date:</b></td>
                        <td><asp:TextBox ID="vOsoleteDateTextBox" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server" Text='<%# Bind("vOsoleteDate") %>'></asp:TextBox>
                            <a href="#" onclick="showCalendarControl(FormView1_vOsoleteDateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /> </a>
                        </td>
                    </tr>                     
                    <tr> 
                        <td align="right" style="padding-right:5px"><b>Customers Est. Annual Usage:</b></td>
                        <td><asp:TextBox ID="vAnnUsageTextBox" runat="server" Text='<%# Bind("vAnnUsage") %>'>
                            </asp:TextBox><asp:CompareValidator ID="CompareValidator11" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vAnnUsageTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                        </td>
                        <td align="right" style="padding-right:5px"><b>Customers On Hand Qty:</b></td>
                        <td><asp:TextBox ID="vCustHandQtyTextBox" runat="server" Text='<%# Bind("vCustHandQty") %>'>
                            </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="vCustHandQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                        </td>
                    </tr>
                
                <tr><td>
                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Insert_Button_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr>
                </table>
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade" width="600px">
                <tr>
                    <td align="right" style="padding-right:5px"><b>Suppliers A/R Code:</b></td>
                    <td><asp:Label ID="vCustNumLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vCustNum") %>'></asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>REV:</b></td>
                    <td><asp:Label ID="vRevisionLabel" runat="server" Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRevision") %>'></asp:Label></td>
                </tr>    
                <tr>
                    <td align="right" style="padding-right:5px"><b>Suppliers FG Item:</b></td>
                    <td><asp:Label ID="vFgItmNumLabel" runat="server" Text='<%# Bind("vFgItmNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Customer Part:</b></td>
                    <td><asp:Label ID="vCustPartNumLabel" runat="server" Text='<%# Bind("vCustPartNum") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>    
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers A/P Code:</b></td>
                    <td><asp:Label ID="vCustVenCodeLabel" runat="server" Text='<%# Bind("vCustVenCode") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Customers Plant Id:</b></td>
                    <td><asp:Label ID="vVendPlantIdLabel" runat="server" Text='<%# Bind("vVendPlantId") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>                   
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers Dept Code:</b></td>
                    <td><asp:Label ID="vVendorDeptCodeLabel" runat="server" Text='<%# Bind("vVendorDeptCode") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"> </asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Obsolete Date:</b></td>
                    <td><asp:Label ID="vOsoleteDateLabel" runat="server" Text='<%# Bind("vOsoleteDate") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>                
                <tr>
                    <td align="right" style="padding-right:5px"><b>Customers Est. Annual Usage:</b></td>
                    <td><asp:Label ID="vAnnUsageLabel" runat="server" Text='<%# Bind("vAnnUsage") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                    <td align="right" style="padding-right:5px"><b>Customers On Hand Qty:</b></td>
                    <td><asp:Label ID="vCustHandQtyLabel" runat="server" Text='<%# Bind("vCustHandQty") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label></td>
                </tr>
                <tr>    
                    <td><asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("vRecKey") %>' Width="120px" BackColor="turquoise" BorderColor="Black" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                        </td> 
                </tr>
                <tr><td colspan="3"> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;              
                <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add">
                </asp:Button>
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update"></asp:Button>
                <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"
                Text="Delete"></asp:Button>
            </td></tr>   
               
                </table>
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="updateCustInv" TypeName="custitem">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />                
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmCustNum" Type="String" />
                <asp:Parameter Name="prmRevision" Type="String" />
                <asp:Parameter Name="prmCustPartNum" Type="String" />
                <asp:Parameter Name="prmFgItmNum" Type="String" />
                <asp:Parameter Name="prmVendCode" Type="String" />
                <asp:Parameter Name="prmVendPlantId" Type="String" />
                <asp:Parameter Name="prmVendDeptCode" Type="String" />
                <asp:Parameter Name="prmOsoleteDate" Type="String" />
                <asp:Parameter Name="prmAnnUsageQty" Type="Decimal" />
                <asp:Parameter Name="prmOnHandQty" Type="Decimal" />
                <asp:SessionParameter Name="prmRecId" SessionField="itemcustinv_no" Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
        
        <br />
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>