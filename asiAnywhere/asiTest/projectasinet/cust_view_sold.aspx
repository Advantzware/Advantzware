<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="cust_view_sold" Codebehind="cust_view_sold.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Sold To</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >

    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
<script language="javascript"  >
function board()
{
}

var bSelected=false;
function ChSel()
{
    var theForm = document.forms['frmList'];
    if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
    var i;
    for (i=0;i<theForm.chDelete.length;++i) 
        theForm.chDelete[i].checked=bSelected;
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
function focusval(obj)
{
    obj.style.backgroundColor='blue';
    obj.style.color = 'white';
}
function blurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
} 
function zipblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    if(document.getElementById("FormView1_vsoldidTextBox"))
    {
        var soldid=document.getElementById("FormView1_vsoldidTextBox");
        soldid.focus();
    }
    else if(document.getElementById("FormView1_vsoldnameTextBox"))
    {
        var soldname = document.getElementById("FormView1_vsoldnameTextBox");
        soldname.focus();
    }
}  

function zipcodelook()
{ 
  var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3)
{
    document.forms[0].FormView1_vsoldcityTextBox.value = ReturnObj2;
    document.forms[0].FormView1_vsoldzipTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vsoldstateTextBox.value = ReturnObj3; 
}
function citylook()
{ 
  var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1)
{
  document.forms[0].FormView1_vsoldcityTextBox.value = ReturnObj1;
}
function statecodelook()
{ 
  var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1)
{ 
  document.forms[0].FormView1_vsoldstateTextBox.value = ReturnObj1;
}
  
 </script>

</head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
            <hd:header id="Header1" runat="server"></hd:header>
                <div>
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                            <TD width=30>&nbsp;</TD>
                            <TD align=center nowrap><font size=+0><b>Sold To&nbsp;</b></font></TD>
                            <td nowrap>
                                <asp:LinkButton ID="backtomenuLinkButton" OnClick ="Back_tomenu_Click" runat="server">Back to menu</asp:LinkButton>
                            </td>
                            <TD  align="left" nowrap>Logged as&nbsp;
                                <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
                                &nbsp;<b>Company: &nbsp;</b><asp:label id="labelcompany"   runat="server" Font-Bold="True">&nbsp;</asp:label>
                            </TD>
                            <TD vAlign="middle" width="20">&nbsp;</TD>
                            <td width=30>&nbsp;</td>
                        </TR>
                </TABLE>
                <table>
                    <tr bgcolor="gray">
                        <td nowrap><div  id="navigation" style="width:100%">
		                        <ul nowrap> <li  >
                            <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_Listcustomers_Click" >List Customers</asp:LinkButton></li>
                            <li><asp:LinkButton ID="lnk_viewcustomers" runat="server"  OnClick="lnk_viewcustomers_Click"  >View Customers</asp:LinkButton></li>
                            <li><asp:LinkButton ID="lnk_listship" runat="server" OnClick="lnk_listship_Click" >List Ship To</asp:LinkButton></li>
                            <li><asp:LinkButton ID="lnk_viewship" runat="server"  OnClick="lnk_viewship_Click"  >View Ship To </asp:LinkButton></li>
                            <li><asp:LinkButton ID="lnk_listsold" runat="server" OnClick="lnk_listsold_Click" >List Sold To </asp:LinkButton></li>
                            <li class="selected"><asp:LinkButton ID="lnk_viewsold" runat="server" OnClick="lnk_viewsold_Click" >View Sold TO</asp:LinkButton></li></ul></div>
                        </td>
                    </tr>
                </table>
                <fieldset style="background-color:#EFF3FB;  width:510px;">
                    <legend style="color:Blue ">Reference Information </legend>
                        <table width="510px">
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Customer:</b></td>
                                <td>
                                    <asp:Label ID="Customer" Font-Bold="true" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                </td>
                                <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                <td>
                                    <asp:Label ID="CustName" Font-Bold="true" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                </td>
                            </tr>
                        </table>    
                </fieldset>

                <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
                    <EditItemTemplate>
                    <fieldset>
                        <table class="shade" width="500px">
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Sold To Id:</b></td>
                                <td>
                                    <asp:Label ID="vsoldidLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vsoldid") %>'></asp:Label>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                <td>
                                    <asp:TextBox ID="vsoldnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldname") %>'></asp:TextBox>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px"><b>Address:</b></td>
                                <td>
                                    <asp:TextBox ID="vsoldaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldaddr1") %>'></asp:TextBox>
                                </td>
                            </tr>
                            <tr>
                                <td></td>
                                <td>
                                    <asp:TextBox ID="vsoldaddr2TextBox"  onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldaddr2") %>'></asp:TextBox>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="padding-right:5px"><b>City:</b></td>
                                <td nowrap>
                                    <asp:TextBox ID="vsoldcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldcity") %>'></asp:TextBox>
                                    <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="CityLookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                                </td>
                                <td><b>State:</b></td>
                                <td nowrap>
                                    <asp:TextBox ID="vsoldstateTextBox"  onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vsoldstate") %>'></asp:TextBox>
                                    <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="statelookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                                </td>
                                <td><b>Zip:</b></td>
                                <td nowrap>
                                    <asp:TextBox ID="vsoldzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:zipblurval(this)"  runat="server" width="80px" Text='<%# Bind("vsoldzip") %>'></asp:TextBox>
                                    <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="ziplookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                                </td>
                            </tr>
                            <tr>
                                <td colspan="3">
                                    <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True"  OnClick="UpdateButton_Click" Text="Save">
                                    </asp:Button>
                                    <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel">
                                    </asp:Button>
                                </td>
                            </tr>
                       </table>
                       </fieldset>
                </EditItemTemplate>
              
                <InsertItemTemplate>
                 <fieldset>
                    <table class="shade" width="500px">
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Sold To Id:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vsoldidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldid") %>'></asp:TextBox>
                                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vsoldidTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td>
                                <asp:TextBox ID="vsoldnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldname") %>'></asp:TextBox>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:TextBox ID="vsoldaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldaddr1") %>'></asp:TextBox>
                            </td>
                        </tr>
                        <tr>
                            <td></td>
                            <td>
                                <asp:TextBox ID="vsoldaddr2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldaddr2") %>'></asp:TextBox>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vsoldcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vsoldcity") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="CityLookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                            </td>
                            <td><b>State:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vsoldstateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="50px" runat="server" Text='<%# Bind("vsoldstate") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="statelookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                            </td>
                            <td><b>Zip:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vsoldzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:zipblurval(this)" runat="server" width="80px" Text='<%# Bind("vsoldzip") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="ziplookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                 
                            </td>
                        </tr>
                        <tr>
                            <td  colspan="3">
                                <asp:Button ID="InsertAddButton" CssClass="button" runat="server" CausesValidation="True"   OnClick="AddButton_Click" Text="Save">
                                </asp:Button>
                                <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False"  CommandName="Cancel" Text="Cancel">
                                </asp:Button>
                            </td>
                        </tr>
                     </table>
                     </fieldset>
                </InsertItemTemplate>
              
                <ItemTemplate>
                 <fieldset>
                    <table class="shade" width="500px" >
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Sold TO ID:</b></td>
                            <td>
                                <asp:Label ID="vsoldidLabel" width="130px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldid") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px" ><b>Name:</b></td>
                            <td>
                                <asp:Label ID="vsoldnameLabel"  width="130px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldname") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:Label ID="vsoldaddr1Label" runat="server" width="130px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vsoldaddr1") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td></td>
                            <td>
                                <asp:Label ID="vsoldaddr2Label" runat="server" width="130px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldaddr2") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td>
                                <asp:Label ID="vsoldcityLabel"  width="130px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldcity") %>'></asp:Label>
                            </td>
                            <td><b>State:</b></td>
                            <td>
                                <asp:Label ID="vsoldstateLabel" width="50px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldstate") %>'></asp:Label>
                            </td>
                            <td><b>Zip:</b></td>
                            <td>
                                <asp:Label ID="vsoldzipLabel" width="100px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vsoldzip") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td colspan="3">
                                <asp:Button ID="AddButton" CssClass="button" runat="server" CausesValidation="true" CommandName="new" Text="Add">
                                </asp:Button>
                                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="edit" Text="Update">
                                </asp:Button>
                                <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False" OnClick="DeleteButton_Clock"   OnClientClick="return confirm('Are you sure to want to Delete?')"  Text="Delete">
                                </asp:Button>
                            </td>
                        </tr>
                    </table>
                    </fieldset>
                </ItemTemplate>
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="Selectsoldto" TypeName="contact">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:SessionParameter Name="prmCustomer" SessionField="customer1_list_cust" Type="String" />
                  <asp:SessionParameter Name="prmsoldid" SessionField="customer_list_soldto" Type="string" DefaultValue="" />
                  <asp:Parameter Name="prmsoldno" Type="Int32" />
                  <asp:Parameter Name="prmsoldname" Type="String" />
                  <asp:Parameter Name="prmsoldcity" Type="String" />
                  <asp:Parameter Name="prmsoldstate" Type="String" />
                  <asp:Parameter Name="prmsoldzip" Type="String" />
                  <asp:Parameter Name="prmsoldaddr1" Type="String" />
                  <asp:Parameter Name="prmsoldaddr2" Type="String" />
                  <asp:Parameter Name="prmsoldreckey" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
           
         <asp:Button ID="newaddButton" runat="server" CssClass="button"  OnClick="newaddButton_Click" Text="Add" />
          
    </div>
  <ft:footer id="Footer1" runat="server"></ft:footer>
 </form>
</body>
</HTML>

