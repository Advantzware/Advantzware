<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="purge_contacts_notes" Codebehind="purge_contacts_notes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Purge Contacts and Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
    <script language = "JavaScript" type="text/javascript">
    
//    var bSelected=false;
//    function ChSel()
//    {
//        var theForm = document.forms['frmList'];
//        if (!theForm) theForm = document.frmList;
//        bSelected = !bSelected; 
//        var i;
//        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
//    } 
//    
//    function OnKeyDown()
//    {
//        e = window.event;
//        if (e.keyCode == 13)
//        {
//            e.cancel = true;
//            var theForm = document.forms['frmList'];
//            if (!theForm) theForm = document.frmList;                
//            theForm.btnSearch.click();              
//        }
//    }
    
    
    function showhide()
    {
    var show= document.getElementById("firstshow");
     show.style.display='inline';
     var hide=document.getElementById("secondshow");
     hide.style.display='none';
     
     
    }
    
    function showhide2()
    {
    var show= document.getElementById("firstshow");
     show.style.display='none';
     var hide=document.getElementById("secondshow");
     hide.style.display='inline';
    
     
    }
    function statuslook(){ 
  var NewWindow = window.open("status_lookup.aspx","statusLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function statusLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].txt_statuscode.value = ReturnObj1;
  
}
 function siclook(){ 
  var NewWindow = window.open("sic_lookup.aspx","sicLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function sicLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].txt_siccode.value = ReturnObj1;
}
 function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1){ 
  document.forms[0].txt_customer.value = ReturnObj1;
  }

function typelook(){ 
  var NewWindow = window.open("type_lookup.aspx","typeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TypeLookup(ReturnObj1){ 
  document.forms[0].txt_type.value = ReturnObj1;
 }    
    </script> 
    
    <script>
var counter = 0; 
var pattern = '^dbGrid_contact';
function Check(parentChk)
{
    var elements =  document.getElementsByTagName("INPUT");   

    for(i=0; i<elements.length;i++)
    {
        if(parentChk.checked == true)
        { 
            if( IsCheckBox(elements[i]) && IsMatch(elements[i].id))
            {
            elements[i].checked = true;
            }        
        }
        else
        {
            elements[i].checked = false;
        }      
    }         

}

function IsMatch(id)
{
    var regularExpresssion = new RegExp(pattern);
    if(id.match(regularExpresssion)) return true;
    else return false;
}
function IsCheckBox(chk)
{
    if(chk.type == 'checkbox') return true;
    else return false;
}

function AddEvent(obj, evType, fn)
{
    if (obj.addEventListener)
    {
    obj.addEventListener(evType, fn, true);
    return true;
    } 

 else if (obj.attachEvent)
 {
    var r = obj.attachEvent("on"+evType, fn);
    return r;
 }
  else
   {
    return false;
   }   
}

function AttachListener()
{
    var elements =  document.getElementsByTagName("INPUT");
    for(i=0; i< elements.length; i++)
     {      
        if( IsCheckBox(elements[i]) &&  IsMatch(elements[i].id))
        {
            AddEvent(elements[i],'click',CheckChild);
        }
    }   
}

function CheckChild(e)
{
    var evt = e || window.event;
    var obj = evt.target || evt.srcElement
    if(obj.checked)
    {
        if(counter < GetChildCheckBoxCount())
            { counter++; }       
    }   
    else
    {
       if(counter > 0) { counter--; }   
    }      

    if(counter == GetChildCheckBoxCount())
    { document.getElementById("chkAll").checked = true; }
    else if(counter < GetChildCheckBoxCount()) { document.getElementById("chkAll").checked = false; }   
}
function Check(parentChk)
{
    var elements =  document.getElementsByTagName("INPUT");
    for(i=0; i<elements.length;i++)
    {
            if(parentChk.checked == true)
        { 
            if( IsCheckBox(elements[i]) &&  IsMatch(elements[i].id))
            {
            elements[i].checked = true;
            }        
        }
        else
        {
            elements[i].checked = false;
            counter = 0;
        }      
    }
   if(parentChk.checked == true)
    {
        counter = GetChildCheckBoxCount();
    }      

}

</script>
  </head>    
   <body>
    <form id="frmList" runat="server" defaultbutton='btnSearch' defaultfocus='txtSearchValue'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
                   
      <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;Contacts and Notes&nbsp;</b></font></td>
          <TD vAlign="middle">
            <asp:linkbutton id="hlkBackToMenu" runat="server" OnClick="hlkBackToMenu_Click">Back to menu</asp:linkbutton>
          </TD> 
          <td valign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label></td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
       <table>
    <tr bgcolor="gray">
    <%--<td>
        <asp:LinkButton ID="lnk_listcontact" runat="server"><img src="Images/list contacts 1.jpg" border="0" alt="List Contacts" /></asp:LinkButton>
        <asp:LinkButton ID="lnk_viewcontact" runat="server" OnClick="lnk_viewcontacts_click"><img src="Images/view contacts 0.jpg" border="0" alt="View Contacts" /></asp:LinkButton>
        <asp:LinkButton ID="lnk_notes" runat="server" OnClick="lnk_notes_click"><img src="Images/notes 0.jpg" border="0" alt="Notes" /></asp:LinkButton>
        <asp:LinkButton ID="lnk_MailList" runat="server" OnClick="lnk_MailList_click"><img src="Images/mail_label_0.jpg" border="0" alt="Mail List" /></asp:LinkButton>
    </td>--%>
    </tr>
    </table>
      
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='95%' border="0">
        <TR>
          <TD style="width: 1330px;">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		   <tr>
        		<td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                    <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btnShowAll_Click" Width="40px" />&nbsp;
                    </td>              
                  
               
                  <td class="shade"  align="center" nowrap>
                  <b>Last Name</b><br />
                    <asp:TextBox ID="txt_lastname" Width="100px" runat="server"></asp:TextBox>
                    </td>
                    <td class="shade" align="center" nowrap>
                   <b>Customer</b><br />
                    <asp:textbox id="txt_customer" runat="server" Width="100px"></asp:textbox>
                    <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </td>
                    <td class="shade" align="center" nowrap>
                    <b>ShipId</b><br />
                    <asp:textbox id="txt_shipid" runat="server" Width="100px"></asp:textbox>
                </td>
                <td class="shade" align="center" nowrap>
                    <b>Type</b><br />
                    <asp:textbox id="txt_type" runat="server" Width="80px"></asp:textbox>
                    <a href="#" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <%--<td class="shade" nowrap>
                    Company:<br />
                    <asp:textbox id="txt_company" runat="server" Width="80px"></asp:textbox>
                </td>--%>
                    <td class="shade" align="center"  nowrap>
                    <b>Sic Code</b><br />
                    <asp:textbox id="txt_siccode" runat="server" Width="80px"></asp:textbox>
                    <a href="#" onClick="siclook(); return false"><asp:Image ID="Siclookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
                <td class="shade" align="center"  nowrap>
                    <b>Status Code</b><br />
                    <asp:textbox id="txt_statuscode" runat="server" Width="80px"></asp:textbox>
                    <a href="#" onClick="statuslook(); return false"><asp:Image ID="statuslookup" runat="server" ImageUrl="images/lookup_icon.gif" />
                </td>               
                
                <%--<td id="tdInfo" runat="server" class="shade" align="center" width="100">
                  <asp:label id="lblCount" runat="server" Height="3px">Details found:&nbsp;0</asp:label><BR>
                  <asp:label id="lblPage" runat="server">Page&nbsp;<%=

                 (dbGrid_contact.PageCount ==0)?0:dbGrid_contact.PageIndex + 1
                  %>&nbsp;of&nbsp;<%=dbGrid_contact.PageCount%></asp:label>
                </td>--%>
                <TD id="tdPageCount" runat="server" class="shade" align="left">
          <table><tr><td align="center">
            <b>Records/Page:</b><BR>
           
            <asp:FormView ID="FormView3" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="40px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
          </td></tr></table>  
                </TD>
              </tr>
            </table>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        
      </table>
      
            <asp:GridView id="dbGrid_contact" runat="server" CssClass="Grid" Width="100%"
                                       
                  OnRowCreated="dbGrid_contact_RowCreated"
         OnPageIndexChanging="dbGrid_contact_PageIndexChanging"
                  
         OnSorting="GridView1_Sorting"
                                    

                  BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_contact_SelectedIndexChanged" AllowPaging="True" AllowSorting="True">
        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle BackColor="Teal" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White"></HeaderStyle>
        
              <Columns>
                <%--<asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />
                </asp:CommandField> --%>    

                    <asp:TemplateField>
                    <HeaderTemplate>
<input type="checkbox" id="chkAll" name="chkAll" onclick="Check(this)" />

</HeaderTemplate>
                    <ItemTemplate>
                    <asp:CheckBox ID="chk1" runat="server" />
                    </ItemTemplate>
                    </asp:TemplateField>          
                
                
                  <asp:TemplateField HeaderText="sman" SortExpression="sman" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label1" runat="server" Text='<%# Eval("sman") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label1" runat="server" Text='<%# Bind("sman") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="middle_initial" SortExpression="middle_initial" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label2" runat="server" Text='<%# Eval("middle_initial") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label2" runat="server" Text='<%# Bind("middle_initial") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="sirname"  SortExpression="sirname" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label3" runat="server" Text='<%# Eval("sirname") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label3" runat="server" Text='<%# Bind("sirname") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="maillist"  Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label4" runat="server" Text='<%# Eval("maillist") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label4" runat="server" Text='<%# Bind("maillist") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="Contact_loc" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label5" runat="server" Text='<%# Eval("contact_loc") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label5" runat="server" Text='<%# Bind("contact_loc") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="addr1" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label6" runat="server" Text='<%# Eval("addr1") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label6" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="addr2" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label7" runat="server" Text='<%# Eval("addr2") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label7" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="city" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label8" runat="server" Text='<%# Eval("city") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label8" runat="server" Text='<%# Bind("city") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="state" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label9" runat="server" Text='<%# Eval("state") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label9" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="zip" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label10" runat="server" Text='<%# Eval("zip") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label10" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="country" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label11" runat="server" Text='<%# Eval("country") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label11" runat="server" Text='<%# Bind("country") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="county" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label12" runat="server" Text='<%# Eval("county") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label12" runat="server" Text='<%# Bind("county") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="territory" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label13" runat="server" Text='<%# Eval("territory") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label13" runat="server" Text='<%# Bind("territory") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="access_code" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label14" runat="server" Text='<%# Eval("access_code") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label14" runat="server" Text='<%# Bind("access_code") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="cell_phone" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label15" runat="server" Text='<%# Eval("cell_phone") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label15" runat="server" Text='<%# Bind("cell_phone") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="fax" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label16" runat="server" Text='<%# Eval("fax") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label16" runat="server" Text='<%# Bind("fax") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="email" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label17" runat="server" Text='<%# Eval("email") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label17" runat="server" Text='<%# Bind("email") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="website" Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label18" runat="server" Text='<%# Eval("website") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label18" runat="server" Text='<%# Bind("website") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField  HeaderText="Supplier Code" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label19" runat="server" Text='<%# Eval("comp_code") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label19" runat="server" Text='<%# Bind("comp_code") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField  HeaderText="Status Code" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label20" runat="server" Text='<%# Eval("status_code") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label20" runat="server" Text='<%# Bind("status_code") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField  HeaderText="Sic Code" Visible="False">
                      <EditItemTemplate>
                          <asp:Label ID="Label21" runat="server" Text='<%# Eval("sic_code") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label21" runat="server" Text='<%# Bind("sic_code") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  
                   <asp:TemplateField HeaderText="Rec_key"  Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label22" runat="server" Text='<%# Eval("rec_key") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label22" runat="server" Text='<%# Bind("rec_key") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  
                  <asp:TemplateField HeaderText="Comp Des"  Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label23" runat="server" Text='<%# Eval("comp_des") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label23" runat="server" Text='<%# Bind("comp_des") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="Status Des"  Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label24" runat="server" Text='<%# Eval("status_des") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label24" runat="server" Text='<%# Bind("status_des") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                  <asp:TemplateField HeaderText="Sic Des"  Visible="False" >
                      <EditItemTemplate>
                          <asp:Label ID="Label25" runat="server" Text='<%# Eval("sic_des") %>'></asp:Label>
                      </EditItemTemplate>
                      <ItemTemplate>
                          <asp:Label ID="Label25" runat="server" Text='<%# Bind("sic_des") %>'></asp:Label>
                      </ItemTemplate>
                  </asp:TemplateField>
                
              </Columns>
            </asp:GridView>
               <br />
     <asp:Button ID="DeleteButton" runat="Server" Text="Delete" CssClass="buttonM" OnClick="Delete_click" />
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
        </DIV>
    </form>
  </body>
</html>

