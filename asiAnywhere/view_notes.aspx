<%@ Page Language="C#" AutoEventWireup="true" Inherits="View_notes" Codebehind="view_notes.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>View Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
     <script language="javascript" src="include/date.js"></script>
     <script language="javascript" src="include/event.js"></script>
     <script language="javascript" src="include/insert.js"></script>
    
    <script language = "JavaScript" src="include/CalendarControl.js">
   
    </script>
    <script>
    function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].FormView1_note_dateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].FormView1_note_dateTextBox.value="";
  Datelook();
}
function datevalidate()
{
    var date=document.getElementById("FormView1_note_dateTextBox").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("FormView1_note_dateTextBox").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("FormView1_note_dateTextBox").value = date + "/";
    }
    
}
    </script>
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <div>
    <table id="tblTop" cellspacing="3" align="center" border="0" width="100%">
        <tr>
          
          <td align="center"><font size="+0"><b>&nbsp;View Notes&nbsp;</b></font></td>
          <td><asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton></td>
          <td valign="middle" align="center"><b>Users</b>&nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b> &nbsp;<asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label></td>
          
          <td valign="middle" width="20">&nbsp;</td>
          
          <td width="30">&nbsp;</td>
        </tr>
      </table>
      
      <table>
    <tr bgcolor="gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap> <li id="lilistcnd" runat="server">
         <asp:LinkButton ID="lnk_listcontact" runat="server" OnClick="lnk_listcontacts_click">List Contacts</asp:LinkButton></li>
         <li id="liviewcnd" runat="server"><asp:LinkButton ID="lnk_viewcontact" runat="server" OnClick="lnk_viewcontacts_click">View Contacts</asp:LinkButton></li>
         <li id="lilistsp" runat="server"><asp:LinkButton ID="lnk_listsupplier" runat="server" OnClick="lnk_listsupplier_Click" >List Supplier</asp:LinkButton></li>
         <li id="liviewsp" runat="server" ><asp:LinkButton ID="lnk_viewsupplier" runat="server" OnClick="lnk_viewsupplier_Click">View Supplier</asp:LinkButton>      </li>
         <li><asp:LinkButton ID="lnk_brwsnotes" runat="server" OnClick="lnk_brwsnotes_click">List Notes</asp:LinkButton></li>
         <li class="selected"><asp:LinkButton ID="lnk_viewnotes" runat="server">View Notes</asp:LinkButton></li>
        
         <li id="limail" runat="server"><asp:LinkButton ID="lnk_MailList" runat="server" OnClick="lnk_MailList_click">Mail Label</asp:LinkButton></li>
         <li id="licalendar" runat="server"><asp:LinkButton ID="lnk_calendar" runat="server" OnClick="lnk_calendar_click">Calendar</asp:LinkButton></li></ul></div>
    </td>
    </tr>
    </table>
    <%--<a href="contact_list.aspx">Contact List</a>--%>
    
   <%-- <asp:LinkButton ID="comp_supplier" runat="server" OnClick="comp_supplier_click">List Supplier</asp:LinkButton>
    <asp:LinkButton ID="contact_list" runat="server" OnClick="contact_list_click">Contact List</asp:LinkButton>
    <br />--%>
    
    <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
    
    <asp:Button ID="AddButton" runat="server" OnClick="AddButton_Click" Text="Add" CssClass="buttonM" />
    
   <fieldset style="background-color:#EFF3FB">
   <asp:FormView ID="FormView2" Width="400px" runat="server" DataSourceID="SqlDataSource2" CellPadding="4" ForeColor="#333333">
            
            <ItemTemplate>
                <b>First Name:</b>
               <b> <asp:Label ID="first_nameLabel" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label></b>
                &nbsp;&nbsp;&nbsp;
                <b>Last Name:</b>
                <b><asp:Label ID="last_nameLabel" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("last_name") %>'></asp:Label></b>
            </ItemTemplate>
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
        </asp:FormView>
        <asp:SqlDataSource ID="SqlDataSource2" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [first_name], [last_name] FROM [contact] WHERE (([first_name] = @first_name) AND ([last_name] = @last_name))">
            <SelectParameters>
                <asp:SessionParameter Name="first_name" SessionField="contact_list_first_name" Type="String" />
                <asp:SessionParameter Name="last_name" SessionField="contact_list_last_name" Type="String" />
            </SelectParameters>
        </asp:SqlDataSource>
    
    <asp:SqlDataSource id="SqlDataSource1" runat="server"
      SelectCommand="select [rec_key],   [note_date],   [note_time],   [user_id],   [viewed],   [note_title],   [note_text],   [note_code],   [note_form_no],   [note_group],[note_source],[note_type] from   [dbo].[notes] where [rec_key]=@rec_key and [note_time]=@note_time"
       UpdateCommand="update [dbo].[notes] set [note_date]=@note_date, [note_time]=@note_time, [user_id]=@user_id, [viewed]=@viewed, [note_title]=@note_title, [note_text]=@note_text where  [note_time]=@note_time"
       
         InsertCommand="insert into [dbo].[notes] ([rec_key],[note_date],   [note_time],   [user_id],   [viewed],   [note_title],   [note_text], [d_rec_key]) values (@rec_key, @note_date, @note_time, @user_id, @viewed, @note_title, @note_text, @d_rec_key)"
        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
        ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName %>"  OnInserted="Sqldatasource1_Inserted" OnUpdated="Sqldatasource1_update"
    >
        <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="contact_rec_key"
                        Type="String" />
        
        
        <asp:SessionParameter Name="note_time" SessionField="list_notes_time"
                        Type="String" />                                           
        </SelectParameters>
        <UpdateParameters>
        
        <asp:SessionParameter Name="note_time" SessionField="list_notes_time"
                        Type="String" />
        </UpdateParameters>
       
        
        <InsertParameters>
        <asp:SessionParameter Name="rec_key" SessionField="contact_rec_key"
                        Type="String" />
        <asp:SessionParameter Name="d_rec_key" SessionField="contact_rec_key2"
                        Type="String" />                        
                        
        </InsertParameters>
         
       
        
        
    </asp:SqlDataSource>
    
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource1" OnDataBound="FormView1_DataBound">
            <EditItemTemplate>
            
            <table>
            <tr>
            <td><b>Note Date:</b></td>
            <td nowrap><b><asp:TextBox ID="note_dateTextBox" Width="75px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("note_date","{0:MM/dd/yyyy}") %>'>
                </asp:TextBox></b>                
                <a href="#" onClick="showCalendarControl(FormView1_note_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
            <td><b>Note Time: &nbsp;</b></td>
            <td width="100px"><b><asp:Label ID="note_timeTextBox" runat="server" Text='<%# Bind("note_time") %>'>
                </asp:Label></b></td>
            <td><b>UserId: &nbsp;</b></td>
            <td width="100px"><b><asp:Label ID="user_idTextBox" runat="server" Text='<%# Bind("user_id") %>'>
                </asp:Label></b></td>
            <td><b><asp:TextBox Width="40px" Visible="false" MaxLength="1" ID="viewedTextBox" runat="server" Text='<%# Bind("viewed") %>'>
                </asp:TextBox>
                <asp:CheckBox ID="editcheckbox" Checked='<%# Bind("viewed") %>' runat="server" />
            Viewed</b></td>
            </tr>
            </table>
            <table>
            <tr>
            <td><b>Note Title:</b></td>
            <td><b><asp:TextBox ID="note_titleTextBox" runat="server" Text='<%# Bind("note_title") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b></b></td>
            <td><b><asp:TextBox ID="note_textTextBox" TextMode="multiline" Height="200px" Width="400px" runat="server" Text='<%# Bind("note_text") %>'>
                </asp:TextBox></b></td>
            </tr>
            </table>
            
               
                <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Save" OnClick="UpdateButton_Click">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
                
               
            </EditItemTemplate>
            <InsertItemTemplate>
           
            <table>
            <tr>
            <td><b>Note Date:</b></td>
            <td nowrap><b><asp:TextBox ID="note_dateTextBox" Width="75px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("note_date") %>'>
                </asp:TextBox></b>
               
                <a href="#" onClick="showCalendarControl(FormView1_note_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td>
            <td><b>Note Time:</b></td>
            <td width="100px"><b><asp:Label ID="note_timeTextBox" runat="server" Text='<%# Bind("note_time") %>'>
                </asp:Label></b></td>
            <td><b>UserId:</b></td>
            <td width="100px"><b><asp:Label ID="user_idTextBox" runat="server" Text='<%# Bind("user_id") %>'>
                </asp:Label></b></td>
            <td><b><asp:TextBox MaxLength="1" Visible="false" Width="40px" ID="viewedTextBox" runat="server" Text='<%# Bind("viewed") %>'>
                </asp:TextBox>
                <asp:CheckBox ID="insertcheckbox" Checked='<%# Bind("viewed") %>' runat="server" />
            Viewed</b></td>
            </tr>
            </table>
            <table>
            <tr>
            <td><b>Note Title:</b></td>
            <td><b><asp:TextBox ID="note_titleTextBox" runat="server" Text='<%# Bind("note_title") %>'>
                </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b></b></td>
            <td><b><asp:TextBox ID="note_textTextBox" TextMode="multiline" Height="200px" Width="400px" runat="server" Text='<%# Bind("note_text") %>'>
                </asp:TextBox></b></td>
            </tr>
            </table>
            
               
                <asp:Button ID="InsertButton" runat="server" CssClass="buttonM" CausesValidation="True" CommandName="Insert"
                    Text="Save" OnClick="InsertButton_Click">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
                
               
            </InsertItemTemplate>
            <ItemTemplate>
            
            <table>
            <tr>
            <td><b>Note Date:</b></td>
            <td><b><asp:Label BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ID="note_dateLabel" runat="server" Text='<%# Bind("note_date","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
            <td><b>Note Time:</b></td>
            <td><b><asp:Label BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ID="note_timeLabel" runat="server" Text='<%# Bind("note_time") %>'></asp:Label></b></td>
            <td><b>UserId:</b></td>
            <td><b><asp:Label BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" ID="user_idLabel" runat="server" Text='<%# Bind("user_id") %>'></asp:Label></b></td>
            <td><b><asp:Label BackColor="Turquoise" Width="40px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Visible="false" ID="viewedLabel" runat="server" Text='<%# Bind("viewed") %>'></asp:Label>
            <asp:CheckBox ID="viewcheckbox" Checked='<%# Bind("viewed") %>' Enabled="false" runat="server" />
            Viewed</b></td>
            </tr>
            </table>
            <table>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Note Title:</b></td>
            <td><b><asp:Label ID="note_titleLabel" BackColor="Turquoise" Width="150px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("note_title") %>'>
                </asp:Label></b></td>
            </tr>
            <tr>
            <td><b></b></td>
            <td><b><asp:TextBox ReadOnly="true" TextMode="multiline" Height="200px" Width="400px" ID="note_textLabel" runat="server" Text='<%# Bind("note_text") %>'></asp:TextBox></b></td>
            </tr>
            <tr><td></td></tr>
            <tr><td></td></tr>
            <tr><td>
                <asp:Button ID="AddButton" runat="server" Text="Add" CssClass="buttonM" CommandName="new" />
                <asp:Button ID="UpdateButton" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" />
                
                <asp:Button ID="DeleteButton" runat="server" Text="Delete" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')" CssClass="buttonM" />
            </td></tr>
            </table>
            
               
            </ItemTemplate>
        </asp:FormView>
    </fieldset>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
