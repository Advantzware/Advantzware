<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="Cmain_menu_Edit" Codebehind="main_menu_edit.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Menu Maintenance - Edit</title>
    <script language="JavaScript" src="include/calendar.js"></script>
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
    <style>
      .visible { VISIBILITY: visible }
      .hide { VISIBILITY: hidden }        
    </style>    
</head>

<body>
    <form id="editform" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
     <div style="padding-right:300px">
        <asp:label id="lblUpdate" runat="server" Font-Bold="True">Table:&nbsp;Menu Maintenance,&nbsp;Edit record</asp:label>
        <hr width="100%" SIZE="1">
       
        <asp:hyperlink id="lnkBackToList" runat="server" NavigateUrl="main_menu_list.aspx">Back to list</asp:hyperlink>
        <p><asp:label id="lblMessage" runat="server" CssClass="message"></asp:label><asp:validationsummary id="vsUpdatePage" runat="server" ShowMessageBox="True" ShowSummary="False" Height="32px"></asp:validationsummary>
        <asp:SqlDataSource id="main_menuSqlDataSource" runat="server" 
            SelectCommand="select [group_id],  [parent],  [menu_label],  [destination],  [security],  [description],  [menu_id],  [order_num],  [allowedidtypes],  [long_desc],  [target_page] from [dbo].[main_menu] where [menu_id]=@menu_id" 
            UpdateCommand="update [dbo].[main_menu] set [menu_label]=@menu_label, [order_num]=@order_num, [target_page]=@target_page, [parent]=@parent, [security]=@security, [description]=@description, [long_desc]=@long_desc, [group_id]=@group_id, [allowedidtypes]=@allowedidtypes, [destination]=@destination where  [menu_id]=@menu_id"
            OnSelected="main_menuSqlDataSource_Selected" 
            OnUpdated="main_menuSqlDataSource_Updated"
            OnUpdating="main_menuSqlDataSource_Updating"
            ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
            ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
        >
        <SelectParameters>
            <asp:QueryStringParameter  QueryStringField="menu_id" Name="menu_id" Type="Int32" DefaultValue="-1" />

        </SelectParameters>       
        <UpdateParameters>
            <asp:Parameter Name="menu_label" Type="String"/>
            <asp:Parameter Name="order_num" Type="Int32"/>
            <asp:Parameter Name="target_page" Type="String"/>
            <asp:Parameter Name="parent" Type="String"/>
            <asp:Parameter Name="security" Type="String"/>
            <asp:Parameter Name="description" Type="String"/>
            <asp:Parameter Name="long_desc" Type="String"/>
            <asp:Parameter Name="group_id" Type="String"/>
            <asp:Parameter Name="allowedidtypes" Type="String"/>
            <asp:Parameter Name="destination" Type="String"/>

            <asp:QueryStringParameter  QueryStringField="menu_id" Name="menu_id" Type="Int32" DefaultValue="-1" />

        </UpdateParameters>
    </asp:SqlDataSource>
    <asp:DetailsView ID="main_menuDetailsView" runat="server" AutoGenerateRows="False" 
      DataKeyNames="menu_id"
            DataSourceID="main_menuSqlDataSource"
            OnDataBound="main_menuDetailsView_DataBound" OnItemUpdating="main_menuDetailsView_ItemUpdating"
            
            DefaultMode="Edit" 
            GridLines="None" 
            RowStyle-CssClass=""
    >          
        <FieldHeaderStyle CssClass="shade" />
        <Fields>

            <asp:TemplateField HeaderText="Menu ID" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldmenu_id"    ReadOnly=True Text='<%# Bind("menu_id") %>' runat="server"/>&nbsp;

                    <img alt="Key field" src="images/icon_key.gif">

                    <asp:RegularExpressionValidator runat="server"
                     ErrorMessage="Menu ID: Must be a Numeric value" Display="None"
                     ControlToValidate="fldmenu_id"
                     ValidationExpression="^\s*([-\+])?(\d+)?([\.\,](\d+))?\s*$" ID="revmenu_id" />

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Menu Label" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldmenu_label"  size="30"   Text='<%# Bind("menu_label") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Order#" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldorder_num"  size="30"  Text='<%# Bind("order_num") %>' runat="server"/>&nbsp;

                    <asp:RegularExpressionValidator runat="server"
                     ErrorMessage="Order#: Must be a Numeric value" Display="None"
                     ControlToValidate="fldorder_num"
                     ValidationExpression="^\s*([-\+])?(\d+)?([\.\,](\d+))?\s*$" ID="revorder_num" />

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Target Page" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldtarget_page"  size="30"   Text='<%# Bind("target_page") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Parent" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:DropDownList id="fldparent" Width="180px"
            DataTextField="menu_label" DataValueField="menu_id" SelectedValue='<%# Bind("parent") %>'
            DataSource=<%# func.GetSqlDataSource("SELECT [menu_id], [menu_label] FROM [dbo].[main_menu]   " + " where " + "main_menu.target_page = 'menuhead'" + " ORDER BY [menu_label]") %>
            AppendDataBoundItems="True" runat="server"  >
              <asp:ListItem Value="top">TOP</asp:ListItem>
          </asp:DropDownList>

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Security Group" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldsecurity"  size="30"   Text='<%# Bind("security") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Description" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="flddescription"  size="30"   Text='<%# Bind("description") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Long Desc" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldlong_desc"  size="30"   Text='<%# Bind("long_desc") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Group ID" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldgroup_id"  size="30"  Text='<%# Bind("group_id") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Allowed ID Types" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="fldallowedidtypes" size="30"   Text='<%# Bind("allowedidtypes") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Destination" HeaderStyle-Font-Bold="true">
                <EditItemTemplate>
                    <asp:TextBox id="flddestination"  size="30"   Text='<%# Bind("destination") %>' runat="server"/>&nbsp;

                </EditItemTemplate>
            </asp:TemplateField>

<asp:TemplateField>
<EditItemTemplate>
<asp:Button ID="UpdateButton" runat="server" Text="Save" CommandName="Update" CssClass="buttonM" />
&nbsp;&nbsp;&nbsp;&nbsp;
<asp:Button ID="CancelButton" runat="server" Text="Cancel" CommandName="Cancel" CssClass="buttonM" OnClick="CancelButton_Click" />
</EditItemTemplate>
</asp:TemplateField>
        </Fields>
    </asp:DetailsView></div>
     <ft:footer id="Footer1" runat="server"></ft:footer>      
    </form>
</body>
</html>