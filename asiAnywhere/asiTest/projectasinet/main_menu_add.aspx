<%@ Page Language="c#" Debug="false" AutoEventWireup="true" Inherits="Cmain_menu_Add" Codebehind="main_menu_Add.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Menu Maintenance - Add new</title>
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
        <asp:label id="lblInsert" runat="server" Font-Bold="True">Table:&nbsp;Menu Maintenance,&nbsp;Add new record</asp:label>
        <hr width="100%" SIZE="1">
        <asp:hyperlink id="lnkBackToList" runat="server" NavigateUrl="main_menu_list.aspx">Back to list</asp:hyperlink>
        <p><asp:label id="lblMessage" runat="server" CssClass="message"></asp:label><asp:validationsummary id="vsUpdatePage" runat="server" ShowMessageBox="True" ShowSummary="False" Height="32px"></asp:validationsummary>    
        <asp:SqlDataSource id="main_menuSqlDataSource" runat="server" 
        
        InsertCommand="insert into [dbo].[main_menu] ([menu_label], [order_num], [target_page], [parent], [security], [description], [long_desc], [group_id], [allowedidtypes], [destination], [menu_id]) values (@menu_label, @order_num, @target_page, @parent, @security, @description, @long_desc, @group_id, @allowedidtypes, @destination, @menu_id ) "
        
    OnInserted="main_menuSqlDataSource_Inserted"
    OnInserting="main_menuSqlDataSource_Inserting"
        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
        ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
    >
        <SelectParameters>
            <asp:QueryStringParameter  QueryStringField="menu_id" Name="menu_id" Type="Int32" DefaultValue="-1" />

        </SelectParameters>       
        <InsertParameters>
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
            <asp:Parameter Name="menu_id"  Type="int32" />
            
        </InsertParameters>
    </asp:SqlDataSource>
    
    
     <asp:SqlDataSource id="SqlDataSource1" runat="server" 
        SelectCommand="select [menu_id2]= (select max(menu_id) from [dbo].[main_menu]) from [dbo].[main_menu]"  
    
        ConnectionString="<%$ ConnectionStrings:Project1ConnectionString%>"
        ProviderName="<%$ ConnectionStrings:Project1ConnectionString.providerName%>"
    >
        <SelectParameters>
            <asp:QueryStringParameter  QueryStringField="menu_id" Name="menu_id" Type="Int32" DefaultValue="-1" />

        </SelectParameters>       
        
    </asp:SqlDataSource>
    <asp:DetailsView  id="main_menuDetailsView" runat="server" AutoGenerateRows="False" 
      DataKeyNames="menu_id"
            DataSourceID="main_menuSqlDataSource"
            OnDataBound="main_menuDetailsView_DataBound" OnItemInserting="main_menuDetailsView_ItemInserting"
           
            DefaultMode="Insert" 
            GridLines="None" 
            RowStyle-CssClass=""
    >          
      <FieldHeaderStyle CssClass="shade" />
        <Fields>
            
            <asp:TemplateField HeaderText="Menu Label">
                <InsertItemTemplate>
                    <asp:TextBox id="fldmenu_label"  size="30"  Text='<%# Bind("menu_label") %>' runat="server" ></asp:textbox>&nbsp;
                    
                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Order#">
                <InsertItemTemplate>
                    <asp:TextBox id="fldorder_num" size="30"  Text='<%# Bind("order_num") %>' runat="server" ></asp:textbox>&nbsp;

                    <asp:RegularExpressionValidator runat="server"
                     ErrorMessage="Order#: Must be a Numeric value" Display="None"
                     ControlToValidate="fldorder_num"
                     ValidationExpression="^\s*([-\+])?(\d+)?([\.\,](\d+))?\s*$" ID="revorder_num" />

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Target Page">
                <InsertItemTemplate>
                    <asp:TextBox id="fldtarget_page"  size="30"  Text='<%# Bind("target_page") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Parent">
                <InsertItemTemplate>
                    <asp:DropDownList id="fldparent" Width="180px" 
                  DataTextField="menu_label" DataValueField="menu_id" SelectedValue='<%# Bind("parent") %>'
                  DataSource=<%# func.GetSqlDataSource("SELECT [menu_id], [menu_label] FROM [dbo].[main_menu]   " + " where " + "main_menu.target_page = 'menuhead'" + " ORDER BY [menu_label]") %>
                  AppendDataBoundItems="True" runat="server">            
	    <asp:ListItem Value="top">TOP</asp:ListItem>
          </asp:DropDownList>

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>
            
         

            <asp:TemplateField HeaderText="Security Group">
                <InsertItemTemplate>
                    <asp:TextBox id="fldsecurity"  size="30"  Text='<%# Bind("security") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Description">
                <InsertItemTemplate>
                    <asp:TextBox id="flddescription"  size="30"  Text='<%# Bind("description") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Long Desc">
                <InsertItemTemplate>
                    <asp:TextBox id="fldlong_desc"  size="30"  Text='<%# Bind("long_desc") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Group ID">
                <InsertItemTemplate>
                    <asp:TextBox id="fldgroup_id"  size="30"  Text='<%# Bind("group_id") %>' runat="server" Value="1"></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Allowed ID Types">
                <InsertItemTemplate>
                    <asp:TextBox id="fldallowedidtypes"  size="30" Text='<%# Bind("allowedidtypes") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>

            <asp:TemplateField HeaderText="Destination">
                <InsertItemTemplate>
                    <asp:TextBox id="flddestination"  size="30"  Text='<%# Bind("destination") %>' runat="server" ></asp:textbox>&nbsp;

                </InsertItemTemplate>
                <HeaderStyle Font-Bold="True" />
            </asp:TemplateField>
            
                    
            
<asp:TemplateField>
<InsertItemTemplate>
<asp:Button ID="InsertButton" runat="server" Text="Save" CommandName="Insert" CssClass="buttonM" />
&nbsp;&nbsp;&nbsp;&nbsp;
<asp:Button ID="CancelButton" runat="server" Text="Cancel" CommandName="Cancel" CssClass="buttonM" OnClick="CancelButton_Click" />&nbsp;
 <asp:DropDownList  Visible="false" ID="DropDownList1" runat="server" DataSourceID="SqlDataSource1" DataTextField="menu_id2" DataValueField="menu_id2">
                    </asp:DropDownList>
                <asp:TextBox  Visible="false" ID="TextBox1" Text='<%# Bind("menu_id") %>' runat="server"></asp:TextBox>  
</InsertItemTemplate>
</asp:TemplateField>
        </Fields>
    </asp:DetailsView> 
    
    
    
    </div>
     <ft:footer id="Footer1" runat="server"></ft:footer>         
    </form>
</body>
</html>