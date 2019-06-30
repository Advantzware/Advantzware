
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class Cmain_menu_print : System.Web.UI.Page 
{
  
    private int iDefaultRecordsPerPage = 100;

protected void Page_Load( object sender,  System.EventArgs e)  
{

    UserClass.CheckLogin(Page);

    if (! (func.CheckUserPermissions("[dbo].[main_menu]", "S") && func.CheckUserPermissions("[dbo].[main_menu]", "P")) ) 
    {
      Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
      Response.End();
    }  

    lblMessage.Text = "";
    dbGrid_main_menu.Visible = true;

    string sCulture = ConfigurationManager.AppSettings["LCID"];
    if (!String.IsNullOrEmpty(sCulture)) 
    {
        int nCulture = int.Parse(sCulture);
        System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
    } 
               
    if (! Page.IsPostBack )
    {    
        BuildDataSource();

    } //  ! Page.IsPostBack

}

private void BuildDataSource()
{
    try
    {
        string sSqlWhere = "";
        main_menuSqlDataSource.SelectParameters.Clear();
    
        if (Session["dbGrid_main_menu_SearchSQL"] != null) 
            sSqlWhere =  func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_main_menu_SearchSQL"].ToString() + ")","And");
        if (Session["dbGrid_main_menu_SearchParams"] != null) 
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_SearchParams"];
            foreach (Parameter p in Params) 
            {
                main_menuSqlDataSource.SelectParameters.Add(p);
            }
        }               

        if (Session["dbGrid_main_menu_AdvSearch"] != null)
            sSqlWhere = func.WhereBuilder( sSqlWhere , Session["dbGrid_main_menu_AdvSearch"].ToString());
        if (Session["dbGrid_main_menu_AdvParam"] != null)
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_AdvParam"];
            foreach (Parameter p in Params) main_menuSqlDataSource.SelectParameters.Add(p);
        }    

        if ( func.GetsAdvSecurityMethod("[dbo].[main_menu]") == "1" && (! func.IsAdminUser()))
        {
            string sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
            if (sOwnerIDField != string.Empty)
            {
                sSqlWhere = func.WhereBuilder(sSqlWhere, sOwnerIDField + "=@" + func.BuildParameterName(sOwnerIDField));            
                main_menuSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), ((UserClass)Session["User"]).UserID);
            }
        }

    if(sSqlWhere != string.Empty) main_menuSqlDataSource.SelectCommand = func.SqlBuilder(main_menuSqlDataSource.SelectCommand, sSqlWhere);
        
        dbGrid_main_menu.DataBind();

        if (Session["dbGrid_main_menu_SortExpression"] != null)
        {
            dbGrid_main_menu.Sort((string)Session["dbGrid_main_menu_SortExpression"], (SortDirection)Session["dbGrid_main_menu_SortDirection"]);
        }
        
        if (Session["dbGrid_main_menu_CurrentPageCount"] != null )
        {
             dbGrid_main_menu.PageSize = Convert.ToInt32(Session["dbGrid_main_menu_CurrentPageCount"]);
        } 
        else
        {
            dbGrid_main_menu.PageSize = iDefaultRecordsPerPage;
        }

        int iCurrentPageIndex = (Session["dbGrid_main_menu_CurrentPageIndex"] == null)?0:Convert.ToInt32(Session["dbGrid_main_menu_CurrentPageIndex"]);
        if (dbGrid_main_menu.PageCount >= iCurrentPageIndex)
            dbGrid_main_menu.PageIndex = iCurrentPageIndex;
        else 
        {
            dbGrid_main_menu.PageIndex = 0;
            Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
        }
    }
    catch (Exception ex)
    {
        lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        dbGrid_main_menu.Visible = false;
    }  
    finally
    {
/*#if(DEBUG)
{

    lblMessage.Text += "<p>SQL = " + main_menuSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in main_menuSqlDataSource.SelectParameters)
        lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";
}
#endif*/
    }
}

protected void dbGrid_main_menu_RowDataBound(object sender,  GridViewRowEventArgs e)
{
    if (e.Row.RowType == DataControlRowType.DataRow)
    {

        DataRowView rowData;

        rowData = (DataRowView)e.Row.DataItem;

        if (rowData["parent"] != System.DBNull.Value) e.Row.Cells[4].Text = func.GetLookupValue("[menu_label]", "[menu_id]", "[dbo].[main_menu]", Convert.ToString(rowData["parent"]), TypeCode.String);

    }

}    

protected TypeCode GetFieldType(string  sField) 
{
    switch (sField)
    {

        case "description": return TypeCode.String;

        case "menu_id": return TypeCode.Int32;

        case "group_id": return TypeCode.String;

        case "menu_label": return TypeCode.String;

        case "long_desc": return TypeCode.String;

        case "order_num": return TypeCode.Int32;

        case "allowedidtypes": return TypeCode.String;

        case "target_page": return TypeCode.String;

        case "parent": return TypeCode.String;

        case "destination": return TypeCode.String;

        case "security": return TypeCode.String;

        default: return TypeCode.String;
    }    
}

}
