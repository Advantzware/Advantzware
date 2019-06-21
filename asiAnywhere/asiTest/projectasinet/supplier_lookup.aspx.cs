
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class supplier_lookup : System.Web.UI.Page
{

    private int iDefaultRecordsPerPage = 20;

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        //UserClass.CheckLogin(Page);


        //if (!func.CheckUserPermissions("[dbo].[comp_suppliers]", "SA"))
        //{
        //    Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
        //    Response.End();
        //}
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        dbGrid_comp_suppliers.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
            if (Session["User"] != null)
            {
               
            }

            //if (func.CheckUserPermissions("[dbo].[comp_suppliers]", "s"))

                BuildDataSource();
        } //  ! Page.IsPostBack


        
        
    }

    private void BuildDataSource()
    {
        try
        {
            string sSqlWhere = "";
            comp_suppliersSqlDataSource.SelectParameters.Clear();

            if (Session["dbGrid_comp_suppliers_SearchSQL"] != null)
                sSqlWhere = func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_comp_suppliers_SearchSQL"].ToString() + ")", "And");
            if (Session["dbGrid_comp_suppliers_SearchParams"] != null)
            {
                ParameterCollection Params = (ParameterCollection)Session["dbGrid_comp_suppliers_SearchParams"];
                foreach (Parameter p in Params)
                {
                    txtSearchValue.Text = p.DefaultValue;
                    comp_suppliersSqlDataSource.SelectParameters.Add(p);
                }
            }

            if (func.GetsAdvSecurityMethod("[dbo].[comp_suppliers]") == "1" && (!func.IsAdminUser()))
            {
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[comp_suppliers]");
                string userId = ((UserClass)Session["User"]).UserID;

                if (sOwnerIDField != string.Empty && userId != string.Empty)
                {
                    sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));
                    comp_suppliersSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
                }
                else
                {
                    return;
                }
            }

            if (sSqlWhere != string.Empty) comp_suppliersSqlDataSource.SelectCommand = func.SqlBuilder(comp_suppliersSqlDataSource.SelectCommand, sSqlWhere);

            dbGrid_comp_suppliers.DataBind();

            if (Session["dbGrid_comp_suppliers_SortExpression"] != null)
            {
                bSort = false;
                dbGrid_comp_suppliers.Sort((string)Session["dbGrid_comp_suppliers_SortExpression"], (SortDirection)Session["dbGrid_comp_suppliers_SortDirection"]);
                bSort = true;
            }

            if (Session["dbGrid_comp_suppliers_CurrentPageCount"] != null)
            {
                //ddlPagerCount.SelectedValue = (string)Session["dbGrid_comp_suppliers_CurrentPageCount"];
                //dbGrid_comp_suppliers.PageSize = Convert.ToInt32(ddlPagerCount.SelectedValue);
            }
            else
            {
               
                dbGrid_comp_suppliers.PageSize = iDefaultRecordsPerPage;
            }

            int iCurrentPageIndex = (Session["dbGrid_comp_suppliers_CurrentPageIndex"] == null) ? 0 : Convert.ToInt32(Session["dbGrid_comp_suppliers_CurrentPageIndex"]);
            if (dbGrid_comp_suppliers.PageCount >= iCurrentPageIndex)
                dbGrid_comp_suppliers.PageIndex = iCurrentPageIndex;
            else
            {
                dbGrid_comp_suppliers.PageIndex = 0;
                Session["dbGrid_comp_suppliers_CurrentPageIndex"] = 0;
            }
        }
        catch (Exception ex)
        {
            //lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
            dbGrid_comp_suppliers.Visible = false;
        }
        finally
        {
/*#if(DEBUG)
{

    //lblMessage.Text += "<p>SQL = " + comp_suppliersSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in comp_suppliersSqlDataSource.SelectParameters)
       // lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
        }
    }

    protected void dbGrid_comp_suppliers_RowCreated(object sender, GridViewRowEventArgs e)
    {

        if (e.Row.RowType == DataControlRowType.Header) func.AddGlyph(dbGrid_comp_suppliers, e.Row);

        if (e.Row.RowType == DataControlRowType.DataRow)
        {

            bool bCheckSecurityD = func.CheckUserPermissions("[dbo].[comp_suppliers]", "d");

            bool bCheckSecurityE = func.CheckUserPermissions("[dbo].[comp_suppliers]", "e");

            try
            {
                DataRowView rowData;
                rowData = (DataRowView)e.Row.DataItem;
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[comp_suppliers]");
                if (!(sOwnerIDField == "" || (rowData == null)))
                {
                    string sData = (rowData.Row[sOwnerIDField] == System.DBNull.Value) ? "" : Convert.ToString(rowData.Row[sOwnerIDField]);

                    bCheckSecurityD = func.CheckSecurity("[dbo].[comp_suppliers]", "d", sData);

                    bCheckSecurityE = func.CheckSecurity("[dbo].[comp_suppliers]", "e", sData);

                }
            }
            catch
            {

                bCheckSecurityD = false;

                bCheckSecurityE = false;

            }

            if (!bCheckSecurityD) e.Row.Cells[0].Text = "";

           
        }

    }

    protected void dbGrid_comp_suppliers_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;

        }

    }

    protected void dbGrid_comp_suppliers_RowCommand(object source, GridViewCommandEventArgs e)
    {
        if (e.CommandName == "Page") return;
        if (e.CommandName == "Sort") return;

        int index = Convert.ToInt32(e.CommandArgument);
        DataKey dkKeys = dbGrid_comp_suppliers.DataKeys[index];

        string sKeysArg = "";
        foreach (string s in dkKeys.Values.Keys)
            sKeysArg += s + "=" + Convert.ToString(dkKeys[s]) + "&";
        if (sKeysArg == String.Empty) return;

        if (e.CommandName == "cmdEdit") Response.Redirect("comp_suppliers_Edit.aspx?" + sKeysArg);

    }

    protected void dbGrid_comp_suppliers_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_comp_suppliers_PageIndexChanged(object source, EventArgs e)
    {
        Session["dbGrid_comp_suppliers_CurrentPageIndex"] = dbGrid_comp_suppliers.PageIndex;
        BuildDataSource();

    }

    protected void dbGrid_comp_suppliers_Sorted(object sender, EventArgs e)
    {
        Session["dbGrid_comp_suppliers_SortExpression"] = null;
        if (bSort) BuildDataSource();
        Session["dbGrid_comp_suppliers_SortExpression"] = dbGrid_comp_suppliers.SortExpression;
        Session["dbGrid_comp_suppliers_SortDirection"] = dbGrid_comp_suppliers.SortDirection;
    }

    
    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txtSearchValue.Text = "";
        ddlSearchOperation.SelectedIndex = 0;
        ddlSearchField.SelectedIndex = 0;

        Session["dbGrid_comp_suppliers_CurrentPageIndex"] = 0;
        Session["dbGrid_comp_suppliers_SearchSQL"] = null;
        Session["dbGrid_comp_suppliers_SearchParams"] = null;
        Session["dbGrid_comp_suppliers_AdvSearch"] = null;
        Session["dbGrid_comp_suppliers_AdvParam"] = null;
        Session["htPeramcomp_suppliers"] = null;

        Session["htPeramcomp_suppliers"] = null;

        Session["comp_supplier_index"] = null;
        BuildDataSource();
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        if (txtSearchValue.Text.Trim() == String.Empty && ddlSearchOperation.SelectedValue.ToString() != "IsNull") return;

        string sSqlWhere = "";
        ParameterCollection Params = new ParameterCollection();

        if (ddlSearchField.SelectedValue.ToString() == "Any Field")
        {

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("code", "Searchcode", Params), "Or");

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("name", "Searchname", Params), "Or");

        }
        else
        {
            sSqlWhere = WhereOneField(ddlSearchField.SelectedValue.ToString(), func.BuildParameterName("Search" + ddlSearchField.SelectedValue.ToString()), Params);
        }

        if (sSqlWhere.Trim() != "")
        {
            Session["dbGrid_comp_suppliers_SearchSQL"] = sSqlWhere;
            Session["dbGrid_comp_suppliers_SearchParams"] = Params;
        }
        else
        {
            Session["dbGrid_comp_suppliers_SearchSQL"] = "2<>2";
            Session["dbGrid_comp_suppliers_SearchParams"] = null;
        }
        Session["dbGrid_comp_suppliers_CurrentPageIndex"] = 0;

        Session["comp_supplier_index"] = null;
        BuildDataSource();

    }

    private string WhereOneField(string sSearchField, string sParamName, ParameterCollection Params)
    {
        string sSearchOperation = ddlSearchOperation.SelectedValue;
        string sValue = txtSearchValue.Text.TrimEnd();
        string sReturn = "";
        string sSearchType = "";
        TypeCode fieldType = GetFieldType(sSearchField);

        sSearchField = "[" + sSearchField + "]";

        if (sSearchOperation == "IsNull") return sSearchField + " is null";

        try { object o = Convert.ChangeType(sValue, fieldType); }
        catch { return String.Empty; }

        if (!(func.IsDate(sValue) || func.IsNumeric(sValue)))
        {

            sSearchType = "upper";

            sValue = sValue.ToUpper();
        }

        switch (sSearchOperation)
        {
            case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'"; break;
            case "Starts with ...":
                sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'"; break;
            case "Equals":
                sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName; break;
            case "More than ...":
                sReturn = sSearchField + ">@" + sParamName; break;
            case "Less than ...":
                sReturn = sSearchField + "<@" + sParamName; break;
            case "Equal or more than ...":
                sReturn = sSearchField + ">=@" + sParamName; break;
            case "Equal or less than ...":
                sReturn = sSearchField + "<=@" + sParamName; break;
            default:
                sReturn = String.Empty; break;
        }

        if (sReturn != string.Empty && (sSearchOperation != "Contains" && sSearchOperation != "Starts with ..."))
        {
            if (func.IsNumeric(sValue)) Params.Add(sParamName, sValue);
            else Params.Add(sParamName, fieldType, sValue);
        }
        return sReturn;
    }

    protected TypeCode GetFieldType(string sField)
    {
        switch (sField)
        {

            case "code": return TypeCode.String;

            case "name": return TypeCode.String;

            default: return TypeCode.String;
        }
    }

   
    protected void comp_suppliersSqlDataSource_Deleting(object sender, SqlDataSourceCommandEventArgs e)
    {

    }


    private void ClearSession()
    {
        Session["dbGrid_comp_suppliers_Sort"] = null;

        Session["dbGrid_comp_suppliers_SearchSQL"] = null;
        Session["dbGrid_comp_suppliers_SearchParams"] = null;

        Session["htPeramcomp_suppliers"] = null;
        Session["dbGrid_comp_suppliers_CurrentPageIndex"] = null;
        Session["dbGrid_comp_suppliers_CurrentPageCount"] = null;

        Session["dbGrid_comp_suppliers_SortExpression"] = null;
        Session["dbGrid_comp_suppliers_SortDirection"] = null;
    }

    protected void ShowWait()
    {
        Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
        Response.Write("<script>mydiv.innerText = '';</script>");
        Response.Write("<script language=javascript>;");
        Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
        Response.Write("{var output; output = '" + "Please wait" + "';dots++;if(dots>=dotmax)dots=1;");
        Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
        Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
        Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
        Response.Write("StartShowWait();</script>");
        Response.Flush();
    }

    protected void comp_suppliersSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
    {
        //lblCount.Text = "Details found" + ": " + e.AffectedRows.ToString();
    }

   
   
    protected void dbGrid_comp_suppliers_SelectedIndexChanged(object sender, EventArgs e)
    {
        //Session["comp_supplier_index"] = dbGrid_comp_suppliers.SelectedIndex;
        //Session["comp_supplier_code"] = dbGrid_comp_suppliers.SelectedRow.Cells[3].Text;
    }
    
    
    
}
