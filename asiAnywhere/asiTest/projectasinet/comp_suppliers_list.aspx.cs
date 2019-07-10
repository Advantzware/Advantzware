
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Ccomp_suppliers_list : System.Web.UI.Page
{

    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";
    //private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        //UserClass.CheckLogin(Page);
        Session["list_notes_date"] = null;
        Session["list_notes_time"] = null;
        Session["list_notes_index"] = null;
        Session["contact_list_first_name"] = null;
        Session["contact_list_last_name"] = null;
        Session["view_contact_list"] = null;
        Session["view_comp_supplier"] = 1;

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "comp_suppliers_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        dbGrid_comp_suppliers.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }


            BuildDataSource();
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            dbGrid_comp_suppliers.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }

        dbGrid_comp_suppliers.SelectedIndex = Convert.ToInt32(Session["comp_supplier_index"]);
        try
        {
            if (Session["comp_supplier_index"] == null)
            {
                dbGrid_comp_suppliers.SelectedIndex = 0;
                Session["comp_supplier_code"] = dbGrid_comp_suppliers.SelectedRow.Cells[2].Text;

            }
            foreach (GridViewRow gv in dbGrid_comp_suppliers.Rows)
            {
                Session["contact_rec_key"] = ((Label)dbGrid_comp_suppliers.SelectedRow.FindControl("Label1")).Text;
            }


        }
        catch
        {
            return;
        }

    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select comp_code as 'Suppliers Code', name as 'Suppliers Name', address1 as 'Address',address2 as 'Address',city as 'City',state as 'State',zip as 'Zip', rec_key from comp_suppliers";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_comp_suppliers.DataSource = ds;
            dbGrid_comp_suppliers.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            conn.Close();
        }
    }
    protected void dbGrid_comp_suppliers_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            e.Row.Cells[9].Visible = false;
        }
        catch { return; }
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
        //if (e.CommandName == "Page") return;
        //if (e.CommandName == "Sort") return;

        //int index = Convert.ToInt32(e.CommandArgument);
        //DataKey dkKeys = dbGrid_comp_suppliers.DataKeys[index];

        //string sKeysArg = "";
        //foreach (string s in dkKeys.Values.Keys)
        //    sKeysArg += s + "=" + Convert.ToString(dkKeys[s]) + "&";
        //if (sKeysArg == String.Empty) return;

        //if (e.CommandName == "cmdEdit") Response.Redirect("comp_suppliers_Edit.aspx?" + sKeysArg);

    }

    protected void dbGrid_comp_suppliers_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_comp_suppliers_PageIndexChanged(object source, EventArgs e)
    {
        //Session["dbGrid_comp_suppliers_CurrentPageIndex"] = dbGrid_comp_suppliers.PageIndex;
        //BuildDataSource();

    }

    protected void dbGrid_comp_suppliers_Sorted(object sender, EventArgs e)
    {
        //Session["dbGrid_comp_suppliers_SortExpression"] = null;
        //if (bSort) BuildDataSource();
        //Session["dbGrid_comp_suppliers_SortExpression"] = dbGrid_comp_suppliers.SortExpression;
        //Session["dbGrid_comp_suppliers_SortDirection"] = dbGrid_comp_suppliers.SortDirection;
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }



    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txt_supplierscode.Text = "";
        txt_suppliersname.Text = "";


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
    protected void dbGrid_comp_suppliers_PageIndexChanging(object source, GridViewPageEventArgs e)
    {
        dbGrid_comp_suppliers.PageIndex = e.NewPageIndex;
        BuildDataSource();

    }


    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "";
            if (txt_supplierscode.Text != "")
            {
                cmd = "select comp_code as 'Suppliers Code', name as 'Suppliers Name' , address1 as 'Address', address2 as 'Address', city as 'City', state as 'State' ,zip as 'Zip', rec_key  from comp_suppliers where comp_code LIKE '" + txt_supplierscode.Text + "%'";
            }
            if (txt_suppliersname.Text != "")
            {
                cmd = "select comp_code as 'Suppliers Code', name as 'Suppliers Name' , address1 as 'Address', address2 as 'Address', city as 'City', state as 'State' ,zip as 'Zip', rec_key  from comp_suppliers where name LIKE  '" + txt_suppliersname.Text + "%'";
            }

            if (txt_suppliersname.Text != "" && txt_supplierscode.Text != "")
            {
                cmd = "select comp_code as 'Suppliers Code', name  as 'Suppliers Name' , address1 as 'Address', address2 as 'Address', city as 'City', state as 'State' ,zip as 'Zip', rec_key  from comp_suppliers where name LIKE '" + txt_suppliersname.Text + "%' and comp_code LIKE '" + txt_supplierscode.Text + "%'";
            }

            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_comp_suppliers.DataSource = ds;
            dbGrid_comp_suppliers.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            //dbGrid_comp_suppliers.PageSize = Convert.ToInt32(Session["size"]);
            conn.Close();
        }



    }


    protected void btnAdd_Click(object sender, EventArgs e)
    {

        //Response.Redirect("comp_suppliers_add.aspx");

    }

    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        ClearSession();
        Response.Redirect(sMenuURL);
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

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
       

    protected void dbGrid_comp_suppliers_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["comp_supplier_index"] = dbGrid_comp_suppliers.SelectedIndex;


        Session["comp_supplier_code"] = dbGrid_comp_suppliers.SelectedRow.Cells[2].Text;
        Session["contact_rec_key"] = ((Label)dbGrid_comp_suppliers.SelectedRow.FindControl("Label1")).Text;


    }


    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        BuildDataSource();

    }



    protected void lnk_listsupplier_Click(object sender, EventArgs e)
    {

        Response.Redirect("comp_suppliers_list.aspx");

    }
    protected void lnk_viewsupplier_Click(object sender, EventArgs e)
    {

        Response.Redirect("comp_suppliers_viewlist.aspx");
    }

    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    public SortDirection GridViewSortDirection
    {

        get
        {

            if (ViewState["sortDirection"] == null)

                ViewState["sortDirection"] = SortDirection.Ascending;

            return (SortDirection)ViewState["sortDirection"];

        }

        set { ViewState["sortDirection"] = value; }

    }
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
    {

        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {

            GridViewSortDirection = SortDirection.Descending;

            SortGridView(sortExpression, " DESC");

        }

        else
        {

            GridViewSortDirection = SortDirection.Ascending;

            SortGridView(sortExpression, " ASC");

        }

    }

    private void SortGridView(string sortExpression, string direction)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select comp_code as 'Suppliers Code', name as 'Suppliers Name', address1 as 'Address', address2 as 'Address', city as 'City', state as 'State' ,zip as 'Zip' , rec_key from comp_suppliers";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_comp_suppliers.DataSource = ds;
            dbGrid_comp_suppliers.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_comp_suppliers.DataSource = dv;

            dbGrid_comp_suppliers.DataBind();


            conn.Close();

        }



        catch { return; }

    }


}
