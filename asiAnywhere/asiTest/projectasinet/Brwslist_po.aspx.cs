using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Data.SqlClient;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class Brwslist_po : System.Web.UI.Page
{

    public Brwslist_po()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    string raw_url;
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";
        
    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
        try
        {
            Session["pur_ord_po"] = ((Label)GridView2.SelectedRow.FindControl("Label4")).Text;
            Session["pur_ord_po_line_no"] = ((Label)GridView2.SelectedRow.FindControl("Label3")).Text;
            Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckeyvalue")).Text;
            Session["item"] = ((Label)GridView2.SelectedRow.FindControl("item_label")).Text;
            
        }
        catch { }
        Session["order_entry_est_no"] = null;
    }
            

    protected void Page_Load(object sender, EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;
   
        if (!Page.IsPostBack)
        {
            if (Session["prmPono_po"] != null)
                txt_po.Text = Convert.ToString(Session["prmPono_po"]);
            else
                txt_po.Text = "";
            if (Session["prmVendor_po"] != null)
                txt_ven.Text = Convert.ToString(Session["prmVendor_po"]);
            else
                txt_ven.Text = "";
            if (Session["prmFgItem_po"] != null)
                txt_ino.Text = Convert.ToString(Session["prmFgItem_po"]);
            else
                txt_ino.Text = "";
            if (Session["prmVenItem_po"] != null)
                txt_venino.Text = Convert.ToString(Session["prmVenItem_po"]);
            else
                txt_venino.Text = "";
            if (Session["prmDueDate_po"] != null)
                txt_due.Text = Convert.ToString(Session["prmDueDate_po"]);
            else
                txt_due.Text = "";
            if (Session["prmJob_po"] != null)
                txt_job.Text = Convert.ToString(Session["prmJob_po"]);
            else
                txt_job.Text = "";
            if (Session["prmJob2_po"] != null)
                txt_job2.Text = Convert.ToString(Session["prmJob2_po"]);
            else
                txt_job2.Text = "";
            if (Session["prmOpen_po"] != null)
                CheckBox1.Checked = Convert.ToBoolean(Session["prmOpen_po"]);
            else
            {
                CheckBox1.Checked = true;
                Session["prmOpen_po"] = "True";
            }

            if (Session["prmClose_po"] != null ||  Convert.ToString(Session["prmClose_po"]) != "")
                CheckBox2.Checked = Convert.ToBoolean(Session["prmClose_po"]);
            try
            {
                Session["Rowuser"] = UserLogin.UserName;
                TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
                GridView2.PageSize = Convert.ToInt32(ddl_display.Text);
            }
            catch { }
            
        }

        try
        {
            raw_url = Page.Request.RawUrl;
            string[] url = raw_url.Split(new char[] { '/' });
            Session["main_prgrm_url"] = url[2].ToString();
            Session["sub_prgrm_url"] = url[2].ToString();            
        }
        catch { }
                
        


        //ImageButton brwsorder = (ImageButton)Master.FindControl("list_po");
        //brwsorder.ImageUrl = "Images/BrowsePo_1.jpg";
              
        GridView2.PageIndex = Convert.ToInt32(Session["page_inder_po"]);

        GridView2.SelectedIndex = Convert.ToInt32(Session["list_po_grid_seqno_index"]) - 1;
        try
        {
            if (Session["list_po_grid_seqno_index"] == null)
            {
                GridView2.SelectedIndex = 0;
                Session["pur_ord_po"] = ((Label)GridView2.SelectedRow.FindControl("Label4")).Text;
                Session["pur_ord_po_line_no"] = ((Label)GridView2.SelectedRow.FindControl("Label3")).Text;
                Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckeyvalue")).Text;
                Session["item"] = ((Label)GridView2.SelectedRow.FindControl("item_label")).Text;
            }
        }
        catch  { }

        txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_ven.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_ino.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_venino.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_due.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        

        try
        {
            TextBox vsearch = (TextBox)FormView1.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }


        if (Session["gridsize"] != null)
        {
            //GridView2.PageSize = Convert.ToInt32(Session["gridsize"]);
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView2.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }




        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

        }

    }


    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["prmPono_po"] == null || Convert.ToString(Session["prmPono_po"]) == "")
            Session["prmPono_po"] = "";
        if (Session["prmVendor_po"] == null || Convert.ToString(Session["prmVendor_po"]) == "")
            Session["prmVendor_po"] = "";
        if (Session["prmFgItem_po"] == null || Convert.ToString(Session["prmFgItem_po"]) == "")
            Session["prmFgItem_po"] = "";
        if (Session["prmVenItem_po"] == null || Convert.ToString(Session["prmVenItem_po"]) == "")
            Session["prmVenItem_po"] = "";
        if (Session["prmDueDate_po"] == null || Convert.ToString(Session["prmDueDate_po"]) == "")
            Session["prmDueDate_po"] = "";
        if (Session["prmJob_po"] == null || Convert.ToString(Session["prmJob_po"]) == "")
            Session["prmJob_po"] = "";
        if (Session["prmJob2_po"] == null || Convert.ToString(Session["prmJob2_po"]) == "")
            Session["prmJob2_po"] = "";
        if (Session["prmOpen_po"] == null || Convert.ToString(Session["prmOpen_po"]) == "")
            Session["prmJob_entry"] = "True";
        if (Session["prmClose_po"] == null || Convert.ToString(Session["prmClose_po"]) == "")
            Session["prmClose_po"] = "False";


        browspo entry = new browspo();
        DataSet ds_entry = new DataSet();

        ds_entry = entry.SelectListPO(UserLogin.UserName, "Search", Convert.ToString(Session["prmPono_po"]), Convert.ToString(Session["prmVendor_po"]), Convert.ToString(Session["prmFgItem_po"]), Convert.ToString(Session["prmVenItem_po"]), Convert.ToString(Session["prmDueDate_po"]), Convert.ToString(Session["prmJob_po"]), Convert.ToString(Session["prmJob2_po"]), Convert.ToString(Session["prmOpen_po"]), Convert.ToString(Session["prmClose_po"]));

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "Brwslist_po.aspx" + "' and sub_program = '" + "Brwslist_po.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "Brwslist_po.aspx" + "' and sub_program = '" + "Brwslist_po.aspx" + "' and display = 0 ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);

            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["browspo_initial_val"] = ds2.Tables[0].Rows.Count;

                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());

                    if (ds2.Tables[0].Rows[0][0].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq - 1);
                        
                    }
                }

               ds_entry.Tables[0].Columns["poline"].SetOrdinal(24);
               ds_entry.Tables[0].Columns["RecKey"].SetOrdinal(25);
               
               for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
               {
                   try
                   {
                       string colval = ds.Tables[0].Rows[i][4].ToString().Trim().ToString();
                       string labelname = ds.Tables[0].Rows[i][3].ToString().Trim().ToString();

                       if (ds_entry.Tables[0].Columns[i].ColumnName == colval)
                       {
                           ds_entry.Tables[0].Columns[i].ColumnName = labelname;
                       }
                   }
                   catch { }
               }

                DataView dv = ds_entry.Tables[0].DefaultView;
                if (Session["browslist_po_grid_sort_ex"] == null)
                {
                    //dv.Sort = "Po# DESC";
                }
                if (Session["browslist_po_grid_sort_ex"] != null)
                {
                    dv.Sort = Convert.ToString(Session["browslist_po_grid_sort_ex"]);
                }

                GridView2.DataSource = dv;
                GridView2.DataBind();
            }
            else
            {

                string cmd_main = "select col_val, seq_no,col_name, display from column_maintenance where  main_program = '" + "Brwslist_po.aspx" + "' and sub_program = '" + "Brwslist_po.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["browspo_initial_val"] = mainds.Tables[0].Rows.Count;
                
                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());
                                                           
                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq - 1);                     
                    }

                }
                ds_entry.Tables[0].Columns["poline"].SetOrdinal(24);

                for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
                {
                    try
                    {
                        string colval = mainds.Tables[0].Rows[i][0].ToString().Trim().ToString();
                        string labelname = mainds.Tables[0].Rows[i][2].ToString().Trim().ToString();

                        if (ds_entry.Tables[0].Columns[i].ColumnName == colval)
                        {
                            ds_entry.Tables[0].Columns[i].ColumnName = labelname;
                        }
                    }
                    catch { }
                }
                
                DataView dv = ds_entry.Tables[0].DefaultView;
                if (Session["browslist_po_grid_sort_ex"] == null)
                {
                    dv.Sort = "Po# DESC";
                }
                if (Session["browslist_po_grid_sort_ex"] != null)
                {
                    dv.Sort = Convert.ToString(Session["browslist_po_grid_sort_ex"]);
                }
                GridView2.DataSource = dv;
                GridView2.DataBind();
            }
        }
        catch { }
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        
        Session["prmAction_po"] = "Search";
        Session["prmPono_po"] = txt_po.Text.Trim();
        Session["prmVendor_po"] = txt_ven.Text.Trim();
        Session["prmFgItem_po"] = txt_ino.Text.Trim();
        Session["prmVenItem_po"] = txt_venino.Text.Trim();
        Session["prmDueDate_po"] = txt_due.Text.Trim();
        Session["prmJob_po"] = txt_job.Text.Trim();
        Session["prmJob2_po"] = txt_job2.Text.Trim();
        Session["prmOpen_po"] = Convert.ToString(CheckBox1.Checked);
        Session["prmClose_po"] = Convert.ToString(CheckBox2.Checked);
        
        Session["list_po_grid_seqno_index"] = null;
        Session["list_po_grid_seqno"] = null;
        GridView2.SelectedIndex = 0;
        Page_Load(sender, e);

    }

    protected void btn_reset_Click(object sender, EventArgs e)
    { 
        ContentPlaceHolder ct = (ContentPlaceHolder)Master.FindControl("ContentPlaceHolder1");
        foreach (Control c in ct.Controls)
        {
            switch (c.GetType().ToString())
            {
                case "System.Web.UI.WebControls.TextBox":
                    ((TextBox)c).Text = "";
                    break;
            }
        }
        
        Session["gridsize"] = null;
        GridView2.SelectedIndex = 0;
        UserClass UserLogin = (UserClass)Session["User"];
        txt_po.Text = "";
        txt_ven.Text = "";
        txt_ino.Text = "";
        txt_venino.Text = "";
        txt_due.Text = "";
        txt_job.Text = "";
        txt_job2.Text = "";
        
        Session["prmAction_po"] = "Search";
        Session["prmPono_po"] = null;
        Session["prmVendor_po"] = null;
        Session["prmFgItem_po"] = null;
        Session["prmVenItem_po"] = null;
        Session["prmDueDate_po"] = null;
        Session["prmJob_po"] = null;
        Session["prmJob2_po"] = null;
        //Session["prmOpen_po"] = null;
        Session["prmClose_po"] = null;
        
        Session["list_po_grid_seqno_index"] = null;       
        
        Session["page_inder_po"] = 0;
        GridView2.PageIndex = Convert.ToInt32(Session["page_inder_po"]);
        Page_Load(sender, e);
    }

    protected void GridView2_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["pur_ord_po"] = ((Label)GridView2.SelectedRow.FindControl("Label4")).Text;
        Session["pur_ord_po_line_no"] = ((Label)GridView2.SelectedRow.FindControl("Label3")).Text;
        Session["order_rec_key"] = ((Label)GridView2.SelectedRow.FindControl("reckeyvalue")).Text;
        Session["item"] = ((Label)GridView2.SelectedRow.FindControl("item_label")).Text;
        Session["list_po_grid_seqno_index"] = GridView2.SelectedIndex + 1;
        
    }

    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void GridView2_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView2.PageIndex = e.NewPageIndex;
        Session["page_inder_po"] = e.NewPageIndex;      
    }

    
   
    protected void GridView2_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Session["browspo_initial_val"] != null)
            {

                for (int show = Convert.ToInt32(Session["browspo_initial_val"]) + 3; show < 60; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }

            }

        }
        catch { }
    }
    protected void GridView2_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        //if (e.Row.RowType == DataControlRowType.DataRow)
        //{
        //    DataRowView drv = (DataRowView)e.Row.DataItem;
        //    for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
        //    {
        //        if (drv.DataView.Table.Columns[i].ColumnName.Equals("Due Date"))
        //        {
        //            string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
        //            e.Row.Cells[i + 35].Text = val[0].ToString();
        //        }
        //        if (drv.DataView.Table.Columns[i].ColumnName.Equals("Order Date"))
        //        {
        //            string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
        //            e.Row.Cells[i + 35].Text = val[0].ToString();
        //        }
        //    }
        //}
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }

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
    protected void GridView2_Sorting(object sender, GridViewSortEventArgs e)
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["browslist_po_grid_sort_ex"] = sortExpression + direction;

    }
   
    
}
