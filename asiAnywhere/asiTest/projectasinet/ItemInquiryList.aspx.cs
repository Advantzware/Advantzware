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
using System.Data.SqlClient;

public partial class fgitem_list : System.Web.UI.Page
{
    string raw_url;
    private string strItem = "";
    private string rbtext = "";
    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";

    int total;
    Int32 seq_no;
    Int32 f_seq_no;
    protected void Page_PreRender(object sender, EventArgs e)
    {

        BuildDataSource();
        try
        {
           //Session["Item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
            GridView1.SelectedIndex = Convert.ToInt32(Session["index_order_itemlist"]);
            if (Session["index_order_itemlist"] == null)
            {
                try
                {
                    GridView1.SelectedIndex = 0;
                    Session["item_list_item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                    Session["Item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                    Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("lbl_rec_key")).Text;
                    Session["item_show_hide_button"] = 1;
                }
                catch { }
            }
            if (Session["index_order_itemlist"] != null)
            {
                Session["item_show_hide_button"] = 1;
                Session["item_list_item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                Session["Item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("lbl_rec_key")).Text;
            }

            

            
        }
        catch
        {
            //return;
        }
    }

    protected void Page_Load(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        raw_url = Page.Request.RawUrl;
        string[] url = raw_url.Split(new char[] { '/' });
        Session["main_prgrm_url"] = url[2].ToString();
        Session["sub_prgrm_url"] = url[2].ToString();
       
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        Session["Rowuser"] = UserLogin.UserName;
        Session["Item_Inquiry_value_check"] = 1;
       
        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "ItemInquiryList.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        try
        {
            //ImageButton listitem = (ImageButton)Master.FindControl("listitem");
            //listitem.ImageUrl = "~/images/lisl item 1.jpg";
            Label name = (Label)Master.FindControl("lbl_page");
            name.Text = "List Item";
            FormView1.ChangeMode(FormViewMode.ReadOnly);
            Label fgitemname = (Label)FormView1.FindControl("ItemLabel");
            Session["newfgitemnameval"] = fgitemname.Text;
            
        }
        catch { }


        

        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
            Session["item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
            Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("lbl_rec_key")).Text;
        }
        catch
        {
            //return;
        }

        try
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["index_order_itemlist"]);
        }
        catch { }
        if (Session["index_order_itemlist"] == null)
        {
            try
            {
                GridView1.SelectedIndex = 0;
                Session["item_list_item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("lbl_rec_key")).Text;
                Session["item_show_hide_button"] = 1;
            }
            catch { }
        }
        if (Session["index_order_itemlist"] != null)
        {
            try
            {
                Session["item_show_hide_button"] = 1;
                Session["item_list_item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
                Session["order_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("lbl_rec_key")).Text;
            }
            catch { }
        }

        if (!Page.IsPostBack)
        {
            txt_fgitem.Text = Convert.ToString(Session["item_inquery_list_item"]);
            txt_name.Text = Convert.ToString(Session["item_inquery_list_itemname"]);
            txt_Cad.Text = Convert.ToString(Session["item_inquery_list_user_cad"]);
            txt_estimate.Text = Convert.ToString(Session["item_inquery_list_user_est"]);
            txt_cat.Text = Convert.ToString(Session["item_inquery_list_user_style"]);
            txt_style.Text = Convert.ToString(Session["item_inquery_list_user_procat"]);

        }
        txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_name.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_Cad.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_cat.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_style.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");


    }




    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }

    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["gridsize"] = ddl_display.Text;
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        }
        catch { }

    }

    private void BuildDataSource()
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["item_inquery_list_item"] == null || Convert.ToString(Session["item_inquery_list_item"]) == "")
            Session["item_inquery_list_item"] = "";

        if (Session["item_inquery_list_itemname"] == null || Convert.ToString(Session["item_inquery_list_itemname"]) == "")
            Session["item_inquery_list_itemname"] = "";

        if (Session["item_inquery_list_user_cad"] == null || Convert.ToString(Session["item_inquery_list_user_cad"]) == "")
            Session["item_inquery_list_user_cad"] = "";

        if (Session["item_inquery_list_user_est"] == null || Convert.ToString(Session["item_inquery_list_user_est"]) == "")
            Session["item_inquery_list_user_est"] = "";

        if (Session["item_inquery_list_user_style"] == null || Convert.ToString(Session["item_inquery_list_user_style"]) == "")
            Session["item_inquery_list_user_style"] = "";

        if (Session["item_inquery_list_user_procat"] == null || Convert.ToString(Session["item_inquery_list_user_procat"]) == "")
            Session["item_inquery_list_user_procat"] = "";

        if (Session["item_inquery_list_user_castpart"] == null || Convert.ToString(Session["item_inquery_list_user_castpart"]) == "")
            Session["item_inquery_list_user_castpart"] = "";

        if (Session["item_inquery_list_user_die"] == null || Convert.ToString(Session["item_inquery_list_user_die"]) == "")
            Session["item_inquery_list_user_die"] = "";

        if (Session["item_inquery_list_user_startdate"] == null || Convert.ToString(Session["item_inquery_list_user_startdate"]) == "")
            Session["item_inquery_list_user_startdate"] = "";

        if (Session["item_inquery_list_user_enddate"] == null || Convert.ToString(Session["item_inquery_list_user_enddate"]) == "")
            Session["item_inquery_list_user_enddate"] = "";

        if (Convert.ToString(Session["item_inquery_list_action"]) != "SearchItem" || Convert.ToString(Convert.ToString(Session["item_inquery_list_action"])) == "" || Session["item_inquery_list_action"] == null)
        {
            Session["item_inquery_list_action"] = "BrwsItem";
        }

        
        Order entry = new Order();
        DataSet ds_entry = new DataSet();
        ds_entry = entry.BrwsItemInq(UserLogin.UserName, Convert.ToString(Session["item_inquery_list_action"]), Convert.ToString(Session["item_inquery_list_item"]), Convert.ToString(Session["item_inquery_list_itemname"]), Convert.ToString(Session["item_inquery_list_user_cad"]), Convert.ToString(Session["item_inquery_list_user_est"]), Convert.ToString(Session["item_inquery_list_user_style"]), Convert.ToString(Session["item_inquery_list_user_procat"]), Convert.ToString(Session["item_inquery_list_user_castpart"]), Convert.ToString(Session["item_inquery_list_user_die"]), Convert.ToString(Session["item_inquery_list_user_startdate"]), Convert.ToString(Session["item_inquery_list_user_enddate"]));
       
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select * from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "ItemInquiryList.aspx" + "' and sub_program = '" + "ItemInquiryList.aspx" + "' and display = 0 order by col_seq ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            string cmd2 = "select display from column_seq_user where user_name = '" + UserLogin.UserName + "' and main_program = '" + "ItemInquiryList.aspx" + "' and sub_program = '" + "ItemInquiryList.aspx" + "' and display = 0 ";
            SqlDataAdapter da2 = new SqlDataAdapter(cmd2, conn);
            DataSet ds2 = new DataSet();
            da2.Fill(ds2);

            if (ds2.Tables[0].Rows.Count > 0)
            {
                Session["initial_val_iteminq"] = ds2.Tables[0].Rows.Count;

                for (int j = 0; j < ds.Tables[0].Rows.Count; j++)
                {
                    string fld = ds.Tables[0].Rows[j][4].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(ds.Tables[0].Rows[j][6].ToString().Trim().ToString());

                    if (ds2.Tables[0].Rows[0][0].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }
                }

            }
            else
            {

                string cmd_main = "select col_val, seq_no, display from column_maintenance where  main_program = '" + "ItemInquiryList.aspx" + "' and sub_program = '" + "ItemInquiryList.aspx" + "' and display = 0  order by seq_no ";
                SqlDataAdapter mainda = new SqlDataAdapter(cmd_main, conn);
                DataSet mainds = new DataSet();
                mainda.Fill(mainds);
                Session["initial_val_iteminq"] = mainds.Tables[0].Rows.Count;

                for (int j = 0; j < mainds.Tables[0].Rows.Count; j++)
                {
                    string fld = mainds.Tables[0].Rows[j][0].ToString().Trim().ToString();
                    Int32 seq = Convert.ToInt32(mainds.Tables[0].Rows[j][1].ToString().Trim().ToString());

                    if (mainds.Tables[0].Rows[0][2].ToString() == "0")
                    {
                        ds_entry.Tables[0].Columns[fld].SetOrdinal(seq);
                    }

                }
            }

            ds_entry.Tables[0].Columns["vcad"].SetOrdinal(16);
            ds_entry.Tables[0].Columns["vSpc"].SetOrdinal(17);
            for (int i = 0; i < ds_entry.Tables[0].Columns.Count; i++)
            {
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vItemno")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Item No";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vName")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Name";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vPartdsc")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Description";
                }

                if (ds_entry.Tables[0].Columns[i].ColumnName == "vCust")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Cust#";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vStyle")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Style";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vProcat")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Category";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vCode")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Stock/Custom";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vEstno")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Estimate";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vStocked")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Stocked";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "vQonh")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Qty On-Hand";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "cCustPart")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Cust Part#";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "cDie")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Die#";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "dAllocQty")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Allocated Qty";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "dAvalQty")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Available Qty";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "dOrderQty")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Ordered Qty";
                }
                if (ds_entry.Tables[0].Columns[i].ColumnName == "dShipQty")
                {
                    ds_entry.Tables[0].Columns[i].ColumnName = "Shipped Qty";
                }
            }
            DataView dv = ds_entry.Tables[0].DefaultView;
            if (Session["iteminq_entry_grid2_sort_ex"] == null)
            {
                dv.Sort = "Item No ASC";
            }
            if (Session["iteminq_entry_grid2_sort_ex"] != null)
            {
                dv.Sort = Convert.ToString(Session["iteminq_entry_grid2_sort_ex"]);
            }
            GridView1.DataSource = dv;
            GridView1.DataBind();
        }

        catch { }
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
            
            Session["item_inquery_list_user"] = UserLogin.UserName;
            Session["item_inquery_list_action"] = "SearchItem";
            Session["item_inquery_list_item"] = txt_fgitem.Text.Trim();
            Session["item_inquery_list_itemname"] = txt_name.Text.Trim();
            Session["item_inquery_list_user_cad"] = txt_Cad.Text.Trim();
            Session["item_inquery_list_user_est"] = txt_estimate.Text.Trim();
            Session["item_inquery_list_user_style"] = txt_style.Text.Trim();
            Session["item_inquery_list_user_procat"] = txt_cat.Text.Trim();
            
            Session["item_inquery_list_user_castpart"] = txt_custpart.Text;
            Session["item_inquery_list_user_die"] = txt_die.Text;
            Session["item_inquery_list_user_startdate"] = txt_startdate.Text.Trim();
            Session["item_inquery_list_user_enddate"] = txt_enddate.Text.Trim();
            

            Page_Load(sender, e);

    }

    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        try
        {
            GridView1.PageIndex = e.NewPageIndex;
        }
        catch { }
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Session["iteminq_entry_grid2_sort_ex"] = sortExpression + direction;

    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        string str = "";
        txt_fgitem.Text = str.ToString();
        txt_name.Text = str.ToString();
        txt_Cad.Text = str.ToString();
        txt_estimate.Text = str.ToString();
        txt_style.Text = str.ToString();
        txt_cat.Text = str.ToString();
        txt_custpart.Text = str.ToString();
        txt_die.Text = str.ToString();
        txt_startdate.Text = str.ToString();
        txt_enddate.Text = str.ToString();
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

       
        Session["item_inquery_list_user"] = UserLogin.UserName;
        Session["item_inquery_list_action"] = "BrwsItem";
        Session["item_inquery_list_item"] = txt_fgitem.Text.Trim();
        Session["item_inquery_list_itemname"] = txt_name.Text.Trim();
        Session["item_inquery_list_user_cad"] = txt_Cad.Text.Trim();
        Session["item_inquery_list_user_est"] = txt_estimate.Text.Trim();
        Session["item_inquery_list_user_style"] = txt_style.Text.Trim();
        Session["item_inquery_list_user_procat"] = txt_cat.Text.Trim();
        Session["item_inquery_list_user_castpart"] = txt_custpart.Text.Trim();
        Session["item_inquery_list_user_die"] = txt_die.Text.Trim();
        Session["item_inquery_list_user_startdate"] = txt_startdate.Text.Trim();
        Session["item_inquery_list_user_enddate"] = txt_enddate.Text.Trim();
        Page_Load(sender, e);

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            int index = GridView1.SelectedIndex;
            Session["index_order_itemlist"] = index;
            Session["item_list_item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
            Session["Item"] = ((Label)GridView1.SelectedRow.FindControl("item_label")).Text;
            Session["item_show_hide_button"] = 1;
        }
        catch { }
    }

    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            if (Session["initial_val_iteminq"] != null)
            {

                for (int show = Convert.ToInt32(Session["initial_val_iteminq"]) + 17; show < 36; show++)
                {
                    e.Row.Cells[show].Visible = false;
                }
                e.Row.Cells[60].Visible = false;
                e.Row.Cells[61].Visible = false;
                e.Row.Cells[62].Visible = false;
                e.Row.Cells[63].Visible = false;
                e.Row.Cells[64].Visible = false;
                e.Row.Cells[65].Visible = false;
                e.Row.Cells[66].Visible = false;
                e.Row.Cells[67].Visible = false;
                e.Row.Cells[68].Visible = false;
                e.Row.Cells[69].Visible = false;
                e.Row.Cells[70].Visible = false;
                e.Row.Cells[71].Visible = false;
                e.Row.Cells[72].Visible = false;
                e.Row.Cells[73].Visible = false;
                e.Row.Cells[74].Visible = false;
                e.Row.Cells[75].Visible = false;
                e.Row.Cells[76].Visible = false;
                e.Row.Cells[77].Visible = false;
                e.Row.Cells[78].Visible = false;
                e.Row.Cells[79].Visible = false;
                e.Row.Cells[80].Visible = false;
                e.Row.Cells[81].Visible = false;
                e.Row.Cells[82].Visible = false;


            }

        }
        catch { }
    }
    protected void GridView1_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView drv = (DataRowView)e.Row.DataItem;
            for (int i = 0; i < drv.DataView.Table.Columns.Count; i++)
            {
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Due Date"))
                {
                    string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 35].Text = val[0].ToString();
                }
                if (drv.DataView.Table.Columns[i].ColumnName.Equals("Order Date"))
                {
                    string[] val = e.Row.Cells[i + 35].Text.Split(new char[] { ' ' });
                    e.Row.Cells[i + 35].Text = val[0].ToString();
                }
            }
        }
        for (int i = 0; i < e.Row.Cells.Count; i++)
        {
            e.Row.Cells[i].Attributes.Add("style", "white-space: nowrap;");
        }

    }
}